{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Rebuild.Nix where

import Cli.Extras
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Rebuild.Helpers
import System.Exit
import System.Posix.Types
import System.Posix.User
import System.Which

class NixConfig a where
  getOptions :: a -> [(Text, Text)]
  getStorePath :: a -> Text
  getExtraArgs :: a -> [Text]
  getExperimentalFeatures :: a -> [Text]
  getProfile :: a -> Maybe Text
  getStoreRoot :: a -> Text

class SSHConfig a where
  getProtoS :: a -> Text
  getHost :: a -> Text
  getPortS :: a -> Maybe Int
  getFullUriS :: a -> Text

class HTTPConfig a where
  getProtoH :: a -> Text
  getSite :: a -> Text
  getPortH :: a -> Maybe Int
  getFullUriH :: a -> Text

class S3Config a where
  getBucket :: a -> Text
  getProfileB :: a -> Maybe Text
  getRegion :: a -> Maybe Text
  getScheme :: a -> Maybe Text
  getEndpoint :: a -> Maybe Text
  getFullUri3 :: a -> Text

data NixSettings = NixSettings
  { _options :: [(Text, Text)],
    _storePath :: Text,
    _extraArgs :: [Text],
    _experimentalFeatures :: [Text],
    _profile :: Maybe Text
  }

data SSHStore = SSHStore
  { _protoS :: Text,
    _sshHost :: Text,
    _sshPort :: Maybe Int
  }
  deriving (Show)

data HTTPStore = HTTPStore
  { _protoH :: Text,
    _site :: Text,
    _port :: Maybe Int
  }
  deriving (Show)

data S3Store = S3Store
  { _bucket :: Text,
    _profileB :: Maybe Text,
    _region :: Maybe Text,
    _scheme :: Maybe Text,
    _endpoint :: Maybe Text
  }
  deriving (Show)

instance NixConfig NixSettings where
  getOptions p = _options p
  getStorePath p = _storePath p
  getExtraArgs p = _extraArgs p
  getExperimentalFeatures p = _experimentalFeatures p
  getProfile p = _profile p
  getStoreRoot p = T.replace "nix/store" "" (_storePath p)

instance SSHConfig SSHStore where
  getProtoS p = _protoS p
  getHost p = _sshHost p
  getPortS p = _sshPort p
  getFullUriS p =
    ( _protoS p
        <> _sshHost p
        <> maybeIntToText (_sshPort p) (":" <> intToText (fromJust (_sshPort p)))
    )

instance HTTPConfig HTTPStore where
  getProtoH p = _protoH p
  getSite p = _site p
  getPortH p = _port p
  getFullUriH p =
    ( _protoH p
        <> _site p
        <> maybeIntToText (_port p) (":" <> intToText (fromJust (_port p)))
    )

instance S3Config S3Store where
  getBucket p = _bucket p
  getProfileB p = _profileB p
  getRegion p = _region p
  getScheme p = _scheme p
  getEndpoint p = _endpoint p
  getFullUri3 p =
    "s3://"
      <> _bucket p
      <> maybeText (_profileB p) ("?profile=" <> fromJust (_profileB p))
      <> maybeText (_region p) ("?region=" <> fromJust (_region p))
      <> maybeText (_scheme p) ("?scheme=" <> fromJust (_scheme p))
      <> maybeText (_endpoint p) ("?endpoint=" <> fromJust (_endpoint p))

data StoreURI
  = S3StoreURI S3Store
  | HTTPStoreURI HTTPStore
  | SSHStoreURI SSHStore
  | Daemon

parseStoreUri :: StoreURI -> Text
parseStoreUri p = case p of
  S3StoreURI v -> getFullUri3 v
  HTTPStoreURI v -> getFullUriH v
  SSHStoreURI v -> getFullUriS v
  Daemon -> "daemon"

maybeText :: Maybe Text -> Text -> Text
maybeText b c = if b == Nothing then "" else c

maybeIntToText :: Maybe Int -> Text -> Text
maybeIntToText b c = if b == Nothing then "" else c

testSSHStore :: SSHStore
testSSHStore =
  SSHStore
    { _protoS = "ssh-ng://",
      _sshHost = "cidkid@10.0.0.68",
      _sshPort = Nothing
    }

testHTTPSStore :: HTTPStore
testHTTPSStore =
  HTTPStore
    { _protoH = "http://",
      _site = "cache.nixos.org",
      _port = Nothing
    }

testS3Store :: S3Store
testS3Store =
  S3Store
    { _bucket = "testing-cache-2",
      _profileB = Just "cache-upload",
      _region = Just "eu-west-1",
      _scheme = Just "https",
      _endpoint = Just "example.com"
    }

defaultSettings :: NixSettings
defaultSettings =
  NixSettings
    { _options =
        [ ("cores", "1")
        ],
      _extraArgs =
        [ "--print-out-paths",
          "--no-link"
        ],
      _storePath = "daemon",
      _experimentalFeatures =
        [ "flakes",
          "nix-command"
        ],
      _profile = Nothing
    }

getNixArgs :: NixSettings -> [Text]
getNixArgs settings =
  M.mconcat
    [ (concatMap (\x -> ["--option", fst x, snd x]) (getOptions settings)),
      getExtraArgs settings,
      ["--store", getStoreRoot settings],
      (concatMap (\x -> ["--extra-experimental-features", x]) (getExperimentalFeatures settings)),
      (if (getProfile settings) == Nothing then [] else ["--profile", fromJust (getProfile settings)])
    ]

nixBuild :: NixRun e m => NixSettings -> FlakeDef -> m Text
nixBuild config flakedef = do
  runProcess nixExePath (["build"] ++ (getNixArgs config) ++ map T.pack (fromFlakeDef flakedef))

buildSystem :: NixRun e m => NixSettings -> FlakeDef -> Maybe Text -> m StorePath
buildSystem settings flakedef profile = do
  withSpinner ("Building system") $ do
    toStorePath <$> nixBuild (settings {_profile = profile}) flakedef

signClosures :: NixRun e m => NixSettings -> StorePath -> Text -> m Text
signClosures settings path key = do
  withSpinner ("Signing path " <> fromStorePath path) $ do
    runProcess nixExePath (getNixArgs settings ++ ["store", "sign", "-k", key, fromStorePath path])

copyDeployment :: NixRun e m => StoreURI -> String -> StorePath -> m Text
copyDeployment uri name outpath = do
  withSpinner ("Copying Deployment for " <> T.pack name) $ do
    runProcess nixExePath ["copy", "-s", "--to", parseStoreUri uri, fromStorePath outpath, "--no-check-sigs"]
