{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Rebuild.Types where

import Cli.Extras
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text as T
import Data.Maybe
import Data.String
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Control.Lens

-- Cli Parser
data Opts = Opts
  { configpath :: !String,
    nixsystem :: !String,
    legacy :: !Bool,
    verbose :: !Bool,
    darwin :: !Bool,
    scommand :: !Command
  }

data Options = Options
  { path :: String,
    name :: String,
    com :: Command
  }

data SwitchCommand
  = B
  | S
  | D

data Command
  = Build
  | VM
  | VMWithBootLoader
  | DryActivate
  | Switch String
  | Boot String
  | BuildISO
  | Deploy String String String Bool
  | NixOSInstall String Bool

-- Nix types
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
  deriving (Show)

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

data StoreURI
  = S3StoreURI S3Store
  | HTTPStoreURI HTTPStore
  | SSHStoreURI SSHStore
  | Daemon

newtype FlakeDef = FlakeDef [String]

newtype StorePath = StorePath Text

class IsStorePath a where
  fromStorePath :: a -> Text
  toStorePath :: Text -> a
  toFilePath :: a -> FilePath

class IsNixStore a where
  fromNixStore :: a -> Text

class IsFlakeDef a where
  fromFlakeDef :: a -> [String]
  toFlakeDef :: String -> String -> String -> String -> a

newtype NixStore = NixStore Text

type OtherOutput = StorePath

data NixError
  = P ProcessFailure
  | T Text

type NixRun e m =
  ( AsUnstructuredError e,
    MonadFail m,
    AsProcessFailure e,
    CliThrow e m,
    HasCliConfig e m,
    MonadMask m,
    MonadIO m,
    CliLog m
  )

-- Helper Functions
maybeText :: Maybe Text -> Text -> Text
maybeText b c = if isNothing b then "" else c

maybeIntToText :: Maybe Int -> Text -> Text
maybeIntToText b c = if isNothing b then "" else c

intToText :: Int -> Text
intToText = T.toStrict . B.toLazyText . B.decimal

-- Instances
instance IsNixStore NixStore where
  fromNixStore (NixStore s) = s

instance IsString StorePath where
  fromString x = StorePath (T.pack x)

instance IsStorePath StorePath where
  fromStorePath (StorePath s) = T.filter (/= '\n') (T.filter (/= '"') s)
  toStorePath = StorePath
  toFilePath x = T.unpack (fromStorePath x)

instance IsStorePath Text where
  fromStorePath (s) = T.filter (/= '\n') (T.filter (/= '"') s)
  toStorePath s = s
  toFilePath s = T.unpack s

instance IsFlakeDef FlakeDef where
  fromFlakeDef (FlakeDef s) = s
  toFlakeDef flakepath config name' typ = FlakeDef [flakepath <> "#" <> config <> "." <> name' <> "." <> "config.system.build" <> "." <> typ]

instance Show NixError where
  show = \case
    P pf -> show pf
    T t -> T.unpack t

makePrisms ''NixError

instance AsUnstructuredError NixError where
  asUnstructuredError = _T

instance AsProcessFailure NixError where
  asProcessFailure = _P

instance NixConfig NixSettings where
  getStoreRoot p = T.replace "nix/store" "" (_storePath p)
  getOptions = _options
  getStorePath = _storePath
  getExtraArgs = _extraArgs
  getExperimentalFeatures = _experimentalFeatures
  getProfile = _profile

instance SSHConfig SSHStore where
  getFullUriS p =
    _protoS p
      <> _sshHost p
      <> maybeIntToText (_sshPort p) (":" <> intToText (fromJust (_sshPort p)))
  getProtoS = _protoS
  getHost = _sshHost
  getPortS = _sshPort

instance HTTPConfig HTTPStore where
  getFullUriH p =
    _protoH p
      <> _site p
      <> maybeIntToText (_port p) (":" <> intToText (fromJust (_port p)))
  getSite = _site
  getPortH = _port
  getProtoH = _protoH

instance S3Config S3Store where
  getFullUri3 p =
    "s3://"
      <> _bucket p
      <> maybeText (_profileB p) ("?profile=" <> fromJust (_profileB p))
      <> maybeText (_region p) ("?region=" <> fromJust (_region p))
      <> maybeText (_scheme p) ("?scheme=" <> fromJust (_scheme p))
      <> maybeText (_endpoint p) ("?endpoint=" <> fromJust (_endpoint p))
  getBucket = _bucket
  getProfileB = _profileB
  getRegion = _region
  getScheme = _scheme
  getEndpoint = _endpoint

