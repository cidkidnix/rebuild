{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Rebuild.Helpers
  ( checkForUser,
    nixExePath,
    commonNixArgs,
    runProcess,
    nixRun,
    runProcessWithSSH,
    withSSH,
    nixEnvPath,
    nixOSBuildargs,
    nixDarwinBuildargs,
    fromFlakeDef,
    toFlakeDef,
    fromStorePath,
    toStorePath,
    toFilePath,
    intToText,
    NixRun,
    NixStore,
    StorePath,
    FlakeDef,
    OtherOutput,
  )
where

import Cli.Extras
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Monoid as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import System.Exit
import System.Posix.Types
import System.Posix.User
import System.Which

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

instance IsNixStore NixStore where
  fromNixStore (NixStore s) = s

newtype StorePath = StorePath Text

instance IsString StorePath where
  fromString x = StorePath (T.pack x)

instance IsStorePath StorePath where
  fromStorePath (StorePath s) = T.filter (/= '\n') (T.filter (/= '"') s)
  toStorePath = StorePath
  toFilePath x = T.unpack (fromStorePath x)

instance IsStorePath Text where
  fromStorePath (s) = T.filter (/= '\n') (T.filter (/= '"') s)
  toStorePath s = s

newtype FlakeDef = FlakeDef [String]

instance IsFlakeDef FlakeDef where
  fromFlakeDef (FlakeDef s) = s
  toFlakeDef flakepath config name typ = FlakeDef [flakepath <> "#" <> config <> "." <> name <> "." <> "config.system.build" <> "." <> typ]

type OtherOutput = StorePath

data NixError
  = P ProcessFailure
  | T Text

instance Show NixError where
  show = \case
    P pf -> show pf
    T t -> T.unpack t

makePrisms ''NixError

instance AsUnstructuredError NixError where
  asUnstructuredError = _T

instance AsProcessFailure NixError where
  asProcessFailure = _P

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

intToText :: Int -> Text
intToText = T.toStrict . B.toLazyText . B.decimal

nixRun :: Severity -> CliT NixError IO a -> IO a
nixRun severity action = do
  cfg <- newCliConfig severity False False (\t -> (T.pack . show $ t, ExitFailure 1))
  runCli cfg action

nixExePath :: FilePath
nixExePath = $(staticWhich "nix")

sshExePath :: FilePath
sshExePath = $(staticWhich "ssh")

nixEnvPath :: FilePath
nixEnvPath = $(staticWhich "nix-env")

commonNixArgs :: [String]
commonNixArgs =
  M.mconcat
    [ ["--option", "sandbox", "true"]
    ]

nixOSBuildargs :: String -> String -> String -> FlakeDef
nixOSBuildargs flakepath = toFlakeDef flakepath "nixosConfigurations"

nixDarwinBuildargs :: String -> String -> String -> FlakeDef
nixDarwinBuildargs flakepath = toFlakeDef flakepath "darwinConfigurations"

runProcess :: NixRun e m => FilePath -> [Text] -> m Text
runProcess com args = do
  toStorePath <$> readProcessAndLogOutput (Debug, Debug) (proc com (map T.unpack args))

runProcessWithSSH :: NixRun e m => String -> String -> [String] -> [Text] -> m OtherOutput
runProcessWithSSH port host sargs com = do
  let pargs' = T.unwords com
      cmd = sshExePath
      args' =
        M.mconcat
          [ sargs,
            [host, "-p", port],
            [T.unpack pargs']
          ]
  toStorePath <$> readProcessAndLogOutput (Debug, Debug) (proc cmd args')

withSSH :: NixRun e m => String -> String -> [String] -> [[Text]] -> m ()
withSSH port host sargs com = mapM_ (\x -> runProcessWithSSH port host sargs x) com

checkForUser :: NixRun e m => CUid -> m ()
checkForUser a = do
  root <- fmap (== a) (liftIO $ getRealUserID)
  case root of
    False -> do
      failWith "Not Root, Exiting"
    _ -> pure ()
