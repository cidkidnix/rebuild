{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    copyDeployment,
    signClosures,
    withSSH,
    filterNixString,
    NixRun,
    nixEnvPath,
    nixOSBuildargs,
    nixDarwinBuildargs,
  )
where

import Cli.Extras
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Monoid as M
import qualified Data.Text as T
import System.Directory
import System.Exit
import System.Linux.Mount
import System.Posix.Types
import System.Posix.User
import System.Which

data NixError
  = P ProcessFailure
  | T T.Text

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

nixOSBuildargs :: String -> String -> String -> String -> [String]
nixOSBuildargs flakepath name typ profile = ["--no-link", "--print-out-paths", "--profile", profile, flakepath <> "#nixosConfigurations." <> name <> ".config.system.build." <> typ]

nixDarwinBuildargs :: String -> String -> String -> String -> [String]
nixDarwinBuildargs flakepath name typ profile = ["--no-link", "--print-out-paths", "--profile", profile, flakepath <> "#darwinConfigurations" <> name <> ".config.system.build." <> typ]

filterNixString :: String -> String
filterNixString a = do
  let a' = filter (/= '"') a
  filter (/= '\n') a'

runProcess :: NixRun e m => FilePath -> [String] -> m String
runProcess com args = do
  let args' = map filterNixString args
      cmd = com
  T.unpack <$> readProcessAndLogOutput (Debug, Debug) (proc cmd args')

runProcessWithSSH :: NixRun e m => String -> String -> [String] -> [String] -> m String
runProcessWithSSH port host sargs com = do
  let sargs' = map filterNixString sargs
      pargs' = unwords com
      cmd = sshExePath
      args' =
        M.mconcat
          [ sargs',
            [host, "-p", port],
            [pargs']
          ]
  T.unpack <$> readProcessAndLogOutput (Debug, Debug) (proc cmd args')

withSSH :: NixRun e m => String -> String -> [String] -> [[String]] -> m ()
withSSH port host sargs com = mapM_ (\x -> runProcessWithSSH port host sargs x) com

signClosures :: NixRun e m => String -> String -> m String
signClosures path key = do
  let outpath' = filterNixString path
  withSpinner ("Signing path " <> T.pack path) $ do
    runProcess nixExePath ["store", "sign", "-k", key, outpath']

copyDeployment :: NixRun e m => String -> String -> String -> String -> m String
copyDeployment host name outpath uri = do
  let outpath' = filterNixString outpath
      nixhost = uri <> host
  withSpinner ("Copying Deployment for " <> T.pack name) $ do
    runProcess nixExePath ["copy", "-s", "--to", nixhost, outpath', "--no-check-sigs"]

checkForUser :: NixRun e m => CUid -> m ()
checkForUser a = do
  root <- fmap (== a) (liftIO $ getRealUserID)
  case root of
    False -> do
      failWith "Not Root, Exiting"
    _ -> pure ()
