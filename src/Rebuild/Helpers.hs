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
import Control.Monad.IO.Class
import Data.Monoid as M
import Data.Text (Text)
import qualified Data.Text as T
import Rebuild.Types
import System.Exit
import System.Posix.Types
import System.Posix.User
import System.Which

makePrisms ''NixError

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
runProcess com' args = do
  toStorePath <$> readProcessAndLogOutput (Debug, Debug) (proc com' (map T.unpack args))

runProcessWithSSH :: NixRun e m => String -> String -> [String] -> [Text] -> m OtherOutput
runProcessWithSSH port host sargs com' = do
  let pargs' = T.unwords com'
      cmd = sshExePath
      args' =
        M.mconcat
          [ sargs,
            [host, "-p", port],
            [T.unpack pargs']
          ]
  toStorePath <$> readProcessAndLogOutput (Debug, Debug) (proc cmd args')

withSSH :: NixRun e m => String -> String -> [String] -> [[Text]] -> m ()
withSSH port host sargs com' = mapM_ (\x -> runProcessWithSSH port host sargs x) com'

checkForUser :: NixRun e m => CUid -> m ()
checkForUser a = do
  root <- fmap (== a) (liftIO $ getRealUserID)
  case root of
    False -> do
      failWith "Not Root, Exiting"
    _ -> pure ()
