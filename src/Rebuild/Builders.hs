{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Rebuild.Builders where

import Cli.Extras
import Data.Text (Text)
import qualified Data.Text as T
import Rebuild.Flake
import Rebuild.Helpers
import Rebuild.Nix
import Rebuild.Types

parseSwitchCommand :: SwitchCommand -> Text
parseSwitchCommand a = case a of
  B -> "boot"
  S -> "switch"
  D -> "dry-activate"

switchToConfig :: NixRun e m => StorePath -> SwitchCommand -> m Text
switchToConfig path' arg = do
  withSpinner ("Switching to " <> fromStorePath path') $ do
    runProcess (toFilePath path' <> "/bin/switch-to-configuration") [(parseSwitchCommand arg)]

runVM :: NixRun e m => StorePath -> String -> m Text
runVM path' sys = do
  withSpinner "Running VM.. " $ do
    runProcess (toFilePath path' <> "/bin/run-" <> sys <> "-vm") []

legacyBuild :: NixRun e m => Options -> m ()
legacyBuild opts = case opts.com of
  _ -> failWith "Not implemented yet"

flakeBuild :: NixRun e m => Options -> m ()
flakeBuild opts = case opts.com of
  Build -> do
    sysbuild <- buildFlakeSystem defaultSettings (nixOSBuildargs opts.path opts.name "toplevel")
    putLog Informational ("System Closure at " <> fromStorePath sysbuild)
    pure ()
  DryActivate -> do
    sysbuild <- buildFlakeSystem defaultSettings (nixOSBuildargs opts.path opts.name "toplevel")
    _ <- switchToConfig sysbuild D
    pure ()
  VM -> do
    sysbuild <- buildFlakeSystem defaultSettings (nixOSBuildargs opts.path opts.name "vm")
    _ <- runVM sysbuild opts.name
    pure ()
  VMWithBootLoader -> do
    sysbuild <- buildFlakeSystem defaultSettings (nixOSBuildargs opts.path opts.name "vmWithBootLoader")
    _ <- runVM sysbuild opts.name
    pure ()
  BuildISO -> do
    sysbuild <- buildFlakeSystem defaultSettings (nixOSBuildargs opts.path opts.name "isoImage")
    putLog Informational ("ISO image at " <> fromStorePath sysbuild)
    pure ()
  Switch profile -> do
    checkForUser 0
    sysbuild <- buildFlakeSystem (defaultSettings {_profile = Just (T.pack profile)}) (nixOSBuildargs opts.path opts.name "toplevel")
    _ <- switchToConfig sysbuild S
    pure ()
  Boot profile -> do
    checkForUser 0
    sysbuild <- buildFlakeSystem (defaultSettings {_profile = Just (T.pack profile)}) (nixOSBuildargs opts.path opts.name "toplevel")
    _ <- switchToConfig sysbuild B
    pure ()
  _ -> pure ()
