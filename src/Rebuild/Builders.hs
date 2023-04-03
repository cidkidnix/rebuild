{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rebuild.Builders where

import Cli.Extras
import Control.Monad.IO.Class
import Data.Monoid as M
import Data.Text (Text)
import qualified Data.Text as T
import Rebuild.Helpers
import Rebuild.Nix

switchToConfig :: NixRun e m => StorePath -> Text -> m Text
switchToConfig path arg = do
  withSpinner ("Switching to " <> fromStorePath path) $ do
    runProcess (toFilePath path <> "/bin/switch-to-configuration") [arg]

runVM :: NixRun e m => StorePath -> String -> m Text
runVM path sys = do
  withSpinner "Running VM.. " $ do
    runProcess (toFilePath path <> "/bin/run-" <> sys <> "-vm") []

-- systemBuild and regBuild are split, because when we switch or boot to a new configuration
-- we have to add it to the system profile, we don't want to do this if we just build
-- the closure or build a VM, since those don't need to be added to the system profile
-- and subsequently the bootloader
--
-- Also we need to pass the profile to systemBuild
systemBuild :: NixRun e m => String -> String -> String -> Text -> m ()
systemBuild path name profile arg = case arg of
  "switch" -> do
    checkForUser 0
    sysbuild <- buildSystem (defaultSettings {_profile = Just (T.pack profile)}) (nixOSBuildargs path name "toplevel")
    _ <- switchToConfig sysbuild arg
    pure ()
  "boot" -> do
    checkForUser 0
    sysbuild <- buildSystem (defaultSettings {_profile = Just (T.pack profile)}) (nixOSBuildargs path name "toplevel")
    _ <- switchToConfig sysbuild arg
    pure ()
  _ -> pure ()

regBuild :: NixRun e m => String -> String -> Text -> m ()
regBuild path name arg = case arg of
  "build" -> do
    sysbuild <- buildSystem defaultSettings (nixOSBuildargs path name "toplevel")
    putLog Informational ("System Closure at " <> fromStorePath sysbuild)
    pure ()
  "dry-activate" -> do
    sysbuild <- buildSystem defaultSettings (nixOSBuildargs path name "toplevel")
    _ <- switchToConfig sysbuild arg
    pure ()
  "vm" -> do
    sysbuild <- buildSystem defaultSettings (nixOSBuildargs path name "vm")
    _ <- runVM sysbuild name
    pure ()
  "vm-with-bootloader" -> do
    sysbuild <- buildSystem defaultSettings (nixOSBuildargs path name "vmWithBootLoader")
    _ <- runVM sysbuild name
    pure ()
  "build-iso" -> do
    sysbuild <- buildSystem defaultSettings (nixOSBuildargs path name "isoImage")
    putLog Informational ("ISO image at " <> fromStorePath sysbuild)
    pure ()
  _ -> pure ()
