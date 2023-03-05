{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rebuild.Builders
  ( buildSystemConfig,
    switchToConfig,
    runVM,
    installToDir,
    deployConfig,
    addSystem,
    systemBuild,
    regBuild,
  )
where

import Cli.Extras
import Control.Monad.IO.Class
import Data.Monoid as M
import qualified Data.Text as T
import Rebuild.Helpers

addSystem :: NixRun e m => String -> String -> String -> String -> m String
addSystem flakepath name profile typ = do
  let args =
        M.mconcat
          [ commonNixArgs,
            ["build"],
            ["--no-link", "--print-out-paths", "--profile", profile, flakepath <> "#nixosConfigurations." <> name <> ".config.system.build." <> typ]
          ]
  withSpinner ("Building System " <> T.pack name) $ do
    runProcess nixExePath args

buildSystemConfig :: NixRun e m => String -> String -> String -> m String
buildSystemConfig flakepath name typ = do
  let args =
        M.mconcat
          [ commonNixArgs,
            ["build", flakepath <> "#nixosConfigurations." <> name <> ".config.system.build." <> typ],
            ["--no-link", "--print-out-paths"]
          ]
  withSpinner ("Building System " <> T.pack name) $ do
    runProcess nixExePath args

switchToConfig :: NixRun e m => String -> String -> m String
switchToConfig path arg = do
  let path' = filterNixString path

  withSpinner ("Switching to " <> T.pack path') $ do
    runProcess (path' <> "/bin/switch-to-configuration") [arg]

runVM :: NixRun e m => String -> String -> m String
runVM path sys = do
  let path' = filterNixString path
  withSpinner "Running VM.. " $ do
    runProcess (path' <> "/bin/run-" <> sys <> "-vm") []

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir root pass path name = do
  checkForUser 0
  sysbuild <- buildSystemConfig path name "toplevel"
  _ <- copyDeployment root name sysbuild ""
  putLog Informational ("Setting up / -> " <> T.pack root)
  liftIO $ setupDir "/" root

  case pass of
    False -> putLog Warning "Not setting root password"
    _ -> pure ()

  putLog Informational ("Running chroot for " <> T.pack root)
  let sysbuild' = filterNixString sysbuild
  _ <-
    withChroot
      root
      (sysbuild' <> "/sw/bin/bash")
      [ [sysbuild' <> "/bin/switch-to-configuration", "switch"]
      ]
  liftIO $ cleanUpDir root
  pure ()

deployConfig :: NixRun e m => Bool -> String -> String -> String -> String -> String -> m ()
deployConfig doSign path name host port key = do
  sysbuild <- buildSystemConfig path name "toplevel"

  _ <- case doSign of
    True -> signClosures sysbuild key
    False -> return "Nothing"

  _ <- copyDeployment host name sysbuild "ssh-ng://"

  let build' = filterNixString sysbuild

  _ <- withSpinner ("Switching " <> T.pack host <> " to " <> T.pack build') $ do
    runProcessWithSSH port host ["-t"] [build' <> "/bin/switch-to-configuration", "switch"]

  pure ()

-- systemBuild and regBuild are split, because when we switch or boot to a new configuration
-- we have to add it to the system profile, we don't want to do this if we just build
-- the closure or build a VM, since those don't need to be added to the system profile
-- and subsequently the bootloader
--
-- Also we need to pass the profile to systemBuild
systemBuild :: NixRun e m => String -> String -> String -> String -> m ()
systemBuild path name profile arg = case arg of
  "switch" -> do
    checkForUser 0
    sysbuild <- addSystem path name profile "toplevel"
    _ <- switchToConfig sysbuild arg
    pure ()
  "boot" -> do
    checkForUser 0
    sysbuild <- addSystem path name profile "toplevel"
    _ <- switchToConfig sysbuild arg
    pure ()
  _ -> pure ()

regBuild :: NixRun e m => String -> String -> String -> m ()
regBuild path name arg = case arg of
  "build" -> do
    sysbuild <- buildSystemConfig path name "toplevel"
    putLog Informational ("System Closure at " <> T.pack sysbuild)
    pure ()
  "dry-activate" -> do
    sysbuild <- buildSystemConfig path name "toplevel"
    _ <- switchToConfig sysbuild arg
    pure ()
  "vm" -> do
    sysbuild <- buildSystemConfig path name "vm"
    _ <- runVM sysbuild name
    pure ()
  "vm-with-bootloader" -> do
    sysbuild <- buildSystemConfig path name "vmWithBootLoader"
    _ <- runVM sysbuild name
    pure ()
  "build-iso" -> do
    sysbuild <- buildSystemConfig path name "isoImage"
    putLog Informational ("ISO image at " <> T.pack sysbuild)
    pure ()
  _ -> pure ()
