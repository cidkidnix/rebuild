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
  )
where

import Cli.Extras
import Control.Monad.IO.Class
import Data.Monoid as M
import qualified Data.Text as T
import Rebuild.Helpers

addSystem :: NixRun e m => String -> String -> String -> m String
addSystem flakepath name typ = do
  let args =
        M.mconcat
          [ commonNixArgs,
            ["build"],
            ["--no-link", "--print-out-paths", "--profile", "/nix/var/nix/profiles/system", flakepath <> "#nixosConfigurations." <> name <> ".config.system.build." <> typ]
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
