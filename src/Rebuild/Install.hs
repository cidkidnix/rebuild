{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rebuild.Install (installToDir) where

import Cli.Extras
import Control.Monad.IO.Class
import Data.Monoid as M
import qualified Data.Text as T
import Rebuild.Helpers
import Rebuild.Linux

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir root pass path name = do
  checkForUser 0

  -- Build and install to system profile in mounted system
  sysbuild <- installBuild path name root "/nix/var/nix/profiles/system"

  -- Prepare Chroot
  putLog Informational ("Setting up / -> " <> T.pack root)
  liftIO $ setupDir "/" root

  case pass of
    False -> putLog Warning "Not setting root password"
    _ -> pure ()

  putLog Informational ("Running chroot for " <> T.pack root)
  let sysbuild' = filterNixString sysbuild

  -- Run switch with NIXOS_INSTALL_BOOTLOADER so we properly install the bootloader
  _ <-
    withChroot
      root
      (sysbuild' <> "/sw/bin/bash")
      [ ["NIXOS_INSTALL_BOOTLOADER=1 " <> sysbuild' <> "/bin/switch-to-configuration", "boot"]
      ]

  -- Attempt cleanup of mountpoint
  liftIO $ cleanUpDir root
  pure ()

installBuild :: NixRun e m => String -> String -> String -> String -> m String
installBuild flakepath name mountpoint profile = do
  let args =
        M.mconcat
          [ commonNixArgs,
            ["build", "--no-link", "--print-out-paths"],
            ["--profile", mountpoint <> profile],
            nixOSBuildargs flakepath name "toplevel" profile
          ]
  withSpinner "Installing profile ..." $ do
    runProcess nixExePath args
