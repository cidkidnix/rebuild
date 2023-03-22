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
import Rebuild.Nix

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir root pass path name = do
  checkForUser 0

  -- Build and install to system profile in mounted system
  sysbuild <- installBuild (nixOSBuildargs name path "toplevel") (T.pack root) "/nix/var/nix/profiles/system"

  -- Prepare Chroot
  putLog Informational ("Setting up / -> " <> T.pack root)
  liftIO $ setupDir "/" root

  case pass of
    False -> putLog Warning "Not setting root password"
    _ -> pure ()

  putLog Informational ("Running chroot for " <> T.pack root)
  -- Run switch with NIXOS_INSTALL_BOOTLOADER so we properly install the bootloader
  _ <-
    withChroot
      root
      (toFilePath sysbuild <> "/sw/bin/bash")
      [ ["NIXOS_INSTALL_BOOTLOADER=1 " <> T.unpack (fromStorePath sysbuild) <> "/bin/switch-to-configuration", "boot"]
      ]

  -- Attempt cleanup of mountpoint
  liftIO $ cleanUpDir root
  pure ()

installBuild :: NixRun e m => FlakeDef -> T.Text -> T.Text -> m StorePath
installBuild flakedef mountpoint profile = withSpinner "Installing profile ..." $ do
  buildSystem defaultSettings flakedef (Just (mountpoint <> profile))
