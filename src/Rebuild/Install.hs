module Rebuild.Install (installToDir) where

import Cli.Extras
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.FilePath

import Rebuild.Flake
import Rebuild.Helpers
import Rebuild.Linux
import Rebuild.Nix
import Rebuild.Types

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir root pass path' name' = do
  checkForUser 0

  -- Build and install to system profile in mounted system
  sysbuild <- installBuild (nixOSBuildargs name' path' "toplevel") (defaultSettings {_profile = Just $ T.pack $ root </> "nix/var/nix/profiles/system"})

  -- Prepare Chroot
  putLog Informational ("Setting up / -> " <> T.pack root)
  liftIO $ setupDir "/" root

  if pass
     then pure ()
     else putLog Warning "Not setting root password"

  putLog Informational ("Running chroot for " <> T.pack root)
  -- Run switch with NIXOS_INSTALL_BOOTLOADER so we properly install the bootloader
  _ <-
    withChroot
      root
      (toFilePath sysbuild </> "sw/bin/bash")
      [ ["NIXOS_INSTALL_BOOTLOADER=1 " <> T.unpack (fromStorePath sysbuild) </> "bin/switch-to-configuration", "boot"]
      ]

  -- Attempt cleanup of mountpoint
  liftIO $ cleanUpDir root
  pure ()

installBuild :: NixRun e m => FlakeDef -> NixSettings -> m StorePath
installBuild flakedef settings = withSpinner "Installing profile ..." $ do
  buildFlakeSystem settings flakedef
