module Earth.Install (installToDir) where

import Cli.Extras
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.FilePath

import Earth.Flake
import Earth.Helpers
import Earth.Linux
import Earth.Types

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir root pass path' name' = do
  checkForUser 0

  -- Build and install to system profile in mounted system
  sysbuild <- installBuild (nixOSBuildargs name' path' "toplevel") (def {_profile = Just $ T.pack $ root </> "nix/var/nix/profiles/system"})

  -- Prepare Chroot
  putLog Informational ("Setting up / -> " <> T.pack root)
  liftIO $ setupDir "/" root

  when pass $ putLog Warning "Not setting root password"

  putLog Informational ("Running chroot for " <> T.pack root)
  -- Run switch with NIXOS_INSTALL_BOOTLOADER so we properly install the bootloader
  void $ withChroot
      root
      (toFilePath sysbuild </> "sw/bin/bash")
      [ ["NIXOS_INSTALL_BOOTLOADER=1 " <> T.unpack (fromStorePath sysbuild) </> "bin/switch-to-configuration", "boot"]
      ]

  -- Attempt cleanup of mountpoint
  liftIO $ cleanUpDir root

installBuild :: NixRun e m => Flake -> NixSettings -> m StorePath
installBuild flakedef settings = withSpinner "Installing profile ..." $ do
  buildFlakeSystem settings flakedef
