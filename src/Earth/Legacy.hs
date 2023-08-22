module Earth.Legacy where

import Cli.Extras
import Data.Text (Text)

import Earth.Helpers
import Earth.Nix
import Earth.Types

legacySettings :: NixSettings
legacySettings = def { _extraArgs = [ "--no-out-link" ] }

nixLegacyBuild :: NixRun e m => NixSettings -> Legacy -> m Text
nixLegacyBuild config_ legacydef = runProcess nixBuildExe $ unNixDef legacydef <> getNixArgs config_

buildLegacySystem :: NixRun e m => NixSettings -> Legacy -> m StorePath
buildLegacySystem settings legacydef = withSpinner "Build system" $ do
  toStorePath <$> nixLegacyBuild settings legacydef
