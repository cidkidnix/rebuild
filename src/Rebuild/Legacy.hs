module Rebuild.Legacy where

import Cli.Extras
import Data.Text (Text)
import qualified Data.Text as T

import Rebuild.Helpers
import Rebuild.Nix
import Rebuild.Types

legacySettings :: NixSettings
legacySettings = defaultSettings { _extraArgs = [ "--no-out-link" ] }

nixLegacyBuild :: NixRun e m => NixSettings -> LegacyDef -> m Text
nixLegacyBuild config legacydef = runProcess nixBuildExe $ map T.pack (fromLegacyDef legacydef) <> getNixArgs config

buildLegacySystem :: NixRun e m => NixSettings -> LegacyDef -> m StorePath
buildLegacySystem settings legacydef = withSpinner "Build system" $ do
  toStorePath <$> nixLegacyBuild settings legacydef
