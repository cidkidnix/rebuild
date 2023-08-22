module Earth.Flake where

import Cli.Extras
import Data.Text (Text)

import Earth.Helpers
import Earth.Nix
import Earth.Types

buildFlakeSystem :: NixRun e m => NixSettings -> Flake -> m StorePath
buildFlakeSystem settings flakedef = withSpinner ("Building " <> _name' flakedef)  $ do
  toStorePath <$> nixBuild settings flakedef

nixBuild :: NixRun e m => NixSettings -> Flake -> m Text
nixBuild config_ flakedef = runProcess nixExePath (["build"] <> getNixArgs config_ <> unNixDef flakedef)
