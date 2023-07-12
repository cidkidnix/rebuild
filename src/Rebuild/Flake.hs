module Rebuild.Flake where

import Cli.Extras
import Rebuild.Nix
import Rebuild.Types

buildFlakeSystem :: NixRun e m => NixSettings -> FlakeDef -> m StorePath
buildFlakeSystem settings flakedef = withSpinner "Building system" $ do
  toStorePath <$> nixBuild settings flakedef
