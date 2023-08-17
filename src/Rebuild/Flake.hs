module Rebuild.Flake where

import Cli.Extras
import Data.Text (Text)
import qualified Data.Text as T

import Rebuild.Helpers
import Rebuild.Nix
import Rebuild.Types

buildFlakeSystem :: NixRun e m => NixSettings -> FlakeDef -> m StorePath
buildFlakeSystem settings flakedef = withSpinner "Building system" $ do
  toStorePath <$> nixBuild settings flakedef

nixBuild :: NixRun e m => NixSettings -> FlakeDef -> m Text
nixBuild config flakedef = runProcess nixExePath (["build"] <> getNixArgs config <> map T.pack (fromFlakeDef flakedef))
