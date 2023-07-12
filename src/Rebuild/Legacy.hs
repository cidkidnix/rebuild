module Rebuild.Legacy where

import Rebuild.Types

buildLegacySystem :: NixRun e m => NixSettings -> String -> m StorePath
buildLegacySystem _ _ = do mempty
