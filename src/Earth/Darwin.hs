module Earth.Darwin where

import Cli.Extras
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
import Earth.Helpers
import Earth.Types
import Earth.Flake

buildDarwinSystem :: NixRun e m => NixSettings -> Flake -> m Text
buildDarwinSystem settings flakedef = do
  withSpinner "Building system" $ do
    nixBuild settings flakedef

switchToConfig :: NixRun e m => Text -> m Text
switchToConfig path' = do
  withSpinner ("Switching to " <> fromStorePath path') $ do
    runProcess (toFilePath path' </> "activate") []

darwinBuild :: NixRun e m => String -> String -> String -> String -> m ()
darwinBuild path' name' profile arg = case arg of
  "switch" -> do
    checkForUser 0
    sysbuild <- buildDarwinSystem (def {_profile = Just (T.pack profile)}) (nixDarwinBuildargs path' name' "toplevel")
    void $ switchToConfig sysbuild
  _ -> pure ()

regDarwinBuild :: NixRun e m => String -> String -> String -> m ()
regDarwinBuild path' name' arg = case arg of
  "build" -> do
    sysbuild <- buildDarwinSystem def (nixDarwinBuildargs path' name' "toplevel")
    putLog Informational ("System Closure at " <> fromStorePath sysbuild)
  _ -> pure ()

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir _ _ _ _ = failWith "Not available on darwin!"
