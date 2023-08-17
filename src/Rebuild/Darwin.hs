module Rebuild.Darwin where

import Cli.Extras
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
import Rebuild.Helpers
import Rebuild.Nix
import Rebuild.Types
import Rebuild.Flake

buildDarwinSystem :: NixRun e m => NixSettings -> FlakeDef -> m Text
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
    sysbuild <- buildDarwinSystem (defaultSettings {_profile = Just (T.pack profile)}) (nixDarwinBuildargs path' name' "toplevel")
    _ <- switchToConfig sysbuild
    pure ()
  _ -> pure ()

regDarwinBuild :: NixRun e m => String -> String -> String -> m ()
regDarwinBuild path' name' arg = case arg of
  "build" -> do
    sysbuild <- buildDarwinSystem defaultSettings (nixDarwinBuildargs path' name' "toplevel")
    putLog Informational ("System Closure at " <> fromStorePath sysbuild)
    pure ()
  _ -> pure ()

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir _ _ _ _ = failWith "Not available on darwin!"
