{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rebuild.Darwin where

import Cli.Extras
import Data.Monoid as M (Monoid (mconcat))
import Data.Text (Text)
import qualified Data.Text as T
import Rebuild.Helpers
import Rebuild.Nix

buildDarwinSystem :: NixRun e m => NixSettings -> FlakeDef -> Maybe Text -> m Text
buildDarwinSystem settings flakedef profile = do
  withSpinner ("Building system") $ do
    nixBuild (settings {_profile = profile}) flakedef

switchToConfig :: NixRun e m => Text -> m Text
switchToConfig path = do
  withSpinner ("Switching to " <> fromStorePath path) $ do
    runProcess (toFilePath path <> "/activate") []

darwinBuild :: NixRun e m => String -> String -> String -> String -> m ()
darwinBuild path name profile arg = case arg of
  "switch" -> do
    checkForUser 0
    sysbuild <- buildDarwinSystem defaultSettings (nixDarwinBuildargs path name "toplevel") (Just (T.pack profile))
    _ <- switchToConfig sysbuild
    pure ()
  _ -> pure ()

regDarwinBuild :: NixRun e m => String -> String -> String -> m ()
regDarwinBuild path name arg = case arg of
  "build" -> do
    sysbuild <- buildDarwinSystem defaultSettings (nixDarwinBuildargs path name "toplevel") Nothing
    putLog Informational ("System Closure at " <> fromStorePath sysbuild)
    pure ()
  _ -> pure ()

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir _ _ _ _ = failWith "Not available on darwin!"
