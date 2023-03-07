{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rebuild.Darwin (addDarwinSystem, buildDarwinConfig, darwinBuild, regDarwinBuild, installToDir) where

import Cli.Extras
import Data.Monoid as M
import Data.Text (Text)
import qualified Data.Text as T
import Rebuild.Helpers

addDarwinSystem :: NixRun e m => FlakeDef -> String -> String -> m OtherOutput
addDarwinSystem flakedef name profile = do
  let args =
        M.mconcat
          [ commonNixArgs,
            ["build"],
            ["--profile", profile],
            fromFlakeDef flakedef
          ]
  withSpinner ("Building System " <> T.pack name <> " and adding to profile " <> T.pack profile) $ do
    runProcess nixExePath (map T.pack args)

buildDarwinConfig :: NixRun e m => FlakeDef -> Text -> m StorePath
buildDarwinConfig flakedef name = do
  let args =
        M.mconcat
          [ commonNixArgs,
            ["build"],
            fromFlakeDef flakedef,
            ["--no-link", "--print-out-paths"]
          ]
  withSpinner ("Building System " <> name) $ do
    runProcess nixExePath (map T.pack args)

switchToConfig :: NixRun e m => StorePath -> m OtherOutput
switchToConfig path = do
  withSpinner ("Switching to " <> fromStorePath path) $ do
    runProcess (toFilePath path <> "/activate") []

darwinBuild :: NixRun e m => String -> String -> String -> String -> m ()
darwinBuild path name profile arg = case arg of
  "switch" -> do
    checkForUser 0
    sysbuild <- addDarwinSystem (nixDarwinBuildargs path name "toplevel") name profile
    _ <- switchToConfig sysbuild
    pure ()
  _ -> pure ()

regDarwinBuild :: NixRun e m => String -> String -> String -> m ()
regDarwinBuild path name arg = case arg of
  "build" -> do
    sysbuild <- buildDarwinConfig (nixDarwinBuildargs path name "toplevel") (T.pack name)
    putLog Informational ("System Closure at " <> fromStorePath sysbuild)
    pure ()
  _ -> pure ()

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir _ _ _ _ = do
  pure ()
