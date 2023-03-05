{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rebuild.Darwin (addDarwinSystem, buildDarwinConfig, darwinBuild, regDarwinBuild, installToDir) where

import Cli.Extras
import Data.Monoid as M
import qualified Data.Text as T
import Rebuild.Helpers

addDarwinSystem :: NixRun e m => String -> String -> String -> String -> m String
addDarwinSystem flakepath name profile typ = do
  let args =
        M.mconcat
          [ commonNixArgs,
            ["build"],
            nixDarwinBuildargs flakepath name typ profile
          ]
  withSpinner ("Building System " <> T.pack name <> " and adding to profile " <> T.pack profile) $ do
    runProcess nixExePath args

buildDarwinConfig :: NixRun e m => String -> String -> String -> m String
buildDarwinConfig flakepath name typ = do
  let args =
        M.mconcat
          [ commonNixArgs,
            ["build", flakepath <> "#darwinConfigurations." <> name <> ".config.system.build." <> typ],
            ["--no-link", "--print-out-paths"]
          ]
  withSpinner ("Building System " <> T.pack name) $ do
    runProcess nixExePath args

switchToConfig :: NixRun e m => String -> m String
switchToConfig path = do
  let path' = filterNixString path

  withSpinner ("Switching to " <> T.pack path') $ do
    _ <- runProcess (path' <> "/activate") []
    runProcess (path' <> "/activae-user") []

darwinBuild :: NixRun e m => String -> String -> String -> String -> m ()
darwinBuild path name profile arg = case arg of
  "switch" -> do
    checkForUser 0
    sysbuild <- addDarwinSystem path name profile "toplevel"
    _ <- switchToConfig sysbuild
    pure ()
  _ -> pure ()

regDarwinBuild :: NixRun e m => String -> String -> String -> m ()
regDarwinBuild path name arg = case arg of
  "build" -> do
    sysbuild <- buildDarwinConfig path name "toplevel"
    putLog Informational ("System Closure at " <> T.pack sysbuild)
    pure ()
  _ -> pure ()

installToDir :: NixRun e m => String -> Bool -> String -> String -> m ()
installToDir _ _ _ _ = do
  pure ()
