{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rebuild.Deploy (deployConfig) where

import Cli.Extras
import qualified Data.Text as T
import Rebuild.Builders
import Rebuild.Helpers

-- Register remotely
installProfileRemote :: NixRun e m => String -> String -> m String
installProfileRemote host outpath = do
  withSpinner ("Installing system to" <> T.pack host) $ do
    runProcessWithSSH "22" host ["-t"] ["nix profile install ", outpath, " --profile /nix/var/nix/profiles/test"]

deployConfig :: NixRun e m => Bool -> String -> String -> String -> String -> String -> m ()
deployConfig doSign path name host port key = do
  build <- buildSystemConfig path name "toplevel"
  _ <- copyDeployment host name (filterNixString build) "ssh-ng://"
  _ <- installProfileRemote host (filterNixString build)

  _ <- case doSign of
    True -> signClosures build key
    False -> return "Nothing"

  _ <- withSpinner ("Switching " <> T.pack host <> " to " <> T.pack (filterNixString build)) $ do
    runProcessWithSSH port host ["-t"] [filterNixString build <> "/bin/switch-to-configuration", "switch"]

  pure ()
