{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rebuild.Deploy (deployConfig) where

import Cli.Extras
import qualified Data.Text as T
import Rebuild.Builders
import Rebuild.Helpers
import Rebuild.Nix

-- Register remotely
-- Hopefully can remove if nix ever supports remote profile installs
installProfileRemote :: NixRun e m => String -> StorePath -> m OtherOutput
installProfileRemote host outpath = do
  withSpinner ("Installing system to" <> T.pack host) $ do
    runProcessWithSSH "22" host ["-t"] ["nix profile install ", fromStorePath outpath, " --profile /nix/var/nix/profiles/test"]

deployConfig :: NixRun e m => Bool -> String -> String -> String -> String -> String -> m ()
deployConfig doSign path name host port key = do
  let sshURI =
        SSHStore
          { _protoS = "ssh-ng://",
            _sshHost = T.pack host,
            _sshPort = Nothing
          }
  build <- buildSystem defaultSettings (nixOSBuildargs path name "toplevel")
  _ <- copyDeployment (SSHStoreURI sshURI) name build
  _ <- installProfileRemote host build

  _ <- case doSign of
    True -> signClosures defaultSettings build (T.pack key)
    False -> return "Nothing"

  _ <- withSpinner ("Switching " <> T.pack host <> " to " <> fromStorePath build) $ do
    runProcessWithSSH port host ["-t"] [fromStorePath build <> "/bin/switch-to-configuration", "switch"]
  pure ()
