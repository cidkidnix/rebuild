{-# LANGUAGE DeriveGeneric #-}
module Earth.Deploy (deployConfig) where

import Cli.Extras
import Control.Monad
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import Earth.Helpers
import Earth.Nix
import Earth.Types
import Earth.Flake

data DeployConfig = DeployConfig
    { _deployConfig_systems :: [SystemConfig]
    , _deployConfig_hostSystem :: String
    } deriving (Show, Eq, Ord, Generic)

data SystemConfig = SystemConfig
    { _systemConfig_name :: Text
    , _systemConfig_system :: Text
    , _systemConfig_path :: FilePath
    , _systemConfig_ip :: Maybe Text
    } deriving (Show, Eq, Ord, Generic)

instance ToJSON SystemConfig
instance FromJSON SystemConfig

instance ToJSON DeployConfig
instance FromJSON DeployConfig

-- Register remotely
-- Hopefully can remove if nix ever supports remote profile installs
installProfileRemote :: NixRun e m => String -> StorePath -> m Text
installProfileRemote host outpath = do
  withSpinner ("Installing system to" <> T.pack host) $ do
    runProcessWithSSH "22" host ["-t"] ["nix profile install ", fromStorePath outpath, " --profile /nix/var/nix/profiles/test"]

deployConfig :: NixRun e m => Bool -> String -> String -> String -> String -> String -> m ()
deployConfig doSign path' name' host port key = do
  let sshURI =
        SSHStore
          { _protoS = "ssh-ng://",
            _sshHost = T.pack host,
            _sshPort = Nothing
          }
  build <- buildFlakeSystem def (nixOSBuildargs path' name' "toplevel")
  void $ copyDeployment (SSHStoreURI sshURI) name' build
  void $ installProfileRemote host build

  when doSign $ void $
    signClosures def build (T.pack key)

  void $ withSpinner ("Switching " <> T.pack host <> " to " <> fromStorePath build) $ do
    runProcessWithSSH port host ["-t"] [fromStorePath build <> "/bin/switch-to-configuration", "switch"]
