module Earth.Nix where

import Cli.Extras
import Data.Maybe
import Data.Monoid as M
import Data.Text (Text)
import qualified Data.Text as T
import Earth.Helpers
import Earth.Types

parseStoreUri :: StoreURI -> Text
parseStoreUri p = case p of
  S3StoreURI v -> getFullUri3 v
  HTTPStoreURI v -> getFullUriH v
  SSHStoreURI v -> getFullUriS v
  Daemon -> "daemon"

testSSHStore :: SSHStore
testSSHStore =
  SSHStore
    { _protoS = "ssh-ng://",
      _sshHost = "cidkid@10.0.0.68",
      _sshPort = Nothing
    }

testHTTPSStore :: HTTPStore
testHTTPSStore =
  HTTPStore
    { _protoH = "http://",
      _site = "cache.nixos.org",
      _port = Nothing
    }

testS3Store :: S3Store
testS3Store =
  S3Store
    { _bucket = "testing-cache-2",
      _profileB = Just "cache-upload",
      _region = Just "eu-west-1",
      _scheme = Just "https",
      _endpoint = Just "example.com"
    }

getNixArgs :: NixSettings -> [Text]
getNixArgs settings =
  M.mconcat
    [ concatMap (\x -> ["--option", fst x, snd x]) (getOptions settings),
      getExtraArgs settings,
      ["--store", getStoreRoot settings],
      concatMap (\x -> ["--extra-experimental-features", x]) (getExperimentalFeatures settings),
      if isNothing (getProfile settings) then [] else ["--profile", fromJust (getProfile settings)]
    ]

signClosures :: NixRun e m => NixSettings -> StorePath -> Text -> m Text
signClosures settings path' key = withSpinner ("Signing path " <> fromStorePath path') $ do
  runProcess nixExePath (getNixArgs settings <> ["store", "sign", "-k", key, fromStorePath path'])

copyDeployment :: NixRun e m => StoreURI -> String -> StorePath -> m Text
copyDeployment uri name' outpath = withSpinner ("Copying Deployment for " <> T.pack name') $ do
  runProcess nixExePath ["copy", "-s", "--to", parseStoreUri uri, fromStorePath outpath, "--no-check-sigs"]
