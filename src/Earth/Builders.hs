module Earth.Builders where

import Cli.Extras
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

import Earth.Flake
import Earth.Helpers
import Earth.GC
import Earth.Legacy
import Earth.Types


parseSwitchCommand :: Command -> Text
parseSwitchCommand a = case a of
  Boot _ -> "boot"
  Switch _ -> "switch"
  DryActivate -> "dry-activate"
  _ -> error "Earth.Builders:parseSwitchCommand: not supported!"

switchToConfig :: NixRun e m => StorePath -> Command -> m Text
switchToConfig path' arg = do
  withSpinner ("Switching to " <> fromStorePath path') $ do
    runProcess (toFilePath path' <> "/bin/switch-to-configuration") [parseSwitchCommand arg]

runVM :: NixRun e m => StorePath -> String -> m Text
runVM path' sys = do
  withSpinner "Running VM.. " $ do
    runProcess (toFilePath path' <> "/bin/run-" <> sys <> "-vm") []

legacyBuild :: NixRun e m => Options -> m ()
legacyBuild opts = stub

flakeBuild :: NixRun e m => Options -> m ()
flakeBuild opts = case com opts of
    GC f t dryRun olderThan -> do
      time <- case textToMaybeInteger (T.pack olderThan) of
                        Nothing -> failWith "Need to be <num>d"
                        Just a -> pure a
      let olderThanNum = timeToSeconds time
      unless dryRun $
          checkForUser 0

      when (f /= 0 && t /= 0 && olderThanNum == 0) $ do
          filepaths <- collectSystems f t
          runSystemGarbageCollect dryRun filepaths

      when (olderThanNum /= 0) $ do
          fps <- timeCollectSystems $ T.pack olderThan
          case fps of
            [] -> do putLog Alert "Nothing to do!"
            _ -> runSystemGarbageCollect dryRun fps
    NixOSInstall _ _ -> stub
    buildRequired -> do
      let buildCmd = toFlakeBuildArgs opts
          profileArgs = toFlakeProfileArgs (com opts)
      build <- buildFlakeSystem profileArgs buildCmd
      case buildRequired of
        Build -> putLog Informational ("System Closure at " <> fromStorePath build)
        VM -> void $ runVM build (name opts)
        VMWithBootLoader -> void $ runVM build (name opts)
        BuildISO -> putLog Informational ("ISO image at " <> fromStorePath build)
        _ -> void $ switchToConfig build (com opts)
