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


parseSwitchCommand :: SwitchCommand -> Text
parseSwitchCommand a = case a of
  B -> "boot"
  S -> "switch"
  D -> "dry-activate"

switchToConfig :: NixRun e m => StorePath -> SwitchCommand -> m Text
switchToConfig path' arg = do
  withSpinner ("Switching to " <> fromStorePath path') $ do
    runProcess (toFilePath path' <> "/bin/switch-to-configuration") [parseSwitchCommand arg]

runVM :: NixRun e m => StorePath -> String -> m Text
runVM path' sys = do
  withSpinner "Running VM.. " $ do
    runProcess (toFilePath path' <> "/bin/run-" <> sys <> "-vm") []


legacyBuild :: NixRun e m => Options -> m ()
legacyBuild opts = case com opts of
  Build -> do
    sysbuild <- buildLegacySystem legacySettings (legacyNixOSBuildArgs (path opts) "toplevel")
    putLog Informational ("System Closure at " <> fromStorePath sysbuild)
  Switch profile -> do
    checkForUser 0
    sysbuild <- buildLegacySystem (legacySettings {_profile = Just $ T.pack profile }) (legacyNixOSBuildArgs (path opts) "toplevel")
    void $ switchToConfig sysbuild S
  Boot profile -> do
    checkForUser 0
    sysbuild <- buildLegacySystem (legacySettings {_profile = Just $ T.pack profile }) (legacyNixOSBuildArgs (path opts) "toplevel")
    void $ switchToConfig sysbuild B
  _ -> failWith "Not implemented"

flakeBuild :: NixRun e m => Options -> m ()
flakeBuild opts = case com opts of
  Build -> do
    sysbuild <- buildFlakeSystem def (nixOSBuildargs (path opts) (name opts) "toplevel")
    putLog Informational ("System Closure at " <> fromStorePath sysbuild)
  DryActivate -> do
    sysbuild <- buildFlakeSystem def (nixOSBuildargs (path opts) (name opts) "toplevel")
    void $ switchToConfig sysbuild D
  VM -> do
    sysbuild <- buildFlakeSystem def (nixOSBuildargs (path opts) (name opts) "vm")
    void $ runVM sysbuild (name opts)
  VMWithBootLoader -> do
    sysbuild <- buildFlakeSystem def (nixOSBuildargs (path opts) (name opts) "vmWithBootLoader")
    void $ runVM sysbuild (name opts)
  BuildISO -> do
    sysbuild <- buildFlakeSystem def (nixOSBuildargs (path opts) (name opts) "isoImage")
    putLog Informational ("ISO image at " <> fromStorePath sysbuild)
  Switch profile -> do
    checkForUser 0
    sysbuild <- buildFlakeSystem (def {_profile = Just (T.pack profile)}) (nixOSBuildargs (path opts) (name opts) "toplevel")
    void $ switchToConfig sysbuild S
  Boot profile -> do
    checkForUser 0
    sysbuild <- buildFlakeSystem (def {_profile = Just (T.pack profile)}) (nixOSBuildargs (path opts) (name opts) "toplevel")
    void $ switchToConfig sysbuild B
  GC f t dryRun olderThan -> do
    a <- case textToMaybeInteger (T.pack olderThan) of
                      Nothing -> failWith "Need to be <num>d"
                      Just a -> pure a
    let olderThanNum = timeToSeconds a
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

  _ -> pure ()
