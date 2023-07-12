module Rebuild.GC where

import Cli.Extras
import Control.Monad
import Control.Monad.IO.Class
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time.Clock
import System.Directory
import System.FilePath

import Rebuild.Helpers
import Rebuild.Types

data SystemLink = GarbageCollect FilePath
                 | NoCollect FilePath Reason
    deriving (Show, Eq)

data Reason = InvalidPath
            | InUse
    deriving (Show, Eq)

collectSystems :: NixRun e m => Int -> Int -> m [SystemLink]
collectSystems a b = liftIO $ do
    forM [a..b] $ \x -> do
        let profilePath = "/nix/var/nix/profiles" </> "system-" <> show x <> "-link"
        checkSystems profilePath


checkSystems :: FilePath -> IO SystemLink
checkSystems fp = do
    exists <- doesPathExist fp
    currentSystem <- getSymbolicLinkTarget "/run/current-system"
    bootedSystem <- getSymbolicLinkTarget "/run/booted-system"
    if not exists
       then pure $ NoCollect fp InvalidPath
       else do
         symlink <- getSymbolicLinkTarget fp
         if symlink == currentSystem || symlink == bootedSystem then
           pure $ NoCollect fp InUse
         else
           pure $ GarbageCollect fp

runSystemGarbageCollect :: NixRun e m => Bool -> [SystemLink] -> m ()
runSystemGarbageCollect dryRun fp = do
    when dryRun $ putLog Critical $ T.pack "Dry Run!"
    forM_ fp $ \case
      GarbageCollect f -> do
          unless dryRun $ liftIO $ removeFile f
          putLog Notice $ T.pack $ "Removing: " <> f
      NoCollect f r -> case r of
                          InvalidPath -> pure ()
                          InUse -> putLog Alert $ "Skipping: " <> T.pack f
    unless dryRun $ withSpinner "Running nix-collect-garbage" $ do
        _ <- runProcess nixCollectGarbage []
        pure ()

timeCollectSystems :: NixRun e m => NominalDiffTime -> m [SystemLink]
timeCollectSystems tt = do
    paths <- liftIO $ listDirectory "/nix/var/nix/profiles"
    let links = L.sort $ filter (\x -> "system-" `T.isPrefixOf` x) (map T.pack paths)
    currentTime <- liftIO getCurrentTime
    t <- forM links $ \x -> do
        time <- liftIO $ getAccessTime $ "/nix/var/nix/profiles" </> T.unpack x
        pure ("/nix/var/nix/profiles" </> T.unpack x, diffUTCTime currentTime time)
    forM (filter (\(_, time) -> time >= nominalDay * tt) t) $ \(file, _) -> do
        putLog Alert $ T.pack $ "Link " <> file <> " is older than " <> case timeInDays of
                                                                          1 -> "1 day"
                                                                          _ -> show timeInDays <> " days"
        liftIO $ checkSystems file

  where
    timeInDays :: Int
    timeInDays = nominalDiffTimeToInt (nominalDay / nominalDay * tt)

    nominalDiffTimeToInt :: NominalDiffTime -> Int
    nominalDiffTimeToInt x = do
        let (time, _) = properFraction x
        time



