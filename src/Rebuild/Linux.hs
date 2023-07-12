module Rebuild.Linux
  ( setupDir,
    cleanUpDir,
    withChroot,
    runChroot,
  )
where

import Data.Monoid as M
import Data.Text (Text)
import qualified Data.Text as T
import Rebuild.Helpers
import Rebuild.Types
import System.Directory
import System.FilePath
import System.Linux.Mount

withChroot :: NixRun e m => FilePath -> FilePath -> [[String]] -> m ()
withChroot path_ sh = mapM_ (runChroot path_ sh)

runChroot :: NixRun e m => String -> FilePath -> [String] -> m Text
runChroot path_ sh com_ = do
  let command = unwords com_
      args' =
        M.mconcat
          [ [path_, sh, "-c", command]
          ]
  runProcess "chroot" (map T.pack args')

setupDir :: FilePath -> FilePath -> IO ()
setupDir root mountpoint = do
  createDirectoryIfMissing True (mountpoint </> "etc")
  writeFile (mountpoint </> "etc/NIXOS") ""
  createDirectoryIfMissing True (mountpoint </> "dev")
  createDirectoryIfMissing True (mountpoint </> "sys")
  rBind (root <> "dev") (mountpoint </> "dev")
  rBind (root <> "sys") (mountpoint </> "sys")

cleanUpDir :: FilePath -> IO ()
cleanUpDir mountpoint = do
  umountWith Detach Follow (mountpoint <> "sys")
  umountWith Detach Follow (mountpoint <> "dev")
