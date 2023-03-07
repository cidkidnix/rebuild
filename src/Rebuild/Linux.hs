{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import System.Directory
import System.Linux.Mount

withChroot :: NixRun e m => FilePath -> FilePath -> [[String]] -> m ()
withChroot path sh com = mapM_ (\x -> runChroot path sh x) com

runChroot :: NixRun e m => String -> FilePath -> [String] -> m OtherOutput
runChroot path sh com = do
  let command = unwords com
      args' =
        M.mconcat
          [ [path, sh, "-c", command]
          ]
  runProcess "chroot" (map T.pack args')

setupDir :: FilePath -> FilePath -> IO ()
setupDir root mountpoint = do
  createDirectoryIfMissing True (mountpoint <> "/" <> "etc")
  writeFile (mountpoint <> "/etc/NIXOS") ""
  createDirectoryIfMissing True (mountpoint <> "/" <> "dev")
  createDirectoryIfMissing True (mountpoint <> "/" <> "sys")
  rBind (root <> "dev") (mountpoint <> "/" <> "dev")
  rBind (root <> "sys") (mountpoint <> "/" <> "sys")

cleanUpDir :: FilePath -> IO ()
cleanUpDir mountpoint = do
  umountWith Detach Follow (mountpoint <> "sys")
  umountWith Detach Follow (mountpoint <> "dev")
