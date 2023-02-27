{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Rebuild.Helpers (checkForUser,
                        nixExePath,
                        commonNixArgs,
                        runProcess,
                        runChroot,
                        nixRun,
                        runProcessWithSSH,
                        copyDeployment,
                        signClosures,
                        setupDir,
                        cleanUpDir,
                        withChroot,
                        filterNixString,
                        NixRun
                       ) where
import Data.Monoid as M
import System.Which
import System.Posix.User
import System.Posix.Types
import System.Exit
import Control.Lens
import Cli.Extras
import Control.Monad.IO.Class
import Control.Monad.Catch
import qualified Data.Text as T
import Data.List as L
import System.Linux.Mount
import System.Directory

data NixError =
    P ProcessFailure | T T.Text

instance Show NixError where
    show = \case
        P pf -> show pf
        T t -> T.unpack t

makePrisms ''NixError

instance AsUnstructuredError NixError where
    asUnstructuredError = _T

instance AsProcessFailure NixError where
    asProcessFailure = _P

type NixRun e m =
    ( AsUnstructuredError e
    , MonadFail m
    , AsProcessFailure e
    , CliThrow e m
    , HasCliConfig e m
    , MonadMask m
    , MonadIO m
    , CliLog m )

nixRun :: Severity -> CliT NixError IO a -> IO a
nixRun severity action = do
    cfg <- newCliConfig severity False False (\t -> (T.pack . show $ t, ExitFailure 1))
    runCli cfg action

nixExePath :: FilePath
nixExePath = $(staticWhich "nix")

sshExePath :: FilePath
sshExePath = $(staticWhich "ssh")

commonNixArgs :: [ String ]
commonNixArgs = M.mconcat [
    [ "--option", "sandbox", "true" ]
                  ]

filterNixString :: String -> String
filterNixString a = do
    let a' = (filter (/='"')) (a)
    (filter (/='\n')) (a')

runProcess :: NixRun e m => FilePath -> [ String ] -> m String
runProcess com args = do
    let args' = map (filterNixString) (args)
        cmd = com
    fmap T.unpack $ readProcessAndLogOutput (Debug, Debug) (proc cmd args')

runProcessWithSSH :: NixRun e m => String -> String -> [ String ] -> [ String ] -> m String
runProcessWithSSH port host sargs com = do
    let sargs' = map (filterNixString) (sargs)
        pargs' = L.intercalate " " com
        cmd = sshExePath
        args' = M.mconcat [
                    sargs',
                    [ host, "-p", port ]
                   , [ pargs' ]
                ]
    fmap T.unpack $ readProcessAndLogOutput (Debug, Debug) (proc cmd args')

withChroot :: NixRun e m => FilePath -> FilePath -> [[ String ]] -> m ()
withChroot path sh com = mapM_ (\x -> runChroot path sh x) com

signClosures :: NixRun e m => String -> String -> m String
signClosures path key = do
    let outpath' = (filterNixString path)
    withSpinner ("Signing path " <> (T.pack path)) $ do
        runProcess nixExePath [ "store", "sign", "-k", key, outpath' ]

copyDeployment :: NixRun e m => String -> String -> String -> String -> m String
copyDeployment host name outpath uri = do
    let outpath' = (filterNixString outpath)
        nixhost = uri <> host
    withSpinner ("Copying Deployment for " <> (T.pack name)) $ do
        runProcess nixExePath [ "copy", "-s", "--to", nixhost, outpath', "--no-check-sigs" ]

runChroot :: NixRun e m => String -> FilePath -> [ String ] -> m String
runChroot path sh com = do
    let command = L.intercalate " " com
        args' = M.mconcat [
                [ path, sh, "-c", command ]
                          ]
    runProcess "chroot" args'

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

checkForUser :: NixRun e m => CUid -> m ()
checkForUser a = do
    root <- fmap (== a) (liftIO $ getRealUserID)
    case root of
      False -> do
          failWith "Not Root, Exiting"
      _ -> pure ()
