{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers (checkForUser,
                        nixExePath,
                        commonNixArgs,
                        runProcess,
                        buildSystemConfig,
                        switchToConfig,
                        runVM,
                        nixRun,
                        NixRun
                       ) where
import Data.Monoid as M
import System.Which
import System.Posix.User
import System.Posix.Types
import System.Exit
import Control.Lens
import Cli.Extras
import Cli.Extras.Logging
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Fail
import qualified Data.Text as T

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

commonNixArgs :: [ String ]
commonNixArgs = M.mconcat [
    [ "--option", "sandbox", "true" ]
                  ]

runProcess :: NixRun e m => FilePath -> [ String ] -> m String
runProcess com args = do
    let args' = map (filter (/='"')) (args)
        cmd = com
    fmap T.unpack $ readProcessAndLogOutput (Debug, Debug) (proc cmd args')

buildSystemConfig :: NixRun e m => String -> String -> String -> m String
buildSystemConfig flakepath name typ = do
    let args = M.mconcat [
                commonNixArgs,
                [ "build", (flakepath <> "#nixosConfigurations." <> name <> ".config.system.build." <> typ) ],
                [ "--no-link", "--print-out-paths" ]
                ]
    withSpinner ("Building System " <> (T.pack name)) $ do
        runProcess (nixExePath) args

switchToConfig :: NixRun e m => String -> String -> m String
switchToConfig path arg = do
    let path' = ((filter (/='\n')) path)
    withSpinner ("Switching to " <> (T.pack path')) $ do
        runProcess (path' <> "/bin/switch-to-configuration") [ arg ]

runVM :: NixRun e m => String -> String -> m String
runVM path sys = do
    let path' = ((filter (/='\n')) path)
    withSpinner ("Running VM.. ") $ do
        runProcess (path' <> "/bin/run-" <> sys <> "-vm") [ ]

checkForUser :: NixRun e m => CUid -> m ()
checkForUser a = do
    root <- fmap (== a) (liftIO $ getRealUserID)
    case root of
      False -> do
          failWith "Not Root, Exiting"
      _ -> pure ()
