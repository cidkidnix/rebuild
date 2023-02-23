{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helpers (checkForUser,
                        nixExePath,
                        commonNixArgs,
                        runProcess,
                        buildSystemConfig,
                        switchToConfig,
                        runVM
                       ) where
import System.Process hiding (runProcess)
import Data.Monoid as M
import System.Which
import System.Posix.User
import System.Posix.Types
import System.Exit

nixExePath :: FilePath
nixExePath = $(staticWhich "nix")

commonNixArgs :: [ String ]
commonNixArgs = M.mconcat [
    [ "--option", "sandbox", "true" ]
                  ]
runProcess :: FilePath -> [ String ] -> IO String
runProcess com args = do
    let args' = map (filter (/='"')) (args)
        cmd = com
    readCreateProcess ((proc cmd args')) ""

buildSystemConfig :: String -> String -> String -> IO String
buildSystemConfig flakepath name typ = do
    let args = M.mconcat [
                commonNixArgs,
                [ "build", (flakepath ++ "#nixosConfigurations." ++ name ++ ".config.system.build." ++ typ) ],
                [ "--no-link", "--print-out-paths" ]
                ]
    putStrLn ("Building System " ++ name)
    runProcess (nixExePath) args

switchToConfig :: String -> String -> IO String
switchToConfig path arg = do
    let path' = ((filter (/='\n')) path)
    putStrLn ("Switching to " ++ path')
    runProcess (path' ++ "/bin/switch-to-configuration") [ arg ]

runVM :: String -> String -> IO String
runVM path sys = do
    let path' = ((filter (/='\n')) path)
    putStrLn ("Running VM.. ")
    runProcess (path' ++ "/bin/run-" ++ sys ++ "-vm") [ ]

checkForUser :: CUid -> IO ()
checkForUser a = do
    root <- fmap (== a) getRealUserID
    case root of
      False -> do
          putStrLn "Not Root, Exiting"
          exitFailure
      _ -> pure ()
