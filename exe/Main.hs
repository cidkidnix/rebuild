{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Options.Applicative
import Network.HostName
import System.Exit
import Cli.Extras
import Control.Monad.IO.Class
import qualified Data.Text as T
import Helpers

data Opts = Opts
    {
      configpath :: !String,
      nixsystem :: !String,
      verbose :: !Bool,
      scommand :: !Command
    }

data Command
    = Build
    | Switch
    | VM
    | VMWithBootLoader
    | DryActivate
    | Deploy String String String Bool
    | NixOSInstall String Bool

genCommandCli :: String -> String -> Command -> Mod CommandFields Command
genCommandCli a b c = command
                    a
                        (info (pure c) (progDesc b))

installNixOS :: String -> String -> String -> Bool -> IO ()
installNixOS path name root nopass = do
    putStrLn (path ++ name ++ root)
    putStrLn "Not implemented!"
    print nopass
    exitFailure

build :: NixRun e m => String -> String -> String -> m ()
build path name arg
   | arg <= "build" = do
       _ <- buildSystemConfig path name "toplevel"
       pure ()
   | arg <= "switch" = do
       checkForUser 0
       sysbuild <- buildSystemConfig path name "toplevel"
       _ <- switchToConfig sysbuild arg
       pure ()
   | arg <= "dry-activate" = do
       sysbuild <- buildSystemConfig path name "toplevel"
       _ <- switchToConfig sysbuild arg
       pure ()
   | arg <= "vm" = do
       sysbuild <- buildSystemConfig path name "vm"
       _ <- runVM sysbuild name
       pure ()
   | arg <= "vm-with-bootloader" = do
       sysbuild <- buildSystemConfig path name "vmWithBootLoader"
       _ <- runVM sysbuild name
       pure ()
   | otherwise = pure ()

deployConfig :: NixRun e m => Bool -> String -> String -> String -> String -> String -> m ()
deployConfig doSign path name host port key = do
    build <- buildSystemConfig path name "toplevel"

    _ <- case doSign of
      True -> signClosures build key
      False -> return "Nothing"

    _ <- copyDeployment port host name build

    let build' = ((filter (/='\n')) build)

    _ <- withSpinner ("Switching " <> (T.pack host) <> " to " <> (T.pack build)) $ do
        runProcessWithSSH port host [ "-t" ] [ (build' <> "/bin/switch-to-configuration"), "switch" ]

    pure ()

impl :: String -> IO ()
impl hostname = do
    opts <- execParser optsParser
    let severity = case verbose opts of
                    True -> Debug
                    False -> Informational
    nixRun severity $ case scommand opts of
      Build -> build (configpath opts) (nixsystem opts) "build"
      VM -> build (configpath opts) (nixsystem opts) "vm"
      Switch -> build (configpath opts) (nixsystem opts) "switch"
      VMWithBootLoader -> build (configpath opts) (nixsystem opts) "vm-with-bootloader"
      DryActivate -> build (configpath opts) (nixsystem opts) "dry-activate"
      NixOSInstall root pass -> pure ()
      Deploy sys port key doSign -> deployConfig doSign (configpath opts) (nixsystem opts) sys port key
          --installNixOS (configpath opts) (nixsystem opts) root pass
    where
        optsParser :: ParserInfo Opts
        optsParser = info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "rebuild system" <>
                header
                    "rebuild -- Rebuild your NixOS system!")

        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.1" (long "version" <> short 'v' <> help "Show version")

        programOptions :: Parser Opts
        programOptions =
           Opts <$> strOption (long "path" <>
                               short 'p' <>
                               metavar "PATH" <>
                               value "/etc/nixos" <>
                               help "Path of configuration" <>
                               showDefault) <*>

                    strOption (long "system" <>
                               short 's' <>
                               metavar "SYSTEM" <>
                               help "Configuration name" <>
                               showDefault <>
                               value hostname) <*>

                    switch (long "verbose") <*>

                    hsubparser (switchCommand <>
                                buildCommand <>
                                vmCommand <>
                                vmWBLCommand <>
                                dryCommand <>
                                nixInstall <>
                                deploy)

        switchCommand :: Mod CommandFields Command
        switchCommand = genCommandCli "switch" "Switch to configuration" Switch

        buildCommand :: Mod CommandFields Command
        buildCommand = genCommandCli "build" "Build Configuration" Build

        vmCommand :: Mod CommandFields Command
        vmCommand = genCommandCli "vm" "Build VM" VM

        vmWBLCommand :: Mod CommandFields Command
        vmWBLCommand = genCommandCli "vm-with-bootloader" "Build VM with Bootloader" VMWithBootLoader

        dryCommand :: Mod CommandFields Command
        dryCommand = genCommandCli "dry-activate" "Show what would have been done if activated" DryActivate

        deploy :: Mod CommandFields Command
        deploy = command
            "deploy"
            (info deployOpts (progDesc "Install NixOS"))

        deployOpts :: Parser Command
        deployOpts = Deploy <$>
            strArgument (metavar "Where to deploy" <> help "Where to install NixOS to") <*>
            strOption (long "port" <> value "22" <> metavar "SSH port") <*>
            strOption (long "key" <> value "example" <> metavar "Signing Key") <*>
            switch (long "sign")

        nixInstall :: Mod CommandFields Command
        nixInstall = command
            "install"
            (info nixInstallOpts (progDesc "Install NixOS"))

        nixInstallOpts :: Parser Command
        nixInstallOpts = NixOSInstall <$>
            strArgument (metavar "ROOT" <> help "Where to install NixOS to") <*>
            switch (long "no-root-password" <> help "Install with no root password")

main :: IO ()
main = do
    hostname <- getHostName
    impl hostname
