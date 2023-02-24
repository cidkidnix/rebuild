module Main where
import Options.Applicative
import Network.HostName
import System.Exit
import Helpers

data Opts = Opts
    {
      configpath :: !String,
      nixsystem :: !String,
      scommand :: !Command
    }

data Command
    = Build
    | Switch
    | VM
    | VMWithBootLoader
    | DryActivate
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

build :: String -> String -> String -> IO()
build path name arg
   | arg <= "build" = do
       sysbuild <- buildSystemConfig path name "toplevel"
       putStr sysbuild
   | arg <= "switch" = do
       checkForUser 0
       sysbuild <- buildSystemConfig path name "toplevel"
       nixswitch <- switchToConfig sysbuild arg
       putStr nixswitch
   | arg <= "dry-activate" = do
       sysbuild <- buildSystemConfig path name "toplevel"
       nixswitch <- switchToConfig sysbuild arg
       putStr nixswitch
   | arg <= "vm" = do
       sysbuild <- buildSystemConfig path name "vm"
       vm <- runVM sysbuild name
       putStr vm
   | arg <= "vm-with-bootloader" = do
       sysbuild <- buildSystemConfig path name "vmWithBootLoader"
       vm <- runVM sysbuild name
       putStr vm
   | otherwise = pure ()

impl :: String -> IO ()
impl hostname = do
    opts <- execParser optsParser
    case scommand opts of
      Build -> build (configpath opts) (nixsystem opts) "build"
      VM -> build (configpath opts) (nixsystem opts) "vm"
      Switch -> build (configpath opts) (nixsystem opts) "switch"
      VMWithBootLoader -> build (configpath opts) (nixsystem opts) "vm-with-bootloader"
      DryActivate -> build (configpath opts) (nixsystem opts) "dry-activate"
      NixOSInstall root pass -> installNixOS (configpath opts) (nixsystem opts) root pass
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

                    hsubparser (switchCommand <>
                                buildCommand <>
                                vmCommand <>
                                vmWBLCommand <>
                                dryCommand <>
                                nixInstall)

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
