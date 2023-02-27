{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

 module Rebuild.Cli (optsParser,
                     versionOption,
                     programOptions,
                     switchCommand,
                     buildCommand,
                     vmCommand,
                     vmWBLCommand,
                     dryCommand,
                     deploy,
                     deployOpts,
                     nixInstall,
                     nixInstallOpts,
                     build,
                     impl,
                     Opts,
                     Command)
                where
import Options.Applicative
import Rebuild.Builders
import Rebuild.Helpers
import qualified Data.Text as T
import Cli.Extras


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
    | Boot
    | Deploy String String String Bool
    | NixOSInstall String Bool

genCommandCli :: String -> String -> Command -> Mod CommandFields Command
genCommandCli a b c = command
                    a
                        (info (pure c) (progDesc b))

optsParser :: String -> ParserInfo Opts
optsParser hostname = info
    (helper <*> versionOption <*> (programOptions hostname))
    (fullDesc <> progDesc "rebuild system" <>
        header
            "rebuild -- Rebuild your NixOS system!")

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1" (long "version" <> short 'v' <> help "Show version")

programOptions :: String -> Parser Opts
programOptions hostname =
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
                          bootCommand <>
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

bootCommand :: Mod CommandFields Command
bootCommand = genCommandCli "boot" "Build Configuration but down switch" Boot

deploy :: Mod CommandFields Command
deploy = command
    "deploy"
    (info deployOpts (progDesc "Deploy NixOS to a machine"))

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


build :: NixRun e m => String -> String -> String -> m ()
build path name arg = case arg of
   "build" -> do
       sysbuild <- buildSystemConfig path name "toplevel"
       putLog (Informational) ("System Closure at " <> (T.pack sysbuild))
       pure ()
   "switch" -> do
       checkForUser 0
       sysbuild <- buildSystemConfig path name "toplevel"
       _ <- switchToConfig sysbuild arg
       pure ()
   "boot" -> do
       checkForUser 0
       sysbuild <- buildSystemConfig path name "toplevel"
       _ <- switchToConfig sysbuild arg
       pure ()
   "dry-activate" -> do
       sysbuild <- buildSystemConfig path name "toplevel"
       _ <- switchToConfig sysbuild arg
       pure ()
   "vm" -> do
       sysbuild <- buildSystemConfig path name "vm"
       _ <- runVM sysbuild name
       pure ()
   "vm-with-bootloader" -> do
       sysbuild <- buildSystemConfig path name "vmWithBootLoader"
       _ <- runVM sysbuild name
       pure ()
   _ -> pure ()

impl :: String -> IO ()
impl hostname = do
    opts <- execParser (optsParser hostname)
    let severity = case verbose opts of
                    True -> Debug
                    False -> Informational
    nixRun severity $ case scommand opts of
      Build -> build (configpath opts) (nixsystem opts) "build"
      VM -> build (configpath opts) (nixsystem opts) "vm"
      Switch -> build (configpath opts) (nixsystem opts) "switch"
      VMWithBootLoader -> build (configpath opts) (nixsystem opts) "vm-with-bootloader"
      DryActivate -> build (configpath opts) (nixsystem opts) "dry-activate"
      Boot -> build (configpath opts) (nixsystem opts) "boot"
      NixOSInstall root pass -> installToDir root pass (configpath opts) (nixsystem opts)
      Deploy sys port key doSign -> deployConfig doSign (configpath opts) (nixsystem opts) sys port key

