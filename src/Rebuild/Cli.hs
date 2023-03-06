{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Rebuild.Cli
  ( optsParser,
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
    impl,
    Opts,
    Command,
  )
where

import Cli.Extras
import Options.Applicative
import Rebuild.Builders
import Rebuild.Deploy
import Rebuild.Helpers
#if !defined(darwin_HOST_OS)
import Rebuild.Install
import Rebuild.Darwin hiding (installToDir)
#elif defined(darwin_HOST_OS)
import Rebuild.Darwin

switchTrue :: Mod FlagFields Bool -> Parser Bool
switchTrue = flag True False
#endif

data Opts = Opts
  { configpath :: !String,
    nixsystem :: !String,
    verbose :: !Bool,
    darwin :: !Bool,
    scommand :: !Command
  }

data Command
  = Build
  | VM
  | VMWithBootLoader
  | DryActivate
  | Switch String
  | Boot String
  | BuildISO
  | Deploy String String String Bool
  | NixOSInstall String Bool

genCommandCli :: String -> String -> Command -> Mod CommandFields Command
genCommandCli a b c =
  command
    a
    (info (pure c) (progDesc b))

optsParser :: String -> ParserInfo Opts
optsParser hostname =
  info
    (helper <*> versionOption <*> programOptions hostname)
    ( fullDesc
        <> progDesc "rebuild system"
        <> header
          "rebuild -- Rebuild your NixOS system!"
    )

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1" (long "version" <> short 'v' <> help "Show version")

programOptions :: String -> Parser Opts
programOptions hostname =
  Opts
    <$> strOption
      ( long "path"
          <> short 'p'
          <> metavar "PATH"
          <> value "/etc/nixos"
          <> help "Path of configuration"
          <> showDefault
      )
    <*> strOption
      ( long "system"
          <> short 's'
          <> metavar "SYSTEM"
          <> help "Configuration name"
          <> showDefault
          <> value hostname
      )
    <*> switch (long "verbose")
#if defined(darwin_HOST_OS)
    <*> switchTrue (long "darwin")
#elif !defined(darwin_HOST_OS)
    <*> switch (long "darwin")
#endif
    <*> hsubparser
      ( switchCommand
          <> buildCommand
          <> vmCommand
          <> vmWBLCommand
          <> dryCommand
          <> bootCommand
          <> buildISOCommand
          <> nixInstall
          <> deploy
      )

switchCommand :: Mod CommandFields Command
switchCommand =
  command
    "switch"
    (info systemOptsS (progDesc "Switch to new configuration"))

bootCommand :: Mod CommandFields Command
bootCommand = do
  command
    "boot"
    (info systemOptsB (progDesc "Boot to new configuration"))

buildCommand :: Mod CommandFields Command
buildCommand = genCommandCli "build" "Build Configuration" Build

vmCommand :: Mod CommandFields Command
vmCommand = genCommandCli "vm" "Build VM" VM

vmWBLCommand :: Mod CommandFields Command
vmWBLCommand = genCommandCli "vm-with-bootloader" "Build VM with Bootloader" VMWithBootLoader

dryCommand :: Mod CommandFields Command
dryCommand = genCommandCli "dry-activate" "Show what would have been done if activated" DryActivate

buildISOCommand :: Mod CommandFields Command
buildISOCommand = genCommandCli "build-iso" "Build ISO image" BuildISO

deploy :: Mod CommandFields Command
deploy =
  command
    "deploy"
    (info deployOpts (progDesc "Deploy NixOS to a machine"))

deployOpts :: Parser Command
deployOpts =
  Deploy
    <$> strArgument (metavar "Where to deploy" <> help "Where to install NixOS to")
    <*> strOption (long "port" <> value "22" <> metavar "SSH port")
    <*> strOption (long "key" <> value "example" <> metavar "Signing Key")
    <*> switch (long "sign")

nixInstall :: Mod CommandFields Command
nixInstall =
  command
    "install"
    (info nixInstallOpts (progDesc "Install NixOS"))

nixInstallOpts :: Parser Command
nixInstallOpts =
  NixOSInstall
    <$> strArgument (metavar "ROOT" <> help "Where to install NixOS to")
    <*> switch (long "no-root-password" <> help "Install with no root password")

systemOptsS :: Parser Command
systemOptsS =
  Switch <$> strOption (long "profile" <> value "/nix/var/nix/profiles/system" <> help "Profile to install to" <> showDefault)

systemOptsB :: Parser Command
systemOptsB =
  Boot <$> strOption (long "profile" <> value "/nix/var/nix/profiles/system" <> help "Profile to install to" <> showDefault)

impl :: String -> IO ()
impl hostname = do
  opts <- execParser (optsParser hostname)
  let severity = case verbose opts of
        True -> Debug
        False -> Informational
  case darwin opts of
    True -> nixRun severity $ case scommand opts of
      Switch profile -> darwinBuild (configpath opts) (nixsystem opts) profile "switch"
      Boot _ -> failWith "Darwin doesn't support boot!"
      VM -> failWith "Darwin doesn't support VM!"
      VMWithBootLoader -> failWith "Darwin doesn't support VM with Bootloader!"
      DryActivate -> failWith "Darwin doesn't support Dry-activate!"
      NixOSInstall _ _ -> failWith "Darwin doesn't support NixOS installer!"
      Deploy _ _ _ _ -> failWith "Not currently implemented"
      Build -> regDarwinBuild (configpath opts) (nixsystem opts) "build"
      _ -> pure ()
    False -> nixRun severity $ case scommand opts of
      Build -> regBuild (configpath opts) (nixsystem opts) "build"
      VM -> regBuild (configpath opts) (nixsystem opts) "vm"
      Switch profile -> systemBuild (configpath opts) (nixsystem opts) profile "switch"
      Boot profile -> systemBuild (configpath opts) (nixsystem opts) profile "boot"
      VMWithBootLoader -> regBuild (configpath opts) (nixsystem opts) "vm-with-bootloader"
      DryActivate -> regBuild (configpath opts) (nixsystem opts) "dry-activate"
      BuildISO -> regBuild (configpath opts) (nixsystem opts) "build-iso"
      NixOSInstall root pass -> installToDir root pass (configpath opts) (nixsystem opts)
      Deploy sys port key doSign -> deployConfig doSign (configpath opts) (nixsystem opts) sys port key
