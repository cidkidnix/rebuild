{-# LANGUAGE CPP #-}
{- ORMOLU_DISABLE -}

module Earth.Cli
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
    nixInstallOpts,
    impl,
    Opts,
    Command,
  )
where

import Cli.Extras
import Options.Applicative
import Earth.Builders
import Earth.Helpers
import Earth.Types

-- Ugly hacks to get darwin to be the default
-- and not depend on linux-mount on all platforms
-- we still want the darwin code on non-darwin system
-- since it's not darwin-specific (somewhat)
#if !defined(darwin_HOST_OS)
#elif defined(darwin_HOST_OS)
import Earth.Darwin

switchTrue :: Mod FlagFields Bool -> Parser Bool
switchTrue = flag True False
#endif

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
          "rebuild -- Earth your NixOS system!"
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
    <*> switch (long "legacy")
    <*> switch (long "verbose")
#if defined(darwin_HOST_OS)
    <*> switchTrue (long "darwin")
#else
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
        <> gc
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

gc :: Mod CommandFields Command
gc = command "gc"
    (info gcOpts (progDesc "Garbage Collect your systems!"))

gcOpts :: Parser Command
gcOpts =
    GC
      <$> option auto (long "from" <> value 0 <> help "First link to attempt to remove")
      <*> option auto (long "to" <> value 0 <> help "Last link to attempt to remove")
      <*> switch (long "dry-run" <> help "Show what would have been done, doesn't actually run GC operation")
      <*> strOption (long "older-than" <> value "1d" <> help "Delete all systems older than a specified amount of days")

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

collectOptions :: Opts -> Options
collectOptions opts = Options {
  path = configpath opts,
  name = nixsystem opts,
  com = scommand opts
                         }

impl :: String -> IO ()
impl hostname = do
  opts <- execParser (optsParser hostname)
  let severity = case verbose opts of
        True -> Debug
        False -> Informational
      colOpts = collectOptions opts

  --nixRun severity $ regBuild colOpts
  case legacy opts of
    False -> nixRun severity $ flakeBuild colOpts
    True -> nixRun severity $ legacyBuild colOpts
