{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.Process hiding (runProcess)
import Data.Monoid as M
import System.Which
import System.Posix.User
import System.Posix.Types
import System.Exit
import Options.Applicative
import Data.Semigroup ((<>))

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

nixExePath :: FilePath
nixExePath = $(staticWhich "nix")

nixShellPath :: FilePath
nixShellPath = $(staticWhich "nix-shell")

nixInstPath :: FilePath
nixInstPath = $(staticWhich "nix-instantiate")

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
runVM path system = do
    let path' = ((filter (/='\n')) path)
    putStrLn ("Running VM.. ")
    runProcess (path' ++ "/bin/run-" ++ system ++ "-vm") [ ]

checkForUser :: CUid -> IO ()
checkForUser a = do
    root <- fmap (== a) getRealUserID
    case root of
      False -> do
          putStrLn "Not Root, Exiting"
          exitFailure
      _ -> pure ()

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
   | otherwise = do putStr "Need more arguments!"

impl :: IO ()
impl = do
    opts <- execParser optsParser
    case scommand opts of
      Build -> build (configpath opts) (nixsystem opts) "build"
      VM -> build (configpath opts) (nixsystem opts) "vm"
      Switch -> build (configpath opts) (nixsystem opts) "switch"
      VMWithBootLoader -> build (configpath opts) (nixsystem opts) "vm-with-bootloader"
      DryActivate -> build (configpath opts) (nixsystem opts) "dry-activate"
    where
        optsParser :: ParserInfo Opts
        optsParser = info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "rebuild system" <>
                header
                    "rebuild -- Rebuild your NixOS system!")

        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.1" (long "version" <> help "Show version")

        programOptions :: Parser Opts
        programOptions =
           Opts <$> strOption (long "path" <> metavar "PATH" <> value "/etc/nixos" <> help "Path of configuration, Default: /etc/nixos") <*>
               strOption (long "system" <> metavar "SYSTEM" <> help "Configuration name") <*>
               hsubparser (switchCommand <> buildCommand <> vmCommand <> vmWBLCommand <> dryCommand)

        switchCommand :: Mod CommandFields Command
        switchCommand =
            command "switch"
                (info (pure Switch) (progDesc "Switch to configuration"))

        buildCommand :: Mod CommandFields Command
        buildCommand =
            command
                "build"
                (info (pure Build) (progDesc "Build configuration"))

        vmCommand :: Mod CommandFields Command
        vmCommand =
            command
                "vm"
                (info (pure VM) (progDesc "Build VM"))

        vmWBLCommand :: Mod CommandFields Command
        vmWBLCommand =
            command
                "vm-with-bootloader"
                (info (pure VMWithBootLoader) (progDesc "Build VM with bootloader"))

        dryCommand :: Mod CommandFields Command
        dryCommand =
            command
                "dry-activate"
                (info (pure DryActivate) (progDesc "Show what would have been done if activated"))
main :: IO ()
main = impl
