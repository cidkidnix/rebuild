module Rebuild.Helpers where

import Cli.Extras
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad (forM_)
import Data.Monoid as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Rebuild.Types
import System.Exit
import System.Posix.Types
import System.Posix.User
import System.Which

makePrisms ''NixError

nixRun :: Severity -> CliT NixError IO a -> IO a
nixRun severity action = do
  cfg <- newCliConfig severity False False (\t -> (T.pack . show $ t, ExitFailure 1))
  runCli cfg action

nixExePath :: FilePath
nixExePath = $(staticWhich "nix")

nixBuildExe :: FilePath
nixBuildExe = $(staticWhich "nix-build")

nixCollectGarbage :: FilePath
nixCollectGarbage = $(staticWhich "nix-collect-garbage")

sshExePath :: FilePath
sshExePath = $(staticWhich "ssh")

nixEnvPath :: FilePath
nixEnvPath = $(staticWhich "nix-env")

commonNixArgs :: [String]
commonNixArgs =
  M.mconcat
    [ ["--option", "sandbox", "true"]
    ]

legacyNixOSBuildArgs :: String -> String -> LegacyDef
legacyNixOSBuildArgs = toLegacyDef

nixOSBuildargs :: String -> String -> String -> FlakeDef
nixOSBuildargs flakepath = toFlakeDef flakepath "nixosConfigurations"

nixDarwinBuildargs :: String -> String -> String -> FlakeDef
nixDarwinBuildargs flakepath = toFlakeDef flakepath "darwinConfigurations"

runProcess :: NixRun e m => FilePath -> [Text] -> m Text
runProcess com' args = do
  toStorePath <$> readProcessAndLogOutput (Debug, Debug) (proc com' (map T.unpack args))

runProcessWithSSH :: NixRun e m => String -> String -> [String] -> [Text] -> m OtherOutput
runProcessWithSSH port host sargs com' = do
  let pargs' = T.unwords com'
      cmd = sshExePath
      args' =
        M.mconcat
          [ sargs,
            [host, "-p", port],
            [T.unpack pargs']
          ]
  toStorePath <$> readProcessAndLogOutput (Debug, Debug) (proc cmd args')

withSSH :: NixRun e m => String -> String -> [String] -> [[Text]] -> m ()
withSSH port host sargs com' = forM_ com' $ \x ->
  runProcessWithSSH port host sargs x

checkForUser :: NixRun e m => CUid -> m ()
checkForUser a = do
  root <- fmap (== a) (liftIO getRealUserID)
  if root then
    pure ()
  else
    failWith "Not Root, Exiting"

textToMaybeInteger :: Text -> Maybe (Integer, Time)
textToMaybeInteger days = case T.decimal days of
            Left _ -> Nothing
            Right (num, letter) -> case letter of
                                     "d" -> Just (num, Day)
                                     "w" -> Just (num, Week)
                                     "m" -> Just (num, Month)
                                     _ -> Nothing

data Time = Day
          | Week
          | Month
  deriving (Show, Eq, Ord)

dayInMinutes :: Integer
dayInMinutes = 1440

weekInMinutes :: Integer
weekInMinutes = 10080

monthInMinutes :: Integer
monthInMinutes = 43800

timeToSeconds :: (Integer, Time) -> Integer
timeToSeconds a = case snd a of
                    Week ->  weekInMinutes * fst a * 60
                    Day -> dayInMinutes * fst a * 60
                    Month -> monthInMinutes * fst a * 60
