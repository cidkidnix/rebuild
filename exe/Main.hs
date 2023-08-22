module Main where

import Network.HostName
import Earth.Cli

main :: IO ()
main = do
  hostname <- getHostName
  impl hostname
