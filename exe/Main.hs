{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Network.HostName
import Rebuild.Cli

main :: IO ()
main = do
  hostname <- getHostName
  impl hostname
