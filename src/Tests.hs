{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (runTestTT, Test(..), Counts(..))
import qualified Core.PPrinter.Tests as PPrinter
import qualified Core.Parser.Tests as Parser

main :: IO ()
main = do
  Counts {..} <- runTestTT $ TestList [PPrinter.tests, Parser.tests]
  if any (/= 0) [errors, failures]
  then exitFailure
  else exitSuccess
