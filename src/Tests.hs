module Main where

import Test.HUnit (runTestTT, Test(..))
import qualified Core.PPrinter.Tests as PPrinter
import qualified Core.Parser.Tests as Parser

main = runTestTT $ TestList [PPrinter.tests, Parser.tests]
