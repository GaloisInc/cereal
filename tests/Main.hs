module Main where

import qualified GetTests
import qualified RoundTrip
import qualified BytesRead

import Test.Framework.Runners.Console


main :: IO ()
main  = defaultMain
  [ GetTests.tests
  , RoundTrip.tests
  , BytesRead.tests
  ]
