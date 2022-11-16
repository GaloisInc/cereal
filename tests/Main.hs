module Main where

import qualified GetTests
import qualified RoundTrip

import Test.Framework.Runners.Console
import Data.Time
import Data.Time.LocalTime
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Serialize


main :: IO ()
main  = do
  ltime <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
  utime <- getCurrentTime
  defaultMain
    [ GetTests.tests
    , RoundTrip.tests
    , (RoundTrip.timeTests ltime utime)
    ]
