{-# LANGUAGE ExistentialQuantification #-}

module BytesRead where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Word

import Test.Framework (Test(),testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- just more than enough bytes to ensure all gets can succeed
pad :: [Word8] -> BS.ByteString
pad ws = BS.append (BS.pack ws) (BS.replicate 16 0)

count :: Get a -> Int -> [Word8] -> Bool
count g n ws = runGet (g *> bytesRead) (pad ws) == Right n

countLA :: Get a -> [Word8] -> Bool
countLA g ws = runGet (lookAhead g *> bytesRead) (pad ws) == Right 0

tests :: Test
tests  = testGroup "Bytes Read"
  [ testProperty "null         get" $ count (pure ())     0
  , testProperty "Word8        get" $ count getWord8      1
  , testProperty "Word16be     get" $ count getWord16be   2
  , testProperty "Word16le     get" $ count getWord16le   2
  , testProperty "Word32be     get" $ count getWord32be   4
  , testProperty "Word32le     get" $ count getWord32le   4
  , testProperty "Word64be     get" $ count getWord64be   8
  , testProperty "Word64le     get" $ count getWord64le   8
  , testProperty "Word16host   get" $ count getWord16host 2
  , testProperty "Word32host   get" $ count getWord32host 4
  , testProperty "Word64host   get" $ count getWord64host 8
  , testProperty "Float32le    get" $ count getFloat32le  4
  , testProperty "Float32be    get" $ count getFloat32be  4
  , testProperty "Float64le    get" $ count getFloat64le  8
  , testProperty "Float64be    get" $ count getFloat64be  8

  , testProperty "null         lookAhead" $ countLA (pure ())
  , testProperty "Word8        lookAhead" $ countLA getWord8
  , testProperty "Word16be     lookAhead" $ countLA getWord16be
  , testProperty "Word16le     lookAhead" $ countLA getWord16le
  , testProperty "Word32be     lookAhead" $ countLA getWord32be
  , testProperty "Word32le     lookAhead" $ countLA getWord32le
  , testProperty "Word64be     lookAhead" $ countLA getWord64be
  , testProperty "Word64le     lookAhead" $ countLA getWord64le
  , testProperty "Word16host   lookAhead" $ countLA getWord16host
  , testProperty "Word32host   lookAhead" $ countLA getWord32host
  , testProperty "Word64host   lookAhead" $ countLA getWord64host
  , testProperty "Float32le    lookAhead" $ countLA getFloat32le
  , testProperty "Float32be    lookAhead" $ countLA getFloat32be
  , testProperty "Float64le    lookAhead" $ countLA getFloat64le
  , testProperty "Float64be    lookAhead" $ countLA getFloat64be
  ]
