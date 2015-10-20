{-# LANGUAGE ExistentialQuantification #-}

--------------------------------------------------------------------------------
-- |
-- Module      : 
-- Copyright   : (c) Galois, Inc, 2009
-- License     : AllRightsReserved
--
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   : 
-- Portability : 
--
module RoundTrip where

import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize.IEEE754
import Data.Word (Word8,Word16,Word32,Word64)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import Test.QuickCheck as QC

import Test.Framework (Test(),testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


roundTrip :: Eq a => Putter a -> Get a -> a -> Bool
roundTrip p g a = res == Right a
  where res = runGet g (runPut (p a))

-- | Did a call to 'quickCheckResult' succeed?
isSuccess :: QC.Result -> Bool
isSuccess (Success _ _ _) = True
isSuccess _ = False

tests :: Test
tests  = testGroup "Round Trip"
  [ testProperty "Word8        Round Trip" $ roundTrip putWord8      getWord8
  , testProperty "Word16be     Round Trip" $ roundTrip putWord16be   getWord16be
  , testProperty "Word16le     Round Trip" $ roundTrip putWord16le   getWord16le
  , testProperty "Word32be     Round Trip" $ roundTrip putWord32be   getWord32be
  , testProperty "Word32le     Round Trip" $ roundTrip putWord32le   getWord32le
  , testProperty "Word64be     Round Trip" $ roundTrip putWord64be   getWord64be
  , testProperty "Word64le     Round Trip" $ roundTrip putWord64le   getWord64le
  , testProperty "Word16host   Round Trip" $ roundTrip putWord16host getWord16host
  , testProperty "Word32host   Round Trip" $ roundTrip putWord32host getWord32host
  , testProperty "Word64host   Round Trip" $ roundTrip putWord64host getWord64host
  , testProperty "Float32le    Round Trip" $ roundTrip putFloat32le  getFloat32le
  , testProperty "Float32be    Round Trip" $ roundTrip putFloat32be  getFloat32be
  , testProperty "Float64le    Round Trip" $ roundTrip putFloat64le  getFloat64le
  , testProperty "Float64be    Round Trip" $ roundTrip putFloat64be  getFloat64be

    -- Containers
  , testProperty "(Word8,Word8) Round Trip"
    $ roundTrip (putTwoOf putWord8 putWord8) (getTwoOf getWord8 getWord8)
  , testProperty "[Word8] Round Trip"
    $ roundTrip (putListOf putWord8) (getListOf getWord8)
  , testProperty "Maybe Word8 Round Trip"
    $ roundTrip (putMaybeOf putWord8) (getMaybeOf getWord8)
  , testProperty "Either Word8 Word16be Round Trip "
    $ roundTrip (putEitherOf putWord8 putWord16be)
                (getEitherOf getWord8 getWord16be)
  ]
