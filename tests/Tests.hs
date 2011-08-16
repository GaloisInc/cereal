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

import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize.IEEE754
import Data.Word (Word8,Word16,Word32,Word64)
import Test.QuickCheck as QC


roundTrip :: Eq a => Putter a -> Get a -> a -> Bool
roundTrip p g a = res == Right a
  where res = runGet g (runPut (p a))

main :: IO ()
main  = mapM_ quickCheck
  [ QC.label "Word8         Round Trip" $ roundTrip putWord8      getWord8
  , QC.label "Word16be      Round Trip" $ roundTrip putWord16be   getWord16be
  , QC.label "Word16le      Round Trip" $ roundTrip putWord16le   getWord16le
  , QC.label "Word32be      Round Trip" $ roundTrip putWord32be   getWord32be
  , QC.label "Word32le      Round Trip" $ roundTrip putWord32le   getWord32le
  , QC.label "Word64be      Round Trip" $ roundTrip putWord64be   getWord64be
  , QC.label "Word64le      Round Trip" $ roundTrip putWord64le   getWord64le
  , QC.label "Word16host    Round Trip" $ roundTrip putWord16host getWord16host
  , QC.label "Word32host    Round Trip" $ roundTrip putWord32host getWord32host
  , QC.label "Word64host    Round Trip" $ roundTrip putWord64host getWord64host
  , QC.label "Float32le     Round Trip" $ roundTrip putFloat32le  getFloat32le
  , QC.label "Float32be     Round Trip" $ roundTrip putFloat32be  getFloat32be
  , QC.label "Float64le     Round Trip" $ roundTrip putFloat64le  getFloat64le
  , QC.label "Float64be     Round Trip" $ roundTrip putFloat64be  getFloat64be

    -- Containers
  , QC.label "(Word8,Word8) Round Trip"
    $ roundTrip (putTwoOf putWord8 putWord8) (getTwoOf getWord8 getWord8)
  , QC.label "[Word8] Round Trip"
    $ roundTrip (putListOf putWord8) (getListOf getWord8)
  , QC.label "Maybe Word8 Round Trip"
    $ roundTrip (putMaybeOf putWord8) (getMaybeOf getWord8)
  , QC.label "Either Word8 Word16be Round Trip "
    $ roundTrip (putEitherOf putWord8 putWord16be)
                (getEitherOf getWord8 getWord16be)
  ]
