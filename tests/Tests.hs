{-# LANGUAGE ExistentialQuantification #-}


import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word (Word8)
import Test.QuickCheck

instance Arbitrary Word8 where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary Word16 where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary Word32 where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary Word64 where
  arbitrary = arbitraryBoundedIntegral


roundTrip :: Eq a => Putter a -> Get a -> a -> Bool
roundTrip p g a = res == Right a
  where res = runGet g (runPut (p a))

main :: IO ()
main  = mapM_ quickCheck
  [ label "Word8         Round Trip" $ roundTrip putWord8      getWord8
  , label "Word16be      Round Trip" $ roundTrip putWord16be   getWord16be
  , label "Word16le      Round Trip" $ roundTrip putWord16le   getWord16le
  , label "Word32be      Round Trip" $ roundTrip putWord32be   getWord32be
  , label "Word32le      Round Trip" $ roundTrip putWord32le   getWord32le
  , label "Word64be      Round Trip" $ roundTrip putWord64be   getWord64be
  , label "Word64le      Round Trip" $ roundTrip putWord64le   getWord64le
  , label "Word16host    Round Trip" $ roundTrip putWord16host getWord16host
  , label "Word32host    Round Trip" $ roundTrip putWord32host getWord32host
  , label "Word64host    Round Trip" $ roundTrip putWord64host getWord64host

    -- Containers
  , label "(Word8,Word8) Round Trip"
    $ roundTrip (putTwoOf putWord8 putWord8) (getTwoOf getWord8 getWord8)
  , label "[Word8] Round Trip"
    $ roundTrip (putListOf putWord8) (getListOf getWord8)
  , label "Maybe Word8 Round Trip"
    $ roundTrip (putMaybeOf putWord8) (getMaybeOf getWord8)
  , label "Either Word8 Word16be Round Trip "
    $ roundTrip (putEitherOf putWord8 putWord16be)
                (getEitherOf getWord8 getWord16be)
  ]
