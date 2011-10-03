{-# LANGUAGE FlexibleContexts #-}

-- | IEEE-754 parsing, as described in this stack-overflow article:
--
-- http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-float/7002812#7002812

module Data.Serialize.IEEE754 (

    -- * IEEE-754 reads
      getFloat32le
    , getFloat32be
    , getFloat64le
    , getFloat64be

    -- * IEEE-754 writes
    , putFloat32le
    , putFloat32be
    , putFloat64le
    , putFloat64be

) where

import Control.Applicative ( (<$>) )
import Control.Monad.ST ( runST, ST )

import Data.Array.ST ( newArray, castSTUArray, readArray, MArray, STUArray )
import Data.Word ( Word32, Word64 )
import Data.Serialize.Get
import Data.Serialize.Put

-- | Read a Float in little endian IEEE-754 format
getFloat32le :: Get Float
getFloat32le = wordToFloat <$> getWord32le

-- | Read a Float in big endian IEEE-754 format
getFloat32be :: Get Float
getFloat32be = wordToFloat <$> getWord32be

-- | Read a Double in little endian IEEE-754 format
getFloat64le :: Get Double
getFloat64le = wordToDouble <$> getWord64le

-- | Read a Double in big endian IEEE-754 format
getFloat64be :: Get Double
getFloat64be = wordToDouble <$> getWord64be

-- | Write a Float in little endian IEEE-754 format
putFloat32le :: Float -> Put
putFloat32le = putWord32le . floatToWord

-- | Write a Float in big endian IEEE-754 format
putFloat32be :: Float -> Put
putFloat32be = putWord32be . floatToWord

-- | Write a Double in little endian IEEE-754 format
putFloat64le :: Double -> Put
putFloat64le = putWord64le . doubleToWord

-- | Write a Double in big endian IEEE-754 format
putFloat64be :: Double -> Put
putFloat64be = putWord64be . doubleToWord

{-# INLINE wordToFloat #-}
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

{-# INLINE floatToWord #-}
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

{-# INLINE wordToDouble #-}
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

{-# INLINE doubleToWord #-}
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) =>
        a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
