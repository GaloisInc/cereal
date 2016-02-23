{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-- | IEEE-754 parsing, as described in this stack-overflow article:
--
-- <http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-float/7002812#7002812>

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

import Data.Word ( Word32, Word64 )
import Data.Serialize.Get
import Data.Serialize.Put
import qualified Data.ByteString.Builder as Builder
import System.IO.Unsafe (unsafeDupablePerformIO)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, poke)
import Foreign.Ptr (castPtr, Ptr)

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ( (<$>) )
#endif

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
putFloat32le = putBuilder . Builder.floatLE

-- | Write a Float in big endian IEEE-754 format
putFloat32be :: Float -> Put
putFloat32be = putBuilder . Builder.floatBE

-- | Write a Double in little endian IEEE-754 format
putFloat64le :: Double -> Put
putFloat64le = putBuilder . Builder.doubleLE

-- | Write a Double in big endian IEEE-754 format
putFloat64be :: Double -> Put
putFloat64be = putBuilder . Builder.doubleBE

{-# INLINE wordToFloat #-}
wordToFloat :: Word32 -> Float
wordToFloat w = unsafeDupablePerformIO $ alloca $ \(ptr :: Ptr Word32) -> do
    poke ptr w
    peek (castPtr ptr)

{-# INLINE wordToDouble #-}
wordToDouble :: Word64 -> Double
wordToDouble w = unsafeDupablePerformIO $ alloca $ \(ptr :: Ptr Word64) -> do
    poke ptr w
    peek (castPtr ptr)
