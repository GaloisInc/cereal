{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Serialize
-- Copyright   : Lennart Kolmodin, Galois Inc. 2009
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   :
-- Portability :
--
-----------------------------------------------------------------------------

module Data.Serialize (

    -- * The Serialize class
      Serialize(..)

    -- $example

    -- * Serialize serialisation
    , encode, encodeLazy
    , decode, decodeLazy

    , module Data.Serialize.Get
    , module Data.Serialize.Put

    ) where

import Data.Serialize.Put
import Data.Serialize.Get

import Control.Monad
import Data.Array.Unboxed
import Data.ByteString (ByteString)
import Data.Char    (chr,ord)
import Data.List    (unfoldr)
import Data.Word
import Foreign

-- And needed for the instances:
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.IntMap          as IntMap
import qualified Data.IntSet          as IntSet
import qualified Data.Ratio           as R
import qualified Data.Tree            as T
import qualified Data.Sequence        as Seq

------------------------------------------------------------------------

class Serialize t where
    -- | Encode a value in the Put monad.
    put :: Putter t
    -- | Decode a value in the Get monad
    get :: Get t

------------------------------------------------------------------------
-- Wrappers to run the underlying monad

-- | Encode a value using binary serialization to a strict ByteString.
encode :: Serialize a => a -> ByteString
encode = runPut . put

-- | Encode a value using binary serialization to a lazy ByteString.
encodeLazy :: Serialize a => a -> L.ByteString
encodeLazy  = runPutLazy . put

-- | Decode a value from a strict ByteString, reconstructing the original
-- structure.
decode :: Serialize a => ByteString -> Either String a
decode = runGet get

-- | Decode a value from a lazy ByteString, reconstructing the original
-- structure.
decodeLazy :: Serialize a => L.ByteString -> Either String a
decodeLazy  = runGetLazy get

------------------------------------------------------------------------
-- Simple instances

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Serialize () where
    put ()  = return ()
    get     = return ()

-- Bools are encoded as a byte in the range 0 .. 1
instance Serialize Bool where
    put     = putWord8 . fromIntegral . fromEnum
    get     = liftM (toEnum . fromIntegral) getWord8

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Serialize Ordering where
    put     = putWord8 . fromIntegral . fromEnum
    get     = liftM (toEnum . fromIntegral) getWord8

------------------------------------------------------------------------
-- Words and Ints

-- Words8s are written as bytes
instance Serialize Word8 where
    put     = putWord8
    get     = getWord8

-- Words16s are written as 2 bytes in big-endian (network) order
instance Serialize Word16 where
    put     = putWord16be
    get     = getWord16be

-- Words32s are written as 4 bytes in big-endian (network) order
instance Serialize Word32 where
    put     = putWord32be
    get     = getWord32be

-- Words64s are written as 8 bytes in big-endian (network) order
instance Serialize Word64 where
    put     = putWord64be
    get     = getWord64be

-- Int8s are written as a single byte.
instance Serialize Int8 where
    put i   = put (fromIntegral i :: Word8)
    get     = liftM fromIntegral (get :: Get Word8)

-- Int16s are written as a 2 bytes in big endian format
instance Serialize Int16 where
    put i   = put (fromIntegral i :: Word16)
    get     = liftM fromIntegral (get :: Get Word16)

-- Int32s are written as a 4 bytes in big endian format
instance Serialize Int32 where
    put i   = put (fromIntegral i :: Word32)
    get     = liftM fromIntegral (get :: Get Word32)

-- Int64s are written as a 8 bytes in big endian format
instance Serialize Int64 where
    put i   = put (fromIntegral i :: Word64)
    get     = liftM fromIntegral (get :: Get Word64)

------------------------------------------------------------------------

-- Words are are written as Word64s, that is, 8 bytes in big endian format
instance Serialize Word where
    put i   = put (fromIntegral i :: Word64)
    get     = liftM fromIntegral (get :: Get Word64)

-- Ints are are written as Int64s, that is, 8 bytes in big endian format
instance Serialize Int where
    put i   = put (fromIntegral i :: Int64)
    get     = liftM fromIntegral (get :: Get Int64)

------------------------------------------------------------------------
-- 
-- Portable, and pretty efficient, serialisation of Integer
--

-- Fixed-size type for a subset of Integer
type SmallInt = Int32

-- Integers are encoded in two ways: if they fit inside a SmallInt,
-- they're written as a byte tag, and that value.  If the Integer value
-- is too large to fit in a SmallInt, it is written as a byte array,
-- along with a sign and length field.

instance Serialize Integer where

    put n | n >= lo && n <= hi = do
        putWord8 0
        put (fromIntegral n :: SmallInt)  -- fast path
     where
        lo = fromIntegral (minBound :: SmallInt) :: Integer
        hi = fromIntegral (maxBound :: SmallInt) :: Integer

    put n = do
        putWord8 1
        put sign
        put (unroll (abs n))         -- unroll the bytes
     where
        sign = fromIntegral (signum n) :: Word8

    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> liftM fromIntegral (get :: Get SmallInt)
            _ -> do sign  <- get
                    bytes <- get
                    let v = roll bytes
                    return $! if sign == (1 :: Word8) then v else - v

--
-- Fold and unfold an Integer to and from a list of its bytes
--
unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

instance (Serialize a,Integral a) => Serialize (R.Ratio a) where
    put r = put (R.numerator r) >> put (R.denominator r)
    get = liftM2 (R.%) get get

------------------------------------------------------------------------

-- Char is serialised as UTF-8
instance Serialize Char where
    put a | c <= 0x7f     = put (fromIntegral c :: Word8)
          | c <= 0x7ff    = do put (0xc0 .|. y)
                               put (0x80 .|. z)
          | c <= 0xffff   = do put (0xe0 .|. x)
                               put (0x80 .|. y)
                               put (0x80 .|. z)
          | c <= 0x10ffff = do put (0xf0 .|. w)
                               put (0x80 .|. x)
                               put (0x80 .|. y)
                               put (0x80 .|. z)
          | otherwise     = error "Not a valid Unicode code point"
     where
        c = ord a
        z, y, x, w :: Word8
        z = fromIntegral (c           .&. 0x3f)
        y = fromIntegral (shiftR c 6  .&. 0x3f)
        x = fromIntegral (shiftR c 12 .&. 0x3f)
        w = fromIntegral (shiftR c 18 .&. 0x7)

    get = do
        let getByte = liftM (fromIntegral :: Word8 -> Int) get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    y <- liftM (xor 0x80) getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- liftM (xor 0x80) getByte
                                y <- liftM (xor 0x80) getByte
                                z <- liftM (xor 0x80) getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0xf0 w))))
        return $! chr r

------------------------------------------------------------------------
-- Instances for the first few tuples

instance (Serialize a, Serialize b) => Serialize (a,b) where
    put = putTwoOf put put
    get = getTwoOf get get

instance (Serialize a, Serialize b, Serialize c) => Serialize (a,b,c) where
    put (a,b,c)         = put a >> put b >> put c
    get                 = liftM3 (,,) get get get

instance (Serialize a, Serialize b, Serialize c, Serialize d)
        => Serialize (a,b,c,d) where
    put (a,b,c,d)       = put a >> put b >> put c >> put d
    get                 = liftM4 (,,,) get get get get

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e)
        => Serialize (a,b,c,d,e) where
    put (a,b,c,d,e)     = put a >> put b >> put c >> put d >> put e
    get                 = liftM5 (,,,,) get get get get get

-- 
-- and now just recurse:
--

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e
         , Serialize f)
        => Serialize (a,b,c,d,e,f) where
    put (a,b,c,d,e,f)   = put (a,(b,c,d,e,f))
    get                 = do (a,(b,c,d,e,f)) <- get ; return (a,b,c,d,e,f)

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e
         , Serialize f, Serialize g)
        => Serialize (a,b,c,d,e,f,g) where
    put (a,b,c,d,e,f,g) = put (a,(b,c,d,e,f,g))
    get                 = do (a,(b,c,d,e,f,g)) <- get ; return (a,b,c,d,e,f,g)

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e,
          Serialize f, Serialize g, Serialize h)
        => Serialize (a,b,c,d,e,f,g,h) where
    put (a,b,c,d,e,f,g,h) = put (a,(b,c,d,e,f,g,h))
    get                   = do (a,(b,c,d,e,f,g,h)) <- get
                               return (a,b,c,d,e,f,g,h)

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e,
          Serialize f, Serialize g, Serialize h, Serialize i)
        => Serialize (a,b,c,d,e,f,g,h,i) where
    put (a,b,c,d,e,f,g,h,i) = put (a,(b,c,d,e,f,g,h,i))
    get                     = do (a,(b,c,d,e,f,g,h,i)) <- get
                                 return (a,b,c,d,e,f,g,h,i)

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e,
          Serialize f, Serialize g, Serialize h, Serialize i, Serialize j)
        => Serialize (a,b,c,d,e,f,g,h,i,j) where
    put (a,b,c,d,e,f,g,h,i,j) = put (a,(b,c,d,e,f,g,h,i,j))
    get                       = do (a,(b,c,d,e,f,g,h,i,j)) <- get
                                   return (a,b,c,d,e,f,g,h,i,j)

------------------------------------------------------------------------
-- Container types

instance Serialize a => Serialize [a] where
    put = putListOf put
    get = getListOf get

instance (Serialize a) => Serialize (Maybe a) where
    put = putMaybeOf put
    get = getMaybeOf get

instance (Serialize a, Serialize b) => Serialize (Either a b) where
    put = putEitherOf put put
    get = getEitherOf get get

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Serialize B.ByteString where
    put bs = do put (B.length bs)
                putByteString bs
    get    = get >>= getByteString

instance Serialize L.ByteString where
    put bs = do put (fromIntegral (L.length bs) :: Int)
                putLazyByteString bs
    get    = get >>= getLazyByteString


------------------------------------------------------------------------
-- Maps and Sets

instance (Ord a, Serialize a) => Serialize (Set.Set a) where
    put = putSetOf put
    get = getSetOf get

instance (Ord k, Serialize k, Serialize e) => Serialize (Map.Map k e) where
    put = putMapOf put put
    get = getMapOf get get

instance Serialize IntSet.IntSet where
    put = putIntSetOf put
    get = getIntSetOf get

instance (Serialize e) => Serialize (IntMap.IntMap e) where
    put = putIntMapOf put put
    get = getIntMapOf get get

------------------------------------------------------------------------
-- Queues and Sequences

instance (Serialize e) => Serialize (Seq.Seq e) where
    put = putSeqOf put
    get = getSeqOf get

------------------------------------------------------------------------
-- Floating point

instance Serialize Double where
    put d = put (decodeFloat d)
    get   = liftM2 encodeFloat get get

instance Serialize Float where
    put f = put (decodeFloat f)
    get   = liftM2 encodeFloat get get

------------------------------------------------------------------------
-- Trees

instance (Serialize e) => Serialize (T.Tree e) where
    put = putTreeOf put
    get = getTreeOf get

------------------------------------------------------------------------
-- Arrays

instance (Serialize i, Ix i, Serialize e) => Serialize (Array i e) where
    put = putIArrayOf put put
    get = getIArrayOf get get

--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (Serialize i, Ix i, Serialize e, IArray UArray e)
  => Serialize (UArray i e) where
    put = putIArrayOf put put
    get = getIArrayOf get get
