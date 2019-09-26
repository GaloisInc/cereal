{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures
           , TypeOperators
           , BangPatterns
           , KindSignatures
           , ScopedTypeVariables #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

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

    , expect
    , module Data.Serialize.Get
    , module Data.Serialize.Put
    , module Data.Serialize.IEEE754

    -- * Generic deriving
    , GSerializePut(..)
    , GSerializeGet(..)
    ) where

import Data.Serialize.Put
import Data.Serialize.Get
import Data.Serialize.IEEE754

import Control.Monad
import Data.Array.Unboxed
import Data.ByteString (ByteString)
import Data.Char    (chr,ord)
import Data.List    (unfoldr)
import Data.Word
import Foreign

-- And needed for the instances:
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Short as S
import qualified Data.Map              as Map
import qualified Data.Monoid           as M
import qualified Data.Set              as Set
import qualified Data.IntMap           as IntMap
import qualified Data.IntSet           as IntSet
import qualified Data.Ratio            as R
import qualified Data.Tree             as T
import qualified Data.Sequence         as Seq

import GHC.Generics

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((*>),(<*>),(<$>),pure)
#endif

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural
#endif

------------------------------------------------------------------------


-- | If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@), the 'put' and 'get'
-- methods will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause to your datatype
-- and declare a 'Serialize' instance for it without giving a definition for
-- 'put' and 'get'.
class Serialize t where
    -- | Encode a value in the Put monad.
    put :: Putter t
    -- | Decode a value in the Get monad
    get :: Get t

    default put :: (Generic t, GSerializePut (Rep t)) => Putter t
    put = gPut . from

    default get :: (Generic t, GSerializeGet (Rep t)) => Get t
    get = to <$> gGet

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
-- Combinators

-- | Perform an action, failing if the read result does not match the argument
--   provided.
expect :: (Eq a, Serialize a) => a -> Get a
expect x = get >>= \y -> if x == y then return x else mzero


------------------------------------------------------------------------
-- Simple instances

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Serialize () where
    put ()  = return ()
    get     = return ()

{-# INLINE boolToWord8 #-}
boolToWord8 :: Bool -> Word8
boolToWord8 False = 0
boolToWord8 True = 1

{-# INLINE boolFromWord8 #-}
boolFromWord8 :: Word8 -> Get Bool
boolFromWord8 0 = return False
boolFromWord8 1 = return True
boolFromWord8 w = fail ("Invalid Bool encoding " ++ show w)

{-# INLINE orderingToWord8 #-}
orderingToWord8 :: Ordering -> Word8
orderingToWord8 LT = 0
orderingToWord8 EQ = 1
orderingToWord8 GT = 2

{-# INLINE orderingFromWord8 #-}
orderingFromWord8 :: Word8 -> Get Ordering
orderingFromWord8 0 = return LT
orderingFromWord8 1 = return EQ
orderingFromWord8 2 = return GT
orderingFromWord8 w = fail ("Invalid Ordering encoding " ++ show w)

-- Bools are encoded as a byte in the range 0 .. 1
instance Serialize Bool where
    put     = putWord8 . boolToWord8
    get     = boolFromWord8 =<< getWord8

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Serialize Ordering where
    put     = putWord8 . orderingToWord8
    get     = orderingFromWord8 =<< getWord8

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
    put     = putInt8
    get     = getInt8

-- Int16s are written as a 2 bytes in big endian format
instance Serialize Int16 where
    put     = putInt16be
    get     = getInt16be

-- Int32s are written as a 4 bytes in big endian format
instance Serialize Int32 where
    put     = putInt32be
    get     = getInt32be

-- Int64s are written as a 8 bytes in big endian format
instance Serialize Int64 where
    put     = putInt64be
    get     = getInt64be

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
        let len = ((nrBits (abs n) + 7) `div` 8)
        putWord64be (fromIntegral len)
        mapM_ put (unroll (abs n))         -- unroll the bytes
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
unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

nrBits :: (Ord a, Integral a) => a -> Int
nrBits k =
    let expMax = until (\e -> 2 ^ e > k) (* 2) 1
        findNr :: Int -> Int -> Int
        findNr lo hi
            | mid == lo    = hi
            | 2 ^ mid <= k = findNr mid hi
            | otherwise    = findNr lo mid
         where mid = (lo + hi) `div` 2
    in findNr (expMax `div` 2) expMax

instance (Serialize a,Integral a) => Serialize (R.Ratio a) where
    put r = put (R.numerator r) >> put (R.denominator r)
    get = liftM2 (R.%) get get

#if MIN_VERSION_base(4,8,0)
-- Fixed-size type for a subset of Natural
type NaturalWord = Word64

instance Serialize Natural where
    {-# INLINE put #-}
    put n | n <= hi = do
        putWord8 0
        put (fromIntegral n :: NaturalWord)  -- fast path
     where
        hi = fromIntegral (maxBound :: NaturalWord) :: Natural

    put n = do
        putWord8 1
        let len = ((nrBits (abs n) + 7) `div` 8)
        putWord64be (fromIntegral len)
        mapM_ put (unroll (abs n))         -- unroll the bytes

    {-# INLINE get #-}
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> liftM fromIntegral (get :: Get NaturalWord)
            _ -> do bytes <- get
                    return $! roll bytes
#endif

------------------------------------------------------------------------

-- Safely wrap `chr` to avoid exceptions.
-- `chr` source: http://hackage.haskell.org/package/base-4.7.0.2/docs/src/GHC-Char.html#chr
chrEither :: Int -> Either String Char
chrEither i
  | i <= 0x10FFFF = Right (chr i) -- Or: C# (chr# i#)
  | otherwise =
     Left ("bad argument: " ++ show i)

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
        case chrEither r of
            Right r' ->
                return $! r'
            Left err ->
                fail err

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
-- Monoid newtype wrappers

instance Serialize a => Serialize (M.Dual a) where
    put = put . M.getDual
    get = fmap M.Dual get

instance Serialize M.All where
    put = put . M.getAll
    get = fmap M.All get

instance Serialize M.Any where
    put = put . M.getAny
    get = fmap M.Any get

instance Serialize a => Serialize (M.Sum a) where
    put = put . M.getSum
    get = fmap M.Sum get

instance Serialize a => Serialize (M.Product a) where
    put = put . M.getProduct
    get = fmap M.Product get

instance Serialize a => Serialize (M.First a) where
    put = put . M.getFirst
    get = fmap M.First get

instance Serialize a => Serialize (M.Last a) where
    put = put . M.getLast
    get = fmap M.Last get

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
    put bs = do put (B.length bs :: Int)
                putByteString bs
    get    = get >>= getByteString

instance Serialize L.ByteString where
    put bs = do put (L.length bs :: Int64)
                putLazyByteString bs
    get    = get >>= getLazyByteString

instance Serialize S.ShortByteString where
    put sbs = do put (S.length sbs)
                 putShortByteString sbs
    get     = get >>= getShortByteString


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
    put = putFloat64be
    get = getFloat64be

instance Serialize Float where
    put = putFloat32be
    get = getFloat32be

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

------------------------------------------------------------------------
-- Generic Serialze

class GSerializePut f where
    gPut :: Putter (f a)

class GSerializeGet f where
    gGet :: Get (f a)

instance GSerializePut a => GSerializePut (M1 i c a) where
    gPut = gPut . unM1
    {-# INLINE gPut #-}

instance GSerializeGet a => GSerializeGet (M1 i c a) where
    gGet = M1 <$> gGet
    {-# INLINE gGet #-}

instance Serialize a => GSerializePut (K1 i a) where
    gPut = put . unK1
    {-# INLINE gPut #-}

instance Serialize a => GSerializeGet (K1 i a) where
    gGet = K1 <$> get
    {-# INLINE gGet #-}

instance GSerializePut U1 where
    gPut _ = pure ()
    {-# INLINE gPut #-}

instance GSerializeGet U1 where
    gGet   = pure U1
    {-# INLINE gGet #-}

-- | Always fails to serialize
instance GSerializePut V1 where
    gPut v = v `seq` error "GSerializePut.V1"
    {-# INLINE gPut #-}

-- | Always fails to deserialize
instance GSerializeGet V1 where
    gGet   = fail "GSerializeGet.V1"
    {-# INLINE gGet #-}

instance (GSerializePut a, GSerializePut b) => GSerializePut (a :*: b) where
    gPut (a :*: b) = gPut a *> gPut b
    {-# INLINE gPut #-}

instance (GSerializeGet a, GSerializeGet b) => GSerializeGet (a :*: b) where
    gGet = (:*:) <$> gGet  <*> gGet
    {-# INLINE gGet #-}

-- The following GSerialize* instance for sums has support for serializing types
-- with up to 2^64-1 constructors. It will use the minimal number of bytes
-- needed to encode the constructor. For example when a type has 2^8
-- constructors or less it will use a single byte to encode the constructor. If
-- it has 2^16 constructors or less it will use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = putSum (0 :: WORD) (fromIntegral size)
#define GETSUM(WORD) GUARD(WORD) = (get :: Get WORD) >>= checkGetSum (fromIntegral size)

instance ( PutSum        a, PutSum        b
         , SumSize       a, SumSize       b) => GSerializePut (a :+: b) where
    gPut | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE gPut #-}

instance ( GetSum        a, GetSum        b
         , SumSize       a, SumSize       b) => GSerializeGet (a :+: b) where
    gGet | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE gGet #-}

sizeError :: Show size => String -> size -> error
sizeError s size = error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

class PutSum f where
    putSum :: (Num word, Bits word, Serialize word) => word -> word -> Putter (f a)

instance (PutSum a, PutSum b) => PutSum (a :+: b) where
    putSum !code !size s = case s of
                             L1 x -> putSum code           sizeL x
                             R1 x -> putSum (code + sizeL) sizeR x
        where
#if MIN_VERSION_base(4,5,0)
          sizeL = size `unsafeShiftR` 1
#else
          sizeL = size `shiftR` 1
#endif
          sizeR = size - sizeL
    {-# INLINE putSum #-}

instance GSerializePut a => PutSum (C1 c a) where
    putSum !code _ x = put code *> gPut x
    {-# INLINE putSum #-}

------------------------------------------------------------------------

checkGetSum :: (Ord word, Num word, Bits word, GetSum f)
            => word -> word -> Get (f a)
checkGetSum size code | code < size = getSum code size
                      | otherwise   = fail "Unknown encoding for constructor"
{-# INLINE checkGetSum #-}

class GetSum f where
    getSum :: (Ord word, Num word, Bits word) => word -> word -> Get (f a)

instance (GetSum a, GetSum b) => GetSum (a :+: b) where
    getSum !code !size | code < sizeL = L1 <$> getSum code           sizeL
                       | otherwise    = R1 <$> getSum (code - sizeL) sizeR
        where
#if MIN_VERSION_base(4,5,0)
          sizeL = size `unsafeShiftR` 1
#else
          sizeL = size `shiftR` 1
#endif
          sizeR = size - sizeL
    {-# INLINE getSum #-}

instance GSerializeGet a => GetSum (C1 c a) where
    getSum _ _ = gGet
    {-# INLINE getSum #-}

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word64

newtype Tagged (s :: * -> *) b = Tagged {unTagged :: b}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                       unTagged (sumSize :: Tagged b Word64)

instance SumSize (C1 c a) where
    sumSize = Tagged 1
