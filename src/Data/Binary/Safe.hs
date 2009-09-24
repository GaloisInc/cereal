{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Safe
-- Copyright   : Lennart Kolmodin, Galois Inc. 2009
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   :
-- Portability :
--
-- Binary serialisation of Haskell values to and from strict ByteStrings.
-- The Binary library provides methods for encoding Haskell values as
-- streams of bytes directly in memory. The resulting @ByteString@ can
-- then be written to disk, sent over the network, or futher processed
-- (for example, compressed with gzip).
--
-- Values are always encoded in network order (big endian) form, and
-- encoded data should be portable across machine endianess, word size,
-- or compiler version. For example, data encoded using the Binary class
-- could be written from GHC, and read back in Hugs.
--
-----------------------------------------------------------------------------

module Data.Binary.Safe (

    -- * The Binary class
      Binary(..)

    -- $example

    -- * The Get and Put monads
    , Get
    , Put

    -- * Useful helpers for writing instances
    , putWord8
    , getWord8

    -- * Binary serialisation
    , encode                    -- :: Binary a => a -> ByteString
    , decode                    -- :: Binary a => ByteString -> a

-- Lazy put and get
--  , lazyPut
--  , lazyGet

    , module Data.Word -- useful

    ) where

import Data.Word

import Data.Binary.Safe.Put
import Data.Binary.Safe.Get

import Control.Monad
import Foreign

import Data.ByteString (ByteString)

import Data.Char    (chr,ord)
import Data.List    (unfoldr)

-- And needed for the instances:
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.IntMap          as IntMap
import qualified Data.IntSet          as IntSet
import qualified Data.Ratio           as R
import qualified Data.Tree            as T

import Data.Array.Unboxed

--
-- This isn't available in older Hugs or older GHC
--
#if __GLASGOW_HASKELL__ >= 606
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
#endif

------------------------------------------------------------------------

-- | The @Binary@ class provides 'put' and 'get', methods to encode and
-- decode a Haskell value to a lazy ByteString. It mirrors the Read and
-- Show classes for textual representation of Haskell types, and is
-- suitable for serialising Haskell values to disk, over the network.
--
-- For parsing and generating simple external binary formats (e.g. C
-- structures), Binary may be used, but in general is not suitable
-- for complex protocols. Instead use the Put and Get primitives
-- directly.
--
-- Instances of Binary should satisfy the following property:
--
-- > decode . encode == id
--
-- That is, the 'get' and 'put' methods should be the inverse of each
-- other. A range of instances are provided for basic Haskell types. 
--
class Binary t where
    -- | Encode a value in the Put monad.
    put :: t -> Put
    -- | Decode a value in the Get monad
    get :: Get t

-- $example
-- To serialise a custom type, an instance of Binary for that type is
-- required. For example, suppose we have a data structure:
--
-- > data Exp = IntE Int
-- >          | OpE  String Exp Exp
-- >    deriving Show
--
-- We can encode values of this type into bytestrings using the
-- following instance, which proceeds by recursively breaking down the
-- structure to serialise:
--
-- > instance Binary Exp where
-- >       put (IntE i)          = do put (0 :: Word8)
-- >                                  put i
-- >       put (OpE s e1 e2)     = do put (1 :: Word8)
-- >                                  put s
-- >                                  put e1
-- >                                  put e2
-- > 
-- >       get = do t <- get :: Get Word8
-- >                case t of
-- >                     0 -> do i <- get
-- >                             return (IntE i)
-- >                     1 -> do s  <- get
-- >                             e1 <- get
-- >                             e2 <- get
-- >                             return (OpE s e1 e2)
--
-- Note how we write an initial tag byte to indicate each variant of the
-- data type.
--
-- We can simplify the writing of 'get' instances using monadic
-- combinators:
-- 
-- >       get = do tag <- getWord8
-- >                case tag of
-- >                    0 -> liftM  IntE get
-- >                    1 -> liftM3 OpE  get get get
--
-- The generation of Binary instances has been automated by a script
-- using Scrap Your Boilerplate generics. Use the script here:
--  <http://darcs.haskell.org/binary/tools/derive/BinaryDerive.hs>.
--
-- To derive the instance for a type, load this script into GHCi, and
-- bring your type into scope. Your type can then have its Binary
-- instances derived as follows:
--
-- > $ ghci -fglasgow-exts BinaryDerive.hs
-- > *BinaryDerive> :l Example.hs
-- > *Main> deriveM (undefined :: Drinks)
-- >
-- > instance Binary Main.Drinks where
-- >      put (Beer a) = putWord8 0 >> put a
-- >      put Coffee = putWord8 1
-- >      put Tea = putWord8 2
-- >      put EnergyDrink = putWord8 3
-- >      put Water = putWord8 4
-- >      put Wine = putWord8 5
-- >      put Whisky = putWord8 6
-- >      get = do
-- >        tag_ <- getWord8
-- >        case tag_ of
-- >          0 -> get >>= \a -> return (Beer a)
-- >          1 -> return Coffee
-- >          2 -> return Tea
-- >          3 -> return EnergyDrink
-- >          4 -> return Water
-- >          5 -> return Wine
-- >          6 -> return Whisky
-- >
--
-- To serialise this to a bytestring, we use 'encode', which packs the
-- data structure into a binary format, in a lazy bytestring
--
-- > > let e = OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
-- > > let v = encode e
--
-- Where 'v' is a binary encoded data structure. To reconstruct the
-- original data, we use 'decode'
--
-- > > decode v :: Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- The lazy ByteString that results from 'encode' can be written to
-- disk, and read from disk using Data.ByteString.Lazy IO functions,
-- such as hPutStr or writeFile:
--
-- > > writeFile "/tmp/exp.txt" (encode e)
--
-- And read back with:
--
-- > > readFile "/tmp/exp.txt" >>= return . decode :: IO Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- We can also directly serialise a value to and from a Handle, or a file:
-- 
-- > > v <- decodeFile  "/tmp/exp.txt" :: IO Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- And write a value to disk
--
-- > > encodeFile "/tmp/a.txt" v
--

------------------------------------------------------------------------
-- Wrappers to run the underlying monad

-- | Encode a value using binary serialisation to a lazy ByteString.
--
encode :: Binary a => a -> ByteString
encode = runPut . put

-- | Decode a value from a lazy ByteString, reconstructing the original structure.
--
decode :: Binary a => B.ByteString -> Either String a
decode = runGet get

------------------------------------------------------------------------
-- Lazy put and get

-- lazyPut :: (Binary a) => a -> Put
-- lazyPut a = put (encode a)

-- lazyGet :: (Binary a) => Get a
-- lazyGet = fmap decode get

------------------------------------------------------------------------
-- Simple instances

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Binary () where
    put ()  = return ()
    get     = return ()

-- Bools are encoded as a byte in the range 0 .. 1
instance Binary Bool where
    put     = putWord8 . fromIntegral . fromEnum
    get     = liftM (toEnum . fromIntegral) getWord8

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Binary Ordering where
    put     = putWord8 . fromIntegral . fromEnum
    get     = liftM (toEnum . fromIntegral) getWord8

------------------------------------------------------------------------
-- Words and Ints

-- Words8s are written as bytes
instance Binary Word8 where
    put     = putWord8
    get     = getWord8

-- Words16s are written as 2 bytes in big-endian (network) order
instance Binary Word16 where
    put     = putWord16be
    get     = getWord16be

-- Words32s are written as 4 bytes in big-endian (network) order
instance Binary Word32 where
    put     = putWord32be
    get     = getWord32be

-- Words64s are written as 8 bytes in big-endian (network) order
instance Binary Word64 where
    put     = putWord64be
    get     = getWord64be

-- Int8s are written as a single byte.
instance Binary Int8 where
    put i   = put (fromIntegral i :: Word8)
    get     = liftM fromIntegral (get :: Get Word8)

-- Int16s are written as a 2 bytes in big endian format
instance Binary Int16 where
    put i   = put (fromIntegral i :: Word16)
    get     = liftM fromIntegral (get :: Get Word16)

-- Int32s are written as a 4 bytes in big endian format
instance Binary Int32 where
    put i   = put (fromIntegral i :: Word32)
    get     = liftM fromIntegral (get :: Get Word32)

-- Int64s are written as a 4 bytes in big endian format
instance Binary Int64 where
    put i   = put (fromIntegral i :: Word64)
    get     = liftM fromIntegral (get :: Get Word64)

------------------------------------------------------------------------

-- Words are are written as Word64s, that is, 8 bytes in big endian format
instance Binary Word where
    put i   = put (fromIntegral i :: Word64)
    get     = liftM fromIntegral (get :: Get Word64)

-- Ints are are written as Int64s, that is, 8 bytes in big endian format
instance Binary Int where
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

instance Binary Integer where

    {-# INLINE put #-}
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

    {-# INLINE get #-}
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

instance (Binary a,Integral a) => Binary (R.Ratio a) where
    put r = put (R.numerator r) >> put (R.denominator r)
    get = liftM2 (R.%) get get

------------------------------------------------------------------------

-- Char is serialised as UTF-8
instance Binary Char where
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

instance (Binary a, Binary b) => Binary (a,b) where
    put (a,b)           = put a >> put b
    get                 = liftM2 (,) get get

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put (a,b,c)         = put a >> put b >> put c
    get                 = liftM3 (,,) get get get

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put (a,b,c,d)       = put a >> put b >> put c >> put d
    get                 = liftM4 (,,,) get get get get

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a,b,c,d,e) where
    put (a,b,c,d,e)     = put a >> put b >> put c >> put d >> put e
    get                 = liftM5 (,,,,) get get get get get

-- 
-- and now just recurse:
--

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f)
        => Binary (a,b,c,d,e,f) where
    put (a,b,c,d,e,f)   = put (a,(b,c,d,e,f))
    get                 = do (a,(b,c,d,e,f)) <- get ; return (a,b,c,d,e,f)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g)
        => Binary (a,b,c,d,e,f,g) where
    put (a,b,c,d,e,f,g) = put (a,(b,c,d,e,f,g))
    get                 = do (a,(b,c,d,e,f,g)) <- get ; return (a,b,c,d,e,f,g)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h)
        => Binary (a,b,c,d,e,f,g,h) where
    put (a,b,c,d,e,f,g,h) = put (a,(b,c,d,e,f,g,h))
    get                   = do (a,(b,c,d,e,f,g,h)) <- get ; return (a,b,c,d,e,f,g,h)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i)
        => Binary (a,b,c,d,e,f,g,h,i) where
    put (a,b,c,d,e,f,g,h,i) = put (a,(b,c,d,e,f,g,h,i))
    get                     = do (a,(b,c,d,e,f,g,h,i)) <- get ; return (a,b,c,d,e,f,g,h,i)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i, Binary j)
        => Binary (a,b,c,d,e,f,g,h,i,j) where
    put (a,b,c,d,e,f,g,h,i,j) = put (a,(b,c,d,e,f,g,h,i,j))
    get                       = do (a,(b,c,d,e,f,g,h,i,j)) <- get ; return (a,b,c,d,e,f,g,h,i,j)

------------------------------------------------------------------------
-- Container types

instance Binary a => Binary [a] where
    put l  = put (length l) >> mapM_ put l
    get    = do n <- get :: Get Int
                getMany n

-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

instance (Binary a) => Binary (Maybe a) where
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 >> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> liftM Just get

instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left  a) = putWord8 0 >> put a
    put (Right b) = putWord8 1 >> put b
    get = do
        w <- getWord8
        case w of
            0 -> liftM Left  get
            _ -> liftM Right get

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Binary B.ByteString where
    put bs = do put (B.length bs)
                putByteString bs
    get    = get >>= getByteString

instance Binary L.ByteString where
    put bs = do put (fromIntegral (L.length bs) :: Int)
                putLazyByteString bs
    get    = get >>= getLazyByteString


------------------------------------------------------------------------
-- Maps and Sets

instance (Ord a, Binary a) => Binary (Set.Set a) where
    put s = put (Set.size s) >> mapM_ put (Set.toAscList s)
    get   = liftM Set.fromDistinctAscList get

instance (Ord k, Binary k, Binary e) => Binary (Map.Map k e) where
    put m = put (Map.size m) >> mapM_ put (Map.toAscList m)
    get   = liftM Map.fromDistinctAscList get

instance Binary IntSet.IntSet where
    put s = put (IntSet.size s) >> mapM_ put (IntSet.toAscList s)
    get   = liftM IntSet.fromDistinctAscList get

instance (Binary e) => Binary (IntMap.IntMap e) where
    put m = put (IntMap.size m) >> mapM_ put (IntMap.toAscList m)
    get   = liftM IntMap.fromDistinctAscList get

------------------------------------------------------------------------
-- Queues and Sequences

#if __GLASGOW_HASKELL__ >= 606
--
-- This is valid Hugs, but you need the most recent Hugs
--

instance (Binary e) => Binary (Seq.Seq e) where
    put s = put (Seq.length s) >> Fold.mapM_ put s
    get = do n <- get :: Get Int
             rep Seq.empty n get
      where rep xs 0 _ = return $! xs
            rep xs n g = xs `seq` n `seq` do
                           x <- g
                           rep (xs Seq.|> x) (n-1) g

#endif

------------------------------------------------------------------------
-- Floating point

instance Binary Double where
    put d = put (decodeFloat d)
    get   = liftM2 encodeFloat get get

instance Binary Float where
    put f = put (decodeFloat f)
    get   = liftM2 encodeFloat get get

------------------------------------------------------------------------
-- Trees

instance (Binary e) => Binary (T.Tree e) where
    put (T.Node r s) = put r >> put s
    get = liftM2 T.Node get get

------------------------------------------------------------------------
-- Arrays

instance (Binary i, Ix i, Binary e) => Binary (Array i e) where
    put a = do
        put (bounds a)
        put (rangeSize $ bounds a) -- write the length
        mapM_ put (elems a)        -- now the elems.
    get = do
        bs <- get
        n  <- get                  -- read the length
        xs <- getMany n            -- now the elems.
        return (listArray bs xs)

--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (Binary i, Ix i, Binary e, IArray UArray e) => Binary (UArray i e) where
    put a = do
        put (bounds a)
        put (rangeSize $ bounds a) -- now write the length
        mapM_ put (elems a)
    get = do
        bs <- get
        n  <- get
        xs <- getMany n
        return (listArray bs xs)
