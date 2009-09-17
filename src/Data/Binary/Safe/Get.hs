{-# LANGUAGE CPP        #-}
{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Safe.Get
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Galois, Inc.
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
-- The Get monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings
--
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.Binary.Safe.Get (

    -- * The Get type
      Get
    , runGet
    , runGetState

    -- * Parsing
    , isolate
    , skip
    , uncheckedSkip
    , lookAhead
    , lookAheadM
    , lookAheadE
    , uncheckedLookAhead

    -- * Utility
    , bytesRead
    , getBytes
    , remaining
    , isEmpty

    -- * Parsing particular types
    , getWord8

    -- ** ByteStrings
    , getByteString
    , getRemaining

    -- ** Big-endian reads
    , getWord16be
    , getWord32be
    , getWord64be

    -- ** Little-endian reads
    , getWord16le
    , getWord32le
    , getWord64le

    -- ** Host-endian, unaligned reads
    , getWordhost
    , getWord16host
    , getWord32host
    , getWord64host

  ) where

import Control.Monad (unless,when,ap,MonadPlus(..))
import Data.Maybe (isNothing)

import qualified Data.ByteString as B

#ifdef BYTESTRING_IN_BASE
import qualified Data.ByteString.Base as B
#else
import qualified Data.ByteString.Internal as B
#endif

#ifdef APPLICATIVE_IN_BASE
import Control.Applicative (Applicative(..))
#endif

import Foreign

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
#endif

-- | The parse state
data S = S {-# UNPACK #-} !B.ByteString -- input
           {-# UNPACK #-} !Int          -- bytes read
           {-# UNPACK #-} !Int          -- bytes available

type Trace = [(Int,String)]

-- | The Get monad is an Exception and State monad.
newtype Get a = Get
  { unGet :: forall r. Trace -> S
                    -> (Trace -> S -> a -> Either String (r, S))
                    -> Either String (r, S) }

instance Functor Get where
    fmap f m = Get (\t0 s0 k -> unGet m t0 s0 (\t s a -> k t s (f a)))


#ifdef APPLICATIVE_IN_BASE
instance Applicative Get where
    pure  = return
    (<*>) = ap
#endif

-- Definition directly from Control.Monad.State.Strict
instance Monad Get where
    return a = Get (\t0 s0 k -> k t0 s0 a)
    m >>= f  = Get (\t0 s0 k -> unGet m t0 s0 (\t s a -> unGet (f a) t s k))
    fail     = failDesc

instance MonadPlus Get where
    mzero = Get (\_ _ _ -> Left "mzero")

    mplus a b = Get $ \t0 s0 k -> case unGet a t0 s0 k of
      Left _    -> unGet b t0 s0 k
      Right res -> Right res

------------------------------------------------------------------------

get :: Get S
get  = Get (\t s0 k -> k t s0 s0)

put :: S -> Get ()
put s = Get (\t _ k -> k t s ())

label :: String -> Get a -> Get a
label l m = Get (\t0 s0@(S _ p _) k -> unGet m ((p,l):t0) s0 k)

initState :: B.ByteString -> S
initState xs = mkState xs 0

mkState :: B.ByteString -> Int -> S
mkState xs off = S xs off (B.length xs)

finalK :: Trace -> S -> a -> Either String (a,S)
finalK _ s a = Right (a,s)

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> B.ByteString -> Either String a
runGet m str = case unGet m [] (initState str) finalK of
  Left  i      -> Left i
  Right (a, _) -> Right a

-- | Run the Get monad applies a 'get'-based parser on the input
-- ByteString. Additional to the result of get it returns the number of
-- consumed bytes and the rest of the input.
runGetState :: Get a -> B.ByteString -> Int
            -> Either String (a, B.ByteString, Int)
runGetState m str off =
    case unGet m [] (mkState str off) finalK of
      Left i                     -> Left i
      Right (a, ~(S s newOff _)) -> Right (a, s, newOff)

------------------------------------------------------------------------

-- | Isolate an action to operating within a fixed block of bytes.  The action
--   is required to consume all the bytes that it is isolated to.
isolate :: String -> Int -> Get a -> Get a
isolate l n m = label l $ do
  S s bytes left <- get
  let boundary = bytes + n
  unless (boundary <= left) (fail "not enough space left to isolate")
  put $! S s bytes boundary
  a             <- m
  S s' bytes' _ <- get
  unless (bytes' == boundary) (fail "not all bytes parsed in isolate")
  put $! S s' boundary left
  return a

failDesc :: String -> Get a
failDesc err = do
    S _ bytes _  <- get
    let fmt (p,l) = "\t" ++ show p ++ "\t" ++ l
    let msg t = concat
          [ "Failed reading at position ", show bytes
          , ", ", err, "\nFrom"
          , unlines (map fmt t) ]
    Get (\t _ _ -> Left (msg t))

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = readN n (const ())

-- | Skip ahead @n@ bytes. No error if there isn't enough bytes.
uncheckedSkip :: Int -> Get ()
uncheckedSkip n = do
    S s bytes left <- get
    let off = min (bytes + n) left
    put $! S (B.drop n s) off left

-- | Run @ga@, but return without consuming its input.
-- Fails if @ga@ fails.
lookAhead :: Get a -> Get a
lookAhead ga = do
    s <- get
    a <- ga
    put s
    return a

-- | Like 'lookAhead', but consume the input if @gma@ returns 'Just _'.
-- Fails if @gma@ fails.
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM gma = do
    s <- get
    ma <- gma
    when (isNothing ma) (put s)
    return ma

-- | Like 'lookAhead', but consume the input if @gea@ returns 'Right _'.
-- Fails if @gea@ fails.
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE gea = do
    s <- get
    ea <- gea
    case ea of
        Left _ -> put s
        _      -> return ()
    return ea

-- | Get the next up to @n@ bytes as a ByteString, without consuming them.
uncheckedLookAhead :: Int -> Get B.ByteString
uncheckedLookAhead n = do
    S s _ _ <- get
    return (B.take n s)

------------------------------------------------------------------------
-- Utility

-- | Get the total number of bytes read to this point.
bytesRead :: Get Int
bytesRead = do
    S _ b _ <- get
    return b

-- | Get the number of remaining unparsed bytes.
-- Useful for checking whether all input has been consumed.
-- Note that this forces the rest of the input.
remaining :: Get Int
remaining = do
    S _ _ left <- get
    return left

-- | Test whether all input has been consumed,
-- i.e. there are no remaining unparsed bytes.
isEmpty :: Get Bool
isEmpty = do
    S _ bytes left <- get
    return (bytes == left) -- compare bytes and left, so that this works within
                           -- isolation blocks.

------------------------------------------------------------------------
-- Utility with ByteStrings

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input.
getByteString :: Int -> Get B.ByteString
getByteString n = readN n id

getRemaining :: Get B.ByteString
getRemaining  = getByteString =<< remaining


------------------------------------------------------------------------
-- Helpers

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get B.ByteString
getBytes n = do
    S s bytes left <- get
    let off = bytes + n
    when (off > left) (fail ("too few bytes"))
    let (consume,rest) = B.splitAt n s
    put $! S rest off left
    return $! consume

-- Pull n bytes from the input, and apply a parser to those bytes,
-- yielding a value. If less than @n@ bytes are available, fail with an
-- error. This wraps @getBytes@.
readN :: Int -> (B.ByteString -> a) -> Get a
readN n f = f `fmap` getBytes n

------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: Storable a => Int -> Get a
getPtr n = do
    (fp,o,_) <- readN n B.toForeignPtr
    return . B.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)

------------------------------------------------------------------------

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = getPtr (sizeOf (undefined :: Word8))

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = do
    s <- readN 2 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.index` 1))

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = do
    s <- readN 2 id
    return $! (fromIntegral (s `B.index` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.index` 0) )

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = do
    s <- readN 4 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.index` 3) )

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = do
    s <- readN 4 id
    return $! (fromIntegral (s `B.index` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.index` 0) )

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = do
    s <- readN 8 id
    return $! (fromIntegral (s `B.index` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.index` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.index` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.index` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.index` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.index` 7) )

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = do
    s <- readN 8 id
    return $! (fromIntegral (s `B.index` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.index` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.index` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.index` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.index` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.index` 0) )

------------------------------------------------------------------------
-- Host-endian reads

-- | /O(1)./ Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Get Word
getWordhost = getPtr (sizeOf (undefined :: Word))

-- | /O(1)./ Read a 2 byte Word16 in native host order and host endianness.
getWord16host :: Get Word16
getWord16host = getPtr (sizeOf (undefined :: Word16))

-- | /O(1)./ Read a Word32 in native host order and host endianness.
getWord32host :: Get Word32
getWord32host = getPtr  (sizeOf (undefined :: Word32))

-- | /O(1)./ Read a Word64 in native host order and host endianess.
getWord64host   :: Get Word64
getWord64host = getPtr  (sizeOf (undefined :: Word64))

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
