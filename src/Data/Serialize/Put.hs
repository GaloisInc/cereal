{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

#ifndef MIN_VERSION_bytestring
#define MIN_VERSION_bytestring(x,y,z) 0
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Serialize.Put
-- Copyright   : Lennart Kolmodin, Galois Inc. 2009
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   :
-- Portability :
--
-- The Put monad. A monad for efficiently constructing bytestrings.
--
-----------------------------------------------------------------------------

module Data.Serialize.Put (

    -- * The Put type
      Put
    , PutM(..)
    , Putter
    , runPut
    , runPutM
    , runPutLazy
    , runPutMLazy
    , runPutMBuilder
    , putBuilder
    , execPut

    -- * Flushing the implicit parse state
    , flush

    -- * Primitives
    , putWord8
    , putInt8
    , putByteString
    , putLazyByteString
    , putShortByteString

    -- * Big-endian primitives
    , putWord16be
    , putWord32be
    , putWord64be
    , putInt16be
    , putInt32be
    , putInt64be

    -- * Little-endian primitives
    , putWord16le
    , putWord32le
    , putWord64le
    , putInt16le
    , putInt32le
    , putInt64le

    -- * Host-endian, unaligned writes
    , putWordhost
    , putWord16host
    , putWord32host
    , putWord64host
    , putInthost
    , putInt16host
    , putInt32host
    , putInt64host

    -- * Containers
    , putTwoOf
    , putListOf
    , putIArrayOf
    , putSeqOf
    , putTreeOf
    , putMapOf
    , putIntMapOf
    , putSetOf
    , putIntSetOf
    , putMaybeOf
    , putEitherOf
    , putNested

  ) where


import           Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Short as BS

import qualified Control.Applicative as A
import Data.Array.Unboxed
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as M
#endif
import qualified Data.Monoid as M
import qualified Data.Foldable as F
import Data.Word
import Data.Int
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import qualified Data.IntMap            as IntMap
import qualified Data.IntSet            as IntSet
import qualified Data.Map               as Map
import qualified Data.Sequence          as Seq
import qualified Data.Set               as Set
import qualified Data.Tree              as T

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
import Data.Foldable (foldMap)
import Data.Monoid
#endif

#if !(MIN_VERSION_bytestring(0,10,0))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L
#endif

------------------------------------------------------------------------

-- XXX Strict in builder only.
data PairS a = PairS a !Builder

sndS :: PairS a -> Builder
sndS (PairS _ b) = b

-- | The PutM type. A Writer monad over the efficient Builder monoid.
newtype PutM a = Put { unPut :: PairS a }

-- | Put merely lifts Builder into a Writer monad, applied to ().
type Put = PutM ()

type Putter a = a -> Put

instance Functor PutM where
        fmap f m = Put $ let PairS a w = unPut m in PairS (f a) w
        {-# INLINE fmap #-}


instance A.Applicative PutM where
        pure a = Put (PairS a M.mempty)
        {-# INLINE pure #-}

        m <*> k = Put $
            let PairS f w  = unPut m
                PairS x w' = unPut k
            in PairS (f x) (w `M.mappend` w')
        {-# INLINE (<*>) #-}

        m *> k  = Put $
            let PairS _ w  = unPut m
                PairS b w' = unPut k
            in PairS b (w `M.mappend` w')
        {-# INLINE (*>) #-}


instance Monad PutM where
    return = pure
    {-# INLINE return #-}

    m >>= k  = Put $
        let PairS a w  = unPut m
            PairS b w' = unPut (k a)
        in PairS b (w `M.mappend` w')
    {-# INLINE (>>=) #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

#if MIN_VERSION_base(4,9,0)
instance M.Semigroup (PutM ()) where
    (<>) = (*>)
    {-# INLINE (<>) #-}
#endif

instance Monoid (PutM ()) where
    mempty = pure ()
    {-# INLINE mempty #-}

#if !(MIN_VERSION_base(4,11,0))
    mappend = (*>)
    {-# INLINE mappend #-}
#endif

tell :: Putter Builder
tell b = Put $! PairS () b
{-# INLINE tell #-}

putBuilder :: Putter Builder
putBuilder = tell
{-# INLINE putBuilder #-}

-- | Run the 'Put' monad
execPut :: PutM a -> Builder
execPut = sndS . unPut
{-# INLINE execPut #-}

-- | Run the 'Put' monad with a serialiser
runPut :: Put -> S.ByteString
runPut = lazyToStrictByteString . runPutLazy
{-# INLINE runPut #-}

-- | Run the 'Put' monad with a serialiser and get its result
runPutM :: PutM a -> (a, S.ByteString)
runPutM (Put (PairS f s)) = (f, lazyToStrictByteString (toLazyByteString s))
{-# INLINE runPutM #-}

-- | Run the 'Put' monad with a serialiser
runPutLazy :: Put -> L.ByteString
runPutLazy = toLazyByteString . sndS . unPut
{-# INLINE runPutLazy #-}

-- | Run the 'Put' monad with a serialiser
runPutMLazy :: PutM a -> (a, L.ByteString)
runPutMLazy (Put (PairS f s)) = (f, toLazyByteString s)
{-# INLINE runPutMLazy #-}

-- | Run the 'Put' monad and get the result and underlying 'Builder'
runPutMBuilder :: PutM a -> (a, Builder)
runPutMBuilder (Put (PairS f s)) = (f, s)
{-# INLINE runPutMBuilder #-}

------------------------------------------------------------------------

-- | Pop the ByteString we have constructed so far, if any, yielding a
-- new chunk in the result ByteString.
flush               :: Put
flush               = tell B.flush
{-# INLINE flush #-}

-- | Efficiently write a byte into the output buffer
putWord8            :: Putter Word8
putWord8            = tell . B.word8
{-# INLINE putWord8 #-}

-- | Efficiently write an int into the output buffer
putInt8             :: Putter Int8
putInt8             = tell . B.int8
{-# INLINE putInt8 #-}

-- | An efficient primitive to write a strict ByteString into the output buffer.
-- It flushes the current buffer, and writes the argument into a new chunk.
putByteString       :: Putter S.ByteString
putByteString       = tell . B.byteString
{-# INLINE putByteString #-}

putShortByteString  :: Putter BS.ShortByteString
putShortByteString   = tell . B.shortByteString

-- | Write a lazy ByteString efficiently, simply appending the lazy
-- ByteString chunks to the output buffer
putLazyByteString   :: Putter L.ByteString
putLazyByteString   = tell . B.lazyByteString
{-# INLINE putLazyByteString #-}

-- | Write a Word16 in big endian format
putWord16be         :: Putter Word16
putWord16be         = tell . B.word16BE
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le         :: Putter Word16
putWord16le         = tell . B.word16LE
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
putWord32be         :: Putter Word32
putWord32be         = tell . B.word32BE
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le         :: Putter Word32
putWord32le         = tell . B.word32LE
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64be         :: Putter Word64
putWord64be         = tell . B.word64BE
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le         :: Putter Word64
putWord64le         = tell . B.word64LE
{-# INLINE putWord64le #-}

------------------------------------------------------------------------

-- | /O(1)./ Write a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost         :: Putter Word
putWordhost         = tell . B.wordHost
{-# INLINE putWordhost #-}

-- | /O(1)./ Write a Word16 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord16host       :: Putter Word16
putWord16host       = tell . B.word16Host
{-# INLINE putWord16host #-}

-- | /O(1)./ Write a Word32 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord32host       :: Putter Word32
putWord32host       = tell . B.word32Host
{-# INLINE putWord32host #-}

-- | /O(1)./ Write a Word64 in native host order
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- For portability issues see @putWordhost@.
putWord64host       :: Putter Word64
putWord64host       = tell . B.word64Host
{-# INLINE putWord64host #-}

-- | Write a Int16 in big endian format
putInt16be         :: Putter Int16
putInt16be         = tell . B.int16BE
{-# INLINE putInt16be #-}

-- | Write a Int16 in little endian format
putInt16le         :: Putter Int16
putInt16le         = tell . B.int16LE
{-# INLINE putInt16le #-}

-- | Write a Int32 in big endian format
putInt32be         :: Putter Int32
putInt32be         = tell . B.int32BE
{-# INLINE putInt32be #-}

-- | Write a Int32 in little endian format
putInt32le         :: Putter Int32
putInt32le         = tell . B.int32LE
{-# INLINE putInt32le #-}

-- | Write a Int64 in big endian format
putInt64be         :: Putter Int64
putInt64be         = tell . B.int64BE
{-# INLINE putInt64be #-}

-- | Write a Int64 in little endian format
putInt64le         :: Putter Int64
putInt64le         = tell . B.int64LE
{-# INLINE putInt64le #-}

------------------------------------------------------------------------

-- | /O(1)./ Write a single native machine int. The int is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Int is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or int sized machines, without conversion.
--
putInthost         :: Putter Int
putInthost         = tell . B.intHost
{-# INLINE putInthost #-}

-- | /O(1)./ Write a Int16 in native host order and host endianness.
-- For portability issues see @putInthost@.
putInt16host       :: Putter Int16
putInt16host       = tell . B.int16Host
{-# INLINE putInt16host #-}

-- | /O(1)./ Write a Int32 in native host order and host endianness.
-- For portability issues see @putInthost@.
putInt32host       :: Putter Int32
putInt32host       = tell . B.int32Host
{-# INLINE putInt32host #-}

-- | /O(1)./ Write a Int64 in native host order
-- On a 32 bit machine we write two host order Int32s, in big endian form.
-- For portability issues see @putInthost@.
putInt64host       :: Putter Int64
putInt64host       = tell . B.int64Host
{-# INLINE putInt64host #-}


-- Containers ------------------------------------------------------------------

encodeListOf :: (a -> Builder) -> [a] -> Builder
encodeListOf f = -- allow inlining with just a single argument
    \xs ->  execPut (putWord64be (fromIntegral $ length xs)) `M.mappend`
            F.foldMap f xs
{-# INLINE encodeListOf #-}

putTwoOf :: Putter a -> Putter b -> Putter (a,b)
putTwoOf pa pb (a,b) = pa a >> pb b
{-# INLINE putTwoOf #-}

putListOf :: Putter a -> Putter [a]
putListOf pa = \l -> do
  putWord64be (fromIntegral (length l))
  mapM_ pa l
{-# INLINE putListOf #-}

putIArrayOf :: (Ix i, IArray a e) => Putter i -> Putter e -> Putter (a i e)
putIArrayOf pix pe a = do
  putTwoOf pix pix (bounds a)
  putListOf pe (elems a)
{-# INLINE putIArrayOf #-}

putSeqOf :: Putter a -> Putter (Seq.Seq a)
putSeqOf pa = \s -> do
    putWord64be (fromIntegral $ Seq.length s)
    F.mapM_ pa s
{-# INLINE putSeqOf #-}

putTreeOf :: Putter a -> Putter (T.Tree a)
putTreeOf pa =
    tell . go
  where
    go (T.Node x cs) = execPut (pa x) `M.mappend` encodeListOf go cs
{-# INLINE putTreeOf #-}

putMapOf :: Putter k -> Putter a -> Putter (Map.Map k a)
putMapOf pk pa = putListOf (putTwoOf pk pa) . Map.toAscList
{-# INLINE putMapOf #-}

putIntMapOf :: Putter Int -> Putter a -> Putter (IntMap.IntMap a)
putIntMapOf pix pa = putListOf (putTwoOf pix pa) . IntMap.toAscList
{-# INLINE putIntMapOf #-}

putSetOf :: Putter a -> Putter (Set.Set a)
putSetOf pa = putListOf pa . Set.toAscList
{-# INLINE putSetOf #-}

putIntSetOf :: Putter Int -> Putter IntSet.IntSet
putIntSetOf pix = putListOf pix . IntSet.toAscList
{-# INLINE putIntSetOf #-}

putMaybeOf :: Putter a -> Putter (Maybe a)
putMaybeOf _  Nothing  = putWord8 0
putMaybeOf pa (Just a) = putWord8 1 >> pa a
{-# INLINE putMaybeOf #-}

putEitherOf :: Putter a -> Putter b -> Putter (Either a b)
putEitherOf pa _  (Left a)  = putWord8 0 >> pa a
putEitherOf _  pb (Right b) = putWord8 1 >> pb b
{-# INLINE putEitherOf #-}

-- | Put a nested structure by first putting a length
--   field and then putting the encoded value.
putNested :: Putter Int -> Put -> Put
putNested putLen putVal = do
    let bs = runPut putVal
    putLen (S.length bs)
    putByteString bs

-------------------------------------------------------------------------------
-- pre-bytestring-0.10 compatibility
-------------------------------------------------------------------------------

{-# INLINE lazyToStrictByteString #-}
lazyToStrictByteString :: L.ByteString -> S.ByteString
#if MIN_VERSION_bytestring(0,10,0)
lazyToStrictByteString = L.toStrict
#else
lazyToStrictByteString = packChunks

-- packChunks is taken from the blaze-builder package.

-- | Pack the chunks of a lazy bytestring into a single strict bytestring.
packChunks :: L.ByteString -> S.ByteString
packChunks lbs = S.unsafeCreate (fromIntegral $ L.length lbs) (copyChunks lbs)
  where
    copyChunks !L.Empty                         !_pf = return ()
    copyChunks !(L.Chunk (S.PS fpbuf o l) lbs') !pf  = do
        withForeignPtr fpbuf $ \pbuf ->
            copyBytes pf (pbuf `plusPtr` o) l
        copyChunks lbs' (pf `plusPtr` l)
#endif
