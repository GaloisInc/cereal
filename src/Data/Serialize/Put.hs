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
    , putBuilder
    , execPut

    -- * Flushing the implicit parse state
    , flush

    -- * Primitives
    , putWord8
    , putByteString
    , putLazyByteString

    -- * Big-endian primitives
    , putWord16be
    , putWord32be
    , putWord64be

    -- * Little-endian primitives
    , putWord16le
    , putWord32le
    , putWord64le

    -- * Host-endian, unaligned writes
    , putWordhost
    , putWord16host
    , putWord32host
    , putWord64host

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

  ) where

import Data.Serialize.Builder (Builder, toByteString, toLazyByteString)
import qualified Data.Serialize.Builder as B

import Control.Applicative
import Data.Array.Unboxed
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Word
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import qualified Data.IntMap            as IntMap
import qualified Data.IntSet            as IntSet
import qualified Data.Map               as Map
import qualified Data.Sequence          as Seq
import qualified Data.Set               as Set
import qualified Data.Tree              as T


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


instance Applicative PutM where
        pure    = return
        {-# INLINE pure #-}

        m <*> k = Put $
            let PairS f w  = unPut m
                PairS x w' = unPut k
            in PairS (f x) (w `mappend` w')
        {-# INLINE (<*>) #-}


instance Monad PutM where
    return a = Put (PairS a mempty)
    {-# INLINE return #-}

    m >>= k  = Put $
        let PairS a w  = unPut m
            PairS b w' = unPut (k a)
        in PairS b (w `mappend` w')
    {-# INLINE (>>=) #-}

    m >> k  = Put $
        let PairS _ w  = unPut m
            PairS b w' = unPut k
        in PairS b (w `mappend` w')
    {-# INLINE (>>) #-}

tell :: Putter Builder
tell b = Put $ PairS () b
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
runPut = toByteString . sndS . unPut
{-# INLINE runPut #-}

-- | Run the 'Put' monad with a serialiser and get its result
runPutM :: PutM a -> (a, S.ByteString)
runPutM (Put (PairS f s)) = (f, toByteString s)
{-# INLINE runPutM #-}

-- | Run the 'Put' monad with a serialiser
runPutLazy :: Put -> L.ByteString
runPutLazy = toLazyByteString . sndS . unPut
{-# INLINE runPutLazy #-}

-- | Run the 'Put' monad with a serialiser
runPutMLazy :: PutM a -> (a, L.ByteString)
runPutMLazy (Put (PairS f s)) = (f, toLazyByteString s)
{-# INLINE runPutMLazy #-}

------------------------------------------------------------------------

-- | Pop the ByteString we have constructed so far, if any, yielding a
-- new chunk in the result ByteString.
flush               :: Put
flush               = tell B.flush
{-# INLINE flush #-}

-- | Efficiently write a byte into the output buffer
putWord8            :: Putter Word8
putWord8            = tell . B.singleton
{-# INLINE putWord8 #-}

-- | An efficient primitive to write a strict ByteString into the output buffer.
-- It flushes the current buffer, and writes the argument into a new chunk.
putByteString       :: Putter S.ByteString
putByteString       = tell . B.fromByteString
{-# INLINE putByteString #-}

-- | Write a lazy ByteString efficiently, simply appending the lazy
-- ByteString chunks to the output buffer
putLazyByteString   :: Putter L.ByteString
putLazyByteString   = tell . B.fromLazyByteString
{-# INLINE putLazyByteString #-}

-- | Write a Word16 in big endian format
putWord16be         :: Putter Word16
putWord16be         = tell . B.putWord16be
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le         :: Putter Word16
putWord16le         = tell . B.putWord16le
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
putWord32be         :: Putter Word32
putWord32be         = tell . B.putWord32be
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le         :: Putter Word32
putWord32le         = tell . B.putWord32le
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64be         :: Putter Word64
putWord64be         = tell . B.putWord64be
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le         :: Putter Word64
putWord64le         = tell . B.putWord64le
{-# INLINE putWord64le #-}

------------------------------------------------------------------------

-- | /O(1)./ Write a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost         :: Putter Word
putWordhost         = tell . B.putWordhost
{-# INLINE putWordhost #-}

-- | /O(1)./ Write a Word16 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord16host       :: Putter Word16
putWord16host       = tell . B.putWord16host
{-# INLINE putWord16host #-}

-- | /O(1)./ Write a Word32 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord32host       :: Putter Word32
putWord32host       = tell . B.putWord32host
{-# INLINE putWord32host #-}

-- | /O(1)./ Write a Word64 in native host order
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- For portability issues see @putWordhost@.
putWord64host       :: Putter Word64
putWord64host       = tell . B.putWord64host
{-# INLINE putWord64host #-}


-- Containers ------------------------------------------------------------------

encodeListOf :: (a -> Builder) -> [a] -> Builder
encodeListOf f = -- allow inlining with just a single argument
    \xs ->  execPut (putWord64be (fromIntegral $ length xs)) `mappend`
            foldMap f xs
{-# INLINE encodeListOf #-}

putTwoOf :: Putter a -> Putter b -> Putter (a,b)
putTwoOf pa pb (a,b) = pa a >> pb b
{-# INLINE putTwoOf #-}

putListOf :: Putter a -> Putter [a]
putListOf pa = tell . encodeListOf (execPut . pa)
{-# INLINE putListOf #-}

putIArrayOf :: (Ix i, IArray a e) => Putter i -> Putter e -> Putter (a i e)
putIArrayOf pix pe a = do
  putTwoOf pix pix (bounds a)
  putListOf pe (elems a)
{-# INLINE putIArrayOf #-}

putSeqOf :: Putter a -> Putter (Seq.Seq a)
putSeqOf pa = \s -> do
    putWord64be (fromIntegral $ Seq.length s) 
    tell (foldMap (execPut . pa) s)
{-# INLINE putSeqOf #-}

putTreeOf :: Putter a -> Putter (T.Tree a)
putTreeOf pa = 
    tell . go
  where
    go (T.Node x cs) = execPut (pa x) `mappend` encodeListOf go cs
{-# INLINE putTreeOf #-}

putMapOf :: Ord k => Putter k -> Putter a -> Putter (Map.Map k a)
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
