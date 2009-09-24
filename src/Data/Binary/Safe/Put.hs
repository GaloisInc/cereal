--------------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Safe.Put
-- Copyright   : (c) Galois Inc. 2009
-- License     : BSD3
--
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   :
-- Portability :
--

module Data.Binary.Safe.Put (

    -- * The Put type
      Put
    , PutM(..)
    , runPut
    , runPutM
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
    , putWordhost           -- :: Word   -> Put
    , putWord16host         -- :: Word16 -> Put
    , putWord32host         -- :: Word32 -> Put
    , putWord64host         -- :: Word64 -> Put

  ) where

import Data.Binary.Compat.Put hiding (runPut,runPutM)
import qualified Data.Binary.Compat.Put as Bin
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L


-- | Run the Put monad.
runPut :: Put -> S.ByteString
runPut m = S.concat (L.toChunks (Bin.runPut m))

-- | Run the Put monad, and get its result.
runPutM :: PutM a -> (a, S.ByteString)
runPutM m = (a, S.concat (L.toChunks bs))
  where (a,bs) = Bin.runPutM m
