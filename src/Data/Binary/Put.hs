module Data.Binary.Put (

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

import Data.Binary.Safe.Put hiding (runPut,runPutM)
import qualified Data.Binary.Safe.Put as Safe
import qualified Data.ByteString.Lazy as L


runPut :: Put -> L.ByteString
runPut m = L.fromChunks [Safe.runPut m]

runPutM :: PutM a -> (a, L.ByteString)
runPutM m = (a, L.fromChunks [bs])
  where (a,bs) = Safe.runPutM m
