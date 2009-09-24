module Data.Binary.Compat.Get (
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

import Data.Binary.Safe.Get hiding (runGet,runGetState)
import qualified Data.Binary.Safe.Get as Safe
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L


runGet :: Get a -> L.ByteString -> Either String a
runGet m bs = Safe.runGet m (S.concat (L.toChunks bs))

runGetState :: Get a -> L.ByteString -> Int -> Either String (a, L.ByteString)
runGetState m bs off = case Safe.runGetState m (S.concat (L.toChunks bs)) off of
  Left err      -> Left err
  Right (a,bs') -> Right (a, L.fromChunks [bs'])
