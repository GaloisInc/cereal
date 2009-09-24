module Data.Binary (
    Safe.Binary(..)

  , encode
  , decode
  ) where

import Data.Binary.Put (runPut)
import qualified Data.Binary.Safe     as Safe
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

encode :: Safe.Binary a => a -> L.ByteString
encode a = runPut (put a)

decode :: Safe.Binary a => L.ByteString -> Either String a
decode bs = Safe.decode (S.concat (L.toChunks bs))
