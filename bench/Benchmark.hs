{-# LANGUAGE GADTs, PackageImports, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark encoding and decoding speed.
module Main (main) where

import           Prelude hiding (words)
import           Criterion.Main

import qualified Data.ByteString.Lazy as L
import           Data.Serialize

import           Data.Binary (Binary)
import qualified Data.Binary as Binary


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- input data (NOINLINE to ensure memoization)
----------------------------------------------

-- | Few-enough repetitions to avoid making GC too expensive.
nRepl :: Int
nRepl = 1000

{-# NOINLINE intData #-}
intData :: [Int]
intData = [start..start +nRepl]
  where start = 0

{-# NOINLINE stringData #-}
stringData :: [String]
stringData = take nRepl $ cycle ["hello", "world"]

-- benchmarks
-------------

main :: IO ()
main = Criterion.Main.defaultMain $ 
    [ benchmarks "[Int]"    intData
    , benchmarks "[String]" stringData
    ]
  where
    benchmarks :: (Binary a, Serialize a) => String -> a -> Benchmark
    benchmarks name x = bgroup (name ++ show nRepl)
      [ bench "cereal" $ whnf (L.length . encodeLazy) x
      , bench "binary" $ whnf (L.length . Binary.encode) x
      ]
