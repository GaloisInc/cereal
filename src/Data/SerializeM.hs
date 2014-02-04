{-# LANGUAGE MultiParamTypeClasses #-}
module Data.SerializeM where

import Data.SerializeM.GetT (GetT)
import Data.SerializeM.PutT (PutT)

-- |
-- A type which can serialize and deserialize in a monad. 
-- Useful for mutable types, which live in monads like `IO`.
class SerializeM a m where
  putT :: a -> PutT m ()
  getT :: GetT m a

