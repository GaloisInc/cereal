{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- A monad-transformer over "Data.Serialize.Put".
module Data.SerializeM.PutT
  (
    PutT,
    run,
    MonadPut(..)
  )
  where

import Data.Serialize.Put
import Control.Monad.Writer
import Control.Applicative



-- | A serialization monad transformer.
-- Useful for mutable types, which live in monads like `IO`.
newtype PutT m a = PutT (WriterT Put m a) 
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadPlus, Alternative)



class MonadPut m where
  liftPut :: Put -> m ()

instance MonadPut PutM where
  liftPut = id

instance Monad m => MonadPut (PutT m) where
  liftPut put = PutT $ tell $ put



run :: Monad m => PutT m a -> m (PutM a)
run (PutT w) = do
  (r, put) <- runWriterT w
  return $ put >> return r
