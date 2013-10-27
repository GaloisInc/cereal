-- |
-- A monad-transformer over "Data.Serialize.Get".
module Data.SerializeM.GetT
  (
    GetT,
    run,
    Result(..),
    MonadGet(..)
  )
  where

import qualified Data.Serialize.Get as Get
import Control.Monad.Trans
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)


data Result m a = 
  Fail String ByteString |
  Partial (ByteString -> m (Result m a)) |
  Done a ByteString

-- | A deserialization monad transformer. 
-- Useful for mutable types, which live in monads like `IO`.
newtype GetT m a = GetT { run :: ByteString -> m (Result m a) }

instance (Monad m) => Monad (GetT m) where
  GetT runA >>= aToGetTB = GetT $ \bs -> runA bs >>= aToMB where
    aToMB a = case a of
      Fail msg bs -> return $ Fail msg bs
      Partial cont -> return $ Partial $ \bs -> cont bs >>= aToMB
      Done a' bs -> case aToGetTB a' of GetT runB -> runB bs
  return a = GetT $ \bs -> return $ Done a bs

instance MonadTrans GetT where
  lift m = GetT $ \bs -> m >>= \a -> return $ Done a bs

instance (MonadIO m) => MonadIO (GetT m) where
  liftIO = lift . liftIO

instance (Monad m) => Applicative (GetT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Functor (GetT m) where
  fmap = liftM



class MonadGet m where 
  liftGet :: Get.Get a -> m a

instance (Monad m) => MonadGet (GetT m) where 
  liftGet get = GetT $ \bs -> return $ convertResult $ Get.runGetPartial get bs where
    convertResult r = case r of
      Get.Fail m bs -> Fail m bs
      Get.Partial cont -> Partial $ \bs -> return $ convertResult $ cont bs
      Get.Done a bs -> Done a bs

instance MonadGet Get.Get where
  liftGet = id

