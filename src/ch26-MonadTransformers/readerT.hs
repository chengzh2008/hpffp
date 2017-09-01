import Control.Monad.Trans
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  -- fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ pure (pure a)
  ReaderT rmfa <*> ReaderT rma = ReaderT $ (<*>) <$> rmfa <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  -- rma :: r -> m a
  -- f :: a -> ReaderT (r -> m b)
  ReaderT rma >>= f = ReaderT $ do
    \r -> do
      v <- rma r
      runReaderT (f v) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const


instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO
