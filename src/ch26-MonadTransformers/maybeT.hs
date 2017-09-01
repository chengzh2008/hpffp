import Control.Monad.Trans
import Control.Monad.Identity

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

-- TODO: really digest it.
instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT $ pure (pure a)
  MaybeT mfa <*> (MaybeT ma) = MaybeT $ (<*>) <$> mfa <*> ma

-- break it down like this:
-- [] Maybe Identity is applicative
innerMost :: [Maybe (Identity (a -> b))]
          -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

-- [] Maybe is applicative
second' :: [Maybe (Identity a -> Identity b)]
        -> [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

-- [] is applicative
final' :: [Maybe (Identity a) -> Maybe (Identity b)]
       -> [Maybe (Identity a)] -> [Maybe (Identity b)]
final' = (<*>)

lmiApply :: [Maybe (Identity (a ->  b))]
         -> [Maybe (Identity a)]
         -> [Maybe (Identity b)]
lmiApply = final' . second' . innerMost

instance Monad m => Monad (MaybeT m) where
  return = pure
  -- ma :: m (Maybe a)
  -- f :: a -> MaybeT m b
  MaybeT ma >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just a -> runMaybeT $ f a

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
