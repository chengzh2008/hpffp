import Control.Monad.Trans

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m =>Applicative (EitherT e m) where
  pure a = EitherT $ pure (pure a)
  EitherT mefa <*> EitherT mea = EitherT $ (<*>) <$> mefa <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  -- mea :: m (Either e a)
  -- f :: a -> EitherT e m b
  EitherT mea >>= f = EitherT $ do
    v <- mea
    case v of
      Left e -> return (Left e)
      Right a -> runEitherT $ f a

instance MonadTrans (EitherT e) where
  lift m = EitherT $ fmap Right m

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT amc bmc (EitherT amb) = do
  v <- amb
  case v of
    Left a -> amc a
    Right b -> bmc b
