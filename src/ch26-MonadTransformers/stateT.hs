import Control.Monad.Trans
import Data.Tuple

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  -- f :: a -> b
  -- TODO: figure out simpler solution
  fmap f (StateT smas) =
    StateT $ \s -> let m = smas s   -- m (a, s)
                       u = fmap swap m -- m (s, a)
                       v = (fmap . fmap) f u -- m (s, b)
                   in fmap swap v -- m (b, s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  -- smfa :: s -> m ((a -> b), s)
  -- sma :: s -> m (a, s)
  -- final :: s -> m (b, s)
  -- here it will generate two state s' and s'', thus type constraint for m should be Monad, not applicative.
  -- TODO: figure out simpler solution
  StateT smfa <*> StateT sma =
    StateT $ \s -> do
      (f, s') <- smfa s -- m ((a -> b) , s)
      (a , s'') <- sma s' -- m (a, s)
      return (f a, s'') -- m (b, s)

instance Monad m => Monad (StateT s m) where
  return = pure
  -- sma :: s -> m (a, s)
  -- f :: a -> StateT  (s -> m (b, s))
  -- final :: StateT (s -> m (b, s))
  -- TODO: figure out simpler solution
  StateT sma >>= f = StateT $ do
    \s -> do
      (a, s') <- sma s
      (b, s'') <- runStateT (f a) s'
      return (b, s'')

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
