module InsideOut where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- base monad here is structurally outmost, but lexically inner: IO
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap


embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
-- explicitly write down the type, which will help you figure out the solution
embedded' =  let a = (return . const (Right (Just (1 :: Int)))) -- () -> IO (Either String (Maybe Int))
                 b = ReaderT a -- ReaderT () IO (Either String (Maybe Int))
                 c = ExceptT b -- ExceptT String (ReaderT () IO) (Maybe Int)
             in MaybeT c
{-
-- in another way
embedded' = MaybeT $ ExceptT $ ReaderT $ (return . const (Right (Just 1)))

-}
