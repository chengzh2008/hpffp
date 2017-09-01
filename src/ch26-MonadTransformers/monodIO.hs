import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

-- liftIO :: IO a -> ExceptT e IO a
-- liftIO :: IO a -> ReaderT r IO a
-- liftIO :: IO a -> StateT s IO a
-- liftIO :: IO a -> StateT s (ReaderT r IO) a
-- liftIO :: IO a -> ExceptT e (StateT s (ReaderT r IO)) a

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a
