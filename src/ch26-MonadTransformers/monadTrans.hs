-- t is the monad transformer
-- m is the structually outmost
class MonadTrans t where
  lift :: Monad m => ma -> t m a

newtype ScottyT e m a =
  ScottyT { runS :: State (ScottyState e m) a }
  deriving (Functor, Applicative, Monad)

newtype ActionT e m a =
  ActionT { runAM :: ExceptT (ActionError e)
                             (ReaderT ActionEnv
                             (StateT ScottyResponse m)) a }
  deriving (Functor, Applicative)

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO
