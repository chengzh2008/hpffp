-- Are thesetypes equivalent
-- ReaderT r Maybe   vs. MaybeT (Reader r)

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

type AInt = ReaderT () Maybe Int -- String -> Maybe Int
type BInt = MaybeT (Reader ()) Int -- String -> Maybe Int
