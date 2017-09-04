import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity

-- Problem 1
rDec :: Num a => Reader a a
rDec = ReaderT $ \a -> return a - 1

-- Problem 2  make it point-free
rDec' :: Num a => Reader a a
rDec' = ReaderT $ return . negate . (1-)

-- Problem 3
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> return $ show a

-- Problem 4 make it point-free
rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ return . show

-- Problem 5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT $ \a -> do
    putStrLn $ "Hi: " ++ show a
    return $ a + 1

-- Problem 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> do
  putStrLn $ "Hi: " ++ show a
  return (show a, a + 1)
