 newtype Reader r a = Reader { runReader :: r -> a }

 instance Functor (Reader r) where
   fmap f (Reader ra) = Reader $ f . ra

 ask :: Reader a a
 ask = Reader id

 myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
 myLiftA2 fab fa fb = fab <$> fa <*> fb

 asks :: (r -> a) -> Reader r a
 asks f = Reader f

 instance Applicative (Reader r) where
   pure a = Reader (\_ -> a)
   Reader rab <*> Reader rb = Reader $ (\x -> (rab x) . rb $ x)

 instance Monad (Reader r) where
   return = pure
   ra >>= aRb = Reader $ (\r -> runReader (aRb $ runReader ra r) r)
