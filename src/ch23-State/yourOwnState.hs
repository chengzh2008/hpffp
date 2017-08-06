newtype Moi s a = Moi { runMoi :: s ->(a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $
    \s -> let (a, s1) = g s in (f a, s1)


instance Applicative (Moi s) where
  pure a = Moi (\s -> (a, s))
  (Moi f) <*> Moi g = Moi $
    \s -> let (a, s1) = g s
              (f1, s2) = f s1
          in (f1 a, s2)

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi $
    \s -> let (a, s1) = f s
          in runMoi (g a) $ s1
