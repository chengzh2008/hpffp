module Mem where

import Data.Monoid

newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance (Eq s, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  Mem f `mappend` Mem g
    = Mem $ \s -> (fst (f s) <> fst (g s), if s == snd (g s) then snd (f s) else snd (g s))

f' = Mem $ \a -> ("hi" , a + 1)

main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
