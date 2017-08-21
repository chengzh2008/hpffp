class BiFunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- bifunction instance of the following datatype
data Deux a b = Deux a b

instance BiFunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a

instance BiFunctor Const where
  bimap f _ (Const a) = Const $ f a

data Drei a b c = Drei a b c

instance BiFunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance BiFunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance BiFunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance BiFunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b

instance BiFunctor Either' where
  bimap f _ (Left' a) = Left' $ f a
  bimap _ g (Right' b) = Right' $ g b
