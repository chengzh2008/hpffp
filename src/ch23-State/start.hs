newtype State s a = State { runState :: s -> (a, s) }

newtype Reader r a = Reader { runReader :: r -> a }

-- isomorphism
type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)
