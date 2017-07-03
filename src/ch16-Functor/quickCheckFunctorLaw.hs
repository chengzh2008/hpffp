import Test.QuickCheck
import Test.QuickCheck.Function
-- Functor law
-- fmap id = id
-- fmap (f . g) = (fmap f) . (fmap g)

-- quickcheck helper function
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == id f

functorCompose :: (Functor f, Eq (f c)) =>
                     (a -> b) ->
                     (b -> c) ->
                     f a ->
                     Bool
functorCompose f g x = (fmap (g . f) x) == (fmap g $ fmap f x)


-- using QuickCheck generating functions
functorCompose' :: (Functor f, Eq (f c)) =>
                      f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
functorCompose' xs (Fun _ f) (Fun _ g) =
  (fmap (g . f) xs) == (fmap g . fmap f $ xs)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  quickCheck $ \xs -> functorIdentity (xs :: [Int])
  quickCheck $ \xs -> functorCompose (+1) (*2) (xs :: [Int])
  -- arbitrary function generated from quickcheck for testing function law
  quickCheck (functorCompose' :: IntFC)
  quickCheck $ ((\xs -> functorCompose' xs) :: IntFC)
