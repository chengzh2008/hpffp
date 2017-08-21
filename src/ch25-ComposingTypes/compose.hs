module ComposeType where

newtype Compose f g a =
  Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure (pure a)
  -- TODO: need to figure out why
  Compose fgfa <*> Compose fga = Compose $ pure (<*>) <*> fgfa <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse afb (Compose fga) = Compose <$> (traverse . traverse) afb fga
