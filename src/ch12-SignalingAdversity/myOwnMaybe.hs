module MyOwnMaybe where

isJust :: Maybe a-> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing = not . isJust

maybbe :: b -> (a -> b) -> Maybe a -> b
maybbe acc f Nothing = acc
maybbe acc f (Just n) = f n

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just a1) = a1

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr go []
  where go :: Maybe a -> [a] -> [a]
        go Nothing xs = xs
        go (Just x) xs = x:xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where go :: Maybe a -> Maybe [a] -> Maybe [a]
        go Nothing _ = Nothing
        go _ Nothing = Nothing
        go (Just x) (Just xs) = Just (x:xs)
