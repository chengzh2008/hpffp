module MaybeMonad where

data Cow = Cow { name :: String
               , age :: Int
               , weight :: Int} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499 then Nothing else Just c

-- without a monad
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck $ Cow nammy agey weighty

-- with a monad
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  namey <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck $ Cow namey agey weighty

-- with a monad using >>=
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = do
  noEmpty name' >>=
    \ namey ->
      noNegative age' >>=
      \ agey ->
        noNegative weight' >>=
        \ weighty ->
          weightCheck $ Cow namey agey weighty
