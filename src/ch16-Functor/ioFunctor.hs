getInt :: IO Int
getInt = fmap read getLine


-- can use do syntax, using monad
myTooIsm :: IO String
myTooIsm = do
  i <- getLine
  return $ i ++ " and me too!"

-- using only functor of IO
myTooIsm' :: IO String
myTooIsm' = fmap (++" and me too!") getLine

-- using monad of IO
bumpIt :: IO Int
bumpIt = do
  int <- getInt
  return (int + 1)

-- using functor of IO
bumpIt' :: IO Int
bumpIt' = fmap (+1) getInt
