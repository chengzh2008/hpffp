module HuttonRazor where

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = eval expr1 + eval expr2

instance Show Expr where
  show (Lit n) = show n
  show (Add expr1 expr2) = show expr1 ++ " + " ++ show expr2

printExpr :: Expr -> String
printExpr = show
