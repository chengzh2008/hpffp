module NormalForm where

data Fiction = Fiction deriving (Eq, Show)

data Nonfiction = Nonfiction deriving (Eq, Show)

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving (Eq, Show)

type AuthorName = String

data Author = Author (AuthorName, BookType)

-- the above is not a normal form because it is not a sum of products.

data Author' = FictionAuthor' Fiction AuthorName
             | NonfictionAuthor' Nonfiction AuthorName
             deriving (Eq, Show)
-- the above is a normal form

-- so is this
data Expr = Number Int
          | Add Expr Expr
          | Minus Expr
          | Mult Expr Expr
          | Devide Expr Expr
