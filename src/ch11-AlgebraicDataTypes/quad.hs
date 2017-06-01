module Quad where

data Quad = One | Two | Three | Four deriving (Eq, Show)

constructors = [Left, Right]
values = [One, Two, Three, Four]

eQuad :: [Either Quad Quad]
eQuad = [ f a | f <- constructors, a <- values]

prodQuad :: [(Quad, Quad)]
prodQuad = [(a, b) | a <- values, b <- values]

-- not quite sure this
funcQuad :: [(\x -> y)]
funcQuad = [(\x -> y) | x <-values, y <- values]
