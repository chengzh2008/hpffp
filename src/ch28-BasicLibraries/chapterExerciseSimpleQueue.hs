data Queue a = Queue { enquene :: [a]
                     , dequeue :: [a]
                     } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a qa = Queue (a : enquene qa) (dequeue qa)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue en de) =
  case de of
    [] -> case en of
      [] -> Nothing
      _ -> let newDe = reverse en
           in Just (head newDe, Queue [] (tail newDe))
    x:xs -> Just (x, Queue en xs)
