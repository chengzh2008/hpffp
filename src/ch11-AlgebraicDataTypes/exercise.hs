module Exercise where

-- As-patterns

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t
