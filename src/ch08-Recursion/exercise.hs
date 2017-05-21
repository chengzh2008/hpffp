module ChapterExercise where

sumAll :: Integral a => a -> a
sumAll 0 = 0
sumAll n = (+) n $ sumAll $ n - 1

multi :: Integral a => a -> a -> a
multi m 0 = 0
multi m n
  | n > 0 = m + multi m (n - 1)
  | n < 0 = multi (-m) (-n)

--MaCarthy 91 funciton
mc :: Integral a => a -> a
mc n
  | n > 100 = n - 10
  | n <= 100 = mc $ mc $ (n + 11)
