newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s1 = State $ \s -> ((), s1)

exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

eval :: State s a -> s -> a
eval sa = fst . runState sa

modify :: (s -> s) -> State s ()
modify ss = State $ \s -> ((), ss s)
