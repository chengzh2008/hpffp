-- fmap is <$> function application in a context

a = (+1) <$> read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi, ", "Hello"])

-- fmap to function is function composition fmap ~ .
c = (*2) . (\x -> x - 2)

-- another fmap to function  fmap ~ <$>
d = ((return '1' ++) . show) <$> (\x -> [x, 1..3])

-- transform the Integer inside the IO
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) $ ioi
    in (*3) <$> changed
