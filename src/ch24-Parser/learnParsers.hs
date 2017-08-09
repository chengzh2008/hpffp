module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
-- TODO: let parse one fail if not eof
-- one'' = string "1" >> eof

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop
-- TODO: let parse one fail if not eof
-- oneTwo'' = string "12" >> eof

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "one'':"
  --testParse one''
  pNL "oneTwo"
  testParse oneTwo
  pNL "oneTwo'"
  testParse oneTwo'
  pNL "oneTwo''"
  -- testParse oneTwo''
