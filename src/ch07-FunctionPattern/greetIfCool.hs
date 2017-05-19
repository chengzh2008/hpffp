module GreetIfCool where

greetIfcool :: String -> IO ()
greetIfcool coolness =
  case cool of
    True -> putStrLn "Cool..."
    False -> putStrLn "Meh..."
  where cool = coolness == "very cool"
