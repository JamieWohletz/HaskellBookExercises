module GreetIfCool where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
  if cool
    then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhhhh."
  where cool =
          coolness == "downright frosty yo"
