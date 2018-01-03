mult1 = x * 3 + y 
  where
    x = 3
    y = 1000

mult2 = x * 5
  where 
    y = 10
    x = 10 * 5 + y

mult3 = z / x + y 
  where 
    x = 7
    y = negate x
    z = y * 10

waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

triple x = x * 3

waxOff x = 10 - triple x * 3 * 3

excite :: String -> String
excite str = str ++ "!"

fifth :: String -> Char
fifth str = str !! 4

dropNine :: String -> String
dropNine = drop 9

third :: String -> Char
third str = str !! 2

atIndex :: Int -> Char
atIndex x = "Curry is awesome!" !! (x-1)

rvrs :: String -> String
rvrs s = awesome ++ is ++ curry 
  where 
    awesome = drop 9 s 
    is = take 4 $ drop 5 s
    curry = take 5 s