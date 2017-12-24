fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 $ fibs

fibsUnder100 = takeWhile (<100) fibs

fibsN x = fibs !! x

factorial n = (scanl (*) 1 [1..]) !! n
