import Nature

-- 1

get :: Nature s s
get = nature $ (\s -> (s, s))

-- 2

put :: s -> Nature s ()
put s = nature $ (\_ -> ((), s))

-- 3

exec :: Nature s a -> s -> s
exec (Nature sa) s = snd $ sa s

-- 4

eval :: Nature s a -> s -> a
eval (Nature sa) = fst . sa

-- 5

modify :: (s -> s) -> Nature s ()
modify ss = nature $ \s -> ((), ss s)