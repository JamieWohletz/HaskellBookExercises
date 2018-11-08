module WriteTheCode where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

-- Write the code

-- 1
rDec :: Num a => Reader a a
rDec = reader $ \r -> r + 1

-- 2
rDec' :: Num a => Reader a a
rDec' = reader (+1)

-- 3
rShow :: Show a => Reader a String
rShow = reader show

-- 4
-- done

-- 5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> putStrLn ("Hi: " ++ show r) >> return (r + 1)

-- 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn $ "Hi: " ++ show s
  return (show s, s + 1)
