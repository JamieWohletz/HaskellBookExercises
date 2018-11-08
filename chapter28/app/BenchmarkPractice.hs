module BenchmarkPractice where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

insertMap :: Int -> Int -> M.Map Int Int
insertMap k v = M.insert k v m

-- um... set insertion is much slower than map insertion, 
-- at least on my computer.
insertSet :: Int -> S.Set Int
insertSet v = S.insert v s

removeMap :: Int -> M.Map Int Int
removeMap k = M.delete k m

-- removal is way slower too... what's going on here!
removeSet :: Int -> S.Set Int
removeSet v = S.delete v s

main :: IO ()
main = defaultMain
  [ bench "member check map" $
    whnf membersMap 9999
  , bench "member check set" $
    whnf membersSet 9999
  , bench "insert into map" $
    whnf insertMap 12345
  , bench "insert into set" $
    whnf insertSet 12345
  , bench "remove from map" $
    whnf insertMap 3333
  , bench "remove from set" $
    whnf insertSet 3333
  ]