module QueueBenchmark where

import Queue
import Data.Maybe (fromJust)
import Criterion.Main
import qualified Data.Sequence as S

newtype BadQueue a = BQ [a] deriving (Eq, Show)

mkBadQueue = BQ []

badPush :: a -> BadQueue a -> BadQueue a
badPush a (BQ as) = BQ $ a:as

badPop :: BadQueue a -> Maybe (a, BadQueue a)
badPop (BQ as) =
  case as of
    [] -> Nothing
    xs -> Just (last xs, BQ $ init xs)

-- WARNING! unsafe!
badPopNTimes :: Int -> BadQueue a -> BadQueue a
badPopNTimes 0 bq = bq
badPopNTimes n bq = badPopNTimes (n - 1) (snd . fromJust $ badPop bq)

popNTimes :: Int -> Queue a -> Queue a
popNTimes 0 q = q
popNTimes n q = popNTimes (n - 1) (snd . fromJust $ pop q)

seqPopNTimes :: Int -> S.Seq a -> S.Seq a
seqPopNTimes 0 seq = seq
seqPopNTimes n seq = seqPopNTimes (n - 1) (snd . fromJust $ seqPop seq)

alternateBad :: Int -> BadQueue Int
alternateBad x = go x mkBadQueue
  where
    go :: Int -> BadQueue Int -> BadQueue Int
    go 0 bq = bq
    go n bq = go (n - 1) (badPopNTimes 2 $ badPush 6 (badPush 5 bq))

-- a real gangster would've made an IsQueue typeclass
alternateGood :: Int -> Queue Int
alternateGood x = go x mkQueue
  where
    go :: Int -> Queue Int -> Queue Int
    go 0 q = q
    go n q = go (n - 1) (popNTimes 2 $ push 6 (push 5 q))

alternateSeq :: Int -> S.Seq Int
alternateSeq x = go x S.empty
  where
    go :: Int -> S.Seq Int -> S.Seq Int
    go 0 seq = seq
    go n seq = go (n - 1) (seqPopNTimes 2 $ seqPush 6 (seqPush 5 seq))

seqPush :: a -> S.Seq a -> S.Seq a
seqPush a seq = a S.<| seq

seqPop :: S.Seq a -> Maybe (a, S.Seq a)
seqPop seq =
  case length seq of
    0 -> Nothing
    n ->
      let el     = S.index seq (n - 1)
          newSeq = S.deleteAt (n - 1) seq
      in  Just (el, newSeq)

main :: IO ()
main = defaultMain
  [ bench "list queue" $
    whnf alternateBad 100000
  , bench "two-stack queue" $
    whnf alternateGood 100000
  , bench "sequence queue" $
    whnf alternateSeq 100000
  ]

-- Results:
-- benchmarking list queue
-- time                 28.85 ms   (28.53 ms .. 29.31 ms)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 29.10 ms   (28.90 ms .. 29.46 ms)
-- std dev              541.9 μs   (319.2 μs .. 878.0 μs)

-- benchmarking two-stack queue
-- time                 3.958 ms   (3.779 ms .. 4.129 ms)
--                      0.990 R²   (0.986 R² .. 0.996 R²)
-- mean                 3.830 ms   (3.775 ms .. 3.908 ms)
-- std dev              197.1 μs   (126.5 μs .. 251.4 μs)
-- variance introduced by outliers: 32% (moderately inflated)

-- benchmarking sequence queue
-- time                 4.158 ms   (4.023 ms .. 4.261 ms)
--                      0.983 R²   (0.954 R² .. 0.998 R²)
-- mean                 4.366 ms   (4.244 ms .. 4.774 ms)
-- std dev              644.0 μs   (292.5 μs .. 1.142 ms)
-- variance introduced by outliers: 79% (severely inflated)