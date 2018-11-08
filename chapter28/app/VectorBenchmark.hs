module VectorBenchmark where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

boxed :: Int -> V.Vector Int
boxed len = V.generate len id

unboxed :: Int -> UV.Vector Int
unboxed len = UV.generate len id

main :: IO ()
main = defaultMain
  [ bench "generate boxed vector" $
    nf boxed 10000
  , bench "generate unboxed vector" $
    nf unboxed 10000
  ]
