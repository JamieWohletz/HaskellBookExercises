import System.Random
import RandomExample

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
              (count + 1) nextGen

-- 1
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= limit = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
              (count + 1) nextGen

-- 2
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 0 g []
  where
    go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
    go sum count gen rolls
      | sum >= limit = (count, rolls)
      | otherwise =
        let (rolled, nextGen) =
              randomR (1, 6) gen
        in go (sum + rolled)
              (count + 1)
              nextGen
              (rolls ++ [intToDie rolled])
