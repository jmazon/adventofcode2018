import Data.List (transpose,elemIndex)
import Control.Arrow ((&&&))

power serial x y = (rackId * y + serial) * rackId `div` 100 `mod` 10 - 5
  where rackId = x + 10

main = interact $ show . (part1 &&& part2) . accums . read

accums serial = [ partials [ power serial x y | x <- [1..300] ] | y <- [1..300] ]

part1 accs = (x,y) where ((_,y),x) = bestSquarePos 3 accs
part2 accs = (x,y,s) where
  (((_,y),x),s) = maxAndPos $ map (\n -> bestSquarePos n accs) [0..300]

bestSquarePos n accs = maxAndPos $ map maxAndPos squaresN where
  rowsN = map (sumNWith n) accs
  squaresN = map (sumNWith n . partials) (transpose rowsN)

partials xs = scanl1 (+) (-3000:0:xs)
sumNWith n a = zipWith (-) (drop n a) a

-- suboptimal, but O(ok)
maxAndPos xs = (m,p) where m = maximum xs; Just p = elemIndex m xs
