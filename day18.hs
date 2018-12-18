import Data.List
import Data.Array
import qualified Data.Map as M
import Control.Arrow

import Debug.Trace

main = do
  ss <- iterate step . parse <$> getContents
  print $ uncurry (*) . count . elems $ ss !! 10
  print $ uncurry (*) . count . elems $ unloop ss 1000000000

parse input = listArray ((1,1),(h,w)) (concat ls) where
  ls = lines input
  h = length ls
  w = length (head ls)

step life = listArray (bounds life) $ map new (indices life) where
  new p = case s of '.' -> if ts >= 3 then '|' else '.'
                    '|' -> if ls >= 3 then '#' else '|'
                    '#' -> if ts*ls>0 then '#' else '.'
    where s = life!p
          (ts,ls) = count (map (life!) (neighbors p))
  neighbors (i0,j0) = [ (i,j) | i <- [i0-1..i0+1], j <- [j0-1..j0+1],
                        (i,j) /= (i0,j0), inRange (bounds life) (i,j) ]

showR m = unlines [ [ m!(i,j) | j <- [jMin..jMax] ] | i <- [iMin..iMax] ]
  where ((iMin,jMin),(iMax,jMax)) = bounds m

count = (length *** length) . partition (== '|') . filter (/= '.')

unloop xs0 n = go M.empty xs0 0 where
  go cl xs@(x:xs') i | i == n = trace "Direct" x
                     | Just j <- M.lookup x cl = traceShow (j,i) $ xs !! ((n-i) `mod` (i-j))
                     | otherwise = go (M.insert x i cl) xs' $! i+1
