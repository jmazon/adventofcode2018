import Data.Char
import Data.List
import qualified Data.Sequence as Q
import System.Environment

s0 = ((0,1),Q.fromList [3,7])

step ((p1,p2),s) = ((p1',p2'),s') where
  v1 = Q.index s p1
  v2 = Q.index s p2
  q = Q.fromList (map digitToInt (show (v1+v2)))
  s' = s <> q
  p1' = (p1 + 1 + v1) `mod` Q.length s'
  p2' = (p2 + 1 + v2) `mod` Q.length s'

part1 n = take 10 $ drop n full

full = go (map snd $ iterate step s0) 0 where
  go qs@(q:qs') i | i < Q.length q = Q.index q i : go qs (i+1)
                  | otherwise = go qs' i

part2 ns = go full 0 where
  go xs i | ns `isPrefixOf` xs = i
          | otherwise = go (tail xs) $! i+1

main = do
  [n] <- getArgs
  putStrLn $ map intToDigit $ part1 (read n)
  print $ part2 (map digitToInt n)
