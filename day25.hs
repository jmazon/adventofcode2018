import Data.List
import qualified Data.Map as M
import Text.Regex.Posix
import Control.Monad

import Debug.Trace

type DS = (M.Map Vec Vec,M.Map Vec Int)
data Vec = V !Int !Int !Int !Int deriving (Show,Eq,Ord)

main :: IO ()
main = interact $ show . part1 . map parse . lines

parse :: String -> Vec
parse l = V x y z w where
  [x,y,z,w] = map read $ getAllTextMatches $ l =~ "-?[0-9]+"

part1 :: [Vec] -> Int
part1 vs = length $ nub $ map (fst . rep ds') vs
  where
    ds0 = (M.fromList (map (join (,)) vs),M.fromList (map (flip (,) 1) vs))
    ds' = foldl' connect ds0 $ filter (uncurry connected) $ pairs vs :: DS
    pairs ss = [ (s1,s2) | (s1:ss') <- tails ss, s2 <- ss' ]
    connected = ((<= 3) .) . dist

dist :: Vec -> Vec -> Int
dist (V a b c d) (V e f g h) = abs (e-a) + abs (f-b) + abs (g-c) + abs (h-d)

connect :: DS -> (Vec,Vec) -> DS
connect ds@(rp,hg) (e1,e2) -- | traceShow (e1,e2) False = undefined
                           | r1 == r2 = ds
                           | otherwise = (M.insert rb ra rp,
                                          if h1 == h2
                                          then M.adjust succ ra hg
                                          else hg)
  where
    (r1,h1) = rep ds e1
    (r2,h2) = rep ds e2
    (ra,rb) | h1 < h2 = (r2,r1)
            | otherwise = (r1,r2)
    
rep :: DS -> Vec -> (Vec,Int)
rep ds@(rp,hg) e -- | traceShow e False = undefined
                 | r == e = (r,h)
                 | otherwise = rep ds r
  where Just r = M.lookup e rp
        Just h = M.lookup e hg
