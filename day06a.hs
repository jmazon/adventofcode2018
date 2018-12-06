import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Debug.Trace

main = interact $ show . solve . parse

radiate o = map (c o) [0..] where
  c   p 0 = [o]
  c (x,y) i = concat [ [ (x+i-j,y+j),(x-j,y+i-j),(x-i+j,y-j),(x+j,y-i+j) ]
                       | j <- [0..i-1] ]

parse = pairs . map read . words . filter (/= ',')

pairs (a:b:cs) = (a,b) : pairs cs
pairs [] = []


solve :: [(Int,Int)] -> Int
solve ss = maximum $ map S.size $ M.elems zs' where
  zs' = M.difference zs rs
  (_,zs,rs) = head $ dropWhile ((> 21) . traceShowId . M.size . thd) $
              iterate f (S.empty,M.empty,M.fromSet radiate (S.fromList ss)) 
  f (cl,zs,rs) = (cl'',zs',rs'') where
    collide = M.keysSet $ M.filter (> 1) $ M.fromListWith (+) $ map (flip (,) 1) $ concat $ M.map head rs
    cl' = S.union cl collide
    rs' = M.map (onHead (filter (`S.notMember` cl'))) rs
    cl'' = S.union cl' (S.fromList $ concat $ M.map head rs')
    rs'' = M.map tail $ M.filter (not . null . head) rs'
    fringe = M.map (S.fromList . head) rs' :: M.Map (Int,Int) (S.Set (Int,Int))
    zs' = M.unionWith S.union (zs :: M.Map (Int,Int) (S.Set (Int,Int))) fringe
    
onHead f (x:xs) = (f x : xs)
thd (_,_,c) = c
