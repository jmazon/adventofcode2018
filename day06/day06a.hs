{-# LANGUAGE TupleSections #-}
import qualified Data.Set as S
import qualified Data.Map as M

main = interact $ show . solve . parse

parse = pairs . map read . words . filter (/= ',')
  where pairs (a:b:cs) = (a,b) : pairs cs
        pairs [] = []

radiate :: (Int,Int) -> [[(Int,Int)]]
radiate o = map (c o) [0..] where
  c   p 0 = [o]
  c (x,y) i = concat [ [ (x+i-j,y+j),(x-j,y+i-j),(x-i+j,y-j),(x+j,y-i+j) ]
                       | j <- [0..i-1] ]

solve :: [(Int,Int)] -> Int
solve ss = maximum (fmap S.size finiteZones) where
  finiteZones = M.difference zones radiants
  (_,zones,radiants) = head $ dropWhile ((> 21) . M.size . thd) $
                       iterate f  ( S.empty, M.empty
                                  , M.fromSet radiate (S.fromList ss) )
  f (cl,zs,rs) = (cl'',zs',rs'') where
    -- drop equidistant radiation
    collisions = M.keysSet $ M.filter (> 1) $ M.fromListWith (+) $
                 map (, 1) $ concat $ fmap head rs
    cl' = S.union cl collisions
    -- also drop late-to-the-party radiation
    rs' = fmap (onHead (filter (`S.notMember` cl'))) rs
    cl'' = S.union cl' (S.fromList $ concat $ fmap head rs')
    -- don't keep dead radiants
    rs'' = fmap tail $ M.filter (not . null . head) rs'
    -- update radiant's proper zones
    fringe = fmap (S.fromList . head) rs'
    zs' = M.unionWith S.union zs fringe
    
onHead f (x:xs) = (f x : xs)
thd (_,_,c) = c
