import Data.Ord
import Data.List
import Text.Regex.Posix
import Control.Monad
import Control.Arrow

main = interact $ show . (part1 &&& part2) . map parse . lines

type Pos = (Int,Int,Int)
data Nanobot = N { nPos :: Pos, nRad :: Int } deriving Show

parse :: String -> Nanobot
parse l = N (x,y,z) r where
  [x,y,z,r] = map read $ getAllTextMatches $ l =~ "-?[0-9]+"

part1 ns = length inRange where
  strongest = maximumBy (comparing nRad) ns
  inRange = filter (covers strongest) ns
  covers n1 n2 = dist (nPos n1) (nPos n2) <= nRad n1

dist (a,b,c) (d,e,f) = abs (d-a) + abs (e-b) + abs (f-c)

data Space = S { sFrame :: [(Int,Int)], sWeight :: !Int }

projs = [s0,s1,s2,s3]
s0 (x,y,z) =  x + y + z
s1 (x,y,z) = -x + y + z
s2 (x,y,z) =  x - y + z
s3 (x,y,z) =  x + y - z

part2 ns = minimum $ map (dist (0,0,0)) vertices where
  -- Evaluate problem granularity: extents and pitch
  rMin = minimum (map nRad ns)
  step = rMin `div` 2
  xl = foldl1' min (map ((\(x,_,_) -> x) . nPos) ns)
  yl = foldl1' min (map ((\(_,y,_) -> y) . nPos) ns)
  zl = foldl1' min (map ((\(_,_,z) -> z) . nPos) ns)
  xh = foldl1' max (map ((\(x,_,_) -> x) . nPos) ns)
  yh = foldl1' max (map ((\(_,y,_) -> y) . nPos) ns)
  zh = foldl1' max (map ((\(_,_,z) -> z) . nPos) ns)

  -- For each point in coarse range, count nanobots in scope
  range = [ (flux p,p) | x <- [xl,xl+step..xh]
                       , y <- [yl,yl+step..yh]
                       , z <- [zl,zl+step..zh]
                       , let p = (x,y,z) ]
  ss = map toSpace ns
  flux = length . flux'
  flux' p = filter (isIn p) ss

  -- Compute exact volume around point with best coverage and its
  -- vertices: one of them (or all) is closest to origin
  zone = head $ foldM1 inter $ flux' $ snd $ maximum $ range
  vertices = filter (`isIn` zone) $ points (sFrame zone)
  points [(a,b),(c,d),(e,f),(g,h)] =
    (noS <$> [c,d] <*> [e,f] <*> [g,h]) ++
    (noX <$> [a,b] <*> [e,f] <*> [g,h]) ++
    (noY <$> [a,b] <*> [c,d] <*> [g,h]) ++
    (noZ <$> [a,b] <*> [c,d] <*> [e,f])
  noS mx my mz = noNothing (mx+my+mz) mz my mz
  noX s my mz = noNothing s (s-my-mz) my mz
  noY s mx mz = noNothing s mx (s-mx-mz) mz
  noZ s mx my = noNothing s mx my (s-mx-my)
  noNothing s mx my mz = ((s-mx)`div`2,(s-my)`div`2,(s-mz)`div`2)

toSpace (N pos rad) = S (map (((subtract rad) &&& (+ rad)) . ($ pos)) projs) 1

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x:xs) = foldM f x xs

inter :: MonadPlus m => Space -> Space -> m Space
inter s1 s2 = mfilter (not . any (uncurry (>)) . sFrame) $ pure $
              S (zipWith mm (sFrame s1) (sFrame s2)) (sWeight s1 + sWeight s2)
  where mm (a,b) (c,d) = (max a c,min b d)

isIn :: Pos -> Space -> Bool
p `isIn` s = and $ zipWith f (map ($ p) projs) (sFrame s)
  where x `f` (a,b) = a <= x && x <= b
