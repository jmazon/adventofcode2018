import Debug.Trace
import Data.Ord
import Data.Maybe
import Data.Bits
import Data.List
import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Regex.Posix
import Control.Monad
import Control.Arrow

data Nanobot = N { nPos :: Pos, nRad :: Int } deriving Show
type Pos = (Int,Int,Int)

data Space = S { sFrame :: [(Int,Int)], sWeight :: !Int } deriving (Show,Eq,Ord)

projs = [s0,s1,s2,s3]
s0 (x,y,z) =  x + y + z
s1 (x,y,z) = -x + y + z
s2 (x,y,z) =  x - y + z
s3 (x,y,z) =  x + y - z

toSpace (N pos rad) = S (map (((subtract rad) &&& (+ rad)) . ($ pos)) projs) 1

split s1 s2 = inter s1 s2 ++ diff s1 s2 ++ diff s2 s1

inter :: MonadPlus m => Space -> Space -> m Space
inter s1 s2 = mfilter (not . any (uncurry (>)) . sFrame) $ pure $
              S (zipWith mm (sFrame s1) (sFrame s2)) (sWeight s1 + sWeight s2)
  where mm (a,b) (c,d) = (max a c,min b d)

diff :: Space -> Space -> [Space]
diff s1 s2 = map (flip S (sWeight s1)) $ filter (or . zipWith out (sFrame s2)) $ sequence $ zipWith expand (sFrame s1) (sFrame s2)
  where
    expand (a,b) (c,d) | both = [(a,c),(c,d),(d,b)]
                       | justC = [(a,c),(c,b)]
                       | justD = [(a,d),(d,b)]
                       | otherwise = [(a,b)]
      where both = justC && justD
            justC = a < c && c < b
            justD = a < d && d < b
    out (l,h) (a,b) = a < l || a >= h || b <= l || b > h
    
inside :: Space -> Space -> Bool
x `inside` y = and $ zipWith f (sFrame x) (sFrame y)
  where (a,b) `f` (c,d) = a >= c && b <= d

isIn :: Pos -> Space -> Bool
p `isIn` s = and $ zipWith f (map ($ p) projs) (sFrame s)
  where x `f` (a,b) = a <= x && x <= b

parse :: String -> Nanobot
parse l = N (x,y,z) r where
  [x,y,z,r] = map read $ getAllTextMatches $ l =~ "-?[0-9]+"

main = interact $ show . (part1 &&& part2'') . map parse . lines

part1 ns = length inRange where
  strongest = maximumBy (comparing nRad) ns
  inRange = filter (covers strongest) ns
  covers n1 n2 = dist (nPos n1) (nPos n2) <= nRad n1

dist (a,b,c) (d,e,f) = abs (d-a) + abs (e-b) + abs (f-c)

part2 = head . groupBy ((==) `on` sWeight) . sortOn (Down . sWeight) . foldl' ins [] . map toSpace
  where
    ins ss s = traceShow (length ss) $
               s : concatMap (inter s) ss ++ filter (not . (`inside` s)) ss
    
foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x:xs) = foldM f x xs

part2' ns = head . groupBy ((==) `on` S.size) . sortOn (Down . S.size) . bronKerbosch $ adj where
  ss = map toSpace ns
  adj :: M.Map Space (S.Set Space)
  adj = M.fromListWith S.union $ concat
        [ [(x,S.singleton y),(y,S.singleton x)] | (x:xs) <- tails ss, y <- xs, _ <- inter x y ]

--bronKerbosch :: Ord a => M.Map a (S.Set a) -> [S.Set a]
bronKerbosch vs = go S.empty (M.keysSet vs) S.empty where
  go r p x | S.null p = if S.null x then traceShow (S.size r) $ traceShow (fromJust $ foldM1 inter (S.elems r)) [r] else []
           | (v,p') <- S.deleteFindMin p =
             let n = vs M.! v
             in go (S.insert v r) (S.intersection p n) (S.intersection x n) ++
                go r p' (S.insert v x)

-- 93859494 wrong but too high, right would be 93859495
distToZero (S s _) = traceShow (x,y,z) (abs x + abs y + abs z) where
  [xyz,mx,my,mz] = map min' s
  x = (xyz - mx) `div` 2
  y = (xyz - my) `div` 2
  z = (xyz - mz) `div` 2
  min' (a,b) | abs a < abs b = a
             | otherwise = b

part2'' ns = map (dist (0,0,0)) $ filter (`isIn` zone) $ points (sFrame zone)
    where
  rMin = minimum (map nRad ns)
  step = rMin `div` 2
  xl = foldl1' min (map ((\(x,_,_) -> x) . nPos) ns)
  yl = foldl1' min (map ((\(_,y,_) -> y) . nPos) ns)
  zl = foldl1' min (map ((\(_,_,z) -> z) . nPos) ns)
  xh = foldl1' max (map ((\(x,_,_) -> x) . nPos) ns)
  yh = foldl1' max (map ((\(_,y,_) -> y) . nPos) ns)
  zh = foldl1' max (map ((\(_,_,z) -> z) . nPos) ns)
  range = [ (flux p,p) | x <- [xl,xl+step..xh]
                       , y <- [yl,yl+step..yh]
                       , z <- [zl,zl+step..zh]
                       , let p = (x,y,z) ]
  ss = map toSpace ns
  flux = length . flux'
  flux' p = filter (isIn p) ss
  zone = head $ foldM1 inter $ flux' $ snd $ maximum $ range
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

-- 1326117 too low
-- 48202279 correct
