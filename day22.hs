
-- depth: 10689
-- target: 11,722

import Data.Array
import Data.Function
import qualified Data.Set as S
import Control.Exception
import Control.Monad

import Debug.Trace

newtype Geo = Geo Int
newtype Ero = Ero Int

grid :: Int -> Int -> Int -> Int -> Int -> Array (Int,Int) Int
grid depth xt yt w h = fmap toType erosion where
  toType (Ero e) = e `mod` 3

  geoToErosion :: Geo -> Ero
  geoToErosion (Geo g) = Ero $ (g + depth) `mod` 20183

  erosion :: Array (Int,Int) Ero
  erosion = listArray ((0,0),(h,w)) [ geoToErosion (geo i j) | i <- [0..h], j <- [0..w] ]

  geo 0 0 = Geo 0
  geo i j | (i,j) == (yt,xt) = Geo 0
  geo 0 j = Geo (16807 * j)
  geo i 0 = Geo (48271 * i)
  geo i j = Geo (up * left `mod` 20183) where
    Ero up = erosion ! (i-1,j)
    Ero left = erosion ! (i,j-1)

patch :: Int -> Int -> a -> [[a]] -> [[a]]
patch x y v xss = up ++ [left ++ [v] ++ drop 1 right] ++ drop 1 bottom where
  ~(~up,~bottom) = splitAt y xss
  ~(~left,~right) = splitAt x (head bottom)

-- subGrid :: Int -> Int -> Int -> Int -> Int -> [[Int]]
-- subGrid depth xt yt w h = take (h+1) $ map (take (w+1)) $ grid depth xt yt

part1 :: Int -> Int -> Int -> Int
part1 depth x y = sum $ grid depth x y x y

data Region = Rocky | Wet | Narrow deriving (Eq,Enum)
data Equip = Torch | Climb | Neither deriving (Eq,Ord)

compatible Rocky = (/= Neither)
compatible Wet = (/= Torch)
compatible Narrow = (/= Climb)

type S = ((Int,Int),Equip)
type S' = (Int,S)

part2 depth xt yt = dijkstra trans ((0,0),Torch) ((xt,yt),Torch) where
  (w,h) = (xt+yt+xt+yt,xt+yt+xt+yt)
  g = fmap toEnum $ grid depth xt yt w h
  trans :: S -> [S']
  trans ((x,y),e) = [ (7,((x,y),e')) | e' <- [Torch,Climb,Neither],
                                       e' /= e, compatible (g!(y,x)) e' ] ++
    [ (1,((x',y'),e)) | (dx,dy) <- [(-1,0),(1,0),(0,-1),(0,1)],
                        let (x',y') = (x+dx,y+dy),
                        assert (x' < w) (x' >= 0),
                        assert (y' < h) (y' >= 0),
                        compatible (g!(y',x')) e ]
  
dijkstra :: (S -> [S']) -> S -> S -> Int
dijkstra tr s g = go S.empty (S.singleton (0,s)) where
  go :: S.Set S -> S.Set S' -> Int
  go cl q | S.member s cl = go cl q'
          | s == g = t
          | otherwise = go (S.insert s cl) q''
    where ((t,s),q') = S.deleteFindMin q
          ns :: [S']
          ns = filter ((`S.notMember` cl) . snd) $ map (\(c,s') -> (t+c,s')) $ tr s
          q'' = S.union q' (S.fromList ns)

main = do
  forM_ [0..15] $ \i -> do
    forM_ [0..15] $ \j ->
      putChar (case grid 510 10 10 15 15 ! (i,j) of
                 0 -> '.'
                 1 -> '='
                 2 -> '|')
    putChar '\n'
  print $ part1 510 10 10    -- 114
  print $ part2 510 10 10    -- 45
-- main' = do
  print $ part1 10689 11 722 -- 8575
  print $ part2 10689 11 722 -- 981 too low

-- 981 too low
-- 988 too low
-- 999 correct. WHY?!?!?!
-- ok, simple x <-> y. I hate computers.
