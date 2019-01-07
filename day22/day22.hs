import Data.Array
import qualified Data.Set as S
import Control.Exception
import Control.Monad

main = do
  depth <- read . last . words <$> getLine
  (xt,yt) <- read . (\x -> "(" ++ x ++ ")") . last . words <$> getLine
  print $ part1 depth xt yt
  print $ part2 depth xt yt

part1 :: Int -> Int -> Int -> Int
part1 depth x y = sum $ grid depth x y x y

type Pos = (Int,Int)
newtype Geo = Geo Int
newtype Ero = Ero Int

grid :: Int -> Int -> Int -> Int -> Int -> Array Pos Int
grid depth xt yt w h = fmap toType erosion
  where toType (Ero e) = e `mod` 3
        geoToErosion (Geo g) = Ero $ (g + depth) `mod` 20183

        erosion :: Array Pos Ero
        erosion = listArray ((0,0),(h,w))
                  [ geoToErosion (geo i j) | i <- [0..h], j <- [0..w] ]

        geo 0 0 = Geo 0
        geo i j | (i,j) == (yt,xt) = Geo 0
        geo 0 j = Geo (16807 * j)
        geo i 0 = Geo (48271 * i)
        geo i j = Geo (up * left `mod` 20183)
          where Ero up = erosion ! (i-1,j)
                Ero left = erosion ! (i,j-1)

data Region = Rocky | Wet | Narrow deriving (Eq,Enum)
data Equip = Torch | Climb | Neither deriving (Eq,Ord)

compatible Rocky  = (/= Neither)
compatible Wet    = (/= Torch)
compatible Narrow = (/= Climb)

type S  = (Pos,Equip)
type S' = (Int,S) -- S with time

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
  go cl q | S.member s cl = go cl q'
          | s == g        = t
          | otherwise     = go (S.insert s cl) q''
    where ((t,s),q') = S.deleteFindMin q
          ns = filter ((`S.notMember` cl) . snd) $
               map (\(c,s') -> (t+c,s')) $tr s
          q'' = S.union q' (S.fromList ns)
