import Data.Array (accumArray,(!))
import Control.Applicative (ZipList(ZipList),getZipList,liftA2)
import Control.Monad (forM_)
import Text.Regex.Posix

type Vec = ZipList Int
data Point = P { pPos :: Vec, pVel :: Vec }

main = do
  ps <- map parse . lines <$> getContents

  let (minAreaTime,bs@((top,left),(bottom,right)),ps')
        = convex $ map analyze $ iterate step ps
      analyze ps = (s,bs,ps) where bs = extents ps; s = size bs
      a = accumArray (flip const) '.' bs
          [ ((y,x),'#') | p <- ps', let ZipList [x,y] = pPos p ]

  forM_ [top..bottom] $ \i -> putStrLn [ a!(i,j) | j <- [left..right] ]
  print minAreaTime

parse :: String -> Point
parse l = P (ZipList [x,y]) (ZipList [vx,vy]) where
  [_,x,y,vx,vy] = map read $ getAllTextSubmatches $
    l =~ "^position=< *(-?[0-9]*), *(-?[0-9]*)> \
          \velocity=< *(-?[0-9]*), *(-?[0-9]*)>$"

step :: [Point] -> [Point]
step ps = map advance ps where advance (P p v) = P (liftA2 (+) p v) v

size :: ((Int,Int),(Int,Int)) -> Int
size ((top,left),(bottom,right)) = (bottom - top + 1) * (right - left + 1)

extents :: [Point] -> ((Int,Int),(Int,Int))
extents ps = ((top,left),(bottom,right))
  where top    = minimum (map ((!!1) . getZipList . pPos) ps)
        bottom = maximum (map ((!!1) . getZipList . pPos) ps)
        left   = minimum (map ((!!0) . getZipList . pPos) ps)
        right  = maximum (map ((!!0) . getZipList . pPos) ps)

convex :: Ord i => [(i,a,b)] -> (Int,a,b)
convex = go . zip [0..] where
  go ((_,(a,_,_)) : xs@( (i,(b,x,y)) : (_,(c,_,_)) : _))
    | b < a && b < c = (i,x,y)
    | otherwise      = go xs
