-- TODO: step from id and g! instead of u

import Data.Ord
import Data.Either
import Data.List
import Data.Array
import qualified Data.Set as S
import Control.Monad
import Control.Arrow

import Debug.Trace

type Pos = (Int,Int)
data Cell = Empty | Wall | Unit { uId :: Int, uType :: Bool, uHp :: Int } deriving (Show,Eq)
type Grid = Array Pos Cell

parse :: String -> Grid
parse i = listArray ((1,1),(h,w)) (snd $ mapAccumL readC 0 $ concat ls) where
  ls = lines i
  w = length (head ls)
  h = length ls
  readC i '.' = (i,Empty)
  readC i '#' = (i,Wall)
  readC i 'E' = (i+1,Unit i True 200)
  readC i 'G' = (i+1,Unit i False 200)

isFree = (== Empty)
isUnit (Unit _ _ _) = True
isUnit _ = False
enemies (Unit _ t1 _) (Unit _ t2 _) = t1 /= t2
neighbors (i,j) = [(i-1,j),(i,j-1),(i,j+1),(i+1,j)]
uIsElf u = uType u

turn :: Int -> Grid -> Either Int Grid
turn ep g0 = step g0 units where
  units = map (second uId) $ filter (isUnit . snd) (assocs g0)
  step g [] = Right g
  step g ((p,i):us) | not (isUnit u) || uId u /= i = step g us
                    | null targets = Left (sum [ uHp u | u <- elems g, isUnit u ]) -- no targets -> end of combat
                    | null reachable = step g us -- no reachable range -> end of unit turn
                    | [] <- closest = attack g p u us -- attack
                    | otherwise = attack (g // [(p,Empty),(p',u)]) p' u us -- move
    where
      u = g!p
      targets = filter (((&&) <$> isUnit <*> enemies u) . snd) (assocs g)
      tRange = filter ((||) <$> (== p) <*> isFree . (g!)) $ concatMap (neighbors . fst) targets
      reachable = bfs g tRange S.empty [[p]]
      closest = head $ sortBy (comparing length <> comparing head <> comparing last) reachable
      p' = last closest

  attack g p u us | null targets = step g us -- no targets, end of unit turn
                  -- | trace (show p ++ " attacks " ++ show target ++ " down to " ++ show newHp) False = undefined
                  | newHp <= 0 = if uIsElf (g!target) && ep > 3
                                 then Left (-1)
                                 else step (g // [(target,Empty)]) us
                  | otherwise = step (g // [(target,(g!target) { uHp = newHp })]) us
    where
      targets = filter (((&&) <$> isUnit <*> enemies u) . (g!)) (neighbors p)
      target = head (sortOn (uHp . (g!)) targets)
      newHp = uHp (g ! target) - if uIsElf u then ep else 3

  bfs g gl cl ts@ ~(t@(p:_):ts')
    | null gl || null ts = []
    | p `S.member` cl = bfs g gl cl ts'
    | p `elem` gl = init t : bfs g (delete p gl) cl' ts'
    | otherwise = bfs g gl cl' (ts' ++ ts'')
      where
        cl' = S.insert p cl
        ts'' = map (:t) $ filter (`S.notMember` cl) $ filter (isFree . (g!)) $ neighbors p

outcome g0 ep = (length pre - 1) * hp
  where gs = iterate (>>= turn ep) (pure g0)
        (pre,(Left hp:_)) = break isLeft gs


main = do
  g0 <- parse <$> getContents
  print $ outcome g0 3

  let isElfWin ep = hp > 0 where
        Left hp = foldM (\g _ -> turn ep g) g0 (repeat undefined)
      Just high = find isElfWin [6,12..]

  let best = bSearch (isElfWin . traceShowId) 3 high
  print best
  print $ outcome g0 best

bSearch f low high | high - low <= 1 = high
                   | f mid = bSearch f low mid
                   | otherwise = bSearch f mid high
  where mid = (low + high + 1) `div` 2

showT (Right g) = unlines [ [ toC (g!(i,j)) | j <- [jMin..jMax] ] | i <- [iMin..iMax] ]
  where ((iMin,jMin),(iMax,jMax)) = bounds g
        toC Empty = '.'
        toC Wall = '#'
        toC (Unit _ True _) = 'E'
        toC (Unit _ False _) = 'G'
