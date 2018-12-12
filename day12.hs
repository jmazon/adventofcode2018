import Data.List (findIndices,stripPrefix)
import qualified Data.IntSet as S

readL l = ([a,b,c,d,e],r) where [a,b,c,d,e,_,_,_,_,r] = map (== '#') l

step lt s = S.fromList $ filter f [S.findMin s-2..S.findMax s+2]
  where f i = r where Just r = lookup (map (`S.member` s) [i-2..i+2]) lt

main = do
  Just s0 <- fmap (S.fromList . findIndices (== '#')) .
             stripPrefix "initial state: " <$> getLine
  getLine
  table <- map readL . lines <$> getContents
  let ss = iterate (step table) s0
  print $ sum $ S.elems (ss !! 20)
  let (s1,i,o,d) = stabilize (map normalize ss) 0
  print $ sum $ S.elems $ S.map ((+) (o + (5*10^10 - i) * d)) s1

stabilize ((x,u):ys@((y,v):_)) i
  | x == y = (x,i,u,v-u)
  | otherwise = stabilize ys $! i+1

normalize s = (S.map (subtract m) s,m) where m = S.findMin s
