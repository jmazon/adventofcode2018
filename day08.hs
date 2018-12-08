import Data.Maybe
import Data.Monoid
import Data.Tree
import Control.Arrow
import Control.Monad.State
main = interact $ show . (part1 &&& part2) . parse . map read . words
parse ns = root where
  (root,[]) = parseNode ns
  parseNode (nChildren:nMeta:ns) = (Node metadata children,ns'') where
    (children,ns') = runState (replicateM nChildren (state parseNode)) ns
    (metadata,ns'') = splitAt nMeta ns'
part1 = foldMap (Sum . sum)
part2 = foldTree f where f metadata [] = sum metadata
                         f metadata cs = sum (mapMaybe (safeIndex cs) metadata)
safeIndex xs i = case splitAt (i-1) xs of (_,(x:_)) -> Just x
                                          _ -> Nothing
