import qualified Data.Map as M
import Data.List (unfoldr,insert,delete)
import Control.Arrow

main = interact $ show . (solve &&& solve' 5 60) . parse

parse = close . M.fromListWith (++) . map edge . lines where
  edge l = (b,[a]) where [_,[a],_,_,_,_,_,[b],_,_] = words l
  close = M.unionWith (++) =<< M.fromList . map (\c -> (c,[]))
                             . concat . M.elems

solve es0 = unfoldr f es0 where
  f es | M.null es = Nothing
       | otherwise = Just (m,es')
    where m = minimum $ M.keys $ M.filter null es
          es' = M.map (delete m) $ M.delete m es

solve' nW o es0 = go (0,nW,es0,[]) where
  taskTime c = fromEnum c - fromEnum 'A' + 1 + o
  go (t,w,es,tl) | w > 0,
                   ms@(_:_) <- M.keys $ M.filter null es,
                   m <- minimum ms =
                     go (t,w-1,M.delete m es,insert (t+taskTime m,m) tl)
                 | ((t',c):tl') <- tl = go (t',w+1,M.map (delete c) es,tl')
                 | M.null es = t
