import Data.List
import Data.List.Split
import qualified Data.Sequence as Q
import System.Environment

data Circle = C { cMarbles :: Q.Seq Int, cPos :: Int }

circle0 = C (Q.singleton 0) 0

insertMarble (C ms p) m
  | m `mod` 23 == 0 = let (l',t) = Q.splitAt p'' ms
                          (s Q.:< r') = Q.viewl t
                          ms'' = l' <> r'
                          p'' = (p-7) `mod` length ms
                      in (C ms'' p'',m+s)
  | otherwise = let (l,r) = Q.splitAt p' ms
                    ms' = l <> (m Q.<| r)
                    p' = (p+1) `mod` length ms + 1
                in (C ms' p',0)

solve np nm = maximum $ map sum $ transpose $ chunksOf np $
              snd $ mapAccumL insertMarble circle0 [1..nm]

main = do
  [np,nm] <- map read <$> getArgs
  print $ solve np nm
  print $ solve np (nm*100)
  
