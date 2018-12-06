import qualified Data.Set as S
import qualified Data.Map.Strict as M

main = interact $ show . solve . parse

parse = pairs . map read . words . filter (/= ',')

pairs (a:b:cs) = (a,b) : pairs cs
pairs [] = []

solve ss = length $ filter (< 10000) $ map (\p -> sum (map (dist p) ss)) $
           (,) <$> [0..400] <*> [0..400]

dist (a,b) (c,d) = abs (c-a) + abs (d-b)
                                    
