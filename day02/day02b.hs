import Data.List
main = interact $ unlines . pairs . lines
pairs xs = [ map fst eq | (x:ys) <- tails xs, y <- ys
                        , let (eq,ne) = partition (uncurry (==)) (zip x y)
                        , null (tail ne) ]
