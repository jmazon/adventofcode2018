import qualified Data.Map as M
import Control.Arrow
import Control.Monad

sample1 = "|\n\
          \v\n\
          \|\n\
          \|\n\
          \|\n\
          \^\n\
          \|\n"

sample2 = "/->-\\        \n\
          \|   |  /----\\\n\
          \| /-+--+-\\  |\n\
          \| | |  | v  |\n\
          \\\-+-/  \\-+--/\n\
          \  \\------/   \n"

sample3 = "/>-<\\  \n\
          \|   |  \n\
          \| /<+-\\\n\
          \| | | v\n\
          \\\>+</ |\n\
          \  |   ^\n\
          \  \\<->/"


parse :: String -> (Tracks,Carts)
parse input = (M.fromList *** M.fromList) $
              foldr readC ([],[]) $ concat $
              zipWith (\i -> map (\(j,c) -> ((i,j),c))) [0..] $
              map (zip [0..]) $ lines input where
  readC (p,c) (as,cs) = case c of '|' -> ((p,Straight):as,cs)
                                  '-' -> ((p,Straight):as,cs)
                                  '/' -> ((p,Curve1):as,cs)
                                  '\\' -> ((p,Curve2):as,cs)
                                  '+' -> ((p,Cross):as,cs)
                                  '^' -> readC (p,'|') (as,(p,((-1,0),0)):cs)
                                  'v' -> readC (p,'|') (as,(p,((1,0),0)):cs)
                                  '<' -> readC (p,'-') (as,(p,((0,-1),0)):cs)
                                  '>' -> readC (p,'-') (as,(p,((0,1),0)):cs)
                                  ' ' -> (as,cs)

data Track = Straight | Curve1 | Curve2 | Cross deriving Show

type Vec = (Int,Int)
type Tracks = M.Map Vec Track
type CartState = (Vec,Int)
type Cart = (Vec,CartState)
type Carts = M.Map Vec CartState

main = do
  (track,cs) <- parse <$> getContents
  print $ sequence $ iterate (>>= step False track) (pure cs)
  print $ sequence $ iterate (>>= step True track) (pure cs)
  
{-
step :: Bool -> Tracks -> Carts -> Either Vec Carts
step remove track cs | remove && M.size cs == 1 = Left $ head $ M.keys cs
                     | otherwise = cs''' where
  cs' = map advance $ M.assocs cs
  advance ((i,j),v@((di,dj),_)) = ((i+di,j+dj),v)
  cs'' = map turn cs'
  turn (p,(d@(i,j),t)) = case track M.! p of
    Straight -> (p,(d,t))
    Curve1 -> (p,((-j,-i),t))
    Curve2 -> (p,((j,i),t))
    Cross -> (p,(d',(t+1) `mod` 3)) where
      d' = case t of 0 -> (-j,i)
                     1 -> d
                     2 -> (j,-i)
  cs''' :: Either Vec Carts
  cs''' = foldM check cs (zip (M.keys cs) cs'')
  check m (p0,(p1,v)) | M.member p1 m = if remove
                                        then pure (M.delete p1 $ M.delete p0 m)
                                        else Left p1
                      | otherwise = pure $ M.insert p1 v $ M.delete p0 m
-}

step :: Bool -> Tracks -> Carts -> Either Vec Carts
step remove track cs | remove && M.size cs == 1 = Left $ head $ M.keys cs
                     | otherwise = advanceAll cs where
  advanceAll :: Carts -> Either Vec Carts
  advanceAll cs = go cs (M.assocs cs) where
    go :: Carts -> [Cart] -> Either Vec Carts
    go f (c@(p,v):cs)
      | p' `M.member` f = if remove
                          then go (M.delete p' $ M.delete p f) (filter ((/= p') . fst) cs)
                          else Left p'
      | otherwise = go (M.insert p' v' $ M.delete p f) cs
      where
        (p',v') = turn (advance c)
    go f [] = Right f

  advance :: Cart -> Cart
  advance ((i,j),v@((di,dj),_)) = ((i+di,j+dj),v)

  turn :: Cart -> Cart
  turn (p,(d@(i,j),t)) = case track M.! p of
    Straight -> (p,(d,t))
    Curve1 -> (p,((-j,-i),t))
    Curve2 -> (p,((j,i),t))
    Cross -> (p,(d',(t+1) `mod` 3)) where
      d' = case t of 0 -> (-j,i)
                     1 -> d
                     2 -> (j,-i)

