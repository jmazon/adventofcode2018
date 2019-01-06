{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Bits
import Data.List
import Text.Parsec hiding (State)
import Control.Monad.State
import Control.Arrow

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

type RE = [(RElem,Text.Parsec.Column)]
data RElem = RAtom Dir | RChoice [RE]
data Dir = N | S | W | E deriving (Read,Enum)
dirs i = map toEnum $ filter (testBit i) [0..3]

move p N = p - width
move p S = p + width
move p W = p - 1
move p E = p + 1

opp N = S
opp S = N
opp W = E
opp E = W

type Maze = M.IntMap Int
p0 = 0
width = 100000 -- input being <15k long

main = interact $ show .
                  fmap ((last &&& length . dropWhile (< 1000)) . bfs . genMaze) .
                  parse regex "input"

regex :: Parsec String u RE
regex = char '^' *> re <* char '$'
re = many relem
relem = getPosition >>= \p -> (, sourceColumn p) <$> (ratom <|> rchoice)
ratom = RAtom . read . (: "") <$> oneOf "NSWE"
rchoice = RChoice <$> between (char '(') (char ')') (re `sepBy` char '|')

genMaze :: RE -> Maze
genMaze s = foldl' ins M.empty $ evalState (go p0 s) M.empty
  where ins m (k,v) = M.insertWith (.|.) k (bit $ fromEnum v) m
        go p ((c,i):cs) = do
          cut <- S.member i . M.findWithDefault S.empty p <$> get
          if cut then return [] else do
            modify' $ M.alter (Just . maybe (S.singleton i) (S.insert i)) p
            case c of RAtom d -> let p' = move p d
                                 in ([(p,d),(p',opp d)] ++) <$> go p' cs
                      RChoice as -> concat <$> mapM (go p . (++ cs)) as
        go _ [] = return []

bfs :: Maze -> [Int]
bfs maze = go S.empty [(p0,0)] where
  go _ [] = []
  go cl ((p,i):q) | p `S.member` cl = go cl q
                  | otherwise = i : go cl' (q ++ q')
    where cl' = S.insert p cl
          q' = map (, i+1) $ filter (`S.notMember` cl) $
               map (move p) $ dirs $ maze M.! p
