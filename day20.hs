{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bits
import Data.List
import Text.Parsec hiding (State)
import Control.Monad.State.Lazy
import Control.Arrow

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

import Control.DeepSeq

import Debug.Trace

type Pos = Int

type RE = [(RElem,Text.Parsec.Column)]
data RElem = RAtom Dir | RChoice [RE] deriving Show
data Dir = N | S | W | E deriving (Read,Show,Enum)
dirs i = map toEnum $ filter (testBit i) [0..3]

instance NFData RElem where rnf (RAtom d) = rnf d
                            rnf (RChoice c) = rnf c
instance NFData Dir where rnf !d = ()

type Maze = M.IntMap Int

regex :: Parsec String u RE
regex = char '^' *> re <* char '$'
re = many relem
relem = getPosition >>= \p -> (, sourceColumn p) <$> (ratom <|> rchoice)
ratom = RAtom . read . (: "") <$> oneOf "NSWE"
rchoice = RChoice <$> between (char '(') (char ')') (re `sepBy` char '|')

p0 :: Pos
p0 = 0

width = 100000

move p N = p - width
move p S = p + width
move p W = p - 1
move p E = p + 1

opp N = S
opp S = N
opp W = E
opp E = W

genMaze :: RE -> Maze
genMaze s = foldl' (\m (k,v) -> M.insertWith (.|.) k (bit $ fromEnum v) m) M.empty $ evalState (go p0 s) M.empty
  where
    go :: Pos -> RE -> State (M.IntMap S.IntSet) [(Pos,Dir)]
    go p ((c,i):cs) = do
      cut <- S.member i . M.findWithDefault S.empty p <$> get
      if cut then return [] else do
        modify' $ M.alter (Just . maybe (S.singleton i) (S.insert i)) p
        case c of RAtom d -> let p' = move p d in ([(p,d),(p',opp d)] ++) <$> go p' cs
                  RChoice as -> concat <$> mapM (go p . (++ cs)) as
    go _ [] = return []

bfs :: Maze -> [Int]
bfs maze = go S.empty [(p0,0)] where
  go :: S.IntSet ->  [(Pos,Int)] -> [Int]
  go _ [] = []
  go cl ((p,i):q) | p `S.member` cl = go cl q
                  | otherwise = i : go cl' (q ++ q')
    where cl' = S.insert p cl
          i' = i + 1
          q' = map (,i') $ filter (`S.notMember` cl) $ map (move p) $ dirs $ maze M.! p

test3 = "^WNE$"
test10 = "^ENWWW(NEEE|SSE(EE|N))$"
test18 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
test23 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
test31 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
tests = [test3,test10,test18,test23,test31]

main = interact $ show . fmap ((part1 &&& part2) . force' "bfs" . bfs . force' "maze" . genMaze . force' "regex") . parse regex "input"

force' tag x = force x `seq` trace tag x

test t = bfs . genMaze <$> parse regex "test" t

part1 = last
part2 = length . dropWhile (< 1000)
