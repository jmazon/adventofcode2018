-- TODO: reader monad for `instr`

import Data.Bits
import Data.List
import Data.List.Split
import Control.Arrow
import Debug.Trace

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
          | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
            deriving (Enum,Show,Eq,Ord)

type Regs = [Int]

op :: Op -> [Int] -> Regs -> Regs
op Addr = instr (+)   reg reg
op Addi = instr (+)   reg imm
op Mulr = instr (*)   reg reg
op Muli = instr (*)   reg imm
op Banr = instr (.&.) reg reg
op Bani = instr (.&.) reg imm
op Borr = instr (.|.) reg reg
op Bori = instr (.|.) reg imm
op Setr = instr const reg undefined
op Seti = instr const imm undefined

op Gtir = instr ((fromEnum .) . (>)) imm reg
op Gtri = instr ((fromEnum .) . (>)) reg imm
op Gtrr = instr ((fromEnum .) . (>)) reg reg
op Eqir = instr ((fromEnum .) . (==)) imm reg
op Eqri = instr ((fromEnum .) . (==)) reg imm
op Eqrr = instr ((fromEnum .) . (==)) reg reg

reg = flip (!!)
imm = const
setReg i v regs = traceShow (i,v,regs) $ traceShowId $ pre ++ v : suf where (pre,(_:suf)) = splitAt i $ regs

op' m (i:os) = traceShow (m!!i,os) $ op (m !! i) os
instr f i1 i2 [a,b,c] regs = setReg c (f (i1 a regs) (i2 b regs)) regs

testSample [before,opCode,after] = (opN,filter test [Addr .. Eqrr]) where
  Just rs0 = read <$> stripPrefix "Before: " before
  Just rs1 = read <$> stripPrefix "After:  " after
  (opN:testedOp) = map read (words opCode)
  test i = op i testedOp rs0 == rs1

testInput = "Before: [3, 2, 1, 1]\n\
            \9 2 1 2\n\
            \After:  [3, 2, 2, 1]"

reduce [] = []
reduce m | not (null closed) = closed ++ reduce open' where
  (closed,open) = first nub $ partition (null . tail . snd) m
  open' = map (second $ (\\ concatMap snd closed)) $
          filter ((`notElem` map fst closed) . fst) open
  
parse input = (map init samples,dropWhile null (concat code)) where
  (samples,code) = break (null . head) $ chunksOf 4 $ lines input

main = do
  (samples,code) <- parse <$> getContents
  let results = map testSample samples
  print $ length $ filter ((>= 3) . length . snd) results
  let opMap = map (head . snd) $ traceShowId $ sort $ reduce results
  print $ foldl' (flip $ op' opMap) [0,0,0,0] $ map (map read . words) code
