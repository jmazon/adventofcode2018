-- TODO: reader monad for `instr`

import Data.Char
import Data.Bits
import Data.List
import Debug.Trace

data Op = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI
          | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
            deriving (Enum,Read,Show,Eq,Ord)

type Regs = [Int]

op :: Op -> [Int] -> Regs -> Regs
op ADDR = instr (+)   reg reg
op ADDI = instr (+)   reg imm
op MULR = instr (*)   reg reg
op MULI = instr (*)   reg imm
op BANR = instr (.&.) reg reg
op BANI = instr (.&.) reg imm
op BORR = instr (.|.) reg reg
op BORI = instr (.|.) reg imm
op SETR = instr const reg undefined
op SETI = instr const imm undefined

op GTIR = instr ((fromEnum .) . (>)) imm reg
op GTRI = instr ((fromEnum .) . (>)) reg imm
op GTRR = instr ((fromEnum .) . (>)) reg reg
op EQIR = instr ((fromEnum .) . (==)) imm reg
op EQRI = instr ((fromEnum .) . (==)) reg imm
op EQRR = instr ((fromEnum .) . (==)) reg reg

reg = flip (!!)
imm = const
setReg i v regs = pre ++ v : suf where (pre,(_:suf)) = splitAt i $ regs

op' m (i:os) = traceShow (m!!i,os) $ op (m !! i) os
instr f i1 i2 [a,b,c] regs = setReg c (f (i1 a regs) (i2 b regs)) regs

parseOp l = op (read $ map toUpper opCode) (map read operands)
  where (opCode:operands) = words l

exec ipR code (ip,regs) = traceShow (ip,regs) ((regs' !! ipR) + 1,regs') where
  regs' = (code !! ip) $ setReg ipR ip regs

run ipR code regs0 = head regs where
  Just (ip,regs) = find (((||) <$> (< 0) <*> (>= length code)) . fst) $
                   iterate (exec ipR code) (0,regs0)
main = do
  Just ipR <- fmap read . stripPrefix "#ip " <$> getLine
  code <- map parseOp . lines <$> getContents
  -- print $ run ipR code [0,0,0,0,0,0]
  print $ run ipR code [1,0,0,0,0,0]
  return ()
