import Data.Char
import Data.Bits
import Data.List
import Debug.Trace

data Op = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI
          | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
            deriving (Enum,Read,Show,Eq,Ord)

op :: Op -> [Int] -> String
op ADDR = instr (binop "+") reg reg
op ADDI = instr (binop "+") reg imm
op MULR = instr (binop "*") reg reg
op MULI = instr (binop "*") reg imm
op BANR = instr (binop "&") reg reg
op BANI = instr (binop "&") reg imm
op BORR = instr (binop "|") reg reg
op BORI = instr (binop "|") reg imm
op SETR = instr const reg undefined
op SETI = instr const imm undefined

op GTIR = instr (binop ">")  imm reg
op GTRI = instr (binop ">")  reg imm
op GTRR = instr (binop ">")  reg reg
op EQIR = instr (binop "==") imm reg
op EQRI = instr (binop "==") reg imm
op EQRR = instr (binop "==") reg reg

binop op a b = "(" ++ a ++ op ++ b ++ ")"
reg = ("r" ++) . show
imm = show

instr f i1 i2 [a,b,c] = reg c ++ " = " ++ f (i1 a) (i2 b)

parseOp l = op (read $ map toUpper opCode) (map read operands)
  where (opCode:operands) = words l

decorate ipReg i op = "lbl" ++ show i ++ ": " ++
                      reg ipReg ++ " = ip = " ++ show i ++ "; " ++
                      op ++ "; " ++
                      "if (" ++ reg ipReg ++ " != ip) { " ++
                      "ip = " ++ reg ipReg ++ "; break; " ++
                      "} else ip++;"

main = do
  Just ipR <- fmap read . stripPrefix "#ip " <$> getLine
  mapM_ putStrLn [ "#include <stdio.h>"
                 , "#include <stdint.h>"
                 , "static uint64_t solve() {"
                 , "uint64_t r0=1, r1=0, r2=0, r3=0, r4=0, r5=0, ip=0;"
                 ,--  "for (;;) {"
                 , "switch (ip) {" ]
  interact $ unlines . zipWith (decorate ipR) [0..] . map parseOp . lines
  mapM_ putStrLn [ "default: return r0;"
                 , "}"
                 , "ip++;"
                 -- , "}"
                 , "}"
                 , "int main() {"
                 , "printf(\"%d\\n\", solve());"
                 , "return 0;"
                 , "}" ]
