module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail, read)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Read String |
    Write Expr.T |
    Skip |
    Begin [Statement]|
    While Expr.T Statement 
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifState = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf ((e, x), y) = If e x y

read = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

skip = accept "skip" # require ";" >-> bSkip
bSkip _ = Skip 

write = accept "write" -# Expr.parse #- require ";" >-> bWrite
bWrite w = Write w

begin = accept "begin" -# iter parse #- require "end" >-> bBegin
bBegin s = Begin s

while = accept "while" -# Expr.parse # require "do" -# parse >-> bWhile
bWhile (e, s) = While e s


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

-- if the stmt list is empty, return empty list
exec [] _ _ = []

-- for If: is if true execute thenStmts, otherwise execute elseStmts
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

-- for Assignment: recursively execute the rest of stmts with the updated dictionary
exec (Assignment str expr: stmts) dict input = exec stmts updatedDict input
  where updatedDict = Dictionary.insert (str, Expr.value expr dict) dict

-- for Read: execute the rest of stmts recursively with the rest of the inputs and
-- the updated dictionary were the input is inserted recursively
exec (Read str : stmts) dict (x:xs) = exec stmts updatedDict xs
  where updatedDict = Dictionary.insert (str, x) dict

exec (Write expr : stmts) dict input = value : exec stmts dict input
  where value = Expr.value expr dict

exec (While cond stmt : stmts) dict input = 
    if (Expr.value cond dict) > 0
    then exec stmtsWithWhile dict input
    else exec stmts dict input
      where stmtsWithWhile = stmt : While cond stmt : stmts

exec (Skip : stmts) dict input = exec stmts dict input

exec (Begin xs: stmts) dict input = exec (xs ++ stmts) dict input
 
instance Parse Statement where
  parse = assignment ! ifState ! read ! write ! begin ! while ! skip
  toString = error "Statement.toString not implemented"
