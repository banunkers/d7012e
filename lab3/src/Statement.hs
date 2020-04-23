-- Hugo Wangler - hugwan-6
module Statement
  ( T
  , parse
  , toString
  , fromString
  , exec
  )
where
import           Prelude                 hiding ( return
                                                , fail
                                                , read
                                                )
import           Parser                  hiding ( T )
import qualified Dictionary
import qualified Expr

type T = Statement

data Statement =
  Assignment String Expr.T
  | Read String
  | Write Expr.T
  | Skip
  | If Expr.T Statement Statement
  | While Expr.T Statement
  | Begin [Statement]
  deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

read = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write

skip = accept "skip" -# require ";" >-> buildSkip
buildSkip _ = Skip

if' =
  (accept "if" -# Expr.parse #- require "then")
    #   (parse #- require "else")
    #   parse
    >-> buildIf
buildIf ((cond, thenStmt), elseStmt) = If cond thenStmt elseStmt

while = (accept "while" -# Expr.parse #- require "do") # parse >-> buildWhile
buildWhile (cond, doStmt) = While cond doStmt

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts : stmts) dict input =
  if Expr.value cond dict > 0
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input
exec (While cond doStmt : stmts) dict input = if Expr.value cond dict > 0
  then exec (doStmt : While cond doStmt : stmts) dict input
  else exec stmts dict input
exec (Begin beginStmts : stmts) dict input =
  exec (beginStmts ++ stmts) dict input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Assignment ident e : stmts) dict input =
  exec stmts (Dictionary.insert (ident, Expr.value e dict) dict) input
exec (Read ident : stmts) dict input =
  exec stmts (Dictionary.insert (ident, head input) dict) (tail input)
exec (Write e : stmts) dict input = Expr.value e dict : exec stmts dict input
exec [] _ _ = []

instance Parse Statement where
  parse = read ! write ! skip ! assignment ! if' ! while ! begin
  toString (If cond thenStmts elseStmts) =
    "if "
      ++ Expr.toString cond
      ++ " then\n\t"
      ++ toString thenStmts
      ++ "\nelse\n\t"
      ++ toString elseStmts
  toString (While cond doStmts) =
    "while " ++ Expr.toString cond ++ " do\n\t" ++ toString doStmts
  toString Skip                 = "skip;"
  toString (Assignment ident e) = ident ++ " := " ++ Expr.toString e
  toString (Begin beginStmts) =
    "begin\n\t" ++ toStringStmts beginStmts ++ "end"
   where
    toStringStmts [stmt        ] = toString stmt
    toStringStmts (stmt : stmts) = toString stmt ++ "\n" ++ toStringStmts stmts
  toString (Write ident) = "write " ++ Expr.toString ident
  toString (Read  ident) = "read " ++ ident
