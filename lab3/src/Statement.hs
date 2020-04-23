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
exec []                _    _     = []

-- helper fucntion for toString which appends correct number of tabs
formatToString :: T -> Int -> String
formatToString (If cond thenStmts elseStmts) tabs =
  formatTabs tabs
    ++ "if "
    ++ Expr.toString cond
    ++ " then\n"
    ++ formatToString thenStmts (tabs + 1)
    ++ "\n"
    ++ formatTabs tabs
    ++ "else\n"
    ++ formatToString elseStmts (tabs + 1)
formatToString (While cond doStmts) tabs =
  "while " ++ Expr.toString cond ++ " do\n" ++ formatTabs tabs ++ formatToString
    doStmts
    (tabs + 1)
formatToString Skip tabs = formatTabs tabs ++ "skip;"
formatToString (Assignment ident e) tabs =
  formatTabs tabs ++ ident ++ " := " ++ Expr.toString e ++ ";"
formatToString (Begin beginStmts) tabs =
  formatTabs tabs
    ++ "begin\n"
    ++ toStringStmts beginStmts (tabs + 1)
    ++ "\n"
    ++ formatTabs tabs
    ++ "end"
 where
  toStringStmts :: [T] -> Int -> String
  toStringStmts [stmt] tabs = formatToString stmt tabs
  toStringStmts (stmt : stmts) tabs =
    formatToString stmt tabs ++ "\n" ++ toStringStmts stmts tabs
formatToString (Write ident) tabs = formatTabs tabs ++ "write " ++ Expr.toString ident ++ ";"
formatToString (Read  ident) tabs = formatTabs tabs ++ "read " ++ ident ++ ";"

formatTabs :: Int -> String
formatTabs 0 = ""
formatTabs n = "    " ++ formatTabs (n - 1)

instance Parse Statement where
  parse = read ! write ! skip ! assignment ! if' ! while ! begin
  toString stmts = formatToString stmts 0
