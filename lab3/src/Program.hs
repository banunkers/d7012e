-- Hugo Wangler - hugwan-6
module Program
  ( T
  , parse
  , fromString
  , toString
  , exec
  )
where
import           Parser                  hiding ( T )
import qualified Statement
import qualified Dictionary
import           Prelude                 hiding ( return
                                                , fail
                                                )
newtype T = Program [Statement.T] deriving Show -- to be defined
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program [stmt]) = Statement.toString stmt ++ "\n"
  toString (Program (stmt : stmts)) =
    Statement.toString stmt ++ "\n" ++ toString (Program stmts)

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty

