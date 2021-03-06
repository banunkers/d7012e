-- Hugo Wangler - hugwan-6
-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

module Expr where

import           Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
 where
  notfirst p (_, []    ) = True
  notfirst p (_, x : xs) = not (p x)

  buildnumber :: String -> (EXPR, String)
  buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
   where
    accdigits :: (EXPR, String) -> (EXPR, String)
    accdigits (Const n, y : ys) = (Const (10 * n + (ord y - 48)), ys)

  buildvar :: String -> (EXPR, String)
  buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
   where
    accletters :: (EXPR, String) -> (EXPR, String)
    accletters (Var s, y : ys) = (Var (s ++ [y]), ys)


  buildexpr :: String -> (EXPR, String)
  buildexpr xs = until (notfirst (\c -> c == '-' || c == '+'))
                       accterms
                       (buildterm xs)
   where
    accterms :: (EXPR, String) -> (EXPR, String)
    accterms (term, y : ys) = (Op (y : []) term term1, zs)
      where (term1, zs) = buildterm ys

  buildterm :: String -> (EXPR, String)
  buildterm xs = until (notfirst (\c -> c == '*' || c == '/'))
                       accfactors
                       (buildfactor xs)
   where
    accfactors :: (EXPR, String) -> (EXPR, String)
    accfactors (fact, y : ys) = (Op (y : []) fact fact1, zs)
      where (fact1, zs) = buildfactor ys

  buildfactor :: String -> (EXPR, String)
  buildfactor []         = error "missing factor"
  buildfactor ('(' : xs) = case buildexpr xs of
    (e, ')' : ws) -> (e, ws)
    _             -> error "missing factor"
  buildfactor (x : xs)
    | isDigit x = buildnumber (x : xs)
    | isLetter x = case buildvar (x : xs) of
      (Var s, '(' : zs) ->
        let (e, ws) = buildfactor ('(' : zs) in (App s e, ws)
      p -> p
    | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n      ) = show n
unparse (Var   s      ) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App func arg ) = show func ++ "(" ++ unparse arg ++ ")"

eval :: EXPR -> [(String, Float)] -> Float
eval (Const n) _   = fromIntegral n
eval (Var   x) env = case lookup x env of
  Just y -> y
  _      -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" arg    ) env = sin (eval arg env)
eval (App "cos" arg    ) env = cos (eval arg env)
eval (App "log" arg    ) env = log (eval arg env)
eval (App "exp" arg    ) env = exp (eval arg env)

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2) | id == id2 = Const 1
                        | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) = Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) = Op
  "/"
  (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2)))
  (Op "*" e2 e2)
diff v (App "sin" arg) = Op "*" (diff v arg) (App "cos" arg)
diff v (App "cos" arg) =
  Op "*" (Const (-1)) (Op "*" (diff v arg) (App "sin" arg))
diff v (App "log" arg) = Op "/" (diff v arg) arg
diff v (App "exp" arg) = Op "*" (diff v arg) (App "exp" arg)
diff _ _               = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n ) = Const n
simplify (Var   id) = Var id
simplify (Op oper left right) =
  let (lefts, rights) = (simplify left, simplify right)
  in  case (oper, lefts, rights) of
        ("+", e      , Const 0) -> e
        ("+", Const 0, e      ) -> e
        ("*", e      , Const 0) -> Const 0
        ("*", Const 0, e      ) -> Const 0
        ("*", e      , Const 1) -> e
        ("*", Const 1, e      ) -> e
        ("-", e      , Const 0) -> e
        ("/", e      , Const 1) -> e
        ("-", le, re) -> if left == right then Const 0 else Op "-" le re
        (op , le     , re     ) -> Op op le re
simplify (App func arg) = App func (simplify arg)

-- Returns a partially applied function if called with no arg
-- which works as a function when binded and called with arg
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body, var) arg = eval body [(unparse var, arg)]

findzero :: String -> String -> Float -> Float
findzero v body v0 =
  let fn  = mkfun (parse body, parse v)
      fn' = mkfun (diff (parse v) (parse body), parse v)
  in  go fn fn' v0
 where
  go fn fn' v0 = if abs (v1 - v0) <= 0.0001 then v1 else go fn fn' v1
    where v1 = v0 - (fn v0 / fn' v0)

-- Tests
testSin =
  putStrLn (unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))"))))

testCos =
  putStrLn (unparse (simplify (diff (Var "x") (parse "exp(cos(2*x))"))))

testLog = putStrLn (unparse (simplify (diff (Var "x") (parse "log(x*x)"))))

testMkfun = mkfun (parse "x*x+2", Var "x")

testFZ1 =
  putStrLn ("0 = x*x*x+x-1 => x = " ++ show (findzero "x" "x*x*x+x-1" 1.0))
testFZ2 = putStrLn
  ("0 = cos(y)*sin(y) => y = " ++ show (findzero "y" "cos(y)*sin(y)" 2.0))
