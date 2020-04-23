-- Hugo Wangler - huwan-6
{- Test for Program -}
module TestProgram where

import           Program
p, p1, repeatProgram :: Program.T
p = fromString
  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end"
  )

p1 = fromString
  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;"
  )

sp = putStr (toString p)

rp = Program.exec p [3, 16]

rp1 = Program.exec p1 [1024, 2]

repeatProgram = fromString
  ("\
\read n;\
\s := 0;\
\repeat\
\  begin\
\    s := s + n;\
\    n := n - 1;\
\  end\
\until (0 - n) + 1;\
\write s;"
  )

repeatPrint = putStr (toString repeatProgram)
repeatExec = Program.exec repeatProgram [100] -- sum 1 to 100

pr :: Program.T
pr = fromString ("\
\read k;\
\write k;\
\repeat\
\ begin\
\ k := k + 1;\
\ write k;\
\ end\
\until k;")

spr = putStr (toString pr)
rpr k = Program.exec pr [k]
