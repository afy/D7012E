-- Haskell program parser
-- Student: Hannes Furhoff, hanfur-0@student.ltu.se
-- No code has been edited by student, except for comments.

{- Test for Program -}
module TestProgram where

import Program
p, p1, p2, p3 :: Program.T
p = fromString  ("\
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
\  end")

p1 = fromString  ("\
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
\write s;")

p2 = fromString ("\
\count := 0;\
\repeat\
\   begin\
\       count := count + 1;\
\   end\
\until 1;\
\write count;")

p3 = fromString ("\
\read k;\
\write k;")

sp = putStr (toString p)

-- Returns: [3,6,9,12,15] OK
rp = Program.exec p [3,16]

-- Returns [0,0,0,0,0,0,0,0,0,0,1,10000000000] NOT SURE 
rp1 = Program.exec p1 [1024, 2]

-- Returns: [1] OK
rp2 = Program.exec p2 []

-- Returns: [1337] OK
rp3 = Program.exec p3 [1337]