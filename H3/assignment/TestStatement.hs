-- Haskell program parser
-- Student: Hannes Furhoff, hanfur-0@student.ltu.se
-- No code has been edited by student, except for comments.

{- Testfor Statement -}
module TestStatement where

import Statement
p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11 :: Statement.T  -- %%%%%%% Test status: All passed :)            
p1 = fromString "skip;"                      
-- Skip  

p2 = fromString "read count;"                
-- Read "count"

p3 = fromString "write count+1;"             
-- Write (Add (Var "count") (Num 1))

p4 = fromString "count := 0;"                
-- Assignment "count" (Num 0)

p5 = fromString "begin skip; end"            
-- Begin [Skip]

p6 = fromString "begin x:=0; x:=x+1; end"       
-- Begin [Assignment "x" (Num 0),Assignment "x" (Add (Var "x") (Num 1))]

p7 = fromString "if x then skip; else x:=0-x;"                          
-- If (Var "x") Skip (Assignment "x" (Sub (Num 0) (Var "x")))

p8 = fromString "while n do n:=n-1;"                                   
-- While (Var "n") (Assignment "n" (Sub (Var "n") (Num 1)))

s9 = "while n do begin fac:=fac*n; n:=n-1; end"                         
p9 = fromString s9                                                      
-- While (Var "n") (Begin [Assignment "fac" (Mul (Var "fac") (Var "n")),Assignment "n" (Sub (Var "n") (Num 1))])

p10 = fromString  "begin read x ; x := x + 1 ; write x; end"           
-- Begin [Read "x",Assignment "x" (Add (Var "x") (Num 1)),Write (Var "x")]

p11 = fromString  ("begin read n; fac:=1; " ++ s9 ++ " write fac; end") 
--Begin [
--  Read "n",Assignment "fac" (Num 1),
--  While (Var "n") 
--      (Begin [Assignment "fac" (Mul (Var "fac") (Var "n")), Assignment "n" (Sub (Var "n") (Num 1))]),
--  Write (Var "fac")]



