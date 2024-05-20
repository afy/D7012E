-- Haskell program parser
-- Student: Hannes Furhoff, hanfur-0@student.ltu.se
-- No code has been edited by student, except for comments.

{- Test for Expr-}
module TestExpr where

import qualified Dictionary
import Expr

dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty 

testValue string = value (fromString string) dict

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test result: ALL PASS
n1 = testValue "1"                                                   -- Result = 1 OK
n2 = testValue "x"                                                   -- Result = 1 OK 
n3 = testValue "x+y"                                                 -- Result = 3 OK
n4 = testValue "x-y-y"                                               -- Result = -3 OK
n21 = testValue "1/(2-y)" {-  Expr.value: division by 0 -}           -- Result = Error OK
n31 = testValue "2+z"     {-  Expr.value: undefined variable z -}    -- Result = Error OK