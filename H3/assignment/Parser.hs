-- Haskell program parser
-- Student: Hannes Furhoff, hanfur-0@student.ltu.se
-- All code edited by student as task requires will be marked with comments

module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

-- Apply m # n in seq => apply k a (snd (Res1, Res2)) 
-- get snd val => throw away fst val
-- (Infix already declared)
(-#) :: Parser a -> Parser b -> Parser b
m -# n = (m # n) >-> snd

-- Same as -# But getting the first tuple value from seq 
-- (Infix already declared)
(#-) :: Parser a -> Parser b -> Parser a
m #- n = (m # n) >-> fst

-- Iterate until failure (iter): (get char and apply isSpace=Bool) = predicate
spaces :: Parser String
spaces = iter (char ? isSpace) 

token :: Parser a -> Parser a
token m = m #- spaces

-- Apply char to extract first char, then pass to boolean predicate isAlpha
letter :: Parser Char
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- Recurse and apply char in sequence, gives Just(('a', char "b....", ...
-- Use >-> cons to concat into >-> Just("ab", char ....)
-- Remember to catch base case n=0 with value [] for cons (:[])
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- Do accept w, catch Nothing via ! statement with err
require :: String -> Parser String
require w = (accept w) ! err ("Expecting "++w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

