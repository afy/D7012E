-- Haskell program parser
-- Student: Hannes Furhoff, hanfur-0@student.ltu.se
-- All code edited by student as task requires will be marked with comments

module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

-- Program is defined as a list of statements to be iteratively executed
newtype T = Program [Statement.T]

instance Parse T where
  -- Parse should iteratively parse text into a list
  -- When executed, branches will be parsed in order
  -- According to exec
  parse = iter Statement.parse >-> Program

  -- Concatmap is good here, traces the statement chain into a list
  -- While also enabling the ability to call toString on each step
  toString (Program statements) = concatMap Statement.toString statements

-- Start execution chain (partial application is responsible for input)    
exec (Program statements) = Statement.exec statements Dictionary.empty
