module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

-- Program is defined as a list of statements to be iteratively executed
newtype T = Program [Statement.T]

-- (CoreParser.hs) parse: 
-- (CoreParser.hs) tostring: 
instance Parse T where
  parse = error "Program.parse not implemented"
  toString = error "Program.toString not implemented"
             
exec = error "Program.exec not implemented"
