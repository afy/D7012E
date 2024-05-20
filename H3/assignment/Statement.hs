module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Read String |
    Write Expr.T | 
    Begin [Statement] |
    While Expr.T Statement 
    deriving Show

-- Builders: Parse out format, then pass on to build using >->
-- (string is parsed at that point, valid build)
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- Format: "skip;"
skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip


-- Exec
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input




-- Parsing
-- (CoreParser.hs) parse: 
-- (CoreParser.hs) tostring: 
instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"

