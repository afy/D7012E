-- Haskell program parser
-- Student: Hannes Furhoff, hanfur-0@student.ltu.se
-- All code edited by student as task requires will be marked with comments

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
    Begin|
    While Expr.T Statement 
    deriving Show

-- Builders: Parse out format, then pass on to build using >->
-- (string is parsed at that point, valid build)
parse_assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- Format: "if <expr> then <statement> else statement"
--  Messy input due to nested expr/stmnts but works for now. Look into cons before build call
parse_ifelse = accept "if" # Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfElse
buildIfElse ((e,st),sf) = If e st sf

-- Format: "skip;"
parse_skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

-- Format: "read <string>;"
parse_read = accept "read" # word #- require ";" >-> buildRead
buildRead (v) = Read v

-- Format: "write <expr>;"
parse_write = accept "write" # Expr.parse #- require ";" >-> buildWrite
buildWrite (expr) = Write expr 

-- Format: Begin
parse_begin = accept "begin" >-> buildBegin
buildBegin _ = Begin

-- Format: "while <expr> do"
parse_while = accept "while" # Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e,s) = While e s






-- Execution of parsed statements
-- exec Program = exec (StatementList) = exec (this_stmt : next_stmts)
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

-- Assignment: string (dict key) and value inserted into dict
--    continue other statements with new dict (no input modification)
-- Dictionary.insert: (k,v), dict => new_dict
exec (Assignment var expr : next_stmts) dict input =
    exec next_stmts (Dictionary.insert (var, (Expr.value expr dict)) dict) input

-- IfElse: If condition is met: go into the if-statement chain followed by the coming statements
-- For else, same thing applies, with the same coming statements
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

-- Skip: Move on to next statement without doing anything
exec (Skip : next_stmts) dict input =
    exec next_stmts dict input

-- Read: Read input[0] to variable with name string
-- Note: removes it from list (so next read statement can read val)
exec (Read var: next_stmts) dict input =
    exec next_stmts (Dictionary.insert (var, head input) dict) (tail input)

-- Write: Output is handled as a return from the nested exec calls. 
-- So, write simply adds to the return list.   WriteOutput : exec next
-- Note, add a catch-all base list to prevent recursion problems
exec (Write expr : next_stmts) dict input =
    Expr.value expr dict : exec next_stmts dict input

-- Begin: 

-- While: If condition is false (0, value expr is Int), move on as past
-- If it is positive, 
exec (While cond s : next_stmts) dict input 
 | cond == 0 = exec next_stmts dict input
 | otherwise =  0

-- Catch-all
exec [] _ _ = []






-- Parsing
-- (CoreParser.hs) parse: string -> Statement (Builders defined earlier)
-- (CoreParser.hs) toString: Statement -> string
instance Parse Statement where
  -- parse behavior: Identify struct (using (!) on all possible builders until success) 
  parse = parse_assignment ! parse_ifelse ! parse_skip ! parse_read 
                           ! parse_write ! parse_begin ! parse_while ! err "No matching parser"
  -- toString: use the helper stmtToStr below (for matching Statement construct)
  toString = stmtToStr





-- Format: "<var> := <expr>;"
stmtToStr :: Statement -> String
stmtToStr (Assignment v e) = Expr.toString v ++ ":=" ++ Expr.toString e ++ ";"

-- Format: "if <expr> then <statement> else statement>"
stmtToStr (If e st sf) = "if"++Expr.toString e++"then"++toString st++"; else"++toString sf++";"

-- Format: "skip;"
stmtToStr (Skip) = "skip;"

-- Format: "read <string>;"
stmtToStr (Read v) = "read" ++ v ++ ";"

-- Format: "write <expr>;"
stmtToStr (Write e) = "write" ++ Expr.toString e ++ ";"

-- Format: Begin
stmtToStr (Begin) = "begin"

-- Format: "while <expr> do"
stmtToStr (While e s) = "while" ++ Expr.toString e ++ "do" ++ toString s