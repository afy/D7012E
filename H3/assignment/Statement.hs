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
    Begin [Statement] |
    While Expr.T Statement |
    Repeat Statement Expr.T 
    deriving Show

-- Builders: Parse out format, then pass on to build using >->
parse_assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- Format: "if <expr> then <statement> else statement"
--  Messy input due to nested expr/stmnts but works for now.
parse_ifelse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfElse
buildIfElse ((e,st),sf) = If e st sf

-- Format: "skip;"
parse_skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

-- Format: "read <string>;"
parse_read = accept "read" -# word #- require ";" >-> buildRead
buildRead (v) = Read v

-- Format: "write <expr>;"
parse_write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite (expr) = Write expr 

-- Format: "begin <statements> end"
-- Note: iter is required on parse, as several statements can appear within
parse_begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin (stmt_chain) = Begin stmt_chain

-- Format: "while <expr> do"
parse_while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e,s) = While e s

-- Format "repeat <statement> until <expr>;"
parse_repeat = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (s,e) = Repeat s e 






-- exec Program = exec (this_stmt : next_stmts)
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

-- Assignment: string (dict key) and value inserted into dict
--    continue other statements with new dict (no input modification)
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

-- Begin: Group several statements togheter, used for multi-line whiles
-- In practice: execute group (/chain) before executing the next statement
exec (Begin stmt_chain : next_stmts) dict input =
    exec (stmt_chain ++ next_stmts) dict input

-- While: If condition is false (0, value expr is Int), move on as past
-- If it is positive: Take the clauses within the statement and append to exec chain 
-- Dont forget to add the actual While statement aswell
exec (While cond internal_stmts : next_stmts) dict input 
 | (Expr.value cond dict) > 0 = exec (internal_stmts : While cond internal_stmts : next_stmts) dict input
 | otherwise = exec next_stmts dict input

-- Repeat: Same as while, but is always ran at least once
-- Same cond case (>0) but for other execute internal_stmts before next_stmts
exec (Repeat internal_stmts cond : next_stmts) dict input = exec (internal_stmts : action) dict input where 
    action 
        | Expr.value cond dict <= 0 = Repeat internal_stmts cond : next_stmts
        | otherwise = next_stmts

-- Catch-all
exec [] _ _ = []






instance Parse Statement where

  -- parse behavior: Identify struct (using (!) on all possible builders until success) 
  parse = parse_assignment ! parse_skip ! parse_begin ! parse_ifelse 
        ! parse_while ! parse_repeat ! parse_read ! parse_write
  -- toString: use the helper stmtToStr below (for matching Statement construct)
  toString = stmtToStr


-- Format: "<string> := <expr>;"
stmtToStr :: Statement -> String
stmtToStr (Assignment v e) = v ++ " := " ++ Expr.toString e ++ ";\n"

-- Format: "if <expr> then <statementTrue> else statementFalse>"
stmtToStr (If e st sf) = "if "++Expr.toString e++" then "++toString st++"; else "++toString sf++";\n"

-- Format: "skip;"
stmtToStr (Skip) = "skip;\n"

-- Format: "read <string>;"
stmtToStr (Read v) = "read" ++ v ++ ";\n"

-- Format: "write <expr>;"
stmtToStr (Write e) = "write " ++ Expr.toString e ++ ";\n"

-- Format: "begin <statement_list> end"
stmtToStr (Begin s) = "begin " ++ concatMap toString s ++ "end \n"

-- Format: "while <expr> do <statement>"
stmtToStr (While e s) = "while " ++ Expr.toString e ++ " do " ++ toString s

-- Format "repeat <statement> until e"
stmtToStr (Repeat s e) = "repeat" ++ toString s ++ "until " ++ Expr.toString e ++ "\n"