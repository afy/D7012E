-- Lab2: Expression parsing
-- Task: See labH2.pdf

-- Most code is from: Haskell lab assignment 2 in the course D7012E by Håkan Jonsson
-- All code modifications by student are marked with a comment 
-- Author: Hannes Furhoff, hanfur-0@student.ltu.se


import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"


unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
-- ===== Unparse general apps
unparse (App oper x) = oper ++ "(" ++ unparse x ++ ")"


eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
-- ===== Eval Sin / Cos / Log / Exp
eval (App "sin" x) env = sin (eval x env)
eval (App "cos" x) env = cos (eval x env)
eval (App "log" x) env = log (eval x env)
eval (App "exp" x) env = exp (eval x env)


diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
-- ===== App derivatives
diff v (App "sin" x) = App "cos" (diff v x)
diff v (App "cos" x) = Op "-" (Const 0) (App "sin" (diff v x))
diff v (App "log" x) = Op "/" (Const 1) (diff v x) -- Natural log 
diff v (App "exp" x) = App "exp" (diff v x)
diff _ _ = error "can not compute the derivative" 


simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
-- ===== Simplify apps (just simplify x)
simplify (App "sin" x) = App "sin" (simplify x)
simplify (App "cos" x) = App "cos" (simplify x)
simplify (App "log" x) = App "log" (simplify x)
simplify (App "exp" x) = App "exp" (simplify x)



-- Task 2: mkfun
-- Note: Code gets highlighted for unnecessary lambda 
-- But this behavior is required in this task (i.e. dont remove)
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (b,Var v) = \vi -> eval b [(unparse v, vi)]


-- Task 3: N-R iteration
findzero :: String -> String -> Float -> Float
findzero x f x0
 | abs (xn - x0) < 0.0001 = xn
 | otherwise = findzero x f xn
 where fp = parse f
       fpd = diff (parse x) fp
       xn = x0 - eval (Op "/" fp fpd) [(x,x0)]