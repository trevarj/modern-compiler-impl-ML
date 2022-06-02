module Main

import Data.List
import Data.Maybe
import Data.Either

||| A simple interpreter for the straight-line programming language
||| from Chapter 1 of Modern Compiler Implementation in ML

data BinOp = Plus | Minus | Times | Div

mutual
    data Expr = Id String
              | Num Int
              | Op Expr BinOp Expr
              | Eseq Statement Expr

    data Statement = CompoundStmt Statement Statement
                   | AssignStmt String Expr
                   | PrintStmt (List Expr)

data Error = VariableNotSet String

Show Error where
    show (VariableNotSet v) = "Variable " ++ v ++ " not set."

-- a sample program
prog = 
    CompoundStmt (AssignStmt "a" (Op (Num 5) Plus (Num 3))) 
                 (CompoundStmt 
                    (AssignStmt "b" (Eseq 
                                          (PrintStmt [(Id "a"), (Op (Id "a") Minus (Num 1))])
                                          (Op (Num 10) Times (Id "a"))))
                    (PrintStmt [(Id "b")]))

||| Returns the maximum number of print statements within any subexpression of
||| a given statement. For example, `maxArgs prog` is 2.
maxArgs : Statement -> Nat
maxArgs (PrintStmt args) = length args
maxArgs (CompoundStmt s1 s2) = max (maxArgs s1) (maxArgs s2)
maxArgs (AssignStmt _ expr) = exprArgs expr where 
    exprArgs : Expr -> Nat
    exprArgs (Op e1 _ e2) = max (exprArgs e1) (exprArgs e2)
    exprArgs (Eseq s e) = max (maxArgs s) (exprArgs e)
    exprArgs _ = 0


---- 
---- Part 2. Interpreter
----

binOp : Int -> BinOp -> Int -> Int
binOp x Plus y = x + y
binOp x Minus y = x - y
binOp x Times y = x * y
binOp x Div y = div x y

varLookup : (id : String) -> List (String, Int) -> Either Error (Int, List (String, Int))
varLookup id table = case lookup id table of
                       Nothing => Left $ VariableNotSet id
                       (Just x) => Right (x, table)
mutual
    evalExpr : List (String, Int) -> Expr -> Either Error (Int, List (String, Int))
    evalExpr t (Id id) = varLookup id t
    evalExpr t (Num num) = Right (num, t)
    evalExpr t (Op expr1 op expr2) = do (x, t) <- evalExpr t expr1
                                        (y, t) <- evalExpr t expr2
                                        Right (binOp x op y, t)
    evalExpr t (Eseq stmt expr) = do table <- evalStatement t stmt
                                     evalExpr table expr

    
    evalStatement : List (String, Int) -> Statement -> Either Error (List (String, Int))
    evalStatement t (CompoundStmt s1 s2) = do t1 <- evalStatement t s1
                                              evalStatement t1 s2
    evalStatement t (AssignStmt id expr) = do (res, t) <- evalExpr t expr
                                              Right $ (id, res) :: t
    evalStatement t (PrintStmt []) = Right t
    evalStatement t (PrintStmt (expr :: exprs)) = do (res, t1) <- evalExpr t expr
                                                     pure $ unsafePerformIO (putStrLn $ show res)
                                                     evalStatement t1 (PrintStmt exprs)


||| Interpreter
||| REPL> :exec interp prog
interp : Statement -> IO ()
interp stmt = case evalStatement [] stmt of
                   (Left err) => putStrLn $ show err
                   (Right table) => putStrLn $ show table

