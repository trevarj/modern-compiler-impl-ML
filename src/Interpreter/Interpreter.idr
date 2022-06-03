module Interpreter.Interpreter

import Data.List
import Data.Maybe
import Data.Either

----  A simple interpreter for the straight-line programming language
----  from Chapter 1 of Modern Compiler Implementation in ML

----
---- Types
----

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

-- Variable table alias
Table : Type
Table = List (String, Int)

-- a sample program
prog = 
    CompoundStmt (AssignStmt "a" (Op (Num 5) Plus (Num 3))) 
                 (CompoundStmt 
                    (AssignStmt "b" (Eseq 
                                          (PrintStmt [(Id "a"), (Op (Id "a") Minus (Num 1))])
                                          (Op (Num 10) Times (Id "a"))))
                    (PrintStmt [(Id "b")]))


----
---- Part 1.
----

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
---- Part 2. Interpreter for "straight-line" language
----

binOp : Int -> BinOp -> Int -> Int
binOp x Plus y = x + y
binOp x Minus y = x - y
binOp x Times y = x * y
binOp x Div y = div x y

varLookup : (id : String) -> Table -> Either Error (Int, Table)
varLookup id table = case lookup id table of
                       Nothing => Left $ VariableNotSet id
                       (Just x) => Right (x, table)
mutual
    evalExpr : Table -> Expr -> Either Error (Int, Table)
    evalExpr table (Id id) = varLookup id table
    evalExpr table (Num num) = Right (num, table)
    evalExpr table (Op expr1 op expr2) = do (x, t) <- evalExpr table expr1
                                            (y, t) <- evalExpr t expr2
                                            Right (binOp x op y, t)
    evalExpr table (Eseq stmt expr) = do t <- evalStatement table stmt
                                         evalExpr t expr

    
    evalStatement : Table -> Statement -> Either Error Table
    evalStatement table (CompoundStmt s1 s2) = do t1 <- evalStatement table s1
                                                  evalStatement t1 s2
    evalStatement table (AssignStmt id expr) = do (res, t) <- evalExpr table expr
                                                  Right $ (id, res) :: t
    evalStatement table (PrintStmt []) = Right table
    evalStatement table (PrintStmt (expr :: exprs)) = do (res, t1) <- evalExpr table expr
                                                         pure $ unsafePerformIO (putStrLn $ show res)
                                                         evalStatement t1 (PrintStmt exprs)


||| Interpreter
||| REPL> :module Interpreter.Interpreter
||| REPL> :exec interp prog
public export
interp : Statement -> IO ()
interp stmt = case evalStatement [] stmt of
                   (Left err) => putStrLn $ "Error: " ++ show err
                   (Right table) => putStrLn $ show table
