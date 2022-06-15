module Interpreter.Interpreter

import Control.Relation
import Data.List
import Decidable.Equality

----  A simple interpreter for the straight-line programming language
----  from Chapter 1 of Modern Compiler Implementation in ML

----
---- Types
----

data BinOp = Plus | Minus | Times | Div

-- Variable scope alias
Scope : Type
Scope = List (String, Integer)

----
---- Proof that a Pair with given key is in a List
----
data Key : key -> List (String, Integer) -> Type where
     Here : Key key ((key, val) :: xs)
     There : Key key xs -> Key key (y :: xs)

-- FIXME: Can I use proof-search auto-implicits here?
lookupVar : (key : String) -> (scope : Scope) -> (prf : Key key scope) -> Integer
lookupVar key ((key, val) :: xs) (Here) = val
lookupVar key ((other, _) :: xs) (There p) = lookupVar key xs p

binOp : Integer -> BinOp -> Integer -> Integer
binOp x Plus y = x + y
binOp x Minus y = x - y
binOp x Times y = x * y
binOp x Div y = div x y



mutual
    data Expr : Scope -> Scope -> Type where
         Id   : (key : String) -> (prf : Key key sIn) -> Expr sIn []
         Num  : (res : Integer) -> Expr sIn []
         Op   : Expr sIn sOut -> (op : BinOp) -> Expr sOut sOut2 -> Expr sIn sOut2
         Eseq : Statement sIn sOut -> Expr sOut sOut2 -> Expr sIn sOut2
    
    data Statement : Scope -> Scope -> Type where
         CompoundStmt : Statement sIn sOut -> Statement sOut sOut2 -> Statement sIn sOut2
         AssignStmt   : (name : String) -> Expr sIn sOut -> Statement sIn ((name, res) :: sIn)
         PrintStmt    : List (Expr sIn []) -> Statement sIn sOut

-- a sample program
-- prog = 
--     CompoundStmt (AssignStmt "a" (Op (Num 5) Plus (Num 3))) 
--                  (CompoundStmt 
--                     (AssignStmt "b" (Eseq 
--                                           (PrintStmt [(Op (Id "a" Here) Minus (Num 1))])
--                                           (Op (Num 10) Times (Id "a" Here))))
--                     (PrintStmt [(Id "b" Here)]))

idTest : Expr [("a", 5)] []
idTest = Id "a" Here

num5 : Expr [] []
num5 = Num 5

assign : Statement [] [("a", 4)]
assign = AssignStmt "a" (Num 4)

add : Expr [] []
add = Op num5 Plus (Num 1)

-- print : Statement []
-- print = PrintStmt [(Num 5)]

eseq : Expr [] []
eseq = Eseq (assign) (Id "a" Here)

-- prog : Statement [] [("a", 4)]
-- prog = CompoundStmt (assign) (PrintStmt [(Id "a" Here), (Num 7), (Num 15), (Num 6)])

prog : Statement [] [("a", 4)]
prog = CompoundStmt (assign) (PrintStmt [(Id "a" Here), (Num 7), (Num 15), (Num 6)])
----
---- Part 1.
----

-- ||| Returns the maximum number of print statements within any subexpression of
-- ||| a given statement. For example, `maxArgs prog` is 2.
-- maxArgs : Statement scope -> Nat
-- maxArgs (PrintStmt args) = length args
-- maxArgs (CompoundStmt s1 s2) = max (maxArgs s1) (maxArgs s2)
-- maxArgs (AssignStmt _ expr) = exprArgs expr where 
--     exprArgs : Expr _ _ -> Nat
--     exprArgs (Op e1 _ e2) = max (exprArgs e1) (exprArgs e2)
--     exprArgs (Eseq s e) = max (maxArgs s) (exprArgs e)
--     exprArgs _ = 0


---- 
---- Part 2. Interpreter for "straight-line" language
----

-- mutual
--     evalExpr : {scope : Scope} -> Expr scope res -> IO (Integer, Scope)
--     evalExpr (Id key prf) = pure $ (lookupVar key scope prf, scope) 
--     evalExpr (Num num) = pure (num, scope)
--     evalExpr (Op expr1 op expr2) = do (x, _) <- evalExpr expr1
--                                       (y, _) <- evalExpr expr2
--                                       pure $ (binOp x op y, scope)
--     evalExpr (Eseq stmt expr) = do t <- evalStatement scope stmt
--                                    evalExpr expr 

--     evalStatement : {sIn, sOut : Scope} -> Scope -> Statement sIn sOut -> IO Scope
--     evalStatement scope (CompoundStmt stmt1 stmt2) = do sOut <- evalStatement sIn stmt1 {sIn}
--                                                         evalStatement sOut2 stmt2
--     evalStatement scope (AssignStmt name expr) = do (res, t) <- evalExpr expr 
--                                                     pure $ (name, res) :: t
--     evalStatement scope (PrintStmt []) = pure scope
--     evalStatement scope (PrintStmt (x :: xs)) = do (res, s') <- evalExpr x
--                                                    putStrLn $ show res
--                                                    ?asd

mutual
     evalExpr : {sIn : _} -> Expr sIn sOut -> IO (Integer, Scope)
     evalExpr {sIn} (Id key prf) = pure $ (lookupVar key sIn prf, [])
     evalExpr {sIn} (Num res) = pure (res, [])
     evalExpr {sIn} (Op exp1 op exp2) = do (x, sOut) <- evalExpr exp1
                                           (y, sOut2) <- evalExpr exp2 
                                           pure $ (binOp x op y, sOut2)
     evalExpr {sIn} (Eseq stmt exp) = do sOut <- evalStatement stmt
                                         evalExpr exp
                                         
     evalStatement : Statement sIn sOut -> IO Scope
     evalStatement (CompoundStmt stmt1 stmt2) = do sOut <- evalStatement stmt1
                                                   evalStatement stmt2
     evalStatement (AssignStmt name x) = ?evalStatement_rhs_1
     evalStatement (PrintStmt exprs) = ?evalStatement_rhs_2

-- ||| Interpreter
-- ||| REPL> :module Interpreter.Interpreter
-- ||| REPL> :exec interp prog
-- public export
-- interp : Statement sIn sOut -> IO ()
-- interp stmt = do scope <- evalStatement [] stmt
--                  putStrLn $ show scope
