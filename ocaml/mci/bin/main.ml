open Mci.Interpreter

let _prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompoundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

(*

   Exercise 1.

   Write a function (maxargs : stm->int) that tells the maximum number of arguments
   of any `print` statement within and subexpression of a given statement.
   For example, `maxargs(prog)` is 2.

*)
