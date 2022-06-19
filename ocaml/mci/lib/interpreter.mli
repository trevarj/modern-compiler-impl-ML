(* Identifier type *)
type id = string

(* Binary operations  *)
type binop = Plus | Minus | Times | Div

(* Mutually recursive enum type of statements and expressions *)
type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * expr
  | PrintStm of expr list

and expr =
  | IdExp of id
  | NumExp of int
  | OpExp of expr * binop * expr
  | EseqExp of stm * expr

(*

   Exercise 1.

   Write a function (maxargs : stm->int) that tells the maximum number of arguments
   of any `print` statement within and subexpression of a given statement.
   For example, `maxargs(prog)` is 2.

*)

val maxargs : stm -> int
val maxargsexpr : expr -> int

(* Interprets a program/statement in this straight-line language *)
val interp : stm -> unit
