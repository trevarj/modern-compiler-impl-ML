type id = string
type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * expr
  | PrintStm of expr list

and expr =
  | IdExp of id
  | NumExp of int
  | OpExp of expr * binop * expr
  | EseqExp of stm * expr

let rec maxargs = function
  | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm (_, expr) -> maxargsexpr expr
  | PrintStm exprs ->
      max (List.length exprs)
        (List.fold_left (fun m exp -> max m (maxargsexpr exp)) 0 exprs)

and maxargsexpr = function
  | OpExp (e1, _, e2) -> max (maxargsexpr e1) (maxargsexpr e2)
  | EseqExp (stm, expr) -> max (maxargs stm) (maxargsexpr expr)
  | _ -> 0

let%test_module _ =
  (module struct
    let prog =
      CompoundStm
        ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
          CompoundStm
            ( AssignStm
                ( "b",
                  EseqExp
                    ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                      OpExp (NumExp 10, Times, IdExp "a") ) ),
              PrintStm [ IdExp "b" ] ) )

    let%test _ = maxargs prog = 2
  end)
