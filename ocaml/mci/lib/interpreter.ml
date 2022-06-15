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

type table = (id * int) list

(* Look up a variable by name in the var table *)
let lookup (name : id) (t : table) =
  let _, value = List.find (fun (id, _) -> id = name) t in
  value

(* Compute binary operation *)
let evalBinOp = function
  | Plus, a, b -> a + b
  | Minus, a, b -> a - b
  | Times, a, b -> a * b
  | Div, a, b -> a / b

(* Interpret a statement with a given state and return a new state table *)
let rec (interpStm : stm * table -> table) = function
  | CompoundStm (s1, s2), t ->
      let t1 = interpStm (s1, t) in
      interpStm (s2, t1)
  | AssignStm (id, expr), t ->
      let res, t1 = interpExpr (expr, t) in
      (id, res) :: t1
  | PrintStm exprs, t ->
      List.fold_left
        (fun t expr ->
          let res, t1 = interpExpr (expr, t) in
          print_endline (string_of_int res);
          t1)
        t exprs

(* Interpret an expression and return a result and new state table *)
and (interpExpr : expr * table -> int * table) = function
  | IdExp id, t -> (lookup id t, t)
  | NumExp n, t -> (n, t)
  | OpExp (exp1, op, exp2), t ->
      let a, t1 = interpExpr (exp1, t) in
      let b, t2 = interpExpr (exp2, t1) in
      (evalBinOp (op, a, b), t2)
  | EseqExp (stm, expr), t ->
      let t1 = interpStm (stm, t) in
      interpExpr (expr, t1)

let interp stm =
  let _ = interpStm (stm, []) in
  ()

(* Tests *)
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

    let%expect_test "interpret prog" =
      interp prog;
      [%expect {| 
          8
          7
          80
      |}]
  end)
