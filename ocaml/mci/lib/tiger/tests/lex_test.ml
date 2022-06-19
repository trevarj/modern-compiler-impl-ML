open Tiger.Lexer
open Lexing

let%test "and" = tokenize (from_string "&") = AND
let%test "array" = tokenize (from_string "array") = ARRAY
let%test "break" = tokenize (from_string "break") = BREAK
let%test "colon" = tokenize (from_string ":") = COLON
let%test "," = tokenize (from_string ",") = COMMA
let%test "do" = tokenize (from_string "do") = DO
let%test "." = tokenize (from_string ".") = DOT
let%test "end" = tokenize (from_string "end") = END
let%test "eof" = tokenize (from_string "") = EOF
let%test "equal" = tokenize (from_string "=") = EQ
let%test "for" = tokenize (from_string "for") = FOR
let%test "function" = tokenize (from_string "function") = FUNCTION
let%test "gt" = tokenize (from_string ">") = GT
let%test "gteq" = tokenize (from_string ">=") = GTEQ
let%test "ident" = tokenize (from_string "functional") = IDENT "functional"
let%test "if" = tokenize (from_string "if") = IF
let%test "in" = tokenize (from_string "in") = IN
let%test "int type" = tokenize (from_string "int") = INT
let%test "int lit" = tokenize (from_string "1500") = INTLIT 1500
let%test "lbrace" = tokenize (from_string "{") = LBRACE
let%test "lbrack" = tokenize (from_string "[") = LBRACK
let%test "let" = tokenize (from_string "let") = LET
let%test "lparen" = tokenize (from_string "(") = LPAREN
let%test "lt" = tokenize (from_string "<") = LT
let%test "lteq" = tokenize (from_string "<=") = LTEQ
let%test "minus" = tokenize (from_string "-") = MINUS
let%test "nil" = tokenize (from_string "nil") = NIL
let%test "of" = tokenize (from_string "of") = OF
let%test "or" = tokenize (from_string "|") = OR
let%test "plus" = tokenize (from_string "+") = PLUS
let%test "rbrace" = tokenize (from_string "}") = RBRACE
let%test "rbrack" = tokenize (from_string "]") = RBRACK
let%test "rparen" = tokenize (from_string ")") = RPAREN
let%test ";" = tokenize (from_string ";") = SEMICOLON
let%test "slash" = tokenize (from_string "/") = SLASH
let%test "star" = tokenize (from_string "*") = STAR
let%test "string type" = tokenize (from_string "string") = STRING

let%test "string lit" =
  tokenize (from_string "\"my string\"") = STRINGLIT "my string"

let%test "then" = tokenize (from_string "then") = THEN
let%test "type" = tokenize (from_string "type") = TYPE
let%test "unit" = tokenize (from_string "()") = UNIT
let%test "var" = tokenize (from_string "var") = VAR
let%test "while" = tokenize (from_string "while") = WHILE

let%test "comment" =
  tokenize (from_string "/* this is my comment with an int after */ 5")
  = INTLIT 5

let%test "unterminated comment" =
  try
    let _ = tokenize (from_string "/* this is my comment") in
    false
  with SyntaxError "Comment is not terminated" -> true

let%test "unterminated string" =
  try
    let _ = tokenize (from_string "\"this is my string") in
    false
  with SyntaxError "String is not terminated" -> true

let%test "string caret control" =
  tokenize (from_string "\"this is my string with cancel ctrl char \\^X\"")
  = STRINGLIT
      ("this is my string with cancel ctrl char " ^ Char.escaped (Char.chr 24))

let%test "string ascii code" =
  tokenize (from_string "\"this is my string with ascii codes \\084 \\114\"")
  = STRINGLIT "this is my string with ascii codes T r"

let%test "multiline string" =
  tokenize
    (from_string
       "\"multiline \\\n\t\\string with \\\\ at the end & beginning of a line\"")
  = STRINGLIT "multiline string with \\ at the end & beginning of a line"

let small_prog =
  {|
let
	type  arrtype = array of int
	var arr1:arrtype := arrtype [10] of 0
in
	arr1
end
|}

let rec chomp lexbuf tokens =
  match tokenize lexbuf with
  | EOF as eof -> List.rev (eof :: tokens)
  | token -> chomp lexbuf (token :: tokens)

let%test "small prog" =
  let got = chomp (from_string small_prog) [] in
  (* List.iter
     (fun a ->
       let s = show_token a in
       Printf.printf "%s " s)
     got; *)
  got
  = [
      LET;
      TYPE;
      IDENT "arrtype";
      EQ;
      ARRAY;
      OF;
      INT;
      VAR;
      IDENT "arr1";
      COLON;
      IDENT "arrtype";
      COLON;
      EQ;
      IDENT "arrtype";
      LBRACK;
      INTLIT 10;
      RBRACK;
      OF;
      INTLIT 0;
      IN;
      IDENT "arr1";
      END;
      EOF;
    ]
