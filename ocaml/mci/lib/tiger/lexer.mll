(* ocamllex input file for Tiger *)

(* header section *)
{
    exception SyntaxError of string

    type token = 
        | AND
        | ARRAY
        | ASSIGN
        | BREAK
        | COLON
        | COMMA
        | DO
        | DOT
        | END
        | EOF
        | EQ
        | FOR
        | FUNCTION
        | GT
        | GTEQ
        | IDENT of string
        | IF
        | IN
        | INT
        | INTLIT of int
        | LBRACE
        | LBRACK
        | LET
        | LPAREN
        | LT
        | LTEQ
        | MINUS
        | NIL
        | OF
        | OR
        | PLUS
        | RBRACE
        | RBRACK
        | RPAREN
        | SEMICOLON
        | SLASH
        | STAR
        | STRING
        | STRINGLIT of string
        | THEN
        | TYPE
        | UNIT
        | VAR
        | WHILE
    [@@deriving show]
    
    (* Helper that assumes a valid caret notation character as an input
    and outputs the control character as a char *)
    let caret_to_char = function
        | '?' -> Char.escaped (Char.chr 127)
        | ch -> Char.escaped (Char.chr ((Char.code ch) lxor 0x40))
}

(* definitions section *)
let letters = ['a'-'z' 'A'-'Z']
let digits = ['0'-'9']
let ident = letters(letters | digits | '_')*
let int_lit = (digits)(digits)*
let whitespace = [' ' '\n' '\t' '\r' '\b']+
let three_digits = (digits)(digits)(digits)
let ctrl_char = ['A'-'Z' '@' '[' '\\' ']' '^' '_' '?']

(* rules section *)
rule tokenize = parse
  | "&" { AND }
  | "array" { ARRAY }
  | ":=" { ASSIGN }
  | "break" { BREAK }
  | ":" { COLON }
  | "," { COMMA }
  | "do" { DO }
  | "." { DOT }
  | "end" { END }
  | "=" { EQ }
  | "for" { FOR }
  | "function" { FUNCTION }
  | ">" { GT }
  | ">=" { GTEQ }
  | "if" { IF }
  | "in" { IN }
  | "int" { INT }
  | "{" { LBRACE }
  | "[" { LBRACK }
  | "let" { LET }
  | "(" { LPAREN }
  | "<" { LT }
  | "<=" { LTEQ }
  | "-" { MINUS }
  | "nil" { NIL }
  | "of" { OF }
  | "|" { OR }
  | "+" { PLUS }
  | "}" { RBRACE }
  | "]" { RBRACK }
  | ")" { RPAREN }
  | ";" { SEMICOLON }
  | "/" { SLASH }
  | "*" { STAR }
  | "string" { STRING }
  | "then" { THEN }
  | "type" { TYPE }
  | "()" { UNIT }
  | "var" { VAR }
  | "while" { WHILE }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | int_lit { INTLIT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' { string_literal (Buffer.create 64) lexbuf }
  | "/*" { comment lexbuf }
  | whitespace { tokenize lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and string_literal buf =
  parse
  | '"'       { STRINGLIT (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; string_literal buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; string_literal buf lexbuf }
  | '\\' '^'  { caret_ctrl_char buf lexbuf }
  | '\\' (three_digits as code) { Buffer.add_char buf (Char.chr (int_of_string code)); string_literal buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; string_literal buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; string_literal buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; string_literal buf lexbuf }
  | '\\' whitespace '\\' { string_literal buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      string_literal buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and comment = 
  parse
  | "*/" { tokenize lexbuf }
  | _ { comment lexbuf }
  | eof { raise (SyntaxError ("Comment is not terminated")) }

and caret_ctrl_char buf =
  parse
  | ctrl_char as ch { Buffer.add_string buf (caret_to_char ch); string_literal buf lexbuf }
  | _ { raise (SyntaxError ("Illegal caret notation control character: " ^ Lexing.lexeme lexbuf)) }

(* trailer section *)
{}