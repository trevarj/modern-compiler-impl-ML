/* Header */

/* Declarations */
%token AND "&"
%token ARRAY
%token ASSIGN ":="
%token BREAK
%token COLON ":"
%token COMMA ","
%token DO
%token DOT "."
%token END
%token EOF
%token EQ "="
%token FOR
%token FUNCTION
%token GT ">"
%token GTEQ ">="
%token <string> IDENT
%token IF
%token IN
%token INT
%token <int> INTLIT
%token LBRACE "{"
%token LBRACK "["
%token LET
%token LPAREN "("
%token LT "<"
%token LTEQ "<="
%token MINUS "-"
%token NIL
%token OF
%token OR "|"
%token PLUS "+"
%token RBRACE "}"
%token RBRACK "]"
%token RPAREN ")"
%token SEMICOLON ";"
%token SLASH "/"
%token STAR "*"
%token STRING
%token <string> STRINGLIT
%token THEN
%token TYPE
%token UNIT "()"
%token VAR
%token WHILE

%start <unit> main

%%

/* Rules */ 
let main := expr; EOF

let expr :=
    | "()"
