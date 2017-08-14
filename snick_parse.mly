/* | Ocamlyacc Parser for Snick language specification | */
/* | ------------------------------------------------- | */
/* | Parser used to generate abstract syntax tree by   | */
/* | analyzing tokens given by the Lexer               | */
/* | ------------------------------------------------- | */

%{
open Snick_ast

let parse_error msg = Printf.eprintf "%s\n" msg
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> STR_CONST
%token <string> IDENT
%token BOOL INT FLOAT
%token AND OR NOT
%token IF THEN ELSE FI
%token WHILE DO OD
%token PROC END
%token REF VAL
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN
%token LSQBRACKET RSQBRACKET
%token DOUBLEDOT COMMA
%token EQ NOTEQ LT GT
%token LTEQ GTEQ
%token PLUS MINUS MUL DIV
%token SEMICOLON
%token EOF

%left OR
%left AND
%nonassoc UNOT
%nonassoc EQ LT GT LTEQ GTEQ NOTEQ
%left PLUS MINUS
%left MUL DIV
%nonassoc UMINUS

%type <Snick_ast.program> program

%start program
%%
program:
  procs { { procs = List.rev $1 } }

procs:
  | procs proc { $2 :: $1 }
  | proc       { [$1] }

proc:
  /* Empty process header */
  | PROC IDENT LPAREN RPAREN proc_body END { ($2, [], $5) }
  | PROC IDENT LPAREN proc_args RPAREN proc_body END { ($2, List.rev $4, $6) }

proc_args:
  | proc_args COMMA arg { $3 :: $1 }
  | arg                 { [$1] }

arg:
  | arg_pass_type typespec IDENT { ($1, $2, $3) }

arg_pass_type:
  | VAL { Val }
  | REF { Ref }

proc_body:
  decls stmts { (List.rev $1, List.rev $2) }

/* 2 types of declarations, one regular, one for arrays */
decl :
  | typespec IDENT SEMICOLON { RegDecl ($2, $1) }
  | typespec IDENT LSQBRACKET intervals RSQBRACKET SEMICOLON { ArrayDecl ($2, $1, List.rev $4) }

decls :
  | decls decl { $2 :: $1 }
  | { [] }

typespec :
  | BOOL { Bool }
  | INT { Int }
  | FLOAT { Float }

/* Builds stmts in reverse order */
stmts:
  | stmts stmt { $2 :: $1 }
  | { [] }

stmt :
  | stmt_body SEMICOLON { $1 }
  | IF expr THEN stmts FI { Ifthen ($2, List.rev $4) }
  | IF expr THEN stmts ELSE stmts FI { IfthenElse ($2, List.rev $4, List.rev $6) }
  | WHILE expr DO stmts OD { WhileDo ($2, List.rev $4) }

stmt_body:
  | proc_call { ProcCall $1 }
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | lvalue ASSIGN rvalue { Assign ($1, $3) }

/* Process call with either no args or list of args */
proc_call:
  | IDENT LPAREN RPAREN       { ($1, []) }
  | IDENT LPAREN exprs RPAREN { ($1, List.rev $3) }

rvalue :
  | expr { Rexpr $1 }

/* Two types of variables, regular and array variable */
lvalue:
  | IDENT { LId $1 }
  | IDENT LSQBRACKET exprs RSQBRACKET { LArray ($1, List.rev $3) }

literal:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | FLOAT_CONST { Efloat $1 }
  | STR_CONST { Estring $1 }

binop:
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr DIV expr { Ebinop ($1, Op_div, $3) }
  /* Comparison */
  | expr EQ expr { Ebinop ($1, Op_eq, $3) }
  | expr LT expr { Ebinop ($1, Op_lt, $3) }
  | expr GT expr { Ebinop ($1, Op_gt, $3) }
  | expr LTEQ expr { Ebinop ($1, Op_lteq, $3) }
  | expr GTEQ expr { Ebinop ($1, Op_gteq, $3) }
  | expr NOTEQ expr { Ebinop ($1, Op_noteq, $3) }
  /* boolean ops */
  | expr AND expr { Ebinop ($1, Op_and, $3) }
  | expr OR expr { Ebinop ($1, Op_or, $3) }

unop:
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  | NOT expr %prec UNOT { Eunop (Op_not, $2) }

/* Interval e.g. [1..8], must be int constants or meaningless
   this is only used when declaring arrays */
interval:
  | INT_CONST DOUBLEDOT INT_CONST { Interval ($1, $3) }

/* List of comma separated intervals, non-empty
   Used for declaring multidimensional arrays
   i.e. int x[1..2,2..3,5..8] is a 3 dimensional array
*/
intervals:
  | intervals COMMA interval { $3 :: $1 }
  | interval { [$1] }

/* list of expressions, non-empty */
exprs:
  | exprs COMMA expr { $3 :: $1 }
  | expr             { [$1] }

expr:
  | literal { $1 }
  | lvalue { Elval $1 }
  /* Binary operators */
  | binop { $1 }
  | unop { $1 }
  | LPAREN expr RPAREN { $2 }
