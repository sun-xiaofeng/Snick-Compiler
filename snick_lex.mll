(* ----------------------------------------------------- | 
 * Lexer for Snick language                              |
 * ----------------------------------------------------- |
 * Reads from OCaml input channel and returns tokens for |
 * processing by Snick parser                            |
 * ----------------------------------------------------- | *)

{
open Snick_parse

(* Define Lexing error messages *)
exception Lex_error of string

(* Gets the line number and column of current lexeme *)
let get_lex_pos lexbuf =
  let pos   = lexbuf.Lexing.lex_curr_p in
  let line  = pos.Lexing.pos_lnum  in
  let col   = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  (line, col)
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let ident = (alpha | '_') alnum*
let str = '"' [^ '\n' '\t' '"']* '"'
rule token = parse
  (* Skip blanks *)
  | [' ' '\t']    { token lexbuf }
  (* Newline *)
  | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
  (* Literals *)
  | '-'? ('0' | ['1'-'9']+['0'-'9']*) '.' digits as lxm { FLOAT_CONST (float_of_string lxm)}
  | '-'? ('0' | ['1'-'9']+['0'-'9']*) as lxm { INT_CONST(int_of_string lxm) }
  (* Skip comments *)
  | '#'[^ '\n']* { token lexbuf } 
  | str as lxm { STR_CONST lxm }
  (* Keywords *)
  | "and" { AND }
  | "do" { DO }
  | "od" { OD }
  | "if" { IF }
  | "fi" { FI }
  | "end" { END }
  | "else" { ELSE }
  | "not" {NOT}
  | "or" { OR }
  | "proc" { PROC }
  | "ref" { REF }
  | "then" { THEN }
  | "val" { VAL }
  | "while" { WHILE }
  | "bool" { BOOL }
  | "int" { INT }
  | "float" { FLOAT }
  | "true" { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read" { READ }
  | "write" { WRITE }
  (* Symbols *)
  | ":=" { ASSIGN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LSQBRACKET }
  | ']' { RSQBRACKET }
  | ".." { DOUBLEDOT }
  | ',' { COMMA }
  (* Operators *)
  | "!=" { NOTEQ }
  | '=' { EQ }
  | "<=" { LTEQ }
  | '<' { LT }
  | ">=" { GTEQ }
  | '>' { GT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  | ';' { SEMICOLON }
  (* Ident is here so it won't interfere with matching keywords *)
  | ident as lxm { IDENT lxm }
  | eof { EOF }
  | _ { raise (Lex_error
              ("Unknown symbol \"" ^ (Lexing.lexeme lexbuf) ^ "\"")) }
