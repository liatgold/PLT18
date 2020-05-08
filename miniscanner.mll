(* Ocaml scanner for minic *)
(* Aaron Pickard *)
(* This scanner logic and design was strongly influenced by the scanners
   in HW 2 - the ones the group members developed, as well as the solutions *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | '=' { ASSIGN }
  | ';' { SEMI }
  | '[' { LBRACE }
  | ']' { RBRACE }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | '!' { NOT }
  | '<' { LT }
  | '>' { GT }
  | ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
  | ['a'-'z']+ as id { VARIABLE(id) }
  | eof { EOF }


rule lex_float = parse
    start_dot | start_dig as f  { f }
{ 
    let buf = Lexing.from_channel stdin in
    let f = lex_float buf in
    print_endline f
}
