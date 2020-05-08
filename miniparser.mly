(* Ocaml parser for minic - second attempt *)
(* Aaron Pickard *)
(* The design of this parser was strongly influenced by Prof Gu's microcparser file
 * and the DeepSEA project's parser *)
 (* The design of this language was also influenced by the DeepSEA project's language
  definition, in the backend/phase/MiniC/Language.v file *)
%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LCURLY RCURLY PLUS MINUS ASSIGN
%token <int> LITERAL
%token <int> 
%token EOF

%start program
%type <Ast.program> program
%right ASSIGN

%%

/* function declarations */

program:
        decls EOF {$1}

/* declarations */
decls:
        { ([], []) /* nothing */
        | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
        | fdecls { (fst $2, ($1 :: snd $2)) }

/* variable declaration */
vdecl:
        typ ID { ($1, $2) }

typ:
        INT {Int}
        INT256 {Int256}
        COQ_TYPE {coq_type}
        ident {IDENT}

fdecl:
        vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
        {
                {
                        typ = fst $1;
                        name = snd $1;
                        formals = $3
                        locals = $6
                        body = $7
                }
        }

formals_opt:
        { [] } /* no formals */
        | formals_list { $1}

formals_list:
        vdecl {[$1]}
        | vdecl COMMA formals_list{$1::$3}

stmt:
        expr SEMI                               { Expr $1 }
        | LBRACE stmt_list RBRACE               { Block $2 }
        | IF LPAREN expr RPAREN stmt ELSE stmt  { If($3, $5, $7) }
        | WHILE LPAREN expr RPAREN stmt         { While($3, $5) }
        | RETURN expr SEMI                      { Return $2 }
        | SKIP
        | ASSIGN                               { expr $1, expr $2 }
        | SET                                  { ident $1, expr $2 }
        | CALL                                 { ident option $1, label $2, expr list $3 }
        | SEQUENCE                             { statement $1, statement$2 }
        | BREAK
        | RETURN                               { expr option $1 }
        | TRANSFER                             { expr $1, expr $2 }
        | CALLMETHOD                           { expr $1, ident list $2, Int.int $3, expr $4, expr list $5 }
        | LOG                                  { expr $1 }
        | REVERT

expr:
        LITERAL                                 { Literal($1)    }
        | BLIT                                  { BoolLit($1)   }
        | expr CONST_INT                        expr { Int.int($1), coq_type($2) }
        | expr CONST_INT256                     expr { Int256.int($1), coq_type($2) }
        | expr VAR                              expr { ident($1), coq_type($2) }
        | expr TEMPVAR                          expr { ident($1), coq_type($2) }
        | expr DEREF                            expr { expr($1) coq_type($2) }
        | expr UNOP                             expr { unary_operation($1), expr($2) coq_type($3) }
        | expr BINOP                            expr { Binop(($1), expr($2), expr($3)), coq_type($4) }
        | expr FIELD                            expr { expr($1), ident($2), coq_type($3) }
        | expr ARRAYDEREF                       expr { expr($1), expr($2), coq_typ($3) }e
        | expr HASHDEREF                        expr { expr($1), expr($2), coq_type($3) }
        | expr CALL0                            expr { builtin0($1), coq_type($2) }
        | expr CALL1                            expr { builtin1($1), expr($2) coq_type($3) }
        | ID                                    { Id($1)                 }
        | expr PLUS                             expr { Binop($1, Add,   $3)   }
        | expr MINUS                            expr { Binop($1, Sub,   $3)   }
        | expr EQ                               expr { Binop($1, Equal, $3)   }
        | expr NEQ                              expr { Binop($1, Neq, $3)     }
        | expr LT                               expr { Binop($1, Less,  $3)   }
        | expr AND                              expr { Binop($1, And,   $3)   }
        | expr OR                               expr { Binop($1, Or,    $3)   }
        | ID ASSIGN                             expr { Assign($1, $3)         }
        | LPAREN expr RPAREN { $2                   }
        | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

args_opt:
        { [] }
        | args { $1 }

args:
        expr { [$1] }
        | expr COMMA arsgs { $1::$3 }
