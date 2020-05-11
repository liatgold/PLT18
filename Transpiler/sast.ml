(* Semantically-checked Abstract Syntax Tree and functions for printing it *)
(*functions for printing AST have been modified to be a YUL transpiler by Gabriel Brown *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list

(*
int main(int a){
  return a+1;
}

function main(a) -> result
{
  result := add(a,1)
}
*)


(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  match e with
        SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_op o ^ "(" ^ string_of_sexpr e1 ^ ", " ^ string_of_sexpr e2 ^ ")"
      | SAssign(v, e) -> v ^ " := " ^ string_of_sexpr e
      | SCall(f, el) ->

          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ "\n"
  | SReturn(expr) -> "result := " ^ string_of_sexpr expr ^ "\n"
  | SIf(e, s1) ->  "if " ^ string_of_sexpr e ^ "\n" ^
                       string_of_sstmt s1
  | SFor(e1,e2,e3,s) -> "for { " ^ string_of_sexpr e1 ^ "} " ^ string_of_sexpr e2
  ^ " { " ^ string_of_sexpr e3 ^ " } " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  "function " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ") -> result\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nTranslation to YUL: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
