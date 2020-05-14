(* Abstract Syntax Tree and functions for printing it *)
(*functions for printing AST have been modified to be a YUL transpiler by Gabriel Brown *)

type op = Add | Sub | Mul | Div | Equal | Neq | Less | And | Or | Greater | LessQ | GreaterQ

type typ = Int | Bool

type expr =
    Literal of int
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  (* function call *)
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt
  | For of expr * expr * expr * stmt
  (* return *)
  | Return of expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Equal -> "eq"
  | Neq -> "neq"
  | Less -> "lt"
  | And -> "and"
  | Or -> "or"
  | Greater -> "gt"
  | LessQ -> "ltq"
  | GreaterQ -> "gtq"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_op o ^ "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Assign(v, e) -> v ^ " := " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ "\n"
  | Return(expr) -> "result := " ^ string_of_expr expr ^ "\n"
  | If(e, s1) ->  "if " ^ string_of_expr e ^ "\n" ^
                      string_of_stmt s1
  | For(s1, s2, e, s3) -> "for { " ^ string_of_expr s1 ^ "} " ^ string_of_expr s2
  ^ " { " ^ string_of_expr e ^ " } " ^ string_of_stmt s3

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"

let string_of_vdecl (t, id) = "let " ^ id ^ "\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
