type expr = 
  | Liste_expr of expr list

  | Classe of string * string * expr

  | Static_attr of string * string * expr
  | Attr of string * string * expr

  | Static_method of string * string * expr list * expr
  | Method of string * string * expr list * expr

  | Negat of expr
  | Not of expr

  | CompSupEq of expr * expr
  | CompSup of expr * expr
  | CompInfEq of expr * expr
  | CompInf of expr * expr
  | CompEq of expr * expr
  | CompDiff of expr * expr

  | Addition of expr * expr
  | Substraction of expr * expr
  | Multiplication of expr * expr
  | Division of expr * expr
  | Modulo of expr * expr

  | Sequence of expr * expr

  | And of expr * expr
  | Or of expr * expr

  | DeclareAssign of string * string * expr * expr
  | Assign of string * expr
  | MemberAssign of expr * string * expr
  | StaticAssign of string * string * expr

  | New of string
  | InstanceOf of expr * string

  | If of expr * expr * expr

  | Call of expr * string * expr list
  | StaticCall of string * string * expr list

  | Param of string * string

  | Var of string
  | MemberVar of expr * string
  | StaticVar of string * string

  | Cast of string * expr

  | Int of int
  | Bool of bool
  | String of string

  | Null
  | This

  | NoExpr


let rec getspace n=
  if n>0
  then "  " ^ (getspace (n-1))
  else ""

let rec string_of_list_expr l =
  match l with
    | [] -> ""
    | [a] -> string_of_Expr a 0
    | t::q -> (string_of_Expr t 0) ^ (string_of_list_expr q)

and

string_of_Expr expr n =
  match expr  with
    | Liste_expr([])  -> ""
    | Liste_expr(t::q)  -> (string_of_Expr t n) ^ "\n" ^ (string_of_Expr (Liste_expr(q)) n)

    | Classe (name,parent, e) -> (getspace n) ^ "Class " ^ name ^ ":" ^ parent  ^ "\n"^ (string_of_Expr e (n+1))

    | Static_method (typ,name,params,e) -> (getspace n) ^ "static Method " ^ name ^ ":" ^ typ ^ " ( " ^ (string_of_list_expr params) ^ ")\n" ^ (string_of_Expr e (n+1))
    | Method (typ,name,params,e) -> (getspace n) ^ "Method " ^ name ^ ":" ^ typ ^ " ( " ^ (string_of_list_expr params) ^ " )\n" ^ (string_of_Expr e (n+1))

    | Static_attr (typ,name,e) -> (getspace n) ^ "static Attribut " ^ name ^ ":" ^ typ ^ " \n" ^ (string_of_Expr e (n+1))
    | Attr (typ,name,e) -> (getspace n) ^ "Attribut " ^ name ^ ":" ^ typ ^ " \n" ^ (string_of_Expr e (n+1))

    | Negat e -> (getspace n) ^ "-(" ^ (string_of_Expr(e) 0) ^ ")"
    | Not e -> (getspace n) ^ "!(" ^ (string_of_Expr(e) 0) ^ ")"

    | CompSupEq (e1,e2) -> (getspace n) ^ "(" ^ (string_of_Expr(e1) 0) ^ ") >= (" ^ (string_of_Expr(e2) 0) ^ ")"
    | CompSup (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") > (" ^ (string_of_Expr(e2) 0) ^ ")"
    | CompInfEq (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") <= (" ^ (string_of_Expr(e2) 0) ^ ")"
    | CompInf (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") < (" ^ (string_of_Expr(e2) 0) ^ ")"
    | CompEq (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") == (" ^ (string_of_Expr(e2) 0) ^ ")"
    | CompDiff (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") != (" ^ (string_of_Expr(e2) 0) ^ ")"

    | Addition (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") + (" ^ (string_of_Expr(e2) 0) ^ ")"
    | Substraction (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") - (" ^ (string_of_Expr(e2) 0) ^ ")"
    | Multiplication (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") * (" ^ (string_of_Expr(e2) 0) ^ ")"
    | Division (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") / (" ^ (string_of_Expr(e2) 0) ^ ")"
    | Modulo (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") % (" ^ (string_of_Expr(e2) 0) ^ ")"

    | And (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") && (" ^ (string_of_Expr(e2) 0) ^ ")"
    | Or (e1,e2) -> (getspace n) ^  "(" ^ (string_of_Expr(e1) 0) ^ ") || (" ^ (string_of_Expr(e2) 0) ^ ")"

    | Sequence (e1,e2) -> (string_of_Expr(e1) n) ^ ";\n" ^ (string_of_Expr(e2) n)

    | DeclareAssign (vartype, varname, value, block) -> (getspace n) ^ varname ^":" ^ vartype ^ " = (" ^ (string_of_Expr value 0) ^ ")" ^ " in\n" ^  (string_of_Expr block (n+1))
    | Assign (var,e) -> (getspace n) ^ var ^ " = (" ^ (string_of_Expr(e) 0) ^ ")"
    | MemberAssign (obj,var,e) -> (getspace n) ^ "(" ^ (string_of_Expr(obj) 0) ^ ")."^ var ^ " = (" ^ (string_of_Expr(e) 0) ^ ")"
    | StaticAssign (c, var,e) -> (getspace n) ^ c ^ "." ^ var ^ " = (" ^ (string_of_Expr(e) 0) ^ ")"

    | New s -> (getspace n) ^ "new " ^ s
    | InstanceOf (expr, classname) -> (getspace n) ^ "(" ^ (string_of_Expr expr 0) ^ ")" ^ " is? " ^ classname

    | If (condition, thenExp, NoExpr) -> (getspace n) ^ "if (" ^ (string_of_Expr condition 0) ^ ")\n" ^ (string_of_Expr thenExp (n+1))
    | If (condition, thenExp, elseExp) -> (getspace n) ^ "if (" ^ (string_of_Expr condition 0) ^ ")\n" ^ (string_of_Expr thenExp (n+1)) ^ "\n" ^ (getspace n) ^ "else\n" ^ (string_of_Expr elseExp (n+1))

    | Param (t,name) -> (getspace n) ^ name ^ ":" ^ t

    | Call (callee, methode, args) -> (getspace n) ^ (string_of_Expr callee 0) ^ "." ^ methode ^ "(" ^ (string_of_list_expr args) ^ ")"
    | StaticCall (callee, methode, args) -> (getspace n) ^ callee ^ "." ^ methode ^ "(" ^ (string_of_list_expr args) ^ ")"

    | Var s -> (getspace n) ^ s
    | MemberVar (obj,var) -> (getspace n) ^ "(" ^ (string_of_Expr obj 0) ^")." ^ var
    | StaticVar (classe,var) -> (getspace n) ^ classe ^ "." ^ var

    | Cast (typename, expr) -> (getspace n) ^ "(" ^ typename ^ "<-" ^ (string_of_Expr expr 0) ^ ")"
      
    | Int i -> (getspace n) ^ string_of_int(i)
    | Bool b -> (getspace n) ^ string_of_bool(b)
    | String s -> (getspace n) ^ "\"" ^ s ^ "\""

    | Null -> (getspace n) ^ "null"
    | This -> (getspace n) ^ "this"

    | NoExpr -> ""
