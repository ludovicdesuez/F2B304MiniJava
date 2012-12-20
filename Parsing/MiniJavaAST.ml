type expr = 
  | Liste_expr of expr list

  | Classe of string * string * expr

  | Static_attr of string * string * expr
  | Attr of string * string * expr

  | Static_method of string * string * expr list * expr
  | Method of string * string * expr list * expr

  | Param of string * string

  | Int of int

  | NoExpr

let rec getspace n=
  if n>0
  then "  " ^ (getspace (n-1))
  else ""

let rec string_of_Expr expr n =
  match expr  with
    | Liste_expr([])  -> ""
    | Liste_expr(t::q)  -> (string_of_Expr t n) ^ "\n" ^ (string_of_Expr (Liste_expr(q)) n)

    | Classe (name,parent, e) -> (getspace n) ^ "Class " ^ name ^ ":" ^ parent  ^ "\n"^ (string_of_Expr e (n+1))

    | Static_method (typ,name,params,e) -> (getspace n) ^ "static Method " ^ name ^ ":" ^ typ ^ "\n" ^ (string_of_Expr (Liste_expr(params)) (n+1)) ^ "\n" ^ (string_of_Expr e (n+1))
    | Method (typ,name,params,e) -> (getspace n) ^ "Method " ^ name ^ ":" ^ typ ^ "\n" ^ (string_of_Expr (Liste_expr(params)) (n+1)) ^ "\n" ^ (string_of_Expr e (n+1))

    | Static_attr (typ,name,e) -> (getspace n) ^ "static Attribut " ^ name ^ ":" ^ typ ^ " \n" ^ (string_of_Expr e (n+1))
    | Attr (typ,name,e) -> (getspace n) ^ "Attribut " ^ name ^ ":" ^ typ ^ " \n" ^ (string_of_Expr e (n+1))

    | Param (t,name) -> (getspace n) ^ t ^ ":" ^ name

    | Int i -> (getspace n) ^ string_of_int(i)

    | NoExpr -> ""
