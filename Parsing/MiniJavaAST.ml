type expr = 
    | List_expr of expr * expr
    | Static_attr of string * string
    | Attr of string * string

type control =
    | Class of string * string * expr

type ast =
    | Controls of control * ast
    | Control of control

let rec string_of_Expr expr =
match expr  with
| List_expr (e1,e2) -> (string_of_Expr e1) ^ ";" ^ (string_of_Expr e2)
| Static_attr (typ,name) -> "static Attribut(" ^ name ^ ":" ^ typ ^")" 
| Attr (typ,name) -> "Attribut(" ^ name ^ ":" ^ typ ^")" 

let rec string_of_Control control =
match control with
| Class (name,parent, e) -> "Class(" ^ name ^ ":" ^ parent  ^ ";"^ (string_of_Expr e) ^ ")"

let rec string_of_AST ast =
match ast with
  | Controls (c,a) -> (string_of_Control c) ^ ";" ^ (string_of_AST a)
  | Control c -> string_of_Control c


