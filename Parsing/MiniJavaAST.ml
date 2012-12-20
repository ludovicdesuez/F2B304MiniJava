type expr = 
    | List_expr of expr * expr
    | Static_attr of string * string
    | Attr of string * string
    | Classe of string * string * expr
    | NoExpression

let rec string_of_Expr expr =
match expr  with
| List_expr (e1,e2) -> (string_of_Expr e1) ^ ";" ^ (string_of_Expr e2)
| Static_attr (typ,name) -> "static Attribut(" ^ name ^ ":" ^ typ ^")" 
| Attr (typ,name) -> "Attribut(" ^ name ^ ":" ^ typ ^")" 
| Classe (name,parent, e) -> "Class(" ^ name ^ ":" ^ parent  ^ ";"^ (string_of_Expr e) ^ ")"



