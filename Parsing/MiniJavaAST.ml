type expr = 
    | Liste_expr of expr list
    | Static_attr of string * string
    | Attr of string * string
    | Classe of string * string * expr

let rec string_of_Expr expr =
match expr  with
| Liste_expr([])  -> ""
| Liste_expr(t::q)  -> (string_of_Expr t) ^ "\n" ^ (string_of_Expr (Liste_expr(q)))
| Static_attr (typ,name) -> "static Attribut(" ^ name ^ ":" ^ typ ^")" 
| Attr (typ,name) -> "Attribut(" ^ name ^ ":" ^ typ ^")" 
| Classe (name,parent, e) -> "Class(" ^ name ^ ":" ^ parent  ^ "\n"^ (string_of_Expr e) ^ ")"



