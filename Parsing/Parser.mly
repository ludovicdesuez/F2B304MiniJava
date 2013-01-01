%{
 open MiniJavaAST
%}

%token COLON, SEMICOLON, COMMA
%token LACCOLADE, RACCOLADE, LPAR, RPAR
%token EQUALS
%token STATIC, CLASS, EXTENDS
%token EOF

%token <string> CLASSNAME
%token <string> OTHERNAME

%token <int> INTEGER

%start fichier
%type <MiniJavaAST.expr> fichier

%%

fichier:
|file_content EOF {Liste_expr($1)}
;

file_content:
| class_or_expr file_content {$1::$2}
| class_or_expr {[$1]}

class_or_expr:
|classe {$1}
|expr {$1}
;

classe:
| CLASS CLASSNAME EXTENDS CLASSNAME LACCOLADE RACCOLADE {Classe($2, $4, Liste_expr([]))}
| CLASS CLASSNAME EXTENDS CLASSNAME LACCOLADE class_content RACCOLADE {Classe($2, $4, Liste_expr($6))}
| CLASS CLASSNAME LACCOLADE RACCOLADE {Classe($2, "Object", Liste_expr([]))}
| CLASS CLASSNAME LACCOLADE class_content RACCOLADE {Classe($2, "Object", Liste_expr($4))}
;

class_content:
| attr_or_method class_content {$1::$2}
| attr_or_method {[$1]}
;

attr_or_method:
| attribut {$1}
| methode {$1}
;

attribut:
| STATIC CLASSNAME OTHERNAME EQUALS expr SEMICOLON {Static_attr($2,$3,$5)}
| STATIC CLASSNAME OTHERNAME SEMICOLON {Static_attr($2,$3, NoExpr)}
| CLASSNAME OTHERNAME EQUALS expr SEMICOLON {Attr($1,$2,$4)}
| CLASSNAME OTHERNAME SEMICOLON {Attr($1,$2, NoExpr)}
;

methode:
| STATIC CLASSNAME OTHERNAME LPAR RPAR LACCOLADE expr RACCOLADE {Static_method($2,$3,[],$7)}
| STATIC CLASSNAME OTHERNAME LPAR params RPAR LACCOLADE expr RACCOLADE {Static_method($2,$3,$5, $8)}
| CLASSNAME OTHERNAME LPAR RPAR LACCOLADE expr RACCOLADE {Method($1,$2,[],$6)}
| CLASSNAME OTHERNAME LPAR params RPAR LACCOLADE expr RACCOLADE {Method($1,$2,$4,$7)}
;

params:
| CLASSNAME OTHERNAME params {Param($1,$2)::$3}
| CLASSNAME OTHERNAME {[Param($1,$2)]}
;

expr:
| LPAR expr RPAR {$2}
| INTEGER {Int($1)}
;
%%
