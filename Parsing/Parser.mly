%{
 open MiniJavaAST
%}

%token EXCLAMATION, COLON, SEMICOLON, COMMA, PERIOD
%token PLUS, MINUS, MULTIPLY, DIVIDE, MODULO
%token AND, OR
%token LACCOLADE, RACCOLADE, LPAR, RPAR
%token EQUALS
%token COMP_SUP, COMP_INF, COMP_SUPEQ, COMP_INFEQ, COMP_EQ, COMP_DIFF
%token STATIC, CLASS, EXTENDS
%token IF, ELSE, IN
%token THIS, NULL
%token INSTANCEOF, NEW
%token EOF

%token <string> CLASSNAME
%token <string> OTHERNAME

%token <int> INTEGER
%token <bool> BOOLEAN

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
| unop_expr {$1}
| binop_expr {$1}

| LPAR expr RPAR {$2}

| NULL {Null}
| THIS {This}

| OTHERNAME {Var($1)}

| INTEGER {Int($1)}
| BOOLEAN {Bool($1)}
;

unop_expr:
| MINUS expr {Negat($2)}
| EXCLAMATION expr {Not($2)}
;

binop_expr:
| expr COMP_SUPEQ expr {CompSupEq($1,$3)}
| expr COMP_SUP expr {CompSup($1,$3)}
| expr COMP_INFEQ expr {CompInfEq($1,$3)}
| expr COMP_INF expr {CompInf($1,$3)}
| expr COMP_EQ expr {CompEq($1,$3)}
| expr COMP_DIFF expr {CompDiff($1,$3)}
| expr PLUS expr {Addition($1,$3)}
| expr MINUS expr {Substraction($1,$3)}
| expr MULTIPLY expr {Multiplication($1,$3)}
| expr DIVIDE expr {Division($1,$3)}
| expr MODULO expr {Modulo($1,$3)}
;

%%
