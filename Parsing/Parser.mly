%{
 open MiniJavaAST
%}

%token CLASS, LACCOLADE, RACCOLADE, EOF, EXTENDS
%token STATIC, SEMICOLON
%token <string> CLASSNAME
%token <string> OTHERNAME

%start fichier
%type <MiniJavaAST.expr> fichier

%%

fichier:
|file_content EOF {$1}
;

file_content:
| class_or_expr file_content {List_expr($1,$2)}
| class_or_expr {$1}

class_or_expr:
|classe {$1}
;

classe:
| CLASS CLASSNAME EXTENDS CLASSNAME LACCOLADE class_content RACCOLADE {Classe($2, $4, $6)}
| CLASS CLASSNAME LACCOLADE class_content RACCOLADE {Classe($2, "Object", $4)}
;

class_content:
| attr_or_method class_content {List_expr($1,$2)}
| attr_or_method {$1}
;

attr_or_method:
| attribut {$1}
;

attribut:
| STATIC CLASSNAME OTHERNAME SEMICOLON {Static_attr($2,$3)}
| CLASSNAME OTHERNAME SEMICOLON {Attr($1,$2)}
;

%%
