%{

%}

%token DQUOTE
%token NEWLINE
%token E_BACKSLASH, E_DQUOTE

%token <string> STRING_CONTENT

%start miniJavaString
%type <string> miniJavaString

%%

miniJavaString:
| DQUOTE DQUOTE {""}
| DQUOTE content DQUOTE {$2}
;

content:
| atomic_content {$1}
| atomic_content content {$1 ^ $2}

atomic_content:
| STRING_CONTENT {$1}
| NEWLINE {"\n"}
| E_BACKSLASH {"\\"}
| E_DQUOTE {"\""}

%%
