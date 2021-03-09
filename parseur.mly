%token NOMBRE PLUS MOINS FOIS GPAREN DPAREN EOL
%left PLUS MOINS
%left FOIS
%nonassoc UMOINS

%type <unit> main expression
%start main
%%
main:
    expression EOL                {}
    ;
expression:
    expression PLUS expression    {}
    | expression MOINS expression   {}
    | expression FOIS expression    {}
    | expression expression     {}
    | GPAREN expression DPAREN      {}
    | MOINS expression %prec UMOINS {}
    | NOMBRE                        {}
    ;
