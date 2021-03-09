%token NOMBRE PLUS MOINS FOIS GPAREN DPAREN PT_VIRG
%left PLUS MOINS
%left FOIS
%nonassoc UMOINS

%type <unit> main expression
%start main
%%
main:
    expression PT_VIRG                {}
    ;
expression:
    expression PLUS expression    {}
    | expression MOINS expression   {}
    | expression FOIS expression    {}
    | GPAREN expression DPAREN      {}
    | MOINS expression %prec UMOINS {}
    | NOMBRE                        {}
    ;
