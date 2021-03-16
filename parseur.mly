%token <float> NOMBRE
%token PLUS MOINS FOIS DIV GPAREN DPAREN PT_VIRG
%left PLUS MOINS
%left FOIS DIV
%nonassoc UMOINS

%type <float> main expression
%start main
%%
main:
    expression PT_VIRG                { $1 }
    ;
expression:
    expression PLUS expression    { $1+.$3}
    | expression MOINS expression   { $1-.$3 }
    | expression FOIS expression    { $1*.$3 }
    | expression DIV expression    { $1/.$3 }
    | GPAREN expression DPAREN      { $2 }
    | MOINS expression %prec UMOINS { -.$2 }
    | NOMBRE                        { $1 }
    ;
