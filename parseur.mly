%token BOOL EGAL SUP_EGAL SUP NON NOMBRE PLUS MOINS FOIS DIV GPAREN DPAREN PT_VIRG VAR AFFECT END
%left EGAL SUP SUP_EGAL
%left PLUS MOINS
%left FOIS DIV
%nonassoc UMOINS
%nonassoc NON

%type <unit> main expression
%start main
%%
main:
    programme END                {}
    | programme                    {}
    ;
expression:
    expression EGAL expression      {}
    | expression SUP expression     {}
    | expression SUP_EGAL expression {}
    | expression PLUS expression    {}
    | expression MOINS expression   {}
    | expression FOIS expression    {}
    | expression DIV expression     {}
    | GPAREN expression DPAREN      {}
    | MOINS expression %prec UMOINS {}
    | NON expression                {}
    | NOMBRE                        {}
    | BOOL                          {}
    | VAR                           {}
    ;
commande:
    VAR AFFECT expression PT_VIRG   {}
    | expression PT_VIRG            {}
    ;
programme:
    commande programme              {}
    | commande                      {}
    ;
