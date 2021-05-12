%token BOOL EGAL SUP_EGAL SUP NON NOMBRE PLUS MOINS FOIS DIV GPAREN DPAREN PT_VIRG VAR AFFECT END IF ELSE GACC DACC AND OR DO WHILE INCREM FOR UNDEF NAN
%right AFFECT
%left OR
%left AND
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
    | programme                   {}
    ;
expression:
    VAR AFFECT expression   {}
    | expression OR expression        {}
    | expression AND expression     {}
    | expression EGAL expression    {}
    | expression SUP expression     {}
    | expression SUP_EGAL expression {}
    | expression PLUS expression    {}
    | expression MOINS expression   {}
    | expression FOIS expression    {}
    | expression DIV expression     {}
    | GPAREN expression DPAREN      {}
    | MOINS expression %prec UMOINS {}
    | NON expression                {}
    | VAR INCREM                    {}
    | NAN                           {}
    | UNDEF                         {}
    | NOMBRE                        {}
    | BOOL                          {}
    | VAR                           {}
    ;
commande:
    IF GPAREN expression DPAREN commande ELSE commande {}
    | DO commande WHILE GPAREN expression DPAREN       {}
    | WHILE GPAREN expression DPAREN commande          {}
    | FOR GPAREN expression PT_VIRG expression PT_VIRG expression DPAREN commande {}
    | expression PT_VIRG            {}
    | PT_VIRG                       {}
    | GACC programme DACC           {}
    ;
programme:
    commande programme              {}
    | commande                      {}
    ;
