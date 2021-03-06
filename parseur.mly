%{
  open AST
%}

%token <float> NOMBRE
%token <bool> BOOL
%token <string> VAR

%token BOOL EGAL SUP_EGAL SUP NON NOMBRE PLUS MOINS FOIS DIV GPAREN DPAREN PT_VIRG
VAR AFFECT END IF ELSE GACC DACC AND OR DO WHILE INCREM FOR UNDEF NAN FUNC RETURN VIRG EOF
%right AFFECT
%left OR
%left AND
%left EGAL SUP SUP_EGAL
%left PLUS MOINS
%left FOIS DIV
%nonassoc RETURN
%nonassoc UMOINS
%nonassoc NON

%type <AST.programme_a> main programme
%start main
%%
main:
    programme END                     { $1 }
    | programme EOF                   { $1 }
    ;
expression:
    VAR AFFECT expression   { Affect($1, $3, (get_size_expression $3) + 2)}
    | expression OR expression        { Or($1, $3, (get_size_expression $1) + (get_size_expression $3) + 3 + size_convert_to_bool)}
    | expression AND expression        { And($1, $3, (get_size_expression $1) + (get_size_expression $3) + 2 + size_convert_to_bool)}
    | expression EGAL expression      { Egal($1, $3, (get_size_expression $1) + (get_size_expression $3) + 1 + size_convert_to_num*2)}
    | expression SUP expression     { Sup($1, $3, (get_size_expression $1) + (get_size_expression $3) + 1 + size_convert_to_num*2) }
    | expression SUP_EGAL expression { Sup_egal($1, $3, (get_size_expression $1) + (get_size_expression $3) + 1 + size_convert_to_num*2) }
    | expression PLUS expression    { Plus($1, $3, (get_size_expression $1) + (get_size_expression $3) + 1 + size_convert_to_num*2)}
    | expression MOINS expression   { Moins($1,$3, (get_size_expression $1) + (get_size_expression $3) + 1 + size_convert_to_num*2) }
    | expression FOIS expression    { Mult($1,$3, (get_size_expression $1) + (get_size_expression $3) + 1 + size_convert_to_num*2) }
    | expression DIV expression     { Div($1, $3, (get_size_expression $1) + (get_size_expression $3) + 1 + size_convert_to_num*2) }
    | VAR GPAREN arguments DPAREN   { Call ($1, $3,(get_size_arguments $3) + 3) }
    | GPAREN expression DPAREN      { $2 }
    | MOINS expression %prec UMOINS { Neg ($2, (get_size_expression $2) + 1 + size_convert_to_num) }
    | VAR INCREM                    { Incr $1 }
    | NON expression                { Non ($2, (get_size_expression $2) + 1 + size_convert_to_bool) }
    | UNDEF                         { Undef "undefined"}
    | NAN                           { Nan "NaN" }
    | NOMBRE                        { Num $1 }
    | BOOL                          { Bool $1 }
    | VAR                           { Var $1 }
    ;
commande:
    IF GPAREN expression DPAREN commande ELSE commande { Ifelse($3, $5, $7, (get_size_expression $3)+(get_size_commande $5) + (get_size_commande $7)+2) }
    | DO commande WHILE GPAREN expression DPAREN       { Dowhile($2, $5, (get_size_commande $2) + (get_size_expression $5) + 2) }
    | WHILE GPAREN expression DPAREN commande          { While($3, $5,(get_size_commande $5) + (get_size_expression $3) + 2) }
    | FOR GPAREN expression PT_VIRG expression PT_VIRG expression DPAREN commande { For($3, $5, $7, $9, (get_size_expression $3) +(get_size_expression $5) + (get_size_expression $7) + (get_size_commande $9) + 3)}
    | expression PT_VIRG            { Cexpression ($1, (get_size_expression $1) +1) }
    | PT_VIRG                       { Ptvirg }
    | GACC programme DACC           { Group($2, (get_size_programme $2)) }
    | FUNC VAR GPAREN decl_args DPAREN GACC programme DACC   { Decl_f($2, $4, $7, (get_size_decl_args $4) + (get_size_programme $7) + 3) }
    | RETURN expression                                          { Return $2 }
    ;
programme:
    commande programme              { NoeudProgramme($1, $2, (get_size_commande $1) + (get_size_programme $2)) }
    | commande                      { Pcommande ($1, get_size_commande $1) }
    ;
decl_args:
    |                               { NoeudVideDA ()}
    | VAR                           { DeclArg $1 }
    | VAR VIRG decl_args            { NoeudDA($1,$3, (get_size_decl_args $3) + 1) }
    ;
arguments:
    |                               { NoeudVideA () }
    | expression                    { Arg $1 }
    | expression VIRG arguments     { NoeudA($1, $3, ((get_size_arguments $3) + (get_size_expression $1) + 1)) }
