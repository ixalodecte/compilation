%token NOMBRE PLUS MOINS FOIS GPAREN DPAREN EOL
%type <unit> main expression terme facteur
%start main
%%main:
    expression EOL                {};
expression:
    expression PLUS terme         {}
    | expression MOINS terme        {}
    | terme                         {}
    ;
terme:
    terme FOIS facteur            {}
    |facteur                       {}
    ;
facteur:
    GPAREN expression DPAREN      {}
    | MOINS facteur                 {}
    | NOMBRE                        {}
    ;
