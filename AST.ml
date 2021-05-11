type expression_a =
    | Affect of string * expression_a * int
    | Plus  of expression_a * expression_a * int
    | Moins of expression_a * expression_a * int
    | Mult  of expression_a * expression_a * int
    | Div   of expression_a * expression_a * int
    | And   of expression_a * expression_a * int
    | Or    of expression_a * expression_a * int
    | Egal   of expression_a * expression_a * int
    | Sup_egal   of expression_a * expression_a * int
    | Sup   of expression_a * expression_a * int
    | Neg   of expression_a * int
    | Num   of float
    | Non of expression_a * int
    | Bool of bool
    | Var of string
    | Incr of string
    ;;

type commande_a =
    | Ifelse of expression_a * commande_a * commande_a * int
    | Dowhile of commande_a * expression_a * int
    | While of expression_a * commande_a * int
    | For of expression_a * expression_a * expression_a * commande_a * int
    | Cexpression of expression_a * int
    | Group of programme_a * int
    | Ptvirg
and
programme_a =
    | NoeudProgramme of commande_a * programme_a * int
    | Pcommande of commande_a * int
;;

let get_size_expression expression =
   match expression with
   | Affect (_,_,i) -> i
   | Plus  (_,_,i) -> i
   | Or  (_,_,i) -> i
   | And  (_,_,i) -> i
   | Moins (_,_,i) -> i
   | Mult  (_,_,i) -> i
   | Div  (_,_,i) -> i
   | Sup  (_,_,i) -> i
   | Sup_egal  (_,_,i) -> i
   | Egal  (_,_,i) -> i
   | Neg    (_,i)   -> i
   | Non    (_,i)    -> i
   | Incr   _    -> 5
   | Num    _    -> 1
   | Bool    _    -> 1
   | Var    _    -> 1;;

let rec get_size_programme programme =
   match programme with
   | NoeudProgramme (a,b,i) -> i
   | Pcommande (a,i) -> i
and
get_size_commande commande =
    match commande with
    | Ifelse (_,_,_,i)  -> i
    | Dowhile (_,_,i)   -> i
    | While (_,_,i)     -> i
    | For (_,_,_,_,i)   -> i
    | Cexpression (_,i) -> i
    | Group (_,i)       -> i
    | Ptvirg            -> 1;;

(* Fonctions d'affichage *)

let rec expression_code expression =
   match expression with
   | Affect (v,e,_) -> Printf.sprintf "%s\n%s %s\n%s %s" (expression_code e) "SetVar" v "GetVar" v
   | Plus  (g,d,_) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "AddiNb"
   | Moins (g,d,_) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "SubiNb"
   | Mult  (g,d,_) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "MultNb"
   | Div  (g,d,_) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "DiviNb"
   | Sup  (g,d,_) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "GrStNb"
   | And  (g,d,_) -> Printf.sprintf "%s\n%s %n\n%s\n%s\n%s" (expression_code g) "ConJmp" ((get_size_expression d)+1) (expression_code d) "Jump 1" "CstBo False"
   | Or  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s %n\n%s" (expression_code g) "ConJmp 2" "CstBo True" "Jump 1" (get_size_expression d) (expression_code d)
   | Sup_egal  (g,d,_) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "GrEqNb"
   | Egal  (g,d,_) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "Equals"
   | Neg    (e,_)    -> Printf.sprintf "%s\n%s" (expression_code e) "NegaNb"
   | Non    (e,_)    -> Printf.sprintf "%s\n%s" (expression_code e) "Not"
   | Incr   v    -> Printf.sprintf "%s\n%s\n%s %s" (expression_code (Var v)) (expression_code (Plus(Var v, Num 1.,1))) "SetVar" v
   | Num    n    -> Printf.sprintf "%s %f" "CsteNb" n
   | Bool    b    -> Printf.sprintf "%s %B" "CsteBo" b
   | Var    s    -> Printf.sprintf "%s %s" "GetVar" s;;

let rec commande_code commande =
    match commande with
    | Ifelse (e, t, l,_ ) -> Printf.sprintf "%s\nConJmp %n\n%s\nJump %n\n%s" (expression_code e) ((get_size_commande t)+1) (commande_code t) (get_size_commande l) (commande_code l)
    | Dowhile (c, e, _) -> Printf.sprintf "%s\n%s\n%s\n%s %i" (commande_code c) (expression_code e) "Not" "ConJmp" (-((get_size_commande c) + get_size_expression e+2))
    | While (e, c, _)   -> Printf.sprintf "%s\n%s %n\n%s\n%s %i" (expression_code e) "ConJmp" ((get_size_commande c) + 1) (commande_code c) "Jump" (-((get_size_commande c) + (get_size_expression e)+2))
    | For (e,f,g,c,_)   -> Printf.sprintf "%s\n%s\n%s %n\n%s\n%s\n%s %i" (expression_code e) (expression_code f) "ConJmp" ((get_size_commande c) + (get_size_expression g) + 1) (commande_code c) (expression_code g) "Jump" (-((get_size_commande c) + (get_size_expression f)  + (get_size_expression g) +2))
    | Cexpression (e,_) -> expression_code e
    | Group (p,_) -> programme_code p
    | Ptvirg -> "Noop"
and
programme_code programme =
   match programme with
   | NoeudProgramme (c, p,_) -> Printf.sprintf "%s \n%s" (commande_code c) (programme_code p)
   | Pcommande (c,_) -> commande_code c;;

let print_gen_code programme =

   String.concat (programme_code programme) [""; "\nHalt"]
   ;;
