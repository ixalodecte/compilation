type expression_a =
    | Plus  of expression_a * expression_a * int
    | Moins of expression_a * expression_a * int
    | Mult  of expression_a * expression_a * int
    | Div   of expression_a * expression_a * int
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
    | Affect of string * expression_a * int
    | Ifelse of expression_a * commande_a * commande_a * int
    | Cexpression of expression_a * int
;;

type programme_a =
    | NoeudProgramme of commande_a * programme_a
    | Pcommande of commande_a
;;

let get_size_expression expression =
   match expression with
   | Plus  (_,_,i) -> i
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

let get_size_commande commande =
    match commande with
    | Affect (_,_,i) -> i
    | Ifelse (_,_,_,i) -> i
    | Cexpression (_,i) -> i;;

(* Fonctions d'affichage *)

let rec expression_code expression =
   match expression with
   | Plus  (g,d,i) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "AddiNb"
   | Moins (g,d,i) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "SubiNb"
   | Mult  (g,d,i) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "MultNb"
   | Div  (g,d,i) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "DiviNb"
   | Sup  (g,d,i) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "GrStNb"
   | Sup_egal  (g,d,i) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "GrEqNb"
   | Egal  (g,d,i) -> Printf.sprintf "%s\n%s\n%s" (expression_code g) (expression_code d) "Equals"
   | Neg    (e,i)    -> Printf.sprintf "%s\n%s" (expression_code e) "NegaNb"
   | Non    (e,i)    -> Printf.sprintf "%s\n%s" (expression_code e) "Not"
   | Incr   v    -> Printf.sprintf "%s\n%s\n%s %s" (expression_code (Var v)) (expression_code (Plus(Var v, Num 1.,1))) "SetVar" v
   | Num    n    -> Printf.sprintf "%s %f" "CsteNb" n
   | Bool    b    -> Printf.sprintf "%s %B" "CsteBo" b
   | Var    s    -> Printf.sprintf "%s %s" "GetVar" s;;

let rec commande_code commande =
    match commande with
    | Affect (v,e,i) -> Printf.sprintf "%s\n%s %s" (expression_code e) "SetVar" v
    | Ifelse (e, t, l,i ) -> Printf.sprintf "%s\nConJmp %n\n%s\nJump %n\n%s" (expression_code e) ((get_size_commande t)+1) (commande_code t) (get_size_commande l) (commande_code l)
    | Cexpression (e,i) -> expression_code e;;

let rec programme_code programme =
   match programme with
   | NoeudProgramme (c, p) -> Printf.sprintf "%s \n%s" (commande_code c) (programme_code p)
   | Pcommande c -> commande_code c;;

let print_gen_code programme =

   String.concat (programme_code programme) [""; "\nHalt"]
   ;;
