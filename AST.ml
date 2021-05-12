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
    | Call of string * arguments_a * int
    | Neg   of expression_a * int
    | Num   of float
    | Non of expression_a * int
    | Bool of bool
    | Var of string
    | Undef of string
    | Nan of string
    | Incr of string
and
arguments_a =
    | NoeudVideA of unit
    | Arg of expression_a
    | NoeudA of expression_a * arguments_a * int

type decl_args_a =
    | NoeudVideDA of unit
    | DeclArg of string
    | NoeudDA of string * decl_args_a * int;;

type commande_a =
    | Ifelse of expression_a * commande_a * commande_a * int
    | Dowhile of commande_a * expression_a * int
    | While of expression_a * commande_a * int
    | For of expression_a * expression_a * expression_a * commande_a * int
    | Cexpression of expression_a * int
    | Group of programme_a * int
    | Ptvirg
    | Decl_f of string * decl_args_a * programme_a * int
    | Return of expression_a
and
programme_a =
    | NoeudProgramme of commande_a * programme_a * int
    | Pcommande of commande_a * int
;;

let convert_to_num = String.concat "\n" ["TypeOf";
                                        "Case 2";
                                        "BoToNumber";
                                        "Jump 6";
                                        "Noop";
                                        "Jump 4";
                                        "Noop";
                                        "Jump 2";
                                        "Drop";
                                        "CsteNb NaN"];;
let size_convert_to_num = 10;;

let convert_to_bool = String.concat "\n" ["TypeOf";
                                        "Case 2";
                                        "Noop";
                                        "Jump 6";
                                        "NbToBe";
                                        "Jump 4";
                                        "Noop";
                                        "Jump 2";
                                        "Drop";
                                        "CsteNb NaN"];;
let size_convert_to_bool = 10;;


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
   | Nan _       -> 1
   | Incr   _    -> 5 + size_convert_to_num*3
   | Num    _    -> 1
   | Bool    _    -> 1
   | Undef _      -> 1
   | Var    _    -> 1
   | Call(_,_,i) -> i;;

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
    | Ptvirg            -> 1
    | Decl_f (_,_,_,i)  -> i
    | Return e          -> (get_size_expression e) + 1
    ;;

let get_size_arguments arg =
    match arg with
    | NoeudVideA _   -> 0
    | Arg e          -> (get_size_expression e) +1
    | NoeudA (_,_,i) -> i
    ;;

let get_size_decl_args decl_args =
    match decl_args with
    | NoeudVideDA _   -> 0
    | DeclArg _       -> 1
    | NoeudDA (_,_,i) -> i;;

(* Fonctions d'affichage *)

let rec expression_code expression =
   match expression with
   | Affect (v,e,_) -> Printf.sprintf "%s\n%s %s\n%s %s" (expression_code e) "SetVar" v "GetVar" v
   | Plus  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s\n%s" (expression_code g) convert_to_num (expression_code d) convert_to_num "AddiNb"
   | Moins (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s\n%s" (expression_code g) convert_to_num (expression_code d) convert_to_num "SubiNb"
   | Mult  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s\n%s" (expression_code g) convert_to_num (expression_code d) convert_to_num "MultNb"
   | Div  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s\n%s" (expression_code g) convert_to_num (expression_code d) convert_to_num "DiviNb"
   | Sup  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s\n%s" (expression_code g) convert_to_num (expression_code d) convert_to_num "GrStNb"
   | And  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s %n\n%s" (expression_code g)
                                                            "Copy"
                                                            convert_to_bool
                                                            "ConJmp" (get_size_expression d)
                                                            (expression_code d)
   | Or  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s\n%s %n\n%s" (expression_code g)
                                                            "Copy"
                                                            convert_to_bool
                                                            "ConJmp 1"
                                                            "Jump" (get_size_expression d)
                                                            (expression_code d)
   | Sup_egal  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s\n%s" (expression_code g) convert_to_num (expression_code d) convert_to_num "GrEqNb"
   | Egal  (g,d,_) -> Printf.sprintf "%s\n%s\n%s\n%s\n%s" (expression_code g) convert_to_num (expression_code d) convert_to_num "Equals"
   | Neg    (e,_)    -> Printf.sprintf "%s\n%s\n%s" (expression_code e) convert_to_num "NegaNb"
   | Non    (e,_)    -> Printf.sprintf "%s\n%s\n%s" (expression_code e) convert_to_bool "Not"
   | Incr   v    -> Printf.sprintf "%s\n%s\n%s\n%s %s" (expression_code (Var v)) convert_to_num (expression_code (Plus(Var v, Num 1.,1))) "SetVar" v
   | Undef u     -> "CsteUn"
   | Nan u     -> "CsteNb NaN"
   | Num    n    -> Printf.sprintf "%s %f" "CsteNb" n
   | Bool    b    -> Printf.sprintf "%s %B" "CsteBo" b
   | Var    s    -> Printf.sprintf "%s %s" "GetVar" s
   | Call (v,a,_)   -> Printf.sprintf "%s %s\n%s\n%s\n%s" "GetVar" v "SrtCal" (args_code a) "Call"
and
args_code args =
   match args with
   | NoeudVideA _ -> ""
   | Arg e     -> Printf.sprintf "\n%s\n%s\n" (expression_code e) "SetArg"
   | NoeudA (e,a,_) -> Printf.sprintf "\n%s\n%s%s\n" (expression_code e) "SetArg" (args_code a);;

let rec decl_args_code decl_args =
    match decl_args with
    | NoeudVideDA _ -> ""
    | DeclArg v     -> Printf.sprintf "\n%s %s\n" "DecArg" v
    | NoeudDA (v,a,_) -> Printf.sprintf "\n%s%s %s\n" (decl_args_code a) "DecArg" v;;

let rec commande_code commande =
    match commande with
    | Ifelse (e, t, l,_ ) -> Printf.sprintf "%s\nConJmp %n\n%s\nJump %n\n%s" (expression_code e) ((get_size_commande t)+1) (commande_code t) (get_size_commande l) (commande_code l)
    | Dowhile (c, e, _) -> Printf.sprintf "%s\n%s\n%s\n%s %i" (commande_code c) (expression_code e) "Not" "ConJmp" (-((get_size_commande c) + get_size_expression e+2))
    | While (e, c, _)   -> Printf.sprintf "%s\n%s %n\n%s\n%s %i" (expression_code e) "ConJmp" ((get_size_commande c) + 1) (commande_code c) "Jump" (-((get_size_commande c) + (get_size_expression e)+2))
    | For (e,f,g,c,_)   -> Printf.sprintf "%s\n%s\n%s\n%s %n\n%s\n%s\n%s %i" (expression_code e) "Drop" (expression_code f) "ConJmp" ((get_size_commande c) + (get_size_expression g) + 1) (commande_code c) (expression_code g) "Jump" (-((get_size_commande c) + (get_size_expression f)  + (get_size_expression g) +3))
    | Cexpression (e,_) -> Printf.sprintf "%s\n%s" (expression_code e) "Drop"
    | Group (p,_) -> programme_code p
    | Ptvirg -> "Noop"
    | Decl_f (v,d,p,_)  -> Printf.sprintf "%s %n\n%s%s %s\n%s %n\n%s" "NewClo" (get_size_decl_args d+2) (decl_args_code d) "SetVar" v "Jump" (get_size_programme p) (programme_code p)
    | Return e          -> Printf.sprintf "%s\n%s" (expression_code e) "Return"
and
programme_code programme =
   match programme with
   | NoeudProgramme (c, p,_) -> Printf.sprintf "%s \n%s" (commande_code c) (programme_code p)
   | Pcommande (c,_) -> commande_code c;;

let print_gen_code programme =

   String.concat (programme_code programme) [""; "\nHalt"]
   ;;
