
type expression_a =
    | Plus  of expression_a * expression_a
    | Moins of expression_a * expression_a
    | Mult  of expression_a * expression_a
    | Div   of expression_a * expression_a
    | Egal   of expression_a * expression_a
    | Sup_egal   of expression_a * expression_a
    | Sup   of expression_a * expression_a
    | Neg   of expression_a
    | Num   of float
    | Non of expression_a
    | Bool of bool
;;


(* Fonctions d'affichage *)

let rec print_binaire form s g d = Format.fprintf form "@[<2>%s%s@ %a%s@ %a%s@]" s "(" print_AST g " ," print_AST d " )"

and print_AST form = let open Format in function
    | Plus  (g,d) -> print_binaire form "Plus" g d
    | Moins (g,d) -> print_binaire form "Moins" g d
    | Mult  (g,d) -> print_binaire form "Mult" g d
    | Div   (g,d) -> print_binaire form "Div" g d
    | Sup   (g,d) -> print_binaire form "Sup" g d
    | Egal   (g,d) -> print_binaire form "Egal" g d
    | Sup_egal   (g,d) -> print_binaire form "Sup_egal" g d
    | Non    e    -> fprintf form "@[<2>%s@ %a@]" "Non" print_AST e
    | Neg    e    -> fprintf form "@[<2>%s@ %a@]" "Neg" print_AST e
    | Num    n    -> fprintf form "@[<2>%s@ %f@]" "Num" n
    | Bool   b    -> fprintf form "@[<2>%s@ %B@]" "Bool" b
;;
