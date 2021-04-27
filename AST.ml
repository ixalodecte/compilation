
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

let print_gen_code expression =
  let rec print_AST_code expression =
      match expression with
      | Plus  (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "AddiNb"
      | Moins (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "SubiNb"
      | Mult  (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "MultNb"
      | Div  (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "DiviNb"
      | Sup  (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "GrStNb"
      | Sup_egal  (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "GrEqNb"
      | Egal  (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "Equals"
      | Neg    e    -> Printf.sprintf "%s\n%s" (print_AST_code e) "NegaNb"
      | Non    e    -> Printf.sprintf "%s\n%s" (print_AST_code e) "Not"
      | Num    n    -> Printf.sprintf "%s %f" "CsteNb" n
      | Bool    b    -> Printf.sprintf "%s %B" "CsteBo" b

    in String.concat (print_AST_code expression) [""; "\nHalt"]
    ;;
