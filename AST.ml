
type expression_a =
    | Plus  of expression_a * expression_a
    | Moins of expression_a * expression_a
    | Mult  of expression_a * expression_a
    | Div   of expression_a * expression_a
    | Neg   of expression_a
    | Num   of int
;;


(* Fonctions d'affichage *)

let rec print_binaire form s g d = Format.fprintf form "@[<2>%s%s@ %a%s@ %a%s@]" s "(" print_AST g " ," print_AST d " )"

and print_AST form = let open Format in function
    | Plus  (g,d) -> print_binaire form "Plus" g d
    | Moins (g,d) -> print_binaire form "Moins" g d
    | Mult  (g,d) -> print_binaire form "Mult" g d
    | Div   (g,d) -> print_binaire form "Div" g d
    | Neg    e    -> fprintf form "@[<2>%s@ %a@]" "Neg" print_AST e
    | Num    n    -> fprintf form "@[<2>%s@ %i@]" "Num" n
;;


let print_gen_code expression =
  let rec print_AST_code expression =
      match expression with
      | Plus  (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "AddiNb"
      | Moins (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "SubiNb"
      | Mult  (g,d) -> Printf.sprintf "%s\n%s\n%s" (print_AST_code g) (print_AST_code d) "MultNb"
      | Neg    e    -> Printf.sprintf "%s\n%s" (print_AST_code e) "NegaNb"
      | Num    n    -> Printf.sprintf "%s %i" "CsteNb" n
    in String.concat (print_AST_code expression) [""; "\nPrint\nHalt"]
    ;;
