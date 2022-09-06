open Cnf_converter

let convert_to_cnf x =
  Cnf.convert (Ast.to_full_ast (Parser.parser (Lexer.lexer x)))

let convert_to_nnf x = convert_to_cnf ("~(" ^ x ^ ")")

let rec map_list f xs =
  match xs with
  | x :: xs ->
      (print_endline "";
       f x)
      :: map_list f xs
  | [] -> []

let with_clauses x =
  Ast.pprint_clauses x;
  x

(*
CHECK CNF OR NNF!!!
*)

let x = convert_to_nnf ("(∀x P(x)) ∧ (∀x Q(x)) ∨ ∃x(P(x) → ¬Q(x))")

(* let x = map_list (fun x -> let y = convert_to_cnf x in with_clauses y) (["P → Q ∧ R"; "P ∧ Q → S"; "¬R ↔ S"; "P ∨ Q"]) *) 
(* let x = with_clauses (convert_to_cnf ("P | Q -> Q | R")) *)
(* let x = map_list (convert_to_cnf) (["P | Q -> Q | R"; "P & R | Q"]) *)