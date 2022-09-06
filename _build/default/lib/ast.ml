type ast =
  | Biimplication of ast list
  | Implication of ast * ast
  | Or of ast list
  | And of ast list
  | Not of ast
  | All of string * ast 
  | Exists of string * ast  
  | Literal of string
  | LitVar of string * string list

let rec equal_string_list xs ys =
  match xs, ys with 
  | x::xs, y::ys -> String.equal x y && equal_string_list xs ys 
  | [], [] -> true 
  | _, _ -> false 

let rec equal t t' =
  match (t, t') with
  | Biimplication ts, Biimplication ts' -> equal_list ts ts'
  | Implication (t1, t2), Implication (t1', t2') -> equal t1 t1' && equal t2 t2'
  | Or ts, Or ts' -> equal_list ts ts'
  | And ts, And ts' -> equal_list ts ts'
  | Not t, Not t' -> equal t t'
  | All (l, t), All (l', t') -> String.equal l l' && equal t t'
  | Exists (l, t), Exists (l', t') -> String.equal l l' && equal t t'
  | Literal l, Literal l' -> String.equal l l'
  | LitVar (l, xs), LitVar (l', xs') -> String.equal l l' && equal_string_list xs xs' 
  | _ -> false

and equal_list ts ts' =
  match (ts, ts') with
  | t :: ts, t' :: ts' -> equal t t' && equal_list ts ts'
  | [], [] -> true
  | _ -> false

let rec pprint_string t =
  match t with
  | Biimplication ts -> term " ⟷ " ts
  | Implication (t1, t2) ->
      "(" ^ pprint_string t1 ^ " → " ^ pprint_string t2 ^ ")"
  | Or ts -> term " ∨ " ts
  | And ts -> term " ∧ " ts
  | Not (Literal l) -> "¬" ^ l
  | Not (LitVar(l, xs)) -> "¬" ^ l ^ "(" ^ (String.concat "," xs) ^ ")"
  | Not t -> "(¬" ^ pprint_string t ^ ")"
  | All (l, t) -> "(∀" ^ l ^ (find_quant t true) ^ ")"
  | Exists (l, t) -> "(∃" ^ l ^ (find_quant t false) ^ ")"
  | Literal t -> t
  | LitVar (l, xs) -> l ^ "(" ^ (String.concat "," xs) ^ ")"

and find_quant t b = 
  match t with 
  | All(l, t) -> (if b then "" else "∀") ^ l ^ (find_quant t true)
  | Exists(l, t) -> (if b then "∃" else "") ^ l ^ (find_quant t false) 
  | _ -> pprint_string t 

and term s ts = "(" ^ String.concat s (List.map pprint_string ts) ^ ")"

let pprint t b =
  let string = pprint_string t in
  let len = String.length string in
  if len <= 3 then print_endline string
  else print_endline ((if b then "≃" else "") ^ (String.sub string 1 (String.length string - 2)))

let pprint_clause_content t = 
  match t with 
  |Or(ts) -> String.concat ", " (List.map pprint_string ts)
  |_ -> pprint_string t

let pprint_clauses t = 
  match t with 
  |And(ts) -> print_endline (String.concat "  " (List.map (fun x -> "{" ^ x ^ "}") (List.map pprint_clause_content ts)))
  |_ -> print_endline ("{" ^ (pprint_clause_content t) ^ "}")

let rec bin_ast_to_ast t =
  match t with
  | Parser.Biimplication (t1, t2) ->
      Biimplication [ bin_ast_to_ast t1; bin_ast_to_ast t2 ]
  | Parser.Implication (t1, t2) ->
      Implication (bin_ast_to_ast t1, bin_ast_to_ast t2)
  | Parser.Or (t1, t2) -> Or [ bin_ast_to_ast t1; bin_ast_to_ast t2 ]
  | Parser.And (t1, t2) -> And [ bin_ast_to_ast t1; bin_ast_to_ast t2 ]
  | Parser.Not t -> Not (bin_ast_to_ast t)
  | Parser.All (l, t) -> All(l, bin_ast_to_ast t)
  | Parser.Exists(l, t) -> Exists(l, bin_ast_to_ast t)
  | Parser.Literal l -> Literal l
  | Parser.LitVar(l, xs) -> LitVar(l, xs)

let rec cleanup_ast t =
  match t with
  | Biimplication _ -> let xs = collect_bi t in ( 
    match xs with 
      |[x] -> x 
      |_ -> Biimplication (xs)
  )
  | Implication (t1, t2) -> Implication (cleanup_ast t1, cleanup_ast t2)
  | And _ -> let xs = collect_and t in ( 
    match xs with 
      |[x] -> x 
      |_ -> And (xs)
  )
  | Or _ -> let xs = collect_or t in ( 
    match xs with 
      |[x] -> x 
      |_ -> Or (xs)
  )
  | Not t -> Not (cleanup_ast t)
  | All (l, t) -> All(l, cleanup_ast t) 
  | Exists (l, t) -> Exists(l, cleanup_ast t)
  | Literal l -> Literal l
  | LitVar(l, xs) -> LitVar(l, xs)
   
and collect_bi t =
  match t with
  | Biimplication ts -> List.flatten (List.map collect_bi ts)
  | _ -> [ cleanup_ast t ]

and collect_and t =
  match t with
  | And ts -> List.flatten (List.map collect_and ts)
  | _ -> [ cleanup_ast t ]

and collect_or t =
  match t with
  | Or ts -> List.flatten (List.map collect_or ts)
  | _ -> [ cleanup_ast t ]

let to_full_ast t =
  let ast = cleanup_ast (bin_ast_to_ast t) in
  pprint ast false;
  ast

(*
  let rec bin_ast_to_ast t =
  match t with
  | Parser.Implication (t1, t2) ->
      Implication (bin_ast_to_ast t1, bin_ast_to_ast t2)
  | Parser.Biimplication (t1, t2) ->
      Biimplication (collect_bi t1 @ collect_bi t2)
  | Parser.And (t1, t2) -> And (collect_and t1 @ collect_and t2)
  | Parser.Or (t1, t2) -> Or (collect_or t1 @ collect_or t2)
  | Parser.Not t -> Not (bin_ast_to_ast t)
  | Parser.Literal l -> Literal l

and collect_bi t =
  match t with
  | Parser.Biimplication (t1, t2) -> collect_bi t1 @ collect_bi t2
  | _ -> [ bin_ast_to_ast t ]

and collect_and t =
  match t with
  | Parser.And (t1, t2) -> collect_and t1 @ collect_and t2
  | _ -> [ bin_ast_to_ast t ]

and collect_or t =
  match t with
  | Parser.Or (t1, t2) -> collect_or t1 @ collect_or t2
  | _ -> [ bin_ast_to_ast t ]
*)
