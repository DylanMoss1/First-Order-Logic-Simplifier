exception ParsingError

type bin_ast =
  | Biimplication of bin_ast * bin_ast
  | Implication of bin_ast * bin_ast
  | Or of bin_ast * bin_ast
  | And of bin_ast * bin_ast
  | Not of bin_ast
  | All of string * bin_ast
  | Exists of string * bin_ast
  | Literal of string
  | LitVar of string * string list

let lookahead xs = match xs with x :: xs -> (x, xs) | [] -> raise ParsingError

let parse_left parse_X xs =
  let y, xs' = parse_X xs in
  let x, xs'' = lookahead xs' in
  (y, x, xs'')

let parse_right parse_X xs =
  let y, xs' = parse_X xs in
  (y, xs')

(*
    y = left AST 
    x = current token
    xs = remaining tokens
    
    y' = right AST 
    xs' = remaining tokens after right AST has been parsed
*)

let rec parser xs = parse_S xs

and parse_S xs =
  let y, xs = parse_B xs in
  if xs = [ Lexer.End ] then y else raise ParsingError

and parse_B xs =
  let y, x, xs = parse_left parse_I xs in
  match x with
  | Lexer.Biimplication ->
      let y', xs' = parse_right parse_B xs in
      (Biimplication (y, y'), xs')
  | _ -> (y, x :: xs)

and parse_I xs =
  let y, x, xs = parse_left parse_O xs in
  match x with
  | Lexer.Implication ->
      let y', xs' = parse_right parse_I xs in
      (Implication (y, y'), xs')
  | _ -> (y, x :: xs)

and parse_O xs =
  let y, x, xs = parse_left parse_A xs in
  match x with
  | Lexer.Or ->
      let y', xs' = parse_right parse_O xs in
      (Or (y, y'), xs')
  | _ -> (y, x :: xs)

and parse_A xs =
  let y, x, xs = parse_left parse_N xs in
  match x with
  | Lexer.And ->
      let y', xs' = parse_right parse_A xs in
      (And (y, y'), xs')
  | _ -> (y, x :: xs)

and parse_N xs =
  let x, xs = lookahead xs in
  match x with
  | Lexer.Not ->
      let y', xs' = parse_right parse_N xs in
      (Not y', xs')
  | _ ->
      let y', xs' = parse_right parse_Q (x :: xs) in
      (y', xs')

and parse_Q xs =
  let x, xs = lookahead xs in
  match x with
  | Lexer.All l ->
      let y', xs' = parse_right parse_Q xs in
      (All (l, y'), xs')
  | Lexer.Exists l ->
      let y', xs' = parse_right parse_Q xs in
      (Exists (l, y'), xs')
  | _ ->
      let y', xs' = parse_right parse_F (x :: xs) in
      (y', xs')

and parse_F xs =
  let x, xs = lookahead xs in
  match x with
  | Lexer.Open_par -> (
      let y', xs' = parse_right parse_B xs in
      match xs' with
      | Lexer.Close_par :: xs' -> (y', xs')
      | _ -> raise ParsingError)
  | Lexer.Literal l -> (Literal l, xs)
  | Lexer.LitVar (l, ys) -> (LitVar (l, ys), xs)
  | _ -> raise ParsingError
