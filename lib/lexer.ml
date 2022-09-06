exception LexingError of string
exception GetVarsError

type token =
  | Open_par
  | Close_par
  | Biimplication
  | Implication
  | Or
  | And
  | Not
  | All of string 
  | Exists of string
  | LitVar of string * string list 
  | Literal of string
  | End

let rec get_vars xs =
  match xs with 
  |","::xs -> get_vars xs
  |")"::xs -> ([], xs)  
  |x::xs -> let (ys, zs) = get_vars xs in (x :: ys, zs)
  |[] -> raise GetVarsError

let rec lexer_list xs =
  match xs with
  | "(" :: xs -> Open_par :: lexer_list xs
  | ")" :: xs -> Close_par :: lexer_list xs
  | "[" :: xs -> Open_par :: lexer_list xs
  | "]" :: xs -> Close_par :: lexer_list xs
  | "|" :: xs -> Or :: lexer_list xs
  | "+" :: xs -> Or :: lexer_list xs
  | "&" :: xs -> And :: lexer_list xs
  | "*" :: xs -> And :: lexer_list xs
  | "~" :: xs -> Not :: lexer_list xs 
  | "-" :: ">" :: xs -> Implication :: lexer_list xs
  | "<" :: "-" :: ">" :: xs -> Biimplication :: lexer_list xs
  | ">" :: xs -> Implication :: lexer_list xs 
  | "<" :: xs -> Biimplication :: lexer_list xs
  | "a" :: xs -> let ys, zs = String_utility.get_following_lower xs in List.map (fun y -> All y) ys @ lexer_list zs
  | "e" :: xs -> let ys, zs = String_utility.get_following_lower xs in List.map (fun y -> Exists y) ys @ lexer_list zs
  | x :: xs ->
      if String_utility.is_upper (String.get x 0) then
        match xs with
        | "("::y::ys -> if String_utility.is_lower (String.get y 0) then let (bs, cs) = get_vars (y :: ys) in LitVar (x, bs) :: lexer_list cs else Literal x :: lexer_list xs 
        | _ -> Literal x :: lexer_list xs
      else raise (LexingError x)
  | [] -> [ End ]

(*
let rec pprint xs = 
  match xs with 
  |x::xs -> (
    match x with 
    | Open_par -> print_endline "Open_par"; pprint xs
    | Close_par -> print_endline "Close_par"; pprint xs
    | Biimplication -> print_endline "Biimplication"; pprint xs
    | Implication -> print_endline "Implication"; pprint xs
    | Or -> print_endline "Or"; pprint xs
    | And -> print_endline "And"; pprint xs
    | Not -> print_endline "Not"; pprint xs
    | All (s)  -> print_endline ("All" ^ s); pprint xs;
    | Exists (s) -> print_endline ("Exists" ^ s); pprint xs
    | LitVar (s, ss)  -> print_endline ("LitVar" ^ s ^ (String.concat "," ss)); pprint xs
    | Literal (s) -> print_endline ("Literal" ^ s); pprint xs
    | End -> print_endline "End"; pprint xs
  )
  |[] -> ()
*)

let lexer input = lexer_list (String_utility.convert_to_standard (String_utility.string_to_list input))