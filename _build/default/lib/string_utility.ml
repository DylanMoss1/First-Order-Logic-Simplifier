exception GetVariableError

let single_string s x = String.sub s x 1

let string_to_list s = List.init (String.length s) (single_string s)

let convert_int_to_string x = 
  match x with
  |194 -> Some(" ")
  |172 -> Some("~") 
  |226 -> Some(" ")
  |134 -> Some(" ")
  |136 -> Some(" ")
  |167 -> Some("&")
  |168 -> Some("|")
  |146 -> Some(">")
  |148 -> Some("<")
  |128 -> Some("a")
  |131 -> Some("e")
  |_ -> None 

let convert_special_symbols x = 
  let i = convert_int_to_string (Char.code (Bytes.get (Bytes.of_string x) 0)) in 
  match i with 
  | Some(s) -> s 
  | None -> x

let convert_to_standard xs = 
  List.filter
    (fun s -> not (s = " ")) (List.map (fun x -> convert_special_symbols x) xs)


let convert_to_standard1 xs = 
  List.map (fun x -> print_int (Char.code (Bytes.get (Bytes.of_string x) 0)); print_endline ""; x) xs 


let is_upper c =
  let i = int_of_char c in
  65 <= i && i <= 90

let is_lower c =
  let i = int_of_char c in
  97 <= i && i <= 122

let is_lower_and_not_quant c =
  is_lower c
  &&
  let i = int_of_char c in
  i != 97 && i != 101

let rec get_following_lower xs =
  match xs with
  | x :: xs ->
      if is_lower_and_not_quant (String.get x 0) then
        let ys, zs = get_following_lower xs in
        (x :: ys, zs)
      else ([], x :: xs)
  | [] -> raise GetVariableError
