exception ConvertImplicationsError
exception PushInNotsError
exception ExpandAnds

let rec convert_implications t =
  match t with
  | Ast.Biimplication ts -> (
      match ts with
      | [ t1; t2 ] ->
          Ast.And [ Ast.Or [ Ast.Not t1; t2 ]; Ast.Or [ t1; Ast.Not t2 ] ]
      | t1 :: ts ->
          Ast.And
            [
              Ast.Or [ Ast.Not t1; Ast.Biimplication ts ];
              Ast.Or [ t1; Ast.Not (Ast.Biimplication ts) ];
            ]
      | _ -> raise ConvertImplicationsError)
  | Ast.Not (Ast.Biimplication ts) -> (
      match ts with
      | [ t1; t2 ] ->
          Ast.And [ Ast.Or [ t1; t2 ]; Ast.Or [ Ast.Not t1; Ast.Not t2 ] ]
      | t1 :: ts ->
          Ast.And
            [
              Ast.Or [ t1; Ast.Biimplication ts ];
              Ast.Or [ Ast.Not t1; Ast.Not (Ast.Biimplication ts) ];
            ]
      | _ -> raise ConvertImplicationsError)
  | Ast.Implication (t1, t2) -> Ast.Or [ Ast.Not t1; t2 ]
  | Ast.Not (Ast.Implication (t1, t2)) -> Ast.And [ t1; Ast.Not t2 ]
  | Ast.Or ts -> Ast.Or (List.map convert_implications ts)
  | Ast.And ts -> Ast.And (List.map convert_implications ts)
  | Ast.Not t -> Ast.Not (convert_implications t)
  | Ast.All (l, t) -> Ast.All (l, convert_implications t)
  | Ast.Exists (l, t) -> Ast.Exists (l, convert_implications t)
  | Ast.Literal t -> Ast.Literal t
  | Ast.LitVar (l, xs) -> Ast.LitVar (l, xs)

let rec push_in_nots t =
  match t with
  | Ast.Not (Ast.Or ts) -> Ast.And (map_nots ts)
  | Ast.Not (Ast.And ts) -> Ast.Or (map_nots ts)
  | Ast.Not (Ast.Not t) -> t
  | Ast.Not (Ast.All (l, t)) -> Ast.Exists (l, push_in_nots (Ast.Not t))
  | Ast.Not (Ast.Exists (l, t)) -> Ast.All (l, push_in_nots (Ast.Not t))
  | Ast.Not (Ast.Literal l) -> Ast.Not (Ast.Literal l)
  | Ast.Not (Ast.LitVar (l, xs)) -> Ast.Not (Ast.LitVar (l, xs))
  | Ast.Or ts -> Ast.Or (List.map push_in_nots ts)
  | Ast.And ts -> Ast.And (List.map push_in_nots ts)
  | Ast.All (l, t) -> Ast.All (l, push_in_nots t)
  | Ast.Exists (l, t) -> Ast.Exists (l, push_in_nots t)
  | Ast.Literal l -> Ast.Literal l
  | Ast.LitVar (l, xs) -> Ast.LitVar (l, xs)
  | _ -> raise PushInNotsError

and map_nots ts = match ts with t :: ts -> Ast.Not t :: map_nots ts | [] -> []

let expand_ands t = match t with Ast.And ts -> ts | _ -> [ t ]

let rec list_map ts tss =
  match ts with
  | t :: ts -> List.map (List.cons t) tss @ list_map ts tss
  | [] -> []

let rec combinations tss =
  match tss with ts :: tss -> list_map ts (combinations tss) | [] -> [ [] ]

let rec add_or xs = match xs with x :: xs -> Ast.Or x :: add_or xs | [] -> []

let or_combinations tss =
  let xs = combinations tss in
  Ast.And (add_or xs)

let rec push_in_ors t =
  match t with
  | Ast.Or ts -> or_combinations (List.map expand_ands ts)
  | Ast.And ts -> Ast.And (List.map push_in_ors ts)
  | Ast.Not t -> Ast.Not (push_in_ors t)
  | Ast.All (l, t) -> Ast.All (l, push_in_ors t)
  | Ast.Exists (l, t) -> Ast.Exists (l, push_in_ors t)
  | Ast.Literal l -> Ast.Literal l
  | Ast.LitVar (l, xs) -> Ast.LitVar (l, xs)
  | _ -> raise ExpandAnds

let rec loop f t =
  let t' = Ast.cleanup_ast (f t) in
  if Ast.equal t t' then t'
  else (
    Ast.pprint t' true;
    loop f t')

let convert t =
  loop push_in_ors (loop push_in_nots (loop convert_implications t))
