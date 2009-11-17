type symbol = string

let string_of_symbol x = x

type expr =
  | Var of symbol
  | Lamb of symbol * expr
  | App of expr * expr
and env = (symbol * expr) list

let rec string_of_expr = function
  | Var x -> "(" ^ string_of_symbol x ^ ")"
  | Lamb (x,f) -> "(\\" ^ (string_of_symbol x) ^ 
                  " -> " ^ (string_of_expr f) ^ ")"
  | App (e1,e2) -> "(" ^ (string_of_expr e1) ^ 
                   " " ^ (string_of_expr e2) ^ ")"
  
let lookup x env = List.assoc x env
let extend env x e = (x,e) :: env
let empty () = []

let rec eval e env = match e with
  | Var x -> lookup x env
  | Lamb (x,f) -> Lamb (x,f)
  | App (e1,e2) ->
    match eval e1 env with
      | Lamb (x,f) ->
        let env' = extend env x e1 in
        eval e2 env'
      | _ -> failwith "Can't apply"
