type symbol = string

let string_of_symbol x = x

type t =
  | Var of symbol
  | Func of symbol * t
  | Closure of symbol * t * env
  | App of t * t
and env = (symbol * t) list

let rec string_of_t = function
  | Var x -> "(" ^ string_of_symbol x ^ ")"
  | Func (x,f) -> "(\\" ^ string_of_symbol x ^ 
                  " -> " ^ (string_of_t f) ^ ")"
  | Closure (x,f,e) -> string_of_t (Func (x,f))
  | App (e1,e2) -> "(" ^ (string_of_t e1) ^ 
                   " " ^ (string_of_t e2) ^ ")"
  
let lookup x env = List.assoc x env
let extend env x e = (x,e) :: env
let empty () = []

let rec eval e env = match e with
  | Var x -> 
    begin try lookup x env with 
      Not_found -> failwith ("Could not find variable " ^ x)
    end
  | Func (x,f) -> Closure (x,f,env)
  | Closure (x,f,env') -> Closure (x,f,env')
  | App (e1,e2) ->
    match eval e1 env with
      | Closure (x,f,env') ->
        let env'' = extend env' x e1 in
        eval e2 env''
      | _ -> failwith "Can't apply"
