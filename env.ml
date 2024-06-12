type ide = string

type 'a env = 
  | TrustedEnv of (ide * 'a) list
  | UntrustedEnv of (ide * 'a) list

let emptyEnv = UntrustedEnv([])

let rec lookup env (x : ide) =
  match env with
    | [] -> failwith (x ^ " not found") 
    | (y, v) :: r -> if x = y then v else lookup r x


    
let rec option_lookup env (x : ide) =
  match env with
    | [] -> None
    | (y, v) :: r -> if x = y then Some(v) else option_lookup r x (* On utilise Some car on retroune None si on trouve rien (il faut des type option)*)


