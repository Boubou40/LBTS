type ide = string

type 'a env = (ide * 'a) list



let rec lookup env (x : ide) =
  match env with
    | [] -> failwith (x ^ " not found") 
    | (y, v) :: r -> if x = y then v else lookup r x


    
let rec option_lookup env (x : ide) =
  match env with
    | [] -> None
    | (y, v) :: r -> if x = y then Some(v) else option_lookup r x