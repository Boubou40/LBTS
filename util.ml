open Ast

let rec collect_secret_ids lst =
  match lst with
  | [] -> []   (* Cas de base: liste vide, renvoie une liste vide *)
  | (id, SecretVal(Funval(_,_,_))) :: tail -> id :: collect_secret_ids tail (* Si exp est Secret_fun, inclut l'id dans le résultat *)
  | _ :: tail -> collect_secret_ids tail  (* Ignore les autres types d'exp et continue le parcours *)


let rec find_fun_id name func_list =
  match func_list with
  | [] -> false 
  | func_name::t ->
      if func_name = name then true 
      else find_fun_id name t 



let rec get_fun name (env: evT env) =
  match env with
  | [] -> false 
  | (func_name,SecretVal(Funval(f_arg,f_body)))::t ->
      if func_name = name then (f_arg,f_body)
      else get_fun name t 


let get_taint v =
  match v with
  | Int (_,t) -> t
  | Bool(_,t) -> t
  | String (_,t) -> t
  | Unbound -> failwith("Unbound value")
  | _ -> failwith("No taint available")


let rec add_taint v t =
  match v with
    | Int(value, _) -> Int(value, t)
    | Bool(value, _) -> Bool(value, t)
    | Funval(ide, result,_) -> Funval(ide, result,t)
    | String(s, _) -> String(s, t)
    | Unbound -> Unbound
    | SecretVal(_) ->   (* only for control flow *)
        if t = Tainted then 
            failwith("Illegal flow") 
        else 
            v
    | PluginVal(e) -> add_taint e Tainted



let bind env (x:string) (v:evT) = 
  match env with
  | TrustedEnv lst -> 
    let taint = get_taint(v) in
    (match taint with
    | Untainted -> TrustedEnv((x,v)::lst)
    | _ -> failwith("Illegal flow")
    )
  | UntrustedEnv lst -> 
    (match v with
    | SecretVal(_) -> failwith("Illegal flow")
    | _ -> UntrustedEnv((x,v)::lst))


let rec update_taint env id new_taint = 
  match env with
  | [] -> []  (* Si la liste est vide, rien à faire, retourne liste vide *)
  | (key, v) :: t ->
      if key = id then 
        (key, add_taint(v,new_taint)) :: t  (* Mise à jour du taint et retour de la nouvelle liste *)
      else
        (key, v) :: update_taint t id new_taint  (* Sinon, parcours du reste de la liste *)
