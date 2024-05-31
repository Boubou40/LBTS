open Ast
open Env


let rec eval (e:exp) (env: evT env) = 
  match e with
  | Den x -> lookup env x
  | CstInt(n) -> Int(n,Untainted) 
  | CstTrue -> Bool(true,Untainted) 
  | CstString s -> String(s,Untainted)
  | CstFalse -> Bool(false,Untainted)

  | Iszero(e1) -> is_zero(eval e1 env)
  | Eq(e1, e2) -> int_eq((eval e1 env), (eval e2 env))
  | Times(e1,e2) -> int_times((eval e1 env), (eval e2 env))
  | Sum(e1, e2) -> int_plus ((eval e1 env), (eval e2 env))
  | Sub(e1, e2) -> int_sub ((eval e1 env), (eval e2 env))
  | And(e1, e2) -> bool_and((eval e1 env), (eval e2 env))
  | Or(e1, e2) -> bool_or ((eval e1 env), (eval e2 env))
  | Not(e1) -> bool_not(eval e1 env)

  | Ifthenelse(cond, e1, e2) ->
    let g = eval cond env in
    (match typecheck("bool", g), g with
    | true, Bool(true, taint_status) -> 
        let result = eval e1 env in
        add_taint result taint_status

    | true, Bool(false, taint_status) -> 
        let result = eval e2 env in
        add_taint result taint_status

    | (_,_ ) -> failwith("nonboolean guard"))

  | Let(id, e, ebody) -> eval ebody (bind env id (eval e env))

  | Fun(arg,ebody) -> Funval(arg,ebody,_) 
 (* taint useless because evaluated in apply according to env*)
  (* in fact it is usefull because the plug-in are tainted*)

  | SecretFun(f) ->
    begin match env with
    | TrustedEnv(_) ->
        let result = eval f env in    (* traite f comme Fun(arg,ebody,taint) ici *)
        SecretVal(result)             (* result = Funval *)
    | UntrustedEnv(_) -> failwith("Illegal flow: Environment is not secured, cannot evaluate")
    end



  | Apply(Den(f), eArg) ->  (* eArg type exp car si cest une variable, eval eArg produit un lookup + composabilité : Apply(f, Apply(g,x)) *)
    let fval = lookup env f in 
    let aVal = eval eArg env in
    let taint = get_taint(aVal) in
    (match fval with 
    | SecretVal(Funval(arg, fbody,_)) -> (* si f(x) -> x = arg *)
      if ( (taint == Untainted) || (match aVal with 
        |SecretVal(_) -> true 
        | _ -> false) 
      ) then let aenv = bind env arg aVal in eval fbody aenv
        else failwith("Illegal flow: try to evaluate tainted arg in Trusted Fun")
      
    | Funval(arg,fbody,_) -> (match aVal with
      | SecretVal(_) -> failwith("Illegal flow: try to evaluate SecretVal with Untrusted Fun")
      | _ -> let aenv = bind env arg aVal in eval fbody aenv)
    
    | _ -> failwith("non functional value"))

  | Apply(_,_) -> failwith("Application: not first order function") 


  | Block (id_b, body) -> 
    if (option_lookup env id_b) != None then 
        failwith (id_b ^ " is already declared")
    else if slvl = EnclaveLvl then
        failwith ("can't declare a block inside another one")
    else if slvl = Untrusted then
        failwith ("can't declare a block inside of untrusted code")
    else
        let result = eval body TrustedEnv([]) in (* create new env *)
        match result with
        | ExecTermination(block_env) -> (
            let handler = collect_secret_ids block_env in
            let tb = BlockVal {block_env = block_env; handler = handler} in (* crée l'enclave*)
            tb
        )
        | _ -> failwith "Trusted block not declared correctly"
  

and eval_b (e:secret_exp) (env: evT env )
  match e with
  | Trusted_let(i,e,ebody) -> SecretVal(eval ebody (trusted_bind env id (eval e env)))




