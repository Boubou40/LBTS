open Ast
open Env


let rec eval (e:exp) (env: evT env) (g: taintness) = 
  match e with
  | Den x -> lookup env x
  | CstInt(n) -> Int(n,Untainted) 
  | CstTrue -> Bool(true,Untainted) 
  | CstString s -> String(s,Untainted)
  | CstFalse -> Bool(false,Untainted)

  | Iszero(e1) -> is_zero(eval e1 env g)
  | Eq(e1, e2) -> int_eq((eval e1 env g), (eval e2 env g))
  | Times(e1,e2) -> int_times((eval e1 env g), (eval e2 env g))
  | Sum(e1, e2) -> int_plus ((eval e1 env g), (eval e2 env g))
  | Sub(e1, e2) -> int_sub ((eval e1 env g), (eval e2 env g))
  | And(e1, e2) -> bool_and((eval e1 env g), (eval e2 env g))
  | Or(e1, e2) -> bool_or ((eval e1 env g), (eval e2 env g))
  | Not(e1) -> bool_not(eval e1 env g)

  | Ifthenelse(cond, e1, e2) ->
    let r = eval cond env g in
    (match typecheck("bool", r), r with
    | true, Bool(true, taint_status) -> 
        eval e1 env taint_status 
  

    | true, Bool(false, taint_status) -> 
         eval e2 env taint_status 
    

    | (_,_ ) -> failwith("nonboolean guard"))

  | Let(id, e, ebody) -> match g with  (* let x = 30 in x+12*)
    | Tainted -> 
      let result = eval ebody (bind env id (eval e env g)) g in
      add_taint(result,Tainted)
    | _ -> eval ebody (bind env id (eval e env g)) g
 

  | Fun(arg,ebody) -> Funval(arg,ebody,Untainted) 


  
 
  | Apply(Den(f), eArg) ->  (* eArg type exp car si cest une variable, eval eArg produit un lookup + composabilité : Apply(f, Apply(g,x)) *) 
    let fval = lookup env f in 
    (match favl with 
      | Funval(arg, fbody) ->
        let aVal = eval eArg env g in
        let taint = get_taint(aVal) in
        let aenv = bind env arg aVal g in 
        let result = eval fbody aenv g in
        add_taint(result,taint)
      | _ -> failwith("non functionalvalue"))
  | Apply(_,_) -> failwith("Application: notfirst orderfunction")


  | DecBlock (id_b, body) -> 
    match env with 
    | TrustedEnv -> failwith "cannot declare a block inside a block"
    | UntrustedEnv ->
    if (g = Tainted) 
      then failwith "cannot declare a block in a Tainted context"
    else if (option_lookup env id_b) != None then 
        failwith (id_b ^ " is already declared")
    else
        let result = eval body TrustedEnv([]) g in (* create new env *)
        match result with
        | ExecTermination(block_env) -> (
            let handler = collect_secret_ids block_env in
            let tb = Block {block_env = block_env; handler = handler} in (* crée l'enclave*)
            tb
        )
        | _ -> failwith "Trusted block not declared correctly"

  | EndBlock -> (
      ExecTermination env
    )

  | Include (parg, pBody) -> (
      match env with
          | TrustedEnv -> failwith "can't include plug-in inside of a Block"
          
          | UntrustedEnv -> PluginVal(parg, pBody)     
    )

  | Exec (pname, arg) -> 
    match env with 
    | TrustedEnv -> failwith "cannot execute plugin in trusted env"
    | UntrustedEnv ->
    let pval = lookup env pname in
    match pval with
      | PluginVal(parg,pbody) -> 
        let aVal = eval arg env Tainted in
        let aenv = bind env arg aVal in 
        let result = eval pbody aenv g
      in add_taint(result, Tainted)
   
     


  | Handle(block_name, fun_id , eArg) -> 
    if g = Tainted
      then faiwith "cannot call a trusted function in a Tainted context"
    else
      let aval = lookup env eArg in
      let taint = get_taint aval in
      let tb = lookup env block_name in
      match taint with
        | Tainted -> failwith "cannot evaluate tainted value in the block_fun"
        | _ -> match find_fun fun_id tb.handler with
          | false -> failwith "cannot found this function"
          | true -> match get_fun fun_id tb.block_env with
            | (arg,fbody) -> 
              let aVal = eval eArg env g in
              let aenv = bind env arg aVal g in 
              eval fbody aenv g
            | _ -> failwith "An error occured" 

  | SecretLet(id, e, ebody) ->
    match env with
    TrustedEnv ->
    let sval = b_eval e env g in
    let new_env = bind env id SecretVal(sval) in
    b_eval ebody new_env g
    | _ -> failwith "cannot declare a secret variable in Untrusted env"

  | Empty -> failwith "end of the progam"
  


