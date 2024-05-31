open Env

type taintness =
  | Tainted
  | Untainted


type ide = string 


type trust=
  | Trusted
  | Untrusted


type exp = 
  | Den of ide 
  | CstInt of int 
  | CstTrue 
  | CstFalse 
  | Times of exp * exp
  | Sum of exp * exp
  | Sub of exp * exp
  | Eq of exp * exp
  | Iszero of exp
  | Or of exp * exp
  | And of exp * exp
  | Not of exp


  | Ifthenelse of exp * exp * exp
  | Let of ide * exp * exp
  | Fun of ide * exp 
  | Apply of exp * exp

  | Block of ide * secret_exp * exp  (* block_name * let(data->fun) * handle *)
  | Handle of ide * ide * exp (* block_name * block_fun * arg*)

  | Empty
 
and secret_exp = 
  | Secret_let of ide * exp * exp

  

type 'a trustBlock = {
  block_env: 'a env;
  handler: ide list;
  }
  

  
type evT = Int of int * taintness
  | String of string * taintness
  | Bool of bool * taintness
  | Unbound
  | Funval of ide * exp * taintness 
  | BlockVal of evT trustBlock

  | SecretVal of evT
  | PluginVal of evT
