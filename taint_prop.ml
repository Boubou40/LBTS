open Ast





let typecheck (t, typeDescriptor) = 
  match t with 
  | "int" -> 
  (match typeDescriptor with
  | Int(u,_) -> true
  | _ -> false) 
  | "bool" ->
  (match typeDescriptor with 
  | Bool(u,_) -> true
  | _ -> false) 
  | "string" ->
  (match typeDescriptor with 
  | String(u,_) -> true
  | _ -> false) 

  | _ -> failwith ("not a valid type");;

(* let handle f env = 1 Impossible d'avoir handle sous cette forme : handle a besoin de eval, mais est appelé dans eval*)

  (* Expression definition*)

let is_zero x = match (typecheck("int",x), x) with 
| (true, Int(y,taint)) -> Bool(y=0,taint) 
| (_, _) -> failwith("run-time error");; 


let int_eq(x,y) = 
match (typecheck("int",x), typecheck("int",y), x, y) with 
| (true, true, Int(v,t1), Int(w,t2)) -> Bool(v = w, if t1 == Tainted || t2 == Tainted then Tainted else Untainted) 
| (_,_,_,_) -> failwith("run-time error ");; 

let int_plus(x, y) =
match(typecheck("int",x), typecheck("int",y), x, y) with 
| (true, true, Int(v,t1), Int(w,t2)) -> Int(v + w, if t1 == Tainted || t2 == Tainted then Tainted else Untainted) 
| (_,_,_,_) -> failwith("run-time error ");;

let int_sub(x, y) =
match(typecheck("int",x), typecheck("int",y), x, y) with 
| (true, true, Int(v,t1), Int(w,t2)) -> Int(v - w, if t1 == Tainted || t2 == Tainted then Tainted else Untainted) 
| (_,_,_,_) -> failwith("run-time error ");;


let int_times(x, y) =
match(typecheck("int",x), typecheck("int",y), x, y) with 
| (true, true, Int(v,t1), Int(w,t2)) -> Int(v * w, if t1 == Tainted || t2 == Tainted then Tainted else Untainted) 
| (_,_,_,_) -> failwith("run-time error ");;


let bool_and(b1,b2) = 
  match (typecheck("bool",b1), typecheck("bool",b2), b1, b2) with
  | (true, true, Bool(v1,t1), Bool(v2,t2)) -> Bool(v1 && v2,  if t1 == Tainted || t2 == Tainted then Tainted else Untainted)
  | (_,_,_,_) -> failwith("run-time error ");;

let bool_or(b1,b2) = 
  match (typecheck("bool",b1), typecheck("bool",b2), b1, b2) with
  | (true, true, Bool(v1,t1), Bool(v2,t2)) -> Bool(v1 || v2, if t1 == Tainted || t2 == Tainted then Tainted else Untainted)
  | (_,_,_,_) -> failwith("run-time error ");;

let bool_not b1 = 
  match (typecheck("bool",b1), b1) with
  | (true, Bool(true,t)) -> Bool(false,t)
  | (true, Bool(false,t)) -> Bool(true,t)
  | (_,_) -> failwith("run-time error ");;
