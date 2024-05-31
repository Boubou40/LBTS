open Ast

let rec collect_secret_ids lst =
  match lst with
  | [] -> []   (* Cas de base: liste vide, renvoie une liste vide *)
  | (id, Funval(_)) :: tail -> id :: collect_secret_ids tail (* Si exp est Secret_fun, inclut l'id dans le résultat *)
  | _ :: tail -> collect_secret_ids tail  (* Ignore les autres types d'exp et continue le parcours *)