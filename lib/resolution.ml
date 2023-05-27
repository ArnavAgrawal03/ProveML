open Prop
open Cnf

(** [negate_literal l] is Pos a if l is Neg a and vice-versa *)
let negate_literal = function Neg a -> Pos a | Pos a -> Neg a

(** [resolve c1 c2] resolves two clauses, returning a set of clauses that are the result
    of resolving [c1] and [c2] *)
let resolve (c1 : clause) (c2 : clause) =
  let new_clauses = ref ClauseSet.empty in
  LiteralSet.iter
    (fun lit ->
      let neg_lit = negate_literal lit in
      if LiteralSet.mem neg_lit c2 then
        let new_clause =
          LiteralSet.union (LiteralSet.remove lit c1) (LiteralSet.remove neg_lit c2)
        in
        new_clauses := ClauseSet.add new_clause !new_clauses)
    c1;
  !new_clauses

exception Done of bool

let resolution (kb : prop) (alpha : prop) =
  let clauses = ref (And (kb, Not alpha) |> cnf_of_prop) in
  let new_clauses = ref ClauseSet.empty in
  try
    ClauseSet.iter
      (fun c_i ->
        ClauseSet.iter
          (fun c_j ->
            let resolvents = resolve c_i c_j in
            if ClauseSet.mem LiteralSet.empty resolvents then raise (Done true)
            else new_clauses := ClauseSet.union resolvents !new_clauses;
            if ClauseSet.subset !new_clauses !clauses then raise (Done false)
            else clauses := ClauseSet.union !new_clauses !clauses)
          !clauses)
      !clauses;
    None
  with Done b -> Some b

(* for each clause c_i, c_j in clauses:*)
(* resolvents = pl-resolve (c_i, c_j)*)
(* if [] in resolvents return true*)
(* new = union new resolvents*)
(* if new is a subset of clauses return false*)
(* clauses = union clauses new*)
