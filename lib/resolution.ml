open Prop
include Cnf

(** [negate_literal l] is Pos a if l is Neg a and vice-versa *)
let negate_literal = function Neg a -> Pos a | Pos a -> Neg a

(** [resolve_aux l c1 c2] *)
let resolve_aux (l : literal) (c1 : clause) (c2 : clause) =
  let negated_l = negate_literal l in
  let c1' = LiteralSet.filter (( <> ) l) c1 in
  let c2' = LiteralSet.filter (( <> ) negated_l) c2 in
  LiteralSet.union c1' c2'

(** [resolve c1 c2] resolves two clauses, returning a list of clauses that are the result
    of resolving [c1] and [c2] *)
let resolve (c1 : clause) (c2 : clause) = ()

(** [resolve_all c] resolves all clauses in [c] with each other, returning a list of
    clauses that are the result of resolving all clauses in [c] *)
let resolve_all (c : cnf) = failwith "unimplemented"

let pl_resolution (kb : prop) (alpha : prop) =
  let clauses = And (kb, Not alpha) |> cnf_of_prop in
  let new_clauses = [] in
  ()
(* for each clause c_i, c_j in clauses:*)
(* resolvents = pl-resolve (c_i, c_j)*)
(* if [] in resolvents return true*)
(* new = union new resolvents*)
(* if new is a subset of clauses return false*)
(* clauses = union clauses new*)
