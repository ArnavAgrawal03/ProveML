open Prop
open Cnf

(** [negate_literal l] is Pos a if l is Neg a and vice-versa *)
let negate_literal = function Neg a -> Pos a | Pos a -> Neg a

(** [resolve c1 c2] resolves two clauses, returning a set of clauses that are the result
    of resolving [c1] and [c2] *)
let resolve (c1 : clause) (c2 : clause) =
  let new_clauses =
    LiteralSet.fold
      (fun lit acc ->
        let neg_lit = negate_literal lit in
        if LiteralSet.mem neg_lit c2 then
          let new_clause =
            LiteralSet.union (LiteralSet.remove lit c1) (LiteralSet.remove neg_lit c2)
          in
          ClauseSet.add new_clause acc
        else acc)
      c1 ClauseSet.empty
  in
  new_clauses

(** [resolution kb alpha] is [Some true] if [alpha] is [true] given [kb]. [Some false] if
    [alpha] is [false] given [kb]. [None] handles the case where we're unsure (shouldn't
    occur)*)
let resolution (kb : prop) (alpha : prop) =
  let clauses = And (kb, Not alpha) |> cnf_of_prop in
  let rec loop clauses =
    if ClauseSet.is_empty clauses then None
    else
      let clause1 = ClauseSet.choose clauses in
      let rest_clauses = ClauseSet.remove clause1 clauses in
      let resolvents =
        ClauseSet.fold
          (fun clause2 acc ->
            let new_clauses = resolve clause1 clause2 in
            ClauseSet.union new_clauses acc)
          rest_clauses ClauseSet.empty
      in
      if ClauseSet.mem LiteralSet.empty resolvents then Some true
      else
        let updated_clauses = ClauseSet.union rest_clauses resolvents in
        if ClauseSet.equal clauses updated_clauses then Some false
        else loop updated_clauses
  in
  loop clauses
