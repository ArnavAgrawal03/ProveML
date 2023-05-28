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
let resolution (kb : prop) (alpha : prop) (print_proof : bool) =
  (* let og_clauses = And (kb, Not alpha) |> cnf_of_prop in *)
  let clauses = And (kb, Not alpha) |> cnf_of_prop in
  if print_proof then (
    print_endline "";
    print_endline ("Suppose for contradiction: " ^ string_of_prop (And (kb, Not alpha)));
    print_endline ("CNF form of assumption: " ^ (clauses |> string_of_cnf)));
  let rec loop clauses =
    if print_proof then
      print_endline ("Entering loop with clauses: " ^ (clauses |> string_of_cnf));
    if ClauseSet.is_empty clauses then (
      if print_proof then print_endline "Clauses are empty. Returning None.";
      Some false)
    else
      let clause1 = ClauseSet.choose clauses in
      let rest_clauses = ClauseSet.remove clause1 clauses in
      let resolvents =
        ClauseSet.fold
          (fun clause2 acc ->
            if print_proof then
              print_endline
                ("Resolving clauses: " ^ string_of_clause clause1 ^ " and "
               ^ string_of_clause clause2);
            let new_clauses = resolve clause1 clause2 in
            if print_proof then
              print_endline ("Resulting clauses: " ^ string_of_cnf new_clauses);
            ClauseSet.union new_clauses acc)
          rest_clauses ClauseSet.empty
      in
      if print_proof then print_endline ("Resolvents: " ^ string_of_cnf resolvents);
      if ClauseSet.mem LiteralSet.empty resolvents then (
        if print_proof then
          print_endline
            "Found empty clause. Reached Contradiction. The assumption is false. So, the \
             original statement is true. QED.";
        Some true)
      else
        let updated_clauses =
          ClauseSet.union rest_clauses (ClauseSet.remove clause1 resolvents)
        in
        if print_proof then (
          print_endline "Did not find empty clause, so haven't reach a contradiction yet.";
          print_endline ("Updated set of clauses: " ^ string_of_cnf updated_clauses));
        (* if ClauseSet.subset resolvents og_clauses then Some false else *)
        if ClauseSet.equal updated_clauses clauses then Some false
        else loop updated_clauses
  in
  loop clauses
