open Prop
open Cnf

let print_proof_start print_proof kb alpha clauses =
  if print_proof then (
    print_endline "";
    print_endline ("Suppose for contradiction: " ^ string_of_prop (And (kb, Not alpha)));
    print_endline ("CNF form of assumption: " ^ (clauses |> string_of_cnf)))

let print_loop1 print_proof clauses =
  if print_proof then
    print_endline ("Entering loop with clauses: " ^ (clauses |> string_of_cnf))

let print_loop_empty print_proof =
  if print_proof then
    print_endline
      "Clauses are empty. So, the assumption evaluates to true. Returning False."

let print_resolving print_proof clause1 clause2 =
  if print_proof then
    print_endline
      ("Resolving clauses: " ^ string_of_clause clause1 ^ " and "
     ^ string_of_clause clause2)

let print_resolved print_proof new_clauses =
  if print_proof then print_endline ("Resulting clauses: " ^ string_of_cnf new_clauses)

let print_resol1 print_proof c_i c_j new_clauses =
  if ClauseSet.cardinal new_clauses > 0 then (
    print_resolving print_proof c_i c_j;
    print_resolved print_proof new_clauses)

let print_resolvents print_proof resolvents =
  if print_proof then print_endline ("Resolvents: " ^ string_of_cnf resolvents)

let print_contradiction print_proof =
  if print_proof then
    print_endline
      "Found empty clause. Reached Contradiction. The assumption is false. So, the \
       original statement is true. QED."

let print_continue print_proof =
  if print_proof then
    print_endline "Did not find empty clause, so haven't reach a contradiction yet."

let print_no_progress print_proof =
  if print_proof then (
    print_endline "No new clauses were added. So, we cannot reach a contradiction.";
    print_endline "The assumption is true. So, the original statement is false. QED.")
