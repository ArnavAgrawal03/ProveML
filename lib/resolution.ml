open Prop
open Cnf
open Printer

(** [negate_literal l] is Pos a if l is Neg a and vice-versa *)
let negate_literal = function Neg a -> Pos a | Pos a -> Neg a

(** [ClausePair] is a module for passing to [Set.Make] to make a set of pairs of clauses.
    This is used for generating a cartesian product of two sets of clauses.*)
module ClausePair = struct
  type t = clause * clause

  let compare (clause_a1, clause_a2) (clause_b1, clause_b2) =
    match LiteralSet.compare clause_a1 clause_b1 with
    | 0 -> LiteralSet.compare clause_a2 clause_b2
    | x -> x
end

module ClausePairSet = Set.Make (ClausePair)
(** A set of pairs of clauses*)

(** [add_single c cs] is a set of pair of clauses with pairs [(c, d)] where [d] is in [cs]*)
let add_single clause clauses =
  clauses |> ClauseSet.elements
  |> List.map (fun x -> (clause, x))
  |> ClausePairSet.of_list

(** [product cs1 cs2] is the cartesian product of [cs1] and [cs2] *)
let product (clauses1 : ClauseSet.t) (clauses2 : ClauseSet.t) =
  ClauseSet.fold
    (fun clause -> ClausePairSet.union (add_single clause clauses2))
    clauses1 ClausePairSet.empty

(** [resolve c1 c2] resolves two clauses, returning a set of clauses that are the result
    of resolving [c1] and [c2] *)
let resolve (c1 : clause) (c2 : clause) =
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

(** [resolution kb alpha] is [Some true] if [alpha] is [true] given [kb]. [Some false] if
    [alpha] is [false] given [kb]. [None] handles the case where we're unsure (shouldn't
    occur)*)
let resolution (kb : prop) (alpha : prop) (print_proof : bool) =
  (* let og_clauses = And (kb, Not alpha) |> cnf_of_prop in *)
  let clauses = And (kb, Not alpha) |> cnf_of_prop in
  print_proof_start print_proof kb alpha clauses;
  let rec loop clauses =
    print_loop1 print_proof clauses;
    if ClauseSet.is_empty clauses then (
      print_loop_empty print_proof;
      false)
    else
      let clause1 = ClauseSet.choose clauses in
      let rest_clauses = ClauseSet.remove clause1 clauses in
      let resolvents =
        ClauseSet.fold
          (fun clause2 acc ->
            print_resolving print_proof clause1 clause2;
            let new_clauses = resolve clause1 clause2 in
            print_resolved print_proof new_clauses;
            ClauseSet.union new_clauses acc)
          rest_clauses ClauseSet.empty
      in
      print_resolvents print_proof resolvents;
      if ClauseSet.mem LiteralSet.empty resolvents then (
        print_contradiction print_proof;
        true)
      else
        let updated_clauses =
          ClauseSet.union rest_clauses (ClauseSet.remove clause1 resolvents)
        in
        print_continue print_proof updated_clauses;
        (* if ClauseSet.subset resolvents og_clauses then Some false else *)
        if ClauseSet.equal updated_clauses clauses then (
          print_no_progress print_proof;
          false)
        else loop updated_clauses
  in
  loop clauses

exception Done of bool

let resolution2_aux kb alpha =
  let og_clauses = And (kb, Not alpha) |> cnf_of_prop in
  let new_clauses = ref ClauseSet.empty in
  let rec loop clauses =
    let pairs = product clauses clauses in
    ClausePairSet.iter
      (fun (c_i, c_j) ->
        let resolvents = resolve c_i c_j in
        if ClauseSet.mem LiteralSet.empty resolvents then raise (Done true)
        else new_clauses := ClauseSet.union !new_clauses resolvents)
      pairs;
    if ClauseSet.subset !new_clauses clauses then raise (Done false)
    else loop (ClauseSet.union clauses !new_clauses)
  in
  loop og_clauses

let resolution2 kb alpha = try resolution2_aux kb alpha with Done b -> b
