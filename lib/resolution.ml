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

exception Done of bool

let resolution2_aux kb alpha print_proof =
  let og_clauses = cnf_of_prop (kb &&& ~~alpha) in
  print_proof_start print_proof kb alpha og_clauses;
  let new_clauses = ref ClauseSet.empty in
  let rec loop clauses =
    print_loop1 print_proof clauses;
    let pairs = product clauses clauses in
    ClausePairSet.iter
      (fun (c_i, c_j) ->
        let resolvents = resolve c_i c_j in
        print_resol1 print_proof c_i c_j resolvents;
        if ClauseSet.mem LiteralSet.empty resolvents then (
          print_contradiction print_proof;
          raise (Done true))
        else new_clauses := ClauseSet.union !new_clauses resolvents)
      pairs;
    if ClauseSet.subset !new_clauses clauses then (
      print_no_progress print_proof;
      raise (Done false))
    else (
      print_continue print_proof;
      loop (ClauseSet.union clauses !new_clauses))
  in
  loop og_clauses

let resolution kb alpha p_p = try resolution2_aux kb alpha p_p with Done b -> b
