open Prop
open Cnf

(** [compare_literals a b] compares [a] and [b] for sorting. It uses these rules: 1. Put
    positive literals first, then negative literals. 2. If two literals share "sign" then
    order them by string*)
let compare_literals (a : literal) (b : literal) =
  match (a, b) with
  | Pos a, Pos b -> compare a b
  | Neg a, Neg b -> compare a b
  | Pos _, Neg _ -> -1
  | Neg _, Pos _ -> 1

(** [compare_clauses a b] compares [a] and [b] for sorting. It orders clauses by length,
    then by literals in them, where literals are compared using [compare_literals] *)
let rec compare_clauses (a : clause) (b : clause) =
  let a_len = List.length a in
  let b_len = List.length b in
  match compare a_len b_len with
  | x when x <> 0 -> x
  | 0 when a = [] -> 0
  | 0 when compare_literals (List.hd a) (List.hd b) < 0 -> -1
  | 0 when compare_literals (List.hd a) (List.hd b) > 0 -> 1
  | _ -> compare_clauses (List.tl a) (List.tl b)

let simplify_cnf (c : cnf) =
  c |> List.map (List.sort_uniq compare_literals) |> List.sort_uniq compare_clauses

(** [combine c1 c2] combines two clauses, removing duplicates and sorting*)
let combine c1 c2 = List.sort_uniq compare_literals (c1 @ c2)

let negate_literal = function Neg a -> Pos a | Pos a -> Neg a

(** [resolve_aux l c1 c2] *)
let resolve_aux (l : literal) (c1 : clause) (c2 : clause) =
  let negated_l = negate_literal l in
  let c1' = List.filter (( <> ) l) c1 in
  let c2' = List.filter (( <> ) negated_l) c2 in
  combine c1' c2'

(** [resolve c1 c2] resolves two clauses, returning a list of clauses that are the result
    of resolving [c1] and [c2] *)
let resolve (c1 : clause) (c2 : clause) = ()

(** [resolve_all c] resolves all clauses in [c] with each other, returning a list of
    clauses that are the result of resolving all clauses in [c] *)
