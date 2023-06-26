open Prop
(*helper/shielded functions specified here. All others sepcified in the interface file*)

type literal =
  | Pos of string
  | Neg of string

let compare_literals (a : literal) (b : literal) =
  match (a, b) with
  | Pos a, Pos b -> compare a b
  | Neg a, Neg b -> compare a b
  | Pos _, Neg _ -> -1
  | Neg _, Pos _ -> 1

module OrderedLiteral = struct
  type t = literal

  let compare = compare_literals
end

module LiteralSet = Set.Make (OrderedLiteral)

type clause = LiteralSet.t

module OrderedClause = struct
  type t = clause

  let compare = LiteralSet.compare
end

module ClauseSet = Set.Make (OrderedClause)

type cnf = ClauseSet.t

type nnf =
  | Lit of literal
  | N_And of nnf * nnf
  | N_Or of nnf * nnf

let rec nnf_of_prop (p : prop) : nnf =
  match p with
  | Atom a -> Lit (Pos a)
  | Not (Atom a) -> Lit (Neg a)
  | Not (Not a) -> nnf_of_prop a
  | Not (Or (a, b)) -> nnf_of_prop (And (Not a, Not b))
  | Not (And (a, b)) -> nnf_of_prop (Or (Not a, Not b))
  | Not (Imp (a, b)) -> nnf_of_prop (And (a, Not b))
  | Not (Iff (a, b)) -> nnf_of_prop (Or (And (a, Not b), And (Not a, b)))
  | And (a, b) -> N_And (nnf_of_prop a, nnf_of_prop b)
  | Or (a, b) -> N_Or (nnf_of_prop a, nnf_of_prop b)
  | Imp (a, b) -> nnf_of_prop (Or (Not a, b))
  | Iff (a, b) -> nnf_of_prop (And (Imp (a, b), Imp (b, a)))

(** [distribute a b] distributes the [Or] operator to both literals of an and operator*)
let rec distribute (a : nnf) (b : nnf) : nnf =
  match (a, b) with
  | N_And (a1, a2), b -> N_And (distribute a1 b, distribute a2 b)
  | a, N_And (b1, b2) -> N_And (distribute a b1, distribute a b2)
  | a, b -> N_Or (a, b)

(** Processes nnf such that [And] constructors are completely on the outside. Resulting
    nnf matches the precondition for [cnf_of_distr]*)
let rec distributed_nnf (n : nnf) : nnf =
  match n with
  | Lit a -> Lit a
  | N_Or (a, b) -> distribute (distributed_nnf a) (distributed_nnf b)
  | N_And (a, b) -> N_And (distributed_nnf a, distributed_nnf b)

(** Given a set of nested ors, [n], [clause_of_ors n] flatten [n] to a list of literals as
    a conjunction. Requires: [n] doesn't contain any ands. *)
let rec clause_of_ors (n : nnf) =
  match n with
  | Lit a -> LiteralSet.singleton a
  | N_Or (a, b) -> LiteralSet.union (clause_of_ors a) (clause_of_ors b)
  | _ -> LiteralSet.empty

(** [cnf_of_distr n] is the cnf version of [n]. Requires: nnf already has distrubuted
    form.*)
let rec cnf_of_distr (n : nnf) : cnf =
  match n with
  | N_And (a, b) -> ClauseSet.union (cnf_of_distr a) (cnf_of_distr b)
  | _ -> ClauseSet.singleton (clause_of_ors n)

let cnf_of_prop p = p |> nnf_of_prop |> distributed_nnf |> cnf_of_distr

let rec string_of_nnf (n : nnf) : string =
  match n with
  | Lit (Pos a) -> a
  | Lit (Neg a) -> "~" ^ a
  | N_And (a, b) -> "(" ^ string_of_nnf a ^ " ^ " ^ string_of_nnf b ^ ")"
  | N_Or (a, b) -> "(" ^ string_of_nnf a ^ " V " ^ string_of_nnf b ^ ")"

let string_of_literal l = match l with Pos a -> a | Neg a -> "~" ^ a

let string_of_list_aux converter joiner lst =
  "(" ^ (lst |> List.map converter |> String.concat joiner) ^ ")"

let list_of_clause = LiteralSet.elements
let list_of_cnf (c : cnf) = ClauseSet.elements c |> List.map list_of_clause

let string_of_clause (c : clause) =
  string_of_list_aux string_of_literal " V " (LiteralSet.elements c)

let string_of_cnf (c : cnf) =
  string_of_list_aux string_of_clause " ^ " (ClauseSet.elements c)
