open Prop
(*helper/shielded functions specified here. All others sepcified in the interface file*)

type literal =
  | Pos of string
  | Neg of string

type nnf =
  | N_True
  | N_False
  | Lit of literal
  | N_And of nnf * nnf
  | N_Or of nnf * nnf

type clause = literal list
type cnf = clause list

let rec nnf_of_prop (p : prop) : nnf =
  match p with
  | True -> N_True
  | Not True -> N_False
  | False -> N_False
  | Not False -> N_True
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
  | N_True -> N_True
  | N_False -> N_False
  | Lit a -> Lit a
  | N_Or (a, b) -> distribute (distributed_nnf a) (distributed_nnf b)
  | N_And (a, b) -> N_And (distributed_nnf a, distributed_nnf b)

(** Given a set of nested ands, [n], [list_of_ands n] flattens [n] to a list of nnfs as a
    disjunction. Helper function to convert from nnf to cnf. Requires: [n] is already
    distributed in terms of [Or]. The resulting value does not contain any [And]
    constructors*)
let rec list_of_ands (n : nnf) =
  match n with N_And (a, b) -> list_of_ands a @ list_of_ands b | _ -> [ n ]

(** Given a set of nested ors, [n], [clause_of_ors n] flatten [n] to a list of literals as
    a conjunction. Requires: [n] doesn't contain any ands. *)
let rec clause_of_ors (n : nnf) =
  match n with
  | Lit a -> [ a ]
  | N_Or (a, b) -> clause_of_ors a @ clause_of_ors b
  | _ -> []

(** [cnf_of_distr n] is the cnf version of [n]. Requires: nnf already has distrubuted
    form.*)
let cnf_of_distr (n : nnf) : cnf =
  match n with
  | N_True -> []
  | N_False -> [ [] ]
  | _ -> list_of_ands n |> List.map clause_of_ors

let cnf_of_prop p = p |> nnf_of_prop |> distributed_nnf |> cnf_of_distr

let rec string_of_nnf (n : nnf) : string =
  match n with
  | N_True -> "true"
  | N_False -> "false"
  | Lit (Pos a) -> a
  | Lit (Neg a) -> "~" ^ a
  | N_And (a, b) -> "(" ^ string_of_nnf a ^ " ^ " ^ string_of_nnf b ^ ")"
  | N_Or (a, b) -> "(" ^ string_of_nnf a ^ " V " ^ string_of_nnf b ^ ")"

let string_of_literal l = match l with Pos a -> a | Neg a -> "~" ^ a

let string_of_list_aux converter joiner lst =
  "(" ^ (lst |> List.map converter |> String.concat joiner) ^ ")"

let string_of_clause = string_of_list_aux string_of_literal " V "
let string_of_cnf = string_of_list_aux string_of_clause " ^ "
