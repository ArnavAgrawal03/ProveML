open Prop

(** A literal in propositional logic*)
type literal =
  | Pos of string
  | Neg of string

module LiteralSet : Set.S with type elt = literal
(** A set of literals*)

type clause = LiteralSet.t
(** A clause is a disjunction of literals*)

module ClauseSet : Set.S with type elt = clause
(** A set of clauses*)

type cnf = ClauseSet.t
(** The Conjunctive Normal form of a statement in propositional logic. A conjunction of
    clauses.*)

(** Represents the Negation Normal form of a statement in propositional logic*)
type nnf =
  | N_True
  | N_False
  | Lit of literal
  | N_And of nnf * nnf
  | N_Or of nnf * nnf

val compare_literals : literal -> literal -> int
(** [compare_literals a b] compares [a] and [b] for sorting. It uses these rules: 1. Put
    positive literals first, then negative literals. 2. If two literals share "sign" then
    order them by string*)

val compare_clauses : clause -> clause -> int
(** [compare_clauses a b] compares [a] and [b] for sorting. It orders clauses by length,
    then by literals in them, where literals are compared using [compare_literals] *)

val nnf_of_prop : prop -> nnf
(** [nnf_of_prop p] is the Negation Normal form of [p]*)

val cnf_of_prop : prop -> cnf
(** [cnf_of_prop p] is the Conjunctive Normal Form of [p]*)

val list_of_clause : clause -> literal list
(** [list_of_clause c] is the list of literals in [c]*)

val list_of_cnf : cnf -> literal list list
(** [list_of_cnf c] is a nested list of disjunctions (clauses) in c*)

val string_of_nnf : nnf -> string
(** [string_of_nnf n] is the string representing [n]*)

val string_of_cnf : cnf -> string
(** [string_of_cnf c] is the string representing [c]*)
