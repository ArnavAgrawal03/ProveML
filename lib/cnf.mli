open Prop

(** A literal in propositional logic*)
type literal =
  | Pos of string
  | Neg of string

(** Represents the Negation Normal form of a statement in propositional logic*)
type nnf =
  | N_True
  | N_False
  | Lit of literal
  | N_And of nnf * nnf
  | N_Or of nnf * nnf

type clause = literal list
(** A clause disjunction of literal*)

type cnf = clause list
(** The Conjunctive Normal form of a statement in propositional logic. A conjunction of
    clauses.*)

val nnf_of_prop : prop -> nnf
(** [nnf_of_prop p] is the Negation Normal form of [p]*)

val cnf_of_prop : prop -> cnf
(** [cnf_of_prop p] is the Conjunctive Normal Form of [p]*)

val string_of_nnf : nnf -> string
(** [string_of_nnf n] is the string representing [n]*)

val string_of_cnf : cnf -> string
(** [string_of_cnf c] is the string representing [c]*)
