open Prop

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

val nnf_of_prop : prop -> nnf
val distribute : nnf -> nnf -> nnf
val distributed_nnf : nnf -> nnf
val list_of_ors : nnf -> nnf list
val clause_of_ors : nnf -> literal list
val cnf_of_distr : nnf -> cnf
val cnf_of_prop : prop -> cnf
val string_of_nnf : nnf -> string
