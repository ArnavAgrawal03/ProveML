(* Start AST *)

(**A proposition in propositional logic*)
type prop =
  | Atom of string
  | Or of prop * prop
  | And of prop * prop
  | Not of prop
  | Imp of prop * prop
  | Iff of prop * prop
  | True
  | False

val base_of : prop -> prop
(** [base_of p] is a proposition [q] that is equivalent to [p], but only contains and, or,
    and not*)

val string_of_prop : prop -> string
(** [string_of_prop p] is a string representation of [p] *)
