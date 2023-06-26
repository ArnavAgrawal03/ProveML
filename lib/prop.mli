(* Start AST *)

(**A proposition in propositional logic*)
type prop =
  | Atom of string
  | Or of prop * prop
  | And of prop * prop
  | Not of prop
  | Imp of prop * prop
  | Iff of prop * prop
(* | True | False *)

(*-------- Infix operators for constructing -------*)
val ( => ) : prop -> prop -> prop
(** [p => q] is the proposition [p] implies [q] *)

val ( <=> ) : prop -> prop -> prop
(** [p <=> q] is the proposition [p] is equivalent to [q] *)

val ( &&& ) : prop -> prop -> prop
(** [p &&& q] is the proposition [p] and [q] *)

val ( || ) : prop -> prop -> prop
(** [p || q] is the proposition [p] or [q] *)

val ( ~~ ) : prop -> prop
(** [~~p] is the proposition not [p] *)

(*-------- Infix operators for constructing -------*)

val base_of : prop -> prop
(** [base_of p] is a proposition [q] that is equivalent to [p], but only contains and, or,
    and not*)

val string_of_prop : prop -> string
(** [string_of_prop p] is a string representation of [p] *)
