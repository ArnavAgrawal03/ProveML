open Prop
open Cnf

val resolve : clause -> clause -> cnf
(** [resolve c1 c2] resolves two clauses, returning a set of clauses that are the result
    of resolving [c1] and [c2] *)

val resolution : prop -> prop -> bool -> bool
(** [resolution kb alpha print_proof] is [true] if [alpha] is [true] given [kb]. [false]
    if [alpha] is [false] given [kb]. Prints out the steps of the proof if [print_proof]
    is [true]. Doesn't otherwise. *)

val resolution2 : prop -> prop -> bool
(** [resolution2 kb alpha] is [true] if [alpha] is [true] given [kb]. [false] if [alpha]
    is [false] given [kb]. *)
