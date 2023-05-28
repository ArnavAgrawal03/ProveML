open Prop
open Cnf

val resolve : clause -> clause -> cnf
(** [resolve c1 c2] resolves two clauses, returning a set of clauses that are the result
    of resolving [c1] and [c2] *)

val resolution : prop -> prop -> bool -> bool
(** [resolution kb alpha print_proof] is [Some true] if [alpha] is [true] given [kb].
    [Some false] if [alpha] is [false] given [kb]. [None] handles the case where we're
    unsure (shouldn't occur). Prints out the proof if print_proof is true. Doesn't
    otherwise. *)
