open Prop
open Cnf

val print_proof_start : bool -> prop -> prop -> cnf -> unit
val print_loop1 : bool -> cnf -> unit
val print_loop_empty : bool -> unit
val print_resolving : bool -> clause -> clause -> unit
val print_resolved : bool -> cnf -> unit
val print_resol1 : bool -> clause -> clause -> cnf -> unit
val print_resolvents : bool -> cnf -> unit
val print_contradiction : bool -> unit
val print_continue : bool -> unit
val print_no_progress : bool -> unit
