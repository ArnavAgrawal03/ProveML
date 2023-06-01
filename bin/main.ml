open Prover.Resolution
open Prover.Prop

let college_town = Atom "College is left of town"
let town_bagel = Atom "Town is left\n   of bagel"
let college_bagel = Atom "College is left of bagel"
let transitivity = college_town &&& town_bagel => college_bagel
let _ = resolution (college_town &&& town_bagel &&& transitivity) college_bagel true
