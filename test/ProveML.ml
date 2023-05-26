open Prover.Prop
open OUnit2

let string_of_prop_test (name : string) (p : prop) (expected : string) =
  name >:: fun _ -> assert_equal expected (string_of_prop p)

let base_of_test (name : string) (p : prop) (expected : prop) =
  name >:: fun _ -> assert_equal expected (base_of p)

let a = Atom "A"
let b = Atom "B"
let c = Atom "C"
let d = Atom "D"
let a_or_b = Or (a, b)
let c_and_not_d = And (c, Not d)
let complicated = Iff (a_or_b, c_and_not_d)

let prop_tests =
  [
    (*string of proposition*)
    string_of_prop_test "Atom" a "A";
    string_of_prop_test "Not" (Not b) "~B";
    string_of_prop_test "Or" a_or_b "(A V B)";
    string_of_prop_test "And" c_and_not_d "(C ^ ~D)";
    string_of_prop_test "Implies" (Imp (a, b)) "(A => B)";
    string_of_prop_test "Iff" complicated "((A V B) <=> (C ^ ~D))";
    (*Base of test*)
    base_of_test "Atom" a a;
  ]

let cnf_tests = []
let suite = "Test suite for Pac-Man" >::: List.flatten [ prop_tests; cnf_tests ]
let _ = run_test_tt_main suite
