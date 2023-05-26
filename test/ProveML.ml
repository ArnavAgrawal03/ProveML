open Prover.Prop
open Prover.Cnf
open OUnit2

let string_of_prop_test (name : string) (p : prop) (expected : string) =
  "Testing Prop.string_of_prop: " ^ name >:: fun _ ->
  assert_equal expected (string_of_prop p)

let base_of_test (name : string) (p : prop) (expected : prop) =
  "Testing Prop.base_of: " ^ name >:: fun _ -> assert_equal expected (base_of p)

let nnf_of_prop_test (name : string) (p : prop) (expected : nnf) =
  "Testing Cnf.nnf_of_prop: " ^ name >:: fun _ -> assert_equal expected (nnf_of_prop p)

let cnf_of_prop_test (name : string) (p : prop) (expected : cnf) =
  "Testing Cnf.cnf_of_prop: " ^ name >:: fun _ -> assert_equal expected (cnf_of_prop p)

let a = Atom "A"
let b = Atom "B"
let c = Atom "C"
let d = Atom "D"
let a_or_b = Or (a, b)
let a_implies_b = Imp (a, b)
let not_a_or_b = Or (Not a, b)
let c_and_not_d = And (c, Not d)
let complicated = Iff (a_or_b, c_and_not_d)
let complicated_base = And (Or (Not a_or_b, c_and_not_d), Or (Not c_and_not_d, a_or_b))
let a_pos = Pos "A"
let a_neg = Neg "A"
let b_pos = Pos "B"
let b_neg = Neg "B"
let c_pos = Pos "C"
let c_neg = Neg "C"
let d_pos = Pos "D"
let d_neg = Neg "D"
let nnf_not_a_and_not_b = N_And (Lit a_neg, Lit b_neg)
let nnf_not_c_or_d = N_Or (Lit c_neg, Lit d_pos)
let b_or_c_or_not_d = Or (Or (b, c), Not d)
let cnf_b_or_c_or_not_d = [ [ b_pos; c_pos; d_neg ] ]

let prop_tests =
  [
    (*string of proposition*)
    string_of_prop_test "Atom" a "A";
    string_of_prop_test "Not" (Not b) "~B";
    string_of_prop_test "Or" a_or_b "(A V B)";
    string_of_prop_test "And" c_and_not_d "(C ^ ~D)";
    string_of_prop_test "Implies" a_implies_b "(A => B)";
    string_of_prop_test "Iff" complicated "((A V B) <=> (C ^ ~D))";
    (*Base of test*)
    base_of_test "Atom" a a;
    base_of_test "Simple Implication" a_implies_b not_a_or_b;
    base_of_test "Complicated Iff" complicated complicated_base;
  ]

let cnf_tests =
  [
    (*nnf of prop tests*)
    nnf_of_prop_test "Atom" a (Lit a_pos);
    nnf_of_prop_test "Simple Not" (Not a) (Lit a_neg);
    nnf_of_prop_test "Double Negation" (Not (Not a)) (Lit a_pos);
    nnf_of_prop_test "DeMorgan's on Or" (Not a_or_b) nnf_not_a_and_not_b;
    nnf_of_prop_test "DeMorgan's on And" (Not c_and_not_d) nnf_not_c_or_d;
    (*cnf of prop tests*)
    cnf_of_prop_test "Atom" b [ [ b_pos ] ];
    cnf_of_prop_test "Multiple Ors" b_or_c_or_not_d cnf_b_or_c_or_not_d;
  ]

let suite = "Test suite for ProveML" >::: List.flatten [ prop_tests; cnf_tests ]
let _ = run_test_tt_main suite
