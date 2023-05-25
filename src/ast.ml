(* Start AST *)

type prop =
  | Atom of string
  | Or of prop * prop
  | And of prop * prop
  | Not of prop
  | Imp of prop * prop
  | Iff of prop * prop
  | True
  | False

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

let rec base_of (p : prop) =
  match p with
  | Atom _ | True | False -> p
  | Or (a, b) -> Or (base_of a, base_of b)
  | And (a, b) -> And (base_of a, base_of b)
  | Not a -> Not (base_of a)
  | Imp (a, b) -> Or (Not (base_of a), base_of b)
  | Iff (a, b) -> base_of (And (Imp (a, b), Imp (b, a)))

let rec nnf_of_prop (p : prop) : nnf =
  match p with
  | True -> N_True
  | Not True -> N_False
  | False -> N_False
  | Not False -> N_True
  | Atom a -> Lit (Pos a)
  | Not (Atom a) -> Lit (Neg a)
  | Not (Not a) -> nnf_of_prop a
  | Not (Or (a, b)) -> nnf_of_prop (And (Not a, Not b))
  | Not (And (a, b)) -> nnf_of_prop (Or (Not a, Not b))
  | Not (Imp (a, b)) -> nnf_of_prop (And (a, Not b))
  | Not (Iff (a, b)) -> nnf_of_prop (Or (And (a, Not b), And (Not a, b)))
  | And (a, b) -> N_And (nnf_of_prop a, nnf_of_prop b)
  | Or (a, b) -> N_Or (nnf_of_prop a, nnf_of_prop b)
  | Imp (a, b) -> nnf_of_prop (Or (Not a, b))
  | Iff (a, b) -> nnf_of_prop (And (Imp (a, b), Imp (b, a)))

let rec distribute (a : nnf) (b : nnf) : nnf =
  match (a, b) with
  | N_And (a1, a2), b -> N_And (distribute a1 b, distribute a2 b)
  | a, N_And (b1, b2) -> N_And (distribute a b1, distribute a b2)
  | a, b -> N_Or (a, b)

let rec distributed_nnf (n : nnf) : nnf =
  match n with
  | N_True -> N_True
  | N_False -> N_False
  | Lit a -> Lit a
  | N_Or (a, b) -> distribute (distributed_nnf a) (distributed_nnf b)
  | N_And (a, b) -> N_And (distributed_nnf a, distributed_nnf b)

(* let rec cnf_of_nnf (n : nnf) : cnf = match distributed_nnf n with | N_True -> [] |
   N_False -> [[]] | Lit a -> [[a]] | N_And (a, b) -> cnf_of_nnf a @ cnf_of_nnf b | N_Or
   (a, b) -> *)

let rec string_of_prop (p : prop) : string =
  match p with
  | Atom a -> a
  | True -> "true"
  | False -> "false"
  | Or (a, b) -> "(" ^ string_of_prop a ^ " V " ^ string_of_prop b ^ ")"
  | And (a, b) -> "(" ^ string_of_prop a ^ " ^ " ^ string_of_prop b ^ ")"
  | Not a -> "~" ^ string_of_prop a
  | Imp (a, b) -> "(" ^ string_of_prop a ^ " => " ^ string_of_prop b ^ ")"
  | Iff (a, b) -> "(" ^ string_of_prop a ^ " <=> " ^ string_of_prop b ^ ")"

let rec string_of_nnf (n : nnf) : string =
  match n with
  | N_True -> "true"
  | N_False -> "false"
  | Lit (Pos a) -> a
  | Lit (Neg a) -> "~" ^ a
  | N_And (a, b) -> "(" ^ string_of_nnf a ^ " ^ " ^ string_of_nnf b ^ ")"
  | N_Or (a, b) -> "(" ^ string_of_nnf a ^ " V " ^ string_of_nnf b ^ ")"

let a = Atom "A"
let b = Atom "B"
let c = Atom "C"
let d = Atom "D"
let a_or_b = Or (a, b)
let c_and_not_d = And (c, Not d)
let complicated = Iff (a_or_b, c_and_not_d)
