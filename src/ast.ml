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

type clause = literal list
type cnf = clause list

let rec convert_to_base (p : prop) =
  match p with
  | Atom _ | True | False -> p
  | Or (a, b) -> Or (convert_to_base a, convert_to_base b)
  | And (a, b) -> And (convert_to_base a, convert_to_base b)
  | Not a -> Not (convert_to_base a)
  | Imp (a, b) -> Or (Not (convert_to_base a), convert_to_base b)
  | Iff (a, b) -> convert_to_base (And (Imp (a, b), Imp (b, a)))

let rec convert_to_nnf (p : prop) =
  match p with
  | Atom _ | True | False | Not (Atom _) -> p
  | Not True -> False
  | Not False -> True
  | Not (Not a) -> convert_to_nnf a
  | Not (Or (a, b)) -> convert_to_nnf (And (Not a, Not b))
  | Not (And (a, b)) -> convert_to_nnf (Or (Not a, Not b))
  | Not (Imp (a, b)) -> convert_to_nnf (And (a, Not b))
  | Not (Iff (a, b)) -> convert_to_nnf (Or (And (a, Not b), And (Not a, b)))
  | And (a, b) -> And (convert_to_nnf a, convert_to_nnf b)
  | Or (a, b) -> Or (convert_to_nnf a, convert_to_nnf b)
  | Imp (a, b) -> convert_to_nnf (Or (Not a, b))
  | Iff (a, b) -> convert_to_nnf (And (Imp (a, b), Imp (b, a)))
