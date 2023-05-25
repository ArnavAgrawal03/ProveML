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

let rec convert_to_cnf (p : prop) =
  match p with
  | Atom a -> [ [ Pos a ] ]
  | _ -> failwith "Not implemented"
