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

let rec base_of (p : prop) =
  match p with
  | Atom _ | True | False -> p
  | Or (a, b) -> Or (base_of a, base_of b)
  | And (a, b) -> And (base_of a, base_of b)
  | Not a -> Not (base_of a)
  | Imp (a, b) -> Or (Not (base_of a), base_of b)
  | Iff (a, b) -> base_of (And (Imp (a, b), Imp (b, a)))

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
