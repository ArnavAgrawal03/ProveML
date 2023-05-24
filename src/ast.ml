(* Start AST *)

type bop =
  | Or
  | Add
  | Impl
  | Equiv

type uop = Not
type value = bool

type prop =
  | Prop of value
  | Binop of bop * prop * prop
  | Unop of uop * prop
