(* Start AST *)

type bop =
  | Or
  | Add
  | Impl
  | Equiv

type uop = Not
type prop = bool

type expr =
  | Prop of prop
  | Binop of bop * expr * expr
  | Unop of uop * expr
