open Core
open Sexp

type unaryOperator = OpNotBool | OpMinusU [@@deriving show]

let unaryOperator_of_sexp s =
  match s with
  | Atom "not" -> OpNotBool
  | Atom "-" -> OpMinusU
  | _ -> of_sexp_error "Not a Unary Operator" s

let sexp_of_unaryOperator t =
  match t with OpNotBool -> Atom "not" | OpMinusU -> Atom "-"
