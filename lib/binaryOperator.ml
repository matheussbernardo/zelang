open Core
open Sexp

type binaryOperator = OpPlus | OpMult | OpMinus | OpNumEq | OpAnd
[@@deriving show]

let binaryOperator_of_sexp s =
  match s with
  | Atom "+" -> OpPlus
  | Atom "*" -> OpMult
  | Atom "-" -> OpMinus
  | Atom "==" -> OpNumEq
  | Atom "and" -> OpAnd
  | _ -> of_sexp_error "Not a Binary Operator" s

let sexp_of_binaryOperator t =
  match t with
  | OpPlus -> Atom "+"
  | OpMult -> Atom "*"
  | OpMinus -> Atom "-"
  | OpNumEq -> Atom "=="
  | OpAnd -> Atom "and"
