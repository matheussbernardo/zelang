open BinaryOperator
open UnaryOperator
open Core
open Sexp

type exprC =
  | TrueC
  | FalseC
  | NumC of Int.t
  | BinaryOpC of binaryOperator * exprC * exprC
  | UnaryOpC of unaryOperator * exprC
  | IfC of exprC * exprC * exprC
  | IdC of string
  | FunDC of string * exprC
  | AppC of exprC * exprC
  | BoxC of exprC
  | UnboxC of exprC
  | SetboxC of exprC * exprC
  | SeqC of exprC * exprC
[@@deriving show]

let rec sexp_of_exprC t =
  match t with
  | TrueC -> Atom "true"
  | FalseC -> Atom "false"
  | NumC n -> Atom (string_of_int n)
  | BinaryOpC (op, l, r) ->
      List [ sexp_of_binaryOperator op; sexp_of_exprC l; sexp_of_exprC r ]
  | UnaryOpC (op, l) -> List [ sexp_of_unaryOperator op; sexp_of_exprC l ]
  | IfC (cond, t, e) ->
      List [ Atom "if"; sexp_of_exprC cond; sexp_of_exprC t; sexp_of_exprC e ]
  | IdC i -> Atom i
  | FunDC (arg, e) -> List [ Atom "lam"; Atom arg; sexp_of_exprC e ]
  | AppC (fundcExpr, expr) ->
      List [ sexp_of_exprC fundcExpr; sexp_of_exprC expr ]
  | BoxC e -> List [ Atom "box"; sexp_of_exprC e ]
  | UnboxC e -> List [ Atom "unbox"; sexp_of_exprC e ]
  | SetboxC (b, v) -> List [ Atom "set-box!"; sexp_of_exprC b; sexp_of_exprC v ]
  | SeqC (b1, b2) -> List [ Atom "begin"; sexp_of_exprC b1; sexp_of_exprC b2 ]
