open Core
open Sexp

type value = NumV of Int.t | BoolV of Bool.t [@@deriving sexp]

type binaryOperator = OpPlus | OpMult | OpMinus | OpNumEq | OpAnd

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

type unaryOperator = OpNotBool | OpMinusU

let unaryOperator_of_sexp s =
  match s with
  | Atom "not" -> OpNotBool
  | Atom "-" -> OpMinusU
  | _ -> of_sexp_error "Not a Unary Operator" s

let sexp_of_unaryOperator t =
  match t with OpNotBool -> Atom "not" | OpMinusU -> Atom "-"

type exprC =
  | TrueC
  | FalseC
  | NumC of Int.t
  | BinaryOpC of binaryOperator * exprC * exprC
  | UnaryOpC of unaryOperator * exprC
  | IfC of exprC * exprC * exprC

type exprS =
  | TrueS
  | FalseS
  | NumS of Int.t
  | BinaryOpS of binaryOperator * exprS * exprS
  | UnaryOpS of unaryOperator * exprS
  | IfS of exprS * exprS * exprS

let rec exprS_of_sexp s =
  match s with
  | Atom "true" -> TrueS
  | Atom "false" -> FalseS
  | Atom str when str |> int_of_string_opt |> is_some ->
      NumS (int_of_string str)
  | Atom _ -> of_sexp_error "Unexpected atom" s
  | List [ Atom "if"; cond; t; e ] ->
      IfS (exprS_of_sexp cond, exprS_of_sexp t, exprS_of_sexp e)
  | List [ op; l; r ] ->
      BinaryOpS (binaryOperator_of_sexp op, exprS_of_sexp l, exprS_of_sexp r)
  | List [ op; l ] -> UnaryOpS (unaryOperator_of_sexp op, exprS_of_sexp l)
  | List _ -> of_sexp_error "Unexpected list of atoms" s

let rec sexp_of_exprS t =
  match t with
  | TrueS -> Atom "true"
  | FalseS -> Atom "false"
  | NumS n -> Atom (string_of_int n)
  | BinaryOpS (op, l, r) ->
      List [ sexp_of_binaryOperator op; sexp_of_exprS l; sexp_of_exprS r ]
  | UnaryOpS (op, l) -> List [ sexp_of_unaryOperator op; sexp_of_exprS l ]
  | IfS (cond, t, e) ->
      List [ Atom "if"; sexp_of_exprS cond; sexp_of_exprS t; sexp_of_exprS e ]

let rec desugar s =
  match s with
  | TrueS -> TrueC
  | FalseS -> FalseC
  | NumS n -> NumC n
  | BinaryOpS (OpMinus, l, r) ->
      BinaryOpC (OpMinus, desugar l, BinaryOpC (OpMult, NumC (-1), desugar r))
  | BinaryOpS (OpAnd, l, r) ->
      IfC (desugar l, IfC (desugar r, TrueC, FalseC), FalseC)
  | BinaryOpS (op, l, r) -> BinaryOpC (op, desugar l, desugar r)
  | UnaryOpS (OpMinusU, l) -> BinaryOpC (OpMult, NumC (-1), desugar l)
  | UnaryOpS (op, l) -> UnaryOpC (op, desugar l)
  | IfS (cond, t, e) -> IfC (desugar cond, desugar t, desugar e)

let rec interp e =
  match e with
  | TrueC -> BoolV true
  | FalseC -> BoolV false
  | NumC i -> NumV i
  | BinaryOpC (op, l, r) -> (
      match op with
      | OpPlus -> arith_binop (fun x y -> x + y) l r
      | OpMult -> arith_binop (fun x y -> x * y) l r
      | OpNumEq -> arith_bool_binop (fun x y -> phys_equal x y) l r
      | _ -> failwith "bug!")
  | UnaryOpC (op, expr) -> (
      match op with
      | OpNotBool -> bool_unaop (fun x -> not x) expr
      | _ -> failwith "bug!")
  | IfC (cond, t, e) -> (
      match interp cond with
      | BoolV true -> interp t
      | BoolV false -> interp e
      | _ -> failwith "argument is not a boolean")

and arith_binop op l r =
  let leftV = interp l in
  let rightV = interp r in
  match (leftV, rightV) with
  | NumV l, NumV r -> NumV (op l r)
  | _ -> failwith "argument is not a number"

and arith_bool_binop op l r =
  let leftV = interp l in
  let rightV = interp r in
  match (leftV, rightV) with
  | NumV l, NumV r -> BoolV (op l r)
  | _ -> failwith "argument is not a number"

and bool_unaop op expr =
  match interp expr with
  | BoolV b -> BoolV (op b)
  | _ -> failwith "argument is not a boolean"

let () =
  In_channel.stdin |> input_sexp |> exprS_of_sexp |> desugar |> interp
  |> sexp_of_value |> Out_channel.print_s
