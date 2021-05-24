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
  | IdC of string
  | AppC of string * exprC

type funDefC = FunDC of string * string * exprC

type binding = Bind of string * value

type environment = binding list

type exprS =
  | TrueS
  | FalseS
  | NumS of Int.t
  | BinaryOpS of binaryOperator * exprS * exprS
  | UnaryOpS of unaryOperator * exprS
  | IfS of exprS * exprS * exprS
  | IdS of string
  | AppS of string * exprS

let rec exprS_of_sexp s =
  match s with
  | Atom "true" -> TrueS
  | Atom "false" -> FalseS
  | Atom str when str |> int_of_string_opt |> is_some ->
      NumS (int_of_string str)
  | Atom s -> IdS s
  | List [ Atom "if"; cond; t; e ] ->
      IfS (exprS_of_sexp cond, exprS_of_sexp t, exprS_of_sexp e)
  | List [ op; l; r ] ->
      BinaryOpS (binaryOperator_of_sexp op, exprS_of_sexp l, exprS_of_sexp r)
  | List [ op; l ] when equal op (Atom "not") || equal op (Atom "-")
    ->
      UnaryOpS (unaryOperator_of_sexp op, exprS_of_sexp l)
  | List [ Atom str; tl ] -> AppS (str, exprS_of_sexp tl)
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
  | IdS i -> Atom i
  | AppS (str, expr) -> List [ Atom str; sexp_of_exprS expr ]

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
  | IdS str -> IdC str
  | AppS (str, expr) -> AppC (str, desugar expr)

let rec interp e nv fds =
  match e with
  | TrueC -> BoolV true
  | FalseC -> BoolV false
  | NumC i -> NumV i
  | BinaryOpC (op, l, r) -> (
      match op with
      | OpPlus -> arith_binop (fun x y -> x + y) l r nv fds
      | OpMult -> arith_binop (fun x y -> x * y) l r nv fds
      | OpNumEq -> arith_bool_binop (fun x y -> equal_int x y) l r nv fds
      | _ -> failwith "bug!")
  | UnaryOpC (op, expr) -> (
      match op with
      | OpNotBool -> bool_unaop (fun x -> not x) expr nv fds
      | _ -> failwith "bug!")
  | IfC (cond, t, e) -> (
      match interp cond nv fds with
      | BoolV true -> interp t nv fds
      | BoolV false -> interp e nv fds
      | _ -> failwith "argument is not a boolean")
  | AppC (f, a) ->
      let arg_v = interp a nv fds in
      let (FunDC (_, arg, body)) = get_fundef f fds in
      interp body (Bind (arg, arg_v) :: nv) fds
  | IdC s -> lookup s nv

and lookup s nv =
  match nv with
  | [] -> failwith "could not find a function"
  | hd :: tl ->
      let (Bind (str, value)) = hd in
      if equal_string s str then value else lookup s tl

and arith_binop op l r nv fds =
  let leftV = interp l nv fds in
  let rightV = interp r nv fds in
  match (leftV, rightV) with
  | NumV l, NumV r -> NumV (op l r)
  | _ -> failwith "argument is not a number"

and arith_bool_binop op l r nv fds =
  let leftV = interp l nv fds in
  let rightV = interp r nv fds in
  match (leftV, rightV) with
  | NumV l, NumV r -> BoolV (op l r)
  | _ -> failwith "argument is not a number"

and bool_unaop op expr nv fds =
  match interp expr nv fds with
  | BoolV b -> BoolV (op b)
  | _ -> failwith "argument is not a boolean"

and get_fundef name fds =
  match fds with
  | [] -> failwith "could not find a function"
  | hd :: tl ->
      let (FunDC (n, _, _)) = hd in
      if equal_string n name then hd else get_fundef name tl

let f1 = FunDC ("double", "x", BinaryOpC (OpPlus, IdC "x", IdC "x"))

let f2 = FunDC ("quad", "x", AppC ("double", AppC ("double", IdC "x")))

let f3 = FunDC ("const5", "_", NumC 5)

(* (def f4 (x) (if x 1 0))*)
let funs = [ f1; f2; f3 ]

let () =
  let desugared = In_channel.stdin |> input_sexp |> exprS_of_sexp |> desugar in
  let interpreted = interp desugared [] funs in
  interpreted |> sexp_of_value |> Out_channel.print_s
