open BinaryOperator
open UnaryOperator
open Core
open Sexp

type exprS =
  | TrueS
  | FalseS
  | NumS of Int.t
  | BinaryOpS of binaryOperator * exprS * exprS
  | UnaryOpS of unaryOperator * exprS
  | IfS of exprS * exprS * exprS
  | IdS of string
  | LetS of string * exprS * exprS
  | FunDS of string * exprS
  | AppS of exprS * exprS
  | BeginS of exprS list
[@@deriving show]

let rec exprS_of_sexp s =
  match s with
  | Atom "true" -> TrueS
  | Atom "false" -> FalseS
  | Atom str when str |> int_of_string_opt |> is_some ->
      NumS (int_of_string str)
  | Atom s -> IdS s
  | List [ Atom "if"; cond; t; e ] ->
      IfS (exprS_of_sexp cond, exprS_of_sexp t, exprS_of_sexp e)
  | List [ Atom "lam"; Atom arg; e ] -> FunDS (arg, exprS_of_sexp e)
  | List [ Atom "let"; List [ Atom id; body ]; e ] ->
      LetS (id, exprS_of_sexp body, exprS_of_sexp e)
  | List [ Atom "begin"; tl ] -> BeginS (list_of_sexp exprS_of_sexp tl)
  | List [ op; l; r ] ->
      BinaryOpS (binaryOperator_of_sexp op, exprS_of_sexp l, exprS_of_sexp r)
  | List [ op; l ] when equal op (Atom "not") || equal op (Atom "-") ->
      UnaryOpS (unaryOperator_of_sexp op, exprS_of_sexp l)
  | List [ ap; tl ] -> AppS (exprS_of_sexp ap, exprS_of_sexp tl)
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
  | LetS (id, body, e) ->
      List [ Atom "let"; List [ Atom id; sexp_of_exprS body ]; sexp_of_exprS e ]
  | FunDS (arg, e) -> List [ Atom "lam"; Atom arg; sexp_of_exprS e ]
  | AppS (ap, expr) -> List [ sexp_of_exprS ap; sexp_of_exprS expr ]
  | BeginS list -> sexp_of_list sexp_of_exprS list