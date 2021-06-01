open Exprc
open Core

type value = 
  | NumV of Int.t 
  | BoolV of Bool.t 
  | ClosV of exprC * environment 
and binding = Bind of string * value
and environment = binding list

let sexp_of_value t =
  match t with
  | NumV i -> sexp_of_int i
  | BoolV b -> sexp_of_bool b
  | ClosV (e, _) -> sexp_of_exprC e 