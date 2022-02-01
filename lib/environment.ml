open Exprc
open Core

type value =
  | NumV of Int.t
  | BoolV of Bool.t
  | ClosV of exprC * environment
  | BoxV of Int.t

and binding = Bind of string * Int.t
and environment = binding list
and storage = Cell of Int.t * value
and store = storage list

let update_store (Cell (update_loc, update_value)) store =
  List.fold store ~init:[] ~f:(fun acc (Cell (current_loc, current_value)) ->
      if equal_int update_loc current_loc then
        Cell (current_loc, update_value) :: acc
      else Cell (current_loc, current_value) :: acc)

let sexp_of_value t =
  match t with
  | NumV i -> sexp_of_int i
  | BoolV b -> sexp_of_bool b
  | ClosV (e, _) -> sexp_of_exprC e
  | BoxV v -> sexp_of_int v

let new_loc =
  let n : int ref = ref 0 in
  fun () ->
    n := !n + 1;
    !n
