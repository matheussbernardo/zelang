open Core
open Exprc
open Environment

let rec interp e nv =
  match e with
  | TrueC -> BoolV true
  | FalseC -> BoolV false
  | NumC i -> NumV i
  | BinaryOpC (op, l, r) -> (
      match op with
      | OpPlus -> arith_binop (fun x y -> x + y) l r nv 
      | OpMult -> arith_binop (fun x y -> x * y) l r nv 
      | OpNumEq -> arith_bool_binop (fun x y -> equal_int x y) l r nv
      | _ -> failwith "bug!")
  | UnaryOpC (op, expr) -> (
      match op with
      | OpNotBool -> bool_unaop (fun x -> not x) expr nv 
      | _ -> failwith "bug!")
  | IfC (cond, t, e) -> (
      match interp cond nv  with
      | BoolV true -> interp t nv 
      | BoolV false -> interp e nv 
      | _ -> failwith "argument is not a boolean")
  | FunDC ( _, _) -> ClosV (e, nv)
  | AppC (f, a) -> application f a nv
  | IdC s -> lookup s nv
and application f a nv=
  let arg_v = interp a nv in
  match interp f nv with
  | ClosV (FunDC(clos_arg, body), clos_nv) -> interp body (Bind(clos_arg, arg_v) :: clos_nv)
  | _ -> failwith "applying to a expression that is not a function"

and lookup s nv =
  match nv with
  | [] -> failwith "could not find a function"
  | hd :: tl ->
      let (Bind (str, value)) = hd in
      if equal_string s str then value else lookup s tl

and arith_binop op l r nv  =
  let leftV = interp l nv  in
  let rightV = interp r nv  in
  match (leftV, rightV) with
  | NumV l, NumV r -> NumV (op l r)
  | _ -> failwith "argument is not a number"

and arith_bool_binop op l r nv  =
  let leftV = interp l nv in
  let rightV = interp r nv in
  match (leftV, rightV) with
  | NumV l, NumV r -> BoolV (op l r)
  | _ -> failwith "argument is not a number"

and bool_unaop op expr nv =
  match interp expr nv with
  | BoolV b -> BoolV (op b)
  | _ -> failwith "argument is not a boolean"
