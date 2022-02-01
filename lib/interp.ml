open Core
open Exprc
open Environment

type result = { value : value; store : store }

let rec interp e env store =
  match e with
  | TrueC -> { value = BoolV true; store }
  | FalseC -> { value = BoolV false; store }
  | NumC i -> { value = NumV i; store }
  | BinaryOpC (op, l, r) -> interpBinaryOpC op l r env store
  | UnaryOpC (op, expr) -> interpUnaryOpC op expr env store
  | IfC (cond, t, e) -> interpIfC cond t e env store
  | FunDC (_, _) -> { value = ClosV (e, env); store }
  | AppC (f, a) -> interpAppC f a env store
  | IdC s -> interpIdC s env store
  | SeqC (b1, b2) -> interpSeqC b1 b2 env store
  | BoxC e -> interpBoxC e env store
  | UnboxC e -> interpUnboxC e env store
  | SetboxC (b, v) -> interpSetboxC b v env store

and interpAppC f a env store =
  let { value = f_value; store = f_store } = interp f env store in
  match f_value with
  | ClosV (FunDC (clos_arg, body), clos_nv) ->
      let { value = a_value; store = a_store } = interp a env f_store in
      let loc = new_loc () in
      interp body
        (Bind (clos_arg, loc) :: clos_nv)
        (Cell (loc, a_value) :: a_store)
  | _ -> failwith "applying to a expression that is not a function"

and lookup s nv =
  match nv with
  | [] -> failwith "could not find in the env"
  | hd :: tl ->
      let (Bind (str, value)) = hd in
      if equal_string s str then value else lookup s tl

and fetch loc sto =
  match sto with
  | [] -> failwith "could not find in the store"
  | hd :: tl ->
      let (Cell (l, value)) = hd in
      if equal_int loc l then value else fetch loc tl

and interpBoxC e env store =
  let { value; store = new_store } = interp e env store in
  let loc = Environment.new_loc () in
  { value = BoxV loc; store = Cell (loc, value) :: new_store }

and interpUnboxC e env store =
  let { value; store = new_store } = interp e env store in
  match value with
  | BoxV v -> { value = fetch v new_store; store = new_store }
  | _ -> failwith "not a box"

and interpSetboxC b v env store =
  let { value = b_value; store = b_store } = interp b env store in
  let { value = v_value; store = v_store } = interp v env b_store in
  match b_value with
  | BoxV loc ->
      { value = v_value; store = update_store (Cell (loc, v_value)) v_store }
  | _ -> failwith "not a box"

and interpIdC s env store =
  let found_location = lookup s env in
  let found_value = fetch found_location store in
  { value = found_value; store }

and interpSeqC b1 b2 env store =
  let { value = _; store = new_store } = interp b1 env store in
  interp b2 env new_store

and interpBinaryOpC op l r env store =
  match op with
  | OpPlus -> arith_binop (fun x y -> x + y) l r env store
  | OpMult -> arith_binop (fun x y -> x * y) l r env store
  | OpNumEq -> arith_bool_binop (fun x y -> equal_int x y) l r env store
  | _ -> failwith "operation not found"

and arith_binop op l r env store =
  let { value = leftV; store = l_store } = interp l env store in
  let { value = rightV; store = r_store } = interp r env l_store in
  match (leftV, rightV) with
  | NumV l, NumV r -> { value = NumV (op l r); store = r_store }
  | _ -> failwith "argument is not a number"

and arith_bool_binop op l r env store =
  let { value = leftV; store = l_store } = interp l env store in
  let { value = rightV; store = r_store } = interp r env l_store in
  match (leftV, rightV) with
  | NumV l, NumV r -> { value = BoolV (op l r); store = r_store }
  | _ -> failwith "argument is not a number"

and interpUnaryOpC op expr env store =
  match op with
  | OpNotBool -> bool_unaop (fun x -> not x) expr env store
  | _ -> failwith "operation not found"

and bool_unaop op expr nv store =
  let { value; store = new_store } = interp expr nv store in
  match value with
  | BoolV b -> { value = BoolV (op b); store = new_store }
  | _ -> failwith "argument is not a boolean"

and interpIfC cond t e env store =
  let { value = new_value; store = new_store } = interp cond env store in
  match new_value with
  | BoolV true -> interp t env new_store
  | BoolV false -> interp e env new_store
  | _ -> failwith "if did not receive a boolean"
