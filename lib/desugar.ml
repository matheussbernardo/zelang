open Exprs
open Exprc

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
  | LetS (id, e, body) -> AppC (FunDC (id, desugar body), desugar e)
  | FunDS (str, expr) -> FunDC (str, desugar expr)
  | AppS (ap, expr) -> AppC (desugar ap, desugar expr)
  | BeginS list -> desugarBeginS list

and desugarBeginS list =
  match list with
  | [] -> failwith "begin need at least 2 expressions"
  | [ b1; b2 ] -> SeqC (desugar b1, desugar b2)
  | hd :: tl -> SeqC (desugar hd, desugarBeginS tl)
