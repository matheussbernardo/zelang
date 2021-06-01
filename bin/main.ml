open Core
open Sexp
open Zelang
(*  
    (let (double (lam x (+ x x))) (double 10))
    ( (lam (double) (double 10)) (lam (x) (+ x x)) )
 *)
let () = begin
  let parsed = In_channel.stdin |> input_sexp |> Exprs.exprS_of_sexp  in
  parsed |> Exprs.sexp_of_exprS |> Out_channel.print_s ;

  let desugared =  parsed |> Desugar.desugar in
  desugared |> Exprc.sexp_of_exprC |> Out_channel.print_s;

  Interp.interp desugared [] |> Environment.sexp_of_value |> Out_channel.print_s

  (* ((def x (x x)) (def x (x x))) *)
  (* let il = AppC(
    FunDC("double", AppC(IdC("double"), NumC 10)), 
    FunDC("x", BinaryOpC(BinaryOperator.OpPlus, IdC("x"), IdC("x")))) in *)
  (* let interpreted = Interp.interp  il [] in *)
end

