open Core
open Zelang

let eval_line line =
  let parsed = Sexp.of_string line |> Exprs.exprS_of_sexp in
  parsed |> Exprs.show_exprS |> Out_channel.print_endline;

  let desugared = parsed |> Desugar.desugar in
  desugared |> Exprc.show_exprC |> Out_channel.print_endline;

  Interp.interp desugared [] |> Environment.sexp_of_value |> Out_channel.print_s

let run file = In_channel.read_all file |> eval_line

let repl () =
  while true do
    Out_channel.print_string "zelang>";
    Out_channel.flush stdout;
    In_channel.input_line Stdio.stdin |> Option.value_exn |> eval_line
  done

let () =
  let argv = Sys.get_argv () in
  if Array.length argv > 1 then run argv.(1) else repl ()
(* Out_channel.flushstdout *)
