open Parser
open Typing

let execute lexbuf verbose =
  try
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;

    print_endline "start typing";
    let t_ast = Typing.typing ast verbose in
    print_endline "end typing";

    let class_descriptors, methods_table = Compiling.compile t_ast
    in let eval_results = Eval.eval t_ast class_descriptors, methods_table
    in ();

  with
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
