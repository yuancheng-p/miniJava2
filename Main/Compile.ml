open Parser
open Typing

let execute lexbuf verbose =
  try
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;

    print_endline "start typing";
    let t_ast = Typing.typing ast in
    print_endline "end typing";

  with
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
