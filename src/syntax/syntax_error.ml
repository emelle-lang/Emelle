module I = Parser.MenhirInterpreter

let parse_handling_errs lexbuf =
  let succeed a = Ok a in
  let fail _checkpoint =
    Message.error
      (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      Message.Syntax_error
  in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.expr lexbuf in
  I.loop_handle succeed fail supplier

let parse_file lexbuf =
  parse_handling_errs lexbuf
    (Parser.Incremental.file lexbuf.Lexing.lex_curr_p)
