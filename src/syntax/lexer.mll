{
  open Base
  open Parser

  exception Error of string
  exception Unclosed_string of Lexing.position
  exception Unclosed_comment of Lexing.position

  let lowercase_keywords =
    Hashtbl.of_alist_exn
      (module String)
      [ "and", AND
      ; "as", AS
      ; "case", CASE
      ; "export", EXPORT
      ; "forall", FORALL
      ; "foreign", FOREIGN
      ; "fun", FUN
      ; "import", IMPORT
      ; "in", IN
      ; "let", LET
      ; "rec", REC
      ; "type", TYPE
      ; "with", WITH ]

  let uppercase_keywords =
    Hashtbl.of_alist_exn
      (module String)
      [ "Ref", REF ]
}

let whitespace = [' ' '\t']
let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let alphanum = upper | lower | digit
let uident = upper (alphanum | '_' | '\'')*
let lident = lower (alphanum | '_' | '\'')*
let integer = ('-' | '+')? digit+
let decimal = integer ('.' digit+)

rule expr = parse
  | whitespace { expr lexbuf }
  | '\n' { Lexing.new_line lexbuf; expr lexbuf }
  | "(*" { lex_comment (Lexing.lexeme_start_p lexbuf) lexbuf; expr lexbuf }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPARENS }
  | ')' { RPARENS }
  | "->" { ARROW }
  | '!' { BANG }
  | '|' { BAR }
  | ':' { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUALS }
  | ',' { COMMA }
  | '.' { DOT }
  | '=' { EQUALS }
  | '?' { QUESTION }
  | ';' { SEMICOLON }
  | '*' { STAR }
  | '_' { UNDERSCORE }
  | uident as str {
      match Hashtbl.find uppercase_keywords str with
      | Some keyword -> keyword
      | None -> UIDENT str
    }
  | lident as str {
      match Hashtbl.find lowercase_keywords str with
      | Some keyword -> keyword
      | None -> LIDENT str
    }
  | integer { INT_LIT (Int.of_string (Lexing.lexeme lexbuf)) }
  | decimal { FLOAT_LIT (Float.of_string (Lexing.lexeme lexbuf)) }
  | '\"' {
      STRING_LIT
        (lex_string (Lexing.lexeme_start_p lexbuf) (Buffer.create 10) lexbuf)
      }
  | eof { EOF }
  | _ { raise (Error (Lexing.lexeme lexbuf)) }

and lex_string startp buffer = parse
  | '\"' { Buffer.contents buffer }
  | "\\\"" { Buffer.add_char buffer '\"'; lex_string startp buffer lexbuf }
  | "\\\'" { Buffer.add_char buffer '\''; lex_string startp buffer lexbuf }
  | "\\\\" { Buffer.add_char buffer '\\'; lex_string startp buffer lexbuf }
  | "\\n" { Buffer.add_char buffer '\n'; lex_string startp buffer lexbuf }
  | "\\t" { Buffer.add_char buffer '\t'; lex_string startp buffer lexbuf }
  | [^ '\"' '\\' '\n' '\t']+ {
      Buffer.add_string buffer (Lexing.lexeme lexbuf);
      lex_string startp buffer lexbuf
    }
  | _ { raise (Unclosed_string startp) }

and lex_comment startp = parse
  | "*)" { () }
  | "(*" {
      lex_comment (Lexing.lexeme_start_p lexbuf) lexbuf;
      lex_comment startp lexbuf
    }
  | eof { raise (Unclosed_comment startp) }
  | _ { lex_comment startp lexbuf }

{
}
