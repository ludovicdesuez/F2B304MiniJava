open StringParser
open StringLexer

let parse_string s =
  let lexbuf = Lexing.from_string s in
  let s = StringParser.miniJavaString StringLexer.nexttoken lexbuf in
  print_string s;
  s

