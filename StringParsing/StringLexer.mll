{

open Error
open Location
open StringParser

}

let newline = ['\n']
let ignored = "\t"

let dquote = '"'
let escapedNL = "\\n"
let escapedBS = "\\\\"
let escapedDQ = "\\q"
let escapedInvalid = '\\'(_ # ['n' '\\' 'q'])
let others = (_ # ['\\' '"' '\n' '\t'])*

rule nexttoken = parse
  | newline { Location.incr_line lexbuf; nexttoken lexbuf }
  | ignored { nexttoken lexbuf }  
  | escapedNL { NEWLINE }
  | escapedBS { E_BACKSLASH }
  | escapedDQ { E_DQUOTE }
  | escapedInvalid  {Error.illegal_escape_char (Location.curr lexbuf) }
  | others { STRING_CONTENT(Lexing.lexeme lexbuf) }
  | dquote { DQUOTE }
{ }
