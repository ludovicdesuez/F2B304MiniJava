{
open Parser
open Error
open Location
open StringManip

}

let space = [' ' '\t']
let newline = ("\010" | "\013" | "\013\010")

let character = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let ShiftLetter = ['A'-'Z']
let NoShiftLetter = ['a'-'z']
let digit = ['0'-'9']

let className = ShiftLetter(character)*
let otherName = NoShiftLetter(character)*
let integer = digit+

let lineComment = "//"[^'\n']*
let validMultiComment = "/*"_*"*/"
let invalidMultiComment = "/*"_*eof


rule nexttoken = parse
| space+ { nexttoken lexbuf }
| newline { Location.incr_line lexbuf; nexttoken lexbuf }
| eof { EOF }

| lineComment { nexttoken lexbuf }
| validMultiComment { 
  Location.incr_line_n lexbuf (StringManip.count_substring (Lexing.lexeme lexbuf) "\n");
  nexttoken lexbuf 
}

| "class" { CLASS }
| "extends" {EXTENDS}
| "static" {STATIC}

| "{" { LACCOLADE }
| "}" { RACCOLADE }
| "(" { LPAR }
| ")" { RPAR }

| "<=" {COMP_INFEQ}
| ">=" {COMP_SUPEQ}
| "!=" {COMP_DIFF}
| "==" {COMP_EQ}
| "<"  {COMP_INF}
| ">"  {COMP_SUP}

| "!" {EXCLAMATION}
| ":" {COLON}
| ";" {SEMICOLON}
| "," {COMMA}
| "," {PERIOD}

| "=" {EQUALS}

| className { CLASSNAME(Lexing.lexeme lexbuf) }
| otherName { OTHERNAME(Lexing.lexeme lexbuf)}
| integer { INTEGER(int_of_string(Lexing.lexeme lexbuf)) }

| _ as c { Error.illegal_char c (Location.curr lexbuf) }


{ }
