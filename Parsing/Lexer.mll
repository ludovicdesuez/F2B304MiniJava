{
open Parser
open Error
open Location

}

let character = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let ShiftLetter = ['A'-'Z']
let NoShiftLetter = ['a'-'z']
let digit = ['0'-'9']

let className = ShiftLetter(character)*
let otherName = NoShiftLetter(character)*
let integer = digit+

let space = [' ' '\t']
let newline = ("\010" | "\013" | "\013\010")

rule nexttoken = parse
| space+ { nexttoken lexbuf }
| newline { Location.incr_line lexbuf; nexttoken lexbuf }
| eof { EOF }

| "class" { CLASS }
| "extends" {EXTENDS}
| "static" {STATIC}

| "{" { LACCOLADE }
| "}" { RACCOLADE }
| "(" { LPAR }
| ")" { RPAR }
| ";" {SEMICOLON}
| "=" {EQUALS}

| className { CLASSNAME(Lexing.lexeme lexbuf) }
| otherName { OTHERNAME(Lexing.lexeme lexbuf)}
| integer { INTEGER(int_of_string(Lexing.lexeme lexbuf)) }

| _ as c { Error.illegal_char c (Location.curr lexbuf) }


{ }
