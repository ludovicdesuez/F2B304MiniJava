{
open Parser
}

let character = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let ShiftLetter = ['A'-'Z']
let NoShiftLetter = ['a'-'z']
let digit = ['0'-'9']

let className = ShiftLetter(character)*
let otherName = NoShiftLetter(character)*
let integer = digit+

let space = [' ' '\t' '\n']
rule nexttoken = parse
space+ { nexttoken lexbuf }
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
| integer {INTEGER(int_of_string(Lexing.lexeme lexbuf))}

{ }
