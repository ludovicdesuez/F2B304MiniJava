{
open Parser
}

let character = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let ShiftLetter = ['A'-'Z']
let NoShiftLetter = ['a'-'z']
let digit = ['0'-'9']
let className = ShiftLetter(character)*
let otherName = NoShiftLetter(character)*
let space = [' ' '\t' '\n']
rule nexttoken = parse
space+ { nexttoken lexbuf }
| eof { EOF }
| "class" { CLASS }
| className { CLASSNAME(Lexing.lexeme lexbuf) }
| "{" { LACCOLADE }
| "}" { RACCOLADE }
| "extends" {EXTENDS}
| "static" {STATIC}
| otherName {OTHERNAME(Lexing.lexeme lexbuf)}
| ";" {SEMICOLON}

{ }
