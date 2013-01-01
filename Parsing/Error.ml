type t =
  | Illegal_character of char
  | Illegal_escape_char
  | Unterminated_string
  | Unterminated_comment
  | Syntax

exception Error of t * Location.t;;

(* Les erreurs. *)
let report_error = function
  | Illegal_character c ->
      print_string "Error : Illegal character(";
      print_char c;
      print_endline "): "
  | Illegal_escape_char ->
      print_endline "Error : Illegal escape character in string: "
  | Unterminated_string ->
      print_endline "Error : String literal not terminated: "
  | Unterminated_comment ->
      print_endline "Error : Comment not terminated: "
  | Syntax ->
      print_endline "Error : Syntax error: "

let illegal_char char loc =
  raise (Error(Illegal_character char, loc))

let illegal_escape_char loc =
  raise (Error(Illegal_escape_char, loc))

let unterminated_string loc =
  raise (Error (Unterminated_string, loc))

let unterminated_comment loc =
  raise (Error (Unterminated_comment, loc))

let syntax loc =
  raise (Error (Syntax, loc))
