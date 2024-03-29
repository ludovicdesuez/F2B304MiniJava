open MiniJavaAST
open Parser
open Lexer

let get_file str =
  let temp2 = Filename.check_suffix str ".mjava" in
  let file = (if temp2 then str else str^".mjava") in
  let filename = 
    begin
      try
	let idx = String.rindex str '/' in
	let temp1 = String.sub str (idx + 1) ((String.length str) - idx - 1) in
	if temp2 then Filename.chop_suffix temp1 ".mjava" else temp1
      with Not_found ->
	if temp2 then Filename.chop_suffix str ".mjava" else str
    end
  in
  file, filename

let compile str =
  let (file, filename) = get_file str in
  try 
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    Location.init lexbuf file;
    print_endline "opening file";
    try
      try
	let t = Parser.fichier Lexer.nexttoken lexbuf in
	print_endline (MiniJavaAST.string_of_Expr t 0);
      with
	| Parsing.Parse_error -> Error.syntax (Location.curr lexbuf)
    with
      | Error.Error (kind, loc) ->
	close_in (input_file);
	Error.report_error kind;
	Location.print loc;
	print_newline();
	exit 0

  with Sys_error s ->
    print_endline ("Can't find file '" ^ file ^ "'")

let _ =
  print_endline "miniJava compiler";
  Arg.parse [] compile ""

