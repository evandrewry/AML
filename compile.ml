open Ast

exception RedeclarationOfStandardFunctionNotAllowedError

module StringMap = Map.Make(String);;

let translate funcs prog_name =
	let out_chan = open_out (prog_name ^ ".java") in
		let translated_prog = Ast.string_of_program funcs prog_name in 
						let proc_status = ignore(Printf.fprintf out_chan "%s" translated_prog); 
							close_out out_chan; 
							Sys.command (Printf.sprintf "javac %s.java" prog_name) in
								match proc_status with
					 				 0 ->  "Compilation successful\n"
							 		| _ -> "\nCompilation into Java bytecode unsuccessful!\n" ^
													Printf.sprintf "Javac Process Code: %i\n" proc_status ^
													Printf.sprintf "Compilation: javac %s.java\n" prog_name
