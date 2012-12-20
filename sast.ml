open Ast

type env = {
	mutable functions : funcs list ;
}

let eql_fname id = function
  Func(fn) -> fn.funcId = id
| _ -> false

let eql_mname id = function
	Main(fn) -> fn.mainId = id
| _ -> false
	
let rec count_fn_id id = function
	| [] -> 0
	| hd::tl -> begin
								match hd with
								| Func(fn) -> if fn.funcId = id then
																1 + count_fn_id id tl
															else
																count_fn_id id tl
							 	| Main(fn) -> 	if fn.mainId = id then
																1 + count_fn_id id tl
															else
																count_fn_id id tl
						  	| _ -> count_fn_id id tl
							end
	
(*determines if the given function exists*)
let isFunction func env =
	let id = (match func with Func(f) -> f.funcId|Main(f) -> f.mainId | _ -> "_DNE") in
			if count_fn_id id env.functions = 1 then
				true
			else
				let e = "Duplicate function name: " ^ id in
					raise (Failure e)

(*Determine if a function with given name exists*)
let isFunction_name id env = List.exists (eql_fname id) env.functions

let isMain_name id env = List.exists (eql_mname id) env.functions

(*Returns the function that has the given name*)
let getFunc_fname id env =
	try
		let afunc = List.find (eql_fname id) env.functions in
			afunc (*Found a function with name like that*)
	with Not_found -> raise(Failure("Function " ^ id ^ "has not yet been declared" ) )

let get_main id env =
	try
		let afunc = List.find (eql_fname id) env.functions in
			afunc
	with Not_found -> raise(Failure(id ^ "has not yet been declared" ) )

(*this is for generic functions only*)
let is_formal_param func fpname = List.exists (function FormalVar(_,name) -> name = fpname) func.formalArgs

(*Determines if a formal parameter with the given name 'fpname' exists in the given function*)
let exists_formal_param func fpname =
	match func with
	| Func(func) -> is_formal_param func fpname
	| _ -> false (*not applicable*)

(*for generic functions only*)
let is_variable_decl func vname = List.exists (function Define(_,name,_) -> name = vname) func.localVars

let is_variable_decl_main func vname = List.exists (function Define(_,name,_) -> name = vname) func.mainVars


(*Determines if a variable declaration with the given name 'vname' exists in the given functioin*)
let exists_variable_decl func vname =
	match func with
	| Func(func) -> is_variable_decl func vname
	| _ -> false 
	
(*this gets formal paramters for a generic function*)
let get_fpdt func fpname =
	try
		let fparam = List.find (function FormalVar(_,name) -> name = fpname) func.formalArgs in
			let FormalVar(dt,_) = fparam in
				dt (*return the data type*)
	with Not_found -> raise (Failure ("Formal Parameter " ^ fpname ^ " should exist but was not found in  function " ^ func.funcId)) (*this shouldn't not happen*)

(*gets the variable type - only for generic functions*)
let get_var_type func vname =
	try
		let var = List.find (function Define(_,vn,_) -> vn = vname) func.localVars in
			let Define(dt,_,_) = var in
				dt (*return the data type*)
	with Not_found -> raise (Failure ("Variable " ^ vname ^ " should exist but was not found in the  function " ^ func.funcId)) (*this shouldn't not happen*)

let get_var_type_main func vname =
	try
		let var = List.find (function Define(_,vn,_) -> vn = vname) func.mainVars in
			let Define(dt,_,_) = var in
				dt (*return the data type*)
	with Not_found -> raise (Failure ("Variable " ^ vname ^ " should exist but was not found in " ^ func.mainId)) (*this shouldn't not happen*)


let get_type_main main name =
	if is_variable_decl_main main name (*It's a variable*)
		then get_var_type_main main name
	else
		let e = "Variable " ^ name ^ " is being used without being declared in main " ^ main.mainId in
				raise (Failure e)

(*Returns the type of a given variable name *)
let get_type func name =
	if is_variable_decl func name (*It's a variable*)
		then get_var_type func name
	else
		if is_formal_param func name then
			get_fpdt func name
		else (*Variable has not been declared as it was not found*)
			let e = "Variable " ^ name ^ " is being used without being declared in function " ^ func.funcId in
				raise (Failure e)

(*Determines if the given identifier exists*)
let exists_id name func = (is_variable_decl func name) or (is_formal_param func name)

let exists_id_main name func = (is_variable_decl_main func name) 

(*see if there is a function with given name "func"*)
let find_function func env =
	try
		let _ = List.find (eql_fname func) env.functions in
			true (*return true on success*)
	with Not_found -> raise Not_found

let isDup_fp_single func = function
	FormalVar(_,my_name) ->
		function c ->
			function FormalVar(_,name) ->
				if my_name = name
					then
						if c = 0
							then c+1
							else let e = "Duplicate formal parameter in function: " ^ func.funcId^ "\n" in
								raise (Failure e)
					else c

(*This check for duplicate formal parameters in a function*)
let cisDup_fp func =
	let isdup f = List.fold_left (isDup_fp_single func f) 0 func.formalArgs
	in let _ = List.map isdup func.formalArgs
	in false

let dup_vdecl_single func = function
	Define(_,mn,_) ->
		function c ->
			function Define(_,tn,_) ->
				if mn = tn
					then
						if c = 0
							then c+1
							else let e = "Duplicate variable declaration '"^ mn ^"' in function : " ^ func.funcId  in
								raise (Failure e) (*throw error on duplicate formal parameter.*)
					else c

(*checks if there is a duplicate variable declaration for  functions*)
let dup_vdecl = function
  Main(func) -> false
| Load(func) -> false
| Func(func) -> 
	let isdup var = List.fold_left (dup_vdecl_single func var) 0 func.localVars in
		let _ = List.map (
			function Define(_,varname,_) ->
				List.map (
					function FormalVar(_,formal_nm) ->
						if formal_nm = varname
							then let e = "Redeclaring a formal parameter '" ^ formal_nm ^"' not allowed in function : " ^ func.funcId ^"\n" in
								raise(Failure e)
						else false
				) func.formalArgs
		) func.localVars in
			let _ = List.map(isdup) func.localVars in
				false

let is_int s =
	try ignore (int_of_string s); true
	with _ -> false

let rec int_flatten = function
    |  Lit_List(xs) -> List.concat (List.map int_flatten xs) 
		|  Lit_Int(x) -> [x] ;;

let rec bool_flatten = function
    |  Lit_List(xs) -> List.concat (List.map bool_flatten xs) 
		|  Lit_Bool(x) -> [x] ;;

let is_int_list ls = 
	try ignore (int_flatten ls); true
	with _ -> false ;;

let is_bool_list ls = 
	try ignore (bool_flatten ls); true
	with _ -> false ;;

let is_list ls = 
	is_int_list ls || is_bool_list ls 
	
let is_string_bool = function "true" -> true | "false" -> true | _ -> false

let rec is_num func env = function
	  Vars(e) -> begin
								match e with 
								| Lit_Int(_) -> true
								| _ -> false
							 end 
	| Id(s) -> (function Integer -> true |  _ -> false) (get_type func s)
	| BinOpr(_,e1,e2) -> (is_num func env e1) && (is_num func env e2)
	| Funcall(f,_) -> let fn = (getFunc_fname f) env in 
												begin
													match fn with 
														| Func(f) -> (string_of_rt f.reType) = (string_of_dt Integer)
														| _ -> false
												end
	| _ -> false

let rec is_num_main func env = function
	  Vars(e) -> begin
								match e with 
								| Lit_Int(_) -> true
								| _ -> false
							 end 
	| Id(s) -> (function Integer -> true |  _ -> false) (get_type_main func s)
	| BinOpr(_,e1,e2) -> (is_num_main func env e1) && (is_num_main func env e2)
	| Funcall(f,_) -> let fn = (getFunc_fname f) env in 
												begin
													match fn with 
														| Func(f) -> (string_of_rt f.reType) = (string_of_dt Integer)
														| _ -> false
												end
	| _ -> false

let rec get_lit_type = function
	| Lit_Int(_) -> Integer
	| Lit_Bool(_) -> Bool
	| Lit_List(e) -> List(get_lit_type (List.hd e)) 

let isArithmetic = function
	| Add -> true
	| Sub -> true
	| Mul -> true
	| Div -> true
	| Mod -> true
	| Pow -> true
	| _ -> false
	
let isEql = function
	| Eql -> true
	| Neq -> true
	| _ -> false
	
let isLogic = function
	| And -> true
	| Or -> true
	| Not -> true
	| _ -> false
	
let rec get_expr_type e func env=
	match e with
		| Id(s) -> Data(get_type func s)
		| Vars(s) -> Data(get_lit_type s)
		| BinOpr(op,e1,e2) -> let t1 = get_expr_type e1 func env and t2 = get_expr_type e2 func env in
			 if isLogic op then
				begin
					match t1,t2 with
					| Data(Bool),Data(Bool) -> Data(Bool)
					| _,_ -> raise (Failure "Invalid Types used in a Logical expression")
				end
			else if isEql op then
				begin
					match t1,t2 with
					| Data(Integer),Data(Integer) -> Data(Bool)
					| Data(Bool),Data(Bool) -> Data(Bool)
					| Data(List(x)),Data(List(y)) -> Data(Bool)
					| Data(Cell), Data(Cell) -> Data(Bool)
					| _,_ -> raise (Failure "Invalid Types used in an equality expression")
				end
			else if isArithmetic op then
				begin
					match t1,t2 with
					| Data(Integer),Data(Integer) -> Data(Integer)
					| _,_ -> raise (Failure "Invalid Types used in an arithmetic expression")
				end
			else 
				begin
					match t1,t2 with
					| Data(Integer),Data(Integer) -> Data(Bool)
					| _,_ -> raise (Failure "Invalid Types used in a relational expression")
				end
		| Funcall(fname,expr) -> let fn = getFunc_fname fname env in 
															begin
																match fn with 
																| Func(f) -> f.reType
																| _ -> Ast.Data(Integer)
															end
		| Paran(e) -> get_expr_type e	func env
		| Assign(_,_) -> Void
		| Assoc(a,b) -> if exists_id b func  then
										begin
											match a with
											| Left -> Data(Cell)
											| Right ->  Data(Cell)
											| Up -> Data(Cell)
											| Down -> Data(Cell)
											| Hleft -> Data(Bool) 
											| Hright -> Data(Bool) 
											| Htop -> Data(Bool)
											| Hbtm -> Data(Bool)
											| Empty -> Data(Bool)
											| Remove -> Data(Cell)
											| _ -> Void
										end
										else
											raise(Failure(b ^ " not defined ")) 
		| Visit(s) -> Data(Bool)
		| Target(b) -> if exists_id b func  then
										Data(Bool)
									else
										raise(Failure("Invalid expression "^ b))
		| Src(b) -> if exists_id b func  then
									Data(Bool)
								else
										raise(Failure("Invalid expression "^ b))	
		| Pointer -> Data(Cell)
		| Loc(b) -> if exists_id b func  then
									Data(Cell)
								else
										raise(Failure("Invalid expression "^ b))	
		| Null -> Void

let rec get_expr_type_main e func env=
	match e with
		| Id(s) -> Data(get_type_main func s)
		| Vars(s) -> Data(get_lit_type s)
		| BinOpr(op,e1,e2) -> let t1 = get_expr_type_main e1 func env and t2 = get_expr_type_main e2 func env in
			 if isLogic op then
				begin
					match t1,t2 with
					| Data(Bool),Data(Bool) -> Data(Bool)
					| _,_ -> raise (Failure "Invalid Types used in a Logical expression")
				end
			else if isEql op then
				begin
					match t1,t2 with
					| Data(Integer),Data(Integer) -> Data(Bool)
					| Data(Bool),Data(Bool) -> Data(Bool)
					| Data(List(x)),Data(List(y)) -> Data(Bool)
					| Data(Cell), Data(Cell) -> Data(Bool)
					| _,_ -> raise (Failure "Invalid Types used in an equality expression")
				end
			else if isArithmetic op then
				begin
					match t1,t2 with
					| Data(Integer),Data(Integer) -> Data(Integer)
					| _,_ -> raise (Failure "Invalid Types used in an arithmetic expression")
				end
			else 
				begin
					match t1,t2 with
					| Data(Integer),Data(Integer) -> Data(Bool)
					| _,_ -> raise (Failure "Invalid Types used in a relational expression")
				end
		| Funcall(fname,expr) -> let fn = get_main fname env in 
															begin
																match fn with 
																| Func(f) -> f.reType
																| _ -> Ast.Data(Integer)
															end
		| Paran(e) -> get_expr_type_main e func env
		| Assign(_,_) -> Void
		| Assoc(a,b) -> if exists_id_main b func  then
										begin
											match a with
											| Left -> Data(Cell)
											| Right ->  Data(Cell)
											| Up -> Data(Cell)
											| Down -> Data(Cell)
											| Hleft -> Data(Bool) 
											| Hright -> Data(Bool) 
											| Htop -> Data(Bool)
											| Hbtm -> Data(Bool)
											| Empty -> Data(Bool)
											| Remove -> Data(Cell)
											| _ -> Void
										end
										else
											raise(Failure(b ^ " not defined ")) 
		| Visit(s) -> Data(Bool)
		| Target(b) -> if exists_id_main b func  then
										Data(Bool)
									else
										raise(Failure("Invalid expression "^ b))
		| Src(b) -> if exists_id_main b func  then
									Data(Bool)
								else
										raise(Failure("Invalid expression "^ b))	
		| Pointer -> Data(Cell)
		| Loc(b) -> if exists_id_main b func  then
									Data(Cell)
								else
										raise(Failure("Invalid expression "^ b))	
		| Null -> Void

(*Makes sure that the given arguments in a function call match the function signature*)
(*fname of function being called*)
(*exprlist - list of expr in funcation call*)
(*env - the enviroment*)
let rec check_types_args cfunc env formalArgs = function
	| [] -> true
	| hd::tl -> begin
								match List.hd formalArgs with
								| FormalVar(dt,_) -> if string_of_rt (Data(dt)) = string_of_rt (get_expr_type hd cfunc env) then
																				check_types_args cfunc env (List.tl formalArgs) tl
																			else
																				raise(Failure("Argument type mismatch"))
							end
	
let rec check_types_argsmain cfunc env formalArgs = function
	| [] -> true
	| hd::tl -> begin
								match List.hd formalArgs with
								| FormalVar(dt,_) -> if string_of_rt (Data(dt)) = string_of_rt (get_expr_type_main hd cfunc env) then
																				check_types_argsmain cfunc env (List.tl formalArgs) tl
																			else
																				raise(Failure("Argument type mismatch"))
							end

let check_types fname exprlist cfunc env =
	let func = getFunc_fname fname env in
		match func with
 			Func(func) ->
					if List.length exprlist = List.length func.formalArgs then
						if check_types_args cfunc env func.formalArgs exprlist then
							true
						else
							raise(Failure("Argument types do not match"))
					else
						raise(Failure("Number of arguments do not match with function signature")) 
		  | _ -> true

let check_types_main fname exprlist cfunc env =
	let func = getFunc_fname fname env in
		match func with
 			Func(func) ->
					if List.length exprlist = List.length func.formalArgs then
						if check_types_argsmain cfunc env func.formalArgs exprlist then
							true
						else
							raise(Failure("Argument types do not match"))
					else
						raise(Failure("Number of arguments do not match with function signature")) 
		  | _ -> true

(*check if variable declation is valid*)
let valid_vdecl func env =
	match func with
		| Load(func) -> false
		| Func(func) ->
			let _ = List.map (function Define(dt,nm,value) ->
				let e = "Invalid variable declaration for '" ^ nm ^ "' in function " ^ func.funcId ^ "\n" in
					let be = e ^ "The only allowed values for initializing boolean variables are 'true' and 'false.' \\n" in
						match dt with
						  Cell  -> if string_of_expr value = "AMLJava.current" then true else raise (Failure e)
						| List(g)  -> begin 
														match value with 
														| Vars(f) -> if is_list f then true else raise (Failure e)
														| Id(f) -> if (get_type func f) = List(g) then true else raise (Failure e)
														| Funcall(fname,list) -> let fn = (getFunc_fname fname) env in 
														  									begin
																									match fn with 
																									| Func(f1) -> if (string_of_rt f1.reType) = (string_of_dt dt) then 
																																	if check_types fname list func env then
																																		true
																																	else
																																		raise(Failure e)
																															 else raise (Failure e)
																									| _ -> raise (Failure e)
																								end
														| _ -> false  
													end
						| Integer  -> begin
														match value with 
														| Vars(f) -> begin
																				 	match f with 
																					|	Lit_Int(t) -> true 
																					| _ -> raise (Failure e)
																			 	 end
													  | Id(f) -> if (get_type func f) = Integer then true else raise (Failure e)
														| Funcall(fname,list) -> let fn = (getFunc_fname fname) env in 
														  									begin
																									match fn with 
																									| Func(f1) -> if (string_of_rt f1.reType) = (string_of_dt dt) then 
																																	if check_types fname list func env then
																																		true
																																	else
																																		raise(Failure e)
																															 else raise (Failure e)
																									| _ -> raise (Failure e)
																								end
														| _ -> false  
													end
						| Bool -> begin
														match value with 
														| Vars(f) -> begin
																				 	match f with 
																					|	Lit_Bool(t) -> true 
																					| _ -> raise (Failure be)
																			 	 end
													  | Id(f) -> if (get_type func f) = Bool then true else raise (Failure be)
														| Funcall(fname,list) -> let fn = (getFunc_fname fname) env in 
														  									begin
																									match fn with 
																									| Func(f1) -> if (string_of_rt f1.reType) = (string_of_dt dt) then 
																																	if check_types fname list func env then
																																		true
																																	else
																																		raise(Failure e)
																															 else raise (Failure e)
																									| _ -> raise (Failure e)
																								end
														| _ -> false  
													end ) func.localVars
							in 
								true
	| Main(func) ->
			let _ = List.map (function Define(dt,nm,value) ->
				let e = "Invalid variable declaration for '" ^ nm ^ "' in " ^ func.mainId ^ "\n" in
					let be = e ^ "The only allowed values for initializing boolean variables are 'true' and 'false.' \\n" in
						match dt with
						  Cell -> if string_of_expr value = "AMLJava.current" then true else raise (Failure e)
						| List(g)  -> begin 
														match value with 
														| Vars(f) -> if is_list f then true else raise (Failure e)
														| Id(f) -> if (get_type_main func f) = List(g) then true else raise (Failure e)
														| Funcall(fname,list) -> let fn = (getFunc_fname fname) env in 
														  									begin
																									match fn with 
																									| Func(f1) -> if (string_of_rt f1.reType) = (string_of_dt dt) then 
																																	if check_types_main fname list func env then
																																		true
																																	else
																																		raise(Failure e)
																															 else raise (Failure e)
																									| _ -> raise (Failure e)
																								end
														| _ -> false  
													end
						| Integer  -> begin
														match value with 
														| Vars(f) -> begin
																				 	match f with 
																					|	Lit_Int(t) -> true 
																					| _ -> raise (Failure e)
																			 	 end
													  | Id(f) -> if (get_type_main func f) = Integer then true else raise (Failure e)
														| Funcall(fname,list) -> let fn = (getFunc_fname fname) env in 
														  									begin
																									match fn with 
																									| Func(f1) -> if (string_of_rt f1.reType) = (string_of_dt dt) then 
																																	if check_types_main fname list func env then
																																		true
																																	else
																																		raise(Failure e)
																															 else raise (Failure e)
																									| _ -> raise (Failure e)
																								end
														| _ -> false  
													end
						| Bool -> begin
														match value with 
														| Vars(f) -> begin
																				 	match f with 
																					|	Lit_Bool(t) -> true 
																					| _ -> raise (Failure be)
																			 	 end
													  | Id(f) -> if (get_type_main func f) = Bool then true else raise (Failure be)
													| Funcall(fname,list) -> let fn = (getFunc_fname fname) env in 
														  									begin
																									match fn with 
																									| Func(f1) -> if (string_of_rt f1.reType) = (string_of_dt dt) then 
																																	if check_types_main fname list func env then
																																		true
																																	else
																																		raise(Failure e)
																															 else raise (Failure e)
																									| _ -> raise (Failure e)
																								end
														| _ -> false  
													end ) func.mainVars
							in 
								true


(*Checks if the given statement list has  return stmt last*)
let has_return_stmt list =
	if List.length list = 0
		then false
		else match (List.hd (List.rev list)) with
		  Return(_) -> true
		| _ -> false

(*checks the given stmt list to determine if it has if/else statement that include a return value in *)
(*both the if body part AND the else part*)
let rec if_else_has_return_stmt stmt_list =
	let if_stmts = List.filter (function If(_,_,_) -> true | _ -> false) stmt_list in
    let rets = List.map (
			function
			  If(_,s1,s2) ->
					begin
						match s1,s2 with
							StmtBlk(lst1),StmtBlk(lst2) -> (has_return_stmt lst1 || if_else_has_return_stmt lst1) && (has_return_stmt lst2 || if_else_has_return_stmt lst2)
						| _ -> raise(Failure("An unexpected error has occured.")) (*shouldn't happen*)
					end
			| _  -> false
		) if_stmts in
			List.fold_left (fun b v -> b || v) false rets

(*Checks that a return statement is present in the given function. *)
let has_return_stmt func =
	let stmt_list = func.body in
		if List.length stmt_list = 0
			then false
			else match List.hd (List.rev stmt_list) with
			  Return(e) -> raise(Failure("Return statement is not permitted in main method"))
			| _ -> false

let rec count_rets = function
	| [] -> 0
	| hd::tl -> begin
								match hd with
								| Return(_) -> 1 + count_rets tl
								| _ -> count_rets tl
							end
		
let has_multiple_ret func = 
	let count = count_rets func.statements in 
	if count > 1 then
		raise(Failure("Multiple return statements"))
	else 
		if count = 1 && if_else_has_return_stmt func.statements then
			raise(Failure("Multiple return statements"))
		else
			false

let has_return func =
	let stmt_list = func.statements in
		if List.length stmt_list = 0
			then false
			else match List.hd (List.rev stmt_list) with
			  Return(e) -> true
			| _ -> false

let rec checkret_type func env ret = function 
	| [] -> true
	| hd::tl -> begin 
							 match hd with 
							| Return(e) -> if get_expr_type e func env = ret then
																checkret_type func env ret tl
														else
																raise(Failure("return type mismatch"))
							| _ -> checkret_type func env ret tl
						  end 

let valid_return_stmt env = function
| Main(func) ->
		let ifelse_has_return = if_else_has_return_stmt func.body in (*whether if/else block both have a return value*)
			let has_return = has_return_stmt func in	 (*if a function's last stmt is a return stmt*)
					 if has_return or ifelse_has_return
						then raise (Failure "Main function cannot have a return value")
						else true
| Func(func) ->
		let ifelse_has_return = if_else_has_return_stmt func.statements in (*whether if/else block both have a return value*)
			let has_return = has_return func in
				let _ = has_multiple_ret func in 	 (*if a function's last stmt is a return stmt*)
					if func.reType = Void then
						if (has_return && not ifelse_has_return) or (not has_return && ifelse_has_return) then
							raise(Failure("Invalid return expression in function " ^ func.funcId ^ ": function is void"))
						else
							true		
					else 
						if (has_return && not ifelse_has_return) or (not has_return && ifelse_has_return) then
							if checkret_type func env func.reType func.statements then
								true
							else
								raise(Failure("Expected return type : " ^ string_of_rt func.reType))
						else
							raise(Failure( func.funcId ^ " does not return any expression"))
| _ -> true

let rec valid_expr (func : Ast.func) expr env =
	match expr with
	  Vars(_) -> true
	| Id(s) -> if exists_id s func then true else raise (Failure ("Undeclared identifier " ^ s ^ " is used"))
	| BinOpr(_,e1,e2) -> let exprtype = get_expr_type expr func env in
													true
	| Assign(id, e1) ->
			if exists_id id func
				then let dt = get_type func id and _ = valid_expr func e1 env and exprtype = get_expr_type e1 func env in
					match dt,exprtype with
					| Integer,Data(Integer) -> true
					| Bool,Data(Bool) -> true
					| List(x),Data(List(y)) -> if x = y then true else raise(Failure ("DataTypes do not match up in an assignment expression to variable " ^ id))
					| List(x),Void -> (e1 = Null)
					| Cell, Data(Cell) -> true
					| _,_ -> raise(Failure ("DataTypes do not match up in an assignment expression to variable " ^ id))
				else raise( Failure ("Undeclared identifier " ^ id ^ " is used" ))
	| Funcall(fname, exprlist) -> if isFunction_name fname env then
																	let _has_valid_exprs = List.map (fun e -> valid_expr func e env) exprlist in
																		if check_types fname exprlist func env then (*check that the types match up otherwise throws an error *)
																			true
																		else
																			raise(Failure("Actual and Formal Parameters do not match"))
																else
																	raise(Failure ("Undefined function "^ fname ^" is used")) 
	| Paran(e) -> valid_expr func e env
	| Assoc(_,s) -> if exists_id s func then true else raise (Failure ("Undeclared identifier " ^ s ^ " is used"))
	| Loc(s) ->  if exists_id s func then
								if (get_type func s = Cell) then
									true
								else
									raise(Failure("Not a cell type")) 
							else
								raise (Failure ("Undeclared identifier " ^ s ^ " is used")) 
	| Target(s) -> if exists_id s func then
									if (get_type func s = Cell) then
										true
									else
										raise(Failure("Not a cell type")) 
								else
									raise (Failure ("Undeclared identifier " ^ s ^ " is used"))
	| Visit(x) -> (valid_expr func x env) && (get_expr_type x func env  = Data(Cell))
	| _ -> false (*should not happen - added this to turn off compiler warnings about incomplete matching for Noexpr*)


let rec valid_expr_main (func : Ast.main) expr env =
	match expr with
	  Vars(_) -> true
	| Id(s) -> if exists_id_main s func then true else raise (Failure ("Undeclared identifier " ^ s ^ " is used"))
	| BinOpr(_,e1,e2) -> let exprtype = get_expr_type_main expr func env in 
												true
	| Assign(id, e1) -> 
			if exists_id_main id func
				then let dt = get_type_main func id and _ = valid_expr_main func e1 env and exprtype = get_expr_type_main e1 func env in
					match dt,exprtype with
					| Integer,Data(Integer) -> true
					| Bool,Data(Bool) -> true
					| List(x),Data(List(y)) -> if x = y then true else raise(Failure ("DataTypes do not match up in an assignment expression to variable " ^ id))
					| List(x),Void -> (e1 = Null)
					| Cell, Data(Cell) -> true
					| _,_ -> raise(Failure ("DataTypes do not match up in an assignment expression to variable " ^ id))
				else raise( Failure ("Undeclared identifier " ^ id ^ " is used" ))
	| Funcall(fname, exprlist) -> if isFunction_name fname env then
																	let _has_valid_exprs = List.map (fun e -> valid_expr_main func e env) exprlist in
																		if check_types_main fname exprlist func env then (*check that the types match up otherwise throws an error *)
																			true
																		else
																			raise(Failure("Actual and Formal Parameters do not match"))
																else
																	raise(Failure ("Undefined function "^ fname ^" is used"))
	| Paran(e) -> valid_expr_main func e env
	| Assoc(_,b) -> valid_expr_main func (Id(b)) env
	| Loc(x) ->  (valid_expr_main func (Id(x)) env) &&  (get_type_main func x = Cell)
	| Target(x) -> (valid_expr_main func (Id(x)) env) && (get_type_main func x = Cell)
	| Visit(x) -> (valid_expr_main func x env) && (get_expr_type_main x func env  = Data(Cell))
	| _ -> false (*should not happen - added this to turn off compiler warnings about incomplete matching for Noexpr*)


let dup_letter_single func = function
	Define(_,mn,_) ->
		function c ->
			function Define(_,tn,_) ->
				if mn = tn
					then
						if c = 0
							then c+1
							else let e = "Duplicate variable declaration '"^ mn ^"' in function : " ^ func.funcId  in
								raise (Failure e) (*throw error on duplicate formal parameter.*)
					else c

(*Checks the body of a  function/main *)
let valid_body func env =
	match func with
		| Func(func) ->
				let rec check_stmt =
					function
						StmtBlk(st_list) ->
							let _ = List.map(fun(x) -> check_stmt x) st_list in (*Check statements in the block. Err will be thrown for an invalid stmt*)
								true
					| Expr(st) ->
							if valid_expr func st env then
								true
							else 
								raise(Failure ("Invalid expression "^ string_of_expr st ^" in function " ^func.funcId ^ "\n")) 
					| Return(st) -> (get_expr_type st func env) = func.reType
					| Display -> true
					| Revert -> true
					| Exit -> true
					| Print(e) -> valid_expr func e env
					| Move(e) ->  (e >= 1) && ( e <= 4) 
					| MoveTo(s) -> valid_expr func (Id(s)) env
					| ListAdd(id,ex) -> if (valid_expr func (Id(id)) env) && (valid_expr func ex env) then
																begin
																	match get_expr_type ex func env with 
																	| Data(x) -> List(x) = get_type func id
																	| _ -> false
																end
															else
																false
					| If(predicate,stmt1,stmt2) ->
							let pred_type = get_expr_type predicate func env in
								let _vpred = (*Check predicate*)
									match pred_type with
										| Data(Bool) -> true
										| _ -> raise(Failure("predicate expression must be a valid boolean expression that evaluates to true/false"))
								in
									if (check_stmt stmt1) && (check_stmt stmt2)
										then true
										else raise(Failure("Invalid expression used in if statement in function " ^ func.funcId ^ "\n"))
				in
					let _ = List.map(check_stmt) func.statements in
						true
		| Main(func) ->
				let rec check_stmt =
					function
						StmtBlk(st_list) ->
							let _ = List.map(fun(x) -> check_stmt x) st_list in (*Check statements in the block. Err will be thrown for an invalid stmt*)
								true
					| Expr(st) ->
							if valid_expr_main func st env then
								true
							else
								 raise(Failure ("Invalid expression "^ string_of_expr st ^" in function " ^func.mainId ^ "\n")) 
					| Return(st) -> false
					| Display -> true
					| Revert -> true
					| Exit -> true
					| Print(e) -> valid_expr_main func e env
					| Move(e) ->  (e >= 1) && ( e <= 4) 
					| MoveTo(s) -> valid_expr_main func (Id(s)) env
					| ListAdd(id,ex) -> if (valid_expr_main func (Id(id)) env) && (valid_expr_main func ex env) then
																begin
																	match get_expr_type_main ex func env with 
																	| Data(x) -> List(x) = get_type_main func id
																	| _ -> false
																end
															else
																false
					| If(predicate,stmt1,stmt2) ->
							let pred_type = get_expr_type_main predicate func env in
								let _vpred = (*Check predicate*)
									match pred_type with
										| Data(Bool) -> true
										| _ -> raise(Failure("predicate expression must be a valid boolean expression that evaluates to true/false"))
								in
									if (check_stmt stmt1) && (check_stmt stmt2)
										then true
										else raise(Failure("Invalid expression used in if statement in function " ^ func.mainId ^ "\n"))
				in
					let _ = List.map(check_stmt) func.body in
						true
		| _ -> true
		

let cisDup_fp func =
	let isdup f = List.fold_left (isDup_fp_single func f) 0 func.formalArgs
	in let _ = List.map isdup func.formalArgs
	in false

let isDup_fp = function
| Func(func) -> cisDup_fp func
| _ -> true

let check_function f env =
	let dup_fname = isFunction f env in
		let dup_formals = isDup_fp f in
			let vlocals = (not (dup_vdecl f)) && (valid_vdecl f env) (*make sure that we've no dup variable names, and data types match up*) in
				let vbody = valid_body f env in
				  let vret = valid_return_stmt env f in
						(*let _ = env.functions <- f :: env.functions (*add function name to environment *) in*)
							(not dup_fname) && (not dup_formals) && vlocals && vbody &&vret

let check_main f env =
	let dup_fname = isFunction f env in
		let vlocals = (not (dup_vdecl f)) && (valid_vdecl f env) in
			let vbody = valid_body f env in
				let vret = valid_return_stmt env f in
				(*let _ = env.functions <- (f) :: env.functions (*add function name to environment *) in*)
					(not dup_fname) && vlocals && vbody && vret

let valid_func env = function
  Func(f) -> let afunc = Func(f) in check_function afunc env
| Main(f) -> let afunc = Main(f) in check_main afunc env
| Load(f) -> true

(*Checks to make sure that the main function exists*)
let exists_main env =
	if (isMain_name "main" env) then
	 	if not (isFunction_name "main" env) then
			true
		else 
			raise(Failure("A generic function cannot be called 'Main'"))
	else raise(Failure("'main' does not exist! No Entry point to the program!"))

let rec numLoad = function
	| [] -> 0
	| hd::tl -> begin
								match hd with
								| Load(s) -> 1 + numLoad tl
								| _ -> numLoad tl
							end

let checkLoad list = begin
											match List.hd list with
											| Load(str) -> begin
																			match List.hd (List.tl list) with
																			| Main(fn) -> true
																			| _ -> raise(Failure ("'main' must be after load"))
																		end
											| _ -> raise(Failure("'load' must be at the start of the program"))
										end

let check_program funclist =
	let (environ : env) = { functions = funclist} in
		let _loadchecker = numLoad funclist = 1 in 
			let _loadmain = checkLoad funclist in 
				let _dovalidation = List.map ( fun(f) -> valid_func environ f) funclist in (*Do the semantic analysis*)
			 			let _mainexists = exists_main environ (*ensure that a main function exists*) in
							let _ = print_endline "\nSemantic analysis successfully completed.\nCompiling...\n" in
								true