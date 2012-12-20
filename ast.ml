type binopr = Add | Sub | Mul | Div | Mod | Eql | Neq | Lsr | Leq | Gtr | Geq | Pow | And | Or | Not

type assoc = Remove| Next | Head | Empty | Up | Down | Left | Right | Hleft | Hright | Htop | Hbtm 

type datatype = 
	Integer 
	| Bool 
	| Cell
	| List of datatype  

type return_type = 
	Void
| Data of datatype

type formal_args = FormalVar of datatype * string 
	
type vars = 
  Lit_Int of int
| Lit_Bool of bool
| Lit_List of vars list

type expr = 
  Id of string
| Vars of vars
| Paran of expr
| BinOpr of binopr * expr * expr
| Assoc of assoc * string
| Assign of string * expr
| Funcall of string * expr list
| Loc of string
| Target of string
| Src of string
| Visit of expr
| Pointer
| Null

type vdecl = 
	Define of datatype * string * expr

type stmt = 
  StmtBlk of stmt list
| Expr of expr
| Display
| Move of int
| MoveTo of string 
| Exit
| Revert
| Print of expr
| Return of expr
| ListAdd of string * expr
| If of expr * stmt * stmt

type main = {
		mainId : string;
		mainVars : vdecl list;
		body : stmt list; 
	}

type func = {
		funcId : string;
		formalArgs : formal_args list;
		reType : return_type;
		localVars : vdecl list;
		statements : stmt list;
	}

type funcs = 
  Main of main
| Func of func
| Load of string 
	
type program = 
	funcs list 

let string_of_dt = function
		Integer -> "int"
	| Cell -> "Cell"
	| List(e) -> "List "
	| Bool -> "Boolean"

let string_of_assoc = function
		Remove -> "remove" 
	| Next -> "clear" 
	| Head -> "peek"
	| Empty -> "isEmpty"
	| Up  -> "up"
	| Down -> "down"
	| Left -> "left"
	| Right -> "right"
	| Hleft -> "hasLeft"
	| Hright -> "hasRight"
	| Htop -> "hasTop"
	| Hbtm -> "hasBottom"
		
let rec string_of_rt = function
	 Void -> "void"
	| Data(e) -> string_of_dt e

let string_of_op = function
	  Add -> "+" 
	| Sub -> "-" 
	| Mul -> "*" 
	| Div -> "/"
	| Eql -> "==" 
	| Neq -> "!="
	| Lsr -> "<" 
	| Leq -> "<=" 
	| Gtr -> ">" 
	| Geq -> ">=" 
	| Pow -> "^"
	| Mod -> "%"
	| And -> "&&"
	| Or -> "||"
	| Not -> "!"

let rec evalListexpr = function
	| [] -> ""
	| hd::[] -> string_of_var hd
	| hd::tl -> string_of_var hd ^ "," ^ evalListexpr tl 
and string_of_var = function
	| Lit_Int(f) -> string_of_int f
	| Lit_Bool(f) -> string_of_bool f
	| Lit_List(f) -> "new List( new Object [] {" ^ evalListexpr f ^ "})"

let rec string_of_expr = function
	Vars(e) ->
			string_of_var e
	| Id(s) -> s
	| BinOpr(o, e1, e2) ->
			begin
				match o with 
					| Pow -> "Math.pow(" ^ string_of_expr e1 ^ " , " ^ string_of_expr e2 ^ ")"
					| Not -> "!" ^ string_of_expr e1
					| _ -> 
							string_of_expr e1 ^ " " ^ (match o with
								  Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
								| Eql -> "==" | Neq -> "!="
								| Lsr -> "<" | Leq -> "<=" | Gtr -> ">" | Geq -> ">=" | And -> "&&" | Or -> "||" | Mod -> "%"| Pow -> "^" | Not -> "!")  
							^ " " ^ string_of_expr e2
			end
	| Assign(v, e) -> v ^ " = " ^ string_of_expr e
	| Funcall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	| Assoc(f, e) ->  begin
											match f with
											| Remove ->"(Cell)"^ e ^ "." ^ string_of_assoc f ^"()"
											| Next -> e ^ "." ^ string_of_assoc f ^"()"
											| Head -> "(Cell)"^ e ^ "." ^ string_of_assoc f ^"()"
											| Empty -> e ^ "." ^ string_of_assoc f ^"()"
											| _ ->  "AMLJava." ^ string_of_assoc f ^"()"
										end
	| Paran(e1) -> " ( " ^ string_of_expr e1 ^ " ) " 
	| Loc(e) -> e^".get_Loc()"
	| Target(e) -> e^".isTarget()"  
	| Src(e) -> e^".isSource()"
	| Visit(e) -> string_of_expr e ^ ".getVisited()"
	| Pointer -> "AMLJava.current"
	| Null -> "null"

let rec string_of_stmt = function
	  StmtBlk(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> string_of_expr expr ^ ";\n";
	| ListAdd(s,t) ->  s ^ ".add(" ^ string_of_expr t ^ ");\n"
	| Move(e) -> 
								begin
									match e with 
										| 1 -> "AMLJava.move_U();\n"
										| 2 -> "AMLJava.move_D();\n"
										| 3 -> "AMLJava.move_R();\n"
										| 4 -> "AMLJava.move_L();\n"
										| _ -> ""
								end
	| Exit -> "return;\n"
	| Revert -> "AMLJava.revert();\n"
	| Display -> "AMLJava.display();\n"
	| Print(e) -> "System.out.println ((" ^  string_of_expr e ^ "));\n"
	| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
	| If(e, s, StmtBlk([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
	| If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
	| MoveTo(x) -> "AMLJava.move(" ^ x ^");\n"
	
let string_of_vdecl = function 
	Define(dtt, nm, v) ->  string_of_dt dtt ^ " " ^ nm ^ " = " ^ string_of_expr v ^ ";\n"

let string_of_fparam = function
	FormalVar(dt,s) -> string_of_dt dt ^ " " ^ s

let string_of_func (func) = 
	"Function name : " ^ func.funcId ^ "\n" ^ 
	"Formal Parameter(s) : " ^ String.concat "," (List.map string_of_fparam func.formalArgs) ^ "\n" ^
	"Return Type: " ^ "\n" ^ string_of_rt func.reType	
 
let string_of_fdecl  = function
	| Func(fdecl) -> 
			"\npublic static "^ string_of_rt fdecl.reType^" " ^ fdecl.funcId ^ "(" ^ String.concat ", " (List.map string_of_fparam fdecl.formalArgs) ^ "){\n" ^
			String.concat "" (List.map string_of_vdecl fdecl.localVars) ^
			String.concat "" (List.map string_of_stmt fdecl.statements) ^
			"}\n"
	| Main(fdecl) -> 
			String.concat "" (List.map string_of_vdecl fdecl.mainVars) ^
			String.concat "" (List.map string_of_stmt fdecl.body) ^
			"}\n"
	| Load(str) -> begin 
									match str with
									| "random" -> "public static void main(String[] args){\nAMLJava.buildMaze(\""^ str ^"\");"
									| _ -> "public static void main(String[] args){\nAMLJava.buildMaze(\""^ str ^".txt\");"
								end
let string_of_program (funcs) prog_name = "import java.util.*;\n\npublic class " ^ prog_name ^ "{\n" ^ (String.concat "\n" (List.map string_of_fdecl funcs)) ^ "}" 
