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

val string_of_rt : return_type -> string
val string_of_dt : datatype -> string
val string_of_assoc : assoc -> string
val string_of_op : binopr -> string
val string_of_expr : expr -> string
val string_of_stmt : stmt -> string
val string_of_vdecl : vdecl -> string
val string_of_fdecl : funcs -> string
val string_of_fparam : formal_args -> string
val string_of_func : func -> string
val string_of_program : funcs list -> string -> string
