type binopr = Add | Sub | Mul | Div | Mod | Eql | Neq | Lsr | Leq | Gtr | Geq | Pow 

type assoc = Remove| Next | Head | Empty | Up | Down | Left | Right | Hleft | Hright | Htop | Hbtm | Src  

type boolvar = True | False 

type boolopr = And | Or | Not 

type datatype = 
	Integer 
	| Bool 
	| Cell
	| List of datatype  

type return_type = 
	Void
|   Data of datatype

type formal_args = FormalVar of datatype * string 
	
type vars = 
  Lit_Int of int
| Lit_Bool of boolvar
| Lit_List of vars list

type expr = 
  Id of string
| Vars of vars
| Paran of expr
| BinOpr of binopr * expr * expr
| BoolOpr of boolopr * expr * expr
| Assoc of assoc * string
| Assign of string * expr
| Define of datatype * string
| AssignDef of datatype * string * expr
| Funcall of string * expr list
| ListAdd of string * expr
| Move of int
| Exit
| Loc of string
| Display
| Target of string
| Visit of string
| Revert
| Print of string
| Pointer
| Null

type stmt = 
  StmtBlk of stmt list
| Expr of expr
| Return of expr
| If of expr * stmt * stmt

type main = {
		mainId : string;
		body : stmt list; 
	}

type func = {
		funcId : string;
		formalArgs : formal_args list;
		dataType : return_type;
		statements : stmt list;
	}

type funcs = 
  Main of main
| Func of func
| Load of string 
	
type program = 
	funcs list 
	