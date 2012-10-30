%{ open Ast 
	let parse_error pErr = 
	print_endline pErr;
	flush stdout
%}

%token LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE
%token PLUS MINUS TIMES DIVIDE MOD EXP 
%token ASSOC ASSIGN
%token GTR LSR GTREQL LSREQL NEQ EQ 
%token TRUE FALSE
%token STMTEND COMMA RTYPE HASH 
%token EXIT RETURN FUNC ENTRY VOID LOAD RANDOM NULL
%token INTEGER BOOLEAN CELL LIST
%token IF ELSE PRINT DISPLAY 
%token MOVEUP MOVEDOWN MOVELEFT MOVERIGHT MOVETO CUR_POS
%token ISTARGET VISIT SOURCE REVERT LOC
%token LEFT RIGHT UP DOWN HASLEFT HASRIGHT HASTOP HASBTM 
%token LISTADD LISTREMOVE LISTNEXT LISTHEAD LISTEMPTY
%token AND OR NOT
%token <string> ID
%token <int> NUM_LITERAL

%nonassoc ELSE
%left GTR LSR GTREQL LSREQL NEQ EQ 
%left PLUS MINUS
%left TIMES DIVIDE
%left MOD
%right ASSIGN EXP
%left OR
%left AND
%right NOT

%start program
%type <Ast.program> program 

%%
	
program: 
  /* empty code */	{ [] }
| program pre_process { $2 :: $1 }
| program func_decl { $2 :: $1 }

pre_process:
	HASH LOAD LSR ID GTR  { Load($4) }
| HASH LOAD MINUS RANDOM { Load("Random") }

func_decl:
	ENTRY LPAREN RPAREN RTYPE VOID LBRACE stmt_list RBRACE 
		{ Main({
					mainId = "Main";
					body = List.rev $7;
					})
		}  
|	FUNC ID LPAREN formal_args RPAREN RTYPE return_type LBRACE stmt_list RBRACE		
		{ Func({
					funcId = $2;
					formalArgs = List.rev $4;
					dataType = $7;
					statements = List.rev $9;
				})
		}
	
return_type:
   VOID { Void }
|  data_type { Data($1) }

data_type:
	INTEGER	{ Integer }
	|CELL     { Cell } 
	|BOOLEAN  { Bool }
	|formal_list { $1 }

formal_list: 
	|LIST LSR data_type GTR { List($3) }
			
formal_args: 
	/* no arguments */ { [] }
	|data_type ID   { [FormalVar($1, $2)] }
	|formal_args COMMA data_type ID { FormalVar($3, $4) :: $1 }  
	
stmt_list:
  stmt { [$1] }
| stmt_list stmt { $2 :: $1 }

stmt:
  expr STMTEND { Expr($1) }
| RETURN expr STMTEND { Return($2) }
| LBRACE stmt_list RBRACE { StmtBlk($2) }
| IF LPAREN expr RPAREN stmt STMTEND { If($3, $5, StmtBlk([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }

expr: 
  vars { Vars($1) }
| ID { Id($1) }
| NULL { Null }
| LPAREN expr RPAREN { Paran($2) }
| expr PLUS expr { BinOpr(Add,$1,$3) }
| expr MINUS expr { BinOpr(Sub,$1,$3) }
| expr TIMES expr { BinOpr(Mul,$1,$3) }
| expr DIVIDE expr { BinOpr(Div,$1,$3) }
| expr EXP expr { BinOpr(Pow,$1,$3) }
| expr MOD expr { BinOpr(Mod,$1,$3) }
| expr EQ expr { BinOpr(Eql,$1,$3) }
| expr NEQ expr { BinOpr(Neq,$1,$3) }
| expr GTR expr { BinOpr(Gtr,$1,$3) }
| expr LSR expr { BinOpr(Lsr,$1,$3) }
| expr GTREQL expr { BinOpr(Geq,$1,$3) }
| expr LSREQL expr { BinOpr(Leq,$1,$3) }
| NOT expr { BoolOpr(Not,$2,$2) }
| expr AND expr { BoolOpr(And,$1,$3) }
| expr OR expr { BoolOpr(Or,$1,$3) }
| ID ASSIGN expr  { Assign($1,$3) }
| data_type ID { Define($1,$2) }
| data_type ID ASSIGN expr { AssignDef($1,$2,$4) }
| ID LPAREN actual_args RPAREN { Funcall($1,$3) }
| ID ASSOC LISTREMOVE LPAREN RPAREN  { Assoc(Remove,$1) }
| ID ASSOC LISTNEXT LPAREN RPAREN { Assoc(Next,$1) }
| ID ASSOC LISTHEAD LPAREN RPAREN { Assoc(Head,$1) }
| ID ASSOC LISTEMPTY LPAREN RPAREN { Assoc(Empty,$1) }
| ID ASSOC LISTADD LPAREN expr RPAREN  { ListAdd($1,$5) }
| ID ASSOC UP LPAREN RPAREN { Assoc(Up,$1) }
| ID ASSOC DOWN LPAREN RPAREN { Assoc(Down,$1) }
| ID ASSOC LEFT LPAREN RPAREN { Assoc(Left,$1) }
| ID ASSOC RIGHT LPAREN RPAREN { Assoc(Right,$1) }
| ID ASSOC HASLEFT LPAREN RPAREN { Assoc(Hleft,$1) }
| ID ASSOC HASRIGHT LPAREN RPAREN { Assoc(Hright,$1) }
| ID ASSOC HASTOP LPAREN RPAREN { Assoc(Htop,$1) }
| ID ASSOC HASBTM LPAREN RPAREN { Assoc(Hbtm,$1) }
| ID ASSOC SOURCE LPAREN RPAREN { Assoc(Src,$1) }
| LOC LPAREN ID RPAREN  { Loc($3) }
| ISTARGET LPAREN ID RPAREN { Target($3) }
| VISIT LPAREN ID RPAREN { Visit($3) }
| REVERT LPAREN RPAREN { Revert }
| EXIT LPAREN RPAREN{ Exit }
| DISPLAY LPAREN RPAREN{ Display }
| PRINT LPAREN ID RPAREN{ Print($3) }
| LPAREN CUR_POS RPAREN { Pointer } 
| MOVEUP { Move(1) }
| MOVEDOWN { Move(2) }
| MOVERIGHT { Move(3) }
| MOVELEFT { Move(4) }
| MOVETO NUM_LITERAL { Move($2) }

prim_vars:
  NUM_LITERAL { Lit_Int($1) }
| TRUE { Lit_Bool(True) }
| FALSE { Lit_Bool(False) }

vars:
  prim_vars { $1 }
| LSR complete_list GTR{ Lit_List($2) }

complete_list:
  LSQUARE RSQUARE{ [] }
| LSQUARE var_list RSQUARE { List.rev $2 }
| LSQUARE complete_list RSQUARE { List.rev $2 }
| LSQUARE complete_list COMMA LSQUARE var_list RSQUARE RSQUARE { List.append $2 $5 }

var_list:
  prim_vars	{ [$1] }
| var_list COMMA prim_vars { $3::$1 }

actual_args:
/* no arguments*/ { [] }
| expr { [$1] }
| actual_args COMMA expr { $3 :: $1 }