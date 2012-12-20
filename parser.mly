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
%token LISTADD LISTREMOVE LISTCLEAR LISTHEAD LISTEMPTY
%token AND OR NOT
%token <string> ID
%token <int> NUM_LITERAL
%token EOF

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
| HASH LOAD MINUS RANDOM { Load("random") }

func_decl:
	ENTRY LPAREN RPAREN RTYPE VOID LBRACE vdecl_list stmt_list RBRACE 
		{ Main({
					mainId = "main";
					mainVars = List.rev $7;
					body = $8;
					})
		}  
|	FUNC ID LPAREN formal_args RPAREN RTYPE return_type LBRACE vdecl_list stmt_list RBRACE		
		{ Func({
					funcId = $2;
					formalArgs = List.rev $4;
					reType = $7;
					localVars = List.rev $9;
					statements = $10;
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
	
vdecl_list: 
/* No variable declaration */ { [] }
|  vdecl_list vdecl { $2 :: $1 }

vdecl:
 data_type ID ASSIGN vars STMTEND { Define($1,$2,Vars($4)) }
| data_type ID ASSIGN LPAREN CUR_POS RPAREN  STMTEND { Define($1,$2,Pointer) }
| data_type ID ASSIGN ID LPAREN actual_args RPAREN  STMTEND { Define($1,$2,Funcall($4,List.rev $6)) }

stmt_list:
  stmt { [$1] }
| stmt stmt_list { $1 :: $2 }

stmt:
  expr STMTEND { Expr($1) }
| RETURN expr STMTEND { Return($2) }
| impl_fns STMTEND{ $1 }
| move_stmt STMTEND { Move($1) }
| ID ASSOC LISTADD LPAREN expr RPAREN  STMTEND { ListAdd($1,$5) }
| MOVETO LPAREN ID RPAREN  STMTEND{ MoveTo($3) }
| LBRACE stmt_list RBRACE { StmtBlk($2) }
| IF LPAREN expr RPAREN stmt STMTEND { If($3, $5, StmtBlk([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }

impl_fns:
| REVERT LPAREN RPAREN { Revert }
| EXIT LPAREN RPAREN{ Exit }
| DISPLAY LPAREN RPAREN{ Display }
| PRINT LPAREN expr RPAREN{ Print($3) }

move_stmt:
| MOVEUP LPAREN RPAREN { 1 }
| MOVEDOWN LPAREN RPAREN { 2 }
| MOVERIGHT LPAREN RPAREN { 3 }
| MOVELEFT LPAREN RPAREN { 4 }

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
| NOT expr { BinOpr(Not,$2,$2) }
| expr AND expr { BinOpr(And,$1,$3) }
| expr OR expr { BinOpr(Or,$1,$3) }
| ID ASSIGN expr  { Assign($1,$3) }
| ID LPAREN actual_args RPAREN { Funcall($1,List.rev $3) }
| ID ASSOC LISTREMOVE LPAREN RPAREN  { Assoc(Remove,$1) }
| ID ASSOC LISTCLEAR LPAREN RPAREN { Assoc(Next,$1) }
| ID ASSOC LISTHEAD LPAREN RPAREN { Assoc(Head,$1) }
| ID ASSOC LISTEMPTY LPAREN RPAREN { Assoc(Empty,$1) }
| ID ASSOC UP LPAREN RPAREN { Assoc(Up,$1) }
| ID ASSOC DOWN LPAREN RPAREN { Assoc(Down,$1) }
| ID ASSOC LEFT LPAREN RPAREN { Assoc(Left,$1) }
| ID ASSOC RIGHT LPAREN RPAREN { Assoc(Right,$1) }
| ID ASSOC HASLEFT LPAREN RPAREN { Assoc(Hleft,$1) }
| ID ASSOC HASRIGHT LPAREN RPAREN { Assoc(Hright,$1) }
| ID ASSOC HASTOP LPAREN RPAREN { Assoc(Htop,$1) }
| ID ASSOC HASBTM LPAREN RPAREN { Assoc(Hbtm,$1) }
| LOC LPAREN ID RPAREN  { Loc($3) }
| SOURCE LPAREN ID RPAREN { Src($3) }
| ISTARGET LPAREN ID RPAREN { Target($3) }
| VISIT LPAREN expr RPAREN { Visit($3) }
| LPAREN CUR_POS RPAREN { Pointer } 

prim_vars:
  NUM_LITERAL { Lit_Int($1) }
| TRUE { Lit_Bool(true) }
| FALSE { Lit_Bool(false) }

vars:
  prim_vars { $1 }
| LSR complete_list GTR{ Lit_List($2) }

complete_list:
  LSQUARE RSQUARE{ [] }
| LSQUARE var_list RSQUARE { $2 }
| LSQUARE complete_list RSQUARE { [Lit_List($2)] }
| LSQUARE complete_list COMMA LSQUARE var_list RSQUARE RSQUARE { Lit_List($2) :: [Lit_List($5)] }

var_list:
  prim_vars	{ [$1] }
| prim_vars COMMA var_list { $1::$3 }

actual_args:
/* no arguments*/ { [] }
| expr { [$1] }
| actual_args COMMA expr { $3 :: $1 }