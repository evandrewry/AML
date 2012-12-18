{open Parser}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = 
  parse [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }
| '-' { MINUS}
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
| '^' { EXP }
| '.' { ASSOC }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LSQUARE }
| ']' { RSQUARE }
| '<' { LSR }
| '>' { GTR }
| ';' { STMTEND }
| ',' { COMMA }
| ':' { RTYPE }
| '#' { HASH }
| ">=" { GTREQL }
| "<=" { LSREQL }
| "~=" { NEQ }
| "=" { EQ }
| ":=" { ASSIGN }
| "true" { TRUE }
| "false" { FALSE }
| "null" { NULL }
| "NOT" { NOT }
| "AND" { AND }
| "OR" { OR }
| "load" { LOAD }
| "random" { RANDOM }
| "return" { RETURN }
| "exit" { EXIT }
| "function" { FUNC }
| "main" { ENTRY }
| "void" { VOID }
| "print" { PRINT }
| "integer" { INTEGER }
| "bool" { BOOLEAN }
| "list" { LIST }
| "cell" { CELL }
| "if" { IF }
| "else" { ELSE }
| "display" { DISPLAY }
| "move_U" { MOVEUP }
| "move_D" { MOVEDOWN }
| "move_L" { MOVELEFT }
| "move_R" { MOVERIGHT }
| "move_To" { MOVETO }
| "get_Loc" { LOC }
| "isTarget" { ISTARGET }
| "visited" { VISIT }
| "isSource" { SOURCE }
| "revert" { REVERT }
| "left" { LEFT }
| "right" { RIGHT }
| "up" { UP }
| "down" { DOWN }
| "hasleft" { HASLEFT }
| "hasright" { HASRIGHT }
| "hastop" { HASTOP }
| "hasbottom" { HASBTM }
| "CPos" { CUR_POS }
| "add" { LISTADD }
| "remove" { LISTREMOVE } 
| "clear" { LISTCLEAR }
| "head" { LISTHEAD }
| "isEmpty" { LISTEMPTY } 
| ['-']?['1'-'9']digit*|'0' as amlex { NUM_LITERAL(int_of_string amlex) }
| letter(letter|digit)* as amlex { ID(amlex) }
| "/*" { multicmnt lexbuf}
| "//" { singlecmnt lexbuf}
| eof { EOF }

and multicmnt = 
	parse "*/" { token lexbuf}
|_ { multicmnt lexbuf}

and singlecmnt =
	parse "\n" { token lexbuf}
|_ { singlecmnt lexbuf}
