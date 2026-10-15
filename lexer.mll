(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule lexer = parse
| digit+ as num  { NUM (int_of_string num) }
| "if"                    { IF }
| "else"                  { ELSE }
| "while"                 { WHILE }
|"do"                     {DO}
| "for"                   {FOR}
| ".."                    {DOTDOT}
| "scan"                  { SCAN }
| "sprint"                { SPRINT }
| "iprint"                { IPRINT }
| "int"                   { INT }
| "return"                { RETURN }
| "type"                  { TYPE }
| "void"                  { VOID }
| id as text              { ID text }
| '\"'[^'\"']*'\"' as str { STR str }
| '='                     { ASSIGN }
| "=="                    { EQ }
| "!="                    { NEQ }
| '>'                     { GT }
| '<'                     { LT }
| ">="                    { GE }
| "<="                    { LE }
| '+'                     { PLUS }
| '-'                     { MINUS }
| '*'                     { TIMES }
| '/'                     { DIV }
|'%'                      {MOD} (*課題４で%追加*)
| '^'                     { HAT }
|"++"                     {INC}
|"+="                     {PLUSEQ}
| '{'                     { LB  }
| '}'                     { RB  }
| '['                     { LS }
| ']'                     { RS }
| '('                     { LP  }
| ')'                     { RP  }
| ','                     { COMMA }
| ';'                     { SEMI }
(*課題１で追加*)
|"//"                     {comment lexbuf}
| [' ' '\t']          { lexer lexbuf }(* eat up whitespace *) 
|['\n']                {Lexing.new_line lexbuf;lexer lexbuf}
| eof                     { raise End_of_file }
| _                       { raise No_such_symbol }

(*課題１　comment用のルール*)
and comment = parse
|'\n'                     {Lexing.new_line lexbuf; lexer lexbuf}
|eof                      {raise End_of_file}
|_                        {comment lexbuf}
