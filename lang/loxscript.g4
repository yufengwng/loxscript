grammar loxscript;

program     : declaration* EOF ;

declaration : fun_decl | let_decl | statement ;
fun_decl    : 'fun' function ;
let_decl    : 'let' IDENT ( '=' expression )? ';' ;

function    : IDENT '(' parameters? ')' ;
parameters  : IDENT ( ',' IDENT )* ;
arguments   : expression ( ',' expression )* ;

statement   : for_stmt | if_stmt | while_stmt | return_stmt | assign_stmt | expr_stmt | block ;
assign_stmt : assign ';' ;
expr_stmt   : expression ';' ;
for_stmt    : 'for' ( let_decl | assign_stmt | expr_stmt | ';' )
            | expression? ';'
            | ( assign | expression )?
            | block ;
if_stmt     : 'if' expression block ( 'elif' expression block )* ( 'else' block )? ;
return_stmt : 'return' expression? ';' ;
while_stmt  : 'while' expression block ;
assign      : IDENT '=' expression ;
block       : '{' declaration* '}' ;

expression  : logical_or ;
logical_or  : logical_and ( 'or' logical_and )* ;
logical_and : equality ( 'and' equality )* ;
equality    : comparison ( ( '==' | '!=' ) comparison )* ;
comparison  : addition ( ( '<' | '<=' | '>' | '>=' ) addition )* ;
addition    : multiply ( ( '+' | '-' ) multiply )* ;
multiply    : unary ( ( '*' | '/' | '%' ) unary )* ;
unary       : ( '-' | 'not' ) unary | primary ;
call        : primary ( '(' arguments? ')' )* ;

primary     : 'none' | 'true' | 'false'
            | NUM | STR | IDENT
            | '(' expression ')'
            ;

NUM         : DIGIT+ ( '.' DIGIT+ )? ;
STR         : '"' .*? '"' ;
IDENT       : ALPHA ( ALPHA | DIGIT )* ;
ALPHA       : 'a'..'z' | 'A'..'Z' | '_' ;
DIGIT       : '0'..'9' ;

COMMENT     : '#' .*? '\n' -> skip ;
WHITESPACE  : [ \t\r\n]+   -> skip ;
