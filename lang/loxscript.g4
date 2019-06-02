grammar loxscript;

program     : declaration* EOF ;

declaration : class_decl | fun_decl | let_decl | statement ;
class_decl  : 'class' IDENT ( '<' IDENT )? '{' function* '}' ;
fun_decl    : 'fun' function ;
let_decl    : 'let' IDENT ( '=' expression )? ';' ;

function    : IDENT '(' parameters? ')' block ;
parameters  : IDENT ( ',' IDENT )* ;
arguments   : expression ( ',' expression )* ;
block       : '{' declaration* '}' ;

statement   : for_stmt | if_stmt | while_stmt
            | break_stmt | cont_stmt | return_stmt
            | assign_stmt | expr_stmt
            | block ;
for_stmt    : 'for' ( let_decl | assign_stmt | expr_stmt | ';' )
            | expression? ';'
            | ( assign | expression )?
            | block ;
if_stmt     : 'if' expression block ( 'elif' expression block )* ( 'else' block )? ;
while_stmt  : 'while' expression block ;
break_stmt  : 'break' ';' ;
cont_stmt   : 'continue' ';' ;
return_stmt : 'return' expression? ';' ;
expr_stmt   : expression ';' ;
assign_stmt : assign ';' ;
assign      : ( call '.' )? IDENT '=' expression ;

expression  : logical_or ;
logical_or  : logical_and ( 'or' logical_and )* ;
logical_and : equality ( 'and' equality )* ;
equality    : comparison ( ( '==' | '!=' ) comparison )* ;
comparison  : addition ( ( '<' | '<=' | '>' | '>=' ) addition )* ;
addition    : multiply ( ( '+' | '-' ) multiply )* ;
multiply    : unary ( ( '*' | '/' | '%' ) unary )* ;
unary       : ( '-' | 'not' ) unary | primary ;
call        : primary ( '(' arguments? ')' | '.' IDENT )* ;

primary     : 'none' | 'true' | 'false' | 'self'
            | NUM | STR | IDENT
            | 'super' '.' IDENT
            | '(' expression ')'
            ;

NUM         : DIGIT+ ( '.' DIGIT+ )? ;
STR         : '"' .*? '"' ;
IDENT       : ALPHA ( ALPHA | DIGIT )* ;
ALPHA       : 'a'..'z' | 'A'..'Z' | '_' ;
DIGIT       : '0'..'9' ;

COMMENT     : '#' .*? '\n' -> skip ;
WHITESPACE  : [ \t\r\n]+   -> skip ;
