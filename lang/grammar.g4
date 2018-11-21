grammar loxscript;

program     : declaration* EOF ;

declaration : statement ;

statement   : expression ';' ;

expression  : logical_or ;
logical_or  : logical_and ( 'or' logical_and )* ;
logical_and : equality ( 'and' equality )* ;
equality    : comparison ( ( '==' | '!=' ) comparison )* ;
comparison  : addition ( ( '<' | '<=' | '>' | '>=' ) addition )* ;
addition    : multiply ( ( '+' | '-' ) multiply )* ;
multiply    : unary ( ( '*' | '/' | '%' ) unary )* ;
unary       : ( '-' | 'not' ) unary | primary ;
primary     : 'none' | 'true' | 'false'
            | NUM | STR | IDENT
            | '(' expression ')'
            ;
