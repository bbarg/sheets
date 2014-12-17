/*
 * Sheets parser
 *
 * Authors: Amelia Brunner, Gabriel Blanco, Ben Barg
 * Copyright 2014, Symposium Software
 */

%{ open Ast;; %}


/////////////////////////////////////////////////////////////////////////////
//////////////////////////// TOKEN DECLARATIONS /////////////////////////////
/////////////////////////////////////////////////////////////////////////////


/* Punctuation Tokens */
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA PERIOD COLON EOF

/* Loop Keywords */
%token WHILE FOR BREAK CONTINUE

/* Conditional Keywords */
%token IF ELSE

/* Function Keywords */
%token FUNC GFUNC BLOCK RETURN

/* Type Keywords */
%token INT FLOAT

/* Operator Tokens */
%token PLUS MINUS TIMES DIVIDE 
%token EQ NEQ LT LEQ GT GEQ

/* Assignment Operator */
%token ASSIGN

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <int list> INT_ARRAY_LITERAL
%token <float list> FLOAT_ARRAY_LITERAL
%token <string> ID

/* Precedence Definition */
%nonassoc NOELSE ELSE
%right ASSIGN G_ASSIGN
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left PERIOD

%start program                           /* start symbol */
%type <Ast.program> program              /* type returned by program */

%%

/////////////////////////////////////////////////////////////////////////////
///////////////////////////// START SYMBOL //////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


program:                        /* [vdecls], [fdecls] */
    | /* Empty Program */       { [], [] } 
    | program vdecl SEMI        { ($2 :: fst $1), snd $1 }
    | program fdecl             { fst $1, ($2 :: snd $1) }
    | program gfdecl            { fst $1, ($2 :: snd $1) }


/////////////////////////////////////////////////////////////////////////////
////////////////////////////// FUNCTIONS ////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


/* func <type> <id>(arg1, arg2, ...):{ <statements> }; */
fdecl:
    FUNC type_name ID LPAREN formals_opt RPAREN COLON 
    LBRACE stmt_list_opt RBRACE
    {{
        r_type    = $2;                  (* return type *)
        fname     = $3;                  (* function name *)
        formals   = $5;                  (* list of arguments *)
        body      = $9;                  (* statement list *)
        isGfunc   = false;               (* false b/c not a gfunc *)
        blocksize = -1                   (* block size unused *)
    }}

/* gfunc <type> <id>(arg1, arg2, ...).[<blocksize>]:{ <statements> }; */
gfdecl:
    GFUNC type_name ID LPAREN formals_opt RPAREN blocksize COLON 
    LBRACE gfunc_stmt_list_opt RBRACE
    {{
        r_type    = $2;                  (* return type *)
        fname     = $3;                  (* gfunction name *)
        formals   = $5;                  (* list of arguments *)
        body      = $10;                 (* gfunction statement list *)
        isGfunc   = true;                (* true b/c a gfunc *)
        blocksize = $7                   (* block size *)
    }}

/* Optional Formal Arguments */
formals_opt:
    | /* Nothing */                       { [] }
    | formal_list                         { List.rev $1 }

formal_list:
    | vdecl                               { [$1] }
    | formal_list COMMA vdecl             { $3 :: $1 }

/* Blocksize in Gfunction Definition */
blocksize:
    | /* Nothing */                       { 1 }
    | PERIOD LBRACK INT_LITERAL RBRACK    { $3 }


/////////////////////////////////////////////////////////////////////////////
////////////////////////////// VARIABLES ////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


/* Type Declaration */
type_name:
    | INT LBRACK RBRACK                   { "int[]" }
    | FLOAT LBRACK RBRACK                 { "float[]" }
    | INT                                 { "int" }
    | FLOAT                               { "float" }

/* Optional Array Size Declaration */
array_opt:
    | /*Nothing*/                         { -1 }
    | LBRACK INT_LITERAL RBRACK           { $2 }

/* <type> name [<array_size>] */
vdecl:
    type_name ID array_opt 
    {{ 
        v_type   = $1;                   (* variable type *)
        v_name   = $2;                   (* variable name *)
        a_size   = $3;                   (* array size *)        
    }}

vdecl_list:
    | vdecl SEMI                          { [$1] }
    | vdecl_list vdecl SEMI               { $2 :: $1 }


/////////////////////////////////////////////////////////////////////////////
///////////////////////////// STATEMENTS ////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


/* Optional List of Statements */
stmt_list_opt:
    | /* Nothing */                       { [] }
    | stmt_list                           { List.rev $1 }

stmt_list:
    | stmt                                { [$1] }    
    | stmt_list stmt                      { $2 :: $1 }

/* Optional List of Gfunction Statements */
gfunc_stmt_list_opt:
    | /* Nothing */                       { [] }
    | gfunc_stmt_list                     { List.rev $1 }

gfunc_stmt_list:
    | gstmt                               { [$1] }
    | gfunc_stmt_list gstmt               { $2 :: $1 }

/* Optional List of Arguments */
args_opt:
    | /* Nothing */                       { [] }
    | args_list                           { List.rev $1 }

args_list:
    | expr                                { [$1] }                               
    | args_list COMMA expr                { $3 :: $1 }

/* Statements are found in the body of a functions, Gstatements 
 * are found in Gfunctions. Gstatements contail all statements
 * as well as access to the special Block construct. */
stmt:
    | vdecl SEMI                                        { Vdecl($1) }
    | expr SEMI                                         { Expr($1) }
    | RETURN expr SEMI                                  { Return($2) }
    | assign_expr ASSIGN expr SEMI                      { Assign($1, $3) }    
    | vdecl ASSIGN expr SEMI                            { Init($1, $3) }
    | LBRACE stmt_list RBRACE                           { Block(List.rev $2) }
    | IF bool_block COLON block_body %prec NOELSE       { If($2, $4, Block[] ) }   
    | IF bool_block COLON block_body ELSE COLON block_body    { If($2, $4, $7) } 
    | FOR for_pt1 for_pt2 for_pt3 COLON block_body      { For($2, $3, $4, $6) }
    | WHILE bool_block COLON block_body                 { While($2, $4) }

gstmt:
    | vdecl SEMI                                        { Vdecl($1) }
    | expr SEMI                                         { Expr($1) }
    | blockexpr SEMI                                    { Expr($1) }
    | g_assign_expr ASSIGN expr SEMI                    { Assign($1, $3) }
    | g_assign_expr ASSIGN blockexpr SEMI               { Assign($1, $3) }    
    | vdecl ASSIGN expr SEMI                            { Init($1, $3) }
    | vdecl ASSIGN blockexpr SEMI                       { Init($1, $3) }
    | IF bool_block COLON gblock_body %prec NOELSE      { If($2, $4, Block([])) }   
    | IF bool_block COLON gblock_body ELSE COLON gblock_body  { If($2, $4, $7) } 
    | FOR gfor_pt1 gfor_pt2 gfor_pt3 COLON gblock_body  { For($2, $3, $4, $6) }
    | WHILE bool_block COLON gblock_body                { While($2, $4) }

/* Conditional and Loop Statements*/
bool_block: LPAREN bool_expr RPAREN                     { $2 }

block_body:  LBRACE loop_stmt_list RBRACE               { Block(List.rev $2) }
gblock_body: LBRACE gloop_stmt_list RBRACE              { Block(List.rev $2) }

for_pt1: LPAREN stmt                                    { $2 }
for_pt2: bool_expr SEMI                                 { $1 }
for_pt3: stmt RPAREN                                    { $1 }

gfor_pt1: LPAREN gstmt                                  { $2 }
gfor_pt2: bool_expr SEMI                                { $1 }
gfor_pt3: gstmt RPAREN                                  { $1 }

/* Loops can contain all normal expressions, and also Break and Continues */
loop_stmt_list:
    | /* Nothing */                                     { [] }
    | stmt_list                                         { $1 }
    | loop_stmt_list loopexpr                           { $2 :: $1 }

gloop_stmt_list:
    | /* Nothing */                                     { [] } 
    | gfunc_stmt_list                                   { $1 }
    | gloop_stmt_list loopexpr                          { $2 :: $1 }


/////////////////////////////////////////////////////////////////////////////
///////////////////////////// EXPRESSIONS ///////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


loopexpr:
    | CONTINUE SEMI                   { Continue }
    | BREAK SEMI                      { Break }

blockexpr:
    | BLOCK PERIOD ID                            { BlockAcc($3, Literal_int(-1)) }
    | BLOCK PERIOD ID LBRACK INT_LITERAL RBRACK  { BlockAcc($3, Literal_int($5)) }
    | BLOCK PERIOD ID LBRACK ID RBRACK           { BlockAcc($3, Id($5)) }

bool_expr:
    | expr EQ expr                    { Binop($1, Equal,   $3) }
    | expr NEQ expr                   { Binop($1, Neq,     $3) }
    | expr LT expr                    { Binop($1, Less,    $3) }
    | expr LEQ expr                   { Binop($1, Leq,     $3) }
    | expr GT expr                    { Binop($1, Greater, $3) }
    | expr GEQ expr                   { Binop($1, Geq,     $3) }

array_expr:
    | ID                              { Id($1) }
    | array_literal                   { $1 }

assign_expr:
    | array_expr LBRACK expr RBRACK   { ArrayAcc($1, $3) }   
    | ID                              { Id($1) }

g_assign_expr:
    | blockexpr                       { $1 }
    | assign_expr                     { $1 }

/* Literals */
literal:
    | INT_LITERAL                     { Literal_int($1) }
    | FLOAT_LITERAL                   { Literal_float($1) }
    | array_literal                   { $1 }

array_literal:
    | LBRACK int_literal_list RBRACK          { Literal_int_a(List.rev $2) }
    | LBRACK float_literal_list RBRACK        { Literal_float_a(List.rev $2) }

int_literal_list:
    | INT_LITERAL                             { [$1] }
    | int_literal_list COMMA INT_LITERAL      { $3 :: $1 }

float_literal_list:
    | FLOAT_LITERAL                           { [$1] }
    | float_literal_list COMMA FLOAT_LITERAL  { $3 :: $1 }

/* Expressions */
expr:
    | literal                         { $1 }
    | ID LPAREN args_opt RPAREN       { Call($1, $3) }
    | array_expr LBRACK expr RBRACK   { ArrayAcc($1, $3) }
    | ID                              { Id($1) }
    | bool_expr                       { $1 }
    | expr PLUS expr                  { Binop($1, Plus, $3) }
    | expr MINUS expr                 { Binop($1, Minus, $3) }
    | expr TIMES expr                 { Binop($1, Times, $3) }
    | expr DIVIDE expr                { Binop($1, Divide, $3) }
    | LPAREN expr RPAREN              { $2 }   
