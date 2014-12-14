/*
 * Sheets parser
 *
 * Authors: Ben Barg, Amelia Brunner, Gabriel Blanco
 * Copyright 2014, Symposium Software
 */

%{
    open Three_tuple;;
    open Ast;;
%}

/////////////////////////////////////////////////////////////////////
/////////////////////////TOKEN DECLARATIONS//////////////////////////
/////////////////////////////////////////////////////////////////////

/* Punctuation Tokens */
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA PERIOD
%token COLON EOF BACKSLASH

/* Loop Keywords */
%token WHILE FOR IN BREAK CONTINUE

/* Conditional Keywords */
%token IF ELSE

/* Function Keywords */
%token FUNC GFUNC STRUCT RETURN

/* Type Keywords */
%token INT LONG FLOAT DOUBLE CHAR CONST TRUE FALSE STRING BLOCK BOOL

/* Operator Tokens */
%token LOR LAND OR XOR NOT AND 
%token EQ NEQ LT LEQ GT GEQ LSHIFT RSHIFT
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NEG

/* G Operator Tokens */
%token G_LOR G_LAND G_OR G_XOR G_NOT G_AND 
%token G_EQ G_NEQ G_LT G_LEQ G_GT G_GEQ G_LSHIFT G_RSHIFT 
%token G_PLUS G_MINUS G_TIMES G_DIVIDE G_MOD G_ASSIGN G_NEG

%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <float> FLOAT_LITERAL
%token <int list> INT_ARRAY_LITERAL
%token <string list> STRING_ARRAY_LITERAL
%token <float list> FLOAT_ARRAY_LITERAL
%token <char> CHAR_LITERAL
%token <char list> CHAR_ARRAY_LITERAL
%token <bool> BOOL_LITERAL
%token <bool list> BOOL_ARRAY_LITERAL

%token <string> ID

/* Precedence Definition */
%nonassoc NOELSE ELSE

%right ASSIGN G_ASSIGN

%left EQ NEQ G_EQ G_NEQ
%left LAND LOR G_LAND G_LOR
%left LT LEQ GT GEQ G_LT G_LEQ G_GT G_GEQ
%left AND OR XOR NOT G_AND G_OR G_XOR G_NOT 

%left PLUS MINUS G_PLUS G_MINUS
%left TIMES DIVIDE G_TIMES G_DIVIDE
%left MOD G_MOD

%left LSHIFT RSHIFT G_LSHIFT G_RSHIFT
%right NEG G_NEG

%left PERIOD

/////////////////////////////////////////////////////////////////////
////////////////////////PROGRAM PARSING//////////////////////////////
/////////////////////////////////////////////////////////////////////

%start program                           /* start symbol */
%type <Ast.program> program              /* type returned by program */

%%

ident:
    | ID                              { Id($1) }
datatype:
    | TYPE                            { type_of_string $1 }
    | TYPE LSQUARE RSQUARE            { ArrayType(type_of_string $1) }
lvalue:
    | ident                           { Variable($1) }
    | ident LSQUARE expr RSQUARE      { ArrayAccess($1, $3) }
    | ident PERIOD ident              { StructAccess($1, $3) }

program:                        /* [vdecls], [sdef], [fdecls] */
    | /* Empty Program */       { [], [], [] } 
    | program vdecl             { ($2 :: first $1), second $1, third $1 }
    | program sdef              { first $1, ($2 :: second $1), third $1 }    
    | program fdecl             { first $1, second $1, ($2 :: third $1) }
    | program gfdecl            { first $1, second $1, ($2 :: third $1) }

/////////////////////////////////////////////////////////////////////
///////////////////////////FUNCTIONS/////////////////////////////////
/////////////////////////////////////////////////////////////////////

/* func int named_func(args):{ <statements>... }; */
fdecl:
    FUNC datatype ident LPAREN formals_opt RPAREN COLON 
    LBRACE stmt_list_opt RBRACE
    {{
        r_type    = $2;
        fname     = $3;
        formals   = $5;
        body      = $9; 
        isGfunc   = false;               (* false b/c not a gfunc *)
        blocksize = -1;
    }}

/* gfunc int named_gfunc(args).[5]:{ <statements>... }; */
gfdecl:
    GFUNC datatype ident LPAREN formals_opt RPAREN blocksize COLON 
    LBRACE gfunc_stmt_list_opt RBRACE
    {{
        r_type    = $2;
        fname     = $3;                  (* gfunc name *)
        formals   = $5;                  (* argument list *)
        body      = $10;                 (* gfunc statement list *)
        isGfunc   = true;                (* true b/c a gfunc *)
        blocksize = $7                   (* block size *)
    }}

/* TODO: Calling functions from inside other functions? */
/* TODO: mixed variable/statements */

/* we give the option of having no arguments with this switch */
formals_opt:
    | /* Nothing */                        { [] }
    | formal_list                          { List.rev $1 }

/* Arguments are declared as a list of variables delimitted by commas */
formal_list:
    | vdecl                                { [$1] }
    | formal_list COMMA vdecl              { $3 :: $1 }

blocksize:
    | /* Nothing */                        { 1 }
    | PERIOD LBRACK INT_LITERAL RBRACK     { $3 }

/////////////////////////////////////////////////////////////////////
///////////////////////////VARIABLES/////////////////////////////////
/////////////////////////////////////////////////////////////////////

type_name:
    | INT LBRACK expr RBRACK            { ("int[]",    false, $3) }
    | INT LBRACK RBRACK                 { ("int[]",    false, Literal_int(0)) }
    | FLOAT LBRACK expr RBRACK          { ("float[]",  false, $3) }
    | FLOAT LBRACK RBRACK               { ("float[]",  false, Literal_int(0)) }
    | LONG LBRACK expr RBRACK           { ("long[]",   false, $3) }
    | LONG LBRACK RBRACK                { ("long[]",   false, Literal_int(0)) }
    | DOUBLE LBRACK expr RBRACK         { ("double[]", false, $3) }
    | DOUBLE LBRACK RBRACK              { ("double[]", false, Literal_int(0)) }
    | STRING LBRACK expr RBRACK         { ("string[]", false, $3) }
    | STRING LBRACK RBRACK              { ("string[]", false, Literal_int(0)) }
    | CHAR LBRACK expr RBRACK           { ("char[]",   false, $3) }
    | CHAR LBRACK RBRACK                { ("char[]",   false, Literal_int(0)) }
    | BOOL LBRACK expr RBRACK           { ("bool[]",   false, $3) }
    | BOOL LBRACK RBRACK                { ("bool[]",   false, Literal_int(0)) }
    | INT                               { ("int",      false, Literal_int(-1)) }
    | FLOAT                             { ("float",    false, Literal_int(-1)) }
    | LONG                              { ("long",     false, Literal_int(-1)) }
    | DOUBLE                            { ("double",   false, Literal_int(-1)) }
    | STRING                            { ("string",   false, Literal_int(-1)) }
    | CHAR                              { ("char",     false, Literal_int(-1)) }
    | BOOL                              { ("bool",     false, Literal_int(-1)) }
    /* TODO: find out why this line produces Shift/Reduce conflict */
    //| STRUCT ID                         { ($2,       true) }

/* <const> <type> name */
vdecl:
    | const datatype ident SEMI
        {{ 
            isConst  = $1;                  (* true or false for if const *)            
            v_type   = $2;                  (* variable type *)
            v_name   = $3;                  (* variable name *)
            a_size   = Literal_int(-1);     (* array size *)
        }}
    | const datatype ident LSQUARE expr RSQUARE SEMI
        {{
            isConst  = $1;                  (* true or false for if const *)            
            v_type   = $2;                  (* variable type *)
            v_name   = $3;                  (* variable name *)
            a_size   = $5;                  (* array size *)
        }}

/* <const> <type> name */
param:
    | const datatype ident
        {{ 
            isConst  = $1;                  (* true or false for if const *)            
            v_type   = $2;                  (* variable type *)
            v_name   = $3;                  (* variable name *)
        }}
    | const datatype ident LSQUARE RSQUARE
        {{
            isConst  = $1;                  (* true or false for if const *)            
            v_type   = $2;                  (* variable type *)
            v_name   = $3;                  (* variable name *)
        }}

const:
    | /* Nothing */                          { false }
    | CONST                                  { true }

vdecl_list:
    | vdecl                                  { [$1] }
    | vdecl_list vdecl                       { $2 :: $1 }

sdef:
    STRUCT ident COLON LBRACE vdecl_list RBRACE
    {{
        s_name = $2;
        s_elements = List.rev $5
    }}

/////////////////////////////////////////////////////////////////////
//////////////////////////STATEMENTS/////////////////////////////////
/////////////////////////////////////////////////////////////////////

stmt_list_opt:
    | /* Nothing */                    { [] }
    | stmt_list                        { List.rev $1 }

stmt_list:
    | stmt                             { [$1] }    
    | stmt_list stmt                   { $2 :: $1 }

gfunc_stmt_list_opt:
    | /* Nothing */                    { [] }
    | gfunc_stmt_list                  { List.rev $1 }

gfunc_stmt_list:
    | gstmt                            { [$1] }
    | gfunc_stmt_list gstmt            { $2 :: $1 }

args_opt:
    | /* Nothing */                      { [] }
    | args_list                        { List.rev $1 }

args_list:
    | expr                             { [$1] }                               
    | args_list COMMA expr             { $3 :: $1 }

/*    Note about gstmt vs stmts:
 *  The difference between these two is that gstmts are the
 *  statements that are allowed to be called within gfuncs and
 *  stmts are the statements that are allowed to be called within
 *  'normal' functions- specifically, statements in gfuncs cannot
 *  call return or have any gpu-expressions, and statements in
 *  normal functions cannot reference block structs.
 *
 *  gstmts are DIFFERENT from statements containing gpu-expressions!
 * 
 * Some formatting assumptions:
 * for( <expr_opt>; <bool_expr_opt>; <expr_opt> ){ <statements> };
 * for <variable name> in <array name> : { <statements> }; 
 * if( <bool_expr> ){ <statements> };
 * if( <bool_expr> ){ <statements> }; else { <statements> }; 
 */

stmt:
    | vdecl SEMI                                        { Vdecl($1) }
    | expr SEMI                                         { Expr($1) }
    | gexpr SEMI                                        { Expr($1) }
    | RETURN expr SEMI                                  { Return($2) }
    | RETURN gexpr SEMI                                 { Return($2) }
    | assign_expr ASSIGN expr SEMI                      { Assign($1, $3) }
    | assign_expr ASSIGN gexpr SEMI                     { Assign($1, $3) }    
    | vdecl ASSIGN expr SEMI                            { Init($1, $3) }
    | vdecl ASSIGN gexpr SEMI                           { Init($1, $3) }
    | LBRACE stmt_list RBRACE                           { Block(List.rev $2) }
    | IF bool_block COLON block_body %prec NOELSE       { If($2, $4, Block[] ) }   
    | IF bool_block COLON block_body ELSE COLON block_body    { If($2, $4, $7) } 
    | FOR for_pt1 for_pt2 for_pt3 COLON block_body      { For($2, $3, $4, $6) }
    | FOR ID IN array_expr COLON block_body             { ForIn(Id($2), $4, $6) }
    | WHILE bool_block COLON block_body                 { While($2, $4) }

gstmt:
    | vdecl SEMI                                        { Vdecl($1) }
    | expr SEMI                                         { Expr($1) }
    | blockexpr SEMI                                    { Expr($1) }
    | assign_expr ASSIGN expr SEMI                      { Assign($1, $3) }
    | assign_expr ASSIGN blockexpr SEMI                 { Assign($1, $3) }    
    | vdecl ASSIGN expr SEMI                            { Init($1, $3) }
    | vdecl ASSIGN blockexpr SEMI                       { Init($1, $3) }
    | IF bool_block COLON gblock_body %prec NOELSE      { If($2, $4, Block([])) }   
    | IF bool_block COLON gblock_body ELSE COLON gblock_body    { If($2, $4, $7) } 
    | FOR for_pt1 for_pt2 for_pt3 gblock_body           { For($2, $3, $4, $5) }
    | FOR ID IN array_expr COLON gblock_body            { ForIn(Id($2), $4, $6) }
    | WHILE bool_block gblock_body                      { While($2, $3) }

bool_block: LPAREN bool_expr RPAREN                     { $2 }

/* note that loop_stmt_list can be used from inside if/else blocks */
block_body:  LBRACE loop_stmt_list RBRACE               { Block(List.rev $2) }
gblock_body: LBRACE gloop_stmt_list RBRACE              { Block(List.rev $2) }

for_pt1: LPAREN expr SEMI                               { $2 }
for_pt2: bool_expr SEMI                                 { $1 }
for_pt3: expr RPAREN                                    { $1 }
    
/* Loops can contain all normal expressions, and also Break and Continues */
loop_stmt_list:
    | /* Nothing */                                     { [] }
    | stmt_list                                         { $1 }
    | loop_stmt_list loopexpr                           { $2 :: $1 }

gloop_stmt_list:
    | /* Nothing */                                     { [] } 
    | gfunc_stmt_list                                   { $1 }
    | gloop_stmt_list loopexpr                          { $2 :: $1 }

/////////////////////////////////////////////////////////////////////
//////////////////////////EXPRESSIONS////////////////////////////////
/////////////////////////////////////////////////////////////////////

loopexpr:
    | CONTINUE SEMI                   { Continue }
    | BREAK SEMI                      { Break }

blockexpr:
    BLOCK PERIOD ID                   { StructId("Block", $3) }

bool_expr:
    | expr EQ expr                    { Binop($1, Equal, $3) }
    | expr NEQ expr                   { Binop($1, Neq, $3) }
    | expr LT expr                    { Binop($1, Less, $3) }
    | expr LEQ expr                   { Binop($1, Leq, $3) }
    | expr GT expr                    { Binop($1, Greater, $3) }
    | expr GEQ expr                   { Binop($1, Geq, $3) }
    | expr LAND expr                  { Binop($1, Land, $3) }
    | expr LOR expr                   { Binop($1, Lor, $3) }

literal:
    | INT_LITERAL                     { Literal_int($1) }
    //| CHAR_LITERAL                    { Literal_char($1) }
    | FLOAT_LITERAL                   { Literal_float($1) }
    | STRING_LITERAL                  { Literal_string($1) }
    | BOOL_LITERAL                    { Literal_bool($1) }
    | array_literal                   { $1 }

array_literal:
    | BOOL_ARRAY_LITERAL              { Literal_bool_a($1) }
    | STRING_ARRAY_LITERAL            { Literal_string_a($1) }
    | INT_ARRAY_LITERAL               { Literal_int_a($1) }
    | CHAR_ARRAY_LITERAL              { Literal_char_a($1) }
    | FLOAT_ARRAY_LITERAL             { Literal_float_a($1) }

/* expr are all the expressions EXCEPT:
    * those with blocks
    * comparison operators */
expr:
    | literal                         { $1 }
    | ident LPAREN args_opt RPAREN    { FunctionCall($1, $3) }

    | array_expr LBRACK expr RBRACK   { ArrayAccess($1, $3) }
    | ident PERIOD ident              { StructId($1, $3) }
    | ident                           { Id($1) }

    | expr AND expr                   { BinaryOp($1, And, $3) }    
    | expr OR expr                    { BinaryOp($1, Or, $3) }
    | expr XOR expr                   { BinaryOp($1, Xor, $3) }
    | expr NOT expr                   { BinaryOp($1, Not, $3) }
    | expr LSHIFT expr                { BinaryOp($1, Lshift, $3) }
    | expr RSHIFT expr                { BinaryOp($1, Rshift, $3) }
    | expr PLUS expr                  { BinaryOp($1, Plus, $3) }
    | expr MINUS expr                 { BinaryOp($1, Minus, $3) }
    | expr TIMES expr                 { BinaryOp($1, Times, $3) }
    | expr DIVIDE expr                { BinaryOp($1, Divide, $3) }
    | expr MOD expr                   { BinaryOp($1, Mod, $3) }
    | expr NEG expr                   { BinaryOp($1, Neg, $3) }

    | LPAREN expr RPAREN              { $2 }

    | lvalue ASSIGN expr              { Assign($1, $3) }
    | lvalue                          { Lval($1) }    


array_expr:
    | ID PERIOD ID                    { StructId($1, $3) }
    | ID                              { Id($1) }
    | array_literal                   { $1 }

assign_expr:
    | array_expr LBRACK expr RBRACK   { ArrayAcc($1, $3) }   
    | LPAREN assign_expr RPAREN       { $2 }
    | ID PERIOD ID                    { StructId($1, $3) }
    | ID                              { Id($1) }


gexpr:
    | gexpr AND gexpr                 { BinaryOp($1, And, $3) }    
    | gexpr G_OR gexpr                { BinaryOp($1, Or, $3) }
    | gexpr G_XOR gexpr               { BinaryOp($1, Xor, $3) }
    | gexpr G_NOT gexpr               { BinaryOp($1, Not, $3) }
    | gexpr G_LSHIFT gexpr            { BinaryOp($1, Lshift, $3) }
    | gexpr G_RSHIFT gexpr            { BinaryOp($1, Rshift, $3) }
    | gexpr G_PLUS gexpr              { BinaryOp($1, Plus, $3) }
    | gexpr G_MINUS gexpr             { BinaryOp($1, Minus, $3) }
    | gexpr G_TIMES gexpr             { BinaryOp($1, Times, $3) }
    | gexpr G_DIVIDE gexpr            { BinaryOp($1, Divide, $3) }
    | gexpr G_MOD gexpr               { BinaryOp($1, Mod, $3) }
    | gexpr G_NEG gexpr               { BinaryOp($1, Neg, $3) }
    | LPAREN gexpr RPAREN             { $2 }

