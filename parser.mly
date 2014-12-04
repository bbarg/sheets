/*
 * Sheets parser
 *
 * Authors: Ben Barg, Amelia Brunner
 * Copyright 2014, Symposium Software
 */

%{ 
    open Ast 
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
%token FUNC GFUNC MAIN STRUCT RETURN

/* Type Keywords */
%token INT LONG FLOAT DOUBLE CHAR CONST TRUE FALSE STRING BLOCK BOOL

/* Operator Tokens */
%token LOR LAND OR XOR NOT AND EQ NEQ LT LEQ GT GEQ LSHIFT RSHIFT
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NEG
%token G_LOR G_LAND G_OR G_XOR G_AND G_NOT G_EQ G_NEQ G_LT G_LEQ G_GT
%token G_LSHIFT G_RSHIFT G_PLUS G_MINUS G_TIMES G_DIVIDE G_MOD 
%token G_ASSIGN G_NEG

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

/* TODO: types? */

/* Precedence definition */
%left LOR LAND OR XOR NOT AND EQ NEQ LT LEQ GT GEQ LSHIFT RSHIFT
%left PLUS MINUS TIMES DIVIDE MOD
%left G_LOR G_LAND G_OR G_XOR G_AND G_NOT G_EQ G_NEQ G_LT G_LEQ G_GT
%left G_LSHIFT G_RSHIFT G_PLUS G_MINUS G_TIMES G_DIVIDE G_MOD 

%right ASSIGN G_ASSIGN NEG G_NEG

%nonassoc NOELSE ELSE

/////////////////////////////////////////////////////////////////////
////////////////////////PROGRAM PARSING//////////////////////////////
/////////////////////////////////////////////////////////////////////

%start program                           /* start symbol */
%type <Ast.program> program              /* type returned by program */

%%

/* Main Program */
program:
      /* empty program */       { [], [], [], [] } 
    | program vdecl             { ($2 :: fst $1), snd $1 }
    | program fdecl             { fst $1, ($2 :: snd $1) }
    | program gfdecl            { fst $1, ($2 :: snd $1) }
    | program sdecl             { ($2 :: fst $1), snd $1 }

/////////////////////////////////////////////////////////////////////
///////////////////////////FUNCTIONS/////////////////////////////////
/////////////////////////////////////////////////////////////////////

/* func int named_func(args):{; <statements>... }; */
fdecl:
   FUNC type_name ID LPAREN formals_opt RPAREN COLON 
   LBRACE SEMI vdecl_list stmt_lists RBRACE SEMI
   { { 
       fname   = $3;                    // function name
       formals = $5;                    // argument list
       locals  = List.rev $10;          // local variable list
       body    = List.rev $11           // normal statement list
   } }

/* gfunc int named_gfunc(args).[5]:{; <statements>... }; */
gfdecl:
   GFUNC type_name ID LPAREN formals_opt RPAREN PERIOD LBRACK INT_LITERAL 
   RBRACK COLON LBRACE SEMI vdecl_list gfunc_stmt_lists RBRACE SEMI
   { { 
       gname   = $3;                    // gfunc name
       formals = $5;                    // argument list
       locals  = List.rev $14;          // local variable list
       body    = List.rev $15;          // gfunc statement list
       blockSize = $9                   // block size
   } }

/* TODO: Calling functions from inside other functions? */
/* TODO: mixed variable/statements */

// we give the option of having no arguments with this switch
formals_opt:
      /* nothing */                     { [] }
    | formal_list                       { List.rev $1 }

// if there is content in the arguments, it needs to  be a 
// list of variable declarations separated by commas
formal_list:
      vdecl                             { [$1] }
    | formal_list COMMA vdecl           { $3 :: $1 }


/////////////////////////////////////////////////////////////////////
///////////////////////////VARIABLES/////////////////////////////////
/////////////////////////////////////////////////////////////////////

/* <const> <type> name */
vdecl:
    const str type_name ID
    { { 
        _type = $3;                     // variable type
        name = $4;                      // variable name
        isConst = $1;                   // true or false for if const
        str = $2                        // true or false for if a struct
    } }

str:
      STRUCT                            { true }
    | /* not struct */                  { false }

const:
      CONST                             { true }
    | /* not const */                   { false }

type_name:
      INT                               { "int" } 
    | FLOAT                             { "float" }
    | LONG                              { "long" }
    | DOUBLE                            { "double" }
    | STRING                            { "string" }
    | CHAR                              { "char" }
    | BOOL                              { "bool" }

vdecl_list:
      /* nothing */                     { [] }
    | vdecl_list vdecl SEMI             { $2 :: $1 }

sdecl:
    STRUCT ID COLON LBRACE SEMI vdecl_list RBRACE SEMI  { Struct( $2, $6) }

/////////////////////////////////////////////////////////////////////
//////////////////////////STATEMENTS/////////////////////////////////
/////////////////////////////////////////////////////////////////////

stmt_lists:
      /* nothing */                     { [] }
    | stmt_lists stmt SEMI               { $2 :: $1 }

gfunc_stmt_lists:
      /* nothing */                     { [] }
    | gfunc_stmt_lists gstmt SEMI       { $2 :: $1 }

/*                  Note about gstmt vs stmts:
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
 * for( <expr_opt>; <bool_expr_opt>; <expr_opt> ){; <statements> };
 * for <variable name> in <array name> : {; <statements> }; 
 * if( <bool_expr> ){; <statements> };
 * if( <bool_expr> ){; <statements> }; else {; <statements> }; 
 */


stmt:
      expr SEMI                                         { Expr($1) }
    | gexpr SEMI                                        { Expr($1) }
    | RETURN expr SEMI                                  { Return($2) }
    | RETURN gexpr SEMI                                 { Return($2) }
    | vdecl ASSIGN expr SEMI                            { Assign($1, $3) }
    | vdecl ASSIGN gexpr SEMI                           { Assign($1, $3) }
    | ID ASSIGN expr SEMI                               { Assign($1, $3) }
    | ID ASSIGN gexpr SEMI                              { Assign($1, $3) }
    | ID ASSIGN ID SEMI                                 { Assign($1, $3) }
    | IF bool_block block_body %prec NOELSE             { If($2, $3, []) }   
    | IF bool_block block_body ELSE block_body          { If($2, $3, $5) } 
    | FOR for_pt1 for_pt2 for_pt3 block_body            { For($2, $3, $4, $5) }
    | FOR ID IN ID COLON block_body                     { Forin($2, $4, $6) }
    | WHILE bool_block block_body                       { While($2, $3) }

gstmt:
      expr SEMI                                         { Expr($1) }
    | blockexpr SEMI                                    { Expr($1) }
    | vdecl ASSIGN expr SEMI                            { Assign($1, $3) }
    | vdecl ASSIGN blockexpr SEMI                       { Assign($1, $3) }
    | ID ASSIGN expr SEMI                               { Assign($1, $3) }
    | ID ASSIGN blockexpr SEMI                          { Assign($1, $3) }
    | ID ASSIGN ID SEMI                                 { Assign($1, $3) }
    | IF bool_block gblock_body %prec NOELSE            { If($2, $3, []) }   
    | IF bool_block gblock_body ELSE gblock_body        { If($2, $3, $5) } 
    | FOR for_pt1 for_pt2 for_pt3 gblock_body           { For($2, $3, $4, $5) }
    | FOR ID IN ID COLON gblock_body                    { Forin($2, $4, $6) }
    | WHILE bool_block gblock_body                      { While($2, $3) }


bool_block: LPAREN bool_expr RPAREN                     { $2 }

// note that loop_stmt_lists can be used from inside if/else blocks
block_body:  LBRACE SEMI loop_stmt_list RBRACE SEMI     { List.rev $3 }
gblock_body: LBRACE SEMI gloop_stmt_list RBRACE SEMI    { List.rev $3 }

for_pt1: LPAREN expr_opt SEMI                           { $2 }
for_pt2: bool_expr_opt SEMI                             { $1 }
for_pt3: expr_opt RPAREN                                { $1 }
    
// Loops can contain all normal expressions, and also Break and Continues
loop_stmt_list:
     /* Nothing */                                      { [] }
    | stmt_lists SEMI                                   { $1 }
    | loop_stmt_list loopexpr SEMI                      { $2 :: $1 }

gloop_stmt_list:
    /* Nothing */                                       { [] }
    | gfunc_stmt_lists SEMI                             { $1 }
    | gloop_stmt_list loopexpr SEMI                     { $2 :: $1 }


/////////////////////////////////////////////////////////////////////
//////////////////////////EXPRESSIONS////////////////////////////////
/////////////////////////////////////////////////////////////////////

loopexpr:
    CONTINUE                          { Continue }
    | BREAK                           { Break }

blockexpr:
    BLOCK PERIOD ID                   { StructID("Block", $3) }

bool_expr:
      expr EQ expr                    { Binop($1, Equal, $3) }
    | expr NEQ expr                   { Binop($1, Neq, $3) }
    | expr LT expr                    { Binop($1, Less, $3) }
    | expr LEQ expr                   { Binop($1, Leq, $3) }
    | expr GT expr                    { Binop($1, Greater, $3) }
    | expr GEQ expr                   { Binop($1, Geq, $3) }
    | expr LAND expr                  { Binop($1, Land, $3) }
    | expr LOR expr                   { Binop($1, Lor, $3) }

expr_opt:
      /* Nothing */                   { Noexpr }
    | expr                            { $1 }

bool_expr_opt:
      /* Nothing */                   { Noexpr }
    | bool_expr                       { $1 }

literal:
      INT_LITERAL                     { $1 }
    | CHAR_LITERAL                    { $1 }
    | FLOAT_LITERAL                   { $1 }
    | INT_ARRAY_LITERAL               { $1 }
    | CHAR_ARRAY_LITERAL              { $1 }
    | FLOAT_ARRAY_LITERAL             { $1 }
    | STRING_LITERAL                  { $1 }
    | STRING_ARRAY_LITERAL            { $1 }
    | BOOL_LITERAL                    { $1 }
    | BOOL_ARRAY_LITERAL              { $1 }

/* expr are all the expressions EXCEPT:
    * those with blocks
    * comparison operators */
expr:
      literal                         { Literal($1) }
    | ID                              { Id($1) }
    | ID PERIOD ID                    { StructID($1, $3) }
    | expr OR expr                    { Binop($1, Or, $3) }
    | expr XOR expr                   { Binop($1, Xor, $3) }
    | expr NOT expr                   { Binop($1, Not, $3) }
    | expr LSHIFT expr                { Binop($1, Lshift, $3) }
    | expr RSHIFT expr                { Binop($1, Rshift, $3) }
    | expr PLUS expr                  { Binop($1, Plus, $3) }
    | expr MINUS expr                 { Binop($1, Minus $3) }
    | expr TIMES expr                 { Binop($1, Times, $3) }
    | expr DIVIDE expr                { Binop($1, Divide, $3) }
    | expr MOD expr                   { Binop($1, Mod, $3) }
    | expr NEG expr                   { Binop($1, Neg, $3) }
    | LPAREN expr RPAREN              { $2 }

gexpr:
    | gexpr G_OR gexpr                { Binop($1, Or, $3) }
    | gexpr G_XOR gexpr               { Binop($1, Xor, $3) }
    | gexpr G_NOT gexpr               { Binop($1, Not, $3) }
    | gexpr G_LSHIFT gexpr            { Binop($1, Lshift, $3) }
    | gexpr G_RSHIFT gexpr            { Binop($1, Rshift, $3) }
    | gexpr G_PLUS gexpr              { Binop($1, Plus, $3) }
    | gexpr G_MINUS gexpr             { Binop($1, Minus $3) }
    | gexpr G_TIMES gexpr             { Binop($1, Times, $3) }
    | gexpr G_DIVIDE gexpr            { Binop($1, Divide, $3) }
    | gexpr G_MOD gexpr               { Binop($1, Mod, $3) }
    | gexpr G_NEG gexpr               { Binop($1, Neg, $3) }
    | LPAREN gexpr RPAREN             { $2 }



