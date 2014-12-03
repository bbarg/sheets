/*
 * Sheets parser
 *
 * Authors: Ben Barg, Amelia Brunner
 * Copyright 2014, Symposium Software
 */

%{ 
    open Ast 
%}

/* Punctuation Tokens */
%token LAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA PERIOD
%token COLON EOF BACKSLASH

/* Loop Keywords */
%token WHILE FOR IN BREAK CONTINUE

/* Conditional Keywords */
%token IF ELSE NOELSE

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

/* Operator tokens
 *
 * Ordinarily we would need to deal with precedence of array operators
 * versus scalar operators, but we don't need to worry about their relative
 * precedence because we prohibit them on the same line. */

/* Precedence definition */
%left LOR LAND OR XOR NOT AND EQ NEQ LT LEQ GT GEQ LSHIFT RSHIFT
%left PLUS MINUS TIMES DIVIDE MOD
%left G_LOR G_LAND G_OR G_XOR G_AND G_NOT G_EQ G_NEQ G_LT G_LEQ G_GT
%left G_LSHIFT G_RSHIFT G_PLUS G_MINUS G_TIMES G_DIVIDE G_MOD 

%right ASSIGN G_ASSIGN NEG G_NEG

%nonassoc NOELSE ELSE ELIF

%start sheet            /* start symbol */
%type <Ast.program> sheet   /* type returned by program */

%%

/* Main Program */
sheet:
    | /* nothing */     { [], [], [] }
    | program vdecl     { ($2 :: fst $1), snd $1 }
    | program fdecl     { fst $1, ($2 :: snd $1) }
    | program gdecl     { fst $1, ($2 :: snd $1) }

/* Function Declarations */
/* func int named_func(args):{ <statements>... } */
fdecl:
   FUNC TYPE ID LPAREN formals_opt RPAREN COLON 
   LBRACE SEMI vdecl_list stmt_lists RBRACE SEMI
            { { fname   = $3;
              formals = $5;
              locals  = List.rev $10;
              body    = List.rev $11 } }

/* gfunc int named_gfunc(args).[5]:{ <statements>... } */
gfdecl:
   GFUNC TYPE ID LPAREN formals_opt RPAREN blocksize COLON
   LBRACE SEMI vdecl_list gfunc_stmt_lists RBRACE SEMI
            { { gname   = $3;
              formals = $5;
              locals  = List.rev $11;
              body    = List.rev $12;
              blockSize = $7 } }

/* TODO: Calling functions from inside other functions? */

blocksize:
    | /* nothing */ {}
    | PERIOD LBRACK INT_LITERAL RBRACK { $3 }

formals_opt:
    | /* nothing */ { [] }
    | formal_list   { List.rev $1 }

formal_list:
    | vdecl { [$1] }
    | formal_list COMMA vdecl {$3 :: $1 }

/* Variable Declarations */
/* <const> <type> name */
vdecl:
    const str type_name ID
    { { _type = $3;
        name = $4;
        isConst = $1;
        str = $2 } }

str:
    | STRUCT      { true }
    | /* not struct */  { false }

const:
    | CONST       { true }
    | /* not const */ { false }

/* TODO: check if const handling works? */

type_name:
    | INT       { $1 }
    | FLOAT     { $1 }
    | LONG      { $1 }
    | DOUBLE    { $1 }
    | STRING    { $1 }
    | CHAR      { $1 }
    | BOOL      { $1 }

vdecl_list:
    | /* nothing */   { [] }
    | vdecl_list vdecl SEMI { $2 :: $1 }

/* Statements */
stmt_lists:
    | /* nothing */   { [] }
    | stmt_list stmt SEMI { $2 :: $1 }

gfunc_stmt_lists:
    | /* nothing */   { [] }
    | gfunc_stmt_lists gstmt SEMI { $2 :: $1 }

stmt:
    | expr SEMI       { Expr($1) }
    | gexpr SEMI      { Expr($1) } /* TODO: separate Gexpr in AST? */
    | RETURN expr SEMI  { Return($2) }
    | RETURN gexpr SEMI { Return($2) }
    | vdecl ASSIGN expr SEMI    { Assign($1, $3) }
    | vdecl ASSIGN gexpr SEMI   { Assign($1, $3) }
    | ID ASSIGN expr SEMI       { Assign($1, $3) }
    | ID ASSIGN gexpr SEMI      { Assign($1, $3) }
    | ID ASSIGN ID SEMI         { Assign($1, $3) }
    | IF LPAREN bool_expr RPAREN LBRACK SEMI stmt_lists RBRACK SEMI %prec NOELSE
    { If($3, $7, []) }   
    | IF LPAREN bool_expr RPAREN LBRACK SEMI stmt_lists RBRACK SEMI ELSE LBRACK SEMI
    loop_stmt_list RBRACK SEMI { If($3, $7, $12) } 
    | FOR LPAREN expr_opt SEMI bool_expr SEMI expr_opt RPAREN LBRACK SEMI
    loop_stmt_list RBRACK SEMI { For($3, $5, $7, $11) }
    | FOR ID IN ID COLON LBRACK SEMI loop_stmt_list RBRACK SEMI { Forin($2, $4,
    $8) }
    | WHILE LPAREN bool_expr RPAREN LBRACK SEMI stmt_lists RBRACK SEMI {
        While($3, $7) }


/* gstmts are similar to gstmts except they do not allow for returns */
/* they also allow for blocks in their expressions */
gstmt:
    | expr SEMI       { Expr($1) }
    | blockexpr SEMI  { Expr($1) }
    | vdecl ASSIGN expr SEMI    { Assign($1, $3) }
    | vdecl ASSIGN blockexpr SEMI {Assign($1, $3) }
    | ID ASSIGN expr SEMI       { Assign($1, $3) }
    | ID ASSIGN blockexpr SEMI  { Assign($1, $3) }
    | ID ASSIGN ID SEMI         { Assign($1, $3) }
    | IF LPAREN bool_expr RPAREN LBRACK SEMI gfunc_stmt_lists RBRACK SEMI %prec NOELSE
    { If($3, $7, []) }   
    | IF LPAREN bool_expr RPAREN LBRACK SEMI gfunc_stmt_lists RBRACK SEMI ELSE LBRACK SEMI
    gloop_stmt_list RBRACK SEMI { If($3, $7, $12) } 
    | FOR LPAREN expr_opt SEMI bool_expr SEMI expr_opt RPAREN RBRACK SEMI
    gloop_stmt_list LBRACK SEMI { For($3, $5, $7, $11) }
    | FOR ID IN ID COLOON LBRACK SEMI
    | WHILE LPAREN bool_expr RPAREN LBRACK SEMI gfunc_stmt_lists RBRACK SEMI {
        While($3, $7) }
   /* TODO: blocks? */

/* TODO: mixed variable declarations and other statements */

/* Loops can contain all normal expressions, and also Break and Continues */
loop_stmt_list:
    | /* Nothing */                  { [] }
    | stmt_lists SEMI                { $1 }
    | loop_stmt_list loopexpr SEMI   { $2 :: $1 }

gloop_stmt_list:
                                     { [] }
    | gfunc_stmt_lists SEMI          { $1 }
    | gloop_stmt_list loopexpr SEMI  { $2 :: $1 }

loopexpr:
    | CONTINUE { $1 }
    | BREAK    { $1 }

blockexpr:
    BLOCK PERIOD ID
    /* TODO: handle structs */

bool_expr:
    | expr EQ expr                    { Binop($1, Equal, $3) }
    | expr NEQ expr                   { Binop($1, Neq, $3) }
    | expr LT expr                    { Binop($1, Less, $3) }
    | expr LEQ expr                   { Binop($1, Leq, $3) }
    | expr GT expr                    { Binop($1, Greater, $3) }
    | expr GEQ expr                   { Binop($1, Geq, $3) }
    | expr LAND expr                  { Binop($1, Land, $3) }
    | expr LOR expr                   { Binop($1, Lor, $3) }

expr_opt:
    | /* Nothing */                   { Noexpr }
    | expr                            { $1 }


/* expr are all the expressions EXCEPT:
    * those with blocks
    * comparison operators */
expr:
    | LITERAL                   { Literal($1) }
    | ID                        { Id($1) }
    | expr OR expr              { Binop($1, Or, $3) }
    | expr XOR expr             { Binop($1, Xor, $3) }
    | expr NOT expr             { Binop($1, Not, $3) }
    | expr LSHIFT expr          { Binop($1, Lshift, $3) }
    | expr RSHIFT expr          { Binop($1, Rshift, $3) }
    | expr PLUS expr            { Binop($1, Plus, $3) }
    | expr MINUS expr           { Binop($1, Minus $3) }
    | expr TIMES expr           { Binop($1, Times, $3) }
    | expr DIVIDE expr          { Binop($1, Divide, $3) }
    | expr MOD expr             { Binop($1, Mod, $3) }
    | expr NEG expr             { Binop($1, Neg, $3) }
    | LPAREN expr RPAREN        { $2 }
    | ID ASSIGN expr            { Assign($1, $3) }

gexpr:
    | gexpr OR gexpr              { Binop($1, Or, $3) }
    | gexpr XOR gexpr             { Binop($1, Xor, $3) }
    | gexpr NOT gexpr             { Binop($1, Not, $3) }
    | gexpr LSHIFT gexpr          { Binop($1, Lshift, $3) }
    | gexpr RSHIFT gexpr          { Binop($1, Rshift, $3) }
    | gexpr PLUS gexpr            { Binop($1, Plus, $3) }
    | gexpr MINUS gexpr           { Binop($1, Minus $3) }
    | gexpr TIMES gexpr           { Binop($1, Times, $3) }
    | gexpr DIVIDE gexpr          { Binop($1, Divide, $3) }
    | gexpr MOD gexpr             { Binop($1, Mod, $3) }
    | gexpr NEG gexpr             { Binop($1, Neg, $3) }
    | LPAREN gexpr RPAREN         { $2 }
    | ID ASSIGN gexpr             { Assign($1, $3) }
