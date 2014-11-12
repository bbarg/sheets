(*
 * Sheets parser
 *
 * Authors: Ben Barg, Amelia Brunner
 * Copyright 2014, Symposium Software
 *)

%{ open Ast %}

(* Punctuation Tokens *)
%token LAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA PERIOD
%token COLON EOF BACKSLASH

(* Loop Keywords *)
%token WHILE FOR IN BREAK CONTINUE

(* Conditional Keywords *)
%token IF ELSE ELIF

(* Function Keywords *)
%token FUNC GFUNC MAIN STRUCT RETURN

(* Type Keywords *)
%token INT LONG FLOAT DOUBLE CHAR CONST TRUE FALSE STRING BLOCK BOOL

(* Operator Tokens *)
%token LOR LAND OR XOR NOT AND EQ NEQ LT LEQ GT GEQ LSHIFT RSHIFT
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NEG
%token G_LOR G_LAND G_OR G_XOR G_AND G_NOT G_EQ G_NEQ G_LT G_LEQ G_GT
%token G_EQ G_LSHIFT G_RSHIFT G_PLUS G_MINUS G_TIMES G_DIVIDE G_MOD 
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

(* Operator tokens
 *
 * Ordinarily we would need to deal with precedence of array operators
 * versus scalar operators, but we don't need to worry about their relative
 * precedence because we prohibit them on the same line. *)

(* Precedence definition *)
%left LOR LAND OR XOR NOT AND EQ NEQ LT LEQ GT GEQ LSHIFT RSHIFT
%left PLUS MINUS TIMES DIVIDE MOD
%left G_LOR G_LAND G_OR G_XOR G_AND G_NOT G_EQ G_NEQ G_LT G_LEQ G_GT
%left G_EQ G_LSHIFT G_RSHIFT G_PLUS G_MINUS G_TIMES G_DIVIDE G_MOD 

%right ASSIGN G_ASSIGN NEG G_NEG

%nonassoc NOELSE ELSE ELIF

%start sheet			(* start symbol *)
%type <Ast.program> sheet	(* type returned by program *)

%%

(* Main Program *)
sheet:
                        { [], [], [] }
    | program vdecl     { ($2 :: fst $1), snd $1 }
    | program fdecl     { fst $1, ($2 :: snd $1) }
    | program gdecl     { fst $1, ($2 :: snd $1) }
   (* | program sdecl   { ($2 :: fst $1), snd $1 } *)

(* TODO: check if const handling works? *)

(* Function Declarations *)
(* func int named_func(args):{ <statements>... } *)
fdecl:
   FUNC TYPE ID LPAREN formals_opt RPAREN COLON 
   LBRACE SEMI vdecl_list stmt_lists RBRACE SEMI
            { { fname   = $3;
			  formals = $5;
			  locals  = List.rev $10;
			  body    = List.rev $11 } }

(* gfunc int named_gfunc(args).[5]:{ <statements>... } *)
gfdecl:
   GFUNC TYPE ID LPAREN formals_opt RPAREN blocksize COLON
   LBRACE SEMI vdecl_list gstmt_lists RBRACE SEMI
            { { gname   = $3;
			  formals = $5;
			  locals  = List.rev $11;
              body    = List.rev $12;
              blockSize = $7 } }

blocksize:
    (* nothing *)   {}
    | PERIOD LBRACK INT_LITERAL RBRACK { $3 }

formals_opt:
    (* nothing *)   { [] }
    | formal_list   { List.rev $1 }

formal_list:
    vdecl                      { [$1] }
    | formal_list COMMA vdecl  {$3 :: $1 }

(* Variable Declarations *)
(* <const> <type> name *)
vdecl:
    const str type_name ID
    { { _type = $3;
        name = $4;
        isConst = $1;
        str = $2 } }

str:
    STRUCT      { true }
    | (* not struct *)  { false }

const:
    CONST       { true }
    | (* not const *) { false }

type_name:
     INT        { $1 }
    | FLOAT     { $1 }
    | LONG      { $1 }
    | DOUBLE    { $1 }
    | STRING    { $1 }
    | CHAR      { $1 }
    | BOOL      { $1 }


vdecl_list:
    (* nothing *)   { [] }
    | vdecl_list vdecl SEMI { $2 :: $1 }

(* Statements *)
stmt_lists:
    (* nothing *)   { [] }
    | stmt_list stmt SEMI { $2 :: $1 }

gstmt_lists:
    (* nothing *)   { [] }
    | gstmt_list gstmt SEMI { $2 :: $1 }

stmt:
      expr SEMI       { Expr($1) }
    | RETURN expr SEMI  { Return($2) }
    | vdecl ASSIGN expr SEMI    { Assign($1, $3) }
    | ID ASSIGN expr SEMI       { Assign($1, $3) }
    | ID ASSIGN ID SEMI         { Assign($1, $3) }
    | IF LPAREN bool_expr RPAREN LBRACK SEMI stmt_list RBRACK SEMI %prec NOELSE
    { If($3, $7, []) }   
    | IF LPAREN bool_expr RPAREN LBRACK SEMI stmt_list RBRACK SEMI ELSE LBRACK SEMI
    stmt_list RBRACK SEMI { If($3, $7, $12) } 
    | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN RBRACK SEMI
    loop_stmt_list LBRACK SEMI { For($3, $5, $7, $11) }
    | WHILE LPAREN bool_expr RPAREN LBRACK SEMI loop_stmt_list RBRACK SEMI {
        While($3, $7) }

    (* TODO: assuming elif has been transformed into else if *)
    (* TODO: For ___ in ___? *)
   
(*
scope:
    (* Nothing *)       { vLocals = []; sLocals = []; body = [] }
    | scope SEMI stmt   { *)

loop_stmt_list:

bool_expr:
expr_opt:

(* gstmts are similar to gstmts except they do not allow for returns *)
(* they also allow for blocks in their expressions *) 
gstmt:
    expr SEMI       { Expr($1) }
    | vdecl ASSIGN expr SEMI    { Assign($1, $3) }
    | ID ASSIGN expr SEMI       { Assign($1, $3) }
    | ID ASSIGN ID SEMI         { Assign($1, $3) }
    | IF LPAREN bool_expr RPAREN LBRACK SEMI stmt_list RBRACK SEMI %prec NOELSE
    { If($3, $7, []) }   
    | IF LPAREN bool_expr RPAREN LBRACK SEMI stmt_list RBRACK SEMI ELSE LBRACK SEMI
    stmt_list RBRACK SEMI { If($3, $7, $12) } 
    | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN RBRACK SEMI
    loop_stmt_list LBRACK SEMI { For($3, $5, $7, $11) }
    | WHILE LPAREN bool_expr RPAREN LBRACK SEMI loop_stmt_list RBRACK SEMI {
        While($3, $7) }
    (* TODO: blocks? *)

