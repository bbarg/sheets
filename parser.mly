(*
 * Sheets parser
 *
 * Authors: Ben Barg, Amelia Brunner
 * Copyright 2014, Symposium Software
 *)

%{ open Ast %}

(* Punctuation Tokens *)
%token LAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA PERIOD
%token BACKSLASH COLON EOF

(* Loop Keywords *)
%token WHILE FOR IN BREAK CONTINUE

(* Function Keywords *)
%token FUNC GFUNC MAIN STRUCT RETURN

(* Type Keywords *)
%token INT LONG FOAT DOUBLE CHAR CONST TRUE FALSE STRING BLOCK

%token LITERAL
%token <string> ID

(* Operator tokens
 *
 * Ordinarily we would need to deal with precedence of array operators
 * versus scalar operators, but we don't need to worry about their relative
 * precedence because we prohibit them on the same line. *)

(* Scalar Operators *)
%right ASSIGN
%left LOR
%left LAND
%left OR
%left XOR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NEG

(* Array Operators *)
%right G_ASSIGN
%left G_LOR
%left G_LAND
%left G_OR
%left G_XOR
%left G_AND
%left G_EQ G_NEQ
%left G_LT G_LEQ G_GT G_GEQ
%left G_LSHIFT G_RSHIFT
%left G_PLUS G_MINUS
%left G_TIMES G_DIVIDE G_MOD
%right G_NEG

%start sheet			(* start symbol *)
%type <Ast.program> sheet	(* type returned by program *)

sheet:
		      { [], [] }

(* Function Declarations *)

fdecl:


gfdecl:
