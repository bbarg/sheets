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

(* Operator token *)
%token PLUS MINUS TIMES DIVIDE MOD
%token NOT XOR AND OR RSHIFT LSHIFT
%token ASSIGN NEG
%token EQ NEQ LT LEQ GT GEQ LAND LOR
%token G_PLUS G_MINUS G_TIMES G_DIVIDE G_MOD
%token G_NOT G_XOR G_AND G_OR G_RSHIFT G_LSHIFT
%token G_ASSIGN G_NEG
%token G_EQ G_NEQ G_LT G_LEQ G_GT G_GEQ G_LAND G_LOR
