# Saturday, Nov 8th

**bbarg** --- We've realized several major issues in our attempts to write
a parser/syntactical analayzer/translator. Here's a lists of things we
need to do:

- Which types do we need in our abstract syntax tree?
- How do we translate the output of our parser into OpenCL?
- When do we enforce type-checking/restrictions on gfuncs?
  + Example: only having g_expressions or scalar expressions in an
    individual statement?
  + How do we deal with including scalar literal/variables in the
    scalar operators?
- How do we deal with the different type declarations (int, float,
  long, double) that are not dealt with in the micro-c compiler?
  + We PROBABLY need to have a "variable" type that keep track of its
    own type...
- Should we be parsing literals into Ocaml types in the scanner?
- Look through interpreter code (MicroC) to see how AST is used in the
  next step?
- How do we handle type-casting?

# Monday, Nov 10th

**bbarg** Trying to build up the AST. New questions:

- How do we represent variable of multiple types?

## WORKING NOTES

- We have determined that gfunc and func requirements are different
  enough that we will have a different AST element for gfunc
  statements and func statements.

- We will need to check that block_size is valid for a given gfunc
  call AT RUNTIME.

- We need an enclosing type for scalar statements and array statements.

- Do we have an "isConst" field in the vDecl type or do we have
  separate "const" types for each of our basic primitives?

- The AST cannot have any definitions that refer to tokens; instead,
  it supplies rules to parse up to the defined types

- There is considerable ambiguity about how to ensure various semantic
  requirements of our language, and whether those things should be
  enforced in the parser:
  + only array operators or scalar operators in one expression
  + type checking for variable assignment
  + type restrictions on gfuncs

- How do we deal with the different types of scopes

## GET RID OF?

Is there any legitimate reason to have `const` or `struct` keywords in
Sheets?

## FUNCTIONALITY

Should assignment operations be expressions in Sheets? (i.e, should
something like `foo = 5` have a value?). Right now these are **NOT**
supported.

