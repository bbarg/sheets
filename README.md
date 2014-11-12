# Sheets
*What's your thread count?*

## Pressing Questions

- **Translator**: When do we deal with semantic checking?
  + Currently, we are pushing most semantic checking to `gcc`. That
    will be really weird for users trying to debug stuff like name
    redefinition and stuff. Is there ANY way to map this stuff back to
    our sheets compiler.

- **Parser**: How can we get the parser to compile so we can test the lexer?

## TA Questions (Nov 13)

- should we deal with scoping in `parser.mly` or in a separate file that builds the symbol tables
- what level of abstraction should we get to before we start actually generating OpenCL code?
  + a followup, how many stages of this additional process are there, and what are they typically called?

## Preprocessor Issues:

- Inline comments that start in the middle of a statement and continue into a new line