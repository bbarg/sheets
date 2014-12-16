# Sheets
*What's your thread count?*

## Preprocessor Issues:

- Inline comments that start in the middle of a statement and continue into a new line


This is how I think we should proceed 

### Types


Ints
Floats
Strings (printing/IO - map to const char*) 
Arrays of Ints or Floats fixed size heap allocated - have to define a struct 1 or 2d  (maybe 3d) 

all arithmetic operations ( + - / * ) typed 
boolean operations ( > < == >= <= != ) 

gops for (vectors and vectors) and (vectors and scalars) 

### Conditionals 


if / else 

### Loops

for in 

for 

while 

### gfuncs 


 - Ben decides 


### funcs

 - typed with a return 


## New Issues (Monday Morning)

There isn't actually a problem. We just were running the generator
directly on stuff that needed to go through the preprocessor first.

* We are not supporting nested multiline block comments:
```
#~ 
Multiline Block comments with
#~ Nested Block Comments~#
Will Fail
~#
```
