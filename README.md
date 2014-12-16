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
No logical and &&
No logical or ||

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

## Final Push

- When you return an array you need to:
  + malloc space equal to the size of the array
  + mem_cpy the expression into that address
  + return the pointer
  - we'll just let this memory leak

- We need to find a solution for making a string literal out of the
kernel body
  + dumbest solution: remove ALL newlines from the generation (except
  kernel invocation stuff)

- general debugging of the gfunc calls

- File IO OR some way of generating data (which we could do with a
  gfunc; especially if we could use trig functions or something)

- POSSIBLY implement Gop
  + basically you just take every array element as an arg of the gfunc
  and replace the gops with with scalar ops.
  + e.g, for int arrays `a` and `b`

```
	my_arr = ((a :+ 2) :* 3) :/ b
```

would map to

```
__kernel
void g_op_random_hash(__global const int __arr_len,
                      __global int *__out,
	                  __global const int *a,
				      __global const int *b) {

	const int _id = get_global_id(0);
	const int __block_start = _id * __block_size;
	const int __block_end = _id * __block_size + __block_size;

	out[_id] = ((a[_id] :+ 2) * 3) / b[_id];
}
```
