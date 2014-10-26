#Sheets Language Reference Manual

* Amelia Brunner - `arb2196`
* Gabriel Blanco - `gab2135`
* Ruchir Khaitan - `rk2660`
* Benjamin Barg  - `bbb2123`


##1. Introduction

The *Graphics Processing Unit* (GPU) was invented in 1999 as a single-chip processor that allowed the CPU to offload graphics-intensive tasks to a separate processor. Unlike the CPU, which is built to contain only a handful of cores but a lot of cache memory, a GPU has limited memory but hundreds of cores, which, thanks to very efficient context switching, allows it to handle thousands of threads concurrently without significantly degrading the performance of the CPU. In the past, GPUs were seen as a luxury reserved for video processing and computer gaming. However, because of the advent of larger screen displays and a greater demand for video, image and signal processing applications, GPUs are quickly becoming more mainstream. 

Although we are seeing more and more applications take advantage of the computational capabilities of the GPU, it is still very difficult to program for the GPU because of the many different types of GPU architectures and chip specific proprietary software. *Sheets* takes a high-level approach to programming on the GPU. By taking the basic concepts of parallelizability and repeatable mathematical operations, *Sheets* allows the user to write clean, concise code that takes advantage of a GPU's ability to handle large-vector data operations without having to worry about hardware specific implementations. It does so by compiling down into *OpenCL*, an open-source programming language that can run on a wide variety of GPUs.

##2. Types

###2.1 Primitive Types


* `int`

Signed 32 bit integer type. Fixed-point number.

* `long`

Signed 64 bit integer type. Higher precision than an integer, also a fixed-point number.

* `float`

Single precision floating point type.

* `double`

Double precision floating point type.

* `char`

Single character, which can be alphanumeric or a form of punctuation

```
int[]
long[]
double[]
float[]
char[]
```


###2.2 Non-Primitive Types
* String
* Block  		

**TODO:** Need to decide how this maps to OpenCL NDrange

* Struct

###2.3 Casting

Casting is allowed between:
* Any two numbers
* floats/doubles to longs/int lose precision
* longs/floats to ints/doubles get truncated
* No casting between primitives and non-primitives

##3. Lexical Convections

###3.1 Identifiers

Refer to a variable or a function.
Must begin with alphabetic character or underscore.
But the rest of the identifier can be alphanumeric or underscores.
Capital and lowercase letters are treated differently.
We reject dashes.

###3.2 Keywords

```      
if
else
elif
while
for
in
break
continue
TRUE
FALSE
NULL
return
const
func
gfunc
main
```

###3.3 Literals

* int literals

numbers without a decimal in them

Regular Expression
`\d+`

* float literals 

numbers with a decimal in them

Regular Expression:
`(\d+\./d+?)|(\d+?\.\d)`

* character literal

```
single quote - \'

double quote - \"

newline - \n

horizontal tab \t
```

* string literal 

```
"string literal"
```

* array literal

```
array = [1, 2, 3]
```

###3.4 Punctuation
```
  ,           function params
              array literal separation
              
  []          array literal declaration
              array access
              
  ()          expression precedence
              conditional parameter
              function arguments
              Casting
              
  :           start of function
  
  '           character literal declaration
  
  "           string literal declaration
```

###3.5 Comments
It's like threads coming out of a sheet!

```
		# for inline comments

		#~ for nested comments ~#
		
		#~
		 ~ for long nested
		 ~#
```


###3.6 Operators
```
	.   Access                  
	
	*   Multiplication              :*  Vector multiplication
	/   Division                    :/  Vector division
	%   Mod                         :%  Vector mod
	+   Addition                    :+  Vector addition
	-   Subtraction                 :-  Vector subtraction
	
	^   XOR                         :^  Vector XOR
	&   AND                         :&  Vector AND
	|   OR                          :|  Vector OR
	~   NOT                         :~  Vector NOT
	>>  Right shift                 :>> Vector right shift
	<<  Left shift                  :<< Vector left shift
	
	=   Assignment                  :=  Vector assignment
	!   Negation                    :!  Vector negation
	
	==  Equivalence                 :== Vector equivalence
	!=  Non-equivalence             :!= Vector non-equivalence
	<   Less than                   :<  Vector less-than
	>   Greater than                :>  Vector greater-than
	<=  Less than or equal to       :<= Vector less-than-or-equal-to
	>=  Greater than or equal to    :>= Vector greater-than-or-equal-to
```
###3.7 Operator Precedence

We will only allow for expressions of only vector operators or only
non-vector operators. Within the two groups, order of precedence will
be the same as for C. The comparison operators will be treated the same
as in C.

###3.8 Whitespace

Blank, tab, and newline characters
Blank characters will be used for program string delimination
Blank characters directly following newline characters will 
    be used for functions/blocks-ing.
Tabs will not be tolerated.

##4. Syntax

###4.1 Program Structure

A program consists of a sequence of zero or more valid statements.

Generally speaking:

* The `main` function is the starting point of the function. If there is no `main` function, the compiler will throw an error.
* There can also be other functions (`func`) that can be called by `main`
* There is a special subcategory of functions called `gfunc`'s that get sent to the GPU. The ordering of gfuncs and funcs within the program does not matter
* Not object oriented, no classes.

###4.2 Expressions

An expression is a sequence of operators and operands that may
have side effects. The order of evaluation is left to right.
Operands must have the same type, except in the case of the 
vector operands (more below).

####4.2.1 Assignment

####4.2.2 Arithmetic

####4.2.3 Comparison Operators

####4.2.4 Logical

####4.2.5 Vector Operators

Vector operators, as described above, are operators that we have included for
convenience in doing vector operations. They can be used as expressions, the same
way that other operators can be used. However, they actually correspond to a short
library of gfuncs that we have implemented for the user. All of these vector
gfuncs require that the operand on the left side of the operator be an array or
vector, and the operand on the right side of it be either an integer constant to 
apply to the entire left-hand operand, or another array/vector with the same 
dimensions, where the operation can be mapped exactly from one index to the other.
Precedence is applied the same as it is for the non-vector operators.
We do not tolerate mixed vector operators and normal operators on the same line.

###4.3 Statements


####4.3.1 Expression statements

Assignment, for example

####4.3.2 Conditional statements

if/else/elif
Using the comparison operators

####4.3.3 Loop statements

while/for
we allow for special for loops to iterate through arrays, 
using the syntax 'for <indexing variable> in <array>:'
            

####4.3.4 Interruption Statements

break/continue
            
####4.3.5 Return Statements

return

####4.3.6 gfunc Statements

statements invoking gfuncs are blocking
until they have completed.
            
###4.4 Functions

There are two kinds of functions: 

`func` and `gfunc`
        
###4.5 Scope

####4.5.1 Scope within the GPU 

*GPU memory hierarchy*

gfuncs do not have access to CPU memory space; This means that all variables that need to be passed into the GPU need to be done so through gfunc arguments. gfuncs do not have access to global variables declared in CPU space. The only caveat to this is that global variables can also be passed to the GPU through specified library functions, such as .blocksize

####4.5.2 Scope within the CPU

Global variables are variables that are declared outside
of a function. These can be accessed by all funcs.
Sheets will use block scoping, such that any variable defined
within or within a greater indentation level will be accessible.
            
##5 GPU Functions (gfunc)


Contents of gfuncs will be compiled into the kernel OpenCL files, meaning that they will be executed on the GPU. Because of this, we have to enforce a few GPU-specific memory and concurrency limitations on the contents of gfuncs; they must be parallelizable, meaning that they must:

1. must not depend on previous values in array
2. they must not write to overlapping regions in memory 
3. they should not require excessive shared memory.

To enforce these, we have implemented the following: 

* Any call to gfuncs will be blocking.
* Function arguments must be immutable.
* Output length will always be equal to the length of the first argument.
* Includes special environmental variable, which only exists in the scope of the gfunc, that contains all the information that will be passed into a block. 
   
`block` is a struct containing:

* `block.size`
        
the number of elements that a block can write into

* `block.index`                

the index of the current block, meaning the index of the segmented region of the output array that this block can write to

i.e. if the output array has 100 indices, and there are 20 total blocks 
that the output could be split into, there are block indices 0-19, where 
each index corresponds to the an increment of 5 in the index of the
output array.
        
* `block.out`

array of size block.size, that represents the writable region in the
cumulative output for the block

##6 Standard Library Functions
###6.1) Vector Operations
* `reverse()`
* `length()`

// TODO can we build-in reduce/fold operations for summing, computing big conditional expressions

###6.2) File I/O

* `print`
* `read`
* `write`

// TODO can we build-in reduce/fold operations for summing, computing big conditional expressions