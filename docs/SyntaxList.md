#Literals

* "An integer literal is a sequence of digits that fit within a 32 bit range"

* "A long literal is a sequence of digits that cannot fit within a 32 bit range but can fit in a 64 bit range."

* "A floating point literal consists of an integer part, a decimal part and a fractional part"

    * UNDECIDED: Whether a double precision floating point number `long` handles the numbers too big (or precise) for a `float`, or if they get rounded into `float`s.

* "A single character ... [that] can be alphanumeric, a form of punctuation or an escaped character (such as a newline `\n`) within a pair of single quotes."" 

* Array literals:

```
<type T> arrayName[size] = [element, element, element, ...  ] 
```
The number of values in the right hand side definition must less than or equal to the size of the array, if a size is given. If no size parameters are given, size is inferred from RHS contents.

If **one** size parameters is given, a single dimensional array of that size is allocated with the right hand side contents, with any undefined elements initialized as zero

    * UNDECIDED: What about if the array is of type "string" or "char", should they be of null type?

If **two** size parameters are given, a two dimensional array is allocated. Undefined terms default to zero, and elements are defined with sub-sets

```
<type T> arrayName[size1,size2] = [[element, element],  [element, element, element],  ...  ]
```

#Arrays

* Arrays are zero indexed

* Can be accessed through square bracket notation `[]`

* Arrays can be multi-dimensional up to at most 3 dimensions

* Row/Column Indexing: can be accessed dimensionally by separating indices with a comma, e.g.

```
array[2,3]
```

* Allocation: arrays get allocated this way. You can have zero, one or two size parameters within the size declaration. 

```
<type T> arrayName [size(, size)]
```

    * UNDECIDED: What do we do with an unsized array if not defined as a literal? Does it become like an array pointer? Are we implicitly supporting pointers then, but only in this case?




