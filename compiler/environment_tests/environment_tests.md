# Environment Tests

Convention 

(p || f)_env_<N>.sht 

p_env tests are designed to succeed 
n_env tests are designed to fail 

## Test 1 -- psitive

 - Declares 4 global variables 
 - Declares a function <add> that takes two integer arguments
 - <add> declares 3 local variables 

## Test 2 -- negative 

  - Declares 4 global variables 
  - Declares a function <add> that takes two integer variables 
  - Declares a second function <add> that takes one integer 
  - Fails because we don't support function overloading 

## Test 3 -- positive 

  - Declares 5 uniquely named variables in global scope

## Test 4 -- negative 
  - Declares 5 variables in a global scope, but there is a name conflict 
  - Fails because you can't redeclare variables in same scope 

## Test 5 -- positive 
  - 
