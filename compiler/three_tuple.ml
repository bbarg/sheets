(* three_tuple.ml
   
   Because Sheets' top-level type is a three-tuple (of vars, funcs,
   and structs), there are a lot of situations in which we need the
   first, second, or third element of a tuple. These simple accessors
   are defined here. *)

let first  (a, _, _) = a;;
let second (_, b, _) = b;;
let third  (_, _, c) = c;;
