(*
  This code conatins information about name less function and remove_if function

  This code will remove odd elements from the odd elements from the list
*)

val lst=[1,2,3,4];

fun   remove_if f [] = []
|     remove_if f (a::b) =  if f(a) then remove_if f b
                            else  a::(remove_if f b);

val evens = remove_if (fn x => (x mod 2)=1) lst;
