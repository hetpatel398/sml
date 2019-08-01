(*

This code reverses first list form the parameter and prepend it to the second list.
e.g. reverse([1,2,3], [4,5,6,7])=[3,2,1,4,5,6,7]

*)

fun 	reverse([],y)=y
|	reverse(a::x, y)=reverse(x, a::y);
