(*

This function strip(a,lst) will remove all the a's that are in the beginning of the list lst.
e.g. strip(1,[1,1,1,2,4,5,1,1])=[2,4,5,1,1]

*)

fun   strip(a,[])=[]
|     strip(a,x::y)=if a=x then strip(a,y)
                    else x::y;
