(*
  This file contains necessary functions required to implement merge sort.

  merge(L,M) : where L and M are sorted list and this function returns merged sorted list.
    merge([0,2,4,6], [1,3,5[])=[0,1,2,3,4,5,6]

  split(L) : Splits list L into two parts
    split([1,2,3,4,5,6])=([1,3,5], [2,4,6])


*)

fun   merge([],[])=[]
|     merge([],M)=M
|     merge(L,[])=L
|     merge(a::L,b::M)=
                      if a>b then b::merge(a::L,M)
                      else a::merge(L, b::M);


fun split [] = ([],[])
|   split([x]) = ([x],[])
|   split(a::(b::L)) =
                      let
                          val (A, B) = split(L)
                      in
                          (a::A, b::B)
                      end;

fun mergeSort([])=[]
|   mergeSort([a])=[a]
|   mergeSort lst=
          let
            val (a,b) = split(lst)
          in
            merge(mergeSort(a), mergeSort(b))
          end;
