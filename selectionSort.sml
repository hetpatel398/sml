(*
  This code implements selction sort algorithm.
*)

fun findMin(max,[], lst)   = (max, lst)
|   findMin(max,rest as h::t, lst) =
                      if h<max then findMin(h, t, max::lst)
                      else findMin(max, t, h::lst);


fun selectionSort([])         = []
|   selectionSort([a])        = [a]
|   selectionSort(L as h::t)  =
            let
                val (max, remaining)=findMin(h,t,[]);
            in
                max::selectionSort(remaining)
            end;
