(*
  This is the implementation of quick sort algorithm
*)

fun partition (pivot, [], l, r)           = (l,r)
|   partition (pivot, lst as h::t, l, r)  =
                                            if pivot>h then partition(pivot, t, h::l, r)
                                            else partition(pivot, t, l, h::r);

fun qSort([])          =  []
|   qSort([a])         =  [a]
|   qSort(lst as h::t) =
                          let
                            val (l,r) = partition(h, t, [] , [])
                          in
                            qSort(l)@(h::qSort(r))
                          end;
