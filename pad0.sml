(*
  Padding  0 in front of list if length%4 is not 4
*)

fun pad0 (l) =
                if Int.mod(length(l),4) = 0 then l
                else pad0(0::l);
