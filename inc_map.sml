(*
  This function is written for demonstration on the inbuilt function map,
  which increments every element of the list by 1.

  Use these command

  map inc [1,2,3,4,6];
  map square [1,2,3];

  Here first_square takes list of tuples and squares first elemnt of every tuples and returns it in a list.
*)

fun inc x = x+1;
fun square x = x*x;

fun first (a,b)=a;

fun first_square L=
    let
      val firsts = map first L
    in
      map square firsts
    end;
