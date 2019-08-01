(*
  Assignment 1 by Het Shaileshkumar Patel (2019MCS2562)
*)

fun char_to_int(c) =  if c = #"1" then 1
                      else if c = #"2" then 2
                      else if c = #"3" then 3
                      else if c = #"4" then 4
                      else if c = #"5" then 5
                      else if c = #"6" then 6
                      else if c = #"7" then 7
                      else if c = #"8" then 8
                      else if c = #"9" then 9
                      else 0;

fun int_to_char(c) =  if c = 1 then #"1"
                      else if c = 2 then #"2"
                      else if c = 3 then #"3"
                      else if c = 4 then #"4"
                      else if c = 5 then #"5"
                      else if c = 7 then #"7"
                      else if c = 6 then #"6"
                      else if c = 8 then #"8"
                      else if c = 9 then #"9"
                      else #"0";

fun fromString("") = []
|   fromString(s)  =
                      let
                        val L = explode(s)
                      in
                        map char_to_int (L)
                      end;

fun toString([]) =""
|   toString(arr)=
                      let
                        val char_array = map int_to_char (arr)
                      in
                        implode(char_array)
                      end;
