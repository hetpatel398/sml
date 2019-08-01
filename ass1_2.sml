(*
  Assignment 1 attempt 2 by HET PATEL 2019MCS2562
*)

exception notADigit;

(*
  val charToInt = fn : char -> int

  This function is a mapper function which I have used to convert character elements of an array to integers.
*)
fun charToInt(c)   =
                if Char.isDigit(c) then (ord(c)-48)
                else raise notADigit;

(*
  val pad0 = fn : char list -> char list

  This function adds padding to character array which we got by exploding string to get length divisible by 4.
*)
fun pad0 (l) =
                if Int.mod(length(l),4) = 0 then l
                else pad0(#"0"::l);

(*
  val makeListOfDigits = fn : int list * int * int * int list -> int list

  This function takes list containing digits as an input and gives list which contains number of base 10^4.
  Here in the Arguments we have passed:
    digits : padded List of digits in the input String
    remaining4 : Counter using which we have count when to create a new element in the list when we reach 4 digits
    x : contains intermediate sum using which we create 4 digit number by picking four digits from the List
    lst : This is our list which contains output

  Sample input : makeListOfDigits([0,0,0,1,2,3,4,5,6,7,8,9], 3, 0, [])
  output : [6789, 2345, 1]
*)
fun makeListOfDigits([], _, _, l)                        = l
|   makeListOfDigits(digits as a::t, remaining4, x, lst) =
                if remaining4=0 then
                    makeListOfDigits(t, 3, 0, (x+a)::lst)
                else if remaining4=1 then
                    makeListOfDigits(t, 0, a*10+x, lst)
                else if remaining4=2 then
                    makeListOfDigits(t, 1, a*100+x, lst)
                else
                    makeListOfDigits(t, 2, a*1000+x, lst);

(*
  val fromString = fn : string -> int list

  This function takes input String and returns list of digits in base B=10^4
*)
fun fromString("") = []
|   fromString(s)  =
                let
                    val chars=String.explode(s)
                in
                    makeListOfDigits(map charToInt (pad0 chars), 3, 0, [])
                end;
