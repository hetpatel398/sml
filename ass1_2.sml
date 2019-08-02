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
  This function takes two list both containing two 4 digit numbers and produce multiplication using karatsuba algorithm
*)
fun karatsuba_single(x as [x1,x0], y as [y1,y0]) =
                let
                  val z2=x1*y1
                  val z0=x0*y0
                  val z1=((x0-x1)*(y1-y0))+z2+z0
                  val x0=z0 mod 10000
                  val x1=(z1+z0 div 10000) mod 10000
                  val x2=(z2+(z1+z0 div 10000) div 10000) mod 10000
                  val x3=((z2+(z1+z0 div 10000) div 10000) div 10000) mod 10000
                  val x4=(((z2+(z1+z0 div 10000) div 10000) div 10000) div 10000) mod 10000

                in
                  if x2=0 then [x1, x0]
                  else if x3=0 then [x2, x1, x0]
                  else [x3, x2, x1, x0]
                end;

(*
  This function takes two reverse lists of same length containg numbers of base 4 and adds both lists and returns another list.

  ex. we want to make sum of two 8 digit numbers
      1234,5678+9999,9999

      So now we will pass the reversed list after splitting into four digits i.e.
      add_lst([5678, 1234], [9999, 9999], [], 0)
          o/p : [1,1234,5677] which is actual(not reversed) sum of two 8 digit numbers
*)

fun add_lst([],[],result as h::t, carry) =
                if carry=0 then result
                else carry::result
|   add_lst(a as ah::at, b as bh::bt, result, carry)  =
                let
                  val sum = (ah+bh+carry) mod 10000
                  val carry = (ah+bh+carry) div 10000
                in
                  add_lst(at, bt, sum::result, carry)
                end;

(*
  This function takes two reverse lists of same length containg numbers of base 4 and adds both lists and returns another list.
*)

fun sub_list([], [], result as h::t, borrow) =
|   sub_list(a as ah::ht, b as b::bt, result, borrow) =




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
