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
  This function reverses given list
*)
fun revH0([], ans)=ans
|   revH0(lst as h::t, ans) = revH0(t, h::ans);

fun revH(l, ans) =
      revH0(l, []);

(*
  This function finds length of given list
*)
fun lenH([])=0
|   lenH(h::t)=1+lenH(t);

(*
  This function takes two integer lists and appends 0's at the head of list which has less length until both lists' length becomes same
*)
fun adjustLength(a,b) =
    if lenH(a) = lenH(b) then (a,b)
    else if lenH(a) > lenH(b) then adjustLength(a,0::b)
    else adjustLength(0::a, b);

(*
  This function splits the list in the argument in two parts
*)
fun splitList0 (lst, 0, A) =(revH(A, []), lst)
|   splitList0 (lst as h::t, diff, A) =
          splitList0(t, diff-1, h::A);

fun splitList(lst as h::t) =
    let
      val l = lenH(lst)
      val m = l div 2
    in
      splitList0(lst, m, [])
    end;

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

      So now we will pass the list after splitting into four digits i.e.
      add_lst([1234, 5678], [9999, 9999], [], 0)
          o/p : [1,1234,5677] which is actual(not reversed) sum of two 8 digit numbers
*)

fun add_lst0([],[],result as h::t, carry) =
                if carry=0 then result
                else carry::result
|   add_lst0(a as ah::at, b as bh::bt, result, carry)  =
                let
                  val sum = (ah+bh+carry) mod 10000
                  val carry = (ah+bh+carry) div 10000
                in
                  add_lst0(at, bt, sum::result, carry)
                end;

fun add_lst(a,b) =
                let
                  val (A,B) = adjustLength(a,b)
                  val revA=revH(A,[])
                  val revB=revH(B,[])
                in
                  add_lst0(revA,revB,[],0)
                end;
(*
  These functions takes two reverse lists of same length containg numbers of base 4 and adds both lists and returns another list.

  isAGreaterThanB function takes two numbers in list of B=10^4 and returns
    true if isAGreaterThanB i.e. A-B will be isPositive
    else it will return true and we will compute b-a which will be positive

  sub_list is a function which takes two lists of integers of base 10^4 and subtracts these two lists
  and returns (answer, boolean value) tuple in which boolen value indicates wether your answer is positive or negative
  if the boolean value is true then answer is positive else it is negative
*)

fun isAGreaterThanB([], []) = true
|   isAGreaterThanB(A as ah::at, B as bh::bt) =
                if ah>bh then true
                else if ah=bh then isAGreaterThanB(at, bt)
                else false;

fun sub_list0([ah], [bh], result as h::t, borrow) =
                if borrow=0 then (ah-bh)::result
                else (ah-1-bh)::result
|   sub_list0(a as ah::at, b as bh::bt, result, borrow) =
                let
                  val sub = (ah-borrow)-bh
                in
                  if sub<0 then sub_list0(at, bt, (10000+sub)::result, 1)
                  else sub_list0(at, bt, sub::result, 0)
                end;

fun sub_list(a as ah::at, b as bh::bt) =
                let
                  val (A,B) = adjustLength(a,b)
                  val isPositive = isAGreaterThanB(A,B)
                  val revA=revH(A,[])
                  val revB=revH(B,[])
                in
                  if isPositive then (sub_list0(revA,revB,[],0), isPositive)
                  else (sub_list0(revB,revA,[],0), isPositive)
                end;

fun karatsuba0(a as ah::at,b as bh::bt,ans) = nil

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
