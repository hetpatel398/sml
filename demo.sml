(*
  Assignment 1 : Introduction to ligical and functional programming

  HET PATEL (2019MCS2562)

  Here in this document I have implemented factorial of any number using karatsuba's multiplication algorithm.

  Here are the function names and short summery of what these function do.

  1.  revH = fn : 'a list -> 'a list --> Reverses given list

  2.  lenH = fn : 'a list -> int --> Gives length of given list

  3.  adjustLength = fn : int list * int list -> int list * int list -->
          Takes two lists of different sizes and returns two list of same size by appending 0 in front of small list

  4.  splitList = fn : 'a list -> 'a list * 'a list -->
          This function splits given list and returns two lists such that length of first list is
          (length of input length) div 2 and remaining components in other one

  5.  appendList = fn : 'a list * 'a list -> 'a list -->
          This function takes two lists and append them and gives one list

  6.  append0 = fn : int list * int -> int list -->
          This function appends 0's at the end m thimes(which is given in parameter) at the end of the list
          to replicate multiplication by 10^(4*m) ass our elements in list are in Base 10^4

  7.  fromString = fn : string -> int list -->
          This function takes a String and gives a list of numbers which are in base B=10^4 form

  8.  toString = fn : int list -> string -->
          This function is reverse of fromString i.e. it takes list of numbers in base 10^4 and generates String

  9.  add_lst = fn : int list * int list -> int list -->
          This function adds twos lists of digits in base B=10^4 and returns list of same type

  10. isAGreaterThanB = fn : int list * int list -> bool -->
          This function checks whether first list of digits(B=10^4) is greater than second list or not
          This function helps is decide sign of outcome of subtraction operation

  11. sub_list = fn : int list * int list -> int list -->
          This function gives us mod(A-B) where A is first list of digits and B is second list of digits in base 10^4

  12. karatsuba = fn : int list -> int list -> int list -->
          This is implementation of karatsuba's multidigit multiplication for multiplication of two lists of ddigits
          of base 10000 and return list of same kind. This function will be used in finding factorial of large numbers

  13. factorial = fn : string -> string -->
          This function takes numerical String as an input and returns a string containing factorial of that number
*)

(*This exception will be thrown when input String is not a digit*)
exception notADigit;

(*
  This function reverses given list
*)
fun revH(l) =
    let
      fun revH0([], ans)=ans
      |   revH0(lst as h::t, ans) = revH0(t, h::ans)
    in
      revH0(l, [])
    end;

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
    else if Int.>(lenH(a),lenH(b)) then adjustLength(a,0::b)
    else adjustLength(0::a, b);

(*
  This function splits the list in the argument in two parts
*)
fun splitList([])=([],[])
|   splitList(lst as h::t) =
    let
      val l = lenH(lst)
      val m = l div 2
      fun splitList0 ([], _, A)=(revH(A), [])
      |   splitList0 (lst, 0, A) =(revH(A), lst)
      |   splitList0 (lst as h::t, diff, A) =
                splitList0(t, diff-1, h::A)
    in
      splitList0(lst, m, [])
    end;

(*
  Thsi function appends two lists and returns appended list
*)
fun 	appendList([],y)=y
|	appendList(a::x,y)=a::appendList(x,y);

(*
  Append 0's at the end of the list to mimick multiplication by 10 pow m where m is argument in the function
*)
fun append0(lst, m) =
    let
      fun append0_logic (lst, 0) = revH(lst)
      |   append0_logic (lst, m) = append0_logic(0::lst, m-1)
    in
      append0_logic(revH(lst), m)
    end;

(*
  val fromString = fn : string -> int list
  This function takes input String and returns list of digits in base B=10^4

  We have created some helper function within this function which are as follows

  {
    val makeListOfDigits = fn : int list * int * int * int list -> int list

    This function takes list containing digits as an input and gives list which contains number of base 10^4.
    Here in the Arguments we have passed:
      digits : padded List of digits in the input String
      remaining4 : Counter using which we have count when to create a new element in the list when we reach 4 digits
      x : contains intermediate sum using which we create 4 digit number by picking four digits from the List
      lst : This is our list which contains output

      Sample input : makeListOfDigits([0,0,0,1,2,3,4,5,6,7,8,9], 3, 0, [])
      output : [6789, 2345, 1]
  }
  {
      val pad0 = fn : char list -> char list
      This function adds padding to character array which we got by exploding string to get length divisible by 4.
  }
  {
      val charToInt = fn : char -> int
      This function is a mapper function which I have used to convert character elements of an array to integers.
  }
*)
fun fromString("") = []
|   fromString(s)  =
                let
                    val chars=String.explode(s)

                    fun makeListOfDigits([], _, _, l)                        = l
                    |   makeListOfDigits(digits as a::t, remaining4, x, lst) =
                          if remaining4=0 then
                              makeListOfDigits(t, 3, 0, (x+a)::lst)
                          else if remaining4=1 then
                              makeListOfDigits(t, 0, a*10+x, lst)
                          else if remaining4=2 then
                              makeListOfDigits(t, 1, a*100+x, lst)
                          else
                              makeListOfDigits(t, 2, a*1000+x, lst)

                    fun pad0 l =
                      if Int.mod(length(l),4) = 0 then l
                      else pad0(#"0"::l);

                    fun charToInt(c)   =
                      if Char.isDigit(c) then (ord(c)-48)
                      else raise notADigit;


                in
                    revH(makeListOfDigits(map charToInt (pad0 chars), 3, 0, []))
                end;
(*
  This fuction converts a list of digits of base B=10^4 into a String.
  We have created some other functions inside this function to make computation easier.
  And these functions are as follows :

  {
    val remove0Char = fn : char list -> char list

    This function takes charater list and returns characters list by removing all the leading 0 characters
    as leading zeros have no significance in number.
  }
  {
    val intToCharArray = fn : int list -> char list

    This function takes list of digits in base 4 and returns the character array representing as a character for a digit in actual number.
    And after we get character list we just implode the list to get the String
  }

*)
fun toString([])        = ""
|   toString(l as h::t) =
                let

                  fun remove0Char([]) = []
                  |   remove0Char(lst as h::t) =  if Char.compare(h,#"0")=EQUAL then remove0Char(t)
                                                else lst

                  fun intToCharArray ([]) = []
                  |   intToCharArray (l as i::t) =
                    let
                      val x0=i mod 10 + 48
                      val x1=(i div 10) mod 10 + 48
                      val x2=(i div 100) mod 10 + 48
                      val x3=(i div 1000) mod 10 + 48
                    in
                      appendList(revH([Char.chr(x3), Char.chr(x2), Char.chr(x1), Char.chr(x0)]),intToCharArray(t))
                    end

                  val revList=revH(l)
                in
                  implode (remove0Char(revH(intToCharArray(revList))))
                end;

(*
  This function takes two lists of same length containg numbers of base 4 and adds both lists and returns another list.

  ex. we want to make sum of two 8 digit numbers
      1234,5678+9999,9999

      So now we will pass the list after splitting into four digits i.e.
      add_lst([1234, 5678], [9999, 9999], [], 0)
          o/p : [1,1234,5677] which is actual(not reversed) sum of two 8 digit numbers

  (
  Here we will get match nonexhaustive warning as we have guessed a precondition that both the input lists are of same length
  which we have made sure while calling this function by using adjustLength function.
  )
*)

fun add_lst(a,b) =
                let
                  fun add_lst0([],[],result as h::t, carry) =
                                  if carry=0 then result
                                  else carry::result
                  |   add_lst0(a as ah::at, b as bh::bt, result, carry)  =
                    let
                      val sum = (ah+bh+carry) mod 10000
                      val carry = (ah+bh+carry) div 10000
                    in
                      add_lst0(at, bt, sum::result, carry)
                    end

                  val (A,B) = adjustLength(a,b)
                  val revA=revH(A)
                  val revB=revH(B)
                in
                  add_lst0(revA,revB,[],0)
                end;

(*
  val isAGreaterThanB = fn : int list * int list -> bool

  This function takes two numbers in list of B=10^4 and returns
    true if isAGreaterThanB i.e. A-B will be isPositive
    else it will return true and we will compute b-a which will be positive

  Here returned boolen value indicates wether your answer to the subtraction operation is positive or negative
  if the boolean value is true then answer is positive else it is negative
*)
fun isAGreaterThanB([], []) = true
|   isAGreaterThanB(a, []) = true
|   isAGreaterThanB([], b) = false
|   isAGreaterThanB(A as ah::at, B as bh::bt) =
                if Int.>(ah,bh) then true
                else if ah=bh then isAGreaterThanB(at, bt)
                else false;

(*
  val sub_list = fn : int list * int list -> int list

  This function which takes two lists of integers of base 10^4 and subtracts these two lists
  and returns answer which will always be positive as I will be returning the big_number-small_number in my logic.

  (
  Here we will get match nonexhaustive warning as we have guessed a precondition in sub_list0 that both the input lists are of same length
  which we have made sure while calling this function by using adjustLength function.
  )
*)
fun sub_list([], []) = []
|   sub_list([], b) = b
|   sub_list(a, []) = a
|   sub_list(a as ah::at, b as bh::bt) =
                let

                  fun sub_list0([],[], result, _) = result
                  |   sub_list0([ah], [bh], result as h::t, borrow) =
                    if borrow=0 then (ah-bh)::result
                    else (ah-1-bh)::result
                  |   sub_list0(a as ah::at, b as bh::bt, result, borrow) =
                    let
                      val sub = (ah-borrow)-bh
                    in
                      if Int.<(sub,0) then sub_list0(at, bt, (10000+sub)::result, 1)
                      else sub_list0(at, bt, sub::result, 0)
                    end
                  val (A,B) = adjustLength(a,b)
                  val isPositive = isAGreaterThanB(A,B)
                  val revA=revH(A)
                  val revB=revH(B)
                in
                  if isPositive then sub_list0(revA,revB,[],0)
                  else sub_list0(revB,revA,[],0)
                end;

(*
  val karatsuba = fn : int list -> int list -> int list

  Implement of Karatsuba algorithm for multiplication
*)
fun karatsuba [] [] =[]
|   karatsuba [] b = b
|   karatsuba a [] = a
|   karatsuba [a] [b] =[(a*b) div 10000, (a*b) mod 10000]
|   karatsuba (a as ah::at) (b as bh::bt) =
                let
                  val (X,Y)=adjustLength(a,b)
                  val m = (lenH(X) + 1) div 2
                  val (X1, X0) = splitList(X)
                  val (Y1, Y0) = splitList(Y)
                  val (x1,x0)=adjustLength(X1,X0)
                  val (y1,y0)=adjustLength(Y1,Y0)
                  val z2 = karatsuba x1 y1
                  val z0 = karatsuba x0 y0
                  val diff_x = sub_list(x0,x1)
                  val sign_x = isAGreaterThanB(x0,x1)
                  val diff_y = sub_list(y1,y0)
                  val sign_y = isAGreaterThanB(y1,y0)
                  val modProdOfDiff = karatsuba diff_x diff_y
                in
                  if sign_x andalso sign_y then add_lst(add_lst(append0(add_lst(add_lst(z2, modProdOfDiff), z0), m), z0), append0(z2,m*2))
                  else add_lst(add_lst((append0(sub_list(add_lst(z2, z0),modProdOfDiff), m)), append0(z2,m*2)), z0)
                end;
fun karatsuba0 a b = let
                    val (A,B) = adjustLength(a,b)
                 in
                    karatsuba A B
                 end;

(*
  val factorial = fn : string -> string

  This function implements factorial
*)
fun factorial(s) =
  let
    fun remove0([])=[]
    |   remove0(lst as h::t)= if h=0 then remove0(t)
                              else lst
    fun factorial0([0]) = [1]
    |   factorial0(n) = remove0(karatsuba0 (n) (factorial0(sub_list(n,[1]))) )
  in
    toString(factorial0(fromString(s)))
  end;
