(*
  Assignment 1 : Introduction to ligical and functional programming

  HET PATEL (2019MCS2562)

  Here in this document I have implemented factorial of any number using karatsuba's multiplication algorithm.

  Here are the function names and short summery of what these function do.

  1.  revH = fn : 'a list -> 'a list --> Reverses given list

  2.  lenH = fn : 'a list -> int --> Gives length of given list

  3. remove0FromFront = fn : int list -> int list --> This function removes all the leading zeros from the list

  4.  adjustLength = fn : int list * int list -> int list * int list -->
          Takes two lists of different sizes and returns two list of same size by appending 0 in front of small list

  5.  split = fn : 'a list -> 'a list * 'a list -->
          This function splits given list and returns two lists such that length of first list is
          (length of input length) div 2 and remaining components in other one

  6.  appendList = fn : 'a list * 'a list -> 'a list -->
          This function takes two lists and append them and gives one list

  7.  fromString = fn : string -> int list -->
          This function takes a String and gives a list of numbers which are in base B=10^4 form

  8.  toString = fn : int list -> string -->
          This function is reverse of fromString i.e. it takes list of numbers in base 10^4 and generates String

  9.  append0Pow = fn : int list * int -> int list -->
          This function appends 0's at the end m thimes(which is given in parameter) at the end of the list
          to replicate multiplication by 10^(4*m) as our elements in list are in Base 10^4

  10.  add_list = fn : int list * int list -> int list -->
          This function adds twos lists of digits in base B=10^4 and returns list of same type

  11. isAGreaterThanB = fn : int list * int list -> int -->
          This function checks whether first list of digits(B=10^4) is greater than second list or not
          This function helps is decide sign of outcome of subtraction operation

  12. sub_list = fn : int list * int list -> int list * int -->
          This function gives us mod(A-B) where A is first list of digits and B is second list of digits in base 10^4 ans also sign of output using isAGreaterThanB

  13. decr = fn : int list -> int list --> This function decrements one from the input list

  14. karatsuba = fn : int list -> int list -> int list -->
          This is implementation of karatsuba's multidigit multiplication for multiplication of two lists of ddigits
          of base 10000 and return list of same kind. This function will be used in finding factorial of large numbers

  15. factorial = fn : string -> string -->
          This function takes numerical String as an input and returns a string containing factorial of that number
*)

(*This exception will be thrown when input String is not a digit*)
exception Invalid_Input_exception of string

(*This exception will be thrown when input is empty*)
exception EmptyInput of string

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
fun lenH([]) = 0
|   lenH(lst as h::t) =
      let
        fun lenH0([], len)=len
        |   lenH0(lst as h::t, len)=lenH0(t, len+1)
      in
        lenH0(lst, 0)
      end;

(*
  This function removes zeros from the front of the list. If the input list contains all 0 then output will be [0]
*)
fun remove0FromFront([])=[]
|   remove0FromFront([0])=[0]
|   remove0FromFront(lst as h::t)= if h=0 then remove0FromFront(t)
                        else lst;

(*
  This function takes two lists as parameter and makes size of both lists same by prepanding 0's to the small list
*)
fun adjustLength([],[])=([],[])
|   adjustLength(a,b)=
          let
            val A=remove0FromFront(a)
            val B=remove0FromFront(b)
            val lenA=lenH(A)
            val lenB=lenH(B)

            fun addLen (lst, 0)= lst
            |   addLen (lst, inc) =
              let
                val inc_1 = inc-1
              in
                addLen(0::lst, inc_1)
              end;

          in
            if lenA < lenB then (addLen(A, lenB-lenA),B)
            else if lenA=lenB then (A,B)
            else (A, addLen(B, lenA-lenB))
          end;

(*
  This function splits the list in the argument in two parts
*)
fun split([]) = ([], [])
|   split([a]) = ([], [a])
|   split(a as ah::at) =
      let
        val len = lenH(a)
        val splitPoint = len div 2
        fun splitLists([], A, _)=(revH(A), [])
        |   splitLists(lst,ans,0) = (revH(ans), lst)
        |   splitLists(lst as h::t,ans,point) = splitLists(t, h::ans, point-1)
      in
        splitLists(a, [], splitPoint)
      end;

(*
  This function appends two input lists
*)
fun appendList([], []) = []
|   appendList(a, []) = a
|   appendList([], b) = b
|   appendList(a as ah::at, b as bh::bt) =
      let
        val revA=revH(a)
        fun append([], ans)=ans
        |   append(x as xh::xt, y) = append(xt, xh::y)
      in
        append(revA,b)
      end;

(*
  val fromString = fn : string -> int list
  This function takes input String and returns list of digits in base B=10^4

  I have created some helper function within this function which are as follows

  {
    val makeListOfDigits = fn : int list * int * int * int list -> int list

    This function takes list containing digits as an input and gives list which contains number of base 10^4.
    Here in the Arguments I have passed:
      digits : padded List of digits in the input String
      remaining4 : Counter using which I have count when to create a new element in the list when I reach 4 digits
      x : contains intermediate sum using which I create 4 digit number by picking four digits from the List
      lst : This is our list which contains output

      Sample input : makeListOfDigits([0,0,0,1,2,3,4,5,6,7,8,9], 3, 0, [])
      output : [6789, 2345, 1]
  }
  {
      val pad0 = fn : char list -> char list
      This function adds padding to character array which I got by exploding string to get length divisible by 4.
  }
  {
      val charToInt = fn : char -> int
      This function is a mapper function which I have used to convert character elements of an array to integers.
  }
*)
fun fromString("") = raise EmptyInput "Your input is empty, please input any number"
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
                      else raise Invalid_Input_exception ("The input "^s^" is invalid")


                in
                    revH(makeListOfDigits(map charToInt (pad0 chars), 3, 0, []))
                end;
(*
  This fuction converts a list of digits of base B=10^4 into a String.
  I have created some other functions inside this function to make computation easier.
  And these functions are as follows :

  {
    val remove0Char = fn : char list -> char list

    This function takes charater list and returns characters list by removing all the leading 0 characters
    as leading zeros have no significance in number.
  }
  {
    val intToCharArray = fn : int list -> char list

    This function takes list of digits in base 4 and returns the character array representing as a character for a digit in actual number.
    And after I get character list I just implode the list to get the String
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
                      revH([Char.chr(x3), Char.chr(x2), Char.chr(x1), Char.chr(x0)])@intToCharArray(t)
                    end

                  val revList=revH(l)
                in
                  implode (remove0Char(revH(intToCharArray(revList))))
                end;

(*
  This function append zeros to the end of the list replication addition by 10^4 by appending 1 zero to the list
*)
 fun append0Pow(lst, 0) = lst
 |   append0Pow(lst, n) =
      let
        val revLst =revH(lst)
        fun append0(l,0) = revH(l)
        |   append0(l,m) = append0(0::l,m-1)
      in
        append0(revLst, n)
      end;

(*
  This function takes two lists containg numbers of base 4 and adds both lists and returns another list.

  ex. we want to make sum of two 8 digit numbers
      1234,5678+9999,9999

      So now we will pass the list after splitting into four digits i.e.
      add_list([1234, 5678], [9999, 9999], [], 0)
          o/p : [1,1234,5677] which is actual(not reversed) sum of two 8 digit numbers

*)
fun add_list([],b)=b
|   add_list(a,[])=a
|   add_list(a,b)=
      let
        val a_stripped = remove0FromFront(a)
        val b_stripped = remove0FromFront(b)
        val (adjustedX,adjustedY) = adjustLength(a_stripped, b_stripped)
        val x=revH(adjustedX)
        val y=revH(adjustedY)
        fun add_lst_helper([], [], ans, carry)=
              if carry=0 then ans
              else 1::ans
        |   add_lst_helper([], b, _, _)=b
        |   add_lst_helper(a, [], _, _)=a
        |   add_lst_helper (l1 as h1::t1, l2 as h2::t2, ans, carry) =
          let
            val sum=h1+h2+carry
          in
            if sum>=10000 then add_lst_helper(t1, t2, (sum mod 10000)::ans, 1)
            else add_lst_helper(t1, t2, sum::ans, 0)
          end
      in
        add_lst_helper(x,y,[],0)
      end;

(*
  val isAGreaterThanB = fn : int list * int list -> int

  This function takes two numbers in list of B=10^4 and returns
    1 if isAGreaterThanB i.e. A-B will be isPositive
    -1 if A is less than B
    and 0 if both lists are the same

  Here returned int value indicates wether your answer to the subtraction operation is positive or negative
  if the int value is 1 then answer is positive else if it is ~1 then it is negative and if it is zero ans is 0
*)

fun isAGreaterThanB ([], []) = 0
|   isAGreaterThanB ([], b) = ~1
|   isAGreaterThanB (a, []) = 1
|   isAGreaterThanB (a as ah::at, b as bh::bt) =
        let
          val a_stripped = remove0FromFront(a)
          val b_stripped = remove0FromFront(b)
          val (x,y) = adjustLength(a_stripped, b_stripped)
          fun isAGreaterThanB_helper([], []) = 0
          |   isAGreaterThanB_helper(a, []) = 1
          |   isAGreaterThanB_helper([], b) = ~1
          |   isAGreaterThanB_helper(l1 as h1::t1, l2 as h2::t2) =
                if h1>h2 then 1
                else if h1<h2 then ~1
                else isAGreaterThanB_helper(t1, t2)
        in
          isAGreaterThanB_helper(x,y)
        end;

(*
  val sub_list = fn : int list * int list -> int list

  This function which takes two lists of integers of base 10^4 and subtracts these two lists
  and returns answer which will always be positive as I will be returning the big_number-small_number in my logic.
*)

fun sub_list(a, []) = (a, 1)
|   sub_list([], b) = (b, ~1)
|   sub_list(a, b) =
      let
        val a_stripped = remove0FromFront(a)
        val b_stripped = remove0FromFront(b)
        val (x,y) = adjustLength(a_stripped, b_stripped)
        val sign=isAGreaterThanB(x,y)
        val xRev=revH(x)
        val yRev=revH(y)
        fun sub_list_helper([], [], ans, borrow) = ans
        |   sub_list_helper([], b, _, _) = b
        |   sub_list_helper(a, [], _, _) = a
        |   sub_list_helper(l1 as h1::t1, l2 as h2::t2, ans, borrow) =
              let
                val sub=h1-borrow-h2
              in
                if sub<0 then sub_list_helper(t1, t2, (sub+10000)::ans, 1)
                else sub_list_helper(t1, t2, sub::ans, 0)
              end
        val ans = if sign = 1 then sub_list_helper(xRev,yRev,[],0)
        else if sign = ~1 then sub_list_helper(yRev,xRev,[],0)
        else [0]
      in
        (remove0FromFront(ans), sign)
      end;

(*
  This function decrements the value represented by input list by 1 which will be used in factorial function
*)
fun decr([]) = []
|   decr([0]) = [0]
|   decr(l) =
    let
      val sub_tuple = sub_list(l,[1])
    in
      remove0FromFront(#1 sub_tuple)
    end;

(*
  val karatsuba = fn : int list -> int list -> int list

  Implement of Karatsuba algorithm for multiplication
*)
fun karatsuba [] [] = []
|   karatsuba a b =
    let
      val strippedA = remove0FromFront(a)
      val strippedB = remove0FromFront(b)

      fun karatsuba_helper [0] [0] = [0]
      |   karatsuba_helper a [0] = [0]
      |   karatsuba_helper [0] b = [0]
      |   karatsuba_helper [1] b = b
      |   karatsuba_helper a [1] = a
      |   karatsuba_helper [a] [b] = if a*b>=10000 then [a*b div 10000, a*b mod 10000]
                              else [a*b]
      |   karatsuba_helper a b =
            let
              val a_stripped = remove0FromFront(a)
              val b_stripped = remove0FromFront(b)
              val (A,B) = adjustLength(a_stripped, b_stripped)
              val len = lenH A
              val m = (len+1) div 2
              val (x1, x0) = split(A)
              val (y1, y0) = split(B)
              val z2 = karatsuba x1 y1
              val z0 = karatsuba x0 y0
              val z2Plusz0 = add_list(z2, z0)
              val (x0SubX1, signX) = sub_list(x0, x1)
              val (y1SubY0, signY) = sub_list(y1, y0)
              val mulForZ0 = karatsuba x0SubX1 y1SubY0
              val z1 = if signX*signY = 1 then (add_list(z2Plusz0, mulForZ0),0)
              else sub_list(z2Plusz0, mulForZ0)
              val z2Pow = append0Pow(z2, m*2)
              val z1Pow = append0Pow(#1 z1, m)
              val sumOfz2z1Pow = add_list(z2Pow, z1Pow)
              val ans = add_list(sumOfz2z1Pow, z0)
            in
              remove0FromFront(ans)
            end
    in
        karatsuba_helper strippedA strippedB
    end;

(*
  val factorial = fn : string -> string

  This function implements factorial
*)
fun factorial "" = raise EmptyInput "Your input is empty Please enter any number"
|   factorial n =
      let
        (*val t =Time.now()*)
        val ipList=fromString n
        fun factorial_helper [0] = [1]
        |   factorial_helper n = karatsuba n (factorial_helper(decr(n)))
      in
      (* I wrote this code to get running time of factorial in seconds
      (
      toString(factorial_helper(ipList)), print(Time.toString(Time.-(Time.now() , t))^"\n")
      )
      *)
      toString(factorial_helper(ipList))

      end
        handle Invalid_Input_exception s => s
        |  EmptyInput s => s
