
(* From Week 2 *)

(* Alternate 
intlist => int
Takes a list of number and adds them with alternating signs
 *)

fun alternate intlist =
    case intlist of
	[] => 0
     |  head::[]  => head
     |  head::neck::rest => head - neck + alternate rest;

val alternate_test1 = alternate [1,2,3,4] = ~2;
val alternate_test2 = alternate [2,3,4,5] = ~2;

fun alternate2 intlist =
    let fun helper (factor, intlist) =
	    case intlist of
		[] => 0
	     |  x::intlist => factor * x + helper (~1 * factor, intlist)
    in
	helper (1, intlist)
    end;

val alternate2_test1 = alternate2 [1,2,3,4] = ~2;

(* Min/Max
int list => int * int 
Produces the minimum and maximum numbers in the list
*) 


fun min_max intlist =
    let fun helper (min, max, intlist) =
	    case intlist of
		[] => (min, max)
	     |  x::intlist => helper (	  
				 if x < min then x else min,
				 if x > max then x else max,
				 intlist)
    in
	case intlist of
	    x::intlist => helper (x,x,intlist)
	  | [] => raise List.Empty 
    end;


val min_max_test1 = min_max ([1,2,3,4]) = (1,4);
val min_max_test2 = min_max ([0,1,2,~10]) = (~10,2);
val min_max_test3 = min_max ([100]) = (100,100);

(* Cumsum
int list -> int list
Produces a list of the partial sums of those numbers e.g. [1,4,20] = [1,5,25]
 *)

fun cumsum intlist =
    let fun helper (res, intlist) =
	    case intlist of
		[] => [res]
	     |  x::intlist => res :: helper (x + res, intlist)
    in
	case intlist of
	    x :: intlist => helper (x, intlist)
	 |  []  => raise List.Empty
    end;


val cumsum_test1 = cumsum ([1,4,20]) = [1,5,25];

(* Greeting
string option -> string
Produces a string "Hello there, ...!" where ... is the string
String is an option Some returns a value, None returns "you".
 *)

fun greeting str =
    case str of
	NONE => "Hello there, you!"
      | SOME str => "Hello there, " ^ str ^ "!";

val greeting_test1 = greeting (SOME "Mike") = "Hello there, Mike!";
val greeting_test2 = greeting (NONE) = "Hello there, you!";
