(* Coursera Programming Languages, Homework 3, Provided Code *)
(*
Function only_capitals 
	 Takes a string list and returns a string list		
	 only the strings in the argument that start 
	 with an uppercase letter. 
	 Assume all strings have at least 1 character.	    
	 Use List.filter, Char.isUpper, and String.sub 
	 to make a 1-2 line solution.

String.sub (s, i) returns the char at ith index of s
*)

fun get_first_letter x =
    String.sub (x, 0);

val test1 = get_first_letter "abc";

fun check_if_upper x =
    Char.isUpper (get_first_letter x);

val test2 = check_if_upper "Abc";

fun only_capitals xs =
    case xs of
	[] => []
      | x::xs' => if check_if_upper x then x :: only_capitals xs'
		  else only_capitals xs';

val test3 = only_capitals (["abc", "def", "De", "Cd"]);

fun only_capitals_2 xs =
    List.filter (check_if_upper x, xs);

(*
fun only_capitals xs =
    let fun get_first_letter x =
	    String.sub (x, 0)
	fun check_if_upper x =
	    Char.isUpper (get_first_letter x)
    in
	List.filter (check_if_upper, xs)
    end;
*)	
    
(*
exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
*)
