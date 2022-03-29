(* Coursera Programming Languages, Homework 3, Provided Code *)
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


(***************  START HERE ************** *)

			       
(*--  #1  --*)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper (String.sub (x,0))) xs;


(*--  #2  --*)
fun longest_string1 xs =
    List.foldl (fn (x, y) => if String.size y >= String.size x
			     then y else x) "" xs;


(*--  #3  --*)
fun longest_string2 xs =
    List.foldl (fn (x, y) => if String.size y > String.size x
			     then y else x) "" xs;

(*--  #4 --*)
fun longest_string_helper f =
    List.foldl (fn (x,y) => if f (String.size x, String.size y)
			    then y else x) "";
    
val longest_string3 = longest_string_helper (fn (x,y) => (y >= x));
val longest_string4 = longest_string_helper (fn (x,y) => (y > x));

	
(*--  #5  --*)
val longest_capitalized = longest_string3 o only_capitals;


(*--  #6  --*)
val rev_string = String.implode o List.rev o String.explode;

						 
(*--  #7 --*)
fun first_answer f ls =
    case ls of
	[] => raise NoAnswer
      | x::xs' => case f(x) of
		      NONE => first_answer f xs'
		    | SOME v => v; 

(*--  #8 --*)
fun all_answers f lst =
    let val has_none = List.exists (fn x => f(x) = NONE)
	val final_result = List.foldl (fn (x, acc) => (case f(x) of SOME v => v@acc))
    in
	case lst of
	    [] => SOME []
	  | _ => if has_none lst
		 then NONE
		 else SOME (final_result [] lst)
    end;
		     
	   

(*--  #9 --*)

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard   => f1 ()
	  | Variable x => f2 x
	  | TupleP ps  => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end;

(*-- #9a --*)

fun count_wildcards (p: pattern) =
    g (fn v => 1) (fn v => 0) p;

(*-- #9b --*)

fun count_wild_and_variable_lengths (p:pattern) =
    g (fn v => 1) (fn x => String.size x) p;

(*-- #9c --*)

fun count_some_var (s,p) =
    g (fn v => 0) (fn x => if x = s then 1 else 0) p;

(*--  #10 --*)
(*--  #11 --*)
(*--  #12 --*)
