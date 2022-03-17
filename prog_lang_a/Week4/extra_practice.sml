(** Week 4 Extra Practice Problems **)

fun map (f, xs) =
    case xs of
	[] => []
      | x::xs' => (f x) :: map (f, xs'); 

fun filter (f, xs) =
    case xs of
	[] => []
      | x::xs' => if f x then x :: (filter (f, xs'))
		  else filter (f, xs');

fun remove_vowels xs =
    let fun is_vowel x =
	    case x of
		"a" => false
	      | "e" => false
	      | "i" => false
	      | "o" => false
	      | "u" => false
	      |  _ => true
    in
    filter (is_vowel, xs)
    end;

val one_string = ["a", "e", "o", "y", "k", "p"];
val one_string_no_vowels = remove_vowels (one_string);


(* Produces pass if a students grade is >= 75 else fail *)
type student_id = int;
type grade = int (* must be in 0 - 100 range *);
type final_grade = { id : student_id, grade : grade option};
datatype pass_fail = pass | fail;

fun pass_or_fail students =
    let fun passed student =
	    case student of
		{id = _, grade = SOME grade} => if grade >= 75 then true else false
	      | _ => false;
    in
	filter (passed, students)
    end;
	    
val test1 = pass_or_fail ([{id=1, grade = SOME 70},
			   {id=2, grade = SOME 100},
			   {id=3, grade = SOME 90},
			   {id=4, grade = SOME 10}]);

fun square xs =
    map (fn x => x * x, xs);

val test2 = square ([1,2,3,4,5]);

fun is_even xs =
    filter (fn x => x mod 2 = 0, xs);

val test3 = is_even ([1,2,3,4,5,6]);

fun greater_than_four xs =
    filter (fn x => x > 4, xs);

val test4 = greater_than_four ([1,2,3,4,5,66,7,8,9,864,56,3,45,24,524,2]);

fun fizzbuzz xs =
    let fun find_mapping x =
	    if x mod 5 = 0 andalso x mod 3 = 0 then "fizzbuzz"
	    else if x mod 3 = 0 then "fizz"
	    else if x mod 5 = 0 then "buzz"
	    else Int.toString (x)
    in
	map (find_mapping, xs)
    end;
		
val test5 = fizzbuzz ([1,2,3,4,56,15,6,23,2,5,31,1,51,35135,3151,2]);


fun allShorterThan1 (xs, s) =
    filter (fn x => String.size x < String.size s, xs);

fun allShorterThan2 (xs, s) =
    let val i = String.size s
    in
	filter (fn x => String.size x < i, xs)
    end;


(* Fold and Closures *)

(* Fold takes a function, an accumulator and a list *)
(* It will apply f to the accumulator and the first element in the list, 
   then recurse with the newly updated accumulator to the next element 
   in the list 

   This version goes from left to right, you can do the opposite as well! *)

fun foldl (f, acc, xs) =
    case xs of
	[] => acc
      | x::xs' => foldl (f, f (acc, x), xs');

(* This iterator like function seperates recursive traversal from data processing *)

fun f1 xs = foldl ((fn (x, y) => x + y), 0, xs); (* sum list *)

val test6 = f1 ([1,2,3,4]);

fun f2 xs = foldl ((fn (x, y) => x andalso y >= 0, true, xs)); (* checks if all element
								  are non-negative *)

val test7 = f2 ([1,2,3,~3]);

(* Counts the number of items that are between lo and hi, inclusive *)

fun f3 (xs, lo, hi) =
    foldl ((fn (x,y) => x + (if y >= lo andalso y <= hi then 1 else 0)), 0, xs);

val test8 = f3 ([1,2,3,4,5,6,7,7,4,23,324,34,234,22,64],4,100);

(* Produces true if every element in the list is shorter than s *)

fun f4 (xs, s) =
    let val i = String.size s
    in
	foldl ((fn (x, y) => x andalso String.size y < i), true, xs)
    end;

val test9 = f4 (["ab", "ce", "dedf", "asd", "zx"], "asdf");

fun f5 (g, xs) =
    foldl ((fn (x,y) => x andalso g y), true, xs);

fun f4again (xs, s) =
    let val i = String.size s
    in
	f5 (fn y => String.size y < i, xs)
    end;

val test10 = f4again (["ab", "cd", "erf", "qwe", "asdf"], "asd");


type student_id = int;
type grade = int (* must be in 0 - 100 range *);
type final_grade = { id : student_id, grade : grade option};

type date = int * int * int;

fun number_in_months (dates, month) =
    case dates of
	[] => 0
      | (y,m,d)::xs' => if m = month then 1 + number_in_months (xs', month)
			else number_in_months (xs', month);

val test11  = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],
				2);

fun get_month date =
    case date of (y, m, d) => m;
    
fun number_in_months2 (dates, month) =
    foldl (fn (x,y) => x + (if (get_month y) = month then 1 else 0), 0, dates)

val test12 = number_in_months2 ([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)],
				2);

fun dates_in_month (dates, month) =
    filter (fn x => (get_month x) = month, dates);

val test13 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test14 = dates_in_month ([(2012,1,23),(2012,2,23),(2013,1,23)], 1) = [(2012,1,23),(2013,1,23)];

fun dates_in_months (dates, months) =
    foldl (fn (x, y) => dates_in_month (dates, y)::x, [], months);
									     

val test15 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]);


											       
(** 1
('b -> c' option) -> ('a -> 'b option) -> 'a -> 'c option 
Composes two functions with "optional" values. If either returns NONE, then the result is NONE 
 *)

