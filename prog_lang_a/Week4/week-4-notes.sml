(* Classroom notes from Week 4 *)

(** Taking Functions as Arguments **)

fun increment_n_times_bad (n,x) =
    if n = 0
    then x
    else 1 + increment_n_times_bad (n-1, x);

fun double_n_times_bad (n, x) =
    if n = 0
    then x
    else 2 * double_n_times_bad (n-1, x);

fun nth_tail_bad (n, xs) =
    if n = 0
    then xs
    else tl (nth_tail_bad (n-1, xs));

fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times(f, n-1, x));

fun increment x = x + 1;
fun double x = x + x;
fun triple x = x * 3;

val double_n_times = n_times (double, 10, 5);
val increment_n_times = n_times(increment, 10, 5);
val get_nth_tail = n_times(tl, 3, [1,2,3,4,5,6,7]);

fun addition (n,x) = n_times(increment, n, x);
fun double_n_times (n,x) = n_times(double, n, x);
fun nth_tail (n,x) = n_times(tl, n, x);



(** Anonymous Functions **)

fun triple_n_times_bad (n,x) = n_times(triple, n, x);

fun triple_n_times_ok (n,x) =
    let fun triple x = 3 * x
    in
	n_times(triple, n, x)
    end;

fun triple_n_times_better (n, x) =
    n_times (let fun triple x = 3 * x in triple end, n, x);

fun triple_n_times_good (n, x) =
    n_times (fn x => 3*x, n, x); 



(** Map and Filter **)

fun map (f, xs) =
    case xs of
	[] => []
      | x::xs' => (f x) :: map(f, xs');

val map1 = map (fn x => x + 1, [1,2,3,4]);
val map2 = map (hd, [[1,2],[3,4],[5,6]]);

fun filter (f, xs) =
    case xs of
	[] => []
      | x::xs' => if (f x)
		  then x::filter(f, xs')
		  else filter(f, xs');

fun is_even v =
    (v mod 2 = 0);

fun all_even xs = filter (is_even, xs);

fun all_even_2 xs = filter ((fn (_,v) => is_even v), xs);



(** Generalizing **)

(* Returning a function *)

fun double_or_triple f = (* (int -> bool) -> (int -> int) *)
    if f 7
    then fn x => 2 * x
    else fn x => 3 * x;

val double = double_or_triple (fn x => x - 3 = 4); (* Produces the function 2 * x *)
val nine = (double_or_triple (fn x => x = 42)) 3; (* Passes 3 to the function 3 * x *)



(* Higher order functions over our own datatypes *)

datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp;

(* given an exp, is every constant in it an .... *)

fun true_of_all_constants (f, e) =
    case e of
	Constant i => f i 
      | Negate e1 => true_of_all_constants (f, e1)
      | Add (e1, e2) => true_of_all_constants (f, e1)
			andalso true_of_all_constants (f, e2)
      | Multiply (e1, e2) => true_of_all_constants (f, e1)
			     andalso true_of_all_constants (f, e2);

fun all_even e = true_of_all_constants ((fn x => x mod 2 = 0), e);



(** Lexical Scope **)

(* We use the scope WHERE THE FUNCTION IS DEFINED, not where it 
   was called - Semantics (rules) called Lexical Scope *)

(* How can functions be evaluated in an old environment that is not around
   anymore? - The language implementation keeps the environment around 

   A function value actually has two parts:\
     1. The code
     2. The environment that was current when the function was defined

  This pair is called a CLOSURE, a call evaluates the code in the environment *)



(** Lexical Scope and Higher-Order Functions **)

(* The rule stays the same - use the scope WHERE THE FUNCTION WAS DEFINED
   What the environment is, becomes more interesting (nested-patt, let-expres)
 *)



(** Why Lexical Scope **)

fun filter (f, xs) =
    case xs of
	[] => []
      | x::xs' => if f x then x :: (filter (f, xs'))
		  else filter (f, xs');

fun greater_than_x x = fn y => y > x;

fun no_negatives xs = filter (greater_than_x ~1, xs);

fun all_greater (xs, n) = filter (greater_than_x n, xs);

fun all_greater_2 (xs, n) = filter (fn x => x > n, xs);

(* we look up the values passed to filter WHERE THEY ARE DEFINED! 
   so n comes from calling all_greater with a list and a n *)

val test_all_greater = all_greater ([1,2,5,253,235,12,412,12],7);
val test_all_greater2 = all_greater_2 ([1,2,5,253,235,12,412,12], 7);

(* Dynamic scope is good for errors, we don't need the closest binding, but the
   dynamic binding - not where the function was defined but where the exception 
   was defined *)



(** Closures and Recomputation **)

(* Both versions make no difference to the caller
   The second version can be faster if the string or list is very long 

   We avoid repeating computations that do not depend of function arguments *)

fun all_shorter_than_1 (xs, s) =
    filter (fn x => String.size x < String.size s, xs);

fun all_shorter_than_2 (xs, s) =
    let val size = String.size s
    in
	filter (fn x => String.size x < size, xs)
    end;

(* Use e1 ; e2 to call e1, delete data, then call e2
   Very useful for print statements *)

fun all_shorter_than_3 (xs, s) =
    let val size = (print "Size of s calculated. " ; String.size s)
    in
	filter (fn x => String.size x < size, xs)
    end;

val _ = print "\n With All Shorter Than 3: ";
val x1 =  all_shorter_than_3 (["ab", "cs", "def", "a"], "b");



(** Fold and More Closures **)

(* Accumulates answer by repeatedly applying f to answer so far *)

fun fold (f, acc, xs) =
    case xs of
	[] => acc
      | x::xs' => fold (f, f(acc, x), xs); 

(* examples not using private data *)

fun f1 xs = fold ((fn (x, y) => x + y), 0, xs);

fun f2 xs = fold ((fn (x, y) => x andalso y >= 0, true, xs));

(* examples using private data *)

fun f3 (xs, lo, hi) =
    fold ((fn (x,y) =>
	      x + (if y >= lo andalso y <= hi then 1 else 0)),
	  0, xs);

fun f4 (xs, s) =
    let val i = String.size s
    in
	fold ((fn (x, y) => x andalso y < i), true, xs)
    end;

fun f5 (g, xs) = fold ((fn (x,y) => x andalso g y), true, xs);

fun f4_again (xs, s) =
    let val i = String.size s
    in
	f5 ((fn y => String.size y < i), xs)
    end;



(** Closure Idiom: Combining Functions **)


							    
(** Practice **)

fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times (f, n-1, x));

fun double x =
    2 * x;

val x1 = n_times (double, 4, 7);

fun increment x =
    x + 1;

val x2 = n_times (increment, 1, 100);

val x3 = n_times (tl,2,[4,8,12,16]);

fun triple x =
    3 * x;

val x4 = n_times (triple, 3, 1);

fun triple_n_times_bad (n, x) = n_times (triple, n, x);

fun triple_n_times_good (n, x) =
    let fun triple x = 3 * x
    in
	n_times (triple, n, x)
    end;

fun triple_n_times_different (n, x) =
    n_times (let fun triple x = 3 * x in triple end, n, x);

fun triple_n_time_best (n, x) =
    n_times ((fn y => 3 * y), n, x);

fun increment x = x + 1;
val increment = fn x => x + 1;

fun map (f, xs) =
    case xs of
	[] => []
      | x::xs' => (f x) :: map (f, xs');

fun increment x =
    1 + x;

val x5 = map (increment, [1,2,3]);

val x6 = map ((fn x => x + 1), [2,3,4,5,6]);

val x7 = map ((fn x => Math.sqrt (Real.fromInt x)), [2,3,4,5]);

val lst1=["Alpine", "Avalanche", "Powder", "Snowflake", "Summit"];

val length_of_lst1_items = map ((fn x => String.size x), lst1);

fun filter (f, xs) =
    case xs of
	[] => []
      | x::xs' => if f x
		  then x :: (filter (f, xs'))
		  else (filter (f, xs'));

val lst2 = [1,2,3,4,5,6,7,8,35,3,143,531,124,42,134,51362,623];
val only_even = filter ((fn x => x mod 2 = 0), lst2);

val lst3 = [(1,"next"), (3,"hello"), (4, "me"), (6, "today")];

val get_all_even_pairs = filter ((fn (v,_) => v mod 2 = 0), lst3);

fun get_all_even_pairs_with_4 lst = filter ((fn (v,_) => v = 4),
					filter ((fn (v,_) => v mod 2 = 0, lst)));


val with_4 = get_all_even_pairs_with_4 (lst3);


