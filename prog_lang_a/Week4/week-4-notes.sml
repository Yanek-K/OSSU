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
fun double x = x * 2;
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

