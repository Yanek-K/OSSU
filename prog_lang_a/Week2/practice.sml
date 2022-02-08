
(* int list -> int list *)
(* Doubles every number in a list *)

fun double_all (xs : int list) =
    if null xs
    then []
    else (hd xs * 2) :: double_all(tl xs);

double_all([1,2,3,4]); (* Should equal ([2,4,6,8]) *)


(* int -> int list *)
(* Produce the list [1,2,...,x] *)

fun countup_from1 (x : int) =
    let fun count (from : int, to : int) =
	    if from = to
	    then to :: [ ]
	    else from :: count (from + 1, to)
    in
	count (1, x)
    end;

(* A better way to write this is to use the value of x in the local env *)

fun countup_from1_better (x : int) =
    let fun count (from : int) =
	    if from = x
	    then x :: []
	    else from :: count (from+1)
    in
	count 1
    end;


(* int list -> int *)
(* Produces the largest number in the list, 0 if empty *)
(* Assume all numbers in the list are greater than zero *)

fun bad_max (xs : int list) =
    if null xs
    then 0
    else
	if (hd xs) > bad_max (tl xs)
	then (hd xs)
	else bad_max (tl xs);

fun good_max (xs : int list) =
    if null xs
    then 0 (* bad style, see below *)
    else
	let val tl_ans = good_max (tl xs)
	in
	    if hd xs > tl_ans
	    then hd xs
	    else tl_ans
	end;

fun better_max (xs : int list) =
    if null xs
    then NONE
    else
	let val tl_ans = better_max (tl xs)
	in if isSome tl_ans andalso valOf tl_ans > hd xs
	   then tl_ans
	   else SOME (hd xs)
	end;

fun better_max2 (xs : int list) =
    if null xs
    then NONE
    else let (*assume arg is nonempty because it is local *)
	fun max_nonempty (xs : int list) =
	    if null (tl xs) (* xs cannot be [] *)
	    then hd xs
	    else
		let val tl_ans = max_nonempty (tl xs)
		in
		   if hd xs > tl_ans
		   then hd xs
		   else tl_ans
		end
    in
	SOME (max_nonempty xs)
    end;

