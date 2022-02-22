

(* 
Date * Date -> Boolean 
Produces True if first argument comes before the second
Produces False if both arguments are the same
*)

fun is_older (date1: int*int*int, date2: int*int*int) =
    if #1 date1 < #1 date2 then true
    else if
	#1 date1 = #1 date2
	andalso #2 date1 < #2 date2
    then true
    else
	#1 date1 = #1 date2
	andalso #2 date1 = #2 date2
	andalso #3 date1 < #3 date2;

(* 
Listof Dates * Month (i.e. an int) -> Int
Produces the number of months that are in the given month
*)

fun number_in_month (date: (int*int*int) list, month: int) =
    if null date then 0
    else if (#2 (hd date)) = month
    then 1 + number_in_month (tl date, month)
    else number_in_month (tl date, month);

(* 
Listof Dates * Listof Months -> Int 
Produces the number of dates in the list that are in the list of months
*)

fun number_in_months (dates : (int*int*int) list, months: int list) =
    if null months orelse null dates then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months);

(* 
Listof Dates * Month (int) -> ListofDates
Produces a list of dates that are in the month
*)

fun dates_in_month (dates: (int*int*int) list, month: int) =
    if null dates then []
    else if
	(#2 (hd dates)) = month
    then hd dates :: dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month);

(* 
ListofDates * Listof Months -> ListofDates 
Produces a list containing the dates that are in the list of months 
ASSUME: The list of months has no repeated numbers
*)

fun dates_in_months (dates: (int*int*int) list, months: int list)  =
    if null dates orelse null months then []
    else dates_in_month(dates, hd months) @ dates_in_months (dates, tl months);

(*
listofString * n:int -> String
Returns the nth element of the list
*)

fun get_nth (strings: string list, n:int) =
    if null strings then ""
    else if n = 1
    then hd strings
    else get_nth (tl strings, n - 1);

(*
Date -> String
Returns a string in the form "January 20, 2013"
*)

fun date_to_string (date: int*int*int) =
    let val month_list =
	    ["January", "February", "March", "April",
	     "May", "June", "July", "August", "September",
	     "October", "November", "December"]
	
    in
	get_nth (month_list, #2 date)
	^ " " ^  Int.toString (#3 date)
	^ ", " ^ Int.toString (#1 date)
    end;

(* 
Sum (int) * int list -> int
ASSUME: Sum is positive, int list is positive, entire list sums to more than sum
Produces an int n such that the first n elements add to less 
than the sum, the first n + 1 elements add to sum or more
*)

fun number_before_reaching_sum (sum: int, nums: int list) =
    if sum > hd nums
    then 1 + (number_before_reaching_sum (sum - hd nums, tl nums))
    else 0;

(* 
int -> month
Produces the month the day is in
*)

fun what_month (day: int) =
    let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 1 + number_before_reaching_sum (day, days_in_months)
    end;


(* 
Day1 * Day2 -> int list 
Returns an int list where m1 is the month of day1, m2 is the month of day1+1 
and mn is the month of day2
Length is 0 if day1> day2
*)

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1 + 1, day2);

(* 
date list -> int*int*int OPTION
Produces NONE if the list has no dates and SOME d if the date d is the oldest
*)

fun oldest (dates : (int*int*int) list) =
    if null (dates)
    then NONE
    else
        let
          val tl_ans = oldest (tl dates)
        in
          if isSome (tl_ans) andalso is_older (valOf tl_ans, hd dates)
          then tl_ans
          else SOME (hd dates)
        end;

(* 
Listof Dates * Listof Months -> Int 
Produces the number of dates in the list that are in the list of months
*)

fun number_in_months_challenge (dates : (int*int*int) list, months: int list) =
    if null months orelse null dates then 0
    else if number_in_month (dates, hd months) > 0 
    then 1 + number_in_months (dates, tl months)
    else number_in_months (dates, tl months);


