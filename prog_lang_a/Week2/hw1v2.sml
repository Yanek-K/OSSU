

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
    else if
	#1 date1 = #1 date2
	andalso #2 date1 = #2 date2
	andalso #3 date1 < #3 date2
    then true
    else false;

(* 
Listof Dates * Month (i.e. an int) -> Int
Produces the number of months that are in the given month
*)

fun number_in_month (date: (int*int*int) list, month: int) =
    if null date then 0
    else if
	(#2 (hd date)) = month
    then 1 + number_in_month (tl date, month)
    else number_in_month (tl date, month);

(* 
Listof Dates * Listof Months -> Int 
Produces the number of dates in the list that are in the list of months
*)

fun number_in_months (dates : (int*int*int) list, months: int list) =
    if null months orelse null dates then 0
    else if number_in_month (dates, hd months) > 0 
    then 1 + number_in_months (dates, tl months)
    else number_in_months (dates, tl months);

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


