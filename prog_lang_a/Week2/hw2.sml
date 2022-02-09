
(* HomeWork for Week 2 *)

(* A date is an SML value of type int * int * int,
   where the first part is a year
   the second part is the month
   the third part is a day

   A reasonable date has a postive year, 
   A  month between 1 and 12 and, 
   A  day no greater than 31, depending on the month

  Solutions need to work only for reasonable dates
  Do not check for dates until challenge problems

  A day of the year is a number for 1 to 365
  E.g. 33 represents February 2 (ignore leap years) *)

(* Function 1 
   Date * Date -> Bool
   Produces true if first arg is date before second arg
   If dates are the same, produce false *)


fun is_older((y1 : int,m1 : int,d1 : int),(y2 : int, m2 : int, d2 : int))=
    if y1 < y2 
    then true 
    else 
	if y1 = y2 andalso m1 < m2 
	then true 
	else 
	    if y1 = y2 andalso m1 = m2 andalso d1 < d2
	    then true 
	    else false;

(* Function 2
   List of Dates * Month -> Int 
   Produces the number of dates in the given month *)

fun number_in_month ((date : (int*int*int) list), (x : int)) =
    if null date
    then 0
    else
	let val tl_ans = number_in_month ((tl date),x)
	in
	    if (#2 (hd date)) = x
	    then 1 + tl_ans
	    else tl_ans
	end;
	

(* Function 3
   List of Dates * ListofMonths -> int 
   Produces the number of dates that are in the list of months *)

fun number_in_months ((dates: (int*int*int) list), (months : int list)) =
    if null months then 0
    else
	let val prev_true = number_in_month (dates, (hd months)) = 1
	in
	    if prev_true
	    then 1 + number_in_months (dates, (tl months))
	    else number_in_months (dates, (tl months))
	end;
	    
(* Function 4 
   Listof Dates * Month -> Listof Dates
   Produces a list hodling the dates that are in the month
   Dates should be in original order *)

fun dates_in_month ((dates: (int*int*int) list), (month : int)) =
    if null dates
    then []
    else
	let val tl_ans = dates_in_month ((tl dates), month)
	in
	    if (#2 (hd dates)) = month
	    then hd dates :: tl_ans
	    else tl_ans
	end;
	     