
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
	     
(* Function 5 
   ListofDates * ListofMonths -> ListofDates
   Returns a list holding the dates that are in list of months *)

fun dates_in_months ((dates: (int*int*int) list), (months: int list)) =
    if null months then []
    else
	let val prev_true = dates_in_month (dates, (hd months))
	in
	   if null prev_true
	   then dates_in_months(dates, (tl months))
	   else prev_true @ dates_in_months(dates, (tl months))
	end;

(* Function 6
   ListofStrings * Int -> String
   Returns the string at int position in the list *)

fun get_nth ((strings: string list), n:int) =
    if length strings < n then ""
    else
	if n = 1
	then (hd strings)
	else get_nth ((tl strings, (n - 1)));

(*
Function 7 
Date -> String
Produces a string of form January 20, 2013

-TIPS
use ^ (shift 6) for concatenating strings 
use library function Int.toString to convert int to string
for month, use a list holding 12 strings
put a comma following th day and capitalized English Month Names *)

fun date_to_string (date: (int*int*int)) =
    let val months = ["January", "February", "March", "April",
			  "May", "June", "July", "August", "September",
			  "October", "November", "December"]		     
    in
	get_nth(months, #2 date) ^ " " ^
	Int.toString(#3 date) ^ ", " ^
	Int.toString (#1 date)
    end;
		      

(* Function 8
int * ListofInt -> int
sum * int list  -> n
Assume : sum is positive, int list is all positive, entire list sums to more than sum

Produces an int n such that 
	 the first n elements of the list add to less than sum
         the first n+1 elements add to sum or more 
*)

(* 
examples 

number_before_reaching_sum (10, [1,2,3,4,5]) = 3
number_before_reaching_sum (2, [1,2,3,4]) = 1
*)


fun number_before_reaching_sum (sum:int, numlist: int list) =
    if hd numlist > sum then 0
    else
	let fun find_sum (count: int, total:int, numlist: int list) =
		if total < sum
		then find_sum (count + 1, total + hd numlist, tl numlist)
		else count - 1
	in
	    find_sum (0, 0, numlist)
	end;


exception EmptyList
fun number_before_reaching_sum_2 (sum: int, numlist: int list) =
    if null numlist then raise EmptyList
    else
	if hd numlist >= sum
	then 0
	else
	    1 + (number_before_reaching_sum_2 (sum - hd numlist, tl numlist));
       
       
(* Function 9 

Int -> Int
Produces the month that the day is from
e.g. Day 30  = January 30  - Month 1
     Day 250 = September 7 - Month 9

 *)

fun what_month (day:int) =
    let val month_list = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1 + number_before_reaching_sum (day, month_list)
    end;
	
	
(* Function 10 

int * int -> int list
day1 day2 -> [m1, m2, ... mn]
Produces a list where m1 is the month of day1, m2 is the month of day1 + 1
up until day2 (mn is month of day2)

 *)

fun month_range (day1:int, day2:int) =
    if day1 < day2
    then what_month(day1) :: month_range (day1 + 1, day2)
    else what_month(day2) :: [];

(* Function 11

ListofDates -> (int*int*int) OPTION
Produces NONE if the list has no dates and SOME d if d is the oldest

 *)


fun oldest (dates0: (int*int*int) list) =
    if null dates0
    then NONE
    else
	let fun current (dates: (int*int*int) list) =
		if null (tl dates)
		then hd dates
		else
		    let val ans = current (tl dates)
		    in
			if   is_older (hd dates, ans)
			then hd dates
			else ans
		    end
	in
	    SOME (current dates0)
	end;
				 
   
