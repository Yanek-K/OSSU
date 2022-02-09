
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
