use "hw1.sml";
(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1  = is_older ((3,2,3),(4,3,4)) = true
val test11 = is_older ((3,2,1),(3,3,1)) = true
val test12 = is_older ((3,2,1),(3,2,2)) = true
val test13 = is_older ((3,2,1),(2,3,2)) = false
val test14 = is_older ((3,2,1),(3,1,2)) = false
val test15 = is_older ((3,2,2),(3,2,1)) = false

	
val test2  = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test21 = number_in_month ([(2012,2,23),(2013,2,23),(2014,3,23),(2015,2,23)],2) = 3

											 
val test3  = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test31 = number_in_months ([], [2,3,4]) = 0						val test32 = number_in_months ([(2012,2,28),(2013,12,1)],[]) = 0
val test33 = number_in_months ([(2012,1,28)], [1]) = 1
val test34 = number_in_months ([(2012,2,28)], [1]) = 0

											
val test4  = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test41 = dates_in_month ([(2012,1,23),(2012,2,23),(2013,1,23)], 1) = [(2012,1,23),(2013,1,23)]
	
val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test60 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test61 = get_nth ([],1) = ""
val test62 = get_nth (["hi","there"], 3) = ""
val test63 = get_nth (["hi", "there", "how", "are", "you"], 4) = "are"
								    
val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test80 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test81 = number_before_reaching_sum (2, [1,2,3,4]) = 1
val test82 = number_before_reaching_sum (5, [2,1,0,1,4,5]) = 4

val test800 = number_before_reaching_sum_2 (10, [1,2,3,4,5]) = 3
val test801 = number_before_reaching_sum_2 (2, [1,2,3,4]) = 1
val test802 = number_before_reaching_sum_2 (5, [2,1,0,1,4,5]) = 4
							     							       
val test90 = what_month 70 = 3
val test91 = what_month 250 = 9
val test92 = what_month 30 = 1
				 

val test100 = month_range (31, 34) = [1,2,2,2]
val test101 = month_range (58, 61) = [2,2,3,3]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

