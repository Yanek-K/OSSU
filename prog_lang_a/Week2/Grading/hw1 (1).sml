(* Problem 1: check if the second data is older than first *)
fun is_older(d1 : int * int * int, d2 : int * int * int) = 
    if (#1 d1) < (#1 d2)
    then true
    else if (#1 d1) > (#1 d2)
    then false
    else
        if (#2 d1) < (#2 d2)
        then true
        else if (#2 d1) > (#2 d2)
        then false
        else 
            if (#3 d1) < (#3 d2)
            then true 
            else false


(* Problem 2 *)
fun number_in_month(dates : (int * int * int) list, month : int) = 
    if null dates
    then 0
    else if #2 (hd dates) = month 
    then number_in_month(tl dates, month) + 1
    else number_in_month(tl dates, month)


(* Problem 3 *)
fun number_in_months(dates : (int * int * int) list, months : int list) = 
    if null months 
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

 
 (* Problem 4 *)
fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then dates 
    else if #2 (hd dates) = month 
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)


(* Problem 5 *)
fun dates_in_months(dates : (int * int * int) list, months : (int) list) = 
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months) 


(* Problem 6 *)
fun get_nth(strings : (string) list, n : int) =  
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)


(* Problem 7 *)
fun date_to_string(date : (int * int * int)) = 
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


(* Problem 8 *)
fun number_before_reaching_sum(sum : int, pos_nums : (int) list) =
    if hd pos_nums < sum 
    then 1 + number_before_reaching_sum(sum - hd pos_nums, tl pos_nums)
    else 0


(* Problem 9 *)
fun what_month(day : int) = 
    let 
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        number_before_reaching_sum(day, months) + 1
    end 


(* Problem 10 *)
fun month_range(day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)


(* Problem 11 *)
fun oldest(dates : (int * int * int) list) = 
    if null dates 
    then NONE
    else 
        let 
            val old = oldest(tl dates)
        in 
            if isSome old andalso is_older(valOf old, hd dates)
            then old
            else SOME(hd dates)
        end 
