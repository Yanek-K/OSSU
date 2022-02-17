(* homework 1 *)

fun is_older(d1 : int*int*int, d2 : int*int*int) =
    if #1 d1 < #1 d2
    then true
    else if #1 d1 > #1 d2
    then false
    else
  if #2 d1 < #2 d2
  then true
  else if #2 d1 > #2 d2
  then false
  else
      if #3 d1 < #3 d2
      then true
      else false

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
  let
      val currentMonth = #2 (hd dates)
      val count = if currentMonth = month then 1 else 0
  in
      count + number_in_month(tl dates, month)
  end

fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null dates
    then 0
    else
  if null months
  then 0
  else
      number_in_month(dates, hd months) + number_in_months(dates, tl months)
      
fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
  let
      val currentDate = hd dates
  in
      if #2 currentDate = month
      then
    currentDate::dates_in_month(tl dates, month)
      else
    dates_in_month(tl dates, month)
  end

fun valid_date_for_months(date : (int*int*int), months : int list) =
    if null months
    then false
    else
  if hd months = #2 date
  then true
  else
      valid_date_for_months(date, tl months)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null dates
    then []
    else
  if null months
  then []
  else
      if valid_date_for_months(hd dates, months)
      then
    (hd dates)::dates_in_months(tl dates, months)
      else
    dates_in_months(tl dates, months)

fun get_nth(strings : string list, n : int) =
    if null strings
    then ""
    else
  if n > 1
  then
      get_nth(tl strings, n - 1)
  else
      hd strings

fun date_to_string(date : int*int*int) =
    let
  val monthList = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
  get_nth(monthList, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int, ints : int list) =
    if null ints
    then 0
    else
  if hd ints < sum
  then 1 + number_before_reaching_sum(sum - (hd ints), tl ints)
  else 0

fun what_month(dayOfYear : int) =
    let
  val monthDayCountList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
  number_before_reaching_sum(dayOfYear, monthDayCountList) + 1
    end

fun count(from : int, to : int) =
    if from > to
    then []
    else
  from::count(from + 1, to)

fun mapping(dayOfYearList : int list, function : int -> int) =
    if null dayOfYearList
    then []
    else
  function(hd dayOfYearList)::mapping(tl dayOfYearList, function)

fun month_range(dayOfYear1 : int, dayOfYear2 : int) =
    let
  val dayOfYearList = count(dayOfYear1, dayOfYear2)
    in
  mapping(dayOfYearList, what_month)
    end

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else
  let
      val current = hd dates
      val remain = oldest(tl dates)
  in
      if isSome remain = false
      then SOME current
      else
    if is_older(valOf remain, current)
    then remain
    else SOME current
  end

(* 12. Challenge Problem *)
fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
    if null dates
    then 0
    else
  if null months
  then 0
  else
      if valid_date_for_months(hd dates, months)
      then
    1 + number_in_months_challenge(tl dates, months)
      else
    number_in_months_challenge(tl dates, months)

                
fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
    if null dates
    then []
    else
  if null months
  then []
  else
      if valid_date_for_months(hd dates, months)
      then
    (hd dates)::dates_in_months_challenge(tl dates, months)
      else
    dates_in_months_challenge(tl dates, months)

(* 13. Challenge Problem *)
fun get_nth_int(ints : int list, n : int) =
    if null ints
    then 0
    else
  if n > 1
  then
      get_nth_int(tl ints, n - 1)
  else
      hd ints				 
           
fun reasonable_date(date : int*int*int) =
    let
  val year = #1 date
  val month = #2 date
  val day = #3 date
  val monthDayCountList = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
  if year <= 0
  then false
  else
      if month < 1 orelse month > 12
      then false
      else
    if day < 1 orelse day > get_nth_int(monthDayCountList, month)
    then false
    else
        if month = 2 andalso day = 29 andalso ((year mod 400) = 0 orelse ((year mod 4 = 0) andalso (year mod 100 <> 0)))
        then true
        else false
    end

