(*1*)
(* year, month, day *)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    (#1 date1) < (#1 date2) (* check the years*)
    orelse  (* check the months *)
      (#1 date1) = (#1 date2)
      andalso (#2 date1) < (#2 date2)
    orelse  (* check the days *)
      (#1 date1) = (#1 date2)
      andalso (#2 date1) = (#2 date2)
      andalso (#3 date1) < (#3 date2)
(*2*)
(* determine count of dates that occur in the given month *)
fun number_in_month ( dates : (int * int * int) list, month : int ) =
    if null dates
    then 0
    else (* why does this return count when tl dates should error? *)
        let val count = number_in_month(tl dates, month)
        in
            if ( #2 (hd dates) = month )
            then 1 + count
            else count
        end
(*3*)
(* determine number of dates in any of the months *)
fun number_in_months ( dates : (int * int * int) list, months : int list ) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
(*4*)
(* return a list of dates that occur in the current month *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
        let val dates_list = dates_in_month(tl dates, month)
        in
            if ( #2 (hd dates) = month )
            then  (hd dates)::(dates_list)
            else dates_list
        end
(*5*)
(* return a list of dates that occur in any of the months *)
fun dates_in_months ( dates : (int * int * int) list, months : int list ) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
(*6*)
fun get_nth (words : string list, n : int) =
    if n = 1
    then hd words
    else get_nth( tl words, n-1)

(*7*)
fun date_to_string ( date : (int * int * int) ) =
    get_nth(
      ["January","February","March","April",
      "May","June","July","August","September",
      "October","November","December"],
      #2 date
    ) ^
    " " ^ Int.toString(#3 date) ^
    ", " ^ Int.toString(#1 date)
(*8*)
fun number_before_reaching_sum (sum : int, nums : int list) =
  if sum <= hd nums
  then 0
  else 1 + number_before_reaching_sum( sum - hd nums, tl nums )

(*9*)
fun what_month (day:int) =
   let val days_in_years = [31, 28, 31, 30, 31, 30, 31, 31, 31, 31, 30, 31]
   in 1 + number_before_reaching_sum(day, days_in_years)
   end

(*10*)
(* return month for day1, month for day1+1..n, month for day2 *)
fun month_range (day1:int, day2:int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

(*11*)
fun oldest (dates:(int*int*int) list) =
    if null dates
    then NONE
    else let
        fun oldest_nonempty (dates:(int*int*int) list) =
            if null (tl dates)
            then hd dates
            else let val tl_ans = oldest_nonempty(tl dates)
                 in
                     if is_older(hd dates, tl_ans)
                     then hd dates
                     else tl_ans
                 end
    in SOME (oldest_nonempty dates)
    end
