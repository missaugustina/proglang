(* 1 *)
(* is_older that takes two dates and evaluates to true or false.
It evaluates to true if the first argument is a date
that comes before the second argument.
(If the two dates are the same, the result is false.)
*)
(* year month day *)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    (* check years *)
    (#1 date1 < #1 date2)
    orelse
    (* check months *)
    (#1 date1 = #1 date2)
        andalso (#2 date1 < #2 date2)
    orelse (* check days *)
    (#1 date1 = #1 date2)
        andalso (#2 date1 = #2 date2)
        andalso (#3 date1 < #3 date2)

(*2*)
(* takes a list of dates and a month (i.e., an int)
and returns how many dates in the list are in the given month. *)
fun number_in_month ( dates : (int * int * int) list, month : int ) =
    if null dates
    then 0
    else
        let
            val count = number_in_month( tl dates, month )
        in
            if ( #2 (hd dates) = month )
            then 1 + count
            else count
        end
