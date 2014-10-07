fun sum_list (xs : int list) =
    if null xs
    then 0
    else hd xs + sum_list(tl xs)

fun list_product (xs : int list) =
    if null xs
    then 1
    else hd xs * list_product(tl xs)

fun countdown (x : int) =
    if x=0
    then []
    else x :: countdown (x - 1) (*create a list*)

fun append (xs : int list, ys : int list) =
    if null xs (* if the first list is empty, just return the second list  *)
    then ys
    else if null ys
    then xs
    (* start with first element of the first list, then cons onto another list  *)
    else (hd xs) :: append ((t1 xs), ys)
    (* append first element to rest of xs and then ys  *)

fun sum_pair_list (xs : (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list(t1 xs)

fun firsts (xs : (int * int) list) = (* [3,5] *)
    if null xs
    then []
    else (#1 (hd xs)) :: firsts(tl xs)

