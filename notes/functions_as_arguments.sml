(* examples from proglang *)

(* we are writing these out by hand to fully appreciate first class functions later *)

fun increment_n_times_lame (n, x) = (* silly way of incrmenting n + x *)
    if n=0
    then x
    else 1 + increment_n_times_lame(n-1, x)

fun double_n_times_lame (n, x) =
    if n=0
    then x
    else 2 * double_n_times_lame(n-1, x)

fun nth_tail_lame (n, xs) =
    if n=0
    then xs
    else tl (nth_tail_lame(n-1, xs))

fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times(f, n-1, x))

fun increment x = x + 1
fun double x = x + x

val x1 = n_times( double, 4, 7);
val x2 = n_times( increment, 4, 7);
val x3 = n_times( tl, 2, [4,8,12,16]);

fun addition (n,x) = n_times(increment, n, x);

(* i don't fully understand this *)
fun times_until_zero (f, x) =
    if x = 0 then 0 else 1 + times_until_zero(f, f x)

fun len xs =
    case xs of
        [] => 0
     | _::xs => 1 + len(xs)

(*
fun triple x = 3 * x;
fun triple_n_times (n, x) = n_times(triple, n, x);
*)

fun triple_n_times (n, x) =
    n_times( (fn x => 3 * x), n, x)

(* conputes a new list by applying the function to the items in the given list *)
fun map (f, xs) =
    case xs of
        [] => []
     | x::xs => (f x)::map(f, xs)

fun is_even x = x mod 2 = 0
fun all_even xs =
    (fn x => is_even x, xs)

datatype exp = Constant of int
       | Negate of exp
       | Add of exp * exp
       | Multiply of exp * exp

(* given an exp, is every constant in it an even number? *)
(* is every constant in it less than 10? *)
fun true_of_all_constants (f, e) =
    case e of
        Constant i => f i
     | Negate e1 => true_of_all_constants(f, e1)
     | Add(e1, e2) => true_of_all_constants(f, e1)
                      andalso true_of_all_constants(f, e2)
     | Multiply(e1, e2)  => true_of_all_constants(f, e1)
                      andalso true_of_all_constants(f, e2)

fun filter (f, xs) =
    case xs of
        [] => []
     | x::xs =>
       if f x
       then x::filter(f, xs)
       else filter(f,xs)

fun greaterThanX x = fn y => y > x

fun noNegatives xs = filter(greaterThanX ~1, xs)
(* why return a function? *)

fun allGreater (xs, n) = filter(fn x => x > n, xs)

fun allShorterThan1 (xs, s) =
    filter(fn x => String.size x < (print "!"; String.size s), xs)
(* recompute size of s for every element of xs *)

fun allShorterThan2 (xs, s) =
    let
        val i = (print "!"; String.size s) (* how to fix this *)
    in
        filter(fn x => String.size x < i, xs)
    end
