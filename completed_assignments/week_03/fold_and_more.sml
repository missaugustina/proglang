(* fold notes *)

fun fold (f, acc, xs) =
    case xs of
        [] => acc
     | x::xs' => fold(f, f(acc,f), xs) (* fold left *)


