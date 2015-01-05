(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

fun g f1 f2 p =
    let
    val r = g f1 f2
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)
(* g = fn : (unit -> int) -> (string -> int) -> pattern -> int *)

(* 1 *)
(* only_capitals = fn : string list -> string list *)
(* Use List.filter, Char.isUpper, and String.sub, 2 lines *)
val only_capitals = fn xs =>
                       let
                           val is_capital = fn x => Char.isUpper(String.sub(x, 0))
                       in
                           List.filter is_capital xs
                       end
(* 2 *)
(* longest_string1 = fn : string list -> string *)

val longest_string1 = fn xs =>
  List.foldl
      (fn (x,y) => if String.size x > String.size y then x else y)
      ""
      xs

(* 3 *)
(* longest_string2 = fn : string list -> string *)
val longest_string2 = fn xs =>
  List.foldl
      (fn (x,y) => if String.size x >= String.size y then x else y)
      ""
      xs

(*
longest_string3 = fn : string list -> string
longest_string4 = fn : string list -> string
*)
(* 4 *)
(* longest_string_helper = fn : (int * int -> bool) -> string list -> string *)
val longest_string_helper = fn f => fn xs =>
  List.foldl
      (fn (x,y) => if f (String.size x, String.size y) then x else y)
      ""
      xs

val longest_string3 = fn xs => longest_string_helper (fn (x,y) => x > y) xs
val longest_string4 = fn xs => longest_string_helper (fn (x,y) => x >= y) xs

(* 5 *)
(* longest_capitalized = fn : string list -> string *)
val longest_capitalized =
    longest_string3 o only_capitals
(* 6 *)
(* rev_string = fn : string -> string *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
(* first_answer = fn : (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f xs =
  let
      fun loop xs =
          case xs of
              [] => NONE
            | x::xs => if f x then SOME x else loop xs
  in
      case loop xs of
          NONE => raise NoAnswer
        | x => x
  end

(* 8 *)
(* all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option *)
(*
 Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
Hints: The sample solution is 8 lines. It uses a helper function with an accumulator and uses @. Note
all_answers f [] should evaluate to SOME [].
*)

      (*
count_wildcards = fn : pattern -> int
count_wild_and_variable_lengths = fn : pattern -> int
count_some_var = fn : string * pattern -> int
check_pat = fn : pattern -> bool
match = fn : valu * pattern -> (string * valu) list option
first_match = fn : valu -> pattern list -> (string * valu) list option
*)
