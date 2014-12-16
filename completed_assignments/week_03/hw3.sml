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

(* 1 *)
(* only_capitals = fn : string list -> string list *)
(* returns a string list that has only the strings in the argument that start with an uppercase letter. *)
(* Use List.filter, Char.isUpper, and String.sub, 2 lines *)
(* g = fn : (unit -> int) -> (string -> int) -> pattern -> int *)
val g = fn x => Char.isUpper(String.sub(x, 0))
val only_capitals = fn xs => List.filter g xs

(* 2 *)
(* longest_string1 = fn : string list -> string *)
(* returns the longest string in the list.
If the list is empty, return ""
tie, return the string closest to the beginning of the list
Use foldl, String.size, and no recursion *)

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
longest_string_helper = fn : (int * int -> bool) -> string list -> string
longest_string3 = fn : string list -> string
longest_string4 = fn : string list -> string
longest_capitalized = fn : string list -> string
rev_string = fn : string -> string
first_answer = fn : (’a -> ’b option) -> ’a list -> ’b
all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option
count_wildcards = fn : pattern -> int
count_wild_and_variable_lengths = fn : pattern -> int
count_some_var = fn : string * pattern -> int
check_pat = fn : pattern -> bool
match = fn : valu * pattern -> (string * valu) list option
first_match = fn : valu -> pattern list -> (string * valu) list option
*)
