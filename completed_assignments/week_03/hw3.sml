(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)


(* 1 *)
(* only_capitals = fn : string list -> string list *)
(* Find strings that start with a capital letter *)
val only_capitals = fn xs =>
                       let
                           val is_capital = fn x => Char.isUpper(String.sub(x, 0))
                       in
                           List.filter is_capital xs
                       end;
(* 2 *)
(* longest_string1 = fn : string list -> string *)
(* find the longest string in the list, for a tie, return the one closes to the beginning of the list *)

val longest_string1 = fn xs =>
  List.foldl
      (fn (x,y) => if String.size x > String.size y then x else y)
      ""
      xs

(* 3 *)
(* longest_string2 = fn : string list -> string *)
(* find the longest string in the list, for a tie, return the one closes to the end of the list *)
val longest_string2 = fn xs =>
  List.foldl
      (fn (x,y) => if String.size x >= String.size y then x else y)
      ""
      xs;

(*
longest_string3 = fn : string list -> string
longest_string4 = fn : string list -> string

Do the same as the other methods but use currying
*)
(* 4 *)
(* longest_string_helper = fn : (int * int -> bool) -> string list -> string *)
val longest_string_helper = fn f => fn xs =>
  List.foldl
      (fn (x,y) => if f (String.size x, String.size y) then x else y)
      ""
      xs;

val longest_string3 = fn xs => longest_string_helper (fn (x,y) => x > y) xs;
val longest_string4 = fn xs => longest_string_helper (fn (x,y) => x >= y) xs;

(* 5 *)
(* longest_capitalized = fn : string list -> string *)
(* Find the longest capitalized string *)
val longest_capitalized =
    longest_string3 o only_capitals;
 
(* 6 *)
(* rev_string = fn : string -> string *)
(* Reverse a string *)
val rev_string = String.implode o List.rev o String.explode;

(* 7 *)
(* first_answer = fn : (’a -> ’b option) -> ’a list -> ’b *)
(* return the first item in a list that meets the criteria of the applied function *)
fun first_answer f xs =
  let
      fun loop xs =
          case xs of
              [] => NONE
            | x::xs =>
              case f x of
                  NONE => loop xs
                | SOME x => SOME x
  in
      case loop xs of
          NONE => raise NoAnswer
        | x => x
  end;

(* 8 *)
(* return all answers in the list that meet the criteria of the applied function *)
(* all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option *)

fun all_answers f xs =
    let
        fun loop xs acc =
            case xs of
                [] => SOME acc
             | x::xs =>
               case f x of
                   NONE => NONE
                | SOME x => loop xs (x @ acc)
    in
        loop xs []
    end;

(* next section (provided code) *)
datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu;
(* g = fn : (unit -> int) -> (string -> int) -> pattern -> int (provided code) *)
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
    end;

(* 9a *)
(* count_wildcards = fn : pattern -> int *)
(* count the number of wildcards in a pattern *)
fun count_wildcards p =
    g (fn _ => 1) (fn _ => 0) p
;

(* 9b *)
(*  count_wild_and_variable_lengths = fn : pattern -> int *)
(* count the number of wildcards and sum the lengths of Variables in a pattern *)
fun count_wild_and_variable_lengths p =
    g (fn _ => 1) (fn (x) => size x) p
;

(* 9c *)
(* count_some_var = fn : string * pattern -> int *)
(* count the number of times a string appears as a variable name in a pattern *)
fun count_some_var (s, p) =
    g (fn _ => 0) (fn (x) => if String.isSubstring s x then 1 else 0) p
;

(* 10 *)
(* check_pat = fn : pattern -> bool *)
(* check that all variable names are different *)
 fun has_repeats xs =
    case xs of
        [] => false
      | x::xs => List.exists (fn y => x = y) xs orelse has_repeats xs
;

fun var_names p =
    case p of
        Variable x => [x]
     | ConstructorP (_,p) => var_names p
     | TupleP ps => List.foldl (fn (p,i) => var_names p @ i) [] ps
     | _ => []

fun check_pat p =
    has_repeats o var_names p
;

(*
Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.

Note that if the value matches but the pattern has no patterns of the form Variable s, then the result is SOME [].

Hints: Sample solution has one case expression with 7 branches. The branch for tuples uses all_answers and ListPair.zip.

Sample solution is 13 lines. Remember to look above for the rules for what patterns match what values, and what bindings they produce. These are hints: We are not requiring all_answers and ListPair.zip here, but they make it easier.
*)
(* 11 *)
(* match = fn : valu * pattern -> (string * valu) list option *)

(* 12 *)
(* first_match = fn : valu -> pattern list -> (string * valu) list option *)
