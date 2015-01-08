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
(* Use List.filter, Char.isUpper, and String.sub, 2 lines *)
val only_capitals = fn xs =>
                       let
                           val is_capital = fn x => Char.isUpper(String.sub(x, 0))
                       in
                           List.filter is_capital xs
                       end;
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
      xs;

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
      xs;

val longest_string3 = fn xs => longest_string_helper (fn (x,y) => x > y) xs;
val longest_string4 = fn xs => longest_string_helper (fn (x,y) => x >= y) xs;

(* 5 *)
(* longest_capitalized = fn : string list -> string *)
val longest_capitalized =
    longest_string3 o only_capitals;
(* 6 *)
(* rev_string = fn : string -> string *)
val rev_string = String.implode o List.rev o String.explode;

(* 7 *)
(* first_answer = fn : (’a -> ’b option) -> ’a list -> ’b *)
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

(* next section *)
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
(* g = fn : (unit -> int) -> (string -> int) -> pattern -> int *)
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
fun count_wildcards p =
    g (fn _ => 1) (fn _ => 0) p
;

(* 9b *)
(*  count_wild_and_variable_lengths = fn : pattern -> int *)
fun count_wild_and_variable_lengths p =
    g (fn _ => 1) (fn (x) => size x) p
;

(* count_some_var = fn : string * pattern -> int *)
fun count_some_var (s, p) =
    g (fn _ => 0) (fn (x) => if String.isSubstring s x then 1 else 0) p
(*
val test9c = count_some_var ("x", Variable("x"))
that takes a string and a pattern (as a pair) and
returns the number of times the string appears as a variable in the pattern.
We care only about variable names; the constructor names are not relevant.
*)
    (*
check_pat = fn : pattern -> bool
match = fn : valu * pattern -> (string * valu) list option
first_match = fn : valu -> pattern list -> (string * valu) list option
*)
