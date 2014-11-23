(*
Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines.
*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* val all_except_option = fn : string * string list -> string list option *)
(* if the string is in the list, return SOME lst without string in it, else return NONE *)
fun all_except_option (search_string : string, string_list : string list ) =
    case string_list of
        [] => NONE
           | s::string_list =>
             if same_string( search_string, s )
             then SOME string_list
             else
                 case all_except_option(search_string, string_list) of
                     SOME x => SOME(s::x)
                   | NONE => NONE

(* val get_substitutions1 = fn : string list list * string -> string list *)
(* make a list of strings from the lists that have the search string
without returning the search string itself *)

fun get_substitutions1 ( string_lists : string list list, search_string : string ) =
    case string_lists of
        [] => []
           | sl::string_lists =>
             let
                 val result_list = get_substitutions1(string_lists, search_string)
             in
                  case all_except_option(search_string, sl) of
                     NONE => result_list
                   | SOME l => l @ result_list
             end

(* get_substitutions2 = fn : string list list * string -> string list *)
fun get_substitutions2 ( string_lists : string list list, search_string : string ) =
    let
        fun loop (acc : string list, string_lists) =
            case string_lists of
                [] => acc
              | sl::string_lists =>
                case all_except_option(search_string, sl) of
                    NONE => loop( acc, string_lists)
                  | SOME l => loop(l @ acc, string_lists)
    in
        loop ([], string_lists)
    end

 (* Write a function similar_names, which takes a string list list of substitutions and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list) *)
(* similar_names =
fn : string list list * {first:string, last:string, middle:string}-> {first:string, last:string, middle:string} list *)

fun get_full_name ( first_name_list : string list, rest_of_name : {middle : string, last : string} ) =
    let
        fun aux (acc, first_name_list, rest_of_name) =
    case first_name_list of
        [] => acc
      | f::first_name_list =>
        let
            val {middle=y,last=z} = rest_of_name
        in
            aux(({first=f, middle=y, last=z})::acc, first_name_list, rest_of_name)
        end
    in
        aux([], first_name_list, rest_of_name)
    end

fun similar_names ( name_lists : string list list, full_name : {first:string, last:string, middle:string} ) =
    let val {first=x,middle=y,last=z} = full_name
    in
        let
            val first_name_list = get_substitutions1(name_lists, x)
        in
            get_full_name (first_name_list, {middle=y, last=z})
        end
    end
(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
(* card_color = fn : card -> color *)
fun card_color (c : card) =
    case c of
         (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black
      | (Clubs, _) => Black

(* card_value = fn : card -> int *)
fun card_value (c : card) =
    case c of
        (_, Num x) => x
      | (_, Ace) => 11
      | _ => 10

(*
remove_card = fn : card list * card * exn -> card list
all_same_color = fn : card list -> bool
sum_cards = fn : card list -> int
score = fn : card list * int -> int
officiate = fn : card list * move list * int -> int
*)
