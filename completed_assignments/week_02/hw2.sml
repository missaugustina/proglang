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
fun all_except_option (search_string : string, string_list : string list) =
    let
        val list_length = length string_list
        fun aux (acc, search_string, string_list) =
            case string_list of
                [] =>
                if length acc = list_length
                then NONE
                else SOME acc
              | s::string_list =>
                if same_string (search_string, s)
                then aux (acc, search_string, string_list)
                else aux (s::acc, search_string, string_list)
    in
        aux([], search_string, string_list)
    end

(* val get_substitutions1 = fn : string list list * string -> string list *)
(* make a list of strings from the lists that have the search string
without returning the search string itself *)
fun get_substitutions1 ( string_lists : string list list, search_string : string ) =
    (* check if the string is in the list *)
    (* if yes, then add the list minus the string to the acc list *)
    (* if no, then return the current acc *)
    let fun aux( acc, string_lists, search_string) =
            case string_lists of
                [] => acc
             |  sl::string_lists =>
                let val foo = all_except_option(search_string, sl)
                in
                    case foo of
                        SOME list => aux (
                                        acc @ valOf foo,
                                        string_lists,
                                        search_string
                                    )
                     |  NONE => aux(acc, string_lists, search_string)
                end
    in
        aux ([], string_lists, search_string)
    end

(* get_substitutions2 = fn : string list list * string -> string list *)
fun get_substitutions2 ( string_lists : string list list, search_string : string ) = get_substitutions1( string_lists, search_string )

 (* Write a function similar_names, which takes a string list list of substitutions and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list) *)
(* similar_names =
fn : string list list * {first:string, last:string, middle:string}-> {first:string, last:string, middle:string} list *)

fun get_full_name ( first_name_list : string list, rest_of_name : {middle : string, last : string} ) =
    let fun aux (acc, first_name_list, rest_of_name) =
    case first_name_list of
        [] => acc
      | f::first_name_list =>
        let val {middle=y,last=z} = rest_of_name
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
            get_full_name ( first_name_list, {middle=y, last=z})
        end
    end
(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(*


card_color = fn : card -> color
card_value = fn : card -> int
remove_card = fn : card list * card * exn -> card list
all_same_color = fn : card list -> bool
sum_cards = fn : card list -> int
score = fn : card list * int -> int
officiate = fn : card list * move list * int -> int
*)
 
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

