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


(* remove_card = fn : card list * card * exn -> card list *)
(*
Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. You can compare cards with =.
*)

fun remove_card (cards : card list, card_to_remove : card, e : exn) =
    case cards of
        [] => raise e (* if the card list is empty, return an exception *)
      | c::cards =>
       if card_to_remove = c (* check if the top card matches our card to remove *)
       then cards (* if it matches remove it and return the rest of the list *)
       else (* if it doesn't match, *)
           case remove_card(cards, card_to_remove, e) of (* check the rest of the list until we find a match *)
               x => c::x (* append the one that didn't match back to the new list *)

(* all_same_color = fn : card list -> bool *)
(* return true if all cards are the same color *)
fun all_same_color (cards: card list) =
    case cards of
        [] => true (* if the list is empty then technically all the colors are the same *)
     | c1::c2::cards =>
       if card_color(c1) = card_color(c2) (* check if the colors are the same *)
       then all_same_color(cards)
       (* we have a true condition but need to keep checking the list *)
       else false (* we can stop checking and return false *)
     | c::cards => true (* only 1 card in the list *)

(* sum_cards = fn : card list -> int *)
(* take a list of cards and return a sum of their values *)
fun sum_cards (cards : card list) =
    let
        fun loop(acc : int, cards) =
            case cards of
                [] => acc
              | c::cards => loop(card_value(c) + acc, cards)
    in
        loop(0, cards)
    end

(* score = fn : card list * int -> int *)
(* If sum is greater than goal, the preliminary score is three times (sum − goal),
else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
with integer division; use ML’s div operator).
*)
fun score (cards : card list, goal : int) =
    let
        val score =
            let
                val sum = sum_cards(cards)
            in
                if ( sum > goal )
                then 3 * ( sum - goal)
                else goal - sum
            end
    in
        case all_same_color(cards) of
            true => score div 2
         | false => score
    end

(*
Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
with a larger held-cards and a smaller card-list.
*)

(* officiate = fn : card list * move list * int -> int *)
fun officiate (cards : card list, moves : move list, goal : int) =
    let
        fun loop(held_cards, cards, moves, goal) =
            case moves of
                [] => score(held_cards, goal) (* game end *)
             |  m::moves =>
                case m of
                    Discard c => loop(remove_card(held_cards, c, IllegalMove), cards, moves, goal)
                  | Draw => (* as is the player can only draw until they have more points than the goal *)
                    case cards of
                        [] => score(held_cards, goal) (* game end *)
                      | c::cards  =>
                        let val sum = sum_cards(c::held_cards) (* cards in hand *)
                        in
                            if sum > goal (* we do this logic in score already...*)
                            then score(c::held_cards, goal)
                            else loop(c::held_cards, cards, moves, goal)
                        end
    in
        loop([], cards, moves, goal)
    end
