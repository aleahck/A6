(*open necessary files*)
open Card

(*types*)

type hand =
HighCard of card list
| Pair of card list
| TwoPair of card list
| Triple of card list
| Straight of card list
| Flush of card list
| FullHouse of card list
| Quads of card list
| StraightFlush of card list
| RoyalFlush of card list

(*general helper functions*)

(* insertion sort
* useful for sorting fiveTuples and lists of cards *)
let rec insertion_sort (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> insert h (insertion_sort t)
  and insert elem lst =
  match lst with
  | [] -> [elem]
  | h1::t1 -> if (val_of_card elem) < h1 then elem::h1::t1
                else h1::insert elem t1

(* A function for converting cards to a player's BEST POSSIBLE hand.
* It will presumably be used extensively in the game to determine the winner
* of the hand.  Gamestate will call this function on each player's hands
* and then call compare on the two generated hands. Since this determines
* the best possible hand, we need all 7 available cards,
* the 2 from a player's hand and the 5 from the board.
* It could be fewer than seven cards for AI *)
let determine_best_hand (c:card list) : hand = failwith "TODO"


(* Converts a hand to its rank relative to other hands.
* This is only enough to compare hands of the same hand type,
* comparisons within hands is done in some helper function
* i.e. HighCard -> 0, Pair -> 1, etc.*)
let hand_to_hand_rank (h:hand) : int = failwith "TODO"

(*helpers for compare_hands*)

(*HighCard comparison*)
let rec compare_high_card (h1:hand) (h2:hand) : hand =
  match (insertion_sort h1, insertion_sort h2) with
  | ([],[]) -> (*both hands are the same?*)
  | (x::xs,[]) -> h1
  | ([],x::xs) -> h2
  | (x1::xs1,x2::xs2) -> if (val_of_card x1) > (val_of_card x2) then h1
                         else if (val_of_card x1) < (val_of_card x2) then h2
                         else compare_high_card xs1 xs2

(*Pair comparison*)
let rec compare_pair (h1:hand) (h2:hand) : hand =
  match (insertion_sort h1, insertion_sort h2) with
  | ([],[]) -> (*both hands are the same?*)
  | (x::xs,[]) -> h1
  | ([],x::xs) -> h2
  | (fst1::snd1::rest1,fst2::snd2::rest2) ->
    begin match ((val_of_card fst1 = val_of_card snd1),
           (val_of_card fst2 = val_of_card snd2)) with
    | (false,false) -> compare_pair (snd1@rest1) (snd2@rest2) (*does this work*)
    | (false,true) ->

(* Function for comapring hands, will call a helper function for
* breaking ties between same rankings of hands.
* Ex: TwoPair AA338 > AA228
* Full House 99944 > 99444
* Returns the hand that is better. What to do if the hands are the same? *)
let compare_hands (h1:hand) (h2:hand) : hand =
  if h1 > h2 then h1
  else if h2 > h1 then h2
  else match h1 with
       | HighCard -> compare_high_card h1 h2
       (*helper functions*)