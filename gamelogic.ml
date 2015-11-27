(*open necessary files*)
open Card

(*types*)

type hand =
| HighCard of card list
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
* useful for sorting lists of cards *)
let rec insertion_sort (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> insert h (insertion_sort t)
  and insert elem lst =
  match lst with
  | [] -> [elem]
  | h1::t1 -> if (val_of_card elem) < (val_of_card h1) then elem::h1::t1
                else h1::insert elem t1

(* converts a hand to a card list *)
let hand_to_card_list (h:hand) : card list =
    match h with
    | HighCard clist -> clist
    | Pair clist -> clist
    | TwoPair clist -> clist
    | Triple clist -> clist
    | Straight clist -> clist
    | Flush clist -> clist
    | FullHouse clist -> clist
    | Quads clist -> clist
    | StraightFlush clist -> clist
    | RoyalFlush clist -> clist

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
let rec compare_high_card (h1:card list) (h2:card list) : card list =
  match (insertion_sort h1, insertion_sort h2) with
  | ([],[]) -> failwith "same hands" (*both hands are the same?*)
  | (x::xs,[]) -> h1 (*code should never actually reach here*)
  | ([],x::xs) -> h2 (*code should never actually reach here*)
  | (x1::xs1,x2::xs2) -> if (val_of_card x1) > (val_of_card x2) then h1
                         else if (val_of_card x1) < (val_of_card x2) then h2
                         else compare_high_card xs1 xs2

(*Pair comparison*)

(* returns the card value of the pair in the hand that contains a pair *)
let rec value_of_pair (h:card list) : value =
  match h with
  | [] -> failwith "Hand does not contain a pair"
  | hd::[] -> failwith "Hand does not contain a pair"
  | h1::h2::t -> if same_value h1 h2 then val_of_card h1
                 else value_of_pair (h2::t)

let compare_pair (h1:card list) (h2:card list) : card list =
  let v1 = value_of_pair (insertion_sort h1) in
    let v2 = value_of_pair (insertion_sort h2) in
      if v1 > v2 then h1
      else if v1 < v2 then h2
      else let rec loop hnd1 hnd2 =
        match (insertion_sort hnd1, insertion_sort hnd2) with
        | ([],[]) -> failwith "same hands" (*both hands are the same?*)
        | (x::xs,[]) -> h1
        | ([],x::xs) -> h2
        | (x1::xs1,x2::xs2) -> if v1 = val_of_card x1 then loop xs1 xs2
                          else if (val_of_card x1) < (val_of_card x2) then hnd2
                          else hnd1
                        in loop h1 h2

(* TwoPair comparison *)

let compare_two_pair (h1:card list) (h2:card list) : card list =
  let largerPair1 = val_of_card (List.nth (insertion_sort h1) 1) in
    let largerPair2 = val_of_card (List.nth (insertion_sort h2) 1) in
    if largerPair1 > largerPair2 then h1
    else if largerPair1 < largerPair2 then h2
    else let smallerPair1 = val_of_card (List.nth (insertion_sort h1) 3) in
      let smallerPair2 = val_of_card (List.nth (insertion_sort h2) 3) in
      if smallerPair1 > smallerPair2 then h1
      else if smallerPair1 < smallerPair2 then h2
      else let rec loop hnd1 hnd2 =
           match (insertion_sort hnd1, insertion_sort h2) with
           | ([],[]) -> failwith "same hands" (*both hands are the same?*)
           | (x::xs,[]) -> h1
           | ([],x::xs) -> h2
           | (x1::xs1,x2::xs2) ->
              if (val_of_card x1 = val_of_card x2) then loop xs1 xs2
              else compare_high_card [x1] [x2]
            in loop h1 h2

(* Triple comparison *)


(* Function for comapring hands, will call a helper function for
* breaking ties between same rankings of hands.
* Ex: TwoPair AA338 > AA228
* Full House 99944 > 99444
* Returns the hand that is better. What to do if the hands are the same?
* Will need to conver result back to hand with determine_best_hand *)
let compare_hands (h1:hand) (h2:hand) : hand =
  if h1 > h2 then h1
  else if h2 > h1 then h2
  else match h1 with
       | HighCard (_) -> determine_best_hand (compare_high_card
        (hand_to_card_list h1) (hand_to_card_list h2))
       | Pair (_) -> determine_best_hand (compare_pair
       (hand_to_card_list h1) (hand_to_card_list h2))
       | TwoPair (_) -> determine_best_hand (compare_two_pair
       (hand_to_card_list h1) (hand_to_card_list h2))
       | Triple (_) -> failwith "Implement"
       | Straight (_) -> failwith "Implement"
       | Flush (_) -> failwith "Implement"
       | FullHouse (_) -> failwith "Implement"
       | Quads (_) -> failwith "Implement"
       | StraightFlush (_) -> failwith "Implement"
       | RoyalFlush (_) -> failwith "Implement"