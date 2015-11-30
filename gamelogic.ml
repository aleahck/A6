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

(* insertion sort for sorting by a cards value
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

(* insertion sort for sorting by a cards SUIT useful for sorting lists of cards
* Suit order is Spades > Clubs > Diamonds > Hearts *
* TEST THIS *)
let rec insertion_sort_suit (l:'a list) : 'a list =
match l with
| [] -> []
| h::t -> insert h (insertion_sort t)
and insert elem lst =
match lst with
| [] -> [elem]
| h1::t1 -> if (suit_of_card elem) < (suit_of_card h1) then elem::h1::t1
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

(* returns the suit that occurs most frequently in the card list
* and the number of times it occurs as a pair
* if more than one suit occurs the same amount of times, one
* is chosen randomly *)
let most_common_suit (c:card list) : suit * int =
let rec loop clist h d cl s =
  match clist with
  | [] -> let final = max h (max d (max cl s)) in
          if final = h then (suit_of_string "H",final)
          else if final = d then (suit_of_string "D",final)
          else if final = cl then (suit_of_string "C",final)
          else (suit_of_string "S",final)
  | hd::t -> if suit_of_card hd = suit_of_string "H"
             then loop t (h+1) d cl s
             else if suit_of_card hd = suit_of_string "D"
             then loop t h (d+1) cl s
             else if suit_of_card hd = suit_of_string "C"
             then loop t h d (cl+1) s
             else loop t h d cl (s+1)
          in loop c 0 0 0 0

(*check if a card list contains a royal flush*)
let royal_flush_check (c:card list) : bool =
let s = fst (most_common_suit c) in
let rec loop clist acc =
  match (insertion_sort clist) with
  | [] -> if acc >= 5 then true else false
  | hd::t -> if suit_of_card hd = s &&
             val_of_card hd > value_of_string "9"
             then loop t (acc + 1)
             else loop t acc
in loop c 0

(*check if a card list contains a straight flush*)
let straight_flush_check (c:card list) : bool =
let s = fst (most_common_suit c) in
let rec loop clist acc =
  match (insertion_sort clist) with
  | h1::h2::t -> if suit_of_card h1 = s && suit_of_card h2 = s
                    && one_step_below h2 h1 || (suit_of_card h1 = s &&
                    val_of_card h1 = value_of_string "A" &&
                    let five = 
                      card_of_string ("5 "^suit_to_string (suit_of_card h1)) in
                    List.mem five clist)
                 then loop (h2::t) (acc + 1)
                 else loop (h2::t) acc
   | _ -> acc >= 5
  in loop c 0

(*check if a card list contains four of a kind
* Returns a bool * value option pair, with value being the
* value of the cards that make up the four of a kind*)
let rec quads_check (c:card list) : bool * value option =
  match (insertion_sort c) with
  | h1::h2::h3::h4::t -> if val_of_card h1 = val_of_card h2 &&
                         val_of_card h2 = val_of_card h3 &&
                         val_of_card h3 = val_of_card h4
                         then (true,Some (val_of_card h1))
                         else quads_check (h2::h3::h4::t)
  | _ -> (false,None)

(*check if a card list contains a flush*)
let flush_check (c:card list) : bool =
  let s = fst (most_common_suit c) in
  let rec loop clist acc =
    match (insertion_sort clist) with
    | [] -> if acc >= 5 then true else false
    | hd::t -> if suit_of_card hd = s
               then loop t (acc + 1)
               else loop t acc
  in loop c 0

(*check if a card list contains a straight *)
let straight_check (c:card list) : bool =
  let rec loop clist acc =
    match (insertion_sort clist) with
    | h1::h2::t -> if one_step_below h2 h1 ||
                      (val_of_card h1 = value_of_string "A" &&
                       List.mem (value_of_string "5") 
                                (List.map val_of_card clist))
                   then loop (h2::t) (acc + 1)
                   else loop (h2::t) acc
    | _ -> acc >= 5
  in loop c 0

(*check if a card list contains three of a kind*)
let rec triple_check (c:card list) : bool =
  match (insertion_sort c) with
  | h1::h2::h3::t -> if val_of_card h1 = val_of_card h2 &&
                     val_of_card h2 = val_of_card h3 &&
                     not (List.mem (val_of_card h1) (List.map val_of_card c))
                     then true
                     else triple_check (h2::h3::t)
  | _ -> false

(*returns the first pair in the card list - this is fine
* because determine best hand will have already checked for everything else*)
let rec pair_check (c:card list) : bool =
  match (insertion_sort c) with
  | h1::h2::t -> if val_of_card h1 = val_of_card h2 &&
                 not (List.mem (val_of_card h1) (List.map val_of_card c))
                 then true
                 else pair_check (h2::t)
  | _ -> false

(*check if a card list contains a full house*)
let rec full_house_check (c:card list) : bool =
  triple_check c && pair_check c

(*check if a card list contains two pair - could be part of other hands,
*but determine_best_hand will have checked for this already *)
let two_pair_check (c:card list) : bool =
  let rec loop clist acc =
    match (insertion_sort clist) with
    | [] -> if acc > 1 then true else false
    | h::[] -> if acc > 1 then true else false
    | h1::h2::t -> if pair_check [h1;h2] then loop t (acc+1)
                   else pair_check (h2::t)
  in loop c 0


(*helpers for compare_hands
*invariant: all card lists have 5 cards upon initial function call?*)

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
let rec value_of_triple (h:card list) : value =
  match h with
  | [] -> failwith "Hand does not contain a triple"
  | hd::[] -> failwith "Hand does not contain a triple"
  | h1::h2::[] -> failwith "Hand does not contain a triple"
  | h1::h2::h3::t -> if (same_value h1 h2) && (same_value h2 h3)
                     then val_of_card h1
                     else value_of_triple (h2::h3::t)

let compare_triple (h1:card list) (h2:card list) : card list =
  let v1 = value_of_triple (insertion_sort h1) in
    let v2 = value_of_triple (insertion_sort h2) in
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

(* Straight comparison *)

let rec compare_straight (h1:card list) (h2:card list) : card list =
  match (insertion_sort h1,insertion_sort h2) with
  | ([],[]) -> failwith "same hands"
  (* because a straight with an A will be AKQJT or A5432 *)
  | (x1::xs1,x2::xs2) -> if val_of_card x1 = val_of_card x2
                         then compare_straight xs1 xs2
                         else compare_high_card [x1] [x2]
  | _ -> failwith "not possible"

(* Flush comparison *)

let rec compare_flush (h1:card list) (h2:card list) : card list =
  match (insertion_sort h1,insertion_sort h2) with
  | ([],[]) -> failwith "same hands"
  | (x1::xs1,x2::xs2) -> if val_of_card x1 = val_of_card x2
                         then compare_flush xs1 xs2
                         else compare_high_card [x1] [x2]
  | _ -> failwith "not possible"

(* FullHouse comparison *)

let rec compare_full_house (h1:card list) (h2:card list) : card list =
  let tripleValue1 = value_of_triple h1 in
  let tripleValue2 = value_of_triple h2 in
  if tripleValue1 > tripleValue2 then h1
  else if tripleValue1 < tripleValue2 then h2
  else let rec loop hnd1 hnd2 =
  match (h1,h2) with
  | ([],[]) -> failwith "same hands"
  | (x1::xs1,x2::xs2) -> if val_of_card x1 = tripleValue1
                         then loop xs1 xs2
                         else
                         (if val_of_card x1 > val_of_card x2 then h1
                         else if val_of_card x1 < val_of_card x2 then h2
                         else h1) (*same hands*)
  | _ -> failwith "not possible"
  in loop h1 h2

(* Quads comparison *)

let rec compare_quads (h1:card list) (h2:card list) : card list =
  match (insertion_sort h1,insertion_sort h2) with
  | ([],[]) -> failwith "same hands"
  (* because a straight with an A will be AKQJT or A5432 *)
  | (fst1::snd1::t1,fst2::snd2::t2) ->
    if val_of_card fst1 = val_of_card snd1 &&
    val_of_card fst2 = val_of_card snd2
    then (if val_of_card fst1 = val_of_card fst2 then compare_high_card h1 h2
    else if val_of_card fst1 > val_of_card fst2 then h1
    else h2)
    else compare_quads (snd1::t1) (snd2::t2)
  | _ -> failwith "not possible"

(* StraightFlush comparison *)

(*same as comparing a regular straight*)
let rec compare_straight_flush (h1:card list) (h2:card list) : card list =
  compare_straight h1 h2

(* RoyalFlush comparison *)

(* The only time both players can have a royal flush is when the
* 5 board cards make a royal flush so this will split the pot *)
let rec compare_royal_flush (h1:card list) (h2:card list) : card list =
  failwith "Same hands"
(* A function for converting cards to a player's BEST POSSIBLE hand.
* It will presumably be used extensively in the game to determine the winner
* of the hand.  Gamestate will call this function on each player's hands
* and then call compare on the two generated hands. Since this determines
* the best possible hand, we need all 7 available cards,
* the 2 from a player's hand and the 5 from the board.
* It could be fewer than seven cards for AI *)
let determine_best_hand (c:card list) : hand =
  if royal_flush_check c then
    let s = fst(most_common_suit c) in
    let rec loop c2 =
      match (insertion_sort_suit (insertion_sort c2)) with
      | h1::h2::h3::h4::h5::t -> if suit_of_card h1 = s
                                 then RoyalFlush ([h1;h2;h3;h4;h5])
                                 else loop (h2::h3::h4::h5::t)
      | _ -> failwith "?"
    in loop c
  else if straight_flush_check c then
    let s = fst(most_common_suit c) in
    let rec loop c2 =
      match (insertion_sort_suit (insertion_sort c2)) with
      | h1::h2::h3::h4::h5::t -> if suit_of_card h1 = s
                                 then StraightFlush ([h1;h2;h3;h4;h5])
                                 else loop (h2::h3::h4::h5::t)
      | _ -> failwith "?"
    in loop c
  else if fst (quads_check c) then
    let v = snd (quads_check c) in
    let rec loop c2 =
      match (insertion_sort c2) with
      | h1::h2::h3::h4::h5::t -> if Some(val_of_card h1) = v
                                    || Some(val_of_card h2) = v
                                 then Quads [h1;h2;h3;h4;h5]
                                 else loop (h3::h4::h5::t)
      | _ -> failwith "?"
    in loop c
  else if full_house_check c then
    let vt = value_of_triple (insertion_sort c) in
    let vp = value_of_pair (insertion_sort c) in
    let rec loop c2 acc =
      match (insertion_sort c2) with
      | [] -> FullHouse (acc)
      | h::t -> if val_of_card h = vt || val_of_card h = vp
                then loop t (acc @ [h])
                else loop t acc
    in loop c []
  else if flush_check c then
    let s = fst(most_common_suit c) in
    let rec loop c2 =
      match (insertion_sort_suit (insertion_sort c2)) with
      | h1::h2::h3::h4::h5::t -> if suit_of_card h1 = s
                                 then Flush ([h1;h2;h3;h4;h5])
                                 else loop (h2::h3::h4::h5::t)
      | _ -> failwith "?"
    in loop c
  else if straight_check c then
    let s = fst(most_common_suit c) in
    let rec loop c2 =
      match (insertion_sort c2) with
      | h1::h2::h3::h4::h5::t -> if suit_of_card h1 = s
                                 then StraightFlush ([h1;h2;h3;h4;h5])
                                 else loop (h2::h3::h4::h5::t)
      | _ -> failwith "?"
    in loop c
  else if triple_check c then
    let vt = value_of_triple (insertion_sort c) in
    let rec loop c2 acc1 acc2 =
      match (insertion_sort c2) with
      | [] -> let highcards = insertion_sort acc2 in
              Triple (acc1 @ [(List.nth highcards 0);(List.nth highcards 1)])
      | h::t -> if val_of_card h = vt
                then loop t (acc1 @ [h]) acc2
                else loop t acc1 (acc2 @ [h])
    in loop c [] []
  else if two_pair_check c then
    let vp = value_of_pair (insertion_sort c) in
    let rec loop c2 acc1 acc2 =
      match (insertion_sort c2) with
      | [] -> let vp2 = value_of_pair (insertion_sort acc2) in
              let rec loop2 c3 acc3 acc4 =
                match acc1 with
                | [] -> let highcard = List.nth (insertion_sort acc4) 0 in
                        TwoPair (c3 @ acc3 @ [highcard])
                | h::t -> if val_of_card h = vp2
                          then loop2 t (acc3@[h]) acc4
                          else loop2 t acc3 (acc4 @ [h])
              in loop2 acc2 [] []
      | h::t -> if val_of_card h = vp
                then loop t (acc1 @ [h]) acc2
                else loop t acc1 (acc2 @ [h])
    in loop c [] []
  else if pair_check c then
    let vp = value_of_pair (insertion_sort c) in
    let rec loop c2 acc1 acc2 =
      match (insertion_sort c2) with
      | [] -> let highcards = insertion_sort acc2 in
              Pair (acc1@[(List.nth highcards 0);(List.nth highcards 1);
                          (List.nth highcards 2)]) (* is this acc1 or acc2 *)
      | h::t -> if val_of_card h = vp
                then loop t (acc1 @ [h]) acc2
                else loop t acc1 (acc2 @ [h])
    in loop c [] []
  else
    match insertion_sort c with
    | h1::h2::h3::h4::h5::t -> HighCard ([h1;h2;h3;h4;h5])
    | _ -> failwith "?"

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
  | Triple (_) -> determine_best_hand (compare_triple
                  (hand_to_card_list h1) (hand_to_card_list h2))
  | Straight (_) -> determine_best_hand (compare_straight
                    (hand_to_card_list h1) (hand_to_card_list h2))
  | Flush (_) -> determine_best_hand (compare_flush
                 (hand_to_card_list h1) (hand_to_card_list h2))
  | FullHouse (_) -> determine_best_hand (compare_full_house
                     (hand_to_card_list h1) (hand_to_card_list h2))
  | Quads (_) -> determine_best_hand (compare_quads
                 (hand_to_card_list h1) (hand_to_card_list h2))
  | StraightFlush (_) -> determine_best_hand (compare_straight_flush
                         (hand_to_card_list h1) (hand_to_card_list h2))
  | RoyalFlush (_) -> determine_best_hand (compare_royal_flush
                      (hand_to_card_list h1) (hand_to_card_list h2))
