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
let rec insertion_sort (l:card list) : card list =
  match l with
  | [] -> []
  | h::t -> insert h (insertion_sort t)
  and insert elem lst =
  match lst with
  | [] -> [elem]
  | h1::t1 -> if (val_of_card elem) > (val_of_card h1) then elem::h1::t1
              else h1::insert elem t1

(* insertion sort for sorting by a cards SUIT useful for sorting lists of cards
* Suit order is Spades > Clubs > Diamonds > Hearts *)
let rec insertion_sort_suit (l:card list) : card list =
  match l with
  | [] -> []
  | h::t -> insert2 h (insertion_sort_suit t)
  and insert2 elem lst =
  match lst with
  | [] -> [elem]
  | h1::t1 -> if (suit_of_card elem) > (suit_of_card h1) then elem::h1::t1
            else h1::insert2 elem t1

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
* and the number of times it occurs as a suit * int pair
* if more than one suit occurs the same amount of times, the
* return does not matter, since this function is only used for
* finding flushes, and since we are looking for flushes in a
* maximum of 7 cards, if more than one suit does occur the same
* amount of times, this means there cannot possibly be a flush *)
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

(*remove duplicates from a list*)
let rec remove_dups (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> h::(remove_dups (List.filter (fun x -> x <> h) t))

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
  if List.mem (card_of_string ("A "^suit_to_string s)) c &&
  List.mem (card_of_string ("5 "^suit_to_string s)) c &&
  List.mem (card_of_string ("4 "^suit_to_string s)) c &&
  List.mem (card_of_string ("3 "^suit_to_string s)) c &&
  List.mem (card_of_string ("2 "^suit_to_string s)) c
  then true else
  let rec loop clist =
    match (insertion_sort (remove_dups clist)) with
    | h1::h2::h3::h4::h5::t -> if one_step_below h2 h1 && one_step_below h3 h2 &&
                               one_step_below h4 h3 && one_step_below h5 h4 &&
                               same_suit h1 h2 && same_suit h2 h3 &&
                               same_suit h3 h4 && same_suit h4 h5
                               then true else loop (h2::h3::h4::h5::t)
    | _ -> false
  in loop c


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
  if List.mem (value_of_string "A") (List.map val_of_card c) &&
  List.mem (value_of_string "5") (List.map val_of_card c) &&
  List.mem (value_of_string "4") (List.map val_of_card c) &&
  List.mem (value_of_string "3") (List.map val_of_card c) &&
  List.mem (value_of_string "2") (List.map val_of_card c)
  then true else
  let rec loop clist =
    match (insertion_sort (remove_dups clist)) with
    | h1::h2::h3::h4::h5::t -> if one_step_below h2 h1 && one_step_below h3 h2 &&
                               one_step_below h4 h3 && one_step_below h5 h4
                               then true else loop (h2::h3::h4::h5::t)
    | _ -> false
  in loop c

(*check if a card list contains three of a kind
* if the triple is part of four of a kind it will return false*)
let rec triple_check (c:card list) : bool =
  let rec loop clist acc =
    match (insertion_sort clist) with
    | h1::h2::h3::t -> if val_of_card h1 = val_of_card h2 &&
                       val_of_card h2 = val_of_card h3 &&
                       not (List.mem (val_of_card h1) (List.map val_of_card t)) &&
                       not (List.mem (val_of_card h1) (List.map val_of_card acc))
                       then true
                       else loop (h2::h3::t) (acc@[h1])
    | _ -> false
  in loop c []

(*returns the first pair in the card list - this is fine
* because determine best hand will have already checked for everything else
* if the pair is part of three or four of a kind it wil return false*)
let pair_check (c:card list) : bool =
  let rec loop clist acc =
    match (insertion_sort clist) with
    | h1::h2::t -> if val_of_card h1 = val_of_card h2 &&
                   not (List.mem (val_of_card h1) (List.map val_of_card t)) &&
                   not (List.mem (val_of_card h1) (List.map val_of_card acc))
                   then true
                   else loop (h2::t) (acc@[h1])
    | _ -> false
  in loop c []

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
                   else loop (h2::t) acc
  in loop c 0

(*helpers for compare_hands
*invariant: all card lists have 5 cards upon initial function call?*)

(*HighCard comparison - return whichever hand (card list) is better*)
let compare_high_card (h1:card list) (h2:card list) : card list =
  let rec loop hand1 hand2 =
    match (hand1,hand2) with
    | ([],[]) -> let r = Random.int 2 in
                 if r = 0 then h1 else h2
    | (x::xs,[]) -> h1 (*code should never reach here - same for other compares*)
    | ([],x::xs) -> h2 (*code should never reach here - same for other compares*)
    | (x1::xs1,x2::xs2) -> if (val_of_card x1) > (val_of_card x2) then h1
                           else if (val_of_card x1) < (val_of_card x2) then h2
                           else loop xs1 xs2
                         in loop (insertion_sort h1) (insertion_sort h2)

(* returns the card value of the cards that make up the triple
* and None if the card list does not contain a triple*)
let rec value_of_triple (h:card list) : value option =
  match h with
  | h1::h2::h3::t -> if (same_value h1 h2) && (same_value h2 h3)
                     then Some (val_of_card h1)
                     else value_of_triple (h2::h3::t)
  | _ -> None

(* returns the card value of the pair in the hand that contains a pair
* card list h is guaranteed to contain at least a pair *)
let value_of_pair (h:card list) : value =
  let vt = value_of_triple h in
  let rec loop hnd =
  match hnd with
  | h1::h2::t -> if same_value h1 h2 && (vt = None ||
                 not(Some (val_of_card h1) = vt))
                 then val_of_card h1 else loop (h2::t)
  | _ -> failwith "violated precondition"
in loop h

(*Pair comparison - returns the hand that is better*)
let compare_pair (h1:card list) (h2:card list) : card list =
  let v1 = value_of_pair (insertion_sort h1) in
    let v2 = value_of_pair (insertion_sort h2) in
      if v1 > v2 then h1
      else if v1 < v2 then h2
      else let rec loop hnd1 hnd2 =
        match (insertion_sort hnd1, insertion_sort hnd2) with
        | ([],[]) -> let r = Random.int 2 in
                     if r = 0 then h1 else h2
        | (x::xs,[]) -> h1
        | ([],x::xs) -> h2
        | (x1::xs1,x2::xs2) -> if v1 = val_of_card x1 then loop xs1 xs2
                          else if (val_of_card x1) < (val_of_card x2) then h2
                          else h1
                        in loop h1 h2

(* TwoPair comparison - returns the hand that is better*)
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
           match (insertion_sort hnd1, insertion_sort hnd2) with
           | ([],[]) -> let r = Random.int 2 in
                        if r = 0 then h1 else h2
           | (x::xs,[]) -> h1
           | ([],x::xs) -> h2
           | (x1::xs1,x2::xs2) ->
              if (val_of_card x1 = val_of_card x2) then loop xs1 xs2
              else if compare_high_card [x1] [x2] = [x1] then h1 else h2
            in loop h1 h2

(* Triple comparison - returns the better hand *)
let compare_triple (h1:card list) (h2:card list) : card list =
  let v1 = value_of_triple (insertion_sort h1) in
    let v2 = value_of_triple (insertion_sort h2) in
      if v1 > v2 then h1
      else if v1 < v2 then h2
      else let rec loop hnd1 hnd2 =
        match (insertion_sort hnd1, insertion_sort hnd2) with
        | ([],[]) -> let r = Random.int 2 in
                     if r = 0 then h1 else h2
        | (x::xs,[]) -> h1
        | ([],x::xs) -> h2
        | (x1::xs1,x2::xs2) -> if v1 = Some (val_of_card x1) then loop xs1 xs2
                          else if (val_of_card x1) < (val_of_card x2) then h2
                          else h1
                        in loop h1 h2

(* Straight comparison - returns the better hand
* Does not work *)
let rec compare_straight (h1:card list) (h2:card list) : card list =
  match (insertion_sort h1,insertion_sort h2) with
  | ([],[]) -> let r = Random.int 2 in
               if r = 0 then h1 else h2
  (* because a straight with an A will be AKQJT or A5432 *)
  | (h11::h12::h13::t1,h21::h22::h23::t2) ->
    let highCard1 =
    if one_step_below h12 h11 && one_step_below h13 h12 then h11
    else if one_step_below h13 h12 then h12 else h13 in
    let highCard2 =
    if one_step_below h22 h21 && one_step_below h23 h22 then h21
    else if one_step_below h23 h22 then h22 else h23 in
    if compare_high_card [highCard1] [highCard2] = [highCard1] then h1 else h2
  | _ -> failwith "incorrect input to function"

(* Flush comparison - returns the better hand *)
let compare_flush (h1:card list) (h2:card list) : card list =
  let s1 = most_common_suit h1 in
  let s2 = most_common_suit h2 in
  let newh1 = (insertion_sort h1) in
  let newh2 = (insertion_sort h2) in
  let rec loop hnd1 hnd2 =
    match (hnd1,hnd2) with
    | ([],[]) -> let r = Random.int 2 in
                 if r = 0 then h1 else h2
    | (x::xs,[]) -> h1
    | ([],x::xs) -> h2
    | (x1::xs1,x2::xs2) -> if val_of_card x1 <> val_of_card x2 &&
                           suit_of_card x1 = fst s1 && suit_of_card x2 = fst s2
                           then (if compare_high_card [x1] [x2] = [x1] then h1
                           else h2)
                           else loop xs1 xs2
                         in loop newh1 newh2

(* FullHouse comparison *)
let rec compare_full_house (h1:card list) (h2:card list) : card list =
  let tripleValue1 = value_of_triple h1 in
  let tripleValue2 = value_of_triple h2 in
  if tripleValue1 > tripleValue2 then h1
  else if tripleValue1 < tripleValue2 then h2
  else let rec loop hnd1 hnd2 =
  match (insertion_sort hnd1,insertion_sort hnd2) with
  | ([],[]) -> let r = Random.int 2 in
               if r = 0 then h1 else h2
  | (x::xs,[]) -> h1
  | ([],x::xs) -> h2
  | (x1::xs1,x2::xs2) -> if Some (val_of_card x1) = tripleValue1
                         then loop xs1 xs2
                         else
                         (if val_of_card x1 > val_of_card x2 then h1
                         else if val_of_card x1 < val_of_card x2 then h2
                         else h1)
  in loop h1 h2

(* Quads comparison - returns the better hand*)
let rec compare_quads (h1:card list) (h2:card list) : card list =
  match (insertion_sort h1,insertion_sort h2) with
  | ([],[]) -> let r = Random.int 2 in
               if r = 0 then h1 else h2
  | (x::xs,[]) -> h1
  | ([],x::xs) -> h2
  | (fst1::snd1::t1,fst2::snd2::t2) ->
    if val_of_card fst1 = val_of_card snd1 &&
    val_of_card fst2 = val_of_card snd2
    then (if val_of_card fst1 = val_of_card fst2 then compare_high_card h1 h2
    else if val_of_card fst1 > val_of_card fst2 then h1 else h2)
    else compare_quads (snd1::t1) (snd2::t2)
  | _ -> failwith "incorrect input to function"

(* StraightFlush comparison *)
(*same as comparing a regular straight - which is not working yet*)
let compare_straight_flush (h1:card list) (h2:card list) : card list =
  compare_straight h1 h2

(* RoyalFlush comparison *)
(* The only time both players can have a royal flush is when the
* 5 board cards make a royal flush so this is automatically a tie *)
let compare_royal_flush (h1:card list) (h2:card list) : card list =
  let r = Random.int 2 in if r = 0 then h1 else h2

(*above here works - except for exceptions*)

(* A function for converting cards to a player's BEST POSSIBLE hand.
* It will presumably be used extensively in the game to determine the winner
* of the hand.  Gamestate will call this function on each player's hands
* and then call compare on the two generated hands. Since this determines
* the best possible hand, we need all 7 available cards,
* the 2 from a player's hand and the 5 from the board.
* It could be fewer than seven cards for AI but not fewer than 5*)
let determine_best_hand (c:card list) : hand =
  if royal_flush_check c then (*checked*)
    let s = fst(most_common_suit c) in
    let rec loop c2 =
      match (insertion_sort_suit (insertion_sort c2)) with
      | h1::h2::h3::h4::h5::t -> if suit_of_card h1 = s
                                 then RoyalFlush ([h1;h2;h3;h4;h5])
                                 else loop (h2::h3::h4::h5::t)
      | _ -> failwith "violated precondition"
    in loop c
  else if straight_flush_check c then
    let s = fst(most_common_suit c) in
    let rec loop c2 =
      match (insertion_sort_suit (insertion_sort c2)) with
      | h1::h2::h3::h4::h5::t -> if suit_of_card h1 = s
                                 then StraightFlush ([h1;h2;h3;h4;h5])
                                 else loop (h2::h3::h4::h5::t)
      | _ -> failwith "violated precondition"
    in loop c
  else if fst (quads_check c) then (* checked *)
    let v = snd (quads_check c) in
    let rec loop c2 =
      match (insertion_sort c2) with
      | h1::h2::h3::h4::h5::t -> if Some(val_of_card h1) = v
                                 || Some(val_of_card h2) = v
                                 then Quads [h1;h2;h3;h4;h5]
                                 else loop (h3::h4::h5::t)
      | _ -> failwith "violated precondition"
    in loop c
  else if full_house_check c then (*checked*)
    let vt = value_of_triple (insertion_sort c) in
    let vp = value_of_pair (insertion_sort c) in
    let rec loop c2 acc =
      match (insertion_sort c2) with
      | [] -> FullHouse (acc)
      | h::t -> if Some (val_of_card h) = vt || val_of_card h = vp
                then loop t (acc @ [h])
                else loop t acc
    in loop c []
  else if flush_check c then (*checked*)
    let s = fst(most_common_suit c) in
    let rec loop c2 count=
      match (insertion_sort_suit c2) with
      | h1::h2::h3::h4::h5::t ->
        if suit_of_card h1 = s && t = []
        then Flush ([h1;h2;h3;h4;h5])
        else if suit_of_card h1 = s && t <> [] && count = 0
        then loop (insertion_sort (h1::h2::h3::h4::h5::t)) (count + 1)
        else loop (h2::h3::h4::h5::t) count
      | _ -> failwith "violated precondition"
    in loop c 0
  else if straight_check c then (*checked*)
    let s = fst(most_common_suit c) in
    let rec loop c2 =
      match (insertion_sort c2) with
      | h1::h2::h3::h4::h5::t -> if suit_of_card h1 = s
                                 then Straight ([h1;h2;h3;h4;h5])
                                 else loop (h2::h3::h4::h5::t)
      | _ -> failwith "violated precondition"
    in loop c
  else if triple_check c then (*checked*)
    let vt = value_of_triple (insertion_sort c) in
    let rec loop c2 acc1 acc2 =
      match (insertion_sort c2) with
      | [] -> let highcards = insertion_sort acc2 in
              Triple (acc1 @ [(List.nth highcards 0);(List.nth highcards 1)])
      | h::t -> if Some(val_of_card h) = vt
                then loop t (acc1 @ [h]) acc2
                else loop t acc1 (acc2 @ [h])
    in loop c [] []
  else if two_pair_check c then (*checked*)
    let vp = value_of_pair (insertion_sort c) in
    let rec loop c2 acc1 acc2 =
      match (insertion_sort c2) with
      | [] -> let vp2 = value_of_pair (insertion_sort acc2) in
              let rec loop2 c3 acc3 acc4 =
                match c3 with
                | [] -> let highcard = List.nth (insertion_sort acc4) 0 in
                        TwoPair (acc1 @ acc3 @ [highcard])
                | h::t -> if val_of_card h = vp2
                          then loop2 t (acc3@[h]) acc4
                          else loop2 t acc3 (acc4 @ [h])
              in loop2 acc2 [] []
      | h::t -> if val_of_card h = vp
                then loop t (acc1 @ [h]) acc2
                else loop t acc1 (acc2 @ [h])
    in loop c [] []
  else if pair_check c then (*checked*)
    let vp = value_of_pair (insertion_sort c) in
    let rec loop c2 acc1 acc2 =
      match (insertion_sort c2) with
      | [] -> let highcards = insertion_sort acc2 in
              Pair (acc1@[(List.nth highcards 0);(List.nth highcards 1);
                          (List.nth highcards 2)])
      | h::t -> if val_of_card h = vp
                then loop t (acc1 @ [h]) acc2
                else loop t acc1 (acc2 @ [h])
    in loop c [] []
  else
    match insertion_sort c with
    | h1::h2::h3::h4::h5::t -> HighCard ([h1;h2;h3;h4;h5]) (*checked*)
    | _ -> failwith "violated precondition"

(* working except for straight and straight flush *)
let compare_hands (h1:hand) (h2:hand) : hand =
  match (h1,h2) with
  | (HighCard (_),HighCard (_)) -> determine_best_hand (compare_high_card
                    (hand_to_card_list h1) (hand_to_card_list h2))
  | (Pair (_),Pair (_)) -> determine_best_hand (compare_pair
                (hand_to_card_list h1) (hand_to_card_list h2))
  | (TwoPair (_),TwoPair (_)) -> determine_best_hand (compare_two_pair
                   (hand_to_card_list h1) (hand_to_card_list h2))
  | (Triple (_),Triple (_)) -> determine_best_hand (compare_triple
                  (hand_to_card_list h1) (hand_to_card_list h2))
  | (Straight (_),Straight (_)) -> determine_best_hand (compare_straight
                    (hand_to_card_list h1) (hand_to_card_list h2))
  | (Flush (_),Flush (_)) -> determine_best_hand (compare_flush
                 (hand_to_card_list h1) (hand_to_card_list h2))
  | (FullHouse (_),FullHouse (_)) -> determine_best_hand (compare_full_house
                     (hand_to_card_list h1) (hand_to_card_list h2))
  | (Quads (_),Quads (_)) -> determine_best_hand (compare_quads
                 (hand_to_card_list h1) (hand_to_card_list h2))
  | (StraightFlush (_),StraightFlush (_)) -> determine_best_hand (compare_straight_flush
                         (hand_to_card_list h1) (hand_to_card_list h2))
  | (RoyalFlush (_),RoyalFlush (_)) -> determine_best_hand (compare_royal_flush
                      (hand_to_card_list h1) (hand_to_card_list h2))
  | _ -> if h1 > h2 then h1 else h2

(*helper that returns the string representation of a card list
* given the hand type as a string argument *)
let hand_to_string_helper (clist:card list) (s:string) : string =
  let s1 = s^ " of " in
  let rec loop cards acc =
    match (insertion_sort cards) with
    | [] -> acc
    | h::t -> if acc = "" then loop t (acc^card_to_string h)
              else loop t (acc^" and "^card_to_string h)
  in s1^(loop clist "")

let hand_to_string (h:hand) : string =
match h with
  | HighCard clist -> hand_to_string_helper clist "HighCard"
  | Pair clist -> hand_to_string_helper clist "Pair"
  | TwoPair clist -> hand_to_string_helper clist "TwoPair"
  | Triple clist -> hand_to_string_helper clist "Triple"
  | Straight clist -> hand_to_string_helper clist "Straight"
  | Flush clist -> hand_to_string_helper clist "Flush"
  | FullHouse clist -> hand_to_string_helper clist "Full House"
  | Quads clist -> hand_to_string_helper clist "Quads"
  | StraightFlush clist -> hand_to_string_helper clist "StraightFlush"
  | RoyalFlush clist -> hand_to_string_helper clist "RoyalFlush"