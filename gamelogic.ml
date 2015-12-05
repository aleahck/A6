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

(* insertion sort for sorting with any comparison function taking
 * a card to some type *)
let rec insertion_sort_abstract (l: card list) (f: card -> 'a) =
  let rec insert elem = function
    | [] -> [elem]
    | h::t -> ( if (f elem) > (f h) then elem::h::t
                else h::(insert elem t) )
  in
  match l with
  | [] -> []
  | h::t -> insert h (insertion_sort_abstract t f)

(* insertion sort for sorting by a cards value
* useful for sorting lists of cards *)
let insertion_sort (l:card list) : card list =
  insertion_sort_abstract l val_of_card

(* insertion sort for sorting by a cards SUIT useful for sorting lists of cards
* Suit order is Spades > Clubs > Diamonds > Hearts *)
let insertion_sort_suit (l:card list) : card list =
  insertion_sort_abstract l suit_of_card

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
  | [] -> ( let final = max h (max d (max cl s)) in
            if final = h then (suit_of_string "H",final)
            else if final = d then (suit_of_string "D",final)
            else if final = cl then (suit_of_string "C",final)
            else (suit_of_string "S",final) )
  | hd::t -> ( if suit_of_card hd = suit_of_string "H"
               then loop t (h+1) d cl s
               else if suit_of_card hd = suit_of_string "D"
               then loop t h (d+1) cl s
               else if suit_of_card hd = suit_of_string "C"
               then loop t h d (cl+1) s
               else loop t h d cl (s+1) )
  in loop c 0 0 0 0

(*remove duplicate values from a card list*)
let rec remove_dups (l:card list) : card list =
  match l with
  | [] -> []
  | h::t -> ( let f = fun x -> val_of_card x <> val_of_card h in
              h::(remove_dups (List.filter f t)) )

(*check if a card list contains a royal flush*)
let royal_flush_check (c:card list) : bool =
  let s,_ = most_common_suit c in
  let rec loop clist acc =
    match clist with
    | [] -> acc >= 5
    | h::t -> ( let is_royal_card = val_of_card h > value_of_string "9" in
                if suit_of_card h = s && is_royal_card then loop t (acc + 1)
                else loop t acc )
  in loop (insertion_sort c) 0

(*check if a card list contains a straight flush*)
let straight_flush_check (c:card list) : bool =
  let s,_ = most_common_suit c in
  if List.mem (card_of_string ("A "^suit_to_string s)) c
     && List.mem (card_of_string ("5 "^suit_to_string s)) c
     && List.mem (card_of_string ("4 "^suit_to_string s)) c
     && List.mem (card_of_string ("3 "^suit_to_string s)) c
     && List.mem (card_of_string ("2 "^suit_to_string s)) c
  then true
  else
    let rec loop clist =
      match clist with
      | h1::h2::h3::h4::h5::t -> ( if one_step_below h2 h1
                                      && one_step_below h3 h2
                                      && one_step_below h4 h3
                                      && one_step_below h5 h4
                                      && same_suit h1 h2 && same_suit h2 h3
                                      && same_suit h3 h4 && same_suit h4 h5
                                   then true
                                   else loop (h2::h3::h4::h5::t) )
      | _ -> false
    in loop (insertion_sort (remove_dups c))

(*check if a card list contains four of a kind
* Returns a bool * value option pair, with value being the
* value of the cards that make up the four of a kind*)
let rec quads_check (c:card list) : bool * value option =
  match (insertion_sort c) with
  | h1::h2::h3::h4::t -> ( if val_of_card h1 = val_of_card h2
                              && val_of_card h2 = val_of_card h3
                              && val_of_card h3 = val_of_card h4
                           then (true,Some (val_of_card h1))
                           else quads_check (h2::h3::h4::t) )
  | _ -> (false,None)

(*check if a card list contains a flush*)
let flush_check (c:card list) : bool =
  let s,_ = most_common_suit c in
  let rec loop clist acc =
    match clist with
    | [] -> acc >= 5
    | h::t -> if suit_of_card h = s then loop t (acc + 1) else loop t acc
  in loop (insertion_sort c) 0

(*return true if the straight is a A5432*)
let low_straight (c:card list) : bool =
  let vals = List.map val_of_card c in
  List.mem (value_of_string "A") vals &&
  List.mem (value_of_string "5") vals &&
  List.mem (value_of_string "4") vals &&
  List.mem (value_of_string "3") vals &&
  List.mem (value_of_string "2") vals &&
  not (List.mem (value_of_string "6") vals)

(*check if a card list contains a straight *)
let straight_check (c:card list) : bool =
  let vlist = List.map val_of_card c in
  if List.mem (value_of_string "A") vlist
     && List.mem (value_of_string "5") vlist
     && List.mem (value_of_string "4") vlist
     && List.mem (value_of_string "3") vlist
     && List.mem (value_of_string "2") vlist
  then true
  else
    let rec loop clist =
      match clist with
      | h1::h2::h3::h4::h5::t -> ( if one_step_below h2 h1
                                      && one_step_below h3 h2
                                      && one_step_below h4 h3
                                      && one_step_below h5 h4
                                   then true
                                   else loop (h2::h3::h4::h5::t) )
      | _ -> false
    in loop (insertion_sort (remove_dups c))

(*check if a card list contains three of a kind
* if the triple is part of four of a kind it will return false*)
let rec triple_check (c:card list) : bool =
  let rec loop clist acc =
    match clist with
    | h1::h2::h3::t -> ( let vacc = List.map val_of_card acc in
                         let vt = List.map val_of_card t in
                         let v1 = val_of_card h1 in
                         let v2 = val_of_card h2 in
                         let v3 = val_of_card h3 in
                         if v1 = v2 && v2 = v3
                            && not (List.mem v1 vt)
                            && not (List.mem v1 vacc)
                         then true
                         else loop (h2::h3::t) (acc@[h1]) )
    | _ -> false
  in loop (insertion_sort c) []

(*returns the first pair in the card list - this is fine
* because determine best hand will have already checked for everything else
* if the pair is part of three or four of a kind it wil return false*)
let pair_check (c:card list) : bool =
  let rec loop clist acc =
    match clist with
    | h1::h2::t -> ( let vacc = List.map val_of_card acc in
                     let vt = List.map val_of_card t in
                     let v1 = val_of_card h1 in
                     let v2 = val_of_card h2 in
                     if v1 = v2 && not (List.mem v1 vt)
                        && not (List.mem v1 vacc)
                     then true
                     else loop (h2::t) (acc@[h1]) )
    | _ -> false
  in loop (insertion_sort c) []

(*check if a card list contains a full house*)
let rec full_house_check (c:card list) : bool =
  triple_check c && pair_check c

(*check if a card list contains two pair - could be part of other hands,
*but determine_best_hand will have checked for this already *)
let two_pair_check (c:card list) : bool =
  let rec loop clist acc =
    match clist with
    | [] -> acc > 1
    | h::[] -> acc > 1
    | h1::h2::t -> ( if pair_check [h1;h2] then loop t (acc+1)
                     else loop (h2::t) acc )
  in loop (insertion_sort c) 0

(*helpers for compare_hands*)

(*HighCard comparison - return whichever hand (card list) is better*)
let compare_high_card (h1:card list) (h2:card list) : card list =
  let rec loop hand1 hand2 =
    match (hand1,hand2) with
    | ([],[]) -> ( let r = Random.int 2 in
                   if r = 0 then h1 else h2 )
    | (x::xs,[]) -> h1 (*should never reach here -same for other compares*)
    | ([],x::xs) -> h2 (*should never reach here - same for other compares*)
    | (x1::xs1,x2::xs2) -> ( let v1 = val_of_card x1 in
                             let v2 = val_of_card x2 in
                             if v1 > v2 then h1
                             else if v1 < v2 then h2
                             else loop xs1 xs2 )
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
*  precondition: h is guaranteed to contain at least a pair *)
let value_of_pair (h:card list) : value =
  let vt = value_of_triple h in
  let rec loop hnd =
    match hnd with
    | h1::h2::t -> ( if same_value h1 h2
                        && (vt = None || not(Some (val_of_card h1) = vt))
                     then val_of_card h1
                     else loop (h2::t) )
    | _ -> failwith "violated precondition"
  in loop h

(*Pair comparison - returns the hand that is better*)
let compare_pair (h1:card list) (h2:card list) : card list =
  let sorted_h1 = insertion_sort h1 in
  let sorted_h2 = insertion_sort h2 in
  let v1 = value_of_pair sorted_h1 in
  let v2 = value_of_pair sorted_h2 in
  if v1 > v2 then h1
  else if v1 < v2 then h2
  else
    let rec loop hnd1 hnd2 =
      match hnd1, hnd2 with
      | ([],[]) -> ( let r = Random.int 2 in
                     if r = 0 then h1 else h2 )
      | (x::xs,[]) -> h1
      | ([],x::xs) -> h2
      | (x1::xs1,x2::xs2) -> ( if v1 = val_of_card x1 then
                                 loop xs1 xs2
                               else if (val_of_card x1) < (val_of_card x2) then
                                 h2
                               else
                                 h1 )
    in loop sorted_h1 sorted_h2

(* TwoPair comparison - returns the hand that is better*)
let compare_two_pair (h1:card list) (h2:card list) : card list =
  let sorted_h1 = insertion_sort h1 in
  let sorted_h2 = insertion_sort h2 in
  let largerPair1 = val_of_card (List.nth (sorted_h1) 1) in
  let largerPair2 = val_of_card (List.nth (sorted_h2) 1) in
  if largerPair1 > largerPair2 then h1
  else if largerPair1 < largerPair2 then h2
  else
    let smallerPair1 = val_of_card (List.nth (sorted_h1) 3) in
    let smallerPair2 = val_of_card (List.nth (sorted_h2) 3) in
    if smallerPair1 > smallerPair2 then h1
    else if smallerPair1 < smallerPair2 then h2
    else
      let rec loop hnd1 hnd2 =
        match hnd1,hnd2 with
        | ([],[]) -> ( let r = Random.int 2 in
                       if r = 0 then h1 else h2 )
        | (x::xs,[]) -> h1
        | ([],x::xs) -> h2
        | (x1::xs1,x2::xs2) -> (if (val_of_card x1 = val_of_card x2) then
                                  loop xs1 xs2
                                else if compare_high_card [x1] [x2] = [x1] then
                                  h1
                                else
                                  h2 )
      in loop sorted_h1 sorted_h2

(* Triple comparison - returns the better hand *)
let compare_triple (h1:card list) (h2:card list) : card list =
  let sorted_h1 = insertion_sort h1 in
  let sorted_h2 = insertion_sort h2 in
  let v1 = value_of_triple (sorted_h1) in
  let v2 = value_of_triple (sorted_h2) in
  if v1 > v2 then h1
  else if v1 < v2 then h2
  else
    let rec loop hnd1 hnd2 =
      match hnd1,hnd2 with
      | ([],[]) -> ( let r = Random.int 2 in
                     if r = 0 then h1 else h2 )
      | (x::xs,[]) -> h1
      | ([],x::xs) -> h2
      | (x1::xs1,x2::xs2) -> ( if v1 = Some (val_of_card x1) then
                                 loop xs1 xs2
                               else if (val_of_card x1) < (val_of_card x2) then
                                 h2
                               else
                                 h1 )
    in loop sorted_h1 sorted_h2

(* Straight comparison - returns the better hand *)
let compare_straight (h1:card list) (h2:card list) : card list =
  match (insertion_sort (remove_dups h1),insertion_sort (remove_dups h2)) with
  (* because a straight with an A will be AKQJT or A5432 *)
  | (x1::x2::x3::xs,y1::y2::y3::ys) -> begin
    let highCard1 = if one_step_below x2 x1 && one_step_below x3 x2 then x1
                    else if one_step_below x3 x2 then x2 else x3 in
    let highCard2 = if one_step_below y2 y1 && one_step_below y3 y2 then y1
                    else if one_step_below y3 y2 then y2 else y3 in
    if low_straight h1 && low_straight h2 then ( let r = Random.int 2 in
                                                 if r = 0 then h1 else h2 )
    else if low_straight h1 then h2
    else if low_straight h2 then h1
    else if val_of_card highCard1 > val_of_card highCard2 then h1
    else h2
    end
  | _ -> failwith "incorrect input to function"

(* Flush comparison - returns the better hand *)
let compare_flush (h1:card list) (h2:card list) : card list =
  let s1,_ = most_common_suit h1 in
  let s2,_ = most_common_suit h2 in
  let newh1 = (insertion_sort h1) in
  let newh2 = (insertion_sort h2) in
  let rec loop hnd1 hnd2 =
    match (hnd1,hnd2) with
    | ([],[]) -> ( let r = Random.int 2 in
                   if r = 0 then h1 else h2 )
    | (x::xs,[]) -> h1
    | ([],x::xs) -> h2
    | (x1::xs1,x2::xs2) -> ( if val_of_card x1 <> val_of_card x2
                                && suit_of_card x1 = s1
                                && suit_of_card x2 = s2
                             then
                               ( if compare_high_card [x1] [x2] = [x1] then h1
                                 else h2 )
                             else loop xs1 xs2 )
  in loop newh1 newh2

(* FullHouse comparison *)
let compare_full_house (h1:card list) (h2:card list) : card list =
  let tripleValue1 = value_of_triple h1 in
  let tripleValue2 = value_of_triple h2 in
  if tripleValue1 > tripleValue2 then h1
  else if tripleValue1 < tripleValue2 then h2
  else
    let rec loop hnd1 hnd2 =
      match hnd1,hnd2 with
      | ([],[]) -> ( let r = Random.int 2 in
                     if r = 0 then h1 else h2 )
      | (x::xs,[]) -> h1
      | ([],x::xs) -> h2
      | (x1::xs1,x2::xs2) -> ( let v1 = val_of_card x1 in
                               let v2 = val_of_card x2 in
                               if Some v1 = tripleValue1 then loop xs1 xs2
                               else if v1 > v2 then h1
                               else if v1 < v2 then h2
                               else h1 )
    in loop (insertion_sort h1) (insertion_sort h2)

(* Quads comparison - returns the better hand*)
let rec compare_quads (h1:card list) (h2:card list) : card list =
  match (insertion_sort h1,insertion_sort h2) with
  | ([],[]) -> ( let r = Random.int 2 in
                 if r = 0 then h1 else h2 )
  | (x::xs,[]) -> h1
  | ([],x::xs) -> h2
  | (x1::x2::t1,y1::y2::t2) ->( let vx1 = val_of_card x1 in
                                let vx2 = val_of_card x2 in
                                let vy1 = val_of_card y1 in
                                let vy2 = val_of_card y2 in
                                if vx1 = vx2 && vy1 = vy2 then
                                  ( if vx1 = vy2 then compare_high_card h1 h2
                                    else if vx1 > vy2 then h1
                                    else h2 )
                                else compare_quads (x2::t1) (y2::t2) )
  | _ -> failwith "incorrect input to function"

(* StraightFlush comparison - returns the better hand *)
(* same as comparing a regular straight *)
let compare_straight_flush (h1:card list) (h2:card list) : card list =
  compare_straight h1 h2

(* RoyalFlush comparison *)
(* The only time both players can have a royal flush is when the
* 5 board cards make a royal flush so this is automatically a tie *)
let compare_royal_flush (h1:card list) (h2:card list) : card list =
  let r = Random.int 2 in
  if r = 0 then h1 else h2

let determine_best_hand (c:card list) : hand =
  if royal_flush_check c then (*checked*)
    let s,_ = most_common_suit c in
    let rec loop c2 =
      match c2 with
      | h1::h2::h3::h4::h5::t -> ( if suit_of_card h1 = s
                                   then RoyalFlush ([h1;h2;h3;h4;h5])
                                   else loop (h2::h3::h4::h5::t) )
      | _ -> failwith "violated precondition"
    in loop (insertion_sort_suit (insertion_sort c))
  else if straight_flush_check c then (*checked*)
    let s,_ = most_common_suit c in
    let rec loop c2 =
      match c2 with
      | h1::h2::h3::h4::h5::t -> ( if suit_of_card h1 = s
                                   then StraightFlush ([h1;h2;h3;h4;h5])
                                   else loop (h2::h3::h4::h5::t) )
      | _ -> failwith "violated precondition"
    in loop (insertion_sort_suit (insertion_sort c))
  else if fst (quads_check c) then (* checked *)
    let _,v = quads_check c in
    let rec loop c2 =
      match c2 with
      | h1::h2::h3::h4::h5::t -> ( if Some(val_of_card h1) = v
                                      || Some(val_of_card h2) = v
                                   then Quads [h1;h2;h3;h4;h5]
                                   else loop (h3::h4::h5::t) )
      | _ -> failwith "violated precondition"
    in loop (insertion_sort c)
  else if full_house_check c then (*checked*)
    let sorted_c = insertion_sort c in
    let vt = value_of_triple sorted_c in
    let vp = value_of_pair sorted_c in
    let rec loop c2 acc =
      match c2 with
      | [] -> FullHouse (acc)
      | h::t -> ( if Some (val_of_card h) = vt || val_of_card h = vp
                  then loop t (acc @ [h])
                  else loop t acc )
    in loop sorted_c []
  else if flush_check c then (*checked*)
    let s,_ = most_common_suit c in
    let rec loop c2 =
      match c2 with
      | h1::h2::h3::h4::h5::t -> ( if suit_of_card h1 = s
                                   then Flush ([h1;h2;h3;h4;h5])
                                   else loop (h2::h3::h4::h5::t) )
      | _ -> failwith "violated precondition"
    in loop (insertion_sort_suit c)
  else if straight_check c then
    let sorted_c = insertion_sort(remove_dups c) in
    let find v = List.find (fun x -> (val_of_card x) = v) sorted_c in
    let rec loop c2 =
      match c2 with
      | h1::h2::h3::h4::h5::t -> ( if one_step_below h2 h1
                                      && one_step_below h3 h2
                                      && one_step_below h4 h3
                                      && one_step_below h5 h4
                                   then Straight ([h1;h2;h3;h4;h5])
                                   else if straight_check (h2::h3::h4::h5::t)
                                   then loop (h2::h3::h4::h5::t)
                                   else
                                     let ace = find (value_of_string "A") in
                                     let five = find (value_of_string "5") in
                                     let four = find (value_of_string "4") in
                                     let three = find (value_of_string "3") in
                                     let two = find (value_of_string "2") in
                                     Straight ([ace;five;four;three;two]) )
      | _ -> failwith "violated precondition"
    in loop (insertion_sort (remove_dups c))
  else if triple_check c then (*checked*)
    let sorted_c = insertion_sort c in
    let vt = value_of_triple  sorted_c in
    let rec loop c2 acc1 acc2 =
      match c2 with
      | [] -> ( let highcards = insertion_sort acc2 in
                Triple (acc1@[(List.nth highcards 0);(List.nth highcards 1)]) )
      | h::t -> ( if Some(val_of_card h) = vt
                  then loop t (acc1 @ [h]) acc2
                  else loop t acc1 (acc2 @ [h]) )
    in loop sorted_c [] []
  else if two_pair_check c then (*checked*)
    let sorted_c = insertion_sort c in
    let vp = value_of_pair sorted_c in
    let rec loop c2 acc1 acc2 =
      match c2 with
      | [] -> ( let vp2 = value_of_pair (insertion_sort acc2) in
                let rec loop2 c3 acc3 acc4 =
                  match c3 with
                  | [] -> ( let highcard = List.nth (insertion_sort acc4) 0 in
                            TwoPair (acc1 @ acc3 @ [highcard]) )
                  | h::t -> ( if val_of_card h = vp2
                              then loop2 t (acc3@[h]) acc4
                              else loop2 t acc3 (acc4 @ [h]) )
                in loop2 acc2 [] [] )
      | h::t -> ( if val_of_card h = vp
                  then loop t (acc1 @ [h]) acc2
                  else loop t acc1 (acc2 @ [h]) )
    in loop sorted_c [] []
  else if pair_check c then (*checked*)
    let sorted_c = insertion_sort c in
    let vp = value_of_pair sorted_c in
    let rec loop c2 acc1 acc2 =
      match c2 with
      | [] -> ( let highcards = insertion_sort acc2 in
                Pair (acc1@[(List.nth highcards 0);(List.nth highcards 1);
                            (List.nth highcards 2)]) )
      | h::t -> ( if val_of_card h = vp
                  then loop t (acc1 @ [h]) acc2
                  else loop t acc1 (acc2 @ [h]) )
    in loop sorted_c [] []
  else
    match insertion_sort c with
    | h1::h2::h3::h4::h5::t -> HighCard ([h1;h2;h3;h4;h5]) (*checked*)
    | _ -> failwith "violated precondition"

let compare_hands (h1:hand) (h2:hand) : hand =
  let c1 = hand_to_card_list h1 in
  let c2 = hand_to_card_list h2 in
  match (h1,h2) with
  | (HighCard _,HighCard _) -> determine_best_hand (compare_high_card c1 c2)
  | (Pair _,Pair _) -> determine_best_hand (compare_pair c1 c2)
  | (TwoPair _,TwoPair _) -> determine_best_hand (compare_two_pair c1 c2)
  | (Triple _,Triple _) -> determine_best_hand (compare_triple c1 c2)
  | (Straight _,Straight _) -> determine_best_hand (compare_straight c1 c2)
  | (Flush _,Flush _) -> determine_best_hand (compare_flush c1 c2)
  | (FullHouse _,FullHouse _) -> determine_best_hand (compare_full_house c1 c2)
  | (Quads _,Quads _) -> determine_best_hand (compare_quads c1 c2)
  | (StraightFlush _,StraightFlush _) -> ( determine_best_hand(
                                             compare_straight_flush c1 c2) )
  | (RoyalFlush _,RoyalFlush _) -> ( determine_best_hand(
                                       compare_royal_flush c1 c2) )
  | _ -> if h1 > h2 then h1 else h2

(*helper that returns the string representation of a card list
* given the hand type as a string argument *)
let hand_to_string_helper (clist:card list) (s:string) : string =
  let s1 = s^ " of " in
  let rec loop cards acc =
    match cards with
    | [] -> acc
    | h::t -> ( if acc = "" then loop t (acc^card_to_string h)
                else loop t (acc^" and "^card_to_string h) )
  in s1^(loop (insertion_sort clist) "")

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
