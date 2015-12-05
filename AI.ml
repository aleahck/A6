open Gamestate
open Gamelogic
open Card

let call_amount g =
  let i,ai = List.hd g.players in
  g.bet - ai.amount_in

let point_standard g = match game_stage g with
  | Initial -> 15.
  | Flop    -> 40.
  | Turn    -> 55.
  | River   -> 70.

let same_suit_bonus = function
  | h1::h2::[] -> if same_suit h1 h2 then 6. else 0.
  | _          -> 0.

let check_within_5 c1 c2 =
  let rec helper a b i = if i = 0 then false
                         else if a = b then true
                         else helper (card_above a) b (i-1)
  in
  let gc,lc = if c1 > c2 then c1,c2 else c2,c1 in
  helper (card_above lc) gc 4
  

let possible_straight_bonus = function
  | h1::h2::[] -> if check_within_5 h1 h2 then 5. else 0.
  | _          -> 0.

let card_bonus c = match value_to_string (val_of_card c) with
  | "2"  -> 0.
  | "3"  -> 1.
  | "4"  -> 2.
  | "5"  -> 3.
  | "6"  -> 4.
  | "7"  -> 5.
  | "8"  -> 6.
  | "9"  -> 7.
  | "10" -> 8.
  | "J"  -> 9.
  | "Q"  -> 10.
  | "K"  -> 11.
  | "A"  -> 12.
  | _    -> failwith "?"

let rec cards_bonus = function
  | [] -> 0.
  | h::t -> (card_bonus h) +. (cards_bonus t)

let best_hand_bonus cards = match determine_best_hand cards with
  | HighCard clist -> cards_bonus clist
  | Pair clist    -> 23. +. cards_bonus clist
  | TwoPair clist  -> 40. +. cards_bonus clist
  | Triple clist  -> 60. +. cards_bonus clist
  | Straight clist -> 90. +. cards_bonus clist
  | Flush clist    -> 120. +. cards_bonus clist
  | FullHouse clist -> 150. +. cards_bonus clist
  | Quads clist    -> 300. +. cards_bonus clist
  | StraightFlush clist -> 500. +. cards_bonus clist
  | RoyalFlush clist -> 1000. +. cards_bonus clist

let pot_odds g = 
  let c = call_amount g in
  let bet_needed = float_of_int (c) in
  if c = 0 then 1.
  else bet_needed /. float_of_int(g.pot)

let hand_points_initial g =
  let _, ai = List.hd g.players in
  (cards_bonus ai.cards) +.(same_suit_bonus ai.cards) 
                         +.(possible_straight_bonus ai.cards)

let hand_points_midgame g =
  let _, ai = List.hd g.players in
  (best_hand_bonus (ai.cards @ g.flop))
  
let hand_points g = match game_stage g with
  | Initial -> hand_points_initial g
  | _       -> hand_points_midgame g

let rand_multiplier () = Random.self_init () ; (Random.float 0.5) +. 0.75

let rand_call_bound_for_game g = 
  Random.self_init() ;
  match game_stage g with
  | Initial -> Random.int 10
  | Flop    -> Random.int 20
  | Turn    -> Random.int 30
  | River   -> Random.int 40

let floor_bet_to_all_in bet g =
  match g.players with
  | (_,p1)::(_,p2)::t -> if bet > p1.stake || bet > p2.stake then
                           if p1.stake > p2.stake then p2.stake else p1.stake
                         else 
                           bet
  | _ -> bet

let risk_factor g =
  let _,ai = List.hd g.players in
  let to_call = float_of_int(call_amount g) in
  let stake = float_of_int(ai.stake) in
  1. -. (to_call /. (to_call +. stake))

let turn g =
  let _,ai = List.hd g.players in
  let risk = (risk_factor g) *. (rand_multiplier()) in
  let modified_points = risk *. (hand_points g) in
  let points_needed = (point_standard g) *. (pot_odds g) in
  let diff_in_points = int_of_float (modified_points -. points_needed) in
  let can_check = g.last_move = Check || 
                  (g.last_move = Deal && not ((game_stage g) = Initial)) ||
                  ((game_stage g) = Initial && g.last_move = Call) in
  let can_raise = not ai.did_raise in
  let rand_call_bound = rand_call_bound_for_game g in
  let to_call = call_amount g in
  let max_call = if rand_call_bound > to_call then rand_call_bound 
                 else to_call in
  if can_check && diff_in_points <= max_call then
    (print_endline "" ;
     print_endline "|-------------|" ;
     print_endline "|             |" ;
     print_endline "|  AI checks  |" ;
     print_endline "|             |";
     print_endline "|-------------|" ; 
     print_endline "" ;
     check g)
  else if diff_in_points <= 0 then
    (print_endline "" ;
     print_endline "|------------|" ;
     print_endline "|            |" ;
     print_endline "|  AI folds  |" ;
     print_endline "|            |" ;
     print_endline "|------------|" ; 
     print_endline "" ;
     fold g)
  else if diff_in_points <= max_call || not can_raise then
    (print_endline "" ;
     print_endline "|------------|" ;
     print_endline "|            |" ;
     print_endline "|  AI calls  |" ;
     print_endline "|            |" ;
     print_endline "|------------|" ; 
     print_endline "" ;
     call g)
  else 
    let to_raise = (diff_in_points - to_call) in
    let amount = floor_bet_to_all_in to_raise g in
    let extra_dashes,extra_spaces = if amount / 100 > 0 then "---","   "
                                    else if amount / 10 > 0 then "--","  "
                                    else "-"," " in 
    (print_endline "" ;
     Printf.printf "|------------%s--|\n" extra_dashes ;
     Printf.printf "|            %s  |\n" extra_spaces ;
     Printf.printf "|  AI raises %d  |\n" amount ;
     Printf.printf "|            %s  |\n" extra_spaces ;
     Printf.printf "|------------%s--|\n" extra_dashes ;
     print_endline "" ;
     do_raise g amount)


