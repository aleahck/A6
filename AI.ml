open Gamestate
open Gamelogic
open Card

type gamestage = Initial | Flop | Turn | River

let game_stage g = match g.flop with
  | h1::h2::h3::h4::h5::[] -> River
  | h1::h2::h3::h4::[]     -> Turn
  | h1::h2::h3::[]         -> Flop
  | _                      -> Initial

let call_amount g =
  let i,ai = List.hd g.players in
  g.bet - ai.amount_in

let point_standard g = match game_stage g with
  | Initial -> 15.
  | Flop    -> 40.
  | Turn    -> 55.
  | River   -> 90.

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
  else bet_needed /. (float_of_int(g.pot) +. bet_needed)

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

let rand_multiplier () = Random.self_init () ; (Random.float 1.) +. 0.5

let floor_bet_to_all_in bet g =
  match g.players with
  | (_,p1)::(_,p2)::t -> if bet > p1.stake || bet > p2.stake then
                           if p1.stake > p2.stake then p2.stake else p1.stake
                         else 
                           bet
  | _ -> bet

let turn g =
  let modified_points = (rand_multiplier ()) *. (hand_points g) in
  let points_needed = (point_standard g) *. (pot_odds g) in
  let diff_in_points = int_of_float (modified_points -. points_needed) in
  let can_check = g.last_move = Check || g.last_move = Deal in
  let call_v_raise = (Random.self_init() ; Random.int 50) ;
  if can_check && diff_in_points <= call_v_raise then
    (print_endline "AI checks" ; check g)
  else if diff_in_points <= 0 then
    (print_endline "AI folds" ; fold g)
  else if diff_in_points <= call_v_raise then
    (print_endline "AI calls" ; call g)
  else (*not sure when he can raise*)
    let amount = floor_bet_to_all_in diff_in_points g in
    (Printf.printf "AI raise %d\n" amount ; do_raise g amount)


