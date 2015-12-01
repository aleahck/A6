open Deck
open Card
open Gamelogic


type player = {
    stake: int;
    mutable cards: card list;
    amount_in: int
  }

let big_blind = 2

let little_blind =1

type id = string

type move = Call | Raise of int | Check | Fold | Deal

type game= {
    flop: card list;
    bet: int;
    pot: int;
    players: (id * player) list;
    deck: deck;
    first_better: id list;
    last_move: move
  }

let current_player (g:game) = snd (List.hd g.players)

(*  *)
let get_current_id (g:game) = fst (List.hd g.players)

(* Adds 1 card to the flop. *)
let add1_flop (g:game) =
  let d1 = top_card g.deck in
  let old_flop = g.flop in
  {g with deck = snd d1; flop = old_flop@[fst d1]}

(* Adds 3 cards to the flop (for new hands). *)
let add3_flop (g:game) =
  let d1 = top3_cards g.deck in
  let new_first = (if g.first_better = fst (List.hd g.players)
    then List.rev g.players
    else g.players)
  {g with deck = snd d1; flop = fst d1; players = new_first}


(*[do_player_bet p i] removes [i] from [p]'s stake and adds it to [p]'s
*amount_in. Returns a new, updated player generated by these actions.*)
let do_player_raise (g:game) (p:player) (i: int)=
  let difference= g.bet - p.amount_in in
  {stake= p.stake- (difference+ i);
   cards= p.cards;
   amount_in= p.amount_in + (difference+i)
  }

(* Returns false if any player in the game is out of money or if the last
* move is Call. *)
let end_betting (g:game) =
  not (List.for_all (fun x -> (snd x).stake > 0) g.players) ||
  g.last_move = Call


(* Returns false if the current player is unable to call.*)
let is_valid_call (g:game) =
  if (g.bet - (current_player g).amount_in) > (current_player g).stake
    then false
  else true

(* Returns false if the current player is unable to check. *)
let is_valid_check (g:game) =
  if g.bet = (current_player g).amount_in then true else false

(* Returns false if the current player is unable to raise the bet by i. *)
let is_valid_raise (i:int) (g:game) =
  let difference = g.bet - (current_player g).amount_in in
  if ((difference + i) >= (current_player g).stake) || (i<=0) then false
  else true

(* Raises the current bet and pot by i, subtracts it from the current player's
* stake, adds it to their amount in, changes the current_player to the next
* player, and changes the last_move to Raise. *)
let do_raise (g:game) (i:int)=
  let new_player= do_player_raise g (current_player g) i in
  let p_id = get_current_id g in
  let new_players= List.tl (g.players)@[(p_id,new_player)] in
  let new_pot = g.pot+(g.bet - (current_player g).amount_in) + i in
  {flop= g.flop;
   bet= g.bet+i;
   pot= new_pot;
   players= new_players;
   deck= g.deck;
   first_better= g.first_better;
   last_move= Raise i
  }

(* Changes the last move to Call and returns the new gamestate. *)
let call (g:game) =
  let new_game = do_raise g 0 in
  { new_game with last_move= Call }

(* Changes the last move to Check  *)
let check (g:game) =
  { g with players = (List.tl g.players)@
    [((get_current_id g),(current_player g))];
	  last_move= Check
  }


(* Helper function for new_hand. *)
let undelt pfield g =
  {flop= [];
     bet=0;
     pot = big_blind+little_blind;
     players= pfield;
     deck= rand_deck();
     first_better= (List.tl g.first_better)@ [List.hd g.first_better];
     last_move= Deal
  }


(* Deals two cards to each player and returns the gamestate with the updated
* deck and player records. *)
let deal_two (g:game) =
  let d1 = top2_cards g.deck in
  let d2 = top2_cards (snd d1) in
  let p1 = snd (List.hd g.players) in
  let p2 = snd (List.nth g.players 1) in
  p1.cards <- fst (d1) ;
  p2.cards <- fst (d2) ;
  { g with
    players = [(fst(List.hd g.players),p1) ; (fst(List.nth g.players 1),p2)] ;
    deck = snd d2;
    last_move= Deal
  }


(* Helper for fold and dealer. Creates new hand on the turn a player folds. *)
let new_hand (g:game) =
  let fst_player1= snd (List.hd g.players) in
  let fst_player= {fst_player1 with stake=fst_player1.stake} in
  let fst_id= fst (List.hd g.players) in
  let snd_player1 = snd (List.nth g.players 1) in
  let snd_id= fst (List.nth g.players 1) in
  let snd_player= {snd_player1 with stake = snd_player1.stake + g.pot} in
  let new_start= List.nth g.first_better 1 in
  if (new_start=fst_id) then
    let undelt1 =
    undelt
      ([(fst_id,{fst_player with stake = fst_player.stake - big_blind});
      (snd_id,{snd_player with stake = snd_player.stake - little_blind})])
      g
    in
    deal_two undelt1
  else
    let undelt2 =
    undelt
      ([(snd_id,{snd_player with stake = snd_player.stake - big_blind});
      (fst_id,{fst_player with stake = fst_player.stake - little_blind})])
      g
    in
    deal_two undelt2


(* Turns card list into string. Helper function for to_string functions *)
let rec string_of_clist lst acc =
  match lst with
    | h::[] -> let acc2 = (acc ^ (card_to_string h)) in
      (string_of_clist [] acc2)
    | h::t -> let acc2 = (acc ^ (card_to_string h) ^ ", ") in
      (string_of_clist t acc2)
    | [] -> acc

(* Turns player list into string.
* Only prints out the player IDs (i.e. "Player 1, Player 2, Player 3")
* Helper function for to_string functions. *)
let rec string_of_plist lst acc =
  match lst with
    | h::[] -> let acc2 = acc ^ "Player " ^ (string_of_int (fst h)) in
      (string_of_plist [] acc2)
    | h::t -> let acc2 = acc ^ "Player " ^ (string_of_int (fst h)) ^ ", " in
      (string_of_plist t acc2)
    | [] -> acc


(* Returns the string of the stake, cards and bet of the player.
* Helper function for game_to_string. *)
let player_to_string (p:player) =
  "Your stake is: " ^ (string_of_int p.stake) ^ "\n" ^
  "Your cards are: " ^ (string_of_clist p.cards "") ^ "\n" ^
  "You have bet: " ^ (string_of_int p.amount_in) ^ "\n"

(* Returns the string of the flop, bet and pot fields of the game state. *)
let game_to_string (g:game) =
  "The flop is: " ^ (string_of_clist g.flop "") ^ "\n" ^
  "The bet is: " ^ (string_of_int g.bet) ^ "\n" ^
  "The pot is: " ^ (string_of_int g.pot) ^ "\n" ^
  player_to_string (List.assoc "You" g.players)


(* Only works for 2 players; only ends the hand instead of continuing hand
  without player who folded. *)
let fold (g:game) =
  let new_h = new_hand g in
  let continue = List.for_all (fun x -> (snd x).stake >=0) new_h.players in
  if continue
    then (print_string ("A new hand has begun \n" ^ game_to_string new_h);new_h)
  else (if (current_player new_h).stake >=0 then
        ((Printf.printf "%s wins!\n" (List.hd new_h.first_better)); exit 0)
        else
        ((Printf.printf "%s wins!\n" (List.nth new_h.first_better 1)); exit 0))


(* Helper function for new_game. *)
let new_player () =
  {stake = 200; (* arbitrary *)
  cards = [card_of_string "A H"; card_of_string "A H"];
  amount_in = 0}

(* Returns a new game state with re-initialized fields. *)
let make_game () =
  let new_player1= new_player () in
  let new_player2= new_player () in
  {flop = [];
  bet = 0;
  pot = 0;
  players = [("You",new_player1);("AI",new_player2)];
  deck = rand_deck();
  first_better = ["You";"AI"];
  last_move= Deal
  }


(* Returns a pair of the ID of the player that won the round and the best
* hand. Helper function for winner_to_string. *)
let winner (g:game) =
  let p1 = snd (List.hd g.players) in
  let h1 = determine_best_hand p1.cards in
  let p2 = snd (List.nth g.players 1) in
  let h2 = determine_best_hand p2.cards in
  if (compare_hands h1 h2) = h1 then (fst (List.hd g.players), h1)
  else (fst (List.nth g.players 1), h2)

(* Returns a string of the best hand and the ID of the player that has it. *)
let winner_to_string (g: game) =
    (fst (winner g) ^ "won with the hand:" ^ hand_to_string (snd (winner g)) )

