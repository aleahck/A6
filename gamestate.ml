open Deck
open Card
open Gamelogic

(*gamestage represents the parts of a hand as determined by the number of cards
*on the table. Initial will be before any cards are out, Flop will be when 3
*cards are out, Turn when 4 and River when 5.*)
type gamestage= Initial | Flop | Turn | River

(*Type player contains user data including fields for
*stake: the total amount of money the user has as an int
*cards: a pair of cards the user has in their hand
amount_in: the amount the user has bet in the current hand as an int*)
type player = {
    stake: int;
    mutable cards: card list;
    amount_in: int;
  }

(* The amount of the big blind. Each round will initialize with the big blind
* as 2. *)
let big_blind = 2

(* The amount of the little blind. Each round will initialize with the little
* blind as 1. *)
let little_blind =1

(*The ID of a player - either "You" or "AI".*)
type id = string

(* The variant of player moves. Contains all possible moves in the game. Raise
* is the only constructor that should have an int field because it is the only
* time a player needs to specify how much money they want to raise by. *)
type move = Call | Raise of int | Check | Fold | Deal

(* Type game contains info on the game including fields for
*flop: the current face-up cards as a card list
*bet: the bet for the current hand as an int
*pot: the total money that has been bet in the hand as an int
*players: player queue for the hand; an association list of (id*player) pairs
*deck: cards left to be dealt, of type deck
*first_better: the player that starts the hand, a queue of player IDs
*last_move: the last move made by either player *)
type game= {
    flop: card list;
    bet: int;
    pot: int;
    players: (id * player) list;
    deck: deck;
    first_better: id list;
    last_move: move
  }

(*Takes in a game [g] and returns a gamestage determined by how many cards are
* in [g.flop]*)
let game_stage g= match g.flop with
    |a::b::c::d::e::[]-> River
    |a::b::c::d::[]-> Turn
    |a::b::c::[]-> Flop
    |_-> Initial

(* Returns the record of the current player. *)
let current_player (g:game) = snd (List.hd g.players)

(* Returns the record of the second player in the player queue. *)
let second_player (g:game) = snd (List.nth g.players 1)

(* Returns the ID of the current player. *)
let get_current_id (g:game) = fst (List.hd g.players)

(* Returns the ID of the second player in the player queue. *)
let get_second_id (g:game) = fst (List.nth g.players 1)

(* Adds 1 card to the flop for Turn and River game stages. *)
let add1_flop (g:game) =
  let d1 = top_card g.deck in
  let old_flop = g.flop in
  let new_first = (if ((List.nth g.first_better 1) = get_current_id g)
    then (List.rev g.players)
    else g.players) in
  {g with deck = snd d1;
	  flop = old_flop@[fst d1];
	  last_move = Deal;
	  players= new_first}

(* Adds 3 cards to the flop for Initial stage. *)
let add3_flop (g:game) =
  let d1 = top3_cards g.deck in
  let new_first = (if ((List.nth g.first_better 1) =get_current_id g)
    then (List.rev g.players)
    else g.players) in
  {g with deck = snd d1; flop = fst d1; players = new_first; last_move = Deal}


(*[do_player_bet p i] removes [i] from [p]'s stake and adds it to [p]'s
*amount_in. Returns a new, updated player generated by these actions.*)
let do_player_raise (g:game) (p:player) (i: int)=
  let difference= g.bet - p.amount_in in
  { stake= p.stake - (difference+ i);
    cards= p.cards;
    amount_in= p.amount_in + (difference+i)
  }


(* Returns true if any player in the game is out of money or if the last
* move is Call. *)
let end_betting (g:game) =
  (not (List.for_all (fun x -> (snd x).stake > 0) g.players)) ||
  (g.last_move = Call)


(* Returns false if the current player is unable to call.*)
let is_valid_call (g:game) =
  if (g.bet - (current_player g).amount_in) > (current_player g).stake
    then false
  else true

(* Returns false if the current player is unable to check. *)
let is_valid_check (g:game) =
  g.bet = (current_player g).amount_in &&
  ((g.last_move = Check) || (g.last_move = Deal && (game_stage g <> Initial)) ||
    (game_stage g = Initial && g.last_move = Call))

(* Returns false if the current player is unable to raise the bet by i. *)
let is_valid_raise (i:int) (g:game) =
  let last_raise= match g.last_move with
    | Raise j -> j
    |_-> 0 in
  let difference = g.bet - (current_player g).amount_in in
  ((((difference + i) <= (current_player g).stake) || (i<0)) && i>=last_raise)

(* Raises the current bet and pot by i, subtracts it from the current player's
* stake, adds it to their amount in, changes the current_player to the next
* player, and changes the last_move to Raise. *)
let do_raise (g:game) (i:int)=
  let num = if i > (second_player g).stake then
    (second_player g).stake
    else i
  in
  let new_player= do_player_raise g (current_player g) num in
  let p_id = get_current_id g in
  let new_players= (List.tl (g.players))@[(p_id,new_player)] in
  let difference= g.bet- (current_player g).amount_in in
  let new_pot = g.pot+ difference + num  in
  {flop= g.flop;
   bet= g.bet+num;
   pot= new_pot;
   players= new_players;
   deck= g.deck;
   first_better= g.first_better;
   last_move= Raise num
  }

(* Changes the last move to Call and returns the new gamestate. *)
let call (g:game) =
  let new_game = do_raise g 0 in
  {new_game with last_move= Call }

(* Changes the last move to Check  *)
let check (g:game) =
  { g with players = List.rev g.players;
	  last_move= Check
  }


(* Deals two cards to each player and returns the gamestate with the updated
* deck and player records. *)
let deal_two (g:game) =
  let d1 = top2_cards g.deck in
  let d2 = top2_cards (snd d1) in
  let p1 = current_player g   in
  let p2 = second_player g in
  p1.cards <- fst (d1) ;
  p2.cards <- fst (d2) ;
  { g with
    players = ([((get_current_id g),p1) ; ((get_second_id g),p2)] );
    deck = snd d2;
    last_move= Deal
  }

(* Helper function for new_hand. *)
let undelt pfield g =
  {flop= [];
     bet=big_blind;
     pot = big_blind+little_blind;
     players= pfield;
     deck= rand_deck();
     first_better= List.rev g.first_better;
     last_move= Deal
  }

(* Helper for fold and dealer. Creates new hand on the turn a player folds. *)
let new_hand (g:game) =
  let fst_player1= current_player g in
  let fst_player=
    {fst_player1 with stake=fst_player1.stake} in
  let fst_id= get_current_id g in
  let snd_player1 = second_player g in
  let snd_id=get_second_id g in
  let snd_player=
    {snd_player1 with stake = snd_player1.stake + g.pot} in
  let new_start= List.hd g.first_better in
    let undealt1=
    if (new_start=fst_id)
    then (undelt
        ([(fst_id,{fst_player with
          stake = fst_player.stake - little_blind;
          amount_in = little_blind});
        (snd_id,{snd_player with
          stake = snd_player.stake - big_blind;
          amount_in = big_blind})])
        g)
    else (undelt
        ([(snd_id,{snd_player with
          stake = snd_player.stake - little_blind;
          amount_in = little_blind});
        (fst_id,{fst_player with
          stake = fst_player.stake - big_blind;
          amount_in = big_blind})])
        g)
  in
    deal_two undealt1

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
let player_to_string (g:game) (p:player) =
  let print = "Your stake is: " ^ (string_of_int p.stake) ^ "\n" ^
  "Your cards are: " ^ (string_of_clist p.cards "") ^ "\n" in
  if (is_valid_check g) then (print ^ "You can check")
  else print ^ "Calling will cost you: " ^ (string_of_int (g.bet - p.amount_in))

(* Returns the string of the flop, bet and pot fields of the game state. *)
let game_to_string (g:game) =
  let c_list_string = if (string_of_clist g.flop "") = "" then "None"
    else (string_of_clist g.flop "") in
  let ai_rec = List.assoc "AI" g.players in
  "The board is: " ^ c_list_string ^ "\n" ^
  "The bet is: " ^ (string_of_int g.bet) ^ "\n" ^
  "The pot is: " ^ (string_of_int g.pot) ^ "\n\n" ^
  ("AI has stake: " ^ (string_of_int ai_rec.stake)) ^ "\n\n" ^
  (player_to_string g (List.assoc "You" g.players))


(* Only works for 2 players; only ends the hand instead of continuing hand
  without player who folded. *)
let fold (g:game) =
  let new_h = new_hand g in
  let continue = List.for_all (fun x -> (snd x).stake >=0) new_h.players in
  if continue
    then (print_string ("\nA NEW HAND HAS BEGUN!\n");new_h)
  else (if (current_player new_h).stake >=0 then
        (Printf.printf "\n%s won the game!\n" (get_current_id new_h); exit 0)
        else
        (Printf.printf "\n%s won the game!\n" (get_second_id new_h); exit 0))


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

(*helper function for checking if hands are the same
* hands should only be 5 cards*)
let check_same_hands (h1:hand) (h2:hand) : bool =
  let cl1 = hand_to_card_list h1 in
  let cl2 = hand_to_card_list h2 in
  let rec loop lst =
    match lst with
    | [] -> true
    | h::t -> if List.mem h cl2 then loop t else false
  in loop cl1

(* Returns a pair of the ID of the player that won the round and the best
* hand. Helper function for winner_to_string. *)
let winner (g:game) =
  let p1 = current_player g in
  let h1 = determine_best_hand (p1.cards@g.flop) in
  let p2 = second_player g in
  let h2 = determine_best_hand (p2.cards@g.flop) in
  if check_same_hands (compare_hands h1 h2) h1
  then (fst (List.hd g.players), h1)
  else (fst (List.nth g.players 1), h2)

(* Returns a string of the best hand and the ID of the player that has won. *)
let winner_to_string (g: game) =
    "\n"^(fst (winner g)^" won with the hand: "^hand_to_string (snd (winner g)))
    ^"\n"
