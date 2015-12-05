open Deck
open Card
open Gamelogic

(*GAMESTATE MLI*)


(*gamestage represents the parts of a hand as determined by the number of cards
*on the table. Initial will be before any cards are out, Flop will be when 3
*cards are out, Turn when 4 and River when 5.*)
type gamestage= Initial| Flop | Turn | River

(*Type player contains user data including fields for
*stake: the total amount of money the user has as an int
*cards: a pair of cards the user has in their hand
amount_in: the amount the user has bet in the current hand as an int*)
type player = {
    stake: int;
    mutable cards: card list;
    amount_in: int
  }

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
val game_stage: game-> gamestage

(* Returns false if the current player is unable to raise the bet by i. *)
val is_valid_raise: int-> game->bool

(* Returns false if the current player is unable to call.*)
val is_valid_call: game-> bool

(* Returns false if the current player is unable to check. *)
val is_valid_check: game-> bool

(* Returns true if any player in the game is out of money or if the last
* move is Call. *)
val end_betting: game-> bool

(* Adds 1 card to the flop for Turn and River game stages. *)
val add1_flop: game-> game

(* Adds 3 cards to the flop for Initial stage. *)
val add3_flop: game-> game

(* Returns a pair of the ID of the player that won the round and the best
* hand. Helper function for winner_to_string. *)
val winner: game-> string * hand

(* Returns a string of the best hand and the ID of the player that has won. *)
val winner_to_string: game-> string

(* Returns the ID of the current player. *)
val get_current_id: game-> string

(*Takes in an int to raise the current bet by and the current game state.
*Updates the bet to match raised value. The int will always be valid as this
*must be called after is_valid_bet for human inputs*)
val do_raise : game-> int-> game

(*Takes in a game. Removes the value of the current bet from the current
*player's stake and moves to the next player*)
val call : game-> game

(*Takes in a game. Moves to next player but does not change gamestate
*otherwise*)
val check : game-> game

(*Takes in a game and ignores player that folds for rest of hand. When
*implemented for only two players, this ends the hand*)
val fold : game-> game

(*Creates a new game*)
val make_game : unit-> game

(*Formats a game to a string for printing*)
val game_to_string : game-> string

(*Formats a player to a string for printing*)
val player_to_string : game-> player-> string
