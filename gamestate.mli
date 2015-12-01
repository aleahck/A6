open Deck
open Card
open Gamelogic

(*GAMESTATE MLI*)

(*Type player contains user data including fields for

*stake: the total amount of money the user has as an int
*cards: a pair of cards the user has in their hand
amount_in: the amount the user has bet in the current hand as an int*)
type player = {
    stake: int;
    mutable cards: card list;
    amount_in: int
  }

type id = string

type move = Call | Raise of int | Check | Fold | Deal

(*Type game contains info on the game including fields for
*flop: the current face-up cards as a card list
*bet: the bet for the current hand as an int
*pot: the total money that has been bet in the hand as an int
*players: the player queue for the hand as a list
*deck: cards left to be delt, of type deck
*first_better: the player that starts the hand*)
type game= {
    flop: card list;
    bet: int;
    pot: int;
    players: (id * player) list;
    deck: deck;
    first_better: id list;
    last_move: move
  }

val is_valid_raise: int-> game->bool

val is_valid_call: game-> bool

val is_valid_check: game-> bool

val end_betting: game-> bool

val add1_flop: game-> game

val add3_flop: game-> game

val winner: game-> string * hand

val winner_to_string: game-> string

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
val player_to_string : player-> string
