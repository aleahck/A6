open Deck
open Card
open Logic

(*GAMESTATE MLI*)

(*Type player contains user data including fields for

*stake: the total amount of money the user has as an int
*cards: a pair of cards the user has in their hand
amount_in: the amount the user has bet in the current hand as an int*)
type player ={
    stake: int;
    cards: card * card;
    amount_in: int
  }


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
    players: player list; (*remove if implem for multiple AI*)
    deck: deck;
    first_better: player;
    (*following are for multiple AI implem*)
    (*players_game: player list
    *players_hand: player list*)
  }

(* Takes in an int and a game. If the int is a valid bet, it returns true. If
*the int is greater than the current players stake, violates rules on blinds,
*or is not sufficient to at least meat the bet for the current hand, it returns
*false*)
val is_valid_bet : int -> game -> bool

(*Takes in an int to raise the current bet by and the current game state.
*Updates the bet to match raised value. The int will always be valid as this
*must be called after is_valid_bet for human inputs*)
val raise_by : int-> game-> game

(*Takes in a game. Removes the value of the current bet from the current
*player's stake and moves to the next player*)
val call : game-> game

(*Takes in a game. Moves to next player but does not change gamestate
*otherwise*)
val check : game->game

(*Takes in a game and ignores player that folds for rest of hand. When
*implemented for only two players, this ends the hand*)
val fold : game->game

(*Deals cards to flop and starts new hand at the end of the hand. Returns Some of the updated game state.
*If the game is over, returns None*)
val dealer : game->option game

(*Creates a new game*)
val new_game : unit-> game

(*Formats a game to a string for printing*)
val game_to_string : game-> string

(*Formats a player to a string for printing*)
val player_to_string : player-> string
