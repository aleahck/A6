open Deck
open Card
open Logic

(*GAMESTATE MLI*)

(*Type player contains user data*)
type player ={
    stake: int;
    cards: card * card;
    best: hand; (*whats this being called from logic?*)
    amount_in: int
  }


(*Type game contains info on bets, deck*)
type game= {
    flop: card list;
    bet: int;
    pot: int;
    players_hand: player list
    deck: deck;
    first_better: player;
    players_game: player list
  }
    

(*Takes in an int to raise the current bet by and the current game state. Updates the bet to match raised value*)
val raise_by= int-> game-> game

(*Takes in a game. Removes the value of the current bet from the current player's stake and moves to the next player*)
val call= game-> game

(*Takes in a game. Moves to next player*)
val check= game->game

(*Takes in a game and ignores player that folds for rest of hand*)
val fold= game->game

(*Deals cards to flop and starts new hand at the end of the hand. Returns Some of the updated game state.
*If the game is over, returns None*)
val dealer= game->option game

(*Creates a new game*)
val new_game= unit-> game

(*Formats a game to a string for printing*)
val game_to_string= game-> string

(*Formats a player to a string for printing*)
val player_to_string= player-> string
