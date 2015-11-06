open Deck
open Card
open Logic

(*GAMESTATE MLI*)

(*Type player contains user data*)
type player ={
    stake: int;
    cards: card * card;
    best: hand (*whats this being called from logic?*)
  }


(*Type game contains info on bets, deck*)
type game= {
    flop: card list;
    bet: int;
    pot: int;
    players: player list
    deck: deck
  }
    

(*Updates the game record*)
val update_game= string-> game-> game

(*Updates a player*)
val update_player= string-> player-> player

(*Formats a game to a string for printing*)
val game_to_string= game-> string

(*Formats a player to a string for printing*)
val player_to_string= player-> string
