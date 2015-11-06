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
    


