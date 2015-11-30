open Gamestate
open Gamelogic
open Card

(* Takes in a game state, calculates a move, and returns the updated game state.*)
val turn : Gamestate.game -> Gamestate.game
