open Gamestate
open Gamelogic
open Card

(* Given a gamestate, calculates a move, prints the result and returns an
 * updated gamestate *)
val turn : Gamestate.game -> Gamestate.game
