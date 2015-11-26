(*open necessary files*)

(*types*)
type hand =
HighCard of card list
| Pair of card list
| TwoPair of card list
| Triple of card list  (*three of a kind*)
| Straight of card list
| Flush of card list
| FullHouse of card list
| Quads of card list  (*four of a kind*)
| StraightFlush of card list
| RoyalFlush of card list

(*functions*)

(* A function for converting cards to a player's BEST POSSIBLE hand.
* It will presumably be used extensively in the game to determine the winner
* of the hand.  Gamestate will call this function on each player's hands
* and then call compare on the two generated hands. Since this determines
* the best possible hand, we need all 7 available cards,
* the 2 from a player's hand and the 5 from the board.
* It could be fewer than seven cards for AI *)
val determine_best_hand : card list -> hand


(* Converts a hand to its rank relative to other hands.
* This is only enough to compare hands of the same hand type,
* comparisons within hands is done in some helper function
* i.e. HighCard -> 0, Pair -> 1, etc.*)
val hand_to_hand_rank : hand -> int

(* Function for comapring hands, will call a helper function for
* breaking ties between same rankings of hands.
* Ex: TwoPair AA338 > AA228
* Full House 99944 > 99444
* Returns the hand that is better. What to do if the hands are the same? *)
val compare_hands : hand -> hand -> hand

