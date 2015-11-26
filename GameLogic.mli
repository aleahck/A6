(*open necessary files*)

(*types*)
type fiveTuple = card * card * card * card * card

type hand =
HighCard of fiveTuple |
Pair of fiveTuple |
TwoPair of fiveTuple |
Triple of fiveTuple | (*three of a kind*)
Straight of fiveTuple |
Flush of fiveTuple |
FullHouse of fiveTuple |
Quads of fiveTuple | (*four of a kind*)
StraightFlush of fiveTuple |
RoyalFlush of fiveTuple

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

