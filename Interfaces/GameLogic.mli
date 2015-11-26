(*open necessary files*)

(*types*)

type hand =
HighCard of card * card * card * card * card |
Pair of card * card * card * card * card |
TwoPair of card * card * card * card * card |
Triple of card * card * card * card * card | (*three of a kind*)
Straight of card * card * card * card * card |
Flush of card * card * card * card * card |
FullHouse of card * card * card * card * card |
Quads of card * card * card * card * card | (*four of a kind*)
StraightFlush of card * card * card * card * card |
RoyalFlush of card * card * card * card * card
(*could be card lists instead, or other data type?*)

(*functions*)

(*function for converting cards to a player's BEST POSSIBLE hand,
will presumably be used extensively in the game to determine the winner
of the hand, gamestate might call this function on each player's hands
and then call compare on the two generated hands.
Since this determines the best possible hand, we need all 7 available
cards, the 2 from a player's hand and 5 from the board.
card list or card * card etc...*)
val determine_best_hand : card list -> hand


(*converts a hand to its rank relative to other hands
but this is only enough to compare hands of the same
hand typ, comparisons within hands is done in some helper function*)
(*i.e. HighCard -> 0, Pair -> 1, etc.*)
val hand_to_hand_rank : hand -> int

(*function for comapring hands, will call a helper function for
breaking ties between same rankings of hands
Ex: TwoPair AA338 > AA228
Full House 99944 > 99444
easy if comparing different hands
returns the hand that is better*)
val compare_hands : hand -> hand -> hand

