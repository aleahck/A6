open Card
 
(* abstract type representing a deck *)
type deck

exception DeckEmpty

(* generates a new deck in order *)
val order_deck: unit -> deck

(* generates a new deck in random order *)
val rand_deck: unit -> deck

(* retrieves the top card off a deck 
 * raises DeckEmpty if the Deck has no cards *)
val top_card: deck -> card * deck

(* retrieves the top 2 cards off a deck 
 * raises DeckEmpty if the Deck has 0 or 1 cards *)
val top2_cards: deck -> card * card * deck

(* retrieves the top 3 cards off a deck 
 * raises DeckEmpty if the Deck has 0, 1, or 2 cards *)
val top3_cards: deck -> card * card * card * deck

