module Deck = sig

  (* abstract type representing a deck *)
  type deck

  (* generates a new deck in order *)
  val order_deck: unit -> deck

  (* generates a new deck in random order *)
  val rand_deck: unit -> deck

  (* shuffles a deck *)
  val shuffle: deck -> deck

  (* retrieves the top card off a deck *)
  val top_card: deck -> card

  (* retrieves the top 2 cards off a deck *)
  val top2_cards: deck -> card * card

  (* retrieves the top 3 cards off a deck *)
  val top3_cards: deck -> card * card * card
end
