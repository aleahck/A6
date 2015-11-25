(* abstract type representing a Card *)
type card

(* abstract type representing a Card's suit *)
type suit

(* abstract type representing a Card's face value *)
type value

(* returns the suit of a card *)
val suit_of_card: card -> suit

(* returns the value of a card *)
val val_of_card: card -> value

(* returns the card represented by the string: "value suit"
 * ie: "K H" returns King of Hearts, "10 D" returns Ten of Diamonds
 * returns None if string is not a valid representation of a card. *)
val card_of_string: string -> card option

(* returns the value represented by the string: "value"
 * ie: "Q" returns Queen, "2" returns Two
 * returns None if string is not a valid representation of a value. *)
val value_of_string: string -> value option

(* returns the suit represented by the string: "suit"
 * ie: "H" returns Hearts, "C" returns Clubs
 * returns None if string is not a valid representation of a suit. *)
val suit_of_string: string -> suit option

(* returns the string representing the provided suit.
 * ie: Hearts returns "Hearts" *)
val suit_to_string: suit -> string

(* returns the string representing the provided values.
 * ie: Two returns "2", Ace returns "A" *)
val value_to_string: value -> string

(* returns the string representing the provided card.
 * ie: Two of Hearts returns "2 of Hearts" *)
val card_to_string: card -> string

(* returns a card that is one step above the given card.
 * "one step" is defined as an incrementation of the value.
 * If the given card is an Ace, the suit is incremented as follows:
 *   Hearts -> Diamonds -> Clubs -> Spades -> Hearts -> etc. *)
val card_above: card -> card

(* returns true if the two given cards are the same suit *)
val same_suit: card -> card -> bool

(* returns true if the two given cards are the same value *)
val same_value: card -> card -> bool

(* returns true if the first card is one step above the second card.
 * "one step above" is defined as a value that is the incremenation of
 *   the first value. 
 *   ie: King is one step above Queen, Ten is one step above Nine. 
 * suit is ignored in this comparison. *)
val one_step_above: card -> card -> bool

(* returns true if the first card is one step below the second card.
 * "one step below" is defines as a value that is the decrementation of
 *   the first value.
 *   ie: King is one step below Ace, Two is one step below Three. 
 * suit is ignored in this comparison. *)
val one_step_below: card -> card -> bool

