
type suit = Hearts | Diamonds | Clubs | Spades

type value = Two | Three | Four | Five | Six | Seven | Eight |
             Nine | Ten | Jack | Queen | King | Ace

type card = value * suit

exception InvalidString

let suit_of_card (c: card) = snd c

let val_of_card (c: card) = fst c

let suit_of_string s =
  let s2 = String.capitalize s in
  match s2 with
  | "H" | "Hearts"   -> Hearts
  | "D" | "Diamonds" -> Diamonds
  | "C" | "Clubs"    -> Clubs
  | "S" | "Spades"   -> Spades
  | _                -> raise InvalidString

let value_of_string s =
  let s2 = String.capitalize s in
  match s2 with
  | "2"  | "Two"   -> Two
  | "3"  | "Three" -> Three
  | "4"  | "Four"  -> Four
  | "5"  | "Five"  -> Five
  | "6"  | "Six"   -> Six
  | "7"  | "Seven" -> Seven
  | "8"  | "Eight" -> Eight
  | "9"  | "Nine"  -> Nine
  | "10" | "Ten"   -> Ten
  | "J"  | "Jack"  -> Jack
  | "Q"  | "Queen" -> Queen
  | "K"  | "King"  -> King
  | "A"  | "Ace"   -> Ace
  | _              -> raise InvalidString

let card_of_string s =
  let slist = Str.split (Str.regexp "[ \t]+") s in
  let slist_cap = List.map String.capitalize slist in
  let card_string s1 s2 =
    try (value_of_string s1),(suit_of_string s2) with
    | InvalidString -> raise InvalidString
  in
  match slist_cap with
  | h1::h2::[]     -> card_string h1 h2
  | h1::h2::h3::[] -> ( if h2 = "Of" then card_string h1 h3 
                        else raise InvalidString )
  | _              -> raise InvalidString

let suit_to_string (s: suit) = match s with
  | Hearts   -> "Hearts"
  | Diamonds -> "Diamonds"
  | Clubs    -> "Clubs"
  | Spades   -> "Spades"

let value_to_string (v: value) = match v with
  | Two   -> "2"
  | Three -> "3"
  | Four  -> "4"
  | Five  -> "5"
  | Six   -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine  -> "9"
  | Ten   -> "10"
  | Jack  -> "J"
  | Queen -> "Q"
  | King  -> "K"
  | Ace   -> "A"

let card_to_string ((v,s): card) = 
  (value_to_string v) ^ " of " ^ (suit_to_string s)

let same_suit ((_,s1): card) ((_,s2): card) = s1 = s2

let same_value ((v1,_): card) ((v2,_): card) = v1 = v2

let value_above = function
  | Two   -> Three
  | Three -> Four
  | Four  -> Five
  | Five  -> Six
  | Six   -> Seven
  | Seven -> Eight
  | Eight -> Nine
  | Nine  -> Ten
  | Ten   -> Jack
  | Jack  -> Queen
  | Queen -> King
  | King  -> Ace
  | Ace   -> Two

let suit_above = function
  | Hearts   -> Diamonds
  | Diamonds -> Clubs
  | Clubs    -> Spades
  | Spades   -> Hearts

let card_above ((v,s): card) =
  let v2 = value_above v in
  let s2 = if v = Ace then suit_above s else s in
  (v2,s2)

let one_step_above ((v1,_): card) ((v2,_): card) = v1 = (value_above v2)

let one_step_below ((v1,_): card) ((v2,_): card) = (value_above v1) = v2
