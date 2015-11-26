#load "str.cma"
type suit = Hearts | Diamonds | Clubs | Spades

type value = Ace | King | Queen | Jack | Ten | Nine | Eight
                 | Seven | Six | Five | Four | Three | Two

type card = value * suit

let suit_of_card (c: card) = snd c

let val_of_card (c: card) = fst c

let suit_of_string s =
  let s2 = String.capitalize s in
  match s2 with
  | "H" | "Hearts"   -> Some Hearts
  | "D" | "Diamonds" -> Some Diamonds
  | "C" | "Clubs"    -> Some Clubs
  | "S" | "Spades"   -> Some Spades
  | _                -> None

let value_of_string s =
  let s2 = String.capitalize s in
  match s2 with
  | "2"  | "Two"   -> Some Two
  | "3"  | "Three" -> Some Three
  | "4"  | "Four"  -> Some Four
  | "5"  | "Five"  -> Some Five
  | "6"  | "Six"   -> Some Six
  | "7"  | "Seven" -> Some Seven
  | "8"  | "Eight" -> Some Eight
  | "9"  | "Nine"  -> Some Nine
  | "10" | "Ten"   -> Some Ten
  | "J"  | "Jack"  -> Some Jack
  | "Q"  | "Queen" -> Some Queen
  | "K"  | "King"  -> Some King
  | "A"  | "Ace"   -> Some Ace
  | _              -> None

let card_of_string s =
  let slist = Str.split (Str.regexp "[ \t]+") (String.capitalize s) in
  let card_string s1 s2 =
    match (value_of_string s1),(suit_of_string s2) with
    | Some v1, Some v2 -> Some (v1,v2)
    | _                -> None
  in
  match slist with
  | h1::h2::[]     -> card_string h1 h2
  | h1::h2::h3::[] -> if h2 = "Of" then card_string h1 h3 else None
  | _              -> None

let suit_to_string = function
  | Hearts   -> "Hearts"
  | Diamonds -> "Diamonds"
  | Clubs    -> "Clubs"
  | Spades   -> "Spades"

let value_to_string = function
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

let card_to_string (v,s) = (value_to_string v) ^ " of " ^ (suit_to_string s)

let same_suit (_,s1) (_,s2) = s1 = s2

let same_value (v1,_) (v2,_) = v1 = v2

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

let card_above (v,s) =
  let v2 = value_above v in
  let s2 = if v = Ace then suit_above s else s in
  (v2,s2)

let one_step_above (v1,_) (v2,_) = v1 = (value_above v2)

let one_step_below (v1,_) (v2,_) = (value_above v1) = v2
