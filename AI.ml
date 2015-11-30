open Gamestate
open Gamelogic
open Card

type gamestage = Initial | Flop | Turn | River

let gamestage_for_list = function
  | h1::h2::h3::h4::h5::[] -> River
  | h1::h2::h3::h4::[]     -> Turn
  | h1::h2::h3::[]         -> Flop
  | _                      -> Initial

let same_suit_bonus c1 c2 = if same_suit c1 c2 then 10 else 0

let card_bonus c = match val_of_card c with
  | "2"  -> 0
  | "3"  -> 1
  | "4"  -> 2
  | "5"  -> 3
  | "6"  -> 4
  | "7"  -> 5
  | "8"  -> 6
  | "9"  -> 7
  | "10" -> 8
  | "J"  -> 9
  | "Q"  -> 10
  | "K"  -> 11
  | "A"  -> 12

let best_hand_bonus = function
  | HighCard _ -> 0
  | Pair _     -> 10
  | TwoPair _  -> 20
  | Triple _   -> 30
  | Straight _ -> 40
  | Flush _    -> 50
  | FullHouse _ -> 60
  | Quads _    -> 70
  | StraightFlush _ -> 80
  | RoyalFlush _ -> 90

let turn g = 
