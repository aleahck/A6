open Card

exception DeckEmpty

type deck = card list

let order_deck () =
  match card_of_string "2 H" with
  | Some start_c -> ( let rec next_card c l =
                        if c = start_c then l
                        else next_card (card_above c) (c::l)
                      in
                      next_card (card_above start_c) [start_c] )
  | None         -> []

let rand_deck () = [] (* TODO *)

let top_card = function
  | []   -> raise DeckEmpty
  | h::t -> (h,t)

let top2_cards = function
  | h1::h2::t -> (h1,h2,t)
  | _         -> raise DeckEmpty

let top3_cards = function
  | h1::h2::h3::t -> (h1,h2,h3,t)
  | _             -> raise DeckEmpty
  


