open Card

exception DeckEmpty

type deck = card list

let order_deck () =
  let start_c = card_of_string "2 H" in
  let rec next_card c l = if c = start_c then l
                          else next_card (card_above c) (c::l) in
  next_card (card_above start_c) [start_c]

let rand_deck () = 
  let arr = Array.of_list (order_deck ()) in
  let swap i j =
    let t = arr.(i) in
    arr.(i) <- arr.(j) ;
    arr.(j) <- t
  in
  (* Fisher-Yates shuffle*)
  for i = (Array.length arr) - 1 downto 1 do
    let j = Random.int i in
    swap i j
  done ;
  Array.to_list arr

let top_card (d: deck) = match d with
  | []   -> raise DeckEmpty
  | h::t -> (h,t)

let top2_cards (d: deck) = match d with
  | h1::h2::t -> (h1,h2,t)
  | _         -> raise DeckEmpty

let top3_cards (d: deck) = match d with
  | h1::h2::h3::t -> (h1,h2,h3,t)
  | _             -> raise DeckEmpty
  


