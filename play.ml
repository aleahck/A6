open AI
open Gamestate

type validity= Valid of game| Invalid

(*Takes in a command as a string and returns a string containing only the 
*first word.*)
let first_word command=
  let lower_trimmed= String.trim (String.lowercase command) in
  let space= try String.index lower_trimmed ' ' with
	       |Not_found-> String.length lower_trimmed in
  String.sub lower_trimmed 0 space

(*Takes in a command as a string and returns a string containing only the 
*content after the first space*)
let second_word command= 
  let lower_trimmed= String.trim (String.lowercase command) in
  let space= try String.index lower_trimmed ' ' with 
	       |Not_found-> failwith "No second word should be valid" in
  let untrimmed= String.sub lower_trimmed space 
			    ((String.length lower_trimmed)-space)in
  String.trim untrimmed

let deal_flop g= 
  let one_added= add1_flop g in
  if (List.length g.flop > 5) 
  then new_hand final_bet  
  else(
    if ((List.length g.flop)=0) 
    then add1_flop (add1_flop one_added) 
    else one_added)
  
(*Takes in a game, performs an action, and returns some of a modified game or 
*None if the command isn't valid*)
let choose_action (g:game) (c:command)=
  let first= first_word command in
  let delt= deal_flop g in
  match first with
  |"exit"-> exit 0;
  |"repeat"-> Valid delt
  |"check"-> if (is_valid_bet 0 delt) then Valid (check delt) else
	       Invalid
  |"call"-> if (is_valid_bet delt.bet delt) then Valid (call delt) else 
	      Invalid
  |"fold"-> Valid (fold delt)
  |"raise"->(
    let second= second_word command in
    let i= try (let num= int_of_string second in
	           if (is_valid_bet (delt.bet+i)) then 
		     Valid (raise_by i delt) else
		     Invalid
	       ) with
	   |Failure "int_of_string"-> Invalid)

  |_-> Invalid

let play_game  (g: game)= 
  let keep_playing= List.for_all (fun x-> ((snd x).stake)> 0) g.players in
  if keep_playing then (
  let command= read_line() in
  let new_game= choose_action g command in 
  match new_game with
    |Valid gm-> print_string (string_of_game gm); play_game gm
    |Invalid-> print_string "Invalid input"; play_game g)
  else (let this_player= snd (List.hd (g.players)) in
       if this_player.stake > 0 then print_string 
				       "You win!\n" exit 0;
       else print_string "You lose!\n" exit 0;
       )	 
 
let _= 
  let new_game= Gamestate.make_game () in
  print_string "TODO";
  failwith "write stuff in the print statement above"
  play_game new_game
  
