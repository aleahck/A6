open AI
open Gamestate

type validity= Valid of game| Invalid

type gamestage= Initial | Flop | Turn | River 

let game_stage g= match g.flop with 
    |a::b::c::d::e::[]-> River
    |a::b::c::d::[]-> Turn
    |a::b::c::[]-> Flop
    |_-> Initial

(*Takes in a command as a string and returns a string containing only the 
*characters starting with the first non-space character and concluding with 
*the character before the next space, or the last character.*)
let first_word command=
  let lower_trimmed= String.trim (String.lowercase command) in
  let space= try String.index lower_trimmed ' ' with
	       |Not_found-> String.length lower_trimmed in
  String.sub lower_trimmed 0 space

(*Takes in a command as a string and returns a string containing only the 
*content after the first space without leading and proceeding spaces*)
let second_word command= 
  let lower_trimmed= String.trim (String.lowercase command) in
  let space= try String.index lower_trimmed ' ' with 
	       |Not_found-> failwith "No second word should be valid" in
  let untrimmed= String.sub lower_trimmed space 
			    ((String.length lower_trimmed)-space)in
  String.trim untrimmed

  
(*Takes in a game, performs an action, and returns some of a modified game or 
*None if the command isn't valid*)
let rec choose_action (g:game)=
  print_string (game_to_string g);
  if (out_of_money g) then g  
  else
  (print_string "Enter a command";
  let command= read_line () in
  let first= first_word command in
  let second= second_word command in
  match g.last_move with 
    |Call-> print_string "This round of betting has concluded\n";g 
    |Check->begin match first with
		  |"check"-> check g
		  |"raise"-> let raised= try play_raise g second with
			       |Failure "int_of_string"->print_string
							   "Invalid input"; g in
			     choose_action raised
		  |"fold"-> fold g
		  |"exit"-> exit 0
	          |_-> print_string "Invalid input"; choose_action g end
    |Raise _-> begin match first with
		  |"raise"-> let raised= try play_raise g second with
			       |Failure "int_of_string"->print_string
							   "Invalid input"; g in
			     choose_action raised
		  |"call"-> print_string 
			      "This round of betting has concluded\n";g 
		  |"fold"-> fold g
		  |"exit"-> exit 0
		  |_-> (print_string "Invalid input"; choose_action g) end
    |Fold-> failwith "a new hand should have started from AI"
    |Deal-> begin match first with
		  |"raise"-> let raised= try play_raise g second with
			       |Failure "int_of_string"->print_string
							   "Invalid input"; g in
			     choose_action raised
		  |"check"-> choose_action (turn (check g))
		  |"fold"-> fold g
		  |"exit"-> exit 0 
	          |_-> (print_string "Invalid input"; choose_action g )end)
and play_raise g second= let num= int_of_string second in
			     if (is_valid_raise num g) 
			     then choose_action (turn (do_raise g num))
					 else (print_string "Invalid input"; g)

let rec play_game  (g: game)= 
  match game_stage g with
  |Initial-> let new_h= fold g in
	     let betting=choose_action new_h in
	     play_game (add3_flop betting)
  |Flop|Turn-> let betting= choose_action g in
	       play_game (add1_flop betting)
  |River-> let betting= choose_action g in
	   let the_winner= fst (winner g) in
	   print_string (winner_to_string g);
	   let new_ps= if (the_winner= get_current_id g)
		       then List.rev g.players
		       else g.players in
	   let ggame= {betting with players= new_ps} in
	   play_game (fold ggame)
	   	 
 
let _= 
  let new_game= make_game () in
  let new_h= fold new_game in 
  print_string "TODO";
  play_game new_h
  
