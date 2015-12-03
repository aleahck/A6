open AI
open Gamestate


(*gamestage represents the parts of a hand as determined by the number of cards
*on the table. Initial will be before any cards are out, Flop will be when 3
*cards are out, Turn when 4 and River when 5.*)
type gamestage= Initial | Flop | Turn | River

(*[game_stage g] takes in a game [g] and returns a gamestage determined by how
*many cards have are in [g.flop]*)
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
	       |Not_found-> failwith "nope" in
  let untrimmed= String.sub lower_trimmed space
			    ((String.length lower_trimmed)-space)in
  String.trim untrimmed


(*[choose_action g] will perform a single player move in a round of betting.
*The round will continue until someone calls, or two people check.*)
let rec choose_action (g:game)=
  print_string (game_to_string g);
  if (end_betting g) then (print_string
			      "\nThis round of betting has concluded because someone has no money\n";g)(*shorten*)
  else
  (print_string "Enter a command:\n";
  let command= read_line () in
  let first= first_word command in
  let second= try (second_word command) with
	      |Failure "nope"-> "" in
  match g.last_move with
    |Call->failwith "Should have been caught in if"
    |Check->begin match first with
		  |"check"->
		    print_string ("\nThis round of betting has concluded because two people checked\n");check g(*shorten*)
		  |"raise"-> print_string "check followed by raise"; let raised= try play_raise g second with
			       |Failure "int_of_string"->
				 (print_string "\n\n\n Invalid input\n";g) in
			     choose_action raised
		  |"fold"-> print_string "check then fold";fold g
		  |"exit"-> exit 0
	          |_-> print_string "\n\n\n Invalid input\n";
		       choose_action g end
    |Raise _-> begin match first with
		  |"raise"-> print_string "raise followed by raise";let raised= try play_raise g second with
			       |Failure "int_of_string"->
				 print_string "\n\n\n Invalid input\n";
							 g in
			     choose_action (turn (raised))
		  |"call"-> print_string
			      "\nThis round of betting has concluded because the player called\n\n";
			    call g
		  |"fold"-> print_string "raise followed by fold"; fold g
		  |"exit"-> exit 0
		  |_->
		    (print_string "\n\n\n Invalid input\n"; choose_action g) end
    |Fold-> failwith "a new hand should have started from AI"
    |Deal-> begin match first with
		  |"raise"-> print_string "first raise";let raised= try play_raise g second with
			       |Failure "int_of_string"->
				 print_string "\n\n\nInvalid input\n"; g in
			     choose_action raised
		  |"check"-> print_string "first check";choose_action (turn (check g))
		  |"fold"-> print_string "first fold";fold g
		  |"exit"-> exit 0
	          |_->
		    (print_string "\n\n\n Invalid input\n"; choose_action g)end)
and play_raise g second= let num= int_of_string second in
			     if (is_valid_raise num g)
			     then choose_action (turn (do_raise g num))
					 else
					   (print_string
					      "\n\n\n Invalid input\n"; g)

(*[play_game g] takes in a game [g] and deals cards in a hand, begins rounds
*of betting, and launches new hands when appropriate. play_game will terminate
*when someone wins or exits*)
let rec play_game  (g: game)=
  match game_stage g with
  |Initial-> print_string "IN INITIAL " ;
  let betting= if (fst (List.hd (g.players))= "You")
	       then choose_action g
	       else choose_action (turn g) in
	         play_game (add3_flop betting)
  |Flop|Turn-> print_string "IN FLOP OR TURN " ;
  	       let betting= if (fst (List.hd (g.players))= "You")
	       then choose_action g
	       else choose_action (turn g) in
	       play_game (add1_flop betting)
  |River-> print_string "IN RIVER " ;
	   let betting= if (fst (List.hd (g.players))= "You")
			then choose_action g
			else choose_action (turn g) in
	   let the_winner= fst (winner g) in
	   print_string (winner_to_string betting);
	   let new_ps= if (the_winner= get_current_id betting)
		       then List.rev betting.players
		       else betting.players in
	   let ggame= {betting with players= new_ps} in
	   play_game (fold ggame)


(*The main function launches the game, creates a new game, and initializes the
*first hand*)
let _=
  print_string ("\nWelcome to TEXAS HOLD'EM! The game has begun.\n"^
	       "You can call,raise by a number, fold, or check "^
		"when it's your turn.\nType exit to quit the game.\n");
  let new_game= make_game () in
  let new_h= fold new_game in
  play_game new_h

