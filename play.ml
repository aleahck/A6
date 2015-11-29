open AI
open Gamestate

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

(*Takes in a game, performs an action, and returns some of a modified game or 
*None if the command isn't valid*)
let choose_action (g:game) (c:command)=
  let first= first_word command in
  match first with 
  |"repeat"-> g
  |"check"-> if (is_valid_bet 0 g) then Some (check g) else
	       None
  |"call"-> if (is_valid_bet g.bet g) then Some (call g) else 
	      None
  |"fold"-> Some (fold g)
  |"raise"->(
    let second= second_word command in
    let i= try (let num= int_of_string second in
	           if (is_valid_bet (g.bet+i)) then Some (raise_by i g) else 
		     None
	       ) with
	   |Failure "int_of_string"-> None)

  |_-> None

let play_game  (g: game)= 
  let command= read_line() in
  let new_game= choose_action g command in 
  match new_game with
    |Some gm-> print_string (string_of_game gm); play_game gm
    |None-> print_string "Invalid input"; play_game g
 
let _= 
  let new_game= Gamestate.make_game () in
  print_string "TODO";
  failwith "write stuff in the print statement above"
  play_game new_game
  
