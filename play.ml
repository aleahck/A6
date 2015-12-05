open AI
open Gamestate

(*[game_stage g] takes in a game [g] and returns a gamestage determined by how
*many cards have are in [g.flop]*)
let game_stage g =
    match g.flop with
    | a::b::c::d::e::[] -> River
    | a::b::c::d::[] -> Turn
    | a::b::c::[] -> Flop
    | _ -> Initial

(*Takes in a command as a string and returns a string containing only the
*characters starting with the first non-space character and concluding with
*the character before the next space, or the last character.*)
let first_word command =
  let lower_trimmed = String.trim (String.lowercase command) in
  let space = try String.index lower_trimmed ' ' with
              | Not_found -> String.length lower_trimmed in
  String.sub lower_trimmed 0 space

(*Takes in a command as a string and returns a string containing only the
*content after the first space without leading and proceeding spaces*)
let second_word command =
  let lower_trimmed = String.trim (String.lowercase command) in
  let space = try String.index lower_trimmed ' ' with
              | Not_found -> failwith "nope" in
	let index = (String.length lower_trimmed) - space in
  let untrimmed = String.sub lower_trimmed space index in
  String.trim untrimmed

(* prints the input was invalid *)
let print_invalid () = print_string "\n\n\n Invalid input\n"

(* prints the rules *)
let print_rules () = 
  print_string("\nRULES:\nYou can CALL, RAISE a number (i.e."^
               "raise 20), FOLD, or CHECK during your turn.\nType RULES"^
               " to see the rules again.\nType EXIT to quit the game.\n\n")  

(*[play_raise g second]  if the integer form of [second] is a valid raise
*for the current player of [g], [play_raise g second] will raise by that 
*int. If second is not a number or is not a valid raise, [play_raise g second]
* will prompt the player for a new command and return [g]*)
let play_raise g second =
  let num = int_of_string second in
  if (is_valid_raise num g) then turn (do_raise g num) 
  else (print_invalid() ; g)

(*[check_no_snd g s f] will apply [f] to game [g] if [s] is empty and
* prompt the user for a new command and return [g] otherwise*)
let check_no_snd g s f = 
  if s = "" then f g else (print_invalid() ; g) 
		
(*[choose_action g] will perform a single player move in a round of betting.
*The round will continue until someone calls, or two people check.*)
let rec choose_action (g:game)=
  if end_betting g then 
    {g with players = List.rev g.players}
  else (
     print_string (game_to_string g) ;
     print_string "\n\nEnter a command:\n" ;
     let command = read_line () in
     let first = first_word command in
     let second = try (second_word command) with
                  | Failure "nope" -> "" in
     match g.last_move, first with
     | _, "exit" -> exit 0
     | _, "rules" -> check_no_snd g second ( fun x -> print_rules() ; x)
     | Call, _ -> failwith "Should have been caught in if"
     | Check, "check" -> check_no_snd g second check
     | Check, "raise" -> ( let raised = try play_raise g second with
                                        | Failure "int_of_string" -> 
                                            ( print_invalid() ; g) in
			                     choose_action raised )
     | Check, "fold" -> check_no_snd g second fold 
     | Raise _, "raise" -> ( let raised = try play_raise g second with
                                          | Failure "int_of_string" ->
                                              ( print_invalid() ; g) in
			                       choose_action raised )
     | Raise _, "call" ->  check_no_snd g second call
     | Raise _, "fold" ->  check_no_snd g second fold
     | Fold, _ -> failwith "a new hand should have started from AI"
     | Deal, "call" -> ( let f g = if is_valid_call g then
                                     let ai_went = turn (call g) in
                                     if ai_went.last_move = Check then ai_went
                                     else choose_action ai_went
                                   else
                                     ( print_invalid() ; g)
                         in check_no_snd g second f )               
     | Deal, "raise" -> ( let raised = try play_raise g second with
                                       | Failure "int_of_string" -> 
                                           (print_invalid() ; g) in
                          if raised.last_move = Deal then raised
                          else choose_action raised )
     | Deal, "check" -> ( let f g = if is_valid_check g then 
                                      let checked = turn(check g) in
                                      if checked.last_move = Check then checked
                                      else choose_action (checked)
                                    else 
                                      ( print_invalid() ; g )
                          in check_no_snd g second f )
     | Deal, "fold" -> check_no_snd g second fold
     | _ -> (print_invalid() ; choose_action g) )


(*[play_game g] takes in a game [g] and deals cards in a hand, begins rounds
*of betting, and launches new hands when appropriate. play_game will terminate
*when someone wins or exits*)
let rec play_game  (g: game)=
  match game_stage g with
	| Initial -> ( print_string "\nIn INITIAL\n";print_string "\nNEW ROUND OF BETTING\n";
                 let betting1 =
                   if get_current_id g = "You" then
                     let betting = choose_action g in
                     if betting.last_move = Deal then betting
                     else add3_flop betting
                   else
                     let betting = turn g in
                     if betting.last_move = Deal then 
                       betting
                     else 
                       let betting2 =
                         if betting.last_move = Call then 
                           choose_action {betting with last_move = Check}
                         else 
                           choose_action betting in
                       if betting2.last_move = Deal then betting2
                       else add3_flop betting2 in
                 play_game betting1 )
  | Flop | Turn -> ( print_string "\nIn FLOP or TURN\n";print_string "\nNEW ROUND OF BETTING\n";
                     let betting1 =
                       if get_current_id g = "You" then 
                         let betting = choose_action g in
                         if betting.last_move = Deal
                            && not (end_betting betting)
                         then betting
                         else add1_flop betting
                       else  
                         let betting = turn g in
                         if betting.last_move = Deal then
                           betting
                         else 
                           let betting2 = choose_action betting in
                           if betting2.last_move = Deal then betting2
                           else add1_flop betting2 in
                     play_game betting1 )
  | River -> (print_string "\n In RIVER\n";
              print_string "\nNEW ROUND OF BETTING\n" ;
               let betting1 = 
                 if get_current_id g = "You" then
                   choose_action g 
                 else 
                   let betting = turn g in
                   if betting.last_move = Deal then betting
                   else choose_action (betting) in
               let ggame =
                 if betting1.last_move = Deal then
                   betting1
                 else 
                   let the_winner,_ = winner betting1 in
                   print_string (winner_to_string betting1) ;
                   let new_ps = 
                     if the_winner = get_current_id betting1 then
                       List.rev betting1.players
                     else
                       betting1.players in
                   fold {betting1 with players = new_ps} in
               play_game ggame )


(*The main function launches the game, creates a new game, and initializes the
*first hand*)
let _ =
  print_string("
                       _..-''--'----_.
                     ,''.-''| .---/ _`-._
                   ,' \\ \\  ;| | ,/ / `-._`-.
                 ,' ,',\\ \\( | |// /,-._  / /
                 ;.`. `,\\ \\`| |/ / |   )/ /
                / /`_`.\\_\\ \\| /_.-.'-''/ /
               / /_|_:.`. \\ |;'`..')  / /
               `-._`-._`.`.;`.\\  ,'  / /
                   `-._`.`/    ,'-._/ /
                     : `-/     \\`-.._/
                     |  :      ;._ (
                     :  |      \\  ` \\
                      \\         \\   |
                       :        :   ;
                       |           /
                       ;         ,'
                      /         /
                     /         /
                              / ");
  print_string (
  "\n ____  ____  _  _    __    ___    _   _  _____  __    ____/ ____  __  __" ^
  "\n(_  _)( ___)( \\/ )  /__\\  / __)  ( )_( )(  _  )(  )  (  _ \\( ___)(  \\/  )"^
  "\n  )(   )__)  )  (  /(__)\\ \\__ \\   ) _ (  )(_)(  )(__  )(_) ))__)  )    ( "^
  "\n (__) (____)(_/\\_)(__)(__)(___/  (_) (_)(_____)(____)(____/(____)(_/\\/\\_)"
  );
	print_string ("\n\nWelcome to TEXAS HOLD'EM! The game has begun.\n") ;
	print_rules() ;
  let new_game = make_game () in
  let new_h = fold new_game in
  play_game new_h

