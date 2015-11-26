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

let playGame (input:Sys.argv) (g: game)= 
  let command= read_line() in
  let new_game= failwith "TODO" in 
  let first= first_word command in
  if (first= "") then failwith "TODO" else (
  if (first= "") then failwith "TODO" else(
  if (first= "") then failwith "TODO" else (
  let second= second_word command in
  failwith "TODO"
  )))
  
  
