NetIDs: xzp2, ack237, ncd29, ajd93

Project: Texas Hold'em

Compiling & Running: To compile the game, type in 'cs3110 compile -p str play.ml' in the directory containing the game files. To run the game, type in 'cs3110 run play.ml'.

Playing the Game: After running the game, the welcome message will appear. Make sure you scroll up if your terminal screen isn't large enough to fully appreciate the art! The welcome message will include instructions on how to play, which you can access again at any point in the game by entering 'rules' as a command.

Each turn, the game will print out the board (the list of cards that everyone can see), the current cumulative bet of the hand, and the cumulative pot of the hand, as well as your own stake (how much money you have), the cards you are dealt, and how much it will cost you to call (if calling will cost you 0, the game instead prints "You can check"). During your turn, you can call, raise by a number, fold, or check.  This is how we interpret raises.  Let's say the user is small blind
and the AI is big bling.  The user puts in a forced bet of 1 and the AI puts
in a forced bet of 2.  It is the user's turn to act first in this case.
If the user types "raise 5", the user will put in 6 on top of 1, for a total of 7.
The idea behind this is the user is raising the big blind of 2 by 5.  So the bet
will be 7 and the AI would need to put in 5 to call.

A raise needs to be at a minimum a raise of the previous amount.
So in the small blind big blind case from above, the raise would need to only
be 1.  In another example, if the user raised 4 as small blind, the AI would need
to raise at least 4 as well.  If the user raises by an amount that is too small,
a message "invalid input" will appear and the user will be prompted again.

In all in situations (situations when the user or the AI has put all their money in the pot) if there are remaining board cards to display they wil not be displayed
but the game will still know what they are and determine a winner.

After each turn, the AI will go (the AI's move is shown in a box with the text "AI /does some move/"), and the game will print out again, prompting you for input.