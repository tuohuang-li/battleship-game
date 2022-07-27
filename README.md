# simplified_battleship
comp90048_Declarative Programming

Author:   Tuohuang Li <tuohuangl@unimelb.edu.au>
Student ID: 1205166

Purpose:  Play a two player guessing game - Battleship Game on a 4×8 grid. 
   
-------------------------------The Game------------------------------

Proj2 is a simple two-player logical guessing game created for this project.

The game is somewhat akin to the game of Battleship™, but somewhat simplified. 
The game is played on a 4×8 grid, and involves one player, the searcher trying
to find the locations of three battleships hidden by the other player, the hider. 
The searcher continues to guess until they find all the hidden ships. 
Unlike Battleship™, a guess consists of three different locations, and the game 
continues until the exact locations of the three hidden ships are guessed in 
a single guess. 

The searcher will repeatedly trying to find the locations of three battleships 
according to the feedback provided by the hider. The feedback is a set of 3 
numbers indicates how many guesses out of three either hit the hidden battleship 
or is 1 distance away or 2 distance away from the hidden locations. The game
continues the above steps until the searcher successfully found all 3 hidden ballteship.

The eight squares adjacent to a square, including diagonally adjacent, are counted as 
distance 1 away. The sixteen squares adjacent to those squares are considered to be 
distance 2 away, as illustrated in this diagram of distances from the center square:
|   2   |   2   |   2   |   2   |   2   |
| :---: | :---: | :---: | :---: | :---: |
|   2   |   1   |   1   |   1   |   2   |
|   2   |   1   |   0   |   1   |   2   |
|   2   |   1   |   1   |   1   |   2   |
|   2   |   2   |   2   |   2   |   2   |

Here are some example ship locations, guesses, and the feedback provided by the hider:


| Locations	| Guess | Feedback |
| :---: | :---: | :---: |
| H1, B2, D3 | B3, C3, H3	|0, 2, 1|
| H1, B2, D3 | B1, A2, H3	|0, 2, 1|
| H1, B2, D3 | B2, H2, H1	|2, 1, 0|
| A1, D2, B3 | A3, D2, H1	|1, 1, 0|
| A1, D2, B3 | H4, G3, H2	|0, 0, 0|
| A1, D2, B3 | D2, B3, A1	|3, 0, 0|

Here is a graphical depiction of the first example above, where ships are shown 
as S and guessed locations are shown as G:

| |A|B|C|D|E|F|G|H|
| :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
|1|	| |	| |	| |	|S|	 	 	 	
|2|	|S|	| |	| |	| |	 	 	 
|3|	|G|G|S|	| |	|G|
|4|	| |	| |	| |	| | 	 	 	 	 



