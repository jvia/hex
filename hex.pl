/* ************************************************ */
/*                                                  */
/*                      HEX                         */
/*                                                  */
/*  To play, run the query play/4 with the          */
/*  argument specifying the first player, their     */
/*  tile color, the second player and their tile    */
/*  color. Available players include:               */
/*     - human                                      */
/*     - random                                     */
/*     - greedy search                              */
/*     - minimax search                             */
/*                                                  */
/*   | ?- play(human, w, minimax, b).               */
/*                                                  */
/* ************************************************ */

%% Load board & Related code
:- [hb_test, hex_ext].
%% Load code for random player
:- use_module(library(random)).

/* ************************************************ */
/*                                                  */
/*                 Main Game Logic                  */
/*                                                  */
/* ************************************************ */

/* ************************************************ */
/*                                                  */
/*   play/{4,5}                                     */
/*      +Arg 1:   the board                         */
/*      +Arg 1,2: player 1                          */
/*      +Arg 2,3: player 1's color                  */
/*      +Arg 3,4: player 2                          */
/*      +Arg 4,5: player 2's color                  */
/*   Summary: This function starts the game.        */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - calling rule
play(P1, P1_Color, P2, P2_Color) :-
	hb(Board), !,
	play(Board, P1, P1_Color, P2, P2_Color).


%% 1 - base case: game has been won
play(Board, _, _, _, _) :-
	game_over(Board, Winner),
	print_board(Board),
	print_winner(Winner).
%% 2 - recursive: game not over
play(Board, P1, P1_Color, P2, P2_Color) :-
	\+ game_over(Board, _),
	print_board(Board, P1_Color),
	make_move(Board, P1, P1_Color), !,
	play(Board, P2, P2_Color, P1, P1_Color).


/* ************************************************ */
/*                                                  */
/*   make_move/3                                    */
/*      +Arg 1: the hex board                       */
/*      +Arg 2: the player                          */
/*      +Arg 3: the player's color                  */
/*   Summary: Puts a stone of the specified color   */
/*            on the specified tile of the game     */
/*            board.                                */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

make_move(Board, Player, Color) :-
	choose_tile(Board, Player, Color, Tile),
	arg(Tile, Board, tile(Tile, Color)).


/* ************************************************ */
/*                                                  */
/*   choose_tile/4                                  */
/*      +Arg 1: the hex board                       */
/*      +Arg 2: the player                          */
/*      +Arg 3: the player's color                  */
/*      -Arg 4: the selected tile                   */
/*   Summary: Allows the player to choose a tile to */
/*            play. This code is the front end for  */
/*            any type of player be it human or ai. */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

choose_tile(Board, human, _, Tile) :-
	repeat,
	read(Tile),
	legal(Tile, Board).
choose_tile(Board, random, _, Tile) :-
	repeat,
	random(1, 37, Tile),
	legal(Tile, Board),
	write(Tile).
choose_tile(Board, greedy, Color, Tile) :-
	minimax(1, Board, -1, Color, Tile, Value),
	write(Tile), write(' Val: '), write(Value).
choose_tile(Board, minimax, b, Tile) :-
	minimax(2, Board, -1, b, Tile, Value),
	write(Tile), write(' Val: '), write(Value).
choose_tile(Board, minimax, w, Tile) :-
	minimax(2, Board, 1, w, Tile, Value),
	write(Tile), write(' Val: '), write(Value).


/* ************************************************ */
/*                                                  */
/*   legal/2                                        */
/*      +Arg 1: tile in question                    */
/*      +Arg 2: game board                          */
/*   Summary: Determines if a tile can legally be   */
/*            played.                               */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

legal(Tile, Board) :-
	in_range(Tile),
	arg(Tile, Board, tile(_, Occupant)),
	var(Occupant).


/* ************************************************ */
/*                                                  */
/*   winning_path/2                                 */
/*      +Arg 1: the tiles                           */
/*      +Arg 2: the color                           */
/*   Summary: Determines if there is a winning path */
/*            for the color.                        */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - calling procedure
winning_path(Tiles, Color) :-
	start(Color, Tile),
	member(Tile, Tiles),
	winning_path(Tiles, Color, Tile, [Tile]).


%% 1 - base case
winning_path(Tiles, Color, Tile, [Tile|_]) :-
	end(Color, Tile),
	member(Tile, Tiles).
%% 2 - recursive
winning_path(Tiles, Color, Tile, Path) :-
	next_tile(Tile, Next_Tile),
	member(Next_Tile, Tiles),
	split(Tiles, Tile, New_Tiles),
	winning_path(New_Tiles, Color, Next_Tile, [Next_Tile|Path]).
	

/* ************************************************ */
/*                                                  */
/*   colored_tiles/{3,5}                            */
/*      +Arg 1: the board                           */
/*      +Arg 2: the color                           */
/*      -Arg 3: list of tiles occupied by color     */
/*                                                  */
/*      +Arg 1: the board                           */
/*      +Arg 2: the color                           */
/*      +Arg 3: current tile                        */
/*      +Arg 4: tile accumulator                    */
/*      -Arg 5: the colored tiles                   */
/*   Summary: Returns a list of tiles occupied by   */
/*            the given color.                      */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - calling procedure
colored_tiles(Board, Color, Tiles) :-
	colored_tiles(Board, Color, 1, [], Tiles).


%% 1 - base case
colored_tiles(_Board, _Color, Tile, Tiles, Tiles) :-
	\+ in_range(Tile).
% 2 - recursive: tile not variable & is right color
colored_tiles(Board, Color, Tile, Accm, Tiles) :-
	in_range(Tile),
	arg(Tile, Board, tile(Num, Tile_Color)),
	nonvar(Tile_Color),
	Color == Tile_Color,
	Next_Tile is Tile + 1,
	colored_tiles(Board, Color, Next_Tile, [Num|Accm], Tiles).
%% 2 - recursive: tile not variable & is wrong color
colored_tiles(Board, Color, Tile, Accm, Tiles) :-
	in_range(Tile),
	arg(Tile, Board, tile(_Num, Tile_Color)),
	nonvar(Tile_Color),
	Color \== Tile_Color,
	Next_Tile is Tile + 1,
	colored_tiles(Board, Color, Next_Tile, Accm, Tiles).
%% 3 - recursive: tile is variable	
colored_tiles(Board, Color, Tile, Accm, Tiles) :-
	in_range(Tile),
	arg(Tile, Board, tile(_Num, Tile_Color)),
	var(Tile_Color),
	Next_Tile is Tile + 1,
	colored_tiles(Board, Color, Next_Tile, Accm, Tiles).


/* ************************************************ */
/*                                                  */
/*   next_tile/2                                    */
/*      +Arg 1: the tile                            */
/*      -Arg 2: the next tile                       */
/*   Summary: Generates a next possible tile for    */
/*            constructing paths through the board. */
/*   Author: 1046411                                */
/*   Date: 27 November 2010                         */
/*                                                  */
/* ************************************************ */

next_tile(Tile, Tile1) :-
	Tile1 is Tile - 6,
	in_range(Tile1).
next_tile(Tile, Tile1) :-
	Tile1 is Tile + 6,
	in_range(Tile1).
next_tile(Tile, Tile1) :-
	cannot(sub, Num, Illegal_Tiles),
	\+ member(Tile, Illegal_Tiles),
	Tile1 is Tile - Num,
	in_range(Tile1).
next_tile(Tile, Tile1) :-
	cannot(add, Num, Illegal_Tiles),
	\+ member(Tile, Illegal_Tiles),
	Tile1 is Tile + Num,
	in_range(Tile1).


/* ************************************************ */
/*                                                  */
/*   game_over/2                                    */
/*      +Arg 1: the board                           */
/*      +Arg 2: color                               */
/*   Summary: Determines if the game is over and    */
/*            if so returns the winner.             */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

game_over(Board, b) :-
	colored_tiles(Board, b, Tiles),
	winning_path(Tiles, b).
game_over(Board, w) :-
	colored_tiles(Board, w, Tiles),
	winning_path(Tiles, w).


/* ************************************************ */
/*                                                  */
/*   in_range/1                                     */
/*      +Arg 1: the tile                            */
/*   Summary: Determines if the tile is in the      */
/*            playable range of tiles.              */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

in_range(Tile) :-
	integer(Tile),
	Tile > 0,
	Tile < 37.


/* ************************************************ */
/*                                                  */
/*                   Game Rules                     */
/*                                                  */
/* ************************************************ */


/* ************************************************ */
/*                                                  */
/*   start/2                                        */
/*      ?Arg 1: color                               */
/*      ?Arg 2: tile                                */
/*   Summary: Specifies which tiles are start tiles */
/*            for the color.                        */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

start(b, 1).         start(b, 2).        start(b, 3).
start(b, 4).         start(b, 5).        start(b, 6).
start(w, 1).         start(w, 7).        start(w, 13).
start(w, 19).        start(w, 25).       start(w, 31).


/* ************************************************ */
/*                                                  */
/*   end/2                                          */
/*      ?Arg 1: color                               */
/*      ?Arg 2: tile                                */
/*   Summary: Specifies which tiles are end tiles   */
/*            for the color.                        */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

end(b, 31).          end(b, 32).          end(b, 33).
end(b, 34).          end(b, 35).          end(b, 36).
end(w, 6).           end(w, 12).          end(w, 18).
end(w, 24).          end(w, 30).          end(w, 36).


/* ************************************************ */
/*                                                  */
/*   cannot/3                                       */
/*      +Arg 1: operator type (add/subtract)        */
/*      ?Arg 2: the amount to change by             */
/*      -Arg 3: list of illegal tiles to perform    */
/*              operator by amount on.              */
/*   Summary: Returns a list of tiles that cannot   */
/*            have the specified opration performed */
/*            on them.                              */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

cannot(add, 1, [6, 12, 18, 24, 30, 36]).
cannot(add, 5, [1, 7, 13, 19, 25, 31]).
cannot(sub, 1, [1, 7, 13, 19, 25, 31]).
cannot(sub, 5, [6, 12, 18, 24, 30, 36]).

/* ************************************************ */
/*                                                  */
/*                    Utilities                     */
/*                                                  */
/* ************************************************ */


/* ************************************************ */
/*                                                  */
/*   open_tiles/{2,4}                               */
/*      +Arg 1: the board                           */
/*      -Arg 2: the tiles                           */
/*                                                  */
/*      +Arg 1: the board                           */
/*      +Arg 2: starting tile                       */
/*      +Arg 2: accumulator                         */
/*      -Arg 2: the tiles (tile numbers only)       */
/*   Summary: Gathers and returns all of the open   */
/*            tiles into a list.                    */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - calling function
open_tiles(Board, Tiles) :-
	open_tiles(Board, 1, [], Tiles).


%% 1 - base case
open_tiles(_, Tile, Tiles, Tiles) :-
	\+ in_range(Tile).
%% 2 - recursive: tile is open
open_tiles(Board, Tile, Accm, Tiles) :-
	in_range(Tile),
	arg(Tile, Board, tile(Num, Color)),
	var(Color),
	Next_Tile is Tile + 1,
	open_tiles(Board, Next_Tile, [Num|Accm], Tiles).
%% 3 - recursive: tile is colored
open_tiles(Board, Tile, Accm, Tiles) :-
	in_range(Tile),
	arg(Tile, Board, tile(_, Color)),
	nonvar(Color),
	Next_Tile is Tile + 1,
	open_tiles(Board, Next_Tile, Accm, Tiles).

/* ************************************************ */
/*   split/3                                        */
/*      +Arg 1: the list to split                   */
/*      +Arg 2: the element to remove               */
/*      -Arg 3: the rest of the list                */
/*   Summary: Removes an element from a list.       */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/* ************************************************ */

%% 1 - base case
split([], _, []).
%% 2 - recursive: elements match
split([Head|Tail], Head, List) :-
	split(Tail, Head, List).
%% 3 - recursive: elements do not match
split([Head|Tail], Elm, [Head|List]) :-
	Head \= Elm,
	split(Tail, Elm, List).


/* ************************************************ */
/*   color/2                                        */
/*      +Arg 1: the color (b or w)                  */
/*      -Arg 2: text representation                 */
/*   Summary: Returns a string representation of    */
/*            the color.                            */
/*   Author: 1046411                                */
/*   Date: 27 November 2010                         */
/* ************************************************ */

color(b, 'Black').
color(w, 'White').


/* ************************************************ */
/*                                                  */
/*   print_winner/1                                 */
/*      +Arg 1: the winner                          */
/*   Summary: A utility function to print the       */
/*            winner of the game.                   */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

print_winner(Winner) :-
	color(Winner, Winning_Color),
	random(1, 7, X),
	msg(X, S),
	format('~2n~s ~s wins.', [S, Winning_Color]).


/* ************************************************ */
/*                                                  */
/*   print_board/{1/2}                              */
/*      +Arg 1: the hex board                       */
/*      +Arg 2: the tile color                      */
/*   Summary: Simply a utility function to otput    */
/*            the board and ask for input.          */
/*            board.                                */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

print_board(Board) :-
	nl, nl,
	display_board(Board),
	flush_output(user_output).
print_board(Board, Color) :-
	print_board(Board),
	color(Color, Color_Text),
	format('~s choose tile: ', Color_Text).


/* ************************************************ */
/*                                                  */
/*   msg/2                                          */
/*      +Arg 1: a number                            */
/*      -Arg 2: end game message                    */
/*   Summary: Used to generate a random end-game    */
/*            message to make the game more fun.    */
/*            board.                                */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

msg(1, 'Close call, but').
msg(2, 'Well played,').
msg(3, 'Congratulations,').
msg(4, 'Annihalation,').
msg(5, 'That was a slaughter,').
msg(6, 'Pwnt. n00b l0lz.').


/* ************************************************ */
/*                                                  */
/*   next_color                                     */
/*      +Arg 1: current color                       */
/*      -Arg 2: next coloressage                    */
/*   Summary: Gives the next color that will play.  */
/*   Author: 1046411                                */
/*   Date: 25 November 2010                         */
/*                                                  */
/* ************************************************ */

next_color(w, b).
next_color(b, w).


/* ************************************************ */
/*                                                  */
/*   empty_tile/2                                   */
/*      +Arg 1: board                               */
/*      +Arg 2: tile number                         */
/*   Summary: Determines if the tile is empty.      */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

empty_tile(Board, Tile_Num) :-
	arg(Tile_Num, Board, tile(Tile_Num, V)),
	var(V).


/* ************************************************ */
/*                                                  */
/*   black_tile/2                                   */
/*      +Arg 1: board                               */
/*      +Arg 2: tile number                         */
/*   Summary: Determines if the tile is black.      */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

black_tile(Board, Tile_Num) :-
	arg(Tile_Num, Board, tile(Tile_Num, V)),
	nonvar(V),
	V = b.


/* ************************************************ */
/*                                                  */
/*   white_tile/2                                   */
/*      +Arg 1: board                               */
/*      +Arg 2: tile number                         */
/*   Summary: determines if the tile is white.      */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

white_tile(HR, Tile_Num) :-
	arg(Tile_Num, HR, tile(Tile_Num, V)),
	nonvar(V),
	V = w.


/* ************************************************ */
/*                  End of program                  */
/* ************************************************ */
