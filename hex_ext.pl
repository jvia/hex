/* ************************************************ */
/*                                                  */
/*  Extensions to the baic Hex game. This is where  */
/*  the AI players are defined.                     */
/*                                                  */
/*  The board evaluation technique was based on a   */
/*  paper by Vadim V. Anshelevich titled 'The Game  */
/*  of Hex: An Automatic Theorem Proving Approach   */
/*  to Game Programming'.                           */
/*                                                  */
/*  Furthermore, the search tehcniques are inspired */
/*  by the search implemented in the Art of Prolog. */
/*                                                  */
/* ************************************************ */

:- use_module(library(lists)).

/* ************************************************ */
/*                                                  */
/*   hr/1                                           */
/*      -Arg 1: Hex resistance structure            */
/*   Summary: Maintains the resistance values for   */
/*            the board from black's and white's    */
/*            points of view.                       */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

hr(hex_resitance(tile(1, _, _), tile(2, _, _), tile(3, _, _),
		 tile(4, _, _), tile(5, _, _), tile(6, _, _),
		 tile(7, _, _), tile(8, _, _), tile(9, _, _),
		 tile(10, _, _),tile(11, _, _),tile(12, _, _),
		 tile(13, _, _),tile(14, _, _),tile(15, _, _),
		 tile(16, _, _),tile(17, _, _),tile(18, _, _),
		 tile(19, _, _),tile(20, _, _),tile(21, _, _),
		 tile(22, _, _),tile(23, _, _),tile(24, _, _),
		 tile(25, _, _),tile(26, _, _),tile(27, _, _),
		 tile(28, _, _),tile(29, _, _),tile(30, _, _),
		 tile(31, _, _),tile(32, _, _),tile(33, _, _),
		 tile(34, _, _),tile(35, _, _),tile(36, _, _))).


/* ************************************************ */
/*                                                  */
/*   get_hr/4                                       */
/*      +Arg 1: the hex resistance structure        */
/*      +Arg 2: tile number                         */
/*      +Arg 2: the color                           */
/*      -Arg 2: the resistane value                 */
/*   Summary: Gets the resistance value for the     */
/*            color and tile.                       */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

get_hr(HR, Tile_Num, b, Resistance) :-
	arg(Tile_Num, HR, Tile),
	arg(2, Tile, Resistance).
get_hr(HR, Tile_Num, w, Resistance) :-
	arg(Tile_Num, HR, Tile),
	arg(3, Tile, Resistance).


/* ************************************************ */
/*                                                  */
/*   resistance/5                                   */
/*      +Arg 1: the board                           */
/*      +Arg 2: tile 1                              */
/*      +Arg 3: tile 2                              */
/*      +Arg 4: color                               */
/*      -Arg 5: the resistance value                */
/*   Summary: Calculates the resistance value for   */
/*            two neighboring tiles. The tiles must */
/*            be direct neghbors.                   */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

resistance(Board, T1, T2, Color, Resistance) :-
	get_hr(Board, T1, Color,  V1),
	get_hr(Board, T2, Color, V2),
	Resistance is V1 +V2.


/* ************************************************ */
/*                                                  */
/*   update_resistance/3                            */
/*      +Arg 1: untouched resistance structure      */
/*      +Arg 2: the board                           */
/*      -Arg 2: updated structure                   */
/*   Summary: Calculates the resistance values for  */
/*            each tile in the board and places it  */
/*            into a new sturcture.                 */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - calling rule
update_resistance(HR, Board, HR1) :-
	copy_term(HR, HR1),
	update_resistance(HR1, Board, 1).

%% 1 - base case
update_resistance(_, _, Tile_Num) :-
	\+ in_range(Tile_Num).
%% 2 - recursive: tile is empty
update_resistance(HR, Board, Tile_Num) :-
	in_range(Tile_Num),
	empty_tile(Board, Tile_Num),
	get_hr(HR, Tile_Num, b, 1),
	get_hr(HR, Tile_Num, w, 1),
	Tile_Num1 is Tile_Num + 1,
	update_resistance(HR, Board, Tile_Num1).
%% 3 - recursive: tile is black
update_resistance(HR, Board, Tile_Num) :-
	in_range(Tile_Num),
	black_tile(Board, Tile_Num),
	get_hr(HR, Tile_Num, b, 0),
	get_hr(HR, Tile_Num, w, 100000),
	Tile_Num1 is Tile_Num + 1,
	update_resistance(HR, Board, Tile_Num1).
%% 4 - recursive: tile is white
update_resistance(HR, Board, Tile_Num) :-
	in_range(Tile_Num),
	white_tile(Board, Tile_Num),
	get_hr(HR, Tile_Num, b, 100000),
	get_hr(HR, Tile_Num, w, 0),
	Tile_Num1 is Tile_Num + 1,
	update_resistance(HR, Board, Tile_Num1).


/* ************************************************ */
/*                                                  */
/*   nr_list/{4,6}                                  */
/*      +Arg 1: hex resistances                     */
/*      +Arg 2: tile                                */
/*      +Arg 3: color                               */
/*      -Arg 4: list (Value-Neighbor)               */
/*   Summary: Returns a list of neighbors and their */
/*            associated values.                    */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - calling rule
nr_list(HR, Tile, Color, List) :-
	bagof(Neighbors, next_tile(Tile, Neighbors), Neighbors),
	nr_list(Neighbors, Tile, HR, Color, [], List).


%% 1 - base case
nr_list([], _,_,_,L,L).
%% 2 - resurvie
nr_list([N|NS], Tile, HR, Color, Accm, List) :-
	resistance(HR, Tile, N, Color, Val),
	nr_list(NS, Tile, HR, Color, [Val-N|Accm], List).


/* ************************************************ */
/*                                                  */
/*   neighbors/4                                    */
/*      +Arg 1: hex resistances                     */
/*      +Arg 2: tile number                         */
/*      +Arg 3: color                               */
/*      -Arg 4: sorted list of neighbors            */
/*   Summary: Returns a sorted list of neighbors.   */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

neighbors(HR, Tile, Color, Neighbors) :-
	nr_list(HR, Tile, Color, List),
	keysort(List, Neighbors).


/* ************************************************ */
/*                                                  */
/*   llr_path/{3,5}                                 */
/*      +Arg 1: hex resistances                     */
/*      +Arg 2: the color                           */
/*      -Arg 3: the value                           */
/*   Summary: Calculates the least resistant path   */
/*            from any of the colors starting tile. */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

llr_path(HR, Color, Value) :-
	bagof(Start, start(Color, Start), Start),
	llr_path(Start, HR, Color, 100000000, Value).

llr_path([], _, _, Value, Value).
llr_path([T|TS], HR, Color, BestSoFar, Value) :-
	lr_path(HR, T, Color, Val),
	update_best(Val, BestSoFar, New_Best),
	llr_path(TS, HR, Color, New_Best, Value).


/* ************************************************ */
/*                                                  */
/*   update_best/3                                  */
/*      +Arg 1: value 1                             */
/*      +Arg 2: value 2                             */
/*      -Arg 2: best value                          */
/*   Summary: Deermines the better value.           */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

update_best(V1, V2, V1) :-
	V1 < V2.
update_best(V1, V2, V2) :-
	V1 >= V2.


/* ************************************************ */
/*                                                  */
/*   lr_path/3                                      */
/*      +Arg 1: hex resistances                     */
/*      +Arg 2: the color                           */
/*      -Arg 2: the value                           */
/*   Summary: Calculates the least resistant path   */
/*            from a given tile to an end tile.     */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

lr_path(HR, Color, Value) :-
	start(Color, Start),
	lr_path(HR, Start, Color, [], 0, Value).

lr_path(HR, Tile, Color, Value) :-
	lr_path(HR, Tile, Color, [], 0, Value).

lr_path(_, Tile, Color, _, Value, Value) :-
	end(Color, Tile).
lr_path(HR, Tile, Color, History, Accm, Value) :-
	\+ end(Color, Tile),
	neighbors(HR, Tile, Color, Neighbors),
	member(Val-Neighbor, Neighbors),
	\+ member(Neighbor, History),
	New_Accm is Accm + Val,
	lr_path(HR, Neighbor, Color, [Tile|History], New_Accm, Value).


/* ************************************************ */
/*                                                  */
/*   update_low_value/6                             */
/*      +Arg 1: value 1                             */
/*      +Arg 2: neighbor 1                          */
/*      +Arg 3: value 2                             */
/*      +Arg 4: neighbor 2                          */
/*      -Arg 5: best value                          */
/*      -Arg 6: best neighbor                       */
/*   Summary: Returns the lowest-valued neighbor.   */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

update_low_val(V1, N1, V2, _, V1, N1) :-
	V1 < V2.
update_low_val(V1, _, V2, N2, V2, N2) :-
	V1 >= V2.
	
	
/* ************************************************ */
/*                                                  */
/*   evaluate/2                                     */
/*      ?Arg 1: the board                           */
/*      -Arg 2: the score                           */
/*   Summary: Evaluates the board based on:         */
/*            E = R_b / R_w                         */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

evaluate(Board, Value) :-
	hr(R),
	update_resistance(R, Board, HR),
	llr_path(HR, b, B),
	llr_path(HR, w, W),
	%% If white wins, W will be 0, so we need
	%% a divide by 0 error
	Value is B / (W + 0.1). 


/* ************************************************ */
/*                                                  */
/*   evaluate_and_choose/7                          */
/*      +Arg 1: list of open tile                   */
/*      +Arg 2: the board                           */
/*      +Arg 3: the color                           */
/*      +Arg 4: depth                               */
/*      +Arg 5: minimax multiplier                  */
/*      +Arg 6: best so far                         */
/*      -Arg 7: the best                            */
/*   Summary: Part of the minimax algorithm. Based  */
/*            off of the example in the Art of      */
/*            Prolog.                               */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - base case
evaluate_and_choose([], _, _, _, _, Best, Best).
%% 2 - recursive
evaluate_and_choose([Tile|Tiles], B, Color, Depth, MaxMin, Record, Best) :-
	copy_term(B, Board),
	arg(Tile, Board, tile(Tile, Color)),
	next_color(Color, Opponent_Color),
	minimax(Depth, Board, MaxMin, Opponent_Color, _, Value),
	update(Tile, Value, Record, Record1),
	evaluate_and_choose(Tiles, B, Color, Depth, MaxMin, Record1, Best).


/* ************************************************ */
/*                                                  */
/*   minimax/6                                      */
/*      +Arg 1: depth                               */
/*      +Arg 2: the board                           */
/*      +Arg 3: minimax multiplier                  */
/*      +Arg 4: color                               */
/*      +Arg 5: current tile                        */
/*      -Arg 6: the best                            */
/*   Summary: Part of the minimax algorithm. Based  */
/*            off of the code in the Art of Prolog. */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - base case
minimax(0, Board, _, _, _, Value) :-
	evaluate(Board, Value).
%% 2 - recursive
minimax(Depth, Board, MaxMin, Color, Tile, Value) :-
	New_Depth is Depth - 1,
	open_tiles(Board, Tiles),
	MinMax is -MaxMin,
	evaluate_and_choose(Tiles, Board, Color, New_Depth,
			    MinMax, (nil, 100000000), (Tile, Value)).


/* ************************************************ */
/*                                                  */
/*   update/4                                       */
/*      +Arg 1: tile                                */
/*      +Arg 2: tile's value                        */
/*      +Arg 2: best so far                         */
/*      -Arg 2: new best                            */
/*   Summary: Updates the best value.               */
/*   Author: 1046411                                */
/*   Date: 04 December 2010                         */
/*                                                  */
/* ************************************************ */

update(Tile, Value, (_, Value2), (Tile, Value)) :-
	Value =< Value2.
update(_, Value, (Tile2, Value2), (Tile2, Value2)) :-
	Value > Value2.


/* ************************************************ */
/*                  End of program                  */
/* ************************************************ */
