% to test this use the query:
%     hb(Board), display_board(Board).

% *************** constants ***************

column_width(8).

tile_offset(6).

% Data structure

hb_test(hex_board(tile(1, _), tile(2, b), tile(3, _),
		  tile(4, _), tile(5, _), tile(6, b),
		  tile(7, _), tile(8, b), tile(9, _),
		  tile(10, w),tile(11, w),tile(12, w),
		  tile(13, w),tile(14, w),tile(15, b),
		  tile(16, _),tile(17, b),tile(18, _),
		  tile(19, _),tile(20, _),tile(21, w),
		  tile(22, w),tile(23, b),tile(24, _),
		  tile(25, _),tile(26, _),tile(27, b),
		  tile(28, _),tile(29, _),tile(30, w),
		  tile(31, w),tile(32, b),tile(33, _),
		  tile(34, _),tile(35, _),tile(36, b))).


hb_test2(hex_board(tile(1, _), tile(2, _), tile(3, _),
		   tile(4, _), tile(5, _), tile(6, _),
		   tile(7, _), tile(8, w), tile(9, _),
		   tile(10, _),tile(11, _),tile(12, _),
		   tile(13, _),tile(14, _),tile(15, _),
		   tile(16, _),tile(17, _),tile(18, _),
		   tile(19, _),tile(20, _),tile(21, _),
		   tile(22, _),tile(23, _),tile(24, _),
		   tile(25, _),tile(26, _),tile(27, _),
		   tile(28, _),tile(29, _),tile(30, _),
		   tile(31, b),tile(32, _),tile(33, _),
		   tile(34, _),tile(35, _),tile(36, _))).


hb(hex_board(tile(1, _), tile(2, _), tile(3, _),
	     tile(4, _), tile(5, _), tile(6, _),
	     tile(7, _), tile(8, _), tile(9, _),
	     tile(10, _),tile(11, _),tile(12, _),
	     tile(13, _),tile(14, _),tile(15, _),
	     tile(16, _),tile(17, _),tile(18, _),
	     tile(19, _),tile(20, _),tile(21, _),
	     tile(22, _),tile(23, _),tile(24, _),
	     tile(25, _),tile(26, _),tile(27, _),
	     tile(28, _),tile(29, _),tile(30, _),
	     tile(31, _),tile(32, _),tile(33, _),
	     tile(34, _),tile(35, _),tile(36, _))).


/* ************************************************ */
/*                                                  */
/*   display_board/1                                */
/*      Arg 1: hex board (structure of arity 36)    */
/*   Summary: Displays a hex board.                 */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

display_board(Board) :-
     draw_space(18),
     write(black),
     draw_space(18),
     write('/'),
     draw_space(16),
     write(white),
     nl,
     draw_board_line1(Board), 
     draw_board_line2(Board), 
     draw_board_line3(Board), 
     draw_board_line4(Board), 
     draw_board_line5(Board), 
     draw_board_line6(Board),
     draw_board_line7(Board),
     draw_board_line8(Board),
     draw_board_line9(Board),
     draw_board_line10(Board),
     draw_board_line11(Board),
     draw_space(18),
     write(white),
     draw_space(13),
     write('/'),
     draw_space(21),
     write(black),
     nl.


/* ************************************************ */
/*                                                  */
/*   display_tile/2                                 */
/*      Arg 1: no. of tile (integer)                */
/*      Arg 2: hex board (structure of arity 36)    */
/*   Summary: Displays middle line of tile, eg:     */
/*            <   2    > or <   w    >              */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

draw_line(Tile, Board) :-
     write('<'),
     draw_inside(Tile, Board),
     write('>').


/* ************************************************ */
/*                                                  */
/*   draw_side.../0                                 */
/*   Summary: Draw a side of a box. 8 variants.     */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

draw_side1 :-
     write('/    ').
draw_side2 :-
     write('/      ').
draw_side3 :-
     write(\), write('    ').
draw_side4 :-
     write(\), write('      ').
draw_side5 :-
     write('/    ').
draw_side6 :-
     write('/      ').
draw_side7 :-
     write(\), write('    ').
draw_side8 :-
     write(\), write('      ').


/* ************************************************ */
/*                                                  */
/*   display_inside/2                               */
/*      Arg 1: no. of tile (integer)                */
/*      Arg 2: hex board (structure of arity 36)    */
/*   Summary: Displays either the number or owner   */
/*            of a tile.                            */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

% 1 - owned
draw_inside(Tile, Board) :-
     arg(Tile, Board, tile(Tile, Owner)),
     nonvar(Owner),
     display_column(Owner).
% 2 - no owner
draw_inside(Tile, Board) :-
     arg(Tile, Board, tile(Tile, Owner)),
     var(Owner),
     display_column(Tile).


/* ************************************************ */
/*                                                  */
/*   draw_board_line.../1  (11 variants)            */
/*      Arg 1: hex board (structure of arity 36)    */
/*   Summary: Displays a row of tiles.              */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

draw_board_line1(Board) :-
     tile_offset(Tab),
     tab(Tab * 6 + 1),
     write('----'),
     nl,
     tab(Tab * 6),
     draw_side1, draw_side4, 
     nl,
     tab((Tab * 6) - 1),
     draw_side2, draw_side3,
     nl,
     tab((Tab * 5) + 1),
     write('---'),
     draw_inside(6, Board),
     write('---'),
     nl.

draw_board_line2(Board) :-
     tile_offset(Tab),
     tab(Tab * 5),
     draw_side1, draw_side4, draw_side1, draw_side4,
     nl,
     tab((Tab * 5) - 1),
     draw_side2, draw_side3, draw_side2, draw_side3,
     nl,
     tab((Tab * 4) + 1),
     write('---'),
     draw_inside(5, Board), 
     write('--'),
     draw_inside(12, Board),
     write('---'),
     nl.

draw_board_line3(Board) :-
     tile_offset(Tab),
     tab(Tab * 4),
     draw_side1, draw_side4, draw_side1, 
     draw_side4, draw_side1, draw_side4,
     nl,
     tab((Tab * 4) - 1),
     draw_side2, draw_side3, draw_side2, 
     draw_side3, draw_side2, draw_side3,
     nl,
     tab((Tab * 3) + 1),
     write('---'),
     draw_inside(4, Board), 
     write('--'),
     draw_inside(11, Board),
     write('--'),
     draw_inside(18, Board),
     write('---'),
     nl.

draw_board_line4(Board) :-
     tile_offset(Tab),
     tab(Tab * 3),
     draw_side1, draw_side4, draw_side1, 
     draw_side4, draw_side1, draw_side4,
     draw_side1, draw_side4, 
     nl,
     tab((Tab * 3) - 1),
     draw_side2, draw_side3, draw_side2, 
     draw_side3, draw_side2, draw_side3,
     draw_side2, draw_side3, 
     nl,
     tab((Tab * 2) + 1),
     write('---'),
     draw_inside(3, Board), 
     write('--'),
     draw_inside(10, Board),
     write('--'),
     draw_inside(17, Board),
     write('--'),
     draw_inside(24, Board),
     write('---'),
     nl.

draw_board_line5(Board) :-
     tile_offset(Tab),
     tab(Tab * 2),
     draw_side1, draw_side4, draw_side1, 
     draw_side4, draw_side1, draw_side4,
     draw_side1, draw_side4, draw_side1, draw_side4, 
     nl,
     tab((Tab * 2) - 1),
     draw_side2, draw_side3, draw_side2, 
     draw_side3, draw_side2, draw_side3,
     draw_side2, draw_side3, draw_side2, draw_side3, 
     nl,
     tab((Tab * 1) + 1),
     write('---'),
     draw_inside(2, Board), 
     write('--'),
     draw_inside(9, Board),
     write('--'),
     draw_inside(16, Board),
     write('--'),
     draw_inside(23, Board),
     write('--'),
     draw_inside(30, Board),
     write('---'),
     nl.

draw_board_line6(Board) :-
     tile_offset(Tab),
     tab(Tab * 1),
     draw_side1, draw_side4, draw_side1, 
     draw_side4, draw_side1, draw_side4,
     draw_side1, draw_side4, draw_side1, 
     draw_side4, draw_side1, draw_side4, 
     nl,
     tab((Tab * 1) - 1),
     draw_side2, draw_side3, draw_side2, 
     draw_side3, draw_side2, draw_side3,
     draw_side2, draw_side3, draw_side2, 
     draw_side3, draw_side2, draw_side3, 
     nl,
     tab((Tab * 0) + 1),
     write('---'),
     draw_inside(1, Board), 
     write('--'),
     draw_inside(8, Board),
     write('--'),
     draw_inside(15, Board),
     write('--'),
     draw_inside(22, Board),
     write('--'),
     draw_inside(29, Board),
     write('--'),
     draw_inside(36, Board),
     write('---'),
     nl,
     tab((Tab * 1) - 1),
     draw_side8, draw_side5, draw_side8, 
     draw_side5, draw_side8, draw_side5,
     draw_side8, draw_side5, draw_side8, 
     draw_side5, draw_side8, draw_side5,
     nl,
     tab(Tab * 1),
     draw_side7, draw_side6, draw_side7, 
     draw_side6, draw_side7, draw_side6,
     draw_side7, draw_side6, draw_side7, 
     draw_side6, draw_side7, draw_side6,
     nl.

draw_board_line7(Board) :-
     tile_offset(Tab),
     tab((Tab * 1) + 1),
     write('---'),
     draw_inside(7, Board), 
     write('--'),
     draw_inside(14, Board),
     write('--'),
     draw_inside(21, Board),
     write('--'),
     draw_inside(28, Board),
     write('--'),
     draw_inside(35, Board),
     write('---'),
     nl,
     tab((Tab * 2) - 1),
     draw_side8, draw_side5, draw_side8, 
     draw_side5, draw_side8, draw_side5,
     draw_side8, draw_side5, draw_side8, draw_side5, 
     nl,
     tab(Tab * 2),
     draw_side7, draw_side6, draw_side7, 
     draw_side6, draw_side7, draw_side6,
     draw_side7, draw_side6, draw_side7, draw_side6, 
     nl.

draw_board_line8(Board) :-
     tile_offset(Tab),
     tab((Tab * 2) + 1),
     write('---'),
     draw_inside(13, Board), 
     write('--'),
     draw_inside(20, Board),
     write('--'),
     draw_inside(27, Board),
     write('--'),
     draw_inside(34, Board),
     write('---'),
     nl,
     tab((Tab * 3) - 1),
     draw_side8, draw_side5, draw_side8, 
     draw_side5, draw_side8, draw_side5,
     draw_side8, draw_side5, 
     nl,
     tab(Tab * 3),
     draw_side7, draw_side6, draw_side7, 
     draw_side6, draw_side7, draw_side6,
     draw_side7, draw_side6,
     nl.

draw_board_line9(Board) :-
     tile_offset(Tab),
     tab((Tab * 3) + 1),
     write('---'),
     draw_inside(19, Board), 
     write('--'),
     draw_inside(26, Board),
     write('--'),
     draw_inside(33, Board),
     write('---'),
     nl,
     tab((Tab * 4) - 1),
     draw_side8, draw_side5, draw_side8, 
     draw_side5, draw_side8, draw_side5,
     nl,
     tab(Tab * 4),
     draw_side7, draw_side6, draw_side7, 
     draw_side6, draw_side7, draw_side6,
     nl.

draw_board_line10(Board) :-
     tile_offset(Tab),
     tab((Tab * 4) + 1),
     write('---'),
     draw_inside(25, Board), 
     write('--'),
     draw_inside(32, Board),
     write('---'),
     nl,
     tab((Tab * 5) - 1),
     draw_side8, draw_side5, draw_side8, draw_side5,
     nl,
     tab(Tab * 5),
     draw_side7, draw_side6, draw_side7, draw_side6,
     nl.

draw_board_line11(Board) :-
     tile_offset(Tab),
     tab((Tab * 5) + 1),
     write('---'),
     draw_inside(31, Board), 
     write('---'),
     nl,
     tab((Tab * 6) - 1),
     draw_side8, draw_side5,
     nl,
     tab(Tab * 6),
     draw_side7, draw_side6,
     nl,
     tab(Tab * 6 + 1),
     write('----'),
     nl.


/* ************************************************ */
/*                                                  */
/*   draw_space/1                                   */
/*      Arg 1: positive integer or zero             */
/*   Summary: Outputs Arg1 number of spaces.        */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

% 1 - terminating condition
draw_space(0).
% 2 - recursive
draw_space(Numb) :-
     Numb > 0,
     write(' '),
     Numb1 is Numb - 1,
     draw_space(Numb1).


/* ************************************************ */
/*                                                  */
/*   display_column/1                               */
/*      Arg 1: atom to be displayed                 */
/*   Summary: Displays Arg 1 centred.               */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

display_column(Char) :-
     write('<'),
     name(Char, Char_List),
     length(Char_List, Char_Len),
     column_width(Col_Width),     % this is declared
                                  % as a "constant"
     Left_Tab_Width is (Col_Width - Char_Len) // 2,
     tab(Left_Tab_Width),
     write(Char),
     Right_Tab_Width is Col_Width - Char_Len - Left_Tab_Width,
     tab(Right_Tab_Width),
     % output '>'
     write('>').


/* ************************************************ */
/*                                                  */
/*   writeln/1                                      */
/*      Arg 1: a term                               */
/*   Summary: Write Arg 1 followed by nl.           */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

writeln(Arg) :-
     write(Arg), nl.


/* ************************************************ */
/*                                                  */
/*   tab/1                                          */
/*      Arg 1: Number                               */
/*   Summary: Output Number of spaces.              */
/*            Only used where there is no in-built  */
/*            tab/1.                                */
/*   Author: P J Hancox                             */
/*   Date: 12 October 2008                          */
/*                                                  */
/* ************************************************ */

:- ( predicate_property(tab(_), _) ->
          % tab/1 is built in
          true
       ;
          % tab/1 is not built in
          assert((tab(N) :- 
                       integer(N), 
                       tab1(N))),
          assert((tab(N) :- 
                       compound(N),
                       R is N,
                       tab1(R))),
          assert((tab1(N) :-
                       integer(N),
                       N =:= 0)),
          assert((tab1(N) :-
                       integer(N),
                       N > 0,
                       write(' '),
                       N1 is N - 1,
                       tab1(N1)))
   ).


/* ************************************************ */
/*                  End of program                  */
/* ************************************************ */
