/* ************************************************ */
/*                                                  */
/*   play/2                                         */
/*      ?Arg 1: input letters (list)                */
/*      ?Arg 2: input letters (list)                */
/*   Summary: Determines if there is a path from a  */
/*            starting square that visits each      */
/*            exactly once and that is equal to the */
/*            input letters.                        */ 
/*   Author: 1046411                                */
/*   Date: 24 November 2010                         */
/*                                                  */
/* ************************************************ */

play(Word1, Word2) :-
	formatted_list(Word1, List1),
	formatted_list(Word2, List2),
	reverse_path(List2, Path2),
	depth_first(Start, List1, [Start], Path),
	are_equal(Path, Path2).

/* ************************************************ */
/*                                                  */
/*   depth_first/4                                  */
/*      ?Arg 1: current state (sq/2)                */
/*      +Arg 2: the list of possible words (sq/2)   */
/*      ?Arg 3: the path taken so far               */
/*      -Arg 4: the final path output               */
/*   Summary: Performs depth first seach on the     */
/*            trackword problem.                    */
/*   Author: 1046411                                */
/*   Date: 24 November 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - base
depth_first(_, _, Path, Path) :-
	final_state(Path).
%% 2 - recursive
depth_first(State, Word, Path, Moves) :-
	next_state(State, Word, Next_State),
	not_visited(Next_State, Path),
	depth_first(Next_State, Word, [Next_State|Path], Moves).


/* ************************************************ */
/*                                                  */
/*   are_equal/2                                    */
/*      +Arg 1: a list of sq/                       */
/*      +Arg 2: a list of sq/2                      */
/*   Summary: Determines if two sq/2 lists are      */
/*            equal.                                */
/*   Author: 1046411                                */
/*   Date: 24 November 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - base
are_equal([], []).
%% 2 - recursive
are_equal([sq(_,Ch)|Tail1], [sq(_,Ch)|Tail2]) :-
	are_equal(Tail1, Tail2).	


/* ************************************************ */
/*                                                  */
/*   next_state/3                                   */
/*      ?Arg 1: the current state                   */
/*      +Arg 2: the list of possible letters        */
/*      -Arg 3: the next state                      */
/*   Summary: Genrates a next potential state from  */
/*            the current state. It does not check  */
/*            whether it has been visitied or not.  */
/*   Author: 1046411                                */
/*   Date: 24 November 2010                         */
/*                                                  */
/* ************************************************ */

next_state(sq(Num,Letter), Word, sq(Next_Num, Next_Letter)) :-
	member(sq(Num,Letter), Word),
	move(Num, Next_Num),
	member(sq(Next_Num, Next_Letter), Word).


/* ************************************************ */
/*                                                  */
/*   not_visited/2                                  */
/*      +Arg 1: the state                           */
/*      +Arg 2: the list of visited states          */
/*   Summary: Determines if the given state has     */
/*            been previously visited.              */
/*   Author: 1046411                                */
/*   Date: 24 November 2010                         */
/*                                                  */
/* ************************************************ */

not_visited(State, History) :-
	\+ member(State, History).


/* ************************************************ */
/*                                                  */
/*   final_state/1                                  */
/*      +Arg 1: a list of visisted state            */
/*   Summary: Determines if the search should stop. */
/*            If there are nine states, then it is  */
/*            impossible to visit anymore state.    */
/*   Author: 1046411                                */
/*   Date: 24 November 2010                         */
/*                                                  */
/* ************************************************ */

final_state([_,_,_,_,_,_,_,_,_]).


/* ************************************************ */
/*                                                  */
/*   reverse_path/2                                 */
/*      +Arg 1: the input path                      */
/*      -Arg 2: the reversed input path             */
/*   Summary: The search returns a path in reverse  */
/*            order so it is convenient to reverse  */
/*            the second list and see if it matches */
/*            the first path.                       */
/*   Author: 1046411                                */
/*   Date: 24 November 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - calling rule
reverse_path(Path, Reversed_Path) :-
	reverse_path(Path, [], Reversed_Path).


%% 1 - base
reverse_path([], Path, Path).
%% 2 - recursive
reverse_path([Car|Cdr], Accm, Path) :-
	reverse_path(Cdr, [Car|Accm], Path).


/* ************************************************ */
/*                                                  */
/*   formatted_list/{2,3}                           */
/*      +Arg 1: input letters (list)                */
/*      -Arg 2: input letters (list of sq/2)        */
/*   Summary: Arg2 is a rewrite of Arg1 in the      */
/*            form [sq(1,a), sq(2,b), ...].         */
/*   Author: P J Hancox                             */
/*   Date:   1 October 2010                         */
/*                                                  */
/* ************************************************ */

%% 1 - calling rule
formatted_list(Letters, Formatter_Letters) :-
	formatted_list(Letters, Formatter_Letters, 0).


%% 1- terminating
formatted_list([], [], _).
%% 2 - recursive
formatted_list([Head|Tail], [sq(Count0, Head)|List], Count) :-
    Count0 is Count + 1,
    formatted_list(Tail, List, Count0).


/* ************************************************ */
/*                                                  */
/*   move/2                                         */
/*      +Arg 1: the start state                     */
/*      +Arg 2: the transitin state                 */
/*   Summary: Determines which states the starting  */
/*            state can transition to.              */
/*   Author: 1046411                                */
/*   Date: 24 November 2010                         */
/*                                                  */
/* ************************************************ */

move(1, 2).    move(1, 4).    move(1, 5).    move(2, 1).
move(2, 3).    move(2, 4).    move(2, 5).    move(2, 6).
move(3, 2).    move(3, 5).    move(3, 6).    move(4, 1).
move(4, 2).    move(4, 5).    move(4, 8).    move(4, 7).
move(5, 1).    move(5, 2).    move(5, 3).    move(5, 4).
move(5, 6).    move(5, 7).    move(5, 8).    move(5, 9).
move(6, 2).    move(6, 3).    move(6, 5).    move(6, 8).
move(6, 9).    move(7, 4).    move(7, 5).    move(7, 8).
move(8, 4).    move(8, 5).    move(8, 6).    move(8, 7).
move(8, 9).    move(9, 5).    move(9, 6).    move(9, 8).


/* ************************************************ */
/*                  End of program                  */
/* ************************************************ */
