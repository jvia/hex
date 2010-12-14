/* ************************************************ */
/*                                                  */
/* A Prolog regression test suite.                  */
/*                                                  */
/* Peter Hancox, October 2007                       */
/* This is not in the public domain.                */
/*                                                  */
/* ************************************************ */


/* ************************************************ */
/*                                                  */
/* Sets SICStus Prolog 3.x into ISO mode.           */
/*                                                  */
/* ************************************************ */

:- prolog_flag(version, Version),
   atom_chars(Version, Version_Chars),
   Temp =.. [f|Version_Chars],
   ( arg(9, Temp, '4') ->
        true
     ;
        prolog_flag(language,_,iso)
   ).




/* ************************************************ */
/*                                                  */
/*   To run the test suite, load this into Prolog,  */
/*   the use the queries:                           */
/*   test_script(classify, 3).                      */
/*   test_script(vowel, 1).                         */
/*                                                  */
/*   To add new tests, add new test_case/6 rules    */
/*   and ensure that the code to be tested is       */
/*   also loaded into Prolog.                       */
/*                                                  */
/* ************************************************ */



/* ************************************************ */
/*                                                  */
/*   test_script/2                                  */
/*     +Arg 1: functor (atom)                       */
/*     +Arg 2: arity (integer)                      */
/*   Summary: Loads set of test cases and executes  */
/*            each, providing a status report on    */
/*            each.                                 */
/*   Author: P J Hancox                             */
/*   Date: 24 October 2007                          */
/*                                                  */
/* ************************************************ */

test_script(Functor, Arity) :-
     findall(test_case(Outcome, Functor, Arity, ID, Args_In, Args_Out),
             test_case(Functor, Arity, ID, Args_In, Args_Out, Outcome),
             Test_Cases),
     test_script(Test_Cases, 0, 0, 0).


/* ************************************************ */
/*                                                  */
/*   test_script/4                                  */
/*     +Arg 1: test cases (list)                    */
/*     +Arg 2: arity (integer)                      */
/*     +Arg 3: arity (integer)                      */
/*     +Arg 4: arity (integer)                      */
/*   Summary: Executes each test case, providing a  */
/*            status report on each.                */
/*   Author: P J Hancox                             */
/*   Date: 24 October 2007                          */
/*                                                  */
/* ************************************************ */


/*  Version with error handling  */

% 1 - terminating
test_script([], Tests, Succeeded, Failed) :-
     nl,
     writeln('Test cases completed'), 
     write('Number of tests: '),
     writeln(Tests),
     write('Number succeeding: '),
     writeln(Succeeded),
     write('Number failing: '),
     writeln(Failed),
     Total_Reported is Succeeded + Failed,
     Caught is Tests - Total_Reported,
     write('Number caught by error handling: '),
     writeln(Caught).
% 2 - recursive
test_script([test_case(Outcome, Functor, _Arity, ID, Args_In, Args_Out)|Test_Cases],
            Tests, Succeeded, Failed) :-
     univ(Goal_In, [Functor|Args_In]),
     univ(Goal_Out, [Functor|Args_Out]),
     write('Test '),
     write(ID),
     tabbing(2),
     write(Goal_In),
     tabbing(2),
     Tests1 is Tests + 1,
     (
       Outcome = true ->
          Goal = Goal_In
       ;
          Goal = (\+ Goal_In)
     ),
     catch( ( (
                 execute_test_case(Goal, Goal_In, Goal_Out) ->
                    Succeeded1 is Succeeded + 1,
                    Failed1 is Failed,
                    writeln(succeeded)
                ;
                    Succeeded1 is Succeeded,
                    Failed1 is Failed + 1,
                    writeln(failed)
              ),
              test_script(Test_Cases, Tests1, Succeeded1, Failed1) 
            ),
            _Throw_Label,
            ( writeln('system error'),
              test_script(Test_Cases, Tests1, Succeeded, Failed))).



/* ************************************************ */
/*                                                  */
/*   test_case/6                                    */
/*     +Arg 1: functor (atom)                       */
/*     +Arg 2: arity (integer)                      */
/*     +Arg 3: test case id (integer)               */
/*     +Arg 4: arguments on call (list)             */
/*     +Arg 5: arguments after call (list)          */
/*     +Arg 6: 'true' or 'fail' (atom)              */
/*   Summary: Specification of a test condition.    */
/*   Author: P J Hancox                             */
/*   Date: 24 October 2007                          */
/*                                                  */
/* ************************************************ */

test_case(winning_path, 2, 1, [[], w],                   [[], w],                   fail).
test_case(winning_path, 2, 2, [[], b],                   [[], b],                   fail).
test_case(winning_path, 2, 3, [[1,2,3,4,5,6], w],        [[1,2,3,4,5,6], w],        true).
test_case(winning_path, 2, 4, [[4,9,15,21,17,27,33], b], [[4,9,15,21,17,27,33], b], true).

test_case(vowel, 1, 1, [a], [a], true).
test_case(vowel, 1, 2, [e], [e], true).
test_case(vowel, 1, 3, [i], [i], true).
test_case(vowel, 1, 4, [o], [o], true).
test_case(vowel, 1, 5, [u], [u], true).
test_case(vowel, 1, 6, [b], [b], fail).
test_case(vowel, 1, 7, [z], [z], fail).


test_case(classify, 3, 1, [[], _Vowels, _Consonants], [[], [], []], true).
test_case(classify, 3, 2, [[], [], []], [[], [], []], true).
test_case(classify, 3, 3, [[], [a,e,i,o,u], [b,c,d,f,g]], [[], [a,e,i,o,u], [b,c,d,f,g]], fail).
test_case(classify, 3, 4, [[], [b,c,d,f,g], [a,e,i,o,u]], [[], [b,c,d,f,g], [a,e,i,o,u]], true).
test_case(classify, 3, 5, [[a,e,i,o,u], _Vowels, _Consonants], [[a,e,i,o,u], [a,e,i,o,u], []], true).
test_case(classify, 3, 6, [[b,c,d,f,g], _Vowels, _Consonants], [[b,c,d,f,g], [], [b,c,d,f,g]], true).


/* ************************************************ */
/*                                                  */
/*   execute_test_case/3                            */
/*     +Arg 1: goal (possibly negated)              */
/*     +Arg 2: goal with arguments on call          */
/*     +Arg 3: goal with arguments after call       */
/*   Summary: Executes a test goal and compares     */
/*            computed arguments with expected      */
/*            arguments.                            */
/*   Author: P J Hancox                             */
/*   Date: 24 October 2007                          */
/*                                                  */
/* ************************************************ */

execute_test_case(Goal, Goal_Before, Goal_After) :-
     call(Goal),
     Goal_Before = Goal_After.


/* ************************************************ */
/*                                                  */
/*   univ/2                                         */
/*     ?Arg 1: term (compound or atom)              */
/*     ?Arg 2: functor and arguments (list)         */
/*   Summary: Re-implementation of =../2 using      */
/*            functor/3 and arg/3.                  */
/*   Author: P J Hancox                             */
/*   Date: 15 October 2007                          */
/*                                                  */
/* ************************************************ */

% 1 - Term is instantiated - doesn't matter if list is
univ(Term, [Functor|Args]) :-
     nonvar(Term),
     functor(Term, Functor, Arity),
     args(1, Arity, Term, Args).
% 2 - Term in uninstantiated - so List must be an instantiated list
univ(Term, List) :-
     var(Term),
     is_list(List),
     List = [Functor|Args],
     nonvar(Functor),
     list_length(Args, Arity),
     functor(Term, Functor, Arity),
     args(1, Arity, Term, Args).


/* ************************************************ */
/*                                                  */
/*   args/4                                         */
/*     +Arg 1: count (integer)                      */
/*     +Arg 2: arity (integer)                      */
/*     -Arg 3: term (compound)                      */
/*     +Arg 4: arguments (list)                     */
/*   Summary: Inserts arguments (arg 3) into term   */
/*            (arg 4).                              */
/*   Author: P J Hancox                             */
/*   Date: 15 October 2007                          */
/*                                                  */
/* ************************************************ */

% 1 - terminating
args(Count, Arity, _Term, []) :-
     Count > Arity.
% 2 - recursive
args(Count, Arity, Term, [Arg|Args]) :-
     Count =< Arity,
     arg(Count, Term, Arg),
     Count1 is Count + 1,
     args(Count1, Arity, Term, Args).

/* ************************************************ */
/*                                                  */
/*     Standard predicates from other chapters      */
/*                                                  */
/* ************************************************ */


/* ************************************************ */
/*                                                  */
/*   is_list/2                                      */
/*     +Arg 1: list                                 */
/*   Summary: True if argument is a list of 0 or    */
/*            more members.                         */
/*   Author: P J Hancox                             */
/*   Date: 15 October 2007                          */
/*                                                  */
/* ************************************************ */

is_list(List) :-
     nonvar(List),
     is_list1(List).


/* ************************************************ */
/*                                                  */
/*   is_list/2                                      */
/*     +Arg 1: list                                 */
/*   Summary: True if argument unifies with a list  */
/*            of 0 or more members.                 */
/*   Author: P J Hancox                             */
/*   Date: 15 October 2007                          */
/*                                                  */
/* ************************************************ */

% 1 - list is empty
is_list1([]).
% 2 list has at least one member
is_list1([_|_]).


/* ************************************************ */
/*                                                  */
/*   list_length/2                                  */
/*     +Arg 1: list                                 */
/*     -Arg 2: length of arg 1 (integer)            */
/*   Summary: Arg2 is the number of items in arg1.  */
/*   Author: P J Hancox                             */
/*   Date: 15 October 2007                          */
/*                                                  */
/* ************************************************ */

list_length(List, Length) :-
     list_length(List, 0, Length).


/* ************************************************ */
/*                                                  */
/*   list_length/3                                  */
/*     +Arg 1: list                                 */
/*     +Arg 2: no of elements counted (accumulator) */
/*     -Arg 2: length of arg 1 (integer)            */
/*   Summary: Arg3 is the number of items in arg1.  */
/*   Author: P J Hancox                             */
/*   Date: 15 October 2007                          */
/*                                                  */
/* ************************************************ */

% 1 - terminating
list_length([], Length, Length).
% 2 - recursive
list_length([_Head|Tail], Count0, Count) :-
     Count1 is Count0 + 1,
     list_length(Tail, Count1, Count).


/* ************************************************ */
/*                                                  */
/*   tabbing/1                                      */
/*     +Arg 1: offset (integer)                     */
/*   Summary: outputs specified no of spaces to     */
/*            current output stream.                */
/*   Author: P J Hancox                             */
/*   Date: 15 October 2007                          */
/*                                                  */
/* ************************************************ */

% 1 – terminating
tabbing(0).
% 2 – recursive
tabbing(Integer) :-
     integer(Integer),
     Integer > 0,
     write(' '),
     Integer1 is Integer - 1,
     tabbing(Integer1).


/* ************************************************ */
/*                                                  */
/*   writeln/1                                      */
/*     +Arg 1: term                                 */
/*   Summary: outputs term followed by new line.    */
/*   Author: P J Hancox                             */
/*   Date: 15 October 2007                          */
/*                                                  */
/* ************************************************ */

writeln(Term) :-
     write(Term),
     nl.


/* ************************************************ */
/*                                                  */
/*    Code that is the subject of the test suite    */
/*                                                  */
/* ************************************************ */


/* ************************************************ */
/*                                                  */
/*   vowel/1                                        */
/*   Summary: True if Arg1 is a vowel.              */
/*     Arg 1: Letter.                               */
/*   Author:  P J Hancox                            */
/*   Date:    30 October 2002                       */
/*                                                  */
/* ************************************************ */

vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).


/* ************************************************ */
/*                                                  */
/*   classify/3                                     */
/*   Summary: Classifies a list of letters into two */
/*            lists: vowels and consonants.         */
/*     Arg 1: List of letters.                      */
/*     Arg 2: List of vowels.                       */
/*     Arg 3: List of consonants.                   */
/*   Author:  P J Hancox                            */
/*   Date:    30 October 2002                       */
/*                                                  */
/* ************************************************ */

% 1 - terminating
classify([], [], []).
% 2 - recursive: letter is a vowel
classify([Vowel|Tail], [Vowel|Vowel_Tail], Non_Vowels) :-
     vowel(Vowel),
     classify(Tail, Vowel_Tail, Non_Vowels).
% 3 - recursive: letter is a consonant
classify([Non_Vowel|Tail], Vowels, [Non_Vowel|Non_Vowel_Tail]) :-
     classify(Tail, Vowels, Non_Vowel_Tail).



/* *************************** */
/*                             */
/*         End of file         */
/*                             */
/* *************************** */
