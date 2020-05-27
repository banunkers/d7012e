/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Hugo Wangler
%    Student user id  : hugwan-6
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
%:- ensure_loaded('play.pl').
:- ensure_loaded('testboards.pl').
:- ensure_loaded('stupid.pl').
:- ensure_loaded('rndBoard.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 
initialize(InitialState, 1) :-
  %initBoard(InitialState).

  % 2. 3 test boards
  %flipAll8Dirs2(InitialState). % flip all -> full board of 2s
  %tie30emptyOnly2canMove(InitialState). % 2 moves then tie
  %forcing2toDoNullMove(InitialState). % 2 null move

  % 3. 3 test runs with random boards
  %rndBoardXYZ(InitialState).

  % 4. 2 runs against stupid
  initBoard(InitialState).

  %% Test boards
  %testBoard1(InitialState).
  %testBoard2(InitialState).
  %testBoard3(InitialState).
  %flipRLtop(InitialState).
  %flipLRbottom(InitialState).
  %flipTBleft(InitialState).
  %flipBTright(InitialState).
  %flipDiagULtoLR(InitialState).
  %flipDiagURtoLL(InitialState).
  %noMovesNoFlipsA(InitialState).
  %noMovesNoFlipsB(InitialState).
  %flipLRonly1(InitialState).
  %flipAll8Dirs1(InitialState).
  %tieInTwoMovesFullBoard(InitialState).
  %tieFourEmptyInCorners(InitialState).
  %tieFourEmptyOnBorders(InitialState).
  %tieFourEmptyOnly1canMove(InitialState).
  %tie30emptyOnly1canMove(InitialState).
  %winInTwoMovesFullBoard(InitialState).
  %onlyTwos(InitialState).
  %onlyOnes(InitialState).
  %forcing1toDoNullMoves(InitialState).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
winner(State, 1) :-
  terminal(State),
  getscore(State, 1, P1Score),
  getscore(State, 2, P2Score),
  P1Score < P2Score.
winner(State, 2) :-
  terminal(State),
  getscore(State, 1, P1Score),
  getscore(State, 2, P2Score),
  P2Score < P1Score.

% returns the Score of Plyr in the current board State
getscore(State, Plyr, Score) :-
  findall(_, get(State, _, Plyr), PlyrStones),
  length(PlyrStones, Score).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
tie(State) :-
  terminal(State),
  getscore(State, 1, P1Score),
  getscore(State, 2, P2Score),
  P1Score == P2Score.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   
terminal(State) :-
  moves(1, State, P1MvList),
  P1MvList == [n],
  moves(2, State, P2MvList),
  P2MvList == [n].





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%
moves(Plyr, State, SortedMvList) :-
  findall(
    [X, Y],
    (get(State, [X, Y], _), is_opponent(Plyr, State, [X, Y])),
    OpponentStones
  ),
  check_moves(Plyr, State, OpponentStones, Moves),
  length(Moves, NumMoves),
  (NumMoves == 0 -> MvList = [n], ! ; MvList = Moves, !),
  % sort left to right top to buttom using predefined predicate
  sort(MvList, SortedMvList). 

% check_moves(Plyr, State, OpponentStones, MvList):
%   Returnes a list of legal Moves the Plyr can make in the current state
%   the moves are sorted left to right, top to bottom
%
% This is done by finding all legal moves to squares
% adjacent to an opponent stone at [X, Y] on the board
check_moves(_, _, [], []).
%check_moves(Plyr, State, [[X, Y]], MvList) :-
%  findall(Move, check_move(Plyr, State, [X, Y], Move), MvList).
check_moves(Plyr, State, [Stone | Stones], MvList) :-
  % all moves possible adjacent to opponent stone [X, Y]
  findall(Move, check_move(Plyr, State, Stone, Move), MovesXY),
  check_moves(Plyr, State, Stones, Moves),
  append(MovesXY, Moves, MvList).

% check_move(Plyr, State, OpponentStone, Move):
%   Checks if placing a stone at positiong Move is legal
%   the Move position is one of the 8 squares surrounding the opponents stone
%   at pos [X, Y]
%
% Example: (all are variations checking different surrounding stones)
%   Placing stone East of opponent stone at [X, Y]
%   legal iff [X + 1, Y] is empty and west of [X, Y] contains at least one
%   opponent stone followed by a player stone
check_move(Plyr, State, [X, Y], [EastX, Y]) :- 
  EastX is X + 1,
  EastX < 6,
  WestX is X - 1,
  WestX >= 0,
  is_empty(State, [EastX, Y]), % east of opponent stone is empty
  check_west(Plyr, State, [WestX, Y]). % search for player stone west
% Placing stone West of opponent stone
check_move(Plyr, State, [X, Y], [WestX, Y]) :-
  WestX is X - 1,
  WestX >= 0,
  EastX is X + 1,
  EastX < 6,
  is_empty(State, [WestX, Y]),
  check_east(Plyr, State, [EastX, Y]).
% Placing stone North of opponent stone
check_move(Plyr, State, [X, Y], [X, NorthY]) :-
  NorthY is Y - 1,
  NorthY >= 0,
  SouthY is Y + 1,
  SouthY < 6,
  is_empty(State, [X, NorthY]),
  check_south(Plyr, State, [X, SouthY]).
% Placing stone south of opponent stone
check_move(Plyr, State, [X, Y], [X, SouthY]) :-
  SouthY is Y + 1,
  SouthY < 6,
  NorthY is Y - 1,
  NorthY >= 0,
  is_empty(State, [X, SouthY]),
  check_north(Plyr, State, [X, NorthY]).
% Placing stone northeast of opponent stone
check_move(Plyr, State, [X, Y], [NortheastX, NortheastY]) :-
  ne(X, Y, NortheastX, NortheastY),
  sw(X, Y, SouthwestX, SouthwestY),
  is_empty(State, [NortheastX, NortheastY]),
  check_sw(Plyr, State, [SouthwestX, SouthwestY]).
% Placing stone southwest of opponent stone
check_move(Plyr, State, [X, Y], [SouthwestX, SouthwestY]) :-
  sw(X, Y, SouthwestX, SouthwestY),
  ne(X, Y, NortheastX, NortheastY),
  is_empty(State, [SouthwestX, SouthwestY]),
  check_ne(Plyr, State, [NortheastX, NortheastY]).
% Placing stone southeast of opponent stone
check_move(Plyr, State, [X, Y], [SoutheastX, SoutheastY]) :-
  se(X, Y, SoutheastX, SoutheastY),
  nw(X, Y, NorthwestX, NorthwestY),
  is_empty(State, [SoutheastX, SoutheastY]),
  check_nw(Plyr, State, [NorthwestX, NorthwestY]).
% Placing stone northwest of opponent stone
check_move(Plyr, State, [X, Y], [NorthwestX, NorthwestY]) :-
  nw(X, Y, NorthwestX, NorthwestY),
  se(X, Y, SoutheastX, SoutheastY),
  is_empty(State, [NorthwestX, NorthwestY]),
  check_se(Plyr, State, [SoutheastX, SoutheastY]).

% The following check_DIRECTION predicates checks that a direction on
% the board contains a legal pattern of stones.
% Case west contains opponent stone -> continue west
check_west(Plyr, State, [X, Y]) :-
  is_opponent(Plyr, State, [X, Y]),
  WestX is X - 1,
  WestX >= 0,
  check_west(Plyr, State, [WestX, Y]).
% Case found player stone west of opponent stone(s) -> move is legal
check_west(Plyr, State, [X, Y]) :-
  is_player(Plyr, State, [X, Y]).

check_east(Plyr, State, [X, Y]) :-
  is_opponent(Plyr, State, [X, Y]),
  EastX is X + 1,
  EastX < 6,
  check_east(Plyr, State, [EastX, Y]).
check_east(Plyr, State, [X, Y]) :-
  is_player(Plyr, State, [X, Y]).

check_south(Plyr, State, [X, Y]) :-
  is_opponent(Plyr, State, [X, Y]),
  SouthY is Y + 1,
  SouthY < 6,
  check_south(Plyr, State, [X, SouthY]).
check_south(Plyr, State, [X, Y]) :-
  is_player(Plyr, State, [X, Y]).

check_north(Plyr, State, [X, Y]) :-
  is_opponent(Plyr, State, [X, Y]),
  NorthY is Y - 1,
  NorthY >= 0,
  check_north(Plyr, State, [X, NorthY]).
check_north(Plyr, State, [X, Y]) :-
  is_player(Plyr, State, [X, Y]).

check_se(Plyr, State, [X, Y]) :-
  is_opponent(Plyr, State, [X, Y]),
  se(X, Y, SoutheastX, SoutheastY),
  check_se(Plyr, State, [SoutheastX, SoutheastY]).
check_se(Plyr, State, [X, Y]) :-
  is_player(Plyr, State, [X, Y]).

check_ne(Plyr, State, [X, Y]) :-
  is_opponent(Plyr, State, [X, Y]),
  ne(X, Y, NortheastX, NortheastY),
  check_ne(Plyr, State, [NortheastX, NortheastY]).
check_ne(Plyr, State, [X, Y]) :-
  is_player(Plyr, State, [X, Y]).

check_nw(Plyr, State, [X, Y]) :-
  is_opponent(Plyr, State, [X, Y]),
  nw(X, Y, NorthwestX, NorthwestY),
  check_nw(Plyr, State, [NorthwestX, NorthwestY]).
check_nw(Plyr, State, [X, Y]) :-
  is_player(Plyr, State, [X, Y]).

check_sw(Plyr, State, [X, Y]) :-
  is_opponent(Plyr, State, [X, Y]),
  sw(X, Y, SouthwestX, SouthwestY),
  check_sw(Plyr, State, [SouthwestX, SouthwestY]).
check_sw(Plyr, State, [X, Y]) :-
  is_player(Plyr, State, [X, Y]).

% Checks if a position [X, Y] is empty on the board
is_empty(State, [X, Y]) :-
  get(State, [X, Y], Value),
  Value == '.'.

% Checks if a position [X, Y] contains a Plyr stone
is_player(Plyr, State, [X, Y]) :-
  get(State, [X, Y], Plyr).

% Checks if position [X, Y] contains an opponent stone
is_opponent(Plyr, State, [X, Y]) :-
  get(State, [X, Y], Stone),
  (Plyr = 1 -> Stone == 2
  ;Stone == 1
  ).

% Returns the the point se, ne, sw or nw of [X, Y] iff it is inside the 6x6 board
se(X, Y, SoutheastX, SoutheastY) :-
  SoutheastX is X + 1,
  SoutheastX < 6,
  SoutheastY is Y + 1,
  SoutheastY < 6.
ne(X, Y, NortheastX, NortheastY) :-
  NortheastX is X + 1,
  NortheastX < 6,
  NortheastY is Y - 1,
  NortheastY >= 0.
sw(X, Y, SouthwestX, SouthwestY) :-
  SouthwestX is X - 1,
  SouthwestX >= 0,
  SouthwestY is Y + 1,
  SouthwestY < 6.
nw(X, Y, NorthwestX, NorthwestY) :-
  NorthwestX is X - 1,
  NorthwestX >= 0,
  NorthwestY is Y - 1,
  NorthwestY >= 0.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(1, n, State, State, 2).
nextState(2, n, State, State, 1).
nextState(1, Move, State, NewState, 2) :-
  do_move(1, Move, State, NewState).
nextState(2, Move, State, NewState, 1) :-
  do_move(2, Move, State, NewState).

do_move(Plyr, [X, Y], State, NewState) :-
  set(State, TempState, [X, Y], Plyr),
  % find the directions which will be affected by the move
  findall(Dir, get_flip_dir(Plyr, TempState, [X, Y], Dir), FlipDirs),
  flip(Plyr, FlipDirs, [X, Y], TempState, NewState).

% Returnes a direction from the newly placed stone [X, Y] in which stones
% will be flipped as a result of the move.
get_flip_dir(Plyr, State, [X, Y], north) :-
  NorthY is Y - 1,
  % prevents check_north resulting in true if first stone north is player
  is_opponent(Plyr, State, [X, NorthY]),
  check_north(Plyr, State, [X, NorthY]).
get_flip_dir(Plyr, State, [X, Y], south) :-
  SouthY is Y + 1,
  is_opponent(Plyr, State, [X, SouthY]),
  check_south(Plyr, State, [X, SouthY]).
get_flip_dir(Plyr, State, [X, Y], west) :-
  WestX is X - 1,
  is_opponent(Plyr, State, [WestX, Y]),
  check_west(Plyr, State, [WestX, Y]).
get_flip_dir(Plyr, State, [X, Y], east) :-
  EastX is X + 1,
  is_opponent(Plyr, State, [EastX, Y]),
  check_east(Plyr, State, [EastX, Y]).
get_flip_dir(Plyr, State, [X, Y], sw) :-
  sw(X, Y, SouthwestX, SouthwestY),
  is_opponent(Plyr, State, [SouthwestX, SouthwestY]),
  check_sw(Plyr, State, [SouthwestX, SouthwestY]).
get_flip_dir(Plyr, State, [X, Y], se) :-
  se(X, Y, SoutheastX, SoutheastY),
  is_opponent(Plyr, State, [SoutheastX, SoutheastY]),
  check_se(Plyr, State, [SoutheastX, SoutheastY]).
get_flip_dir(Plyr, State, [X, Y], ne) :-
  ne(X, Y, NortheastX, NortheastY),
  is_opponent(Plyr, State, [NortheastX, NortheastY]),
  check_ne(Plyr, State, [NortheastX, NortheastY]).
get_flip_dir(Plyr, State, [X, Y], nw) :-
  nw(X, Y, NorthwestX, NorthwestY),
  is_opponent(Plyr, State, [NorthwestX, NorthwestY]),
  check_nw(Plyr, State, [NorthwestX, NorthwestY]).

% flip in all directions
flip(Plyr, north, [X, Y], State, NewState) :-
  flip_north(Plyr, [X, Y], State, NewState).
flip(Plyr, east, [X, Y], State, NewState) :-
  flip_east(Plyr, [X, Y], State, NewState).
flip(Plyr, south, [X, Y], State, NewState) :-
  flip_south(Plyr, [X, Y], State, NewState).
flip(Plyr, west, [X, Y], State, NewState) :-
  flip_west(Plyr, [X, Y], State, NewState).
flip(Plyr, ne, [X, Y], State, NewState) :-
  flip_ne(Plyr, [X, Y], State, NewState).
flip(Plyr, se, [X, Y], State, NewState) :-
  flip_se(Plyr, [X, Y], State, NewState).
flip(Plyr, sw, [X, Y], State, NewState) :-
  flip_sw(Plyr, [X, Y], State, NewState).
flip(Plyr, nw, [X, Y], State, NewState) :-
  flip_nw(Plyr, [X, Y], State, NewState).
flip(Plyr, [Dir], [X, Y], State, NewState) :-
  flip(Plyr, Dir, [X, Y], State, NewState).
flip(Plyr, [Dir | Dirs], [X, Y], State, NewState) :-
  flip(Plyr, Dir, [X, Y], State, TempState),
  flip(Plyr, Dirs, [X, Y], TempState, NewState).

% flip stones north until player stone is reached
flip_north(Plyr, [X, Y], State, NewState) :-
  NorthY is Y - 1,
  is_opponent(Plyr, State, [X, NorthY]),
  set(State, TempState, [X, NorthY], Plyr),
  flip_north(Plyr, [X, NorthY], TempState, NewState).
flip_north(Plyr, [X, Y], State, State) :-
  NorthY is Y - 1,
  is_player(Plyr, State, [X, NorthY]).

% flip stones east until player stone is reached
flip_east(Plyr, [X, Y], State, NewState) :-
  EastX is X + 1,
  is_opponent(Plyr, State, [EastX, Y]),
  set(State, TempState, [EastX, Y], Plyr),
  flip_east(Plyr, [EastX, Y], TempState, NewState).
flip_east(Plyr, [X, Y], State, State) :-
  EastX is X + 1,
  is_player(Plyr, State, [EastX, Y]).

% flip stones north until player stone is reached
flip_south(Plyr, [X, Y], State, NewState) :-
  SouthY is Y + 1,
  is_opponent(Plyr, State, [X, SouthY]),
  set(State, TempState, [X, SouthY], Plyr),
  flip_south(Plyr, [X, SouthY], TempState, NewState).
flip_south(Plyr, [X, Y], State, State) :-
  SouthY is Y + 1,
  is_player(Plyr, State, [X, SouthY]).

% flip stones west until player stone is reached
flip_west(Plyr, [X, Y], State, NewState) :-
  WestX is X - 1,
  is_opponent(Plyr, State, [WestX, Y]),
  set(State, TempState, [WestX, Y], Plyr),
  flip_west(Plyr, [WestX, Y], TempState, NewState).
flip_west(Plyr, [X, Y], State, State) :-
  WestX is X - 1,
  is_player(Plyr, State, [WestX, Y]).

% flip stones NE until player stone is reached
flip_ne(Plyr, [X, Y], State, NewState) :-
  ne(X, Y, NortheastX, NortheastY),
  is_opponent(Plyr, State, [NortheastX, NortheastY]),
  set(State, TempState, [NortheastX, NortheastY], Plyr),
  flip_ne(Plyr, [NortheastX, NortheastY], TempState, NewState).
flip_ne(Plyr, [X, Y], State, State) :-
  ne(X, Y, NortheastX, NortheastY),
  is_player(Plyr, State, [NortheastX, NortheastY]).

% flip stones SE until player stone is reached
flip_se(Plyr, [X, Y], State, NewState) :-
  se(X, Y, SoutheastX, SoutheastY),
  is_opponent(Plyr, State, [SoutheastX, SoutheastY]),
  set(State, TempState, [SoutheastX, SoutheastY], Plyr),
  flip_se(Plyr, [SoutheastX, SoutheastY], TempState, NewState).
flip_se(Plyr, [X, Y], State, State) :-
  se(X, Y, SoutheastX, SoutheastY),
  is_player(Plyr, State, [SoutheastX, SoutheastY]).

% flip stones SW until player stone is reached
flip_sw(Plyr, [X, Y], State, NewState) :-
  sw(X, Y, SouthwestX, SouthwestY),
  is_opponent(Plyr, State, [SouthwestX, SouthwestY]),
  set(State, TempState, [SouthwestX, SouthwestY], Plyr),
  flip_sw(Plyr, [SouthwestX, SouthwestY], TempState, NewState).
flip_sw(Plyr, [X, Y], State, State) :-
  sw(X, Y, SouthwestX, SouthwestY),
  is_player(Plyr, State, [SouthwestX, SouthwestY]).

% flip stones NW until player stone is reached
flip_nw(Plyr, [X, Y], State, NewState) :-
  nw(X, Y, NorthwestX, NorthwestY),
  is_opponent(Plyr, State, [NorthwestX, NorthwestY]),
  set(State, TempState, [NorthwestX, NorthwestY], Plyr),
  flip_nw(Plyr, [NorthwestX, NorthwestY], TempState, NewState).
flip_nw(Plyr, [X, Y], State, State) :-
  nw(X, Y, NorthwestX, NorthwestY),
  is_player(Plyr, State, [NorthwestX, NorthwestY]).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.
validmove(Plyr, State, Proposed) :-
  moves(Plyr, State, MvList),
  member(Proposed, MvList).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.
h(State, -100) :-
  winner(State, 2).
h(State, 100) :-
  winner(State, 1).
h(State, 0) :-
  tie(State).
h(State, Val) :-
  getscore(State, 2, P2Score),
  getscore(State, 1, P1Score),
  Val is P2Score - P1Score.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(101).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
