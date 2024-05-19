/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Hannes Furhoff
%    Student user id  : hanfur-0
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').


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
% Human moves first = 1, robot = 2
initialize(InitialState, InitialPlayer) :- 
	initBoard(InitialState),
	InitialPlayer = 1.

opponent(1,2).
opponent(2,1).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
countstones([],_,0).
countstones([R1|RR],Player,Tot) :-
	countrow(R1,Player,Tot2), countstones(RR,Player,Tot3),
	Tot is Tot2 + Tot3.
countrow([],_,0).
countrow([V1|VR],Player,Tot) :- 
	( V1==Player 
		-> countrow(VR,Player,TN), Tot is TN+1 
		;  countrow(VR,Player,TN), Tot is TN
	).

% Return the #of stones for both players
getstones(State,P1,P2) :- 
	countstones(State,1,P1),
	countstones(State,2,P2).

winner(State,WinningPlayer) :- 
	terminal(State),
	getstones(State,P1,P2),
	P1 < P2,  WinningPlayer = P1.

winner(State,WinningPlayer) :- 
	terminal(State),
	getstones(State,P1,P2),
	P1 > P2,  WinningPlayer = P2.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
tie(State) :- 
	terminal(State),
	getstones(State,P1,P2), 
	P1==P2.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   
terminal(State) :- 
	moves(1,State,LiP), 
	moves(2,State,LiR),
	length(LiP,LP), length(LiR,LR),
	LP==0, LR==0.


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
moves(Player,State,MyList) :- findall([X,Y], (
	between(0,5,X),
	between(0,5,Y),
	get(State, [X,Y], '.'),
	dir(DX,DY),
	is_adjacent_opponent(State,Player,[X,Y],[DX,DY]),	
	can_flip_path(Player,State,[X,Y],[DX,DY])
), MvL), list_to_set(MvL,MyList).

move(X,Y,DX,DY,X2,Y2) :- 
	X2 is X + DX, Y2 is Y + DY, 
	X2 >= 0, X2 < 6, 
	Y2 >= 0, Y2 < 6. 

% T/F if path from P->P exists
can_flip_path(Player,State,[X,Y],[DX,DY]) :-
	move(X,Y,DX,DY,X2,Y2),
	get(State,[X2,Y2],Tile),
	( Tile == Player ; can_flip_path_step(Player,State,[X2,Y2],[DX,DY]) ).
	
can_flip_path_step(Player,State,[X,Y],[DX,DY]) :-
	move(X,Y,DX,DY,X2,Y2),
	get(State,[X2,Y2],Tile),
	( Tile == Player
	; Tile == Opponent -> can_flip_path_step(Player,State,[X2,Y2],[DX,DY])).

is_adjacent_opponent(State,Player,[X,Y],[DX,DY]) :- 
	move(X,Y,DX,DY,X2,Y2), opponent(Player,Opponent), get(State,[X2,Y2],Opponent).

dir(-1,0). dir(1,0). dir(0,-1). dir(0,1).
dir(-1,-1). dir(-1,1). dir(1,-1). dir(1,1).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Player,[X,Y],State,NewState,NextPlayer) :-
	validmove(Player,State,[X,Y]),
	dir(DX,DY),
	set(State,NState,[X,Y],Player),
	opponent(Player,NextPlayer),
	trace_flip_path(Player,NState,NewState,[X,Y],[DX,DY],[]).

trace_flip_path(Player,State,NewState,[X,Y],[DX,DY],[]) :-
	move(X,Y,DX,DY,X2,Y2),
	get(State,[X2,Y2],Tile),
	( Tile == Player ; 
		trace_flip_path_step(Player,State,NewState,[X2,Y2],[DX,DY],[[X2,Y2]]) 
	).
	
trace_flip_path_step(Player,State,NewState,[X,Y],[DX,DY],Trace) :-
	move(X,Y,DX,DY,X2,Y2),
	get(State,[X2,Y2],Tile),
	( Tile == Player -> retrace_path(Player,State,NewState,Trace)
	; Tile == Opponent -> 
		trace_flip_path_step(Player,State,NewState,[X2,Y2],[DX,DY],[[X2,Y2]|Trace])
	).

retrace_path(_,State,State,[]).
retrace_path(Player,State,NewState,[H|T]) :-
	write('Tracing: ' ), write(H), write(' in '), writeln([H|T]),
	set(State, State1, H, Player),
	retrace_path(Player,State1,NewState,T).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.
validmove(Player,State,[X,Y]) :- 
	moves(Player,State,R),
	member([X,Y],R).
	


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
h(State,Val) :- 
	terminal(State),
	( winner(State, 1) 
		-> val is -100
		;  ( winner(State, 2) 
			-> val is 100
			;  tie(State) -> val is 0
	)).
h(State,Val) :-
	val is 0.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(B) :- B = -101.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(B) :- B = 101.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(State, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(State, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in State and bind resulting grid to NewBoard
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
% get(State, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( State, [X, Y], Value) :- 
	nth0( Y, State, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( State, NewBoard, [X, Y], Value): set the value of the board at position
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
 
