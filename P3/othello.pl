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
% :- ensure_loaded('play.pl').
:- ensure_loaded('stupid.pl').


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

%([ [.,.,.,.,.,.], 
%            [.,.,.,.,.,.],
%	    	[.,.,1,2,.,.], 
%	    	[.,.,2,1,.,.], 
%            [.,.,.,.,.,.], 
%	    	[.,.,.,.,.,.] ]).

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

testBoard1([ [.,2,.,.,.,2], 
             [.,.,1,.,1,.],
             [.,.,.,1,.,.],
             [.,.,1,1,1,.],
             [.,1,.,1,.,.],
             [.,.,.,2,.,.] ]).

testBoard2([[.,2,1,1,1,2], 
			[2,2,2,2,2,1], 
			[1,2,2,2,2,1],
			[1,2,2,1,2,1], 
			[1,2,2,2,1,1],
			[2,1,1,1,1,.]]).

testBoard3([[.,.,.,.,.,.],
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,1,1,1,1,2]]).

:-ensure_loaded('rndBoard.pl').

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 
% Human moves first = 1, robot = 2

%initialize(InitialState, 1) :- rndBoardXYZ(InitialState).
initialize(InitialState, 1) :- initBoard(InitialState).
%initialize(InitialState, 1) :- testBoard1(InitialState).
%initialize(InitialState, 1) :- testBoard2(InitialState).
%initialize(InitialState, 1) :- testBoard3(InitialState).

opponent(1,2).
opponent(2,1).

dir(-1,0). 	% W
dir(1,0).	% E
dir(0,-1). 	% N
dir(0,1).	% S
dir(-1,-1).	% NW
dir(-1,1).	% SW
dir(1,-1).	% NE
dir(1,1).	% SE


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
countstones([],_,0).
countstones([R1|RR],Player,Tot) :-
	countrow(R1,Player,Tot2), countstones(RR,Player,RecTot),
	Tot is Tot2 + RecTot.

countrow([],_,0).
countrow([V1|VR],Player,Tot) :- 
	( V1==Player 
		-> countrow(VR,Player,TN), Tot is TN+1 
		;  countrow(VR,Player,TN), Tot is TN
	).

% Return the #of stones for both players
getstones(State,C1,C2) :- 
	countstones(State,1,C1),
	countstones(State,2,C2).

winner(State,Player) :- 
	terminal(State), !,
	getstones(State,P1,P2), !,
	P1 =\= P2, !,
	(P1 < P2 
		-> Player is 1
		;  Player is 2	
	).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
tie(State) :- 
	terminal(State), !,
	countstones(State,1,P1),
	countstones(State,2,P2),
	P1==P2.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   
invalid_moves([]).
invalid_moves([null]).
invalid_moves(['n']).
terminal(State) :- 
	moves(1,State,LiP), 
	moves(2,State,LiR),
	invalid_moves(LiP),
	invalid_moves(LiR).


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
moves(Player,State,MyList) :- 
	findall([X,Y], ( validmove(Player,State,[X,Y]) ), MvL),
	( MvL == [] -> MyList = [null] ; list_to_set(MvL,MyList)).

move(X,Y,DX,DY,X2,Y2) :- 
	X2 is X + DX, Y2 is Y + DY, 
	X2 >= 0, X2 < 6, 
	Y2 >= 0, Y2 < 6. 

in_bounds(X2,Y2) :-
	X2 >= 0, X2 < 6, 
	Y2 >= 0, Y2 < 6. 

% T/F if path from P->P exists
can_flip_path(Player,[X,Y],[DX,DY],State) :-
	move(X,Y,DX,DY,X2,Y2),
	opponent(Player,Opponent),
	get(State,[X2,Y2],Tile),
	( Tile==Opponent -> can_flip_path_step(Player,[X2,Y2],[DX,DY],State)
	; false
	).
	
can_flip_path_step(Player,[X,Y],[DX,DY],State) :-
	move(X,Y,DX,DY,X2,Y2),
	get(State,[X2,Y2],Tile),
	opponent(Player,Opponent),
	( Tile == Player -> true
	; Tile == '.' -> false
	; can_flip_path_step(Player,[X2,Y2],[DX,DY],State) 
	).

is_adjacent_opponent(Player,[X,Y],[DX,DY],State) :- 
	move(X,Y,DX,DY,X2,Y2), opponent(Player,Opponent), get(State,[X2,Y2],Opponent).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Player,[X,Y],State,NewState,NextPlayer) :-
	opponent(Player,NextPlayer),
	set(State,State1,[X,Y],Player),
	Dirs = [[-1,0],[1,0],[0,-1],[0,1],[-1,-1],[-1,1],[1,-1],[1,1]],
	dir(DX,DY),
	trace_flip_path(Player,[X,Y],[DX,DY],State1,NewState).

nextState(Player,'n',State,State,NextPlayer) :- % Skip action
	opponent(Player, NextPlayer).
nextState(Player,null,State,State,NextPlayer) :- % Skip action (computer gives null instead of 'n'...)
	opponent(Player, NextPlayer).

trace_flip_path(_,_,[],S,S).
trace_flip_path(Player,[X,Y],[Dir|T],State,NewState) :-
	trace_flip_path_step(Player,[X,Y],Dir,[],State,ResState),
	trace_flip_path(Player,[X,Y],T,ResState,NewState).

trace_flip_path_step(Player,[X,Y],[DX,DY],Trace,State,NewState) :-
	move(X,Y,DX,DY,X2,Y2),
	( not(in_bounds(X2,Y2)) -> true ; % move_
		get(State,[X2,Y2],Tile),
		opponent(Player,Opponent),
		( Tile == Player -> retrace_path(Player,Trace,State,NewState)
		; Tile == Opponent -> trace_flip_path_step(Player,[X2,Y2],[DX,DY],[[X2,Y2]|Trace],State,NewState)
		; NewState = State
	)).

retrace_path(_,[],State,State).
retrace_path(Player,[H|T],State,NewState) :-
	set(State, State1, H, Player),
	retrace_path(Player,T,State1,NewState).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.
validmove(Player,State,[X,Y]) :- 
	between(0,5,X),
	between(0,5,Y),
	get(State, [X,Y], '.'),
	% Dirs = [[-1,0],[1,0],[0,-1],[0,1],[-1,-1],[-1,1],[1,-1],[1,1]],
	dir(DX,DY),
	is_adjacent_opponent(Player,[X,Y],[DX,DY],State),	
	can_flip_path(Player,[X,Y],[DX,DY],State).
	

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
h(State,Val):-
    (winner(State,1) -> Val is -100
		; ( winner(State,2) -> Val is 100
				; tie(State) -> Val is 0
		)
    ).

% From paper, swapping V1 for V2 to flip sign
h(State,Val) :-
	countstones(State,1,V1),
	countstones(State,2,V2),
	Val is V2 - V1.


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
 