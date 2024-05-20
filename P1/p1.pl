% State: (RobotRoom, SKRoom, BKRoom, PRoom)
% change_state: (State, move, NewState)
% key states: holding,r1,r2,r3
% robot room states: r1,r2,r3

% Robot can only pick up if a #[SK,BK,P] < 2
too_many_items(state(_,holding,holding,holding)).



% Robot movement
change_state( % R1 -> R2
    state(r1,holding,BK,P),
    move(r2),
    state(r2,holding,BK,P)
).

change_state( % R1 -> R3
    state(r1,SK,holding,P),
    move(r3),
    state(r3,SK,holding,P)
).

change_state( % R2 -> R1
    state(r2,holding,BK,P),
    move(r1),
    state(r1,holding,BK,P)
).

change_state( % R3 -> R1
    state(r3,SK,holding,P),
    move(r1),
    state(r1,SK,holding,P)
).



% Robot pickup action
change_state( % Pick up BK
    state(R,SK,R,P),
    pickup(bk),
    NewState ) :- 
        NewState = state(R,SK,holding,P),
        not( too_many_items(NewState) ).

change_state( % Pick up SK
    state(R,R,BK,P),
    pickup(sk),
    NewState ) :- 
        NewState = state(R,holding,BK,P),
        not( too_many_items(NewState) ).

change_state( % Pick up package
    state(R,SK,BK,R),
    pickup(package),
    NewState ) :- 
        NewState = state(R,SK,BK,holding),
        not( too_many_items(NewState) ).




change_state( % Drop BK
    state(R,SK,holding,P),
    drop(bk),
    state(R,SK,R,P)
).

change_state( % Drop SK
    state(R,holding,BK,P),
    drop(sk),
    state(R,R,BK,P)
).

change_state( % Drop package
    state(R,SK,BK,holding),
    drop(package),
    state(R,SK,BK,R)
).


% Goal
traverse((r2,_,_,r2),_,[done|_],_) :- !. % Cut to quit searching
traverse(State,N,Trace,NodeTrace) :-
    N > 0,                                                  % depth-bounding
    change_state(State, ChangeState, NewState),                          
    not(member(NewState, [State|NodeTrace])),
    NewN is N - 1,
    traverse(NewState,NewN,TraceNext,[NewState,State|NodeTrace]),
    Trace = [TraceNext | Trace].


% Depth-bounded DFS solver with node tracing
solveR(State,N,Trace) :-
    traverse(State,N,Trace,[]).

% solveR(state(r1,r1,r2,r3),10,Trace).

package_recovered( state(room2, _, _, has), _, [done]) :- !.
package_recovered( CurrentState, N, [Action|Trace]) :-
  N >= 0,
  N2 is N - 1,
  change_state(CurrentState, Action, NextState),
  package_recovered(NextState, N2, Trace).


%%%%%%%%%%%%%%%%%%%%%%%%%
%     User functions    %
%%%%%%%%%%%%%%%%%%%%%%%%%
tell_trace([]).
tell_trace([T|Trace]) :-
  write_ln(T),
  tell_trace(Trace).

findpath :- findpath(1).
findpath(Steps) :-
  package_recovered(state(room1, room2, room1, room3), Steps, Trace),
  tell_trace(Trace).
findpath(Steps) :- 
  S2 is Steps + 1,
  findpath(S2).