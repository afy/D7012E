% Simulate robot
% Task: Implement states and solver SolveR for a robot 
% Moving between rooms and delivering a package.
% Author: Hannes Furhoff, hanfur-0@student.ltu.se

%%%%%%%%%%% Reference %%%%%%%%%%%%%%%%%%%%%%%
% State: (RobotRoom, SKRoom, BKRoom, PRoom)
% change_state: (State, move(atom), NewState),   NewState = Move(State)
% key/package states: holding,r1,r2,r3
% robot room states: r1,r2,r3

% Robot can only pick up if a #[SK,BK,P] < 2
% Note: required to be inverted (not) in practice for this task
too_many_items(holding,holding,holding).



% Robot movement
change_state( % R1 -> R2
    state(r1,holding,BK,P),
    move_r1_to_r2,
    state(r2,holding,BK,P)
).

change_state( % R2 -> R1
    state(r2,holding,BK,P),
    move_r2_to_r1,
    state(r1,holding,BK,P)
).

change_state( % R1 -> R3
    state(r1,SK,holding,P),
    move_r1_to_r3,
    state(r3,SK,holding,P)
).

change_state( % R3 -> R1
    state(r3,SK,holding,P),
    move_r3_to_r1,
    state(r1,SK,holding,P)
).



% Robot pickup
change_state( % Pick up SK
    state(R,R,BK,P),
    pickup_steel_key,
    state(R,holding,BK,P)
) :- not( too_many_items(holding,BK,P)).

change_state( % Pick up BK
    state(R,SK,R,P),
    pickup_brass_key,
    state(R,SK,holding,P) 
) :- not( too_many_items(SK,holding,P) ).

change_state( % Pick up package
    state(R,SK,BK,R),
    pickup_package,
    state(R,SK,BK,holding)
) :- not( too_many_items(SK,BK,holding) ).



% Robot drop 
change_state( % Drop SK
    state(R,holding,BK,P),
    drop_steel_key,
    state(R,R,BK,P)
).

change_state( % Drop BK
    state(R,SK,holding,P),
    drop_brass_key,
    state(R,SK,R,P)
).

change_state( % Drop package
    state(R,SK,BK,holding),
    drop_package,
    state(R,SK,BK,R)
).



% Goal: Package is in r2
traverse(state(_,_,_,r2),_,[done],_). 
traverse(State,N,Trace,NodeTrace) :-
    N > 0,                                                  % Check depth                      
    change_state(State, ChangeState, NewState),             % Try action
    not(member(NewState, NodeTrace)),                       % Prevent cycles
    NewN is N - 1,                                          % Decrement depth counter
    traverse(NewState,NewN,TraceNext,[State|NodeTrace]),    % Continue to next node
    Trace = [ ChangeState | TraceNext ].                    % Store move in trace log

% Print out the trace
showtrace([]).
showtrace([H|T]) :-
    write('-> '), writeln(H),
    showtrace(T).

% Depth-bounded DFS solver with node tracing
solveR(State,N,Trace) :-
    traverse(State,N,Trace,[]),
    showtrace(Trace).

% Experimenting found that 12 is the minimum # of steps required to find
% solveR(state(r1,r1,r2,r3),12,Trace).