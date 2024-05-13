% State struct:     state(Room_Robot, Item_Rooms, Hands) 
% Items_rooms:      [Steel_key, Brass_key, Package]
% Hands:            [Left_hand, Right_hands]

% Move struct:  move(State1, Item_Requirement, Move, State2)



% Movement
change_state(
    state(r1, [_,_,_], Hands), member(steel_key, Hands), 
    move_room,
    state(r2, [_,_,_], Hands)
)




move(
    state(r1, [_,_,_], Hands), member(brass_key, Hands), 
    move_rooms,
    state(r3, [_,_,_], Hands)
)
move(
    state(r2, [_,_,_], Hands), member(steel_key, Hands), 
    move_rooms,
    state(r1, [_,_,_], Hands)
)
move(
    state(r3, [_,_,_], Hands), member(brass_key, Hands), 
    move_rooms,
    state(r1, [_,_,_], Hands)
)
 


% Item handling
move(
    state(R, ), member(steel_key, Hands), 
    grab_item,
    state()
)
move(
    state(), _, 
    drop_item,
    state()
)



% Goal + code run
can_find(State, [T | Trace]) :-
    ...
    ...


can_find(
    state(r1, r2, [empty, empty]), Trace
)