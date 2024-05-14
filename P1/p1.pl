% State struct:     state(Room_Robot, Item_Rooms, Hands) 
% Items_rooms:      [Steel_key, Brass_key, Package]
% Hands:            [Left_hand, Right_hand]
% Move struct:  move(State1, Move, State2)



% Return a list of item positions, given Item, List, Update_Value (V), NewList (return R) 
itemlist(steel_key, L, V, R) :- [_ | [Mid | [Tail]]] = L, R = [V, Mid, Tail].
itemlist(brass_key, L, V, R) :- [Head | [_ | [Tail]]] = L, R = [Head, V, Tail].
itemlist(package, L, V, R)   :- [Head | [Mid | [_]]] = L, R = [Head, Mid, V].

% Check if Item (var 0) is in room R given L
checkroom(steel_key, L, R) :- [Head | _ ] = L, R == Head.
checkroom(brass_key, L, R) :- [_ | [Mid | _]] = L, R == Mid.
checkroom(package, L, R)   :- [_| [_ | [Tail]]] = L, R == Tail.

% Check if atom=empty
isempty(empty).



% Movement
move(
    state(r1, L, H),
    move_room,
    state(r2, L, H)
) :- member(steel_key, H), writeln("R1 -> R2").

move(
    state(r2, L, H), 
    move_rooms,
    state(r1, L, H)
) :- member(steel_key, H), writeln("R2 -> R1").

move(
    state(r1, L, H), 
    move_rooms,
    state(r3, L, H)
) :- member(brass_key, H), writeln("R1 -> R3").

move(
    state(r3, L, H),  
    move_rooms,
    state(r1, L, H)
) :- member(brass_key, H), writeln("R3 -> R1").




% Item drop
move(
    state(R, L, H), 
    drop_item_left,
    state(R, Newlist, NewHands)
) :- [I | Tail] = H, itemlist(I, L, R, Newlist), NewHands = [empty | Tail].

move(
    state(R, L, H), 
    drop_item_right,
    state(R, Newlist, NewHands)
) :- [Head | [I]] = H, itemlist(I, L, R, Newlist), NewHands = [Head | [empty]].




 % Item pickup
 move(
    state(R, L, H),
    pickup_item_left(Item),
    state(R, NewList, NewHands)
 ) :- [I | Tail] = H, isempty(I), checkroom(Item, L, R),
      itemlist(Item, L, empty, NewList), NewHands = [Item | Tail].

 move(
    state(R, L, H),
    pickup_item_right(Item),
    state(R, NewList, NewHands)
 ) :- [Head | [I]] = H, isempty(I), checkroom(Item, L, R),
      itemlist(Item, L, empty, NewList), NewHands = [Head | Item].



invalid(state(_,A,A,B)) :-
    A \== B.
invalid(state(A,A,_,B)) :-
    A \== B.


% Run DFS
package_delivered(r2, [_,_,r2], [done| []]).
package_delivered(State, N, Trace) :-
    N > 0,
    move(State, Move, NewState),
    \+(invalid(NewState)),
    package_delivered(NewState, N-1, TraceCo),
    Trace = [Move | TraceCo].

% package_delivered(state(r1, [r1, r2, r3], [empty, empty]), 13, Trace).