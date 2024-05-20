robot_is_overloaded(state(_, has, has, has)).

%%%%%%%%%%%%%%%%%%%%%%%%%
%   Move between rooms  %
%%%%%%%%%%%%%%%%%%%%%%%%%
% R1 => R2
move( state(room1, BKey, has, Package),
      moveto(room2),
      state(room2, BKey, has, Package)).

% R1 => R3
move( state(room1, has, SKey, Package),
      moveto(room3),
      state(room3, has, SKey, Package)).

% R2 => R1
move( state(room2, BKey, has, Package),
      moveto(room1),
      state(room1, BKey, has, Package)).

% R3 => R1
move( state(room3, has, SKey, Package),
      moveto(room1),
      state(room1, has, SKey, Package)).


%%%%%%%%%%%%%%%%%%%%%%%%%
%     Pickup objects    %
%%%%%%%%%%%%%%%%%%%%%%%%%
% Brass key
move( state(R, R, SKey, Package),
      pickup(brassKey),
      NextState) :-
        NextState = state(R, has, SKey, Package),
        not(robot_is_overloaded(NextState)).

% Steel key
move( state(R, BKey, R, Package),
      pickup(steelKey),
      NextState) :-
        NextState = state(R, BKey, has, Package),
        not(robot_is_overloaded(NextState)).

% Package
move( state(R, BKey, SKey, R),
      pickup(package),
      NextState) :-
        NextState = state(R, BKey, SKey, has),
        not(robot_is_overloaded(NextState)).


%%%%%%%%%%%%%%%%%%%%%%%%%
%      Drop objects     %
%%%%%%%%%%%%%%%%%%%%%%%%%
% Brass key
move( state(R, has, SKey, Package),
      drop(brassKey),
      state(R, R, SKey, Package)).

% Steel key
move( state(R, BKey, has, Package),
      drop(steelKey),
      state(R, BKey, R, Package)).

% Package
move( state(R, BKey, SKey, has),
      drop(package),
      state(R, BKey, SKey, R)).
