% Smallest K-List (Prolog ver.)
% Task: K-list implementation from H1 in prolog
% Author: Hannes Furhoff, hanfur-0@student.ltu.se

% Generate list of all indices [i,j] for subsets of list
getIndices([],[]).
getIndices(L, Slices) :-
    length(L, LLen), ListLen is LLen-1,
    findall(N, between(0, ListLen, N), Range),  % Generate range [0..len(L)-1]

    % Generate all possible slice indices [i,j]
    % Where i=[0..len(L)-1], j=[0..len(L)-1], and i<=j 
    % With list_to_set to remove duplicates (just to be safe).
    findall([I,J], (
        member(I, Range),
        member(J, Range),
        I =< J
    ), Res), list_to_set(Res, Slices).



% Get a slice orig[i:j]
getSlice(Orig,[I,J],Slice) :- 
    findall(V, (
        nth0(Index, Orig, V),
        Index >= I,
        Index =< J
    ), Slice).
    


% From the previous list of indices, get all slices
% Note: Orig is not modified, just used to slice
getSubsetsFromIndices(_,[],[]).
getSubsetsFromIndices(Orig, [[I,J]|T], Subsets) :- 
    getSubsetsFromIndices(Orig, T, RecRes),
    getSlice(Orig,[I,J], Subsliced),
    append(RecRes, [[I,J,Subsliced]], Subsets).



% Get sum of list elements 
getListSum([],0).
getListSum([H|T], Sum) :-
    getListSum(T, RecSum),
    Sum is RecSum + H.



% Sort a given list of subsets according to sum(V) 
% Return with sum(V) in sublist in accordance to H1 presentation feedback
sortSubsets([], []).
sortSubsets([[I,J,V]|T], R) :-
    sortSubsets(T, RecRes),
    getListSum(V, VSum),

    % Get all previous results size(rec) <= size(V)
    findall([V2Sum,I2,J2,V2], (
        member([V2Sum,I2,J2,V2], RecRes),
        VSum >= V2Sum
    ), Lower),

    % Get all previous results size(rec) > size(V)
    findall([V2Sum,I2,J2,V2], (
        member([V2Sum,I2,J2,V2], RecRes),
        VSum < V2Sum
    ), Upper),
    
    % Concat Lower : [V] : upper
    append(Lower, [[VSum,I,J,V]], R1),
    append(R1, Upper, R).



% Printout the first k sets in desc order (lowest size highest up in list)
printFirstKSets([[VSum,I,J,V]|T], K) :-
    K > 0,
    AdjI is I + 1, AdjJ is J + 1,                   % Adjust indices for printing
    format('~w\t~w\t~w\t~w\n', [VSum,AdjI,AdjJ,V]),
    NewK is K - 1,
    printFirstKSets(T, NewK).



% Main
getKSubsets(Source, K) :-
    getIndices(Source, Indices),
    getSubsetsFromIndices(Source, Indices, Subsets),
    sortSubsets(Subsets, SortedSubsets),

    % Show results
    writeln('size\ti\tj\tsublist'),
    printFirstKSets(SortedSubsets, K).

% Test cases from H1 (Both pass)
% getKSubsets([24,-11,-34,42,-24,7,-19,21], 6).
% getKSubsets([3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3], 8).