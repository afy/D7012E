getSubsets([],[]).
getSubsets([H|T], Subsets) :-
    writeln(H),
    getSubsets(T, Subsets).

sortSubsets([H|T], R).

getFirstKSets().

getKsubsets(Source, K, Res) :-
    getSubsets(Source, Subsets).
