% tree(T) "leaf", "node(L, X, R)"
tree(leaf).
tree(node(L, _, R)) :- tree(L), tree(R).

% insertBST(TREE, ELEMENT, RESULT)
insertBST(leaf, N, node(leaf, N, leaf)).
insertBST(node(L1, M, R), N, node(L2, M, R)) :- M > N, insertBST(L1, N, L2).
insertBST(node(L, M, R1), N, node(L, M, R2)) :- M =< N, insertBST(R1, N, R2).

existsBST(node(_, N, _), N).
existsBST(node(L, N, _), M) :- M > N, existsBST(L, M).
existsBST(node(_, N, R), M) :- N > M, existsBST(R, M).

%insertallBST(L, T, R)
insertallBST([], T, T).
insertallBST([E | L], T, R) :- insertBST(T, E, R1), insertallBST(L, R1, R).

% toList(T, L, R)
toList(leaf, L, L).
toList(node(L, N, R), A, O) :-
    toList(L, A, L2),
    L3 = [N | L2],
    toList(R, L3, O).

sortBST(L, U) :-
    insertallBST(L, leaf, R),
    toList(R, [], U).

leafs(leaf, A, A).
leafs(node(leaf, E, leaf), A, [E | A]).
leafs(node(L, _, R), A, A2) :-
    (L, R) \= (leaf, leaf),
    leafs(L, A, A1),
    leafs(R, A1, A2).
