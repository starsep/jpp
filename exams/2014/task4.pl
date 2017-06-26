check(L, K) :- check(L, 0, K).

check([], K, K).
check([X|L], A, K) :-
    trans(X, KX),
    ( odd(KX), A1 is A + 1 ; A1 is A ),
    check(L, A1, K).
odd(s(0)).
odd(s(s(X))) :- odd(X).
trans(0, 0).
trans(N, s(K)) :- N1 is N-1, trans(N1, K).

% check([1, 3], K).

% check([2, 3], K).

% check([-1], K).
