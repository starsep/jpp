% split(C, E, L, R) L + E + R = C
split([], _, [], []).
split([A | X], E, [A | L], R) :- A < E, split(X, E, L, R).
split([A | X], E, L, [A | R]) :- A >= E, split(X, E, L, R).

qsort([], []).
qsort([E | X], Z) :-
  split(X, E, L, R),
  qsort(L, L2),
  qsort(R, R2),
  append(L2, [E | R2], Z).

qsort_acc([], A, A).
qsort_acc([E | X], A, U) :-
  split(X, E, L, R),
  qsort_acc(R, A, R2),
  qsort_acc(L, [E | R2], U).

qsort2(X, Y) :- qsort_acc(X, [], Y).
