mylist([]).
mylist([X | L]) :- mylist(L).

myfirst(E, [E | _]).

mylast(E, [E]).
mylast(E, [F | L]) :- mylast(E, L).

mymember(E, [E | _]).
mymember(E, [_ | L]) :- mymember(E, L).

myjoin([], L, L).
myjoin([E | L], K, [E | M]) :- myjoin(L, K, M).

mysufixes([], [[]]).
mysufixes([E | L], [[E | L], L2]) :- mysufixes(L, L2).

myintersect(L, K) :- mymember(E, L), mymember(E, K).

mydivide([], [], []).
mydivide([H | L], [H | O], E) :- mydivide(L, E, O).

mysublist([], L).
mysublist(S, L) :- myjoin(Z, _, L), myjoin(_, S, Z).
