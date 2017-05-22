edge(a, b, graph1).
edge(b, c, graph1).
edge(c, a, graph1).
edge(g, h, graph1).

path1(A, B, G) :- edge(A, B, G).
path1(A, B, G) :- edge(A, C, G), path1(C, B, G).


nonmember(Arg, [Arg | _]) :- !, fail.
nonmember(Arg, [_ | Tail]) :- !, nonmember(Arg,Tail).
nonmember(_ , []).

path2h(A, B, _, G) :- edge(A, B, G).
path2h(A, B, V, G) :-
    edge(A, C, G),
    nonmember(C, V),
    path2h(C, B, [V | C], G).

path2(A, B, G) :- path2h(A, B, [A], G).

addvertex(X, L, R) :- member(X, L), !, L=R.
addvertex(X, L, [X | L]).


