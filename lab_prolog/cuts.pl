f(X, Y) :- g(X, Y).
f(X, Y) :- i(X, Y).
g(X, Y) :- h(X), j(Y), !.
g(a1, c1).
h(a2).
h(a3).
j(c2).
j(c3).
i(a4, c4).
