m(X, Y, Y) :- X =< Y, ! .
m(X, Y, X).
h(X, Y) :- g(X), !, f(Y).
h(p, q).
f(a).
f(b).
g(c).
g(d).

% m(2, 3, 2).

% m(2, 3, Z).

% h(X, Y).

% h(p, Y).
