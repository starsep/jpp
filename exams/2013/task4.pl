f(a) :- fail, !.
f(b).
g(X) :- f(X), !, fail.
g(X).

% f(X).

% f(b).

% g(a).

% f(X), g(X).
