nat(z).
nat(s(N)) :- nat(N).
p(X, s(X)) :- nat(X).
le(X, X) :- nat(X).
le(X, s(Y)) :- le(X, Y).
lt(X, s(X)) :- nat(X).
lt(X, Y) :- lt(X, Z), lt(Z, Y).
l4(X, s(X)) :- nat(X), !.
l4(X, Y) :- l4(X, Z), l4(Z, Y).

% p(s(z), s(s(z))).

% le(s(z), s(s(z))).

% lt(s(z), s(s(z))).

% l4(s(z), s(s(z))).
