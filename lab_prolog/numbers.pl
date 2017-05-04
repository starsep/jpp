nat(zero).
nat(s(N)) :- nat(N).
even(zero).
even(s(s(N))) :- even(N).
odd(s(N)) :- even(N).

plus(zero, X, X).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).
