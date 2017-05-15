nat(z).
nat(s(N)) :- nat(N).
even(z).
even(s(s(N))) :- even(N).
odd(s(N)) :- even(N).

plus(z, X, X).
plus(s(X), Y, s(z)) :- plus(X, Y, Z).

minus(X, Y, z) :- plus(Y, Z, X).

fib(z, s(z)).
fib(s(z), s(z)).
fib(s(s(N)), R) :- fib(N, X), fib(s(N), Y), plus(X, Y, R). 

fib2(z, s(z)).
fib2(s(z), s(z)).
fib2(s(s(X)), s(s(W))) :- plus(Y, Z, W), fib2(s(X), s(Y)), fib2(X, s(Z)).

fib3(X, Y) :- fibl(X, Y, _).
