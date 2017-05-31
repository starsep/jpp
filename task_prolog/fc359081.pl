% Filip Czaplicki

% createLR(+Grammar, -Automat, -Info)
createLR(G, A, I) :-
  I = yes,
  A = null.

% accept(+Automat, +Word)
accept(A, W).
