% Filip Czaplicki

:- use_module(library(lists)).

% createLR(+Grammar, -Automat, -Info)
createLR(G, A, I) :-
  extendGrammar(G, E),
  createAutomat(E, A, I).

% createAutomat(+Grammar, -Automat, -Info)
createAutomat(G, A, I) :-
  I = yes,
  A = null.

% accept(+Automat, +Word)
accept(A, W).

% extendGrammar(+Grammar, -ExtendedGrammar)
extendGrammar(G, E) :-
  G = gramatyka(S, L),
  E = gramatyka('Z', [prod('Z', [[nt(S), #]]) | L]),
  debugGrammar(E).

% --------------------------- DEBUG -------------------------------------

% debugGrammar(+Grammar)
debugGrammar(G) :-
  G = gramatyka(S, L),
  write('---\n'),
  write('Grammar: starting symbol = '),
  write(S),
  write('\n'),
  write('productions:\n'),
  debugProductions(L),
  write('---\n').

% debugProductions(+ProductionsList)
debugProductions([]).
debugProductions([H | T]) :-
  H = prod(L, R),
  debugProduction(L, R),
  debugProductions(T).

% debugProduction(+LeftSymbol, +RightSidesList)
debugProduction(_, []).
debugProduction(L, [H | T]) :-
  write(L),
  write(' -> '),
  debugRightSides(H),
  write('\n'),
  debugProduction(L, T).

% debugRightSides(+RightSidesList)
debugRightSides([]).
debugRightSides([H | T]) :-
  H = nt(N),
  !,
  write(N),
  debugRightSides(T).

debugRightSides([H | T]) :-
  write(H),
  debugRightSides(T).

% --------------------------- DEBUG -------------------------------------
