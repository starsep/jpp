% Filip Czaplicki

:- use_module(library(lists)).

% createLR(+Grammar, -Automaton, -Info)
createLR(G, A, I) :-
  extendGrammar(G, E),
  createAutomaton(E, A, I).

% createAutomaton(+Grammar, -Automaton, -Info)
createAutomaton(G, A, I) :-
  buildTable(G, T, InfoTable),
  ( InfoTable \== yes ->
    makeConflict(InfoTable, A, I)
  ;
    createAutomatonWithTable(G, T, A, I)
  ).

% buildTable(+Grammar, -Table, -Info)
buildTable(G, T, I) :-
  G = gramatyka(S, L),
  clojure([item('Z', [S, #], 0)], L, InitState),
  write(InitState),
  T = null,
  I = yes.

% createAutomatonWithTable(+Grammar, +Table, -Automaton, -Info)
createAutomatonWithTable(G, T, A, I) :-
  A = null,
  I = yes.

% makeConflict(+ErrorMessage, -Automaton, -Info)
makeConflict(E, A, I) :-
  I = konflikt(E),
  A = null.

% clojure(+Items, +ProductionsList, -ClojureOfItems)
clojure(I, P, C) :-
  clojureOnce(I, P, C1),
  clojureOnce(C1, P, C2),
  ( C1 == C2 -> C = C2 ; clojure(C2, P, C) ).

% clojureOnce(+Items, +ProductionsList, -ClojureOfItems)
clojureOnce([], _, C) :-
  C = [].

clojureOnce([H | T], P, C) :-
  clojureItem(H, P, CH),
  clojureOnce(T, P, CT),
  append(CH, CT, C).

% clojureItem(+Item, +ProductionsList, -ClojureOfItem)
clojureItem(I, P, C) :-
  I = item(_, R, Index),
  length(R, Len),
  ( Len =< Index -> C = [I] ;
    nth0(Index, R, L),
    itemsFromProductions(L, P, NewItems),
    append([I], NewItems, C),
    write(C)
  ).

% itemsFromProductions(+Symbol, +ProductionsList, -Items)
itemsFromProductions(_, [], I) :- I = [].
itemsFromProductions(S, [H | T], I) :-
  H = prod(L, R),
  ( L = S ->
    rightSidesToItems(L, R, I)
  ;
    itemsFromProductions(S, T, I)
  ).

% rightSidesToItems(+LeftSymbol, +RightSidesList, -Items)
rightSidesToItems(_, [], I) :- I = [].
rightSidesToItems(L, [H | T], I) :-
  rightSidesToItems(L, T, IT),
  Item = item(L, H, 0),
  I = [Item | IT].


% accept(+Automaton, +Word)
accept(_A, _W).

% extendGrammar(+Grammar, -ExtendedGrammar)
extendGrammar(G, E) :-
  G = gramatyka(S, L),
  E = gramatyka(S, [prod('Z', [[nt(S), #]]) | L]).

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
