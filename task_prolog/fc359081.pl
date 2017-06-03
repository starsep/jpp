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
  G = gramatyka(S, P),
  clojure([item('Z', [nt(S), #], 0)], P, InitState),
  debugItems(InitState),
  symbols(P, Symbols),
  runGoto(Symbols, InitState, P),
  T = null,
  I = yes.

% runGoto(+Symbols, +Items, ProductionsList)
runGoto([], _, _).
runGoto([H | T], I, P) :-
  goto(I, H, P, R),
  write('runGoto '),
  write(H),
  write('\n'),
  debugItems(R),
  runGoto(T, I, P).


% createAutomatonWithTable(+Grammar, +Table, -Automaton, -Info)
createAutomatonWithTable(G, T, A, I) :-
  A = null,
  I = yes.

% makeConflict(+ErrorMessage, -Automaton, -Info)
makeConflict(E, A, I) :-
  I = konflikt(E),
  A = null.

% symbols(+ProductionsList, -Symbols)
symbols([], S) :- S = [].
symbols([H | T], S) :-
  symbols(T, ST),
  H = prod(L, P),
  append(P, PJoin),
  append([PJoin, [nt(L)], ST], SJoin),
  remove_dups(SJoin, S).

% clojure(+Items, +ProductionsList, -ClojureOfItems)
clojure(I, P, C) :-
  clojureOnce(I, P, C1),
  clojureOnce(C1, P, C2),
  ( C1 == C2 -> C = C2 ; clojure(C2, P, C) ).

% clojureOnce(+Items, +ProductionsList, -ClojureOfItems)
clojureOnce([], _, C) :- C = [].
clojureOnce([H | T], P, C) :-
  clojureItem(H, P, CH),
  clojureOnce(T, P, CT),
  append(CH, CT, CJoin),
  remove_dups(CJoin, C).

% clojureItem(+Item, +ProductionsList, -ClojureOfItem)
clojureItem(I, P, C) :-
  I = item(_, R, Index),
  length(R, Len),
  ( Len =< Index -> C = [I] ;
    nth0(Index, R, L),
    itemsFromProductions(P, L, NewItems),
    append([I], NewItems, C)
  ).

% itemsFromProductions(+ProductionsList, +Symbol, -Items)
itemsFromProductions([], _, I) :- I = [].
itemsFromProductions([H | T], S, I) :-
  H = prod(L, R),
  ( nt(L) = S ->
    rightSidesToItems(R, L, I)
  ;
    itemsFromProductions(T, S, I)
  ).

% rightSidesToItems(+RightSidesList, +LeftSymbol, -Items)
rightSidesToItems([], _, I) :- I = [].
rightSidesToItems([H | T], L, I) :-
  rightSidesToItems(T, L, IT),
  Item = item(L, H, 0),
  I = [Item | IT].

% goto(+Items, +Symbol, +ProductionsList, -ResultItems)
goto(I, S, P, Res) :-
  moveItems(I, S, ResM),
  clojure(ResM, P, Res).

% moveItems(+Items, +Symbol, -ResultItems)
moveItems([], _, R) :- R = [].
moveItems([H | T], S, Res) :-
  moveItems(T, S, ResT),
  H = item(L, R, Index),
  length(R, Len),
  ( Index < Len, nth0(Index, R, S) ->
    Index1 is Index + 1,
    Res = [item(L, R, Index1) | ResT]
  ;
    Res = ResT
  ).

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
  debugProduction(R, L),
  debugProductions(T).

% debugProduction(+RightSidesList, +LeftSymbol)
debugProduction([], _).
debugProduction([H | T], L) :-
  write(L),
  write(' -> '),
  debugRightSides(H),
  write('\n'),
  debugProduction(T, L).

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

% debugItems(+Items)
debugItems([]).
debugItems([H | T]) :-
  H = item(L, R, Index),
  write(L),
  write(' -> '),
  debugRightSidesItem(R, Index),
  write('\n'),
  debugItems(T).

% debugRightSidesItem(+RightSidesList, +Length)
debugRightSidesItem([], L) :- ( L == 0 -> put_code(8226) ; true ).
debugRightSidesItem([H | T], L) :-
  ( L == 0 -> put_code(8226) ; true ),
  debugRightSides([H]),
  L1 is L - 1,
  debugRightSidesItem(T, L1).

% --------------------------- DEBUG -------------------------------------
