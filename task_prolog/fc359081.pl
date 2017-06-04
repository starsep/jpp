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
  clojure([item('Z', [nt(S), #], 0)], P, InitItem),
  rightSymbols(P, Symbols),
  runGoto(Symbols, [InitItem], P, [], _States, T, I).
  % ----- DEBUG
  % write(States),
  % write('\n'),
  % debugStates(States),
  % write('\n'),
  % ----- DEBUG

% runGoto(+Symbols, +States, +Productions, +Table, -ResStates, -ResTable, -Info)
runGoto(Symbols, States, P, Table, RStates, RTable, Info) :-
  % debugStates(States),
  % runGotoOnce(Symbols, States, P, Table, RStates, RTable, Info).
  % debugStates(RStates).
  runGotoOnce(Symbols, States, P, Table, States2, Table2, I),
  ( I \= yes ->
    Info = I
  ;
    runGotoOnce(Symbols, States2, P, Table2, RStates, RTable, Info),
    debugStates(RStates)
  ),
  debugTable(RTable).

% runGotoOnce(+Symbols, +States, +Productions, +Table, -RStates, -RTable, -Info)
runGotoOnce([], States, _, Table, RStates, RTable, Info) :-
  RStates = States,
  RTable = Table,
  Info = yes.

runGotoOnce([H | T], States, P, Table, RStates, RTable, Info) :-
  % debugStates(States),
  runGotoSymbol(States, States, H, P, Table, RS, RT, I),
  % debugStates(RS),
  % write('--------------------\n'),
  ( Info \= yes ->
    Info = I,
    RTable = RT,
    RStates = RS
  ;
    runGotoOnce(T, RS, P, RT, RStates, RTable, Info)
  ).

% runGotoSymbol(+States, +AllS, +Symbol, +P, +Table, -RStates, -RTable, -Info)
runGotoSymbol([], AllS, _, _, Table, RStates, RTable, Info) :-
  RStates = AllS,
  RTable = Table,
  Info = yes.

runGotoSymbol([H | T], AllS, S, P, Table, RStates, RTable, Info) :-
  % debugItems(H),
  goto(H, S, P, To),
  addStateToStateList(AllS, To, AllS2),
  % % ----------- DEBUG
  % write(S),
  % write('>>>>>>>>>\n'),
  % debugItems(To),
  % write('<<<<<<<<<\n'),
  % % ----------- DEBUG
  ( To == [] ->
    Table2 = Table,
    I = yes
  ;
    nth0(Index0, AllS2, H),
    nth0(Index1, AllS2, To),
    addElemToTable(Table, (Index0, S, Index1), Table2, I)
  ),
  ( I \= yes ->
    Info = I,
    RStates = [],
    RTable = []
  ;
    runGotoSymbol(T, AllS2, S, P, Table2, RStates, RTable, Info)
  ).

% addStateToStateList(+List, +State, -Result)
addStateToStateList(L, [], R) :- R = L.
addStateToStateList(L, S, R) :-
  ( member(S, L) ->
    R = L
  ;
    append([L, [S]], R)
  ).

% checkTableConflict(+Table, +Element)
checkTableConflict([], _).
checkTableConflict([H | T], E) :-
  H = (A, S, B),
  E = (X, C, Y),
  ( A == X, S == C, B \= Y ->
    fail
  ;
    checkTableConflict(T, E)
  ).

% addElemToTable(+Table, +Element, -ResultTable, -Info)
addElemToTable(T, E, R, I) :-
  ( checkTableConflict(T, E) ->
    append([T, [E]], R0),
    remove_dups(R0, R),
    % write(R),
    I = yes
  ;
    makeConflict('TODO: change, konflikt tabeli :c', R, I)
  ).


% createAutomatonWithTable(+Grammar, +Table, -Automaton, -Info)
createAutomatonWithTable(_G, _T, A, I) :-
  A = null,
  I = yes.

% makeConflict(+ErrorMessage, -Automaton, -Info)
makeConflict(E, A, I) :-
  I = konflikt(E),
  A = null.

% rightSymbols(+ProductionsList, -Symbols)
rightSymbols([], S) :- S = [].
rightSymbols([H | T], S) :-
  rightSymbols(T, ST),
  H = prod(_, P),
  append(P, PJoin),
  append([PJoin, ST], SJoin),
  remove_dups(SJoin, SUniq),
  delete(SUniq, #, S).

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

% goto(+From, +Symbol, +ProductionsList, -To)
goto(F, S, P, To) :-
  moveItems(F, S, Res),
  clojure(Res, P, To).

% indexOf(+List, +Element, -Index)
indexOf([E | _], E, I) :- I = 0.
indexOf([_ | T], E, I) :-
  indexOf(T, E, Index),
  I is Index + 1.

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

% tableSymbols(+Table, -Symbols)
tableSymbols([], S) :- S = [].
tableSymbols([H | T], S) :-
  (_, X, _) = H,
  tableSymbols(T, S1),
  remove_dups([X | S1], S).

% tableToRectTable(+Table, -Symbols, -RectTable)
tableToRectTable(T, S, R) :-
  tableSymbols(T, S),
  tableRows(T, N),
  convertTable(T, S, N, R).

% tableRows(+Table, -Rows)
tableRows(T, R) :-
  tableRowsH(T, R1),
  R is R1 + 1.

% tableRowsH(+Table, -Rows)
tableRowsH([], 0).
tableRowsH([H | T], R) :-
  (X, _, Y) = H,
  tableRowsH(T, R1),
  R2 is max(X, Y),
  R is max(R1, R2).

% convertTable(+Table, +Symbols, +NRows, -RectTable)
convertTable([], S, N, R) :-
  length(S, L),
  emptyTable(N, L, R).
convertTable([H | T], S, N, R) :-
  convertTable(T, S, N, R1),
  (X, C, Y) = H,
  nth0(Q, S, C),
  changeElem(R1, X, Q, Y, R).

% changeElem(+List, +X, +Y, +Value, -Result)
changeElem([H | T], 0, Y, V, R) :-
  changeElemRow(H, Y, V, R1),
  R = [R1 | T].
changeElem([H | T], X, Y, V, R) :-
  X1 is X - 1,
  changeElem(T, X1, Y, V, R1),
  R = [H | R1].

% changeElemRow(+List, +Y, +Value, -Result)
changeElemRow([_ | T], 0, V, R) :-
  R = [V | T].
changeElemRow([H | T], Y, V, R) :-
  Y1 is Y - 1,
  changeElemRow(T, Y1, V, R1),
  R = [H | R1].

% emptyTable(+N, +M, -Table)
emptyTable(0, _, []).
emptyTable(N, M, T) :-
  N1 is N - 1,
  emptyTable(N1, M, T1),
  emptyRow(M, R),
  T = [R | T1].

emptyRow(0, []).
emptyRow(L, R) :-
  L1 is L - 1,
  emptyRow(L1, R1),
  R = ['.' | R1].


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
  write('\t'),
  write(L),
  write(' -> '),
  debugRightSides(H),
  write('\n'),
  debugProduction(T, L).

% debugRightSides(+RightSidesList)
debugRightSides([]).
debugRightSides([H | T]) :-
  debugSymbol(H),
  debugRightSides(T).

% debugSymbol(+Symbol)
debugSymbol(S) :-
  ( nt(N) = S ->
    write(N)
  ;
    write(S)
  ).

% debugItems(+Items)
debugItems([]).
debugItems([H | T]) :-
  H = item(L, R, Index),
  write('\t'),
  write(L),
  write(' -> '),
  debugRightSidesItem(R, Index),
  write('\n'),
  debugItems(T).

% debugStates(+States)
debugStates(S) :- debugStates(S, 0).

% debugStates(+States, +Index)
debugStates([], _).
debugStates([H | T], I) :-
  write(I),
  write(': {\n'),
  debugItems(H),
  write('}\n'),
  I1 is I + 1,
  debugStates(T, I1).

% debugRightSidesItem(+RightSidesList, +Length)
debugRightSidesItem([], L) :- ( L == 0 -> put_code(8226) ; true ).
debugRightSidesItem([H | T], L) :-
  ( L == 0 -> put_code(8226) ; true ),
  debugRightSides([H]),
  L1 is L - 1,
  debugRightSidesItem(T, L1).

% debugHeader(+Header)
debugHeader([]) :- write('\n').
debugHeader([H | T]) :-
  write('  '),
  debugSymbol(H),
  debugHeader(T).


% debugTable(+Table)
debugTable(T) :-
  tableToRectTable(T, S, R),
  write(' '),
  debugHeader(S),
  debugRows(R, 0).

% debugRows(+Rows, +Index)
debugRows([], _).
debugRows([H | T], I) :-
  write(I),
  debugHeader(H),
  I1 is I + 1,
  debugRows(T, I1).

% --------------------------- DEBUG -------------------------------------
