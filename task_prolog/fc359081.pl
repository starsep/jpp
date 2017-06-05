% Filip Czaplicki

:- use_module(library(lists)).

% createLR(+Grammar, -Automaton, -Info)
createLR(G, A, I) :-
  extendGrammar(G, E),
  createAutomaton(E, A, I).

% createAutomaton(+Grammar, -Automaton, -Info)
createAutomaton(G, A, I) :-
  buildTable(G, Table, States, InfoTable),
  ( InfoTable \== yes ->
    makeConflict(InfoTable, A, I)
  ;
    createAutomatonWithTable(G, Table, States, A, I)
  ).

% buildTable(+Grammar, -Table, -States, -Info)
buildTable(G, T, States, I) :-
  gramatyka(S, P) = G,
  clojure([item('Z', [nt(S), #], 0)], P, InitItem),
  rightSymbols(P, Symbols),
  runGoto(Symbols, [InitItem], P, [], States, T, I).

% runGoto(+Symbols, +States, +Productions, +Table, -ResStates, -ResTable, -Info)
runGoto(Symbols, States, P, Table, RStates, RTable, Info) :-
  runGotoOnce(Symbols, States, P, Table, States2, Table2, I),
  ( I \== yes ->
    Info = I
  ;
    ( States == States2, Table == Table2 ->
      Info = I,
      RStates = States,
      RTable = Table
    ;
      runGoto(Symbols, States2, P, Table2, RStates, RTable, Info)
    )
  ).

% runGotoOnce(+Symbols, +States, +Productions, +Table, -RStates, -RTable, -Info)
runGotoOnce([], States, _, Table, RStates, RTable, Info) :-
  RStates = States,
  RTable = Table,
  Info = yes.

runGotoOnce([H | T], States, P, Table, RStates, RTable, Info) :-
  runGotoSymbol(States, States, H, P, Table, RS, RT, I),
  ( I \== yes ->
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
  goto(H, S, P, To),
  addStateToStateList(AllS, To, AllS2),
  ( To == [] ->
    Table2 = Table,
    I = yes
  ;
    nth0(Index0, AllS2, H),
    nth0(Index1, AllS2, To),
    addElemToTable(Table, (Index0, S, Index1), Table2, I)
  ),
  ( I \== yes ->
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
  (A, S, B) = H,
  (X, C, Y) = E,
  ( A == X, S == C, B \= Y -> fail ; checkTableConflict(T, E) ).

% addElemToTable(+Table, +Element, -ResultTable, -Info)
addElemToTable(T, E, R, I) :-
  ( checkTableConflict(T, E) ->
    append([T, [E]], R0),
    remove_dups(R0, R),
    I = yes
  ;
    makeConflict('shift-reduce', R, I)
  ).

% createAutomatonWithTable(+Grammar, +Table, +States, -Automaton, -Info)
createAutomatonWithTable(G, T, S, A, I) :-
  gramatyka(_, GP) = G,
  % debugStates(S),
  % debugTable(T),
  splitTable(T, GT, AT1),
  addAccepts(S, AT1, AT2),
  tableSymbols(AT2, Symbols),
  flattenProductions(GP, P),
  addReduces(S, 0, P, Symbols, AT2, AT, I),
  % debugGATables(GT, AT),
  A = (GT, AT).

% addReduces(+States, +Index, +Productions, +Symbols, +ATable, -ResultAT, -Info)
addReduces([], _, _, _, AT, AT, yes).
addReduces([H | T], I, P, S, AT, RT, Info) :-
  productionsInState(H, P, PNums),
  length(PNums, Len),
  ( Len < 2 ->
    I1 is I + 1,
    addReduces(T, I1, P, S, AT, RT1, Info1),
    ( Info1 \== yes ->
      Info = Info1
    ;
      ( Len == 0 ->
        RT = RT1,
        Info = Info1
      ;
        [Target] = PNums,
        ( Target == 0 ->
          RT = RT1,
          Info = Info1
        ;
          addReduceRow(S, I, Target, RT1, RT, Info)
        )
      )
    )
  ;
    makeConflict('reduce-reduce', RT, Info)
  ).

% addReduceRow(+Symbols, +From, +To, +ATable, -Result, -Info)
addReduceRow([], _, _, A, A, yes).
addReduceRow([H | T], F, To, AT, R, Info) :-
  addReduceRow(T, F, To, AT, R1, I1),
  ( I1 \== yes ->
    Info = I1
  ;
    addElemToTable(R1, (F, H, r(To)), R, Info)
  ).

% productionsInState(+State, +Productions, -NumP)
productionsInState([], _, []).
productionsInState([H | T], P, R) :-
  productionsInState(T, P, R1),
  ( nthProductionItem(P, H, Q), Q \== -1 ->
    R = [Q | R1]
  ;
    R = R1
  ).

% nthProductionItem(+Productions, +Item, -Res)
nthProductionItem(P, I, R) :- nthProductionItem(P, I, 0, R).

% nthProductionItem(+Productions, +Item, +Index, -Res)
nthProductionItem([], _, _, -1).
nthProductionItem([(L, P) | T], item(L, P, X), Index, R) :-
  !,
  length(P, Len),
  ( X == Len ->
    R = Index
  ;
    Index1 is Index + 1,
    nthProductionItem(T, item(L, P, X), Index1, R)
  ).

nthProductionItem([_ | T], I, Index, R) :-
  Index1 is Index + 1,
  nthProductionItem(T, I, Index1, R).

% addAccepts(+States, +ActionTable, -ResultActionTable)
addAccepts(S, AT, R) :- addAccepts(S, 0, AT, R).

% addAccepts(+States, +Index, +ActionTable, -ResultActionTable)
addAccepts([], _, AT, AT).
addAccepts([H | T], I, AT, R) :-
  I1 is I + 1,
  addAccepts(T, I1, AT, R1),
  ( stateIsAccepting(H) ->
    R = [(I, #, acc) | R1]
  ;
    R = R1
  ).

% flattenProductions(+Productions, -FlatProductions)
flattenProductions([], []).
flattenProductions([prod(_, []) | T], F) :-
  flattenProductions(T, F).
flattenProductions([prod(L, [P | R]) | T], F) :-
  flattenProductions([prod(L, R) | T], F1),
  F = [(L, P) | F1].

% stateIsAccepting(+State)
stateIsAccepting([item('Z', [nt(_), #], 1) | _]) :- !.
stateIsAccepting([_ | T]) :- stateIsAccepting(T).

% splitTable(+Table, -GotoTable, -ActionTable)
splitTable([], [], []).
splitTable([H | T], G, A) :-
  splitTable(T, G1, A1),
  ( (_, nt(_), _) = H ->
    G = [H | G1],
    A = A1
  ;
    G = G1,
    makeShift(H, Shift),
    A = [Shift | A1]
  ).

% makeShift(+Rule, -Shift)
makeShift(R, S) :-
  (A, C, B) = R,
  S = (A, C, s(B)).

% makeConflict(+ErrorMessage, -Automaton, -Info)
makeConflict(E, A, I) :-
  I = konflikt(E),
  A = null.

% rightSymbols(+ProductionsList, -Symbols)
rightSymbols([], S) :- S = [].
rightSymbols([H | T], S) :-
  rightSymbols(T, ST),
  prod(_, P) = H,
  append(P, PJoin),
  append([PJoin, ST], SJoin),
  remove_dups(SJoin, SUniq),
  delete(SUniq, #, S).

% clojure(+Items, +ProductionsList, -ClojureOfItems)
clojure(I, P, C) :-
  clojureOnce(I, P, C1),
  ( I == C1 -> C = C1 ; clojure(C1, P, C) ).

% clojureOnce(+Items, +ProductionsList, -ClojureOfItems)
clojureOnce([], _, C) :- C = [].
clojureOnce([H | T], P, C) :-
  clojureItem(H, P, CH),
  clojureOnce(T, P, CT),
  append(CH, CT, CJoin),
  remove_dups(CJoin, C).

% clojureItem(+Item, +ProductionsList, -ClojureOfItem)
clojureItem(I, P, C) :-
  item(_, R, Index) = I,
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
  item(L, R, Index) = H,
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
  gramatyka(S, L) = G,
  E = gramatyka(S, [prod('Z', [[nt(S), #]]) | L]).

% tableSymbols(+Table, -Symbols)
tableSymbols([], []).
tableSymbols([H | T], S) :-
  (_, X, _) = H,
  tableSymbols(T, S1),
  remove_dups([X | S1], S).

% actionNumber(+Action, -Number)
actionNumber(s(X), X) :- !.
actionNumber(r(X), X) :- !.
actionNumber(acc, 0) :- !.
actionNumber(err, 0) :- !.
actionNumber(X, X).

% -------------------------- DEBUG UTILS --------------------------------------

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
  (X, _, Y1) = H,
  actionNumber(Y1, Y),
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
  gramatyka(S, L) = G,
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
  prod(L, R) = H,
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
debugSymbol(nt(N)) :- write(N), !.
debugSymbol(s(N)) :- write('s'), write(N), !.
debugSymbol(r(N)) :- write('r'), write(N), !.
debugSymbol(acc) :- write('a'), !.
debugSymbol(err) :- write('e'), !.
debugSymbol(S) :- write(S).

% debugItems(+Items)
debugItems([]).
debugItems([H | T]) :-
  item(L, R, Index) = H,
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

% debugGATables(+GotoTable, +ActionTable)
debugGATables(G, A) :-
  write('----- GotoTable -----\n'),
  debugTable(G),
  write('----- ActionTable -----\n'),
  debugTable(A).

% --------------------------- DEBUG -------------------------------------
