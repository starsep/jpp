% Filip Czaplicki 359081

:- use_module(library(lists)).

% Korzystałem z https://pl.wikipedia.org/wiki/Generowanie_parser%C3%B3w_LR
% Automat reprezentuję zgodnie z tym opisem, tj. jako dwie tabele: goto i action
% oraz stos.
% Stos to lista numerów stanów.
% Stany to listy sytuacji
% Sytuacja to produkcja 'z kropką', reprezentowana jako trójka (L, P, N), gdzie
% L to lewa strona produkcji, P prawa, N nr miejsca kropki
% Tabele goto i action reprezentuję jako listy trójek postaci (N, C, A), gdzie
% - N to numer stanu, z którego przechodzimy
% - C to symbol (terminal/nonterminal)
% - A to akcja:
%   * w przypadku tabeli goto liczba
%   * w przypadku tabeli action jedno z:
%     - s(N) - shift do stanu N
%     - r(N) - reduce z regułą N
%     - acc - akcja akceptacji wyrazu

% createLR(+Grammar, -Automaton, -Info)
createLR(G, A, I) :-
  extendGrammar(G, E),
  createAutomaton(E, A, I).

% extendGrammar(+Grammar, -ExtendedGrammar)
% rozszerza gramatykę - dodaje produkcję Z -> S #, gdzie S to symbol początkowy
extendGrammar(G, E) :-
  gramatyka(S, L) = G,
  E = gramatyka(S, [prod('Z', [[nt(S), #]]) | L]).

% createAutomaton(+Grammar, -Automaton, -Info)
% tworzy automat dostając gramatykę rozszerzoną
createAutomaton(G, A, I) :-
  buildTable(G, Table, States, InfoTable),
  ( InfoTable \== yes ->
    makeConflict(InfoTable, A, I)
  ;
    createAutomatonWithTable(G, Table, States, A, I)
  ).

% buildTable(+Grammar, -Table, -States, -Info)
% tworzy tabele, z której powstaną tabele goto oraz action
buildTable(G, T, States, I) :-
  gramatyka(S, P) = G,
  clojure([item('Z', [nt(S), #], 0)], P, InitItem),
  rightSymbols(P, Symbols),
  runGoto(Symbols, [InitItem], P, [], States, T, I).

% runGoto(+Symbols, +States, +Productions, +Table, -ResStates, -ResTable, -Info)
% domknięcie względem operacji runGotoOnce
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
% dodaje przejścia goto względem każdego symbolu
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
% dodaje prześcia do tabeli oraz nowe stany wykonując goto dla danego symbolu
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
% dostaje listę stanów oraz stan, zwraca rozszerzoną listę stanów
addStateToStateList(L, [], L).
addStateToStateList(L, S, R) :-
  ( member(S, L) ->
    R = L
  ;
    append([L, [S]], R)
  ).

% checkTableConflict(+Table, +Element)
% sprawdza czy dodanie danego elementu do tabeli spowoduje konflikt tj.
% już istnieje w tabeli reguła z tego samego stanu, po tym samym symbolu,
% ale z inną akcją końcową
checkTableConflict([], _).
checkTableConflict([H | T], E) :-
  (A, S, B) = H,
  (X, C, Y) = E,
  ( A == X, S == C, B \= Y -> fail ; checkTableConflict(T, E) ).

% addElemToTable(+Table, +Element, -ResultTable, -Info)
% dodaje element do tabeli
addElemToTable(T, E, R, I) :-
  ( checkTableConflict(T, E) ->
    append([T, [E]], R0),
    remove_dups(R0, R),
    I = yes
  ;
    makeConflict('shift-reduce', R, I)
  ).

% createAutomatonWithTable(+Grammar, +Table, +States, -Automaton, -Info)
% dostaje gramatykę, tabelę oraz stany, tworzy automat
createAutomatonWithTable(G, T, S, A, I) :-
  gramatyka(_, GP) = G,
  splitTable(T, GT, AT1),
  addAccepts(S, AT1, AT2),
  tableSymbols(AT2, Symbols),
  flattenProductions(GP, P),
  addReduces(S, 0, P, Symbols, AT2, AT, I),
  A = (AT, GT, P).

% addReduces(+States, +Index, +Productions, +Symbols, +ATable, -ResultAT, -Info)
% dodaje do tabeli action redukcje tj. punkt 4 w
% https://pl.wikipedia.org/wiki/Generowanie_parser%C3%B3w_LR#Tworzenie_tabel_action_i_goto
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
% dodaje do tabeli action pełny wiersz redukcji reguły To
addReduceRow([], _, _, A, A, yes).
addReduceRow([H | T], F, To, AT, R, Info) :-
  addReduceRow(T, F, To, AT, R1, I1),
  ( I1 \== yes ->
    Info = I1
  ;
    addElemToTable(R1, (F, H, r(To)), R, Info)
  ).

% productionsInState(+State, +Productions, -NumP)
% dla stanu (zbioru sytuacji) oraz listy produkcji zwraca listę produkcji
% takich że stan zawiera sytuację A → α•, dla produkcji postaci A → α
productionsInState([], _, []).
productionsInState([H | T], P, R) :-
  productionsInState(T, P, R1),
  ( nthProductionItem(P, H, Q), Q \== -1 ->
    R = [Q | R1]
  ;
    R = R1
  ).

% nthProductionItem(+Productions, +Item, -Res)
% pomocnicza do nthProductionItem/4
nthProductionItem(P, I, R) :- nthProductionItem(P, I, 0, R).

% nthProductionItem(+Productions, +Item, +Index, -Res)
% dla listy produkcji oraz sytuacji postaci A → α• zwraca numer produkcji
% A → α lub -1 w p. p.
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
% pomocnicza do addAccepts/4
addAccepts(S, AT, R) :- addAccepts(S, 0, AT, R).

% addAccepts(+States, +Index, +ActionTable, -ResultActionTable)
% dodaje regułę acc, punkt 3 w
% https://pl.wikipedia.org/wiki/Generowanie_parser%C3%B3w_LR#Tworzenie_tabel_action_i_goto
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
% zamienia listę produkcji na 'płaską' reprezentację tj.
% listę par postaci (lewa, prawa) strona produkcji
flattenProductions([], []).
flattenProductions([prod(_, []) | T], F) :-
  flattenProductions(T, F).
flattenProductions([prod(L, [P | R]) | T], F) :-
  flattenProductions([prod(L, R) | T], F1),
  F = [(L, P) | F1].

% stateIsAccepting(+State)
% powodzi się jeżeli stan (zbiór sytuacji) zawiera sytuację Z -> S•#
stateIsAccepting([item('Z', [nt(_), #], 1) | _]) :- !.
stateIsAccepting([_ | T]) :- stateIsAccepting(T).

% splitTable(+Table, -GotoTable, -ActionTable)
% dzieli tabelę stworzoną przez buildTable na tabele goto oraz action
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
% tworzy z reguły przejścia z tabeli regułę shift
makeShift(R, S) :-
  (A, C, B) = R,
  S = (A, C, s(B)).

% makeConflict(+ErrorMessage, -Data, -Info)
% dostaje opis konfliktu, ustawia Info oraz Data
makeConflict(E, D, I) :-
  I = konflikt(E),
  D = null.

% rightSymbols(+ProductionsList, -Symbols)
% dostaje listę produkcji gramatyki, zwraca listę symboli występujących po
% prawych stronach poza #
rightSymbols([], []).
rightSymbols([H | T], S) :-
  rightSymbols(T, ST),
  prod(_, P) = H,
  append(P, PJoin),
  append([PJoin, ST], SJoin),
  remove_dups(SJoin, SUniq),
  delete(SUniq, #, S).

% clojure(+Items, +ProductionsList, -ClojureOfItems)
% domknięcie zbioru sytuacji względem operacji clojureOnce
% implementacja algorytmu z
% https://pl.wikipedia.org/wiki/Generowanie_parser%C3%B3w_LR #Domkni.C4.99cie_zbior.C3.B3w_sytuacji
clojure(I, P, C) :-
  clojureOnce(I, P, C1),
  ( I == C1 -> C = C1 ; clojure(C1, P, C) ).

% clojureOnce(+Items, +ProductionsList, -ClojureOfItems)
clojureOnce([], _, []).
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
itemsFromProductions([], _, []).
itemsFromProductions([H | T], S, I) :-
  H = prod(L, R),
  ( nt(L) = S ->
    rightSidesToItems(R, L, I)
  ;
    itemsFromProductions(T, S, I)
  ).

% rightSidesToItems(+RightSidesList, +LeftSymbol, -Items)
rightSidesToItems([], _, []).
rightSidesToItems([H | T], L, I) :-
  rightSidesToItems(T, L, IT),
  Item = item(L, H, 0),
  I = [Item | IT].

% goto(+From, +Symbol, +ProductionsList, -To)
% implementacja Goto z
% https://pl.wikipedia.org/wiki/Generowanie_parser%C3%B3w_LR#Znajdowanie_osi.C4.85galnych_zbior.C3.B3w_sytuacji_i_przej.C5.9B.C4.87_pomi.C4.99dzy_nimi
goto(F, S, P, To) :-
  moveItems(F, S, Res),
  clojure(Res, P, To).

% indexOf(+List, +Element, -Index)
% funkcja pomocnicza: dostaje listę oraz element, zwraca pierwszy indeks
% wystąpienia tego elementu na liście
indexOf([E | _], E, 0).
indexOf([_ | T], E, I) :-
  indexOf(T, E, Index),
  I is Index + 1.

% moveItems(+Items, +Symbol, -ResultItems)
moveItems([], _, []).
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
accept(A, W) :-
  append([W, [#]], WWithEnd),
  accept(WWithEnd, A, [0]).

% accept(+Word, +Automaton, +Stack)
% dostaje wyraz z dodanym znakiem końcowym, automat oraz aktualny stos
% powodzi się wtw gdy automat akceptuje wyraz
accept([], _, _) :- fail.
accept([C | T], A, S) :-
  (AT, GT, Prod) = A,
  head(S, State),
  currentAction(AT, State, C, Action),
  ( actionIsTerminal(Action) ->
    Action == acc
  ;
    ( s(N) = Action ->
      accept(T, A, [N | S])
    ;
      r(N) = Action,
      nth0(N, Prod, (L, P)),
      length(P, Q),
      dropN(S, Q, S1),
      head(S1, State1),
      currentAction(GT, State1, nt(L), NewState),
      accept([C | T], A, [NewState | S1])
    )
  ).

% dropN(+List, +N, -Result)
% dostaje listę oraz liczbę N, zwraca listę bez N pierwszych elementów
dropN(L, 0, L).
dropN([_ | T], N, R) :-
  N1 is N - 1,
  dropN(T, N1, R).

% actionIsTerminal(+Action)
% udaje się jeżeli akcja jest terminalna
% tj. taka że accept kończy swoje działanie na tej akcji
actionIsTerminal(err) :- !.
actionIsTerminal(acc) :- !.
actionIsTerminal(_) :- !, fail.

% currentAction(+Table, +State, +Character, -Action)
% dostaje tabelę (goto/action), aktualny stan oraz symbol, zwraca akcję
currentAction([], _, _, err).
currentAction([(S, C, A) | _], S, C, A) :- !.
currentAction([_ | T], S, C, A) :-
  currentAction(T, S, C, A).

% tableSymbols(+Table, -Symbols)
% zwraca wszystkie symbole, które występują w tabeli
% tj. istnieje przejście po tym symbolu
tableSymbols([], []).
tableSymbols([H | T], S) :-
  (_, X, _) = H,
  tableSymbols(T, S1),
  remove_dups([X | S1], S).

% actionNumber(+Action, -Number)
% dla akcji zwraca liczbę - nr stanu/reguły gramatyki
actionNumber(s(X), X) :- !.
actionNumber(r(X), X) :- !.
actionNumber(acc, 0) :- !.
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
