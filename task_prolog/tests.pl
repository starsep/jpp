:- set_prolog_flag(verbose, silent).
:- initialization(main).
main :-
  consult(fc359081),
  test(ex0, []),
  test(ex1, [[id], ['(',id,')'], [id,'+',ident], [id,'+',id]]),
  % test(ex2, [[id], ['(',id,')'], [id,'+',ident], [id,'+',id]]),
  % test(ex3, [[id], ['(',id,')'], [id,'+',ident], [id,'+',id]]),
  % test(ex4, [[id], ['(',id,')'], [id,'+',ident], [id,'+',id]]),
  % test(ex5, [[id], ['(',id,')'], [id,'+',ident], [id,'+',id]]),
  % test(ex6, [[id], ['(',id,')'], [id,'+',ident], [id,'+',id]]),
  halt.
main :-
  halt(1).

% LR(0)
grammar(ex0, gramatyka('E', [
  prod('E', [
    [nt('E'), '*', nt('B')],
    [nt('E'), '+', nt('B')],
    [nt('B')]
  ]),
  prod('B', [
    ['0'],
    ['1']
  ])
])).

% LR(0)
grammar(ex1, gramatyka('E', [
  prod('E', [
    [nt('E'),'+', nt('T')],
    [nt('T')]
  ]),
  prod('T', [
    [id],
    ['(', nt('E'), ')']
  ])
])).

% LR(0)
grammar(ex2, gramatyka('A', [
  prod('A', [
    [nt('A'), x],
    [x]
  ])
])).

% SLR(1)
grammar(ex3, gramatyka('A', [
  prod('A', [
    [x, nt('A')],
    [x]
  ])
])).

% nie SLR(1)
grammar(ex4, gramatyka('A', [
  prod('A', [
    [x, nt('B')],
    [nt('B'), y],
    []
  ]),
  prod('B', [
    []
  ])
])).

% nie SLR(1)
grammar(ex5, gramatyka('S', [
  prod('S', [
    [id],
    [nt('V'), ':=', nt('E')]
  ]),
  prod('V', [
    [id],
    [id, '[', nt('E'), ']']
  ]),
  prod('E', [
    [v]
  ])
])).

% nie SLR(1)
grammar(ex6, gramatyka('A', [
  prod('A', [
    [x],
    [nt('B'), nt('B')]
  ]),
  prod('B', [
    [x]
  ])
])).

% test(+NazwaGramatyki, +ListaSlowDoZbadania)
test(NG, ListaSlow) :-
  grammar(NG, G),
  debugGrammar(G),
  createLR(G, Automat, Info),
  write('Automat building info: '),
  write(Info),
  write('\n'),
  checkWords(ListaSlow, Automat).

checkWords([], _) :-
  write('Koniec testu.\n').

checkWords([S|RS], Automat) :-
  format(" Slowo: ~p ", [S]),
  (accept(Automat, S) -> true; write('NIE ')),
  write('nalezy.\n'),
  checkWords(RS, Automat).
