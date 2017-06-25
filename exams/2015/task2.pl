q(a).
q(c).
r(b).
r(c).

p(0, X) :- q(X), !, r(X).
p(N, X) :- N>0, M is N-1, p(M, X).
p(1, b).

% a) p(0, c).
% b) p(0, X).
% c) p(X, b).
% d) p(2, X).
% e) p(-1, X).
