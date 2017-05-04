% child(Child, Mother, Father)

child(jan, maria, stefan).
child(magda, maria, stefan).
child(maria, joanna, roman).
child(stefan, ela, zbyszek).
child(zbyszek, jolanta, maciej).

mother(Mother, Child) :- child(Child, Mother, _).
father(Father, Child) :- child(Child, _, Father).
mother(Mother) :- mother(Mother, _).
father(Father) :- father(Father, _).
parent(Parent, Child) :- mother(Parent, Child).
parent(Parent, Child) :- father(Parent, Child).
parent(Parent) :- mother(Parent).
parent(Parent) :- father(Parent).
grandchild(Grandchild, Grandparent) :- parent(Grandparent, X), parent(X, Granchild).
ancestor(Ancestor, Descendant) :- parent(Ancestor, Descendant).
ancestor(Ancestor, Descendant) :- parent(P, Descendant), ancestor(Ancestor, P).
