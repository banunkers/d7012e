parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

female(pam).
female(liz).
female(pat).
female(ann).
male(tom).
male(bob).
male(jim).

happy(Person) :-
  parent(Person, _).

/* 
 * a) variable
 * b) atom
 * c) atom
 * d) variable
 * e) atom
 * f) structure
 * g) number
 * h) -
 * i) -
 * j) -
 * */

% 2.2
rectangle(P1, P2, P3, P4).
circle(Radius).
triangle(P1, P2, P3).

% 2.5
regular(R) :-
  rectangle(point(X1, Y1), point(X2, Y2), point(X3, Y3), point(X4, Y4)) = R,
  X1 = X2,
  Y1 = Y4,
  Y2 = Y3,
  X3 = X4.

