% 18-05-28

% 5.
% a)
fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, FN) :-
  N1 is N - 1,
  N2 is N - 2,
  fib(N1, FN1),
  fib(N2, FN2),
  FN is FN1 + FN2.

% b)
fibs(N, L) :-
  N >= 1,
  N1 is N - 1,
  fib(N, FN),
  fibs(N1, L1),
  append(L1, [FN], L).
fibs(0, [0]).

% 7.
% a)
arc(n1,n2).
arc(n6,n2).
arc(n2,n5).
arc(n2,n3).
arc(n3,n7).
arc(n3,n4).
%arc(n6,n3).

path(A, B, [arc(A, B)]) :- arc(A, B).
path(A, B, [arc(A, X) | R]) :-
  arc(A, X),
  path(X, B, R).

allPaths(N, L) :-
  N2 is N - 1,
  setof(P, A^B^(path(A, B, P), length(P, N2)), L).
