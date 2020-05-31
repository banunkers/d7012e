% Exam 18-08-22

% 4.
% a)
factorial(0, 1) :- !.
factorial(N, FN) :-
  N1 is N - 1,
  factorial(N1, FN1),
  FN is N * FN1.

% b)
factorials(0, [1]) :- !.
factorials(N, L) :-
  N > 0,
  N1 is N - 1,
  factorials(N1, L1),
  factorial(N, FN),
  append(L1, [FN], L).

% 7.
% a)
f(_, [], 0) :- !.
f(X, [X | T], N) :-
  f(X, T, N1),
  N is N1 + 1,
  !.
f(X, [_ | T], N) :-
  f(X, T, N),
  !.

% b)
fL([], []) :- !.
fL(L, FT) :-
  setof([X, N], (member(X, L), f(X, L, N)), FT).
