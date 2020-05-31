% 5.
mergeUnq([], [], []).
mergeUnq(L1, L2, L) :-
  append(L1, L2, L3),
  setof(X, member(X, L3), L).

test5(R) :-
  mergeUnq([2,4,6,8],[1,2,5,6,9], R).

% 6
% a)
swap(t(A, B), t(B, A)).
% b)
swapAll([X], [Y]) :-
  swap(X, Y).
swapAll([X | Xs], L2) :-
  swap(X, Y),
  swapAll(Xs, TempL),
  append([Y], TempL, L2).
