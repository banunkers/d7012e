% 17-10-18

% 6.
% a)
select1(_, [], []) :- !, fail.
select1(X, [X | Xs], Xs) :- !.
select1(X, [Y | Ys], [Y | Zs]) :-
  select1(X, Ys, Zs).

select2(_, [], []).
select2(X, [X | Xs], Xs) :- !.
select2(X, [Y | Ys], [Y | Zs]) :-
  select2(X, Ys, Zs).

% b) wrong?
pickTwoDifferent(L, E1, E2) :-
  select1(E1, L, L1),
  select1(E2, L1, _),
  E1 \= E2.

% 8.
% a)
subLists(L, R) :-
  findall(X, comb(L, X), R).

comb([], []).
comb([X | Xs], [X | Ys]) :- comb(Xs, Ys).
comb([_ | Xs], Ys) :- comb(Xs, Ys).

% b)
keep([], _, []).
keep([X | Xs], Max, [X | Ys]) :-
  sum(X, XSum),
  XSum =< Max,
  !,
  keep(Xs, Max, Ys).
keep([_ | Xs], Max, Ys) :-
  keep(Xs, Max, Ys).

sum([], 0).
sum([H | T], S) :-
  sum(T, TSum),
  S is H + TSum.
