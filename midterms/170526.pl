% 17-05-26

% 5.
% a)
member2(X, [X | _]).
member2(X, [_ | Ys]) :-
  member2(X, Ys).
