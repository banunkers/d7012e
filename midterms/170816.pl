% 17-08-16

% 5.
% b)
concat([], []).
concat([H | T], T3) :-
  concat(T, T2),
  append(H, T2, T3).
