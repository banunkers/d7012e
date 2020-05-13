% Hugo Wangler - hugwan-6

% The sum of all elemens in a list
sum([], 0).
sum([X | Xs], Sum) :-
  sum(Xs, SumXs),
  Sum is X + SumXs.

% Sorts an array of sublists using insertion sort
isort([X], [X]).
isort([X | Xs], Ys) :-
  isort(Xs, SortedXs),
  insert(X, SortedXs, Ys).

insert(X, [], [X]).
insert(X, [Y | Ys], Zs) :-
  X = sublist(SizeX, _, _, _),
  Y = sublist(SizeY, _, _, _),
  (SizeX =< SizeY
  -> append([X], [Y | Ys], Zs) % X : [Y : Ys]
  ; insert(X, Ys, NewYs),
    append([Y], NewYs, Zs) % Y : ins (X Ys)
  ).


% Computes all the sublists of an input list Xs as well as their indices
% and size (sum)
sublists(Xs, sublist(Size, SubL, I, J)) :-
  append(Pref, Suff, Xs), % Suff = suffix of list Xs
  append(SubL, _, Suff), % XSub = prefixes of suffixes
  length(SubL, SubLen),
  length(Pref, PrefLen),
  SubLen > 0,
  I is PrefLen + 1, % Sublist starts where prefix ends
  J is PrefLen + SubLen, % End index of sublist is prefix + sublist length
  sum(SubL, Size).

% Finds all the sublists of Xs and returns them as an array of sublists
% the array of sublists are sorted with respect to their sizes
get_sublists(Xs, ListOfSubL) :-
  findall(X0, sublists(Xs, X0), L), 
  isort(L, ListOfSubL). 

print_sets(Sets, Input) :-
  format('Entire List: ~w ~n ~n', [Input]), % input list
  format('~t size ~t ~10| i ~+ j ~+ sublist~n'), % header
  format_set(Sets).

format_set([sublist(Size, SubL, I, J)]) :-
  format('~t ~d ~t ~10| ~d ~+ ~d ~+ ~w ~n', [Size, I, J, SubL]).
format_set([sublist(Size, SubL, I, J) | Tail]) :-
  format('~t ~d ~t ~10| ~d ~+ ~d ~+ ~w ~n', [Size, I, J, SubL]),
  format_set(Tail).

smallest_k_set(Xs, K, SmallestKSet) :-
  get_sublists(Xs, ListOfSubL),
  take(K, ListOfSubL, SmallestKSet),
  print_sets(SmallestKSet, Xs).

% like take in haskell
take(0, _, []).
take(_, [], []).
take(N, [X | Xs], [X | Xs2]) :-
  N > 0,
  N1 is N - 1,
  take(N1, Xs, Xs2).

% Test cases
test_case1(R) :-
  createL(100, [], Xs),
  smallest_k_set(Xs, 15, R).

createL(0, Xs, Xs) :- !.
createL(N, Xs, Ys) :-
  N > 0,
  X is N * ((-1) ** N),
  N1 is N - 1,
  createL(N1, [X | Xs], Ys).

test_case2(R) :-
  smallest_k_set([24, -11, -34, 42, -24, 7, -19, 21], 6, R).

test_case3(R) :-
  smallest_k_set([3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3], 8, R).
