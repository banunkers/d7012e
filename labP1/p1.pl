% Hugo Wangler - hugwan-6

% The state:
%  state(
%    robot(Pos, Items[]),
%    item(steelkey, SKPos),
%    item(brasskey, BKPos),
%    item(package, PPos)
%  )
%
% Positions(rooms): r1, r2, r3
%
% moves:
%  pickup(Item): robot same room as item and not holding two items ?
%  drop(Item): ok if holding Item
%  walk(Curr, New): ok if holding req. key

% walk r1 -> r2 requires steelkey
move(
  state(robot(r1, Items), SK, BK, P),
  walk(r1, r2),
  state(robot(r2, Items), SK, BK, P)
) :-
  member(steelkey, Items).

% walk r2 -> r1 requires steel key
move(
  state(robot(r2, Items), SK, BK, P),
  walk(r2, r1),
  state(robot(r1, Items), SK, BK, P)
) :-
  member(steelkey, Items).

% walk r1 -> r3 requires brass key
move(
  state(robot(r1, Items), SK, BK, P),
  walk(r1, r3),
  state(robot(r3, Items), SK, BK, P)
) :-
  member(brasskey, Items).

% walk r3 -> r1 requires brass key
move(
  state(robot(r3, Items), SK, BK, P),
  walk(r3, r1),
  state(robot(r1, Items), SK, BK, P)
) :-
  member(brasskey, Items).

% pickup item
move(
  state(robot(RPos, Items0), SK, BK, P),
  pickup(I),
  state(robot(RPos, Items1), SK, BK, P)
) :-
  % item to pick up needs to be in same room as robot
  member(item(I, RPos), [SK, BK, P]), 
  not(member(I, Items0)), % not already in hands
  length(Items0, NumItems),
  NumItems < 2,
  Items1 = [I | Items0].

% drop item
move(
state(robot(RPos, Items0), SK0, BK0, P0),
drop(I),
  state(robot(RPos, Items1), SK1, BK1, P1)
) :-
  member(I, Items0),
  delete(Items0, I, Items1),
  % update pos of dropped item
  select(item(I, _), [SK0, BK0, P0], item(I, RPos), [SK1, BK1, P1]). 

% robot in r2 and package in one of the two hands => solved
solveR(state(robot(r2, Items), _, _, _), _, [done | []]) :-
  member(package, Items).
solveR(State0, N, [Move | Trace]) :-
  N > 0,
  move(State0, Move, State1),
  solveR(State1, N - 1, Trace).

% helper functions for solving containing the initial state of building
dosolve(R) :-
  solveR(
    state(
      robot(r1, []),
      item(steelkey, r1),
      item(brasskey, r2),
      item(package, r3)
    ),
    11,
    R
  ).
dosolveRN(R, N) :-
  solveR(
    state(
      robot(r1, []),
      item(steelkey, r1),
      item(brasskey, r2),
      item(package, r3)
    ),
    N,
    R
  ).
