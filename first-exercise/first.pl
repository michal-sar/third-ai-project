% POP planner

% literals!
% conditions!

% wall([0/0]), wall([1/0]), wall([2/0]), wall([3/0]),
% wall([4/0]), wall([4/1]), wall([4/2]), wall([4/3]),
% wall([4/4]), wall([3/4]), wall([2/4]), wall([1/4]),
% wall([0/4]), wall([0/3]), wall([0/2]), wall([0/1]),

% # # # # #
% #       #
% #   x   #
% # o   * #
% # # # # #

sokoban('a').

box('1').

state0([sokoban('a', [1/1]), box('1', [2/2]), goal([3/1]),
empty([2/1]), empty([3/1]),
empty([1/2]), empty([3/2]),
empty([1/3]), empty([2/3]), empty([3/2])]).

checkResultant([X/Y], [NX/NY], [RX/RY]) :-
  (
  [NX/NY] == [(X + 1)/Y], RX is X + 2, RY is Y
  ;
  [NX/NY] == [(X - 1)/Y], RX is X - 2, RY is Y
  ;
  [NX/NY] == [X/(Y + 1)], RX is X, RY is Y + 2
  ;
  [NX/NY] == [X/(Y - 1)], RX is X, RY is Y - 2
  ).

% adjacent([X/Y], [NX/NY]) :-
%   [X/Y] == [(NX + 1)/NY]);
%   [X/Y] == [(NX - 1)/NY]);
%   [X/Y] == [NX/(NY + 1)]);
%   [X/Y] == [NX/(NY - 1)]).

adjacent([X/Y], [NX/NY]) :-
  (
  X == NX + 1, Y == NY
  ;
  X == NX - 1, Y == NY
  ;
  X == NX, Y == NY + 1
  ;
  X == NX, Y == NY - 1
  ).

can(move_sokoban(Sokoban, Position, NewPosition), [sokoban(Sokoban, Position), empty(NewPosition)]) :-
  sokoban(Sokoban),
  adjacent(Position, NewPosition).

can(move_sokoban_and_box(Sokoban, Box, Position, NewPosition, NewBoxPosition), [sokoban(Sokoban, Position), box(Box, NewPosition), empty(NewBoxPosition)]) :-
  sokoban(Sokoban),
  adjacent(Position, NewPosition),
  checkResultant(Position, NewPosition, NewBoxPosition).

effects(move_sokoban(Sokoban, Position, NewPosition), [sokoban(Sokoban, NewPosition), empty(Position), ~sokoban(Sokoban, Position), ~empty(NewPosition)]).

effects(move_sokoban_and_box(Sokoban, Box, Position, NewPosition, NewBoxPosition), [sokoban(Sokoban, NewPosition), box(Box, NewBoxPosition), empty(Position), ~sokoban(Sokoban, Position), ~box(Box, NewPosition), ~empty(NewBoxPosition)]).

inconsistent(G, ~G).
inconsistent(~G, G).

inconsistent(sokoban(S1, P1), sokoban(S1, P2)) :- P1 \== P2.
inconsistent(sokoban(S1, P1), sokoban(S2, P1)) :- S1 \== S2.

inconsistent(box(B1, P1), box(B1, P2)) :- P1 \== P2.
inconsistent(box(B1, P1), box(B2, P1)) :- B1 \== B2.

inconsistent(sokoban(_, P1), box(_, P1)).
inconsistent(sokoban(_, P1), empty(P1)).
inconsistent(box(_, P1), empty(P1)).














































%
