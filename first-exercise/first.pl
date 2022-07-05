% POP planner

% - - - - - - - - - - - - - - - - - - - - - - %

% Example 1

% # # # # #
% #       #
% #   x   #
% # o   * #
% # # # # #

sokoban(a).
% ...

box(1).
% ...

state0([sokoban(a, 1, 1), box(1, 2, 2),
empty(2, 1), empty(3, 1),
empty(1, 2), empty(3, 2),
empty(1, 3), empty(2, 3), empty(3, 3)]).

solve1 :- state0(S), plan(S, [box(1, 3, 1)], P), show_pop(P).

% - - - - - - - - - - - - - - - - - - - - - - %

% Example 2

% ...

% ...

% ...

% ...

% ...

% - - - - - - - - - - - - - - - - - - - - - - %

:- op(100, fx, ~).

getResultant(X, Y, NX, NY, RX, RY) :-
  var(X), var(Y), var(NX), var(NY),
  (
  NX is RX, X is RX, NY is RY + 1, Y is RY + 2
  ;
  NX is RX, X is RX, NY is RY - 1, Y is RY - 2
  ;
  NX is RX + 1, X is RX + 2, NY is RY, Y is RY
  ;
  NX is RX - 1, X is RX - 2, NY is RY, Y is RY
  ).

getResultant(X, Y, NX, NY, RX, RY) :-
  var(X), var(Y), var(RX), var(RY),
  (
  RX is NX, X is NX, RY is NY + 1, Y is NY - 1
  ;
  RX is NX, X is NX, RY is NY - 1, Y is NY + 1
  ;
  RX is NX + 1, X is NX - 1, RY is NY, Y is NY
  ;
  RX is NX - 1, X is NX + 1, RY is NY, Y is NY
  ).

getResultant(X, Y, NX, NY, RX, RY) :-
  var(NX), var(NY), var(RX), var(RY),
  (
  NX is X, RX is X, NY is Y + 1, RY is Y + 2
  ;
  NX is X, RX is X, NY is Y - 1, RY is Y - 2
  ;
  NX is X + 1, RX is X + 2, NY is Y, RY is Y
  ;
  NX is X - 1, RX is X - 2, NY is Y, RY is Y
  ).

getAdjacent(X, Y, NX, NY) :-
  var(X), var(Y),
  (
  X is NX + 1, Y is NY
  ;
  X is NX - 1, Y is NY
  ;
  X is NX, Y is NY + 1
  ;
  X is NX, Y is NY - 1
  ).

getAdjacent(X, Y, NX, NY) :-
  var(NX), var(NY),
  (
  NX is X + 1, NY is Y
  ;
  NX is X - 1, NY is Y
  ;
  NX is X, NY is Y + 1
  ;
  NX is X, NY is Y - 1
  ).

can(move_sokoban(Sokoban, Position_X, Position_Y, NewPosition_X, NewPosition_Y), [sokoban(Sokoban, Position_X, Position_Y), empty(NewPosition_X, NewPosition_Y)]) :-
  sokoban(Sokoban),
  getAdjacent(Position_X, Position_Y, NewPosition_X, NewPosition_Y).

can(move_sokoban_and_box(Sokoban, Box, Position_X, Position_Y, NewPosition_X, NewPosition_Y, NewBoxPosition_X, NewBoxPosition_Y), [sokoban(Sokoban, Position_X, Position_Y), box(Box, NewPosition_X, NewPosition_Y), empty(NewBoxPosition_X, NewBoxPosition_Y)]) :-
  sokoban(Sokoban),
  getResultant(Position_X, Position_Y, NewPosition_X, NewPosition_Y, NewBoxPosition_X, NewBoxPosition_Y).

effects(move_sokoban(Sokoban, Position_X, Position_Y, NewPosition_X, NewPosition_Y), [sokoban(Sokoban, NewPosition_X, NewPosition_Y), empty(Position_X, Position_Y), ~sokoban(Sokoban, Position_X, Position_Y), ~empty(NewPosition_X, NewPosition_Y)]).

effects(move_sokoban_and_box(Sokoban, Box, Position_X, Position_Y, NewPosition_X, NewPosition_Y, NewBoxPosition_X, NewBoxPosition_Y), [sokoban(Sokoban, NewPosition_X, NewPosition_Y), box(Box, NewBoxPosition_X, NewBoxPosition_Y), empty(Position_X, Position_Y), ~sokoban(Sokoban, Position_X, Position_Y), ~box(Box, NewPosition_X, NewPosition_Y), ~empty(NewBoxPosition_X, NewBoxPosition_Y)]).

inconsistent(G, ~G).
inconsistent(~G, G).

inconsistent(sokoban(S1, P1X, P1Y), sokoban(S1, P2X, P2Y)) :- (P1X \== P2X; P1Y \== P2Y).
inconsistent(sokoban(S1, P1X, P1Y), sokoban(S2, P1X, P1Y)) :- S1 \== S2.

inconsistent(box(B1, P1X, P1Y), box(B1, P2X, P2Y)) :- (P1X \== P2X; P1Y \== P2Y).
inconsistent(box(B1, P1X, P1Y), box(B2, P1X, P1Y)) :- B1 \== B2.

inconsistent(sokoban(_, P1X, P1Y), box(_, P1X, P1Y)).
inconsistent(sokoban(_, P1X, P1Y), empty(P1X, P1Y)).
inconsistent(box(_, P1X, P1Y), empty(P1X, P1Y)).

% - - - - - - - - - - - - - - - - - - - - - - %

%
% Figure 18.5 Partial order planning program.
%

% Partial Order Planner, using CLP(FD) and iterative deepening search
%
% Partially ordered plan = pop( Actions, OpenConditions, TrueConditions, FinishingTime)
%
% Actions = [ Action1:Time1, Action2:Time2, ...] Actions and their execution times
% OpenConditions = [ Cond1:Time1, Cond2:Time2, ...]
% TrueConds = [ Cond1:Time11/Time12, Cond2:Time21/Time22, ... ]
% Note: Ordering constraints are implemented as CLP(FD) constraints

:- use_module(library(clpfd)). % Load library for CLP(FD)

:- op(100, fx, ~). % Notation for negative effects of an action

% plan(StartState, Goals, Plan, Finish):
% Plan is partially ordered plan that achieves Goals from StartState at time Finish
%
plan(StartState, Goals, Plan) :-
	add_intervals(0, StartState, TrueConds, Finish),    % StartState true at time 0
	add_times(Finish, Goals, OpenConds),                % Goals should be true at time Finish
	EmptyPlan = pop([], OpenConds, TrueConds, Finish),  % No actions in initial plan
	MaxActions in 0..100,                               % Maximally 100 actions in plan
	indomain(MaxActions),                               % Enforce iterative deepening search
	Finish in 1..MaxActions,                            % Domain for finishing time of Plan
	depth_first(EmptyPlan, SolutionPath, MaxActions),   % Search in space of POP's
	once(indomain(Finish)),                             % Minimize finishing time
	append(_, [Plan], SolutionPath).                    % Working plan is last element of solution path


% s(POP, NewPOP): successor relation between partially ordered plans
%     NewPOP is POP with the first open condition in POP achieved
%
s( pop(Acts, [Cond:Time | OpenPs], TrueConds, Fin),
  pop(Acts, OpenPs, TrueConds, Fin) ) :-
	member(Cond:Time1/Time2, TrueConds), % Cond already true between Time1 and Time2
	Time1 #< Time, Time #=< Time2.       % Constrain Time to interval Time1/Time2

s( pop(Acts, [Cond:Time | OpenPsO], TrueCondsO, Fin),
	 pop([Action1:Time1 | Acts], OpenPs, TrueConds, Fin) ) :-
	effects(Action1, Effects),              % Look for action that may achieve Cond
	del(Cond, Effects, RestEffects),        % Cond in Effects, that is Action1 achieves Cond
	can(Action1, PreConds1),                % Preconditions for Action1
	0 #< Time1, Time1 #< Time,              % Action1 must occur after 0 and before Time
	add_times(Time1, PreConds1, NewOpenPs),                   % Add Time1 to all preconditions
	add_intervals(Time1, RestEffects, RestEffectsTimes, Fin), % Add time intervals to all effects
	Time #=< Time2,                         % Achieved condition must be true until Time
	add_conds([Cond:Time1/Time2 | RestEffectsTimes], TrueCondsO, TrueConds), % Add effects to TrueCondsO
	append(NewOpenPs, OpenPsO, OpenPs).       % Add preconditions of Action to goals


% add_conds(Conds, TrueConds, NewTrueConds):
%    Add conditions Conds to list TrueConds, and set corresponding precedence constraints
%
add_conds([], TrueConds, TrueConds).

add_conds([CondTime | Conds], TrueCondsO, TrueConds) :-
	no_conflict(CondTime, TrueCondsO), % No conflict between CondTime and TrueCondsO
	add_conds(Conds, [CondTime | TrueCondsO], TrueConds).


% no_conflict(CondTime, TrueCondsO):
%   Set constraints to ensure no conflict between CondTime and TrueCondsO
%
no_conflict(_, []).

no_conflict(CondTime, [Cond1Time1 | TrueConds]) :-
	no_conflict1(CondTime, Cond1Time1),
	no_conflict(CondTime, TrueConds).

no_conflict1(CondA:Ta1/Ta2, CondB:Tb1/Tb2) :-
	inconsistent(CondA, CondB), !, % CondA inconsistent with CondB
	(Ta2 #=< Tb1; Tb2 #=< Ta1)     % Ensure no time overlap between CondA and CondB
	;
	true. % Case when CondA consistent with CondB - no constraint needed

% add_times(Time, Conds, TimedConds)
%
add_times(_, [], []).

add_times(Time, [Cond | Conds], [Cond:Time | TimedConds]) :-
	add_times(Time, Conds, TimedConds).

% add_intervals(Time, Conds, TimedConds, Finish):
%   every condition in Conds true from Time till some later time
%
add_intervals(_, [], [], _).

add_intervals(Time, [Cond | Conds], [Cond:Time/Time2 | TimedConds], Finish) :-
	Time #< Time2, Time2 #=< Finish, % Cond true from Time until Time2 =< Finish
	add_intervals(Time, Conds, TimedConds, Finish).


% depth_first(POP, SolutionPath, MaxActionsInPOP):
%   Depth-first search, with respect to number of actions, among partially ordered plans
%
depth_first(POP, [POP], _) :-
	POP = pop(_, [], _, _). % No open preconditions - this is a working plan

depth_first(First, [First | Rest], MaxActions) :-
	First = pop(Acts, _, _, _),
	length(Acts, NActs),
	(NActs < MaxActions, !         % # actions in plan is below MaxActions
	 ;
	 Second = pop(Acts, _, _, _)), % # actions in plan at maximum, no action may be added
	s(First, Second),
	depth_first(Second, Rest, MaxActions).


% Display all possible execution schedules of a partial order plan
%
show_pop(pop(Actions, _, _, _)) :-
	instantiate_times(Actions),               % Instantiate times of actions for readability
	setof(T:A, member(A:T, Actions), Sorted), % Sort actions according to times
	nl, write('Actions = '), write(Sorted),   % Write schedule
	fail                                      % Backtrack to produce other schedules
	;
	nl, nl.                                   % All schedues produced


% instantiate_times( Actions): instantiate times of actions respecting ordering constraints
%
instantiate_times([]).

instantiate_times([_:T | Acts]) :-
	indomain(T),                   % A value in domain of T
	instantiate_times(Acts).


del( X, [X | Tail], Tail).

del( X, [Y | Tail], [Y | Tail1]) :-
	del( X, Tail, Tail1).
