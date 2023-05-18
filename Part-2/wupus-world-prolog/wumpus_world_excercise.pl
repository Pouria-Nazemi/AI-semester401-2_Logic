% wumpus_world.pl

:- module(wumpus,[initialize/2, execute/2, display_world/0]).

:- use_module(library(lists)).

type_of_evaluate_world(fig72). % Change this to random for true evaluation.

% changes: max_agent_actions = 8*n*n

:- load_files([utils]).  % Basic utilities

:- dynamic  wumpus_world_default_extent/1, max_agent_actions/1.   
:- dynamic  wumpus_world_extent/1,  wumpus_location/2, wumpus_health/1,  gold/2,  pit/2.
:- dynamic  agent_location/2,  agent_orientation/1,  agent_in_cave/1,  agent_health/1.
:- dynamic  agent_gold/1,  agent_arrows/1,  agent_score/1.

wumpus_world_default_extent(4).  % Default size of the cave is 4x4
%WNe


gold_probability(0.10).  % Probability that a location has gold
pit_probability(0.15).   % Probability that a non-(1,1) location has a pit
max_agent_actions(128).  % Maximum actions per trial allowed by agent



evaluate_agent(Trials,Score,Time) :-
  run_agent_trials(Trials,1,Score,Time).

run_agent_trials(Trials,NextTrial,0,0) :-
  NextTrial > Trials.

run_agent_trials(Trials,NextTrial,Score,Time) :-
  NextTrial =< Trials,
  format("Trial ~d~n",[NextTrial]),
  type_of_evaluate_world(W),
  initialize(W,Percept),
%WN
%  statistics(runtime,[T1|_]),
  statistics(cputime,T1),
%WN
  init_agent,                         
%WN
%  statistics(runtime,[T2|_]),

  statistics(cputime,T2),
%WN
  run_agent_trial(1,Percept,Time1),
  agent_score(Score1),
  NextTrial1 is NextTrial + 1,
  run_agent_trials(Trials,NextTrial1,Score2,Time2),
  Score is Score1 + Score2,
  Time is Time1 + Time2 + (T2 - T1).

run_agent_trial(_,_,0) :-             
  ( agent_health(dead) ;              
    agent_in_cave(no) ),
  !.

run_agent_trial(NumActions,_,0) :-    
  max_agent_actions(N),               
  NumActions > N,
  !.

run_agent_trial(NumActions,Percept,Time) :-
%WN
%  statistics(runtime,[T1|_]),
  statistics(cputime,T1),

%WN
  run_agent(Percept,Action),          
%WN
%  statistics(runtime,[T2|_]),
  statistics(cputime,T2),

%WN
  execute(Action,Percept1),
  NumActions1 is NumActions + 1,
  run_agent_trial(NumActions1,Percept1,Time1),
  Time is Time1 + (T2 - T1).


initialize(World,[Stench,Breeze,Glitter,no,no]) :-
  initialize_world(World),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter).
%  display_action(initialize).



initialize_world(fig72) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(3,1)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(4,1)),
  addto_ww_init_state(pit(4,4)),
  addto_ww_init_state(pit(4,2)),
  ww_initial_state(L),
  assert_list(L).

initialize_world(random) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
%WNb
  wumpus_world_default_extent(E),
  retract(max_agent_actions(_)),
  M is (8*E*E), 
  assert(max_agent_actions(M)),
  addto_ww_init_state(wumpus_world_extent(E)),
  all_squares(E,AllSqrs),
  gold_probability(PG),             
  place_objects(gold,PG,AllSqrs,GRestSqrs),
  at_least_one_gold(E,AllSqrs,GRestSqrs,PSqrs),
  delete(AllSqrs,[1,1],AllSqrs1),   
  random_member([WX,WY],AllSqrs1),  
  delete(PSqrs,[1,1],PSqrs1),       
  delete(PSqrs1,[WX,WY],PSqrs2),    
  pit_probability(PP),              
  place_objects(pit,PP,PSqrs2,_),
  
%WNe
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_health(alive)),
  ww_initial_state(L),
  assert_list(L).


initialize_agent :-
  retractall(agent_location(_,_)),
  retractall(agent_orientation(_)),
  retractall(agent_in_cave(_)),
  retractall(agent_health(_)),
  retractall(agent_gold(_)),
  retractall(agent_arrows(_)),
  retractall(agent_score(_)),
  assert(agent_location(1,4)),
  assert(agent_orientation(0)),
  assert(agent_in_cave(yes)),
  assert(agent_health(alive)),
  assert(agent_gold(0)),
  assert(agent_arrows(2)),
  assert(agent_score(0)).


ww_retractall :-
  retractall(wumpus_world_extent(_)),
  retractall(wumpus_location(_,_)),
  retractall(wumpus_health(_)),
  retractall(gold(_,_)),
  retractall(pit(_,_)).


addto_ww_init_state(Fact) :-
  retract(ww_initial_state(L)),
  assert(ww_initial_state([Fact|L])).



assert_list([]).

assert_list([Fact|Facts]) :-
  assert(Fact),
  assert_list(Facts).



all_squares(Extent,AllSqrs) :-
  all_squares_1(Extent,1,1,AllSqrs).

all_squares_1(Extent,Extent,Extent,[[Extent,Extent]]).

all_squares_1(Extent,Row,Extent,[[Row,Extent]|RestSqrs]) :-
  Row < Extent,
  Row1 is Row + 1,
  all_squares_1(Extent,Row1,1,RestSqrs).

all_squares_1(Extent,Row,Col,[[Row,Col]|RestSqrs]) :-
  Col < Extent,
  Col1 is Col + 1,
  all_squares_1(Extent,Row,Col1,RestSqrs).


%WNb
place_objects(_,_,[],[]).

place_objects(Object,P,[Square|Squares],RestSquares) :-
  maybe(P),   % succeeds with probability P
  !,
  Fact =.. [Object|Square],
  addto_ww_init_state(Fact),
  place_objects(Object,P,Squares,RestSquares).

place_objects(Object,P,[Square|Squares],[Square|RestSquares]) :-
  place_objects(Object,P,Squares, RestSquares).


at_least_one_gold(_,AllSqrs,GRestSqrs,GRestSqrs) :-
  \+ AllSqrs=GRestSqrs,
  !.

at_least_one_gold(E,AllSqrs,AllSqrs,GRestSqrs) :-
   random3(1,E,X),
   random3(1,E,Y),
   delete(AllSqrs,[X,Y],GRestSqrs),
%WNe
  addto_ww_init_state(gold(X,Y)).



execute(_,[no,no,no,no,no]) :-
  agent_health(dead), !,         
  format("You are dead!~n",[]).

execute(_,[no,no,no,no,no]) :-
  agent_in_cave(no), !,         
  format("You have left the cave.~n",[]).

execute(goforward,[Stench,Breeze,Glitter,Bump,no]) :-
  decrement_score,
  goforward(Bump),        
  update_agent_health,    
  stench(Stench),         
  breeze(Breeze),
  glitter(Glitter),
  display_action(goforward).

execute(turnleft,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(turnleft).

execute(turnright,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 270) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(turnright).

execute(grab,[Stench,Breeze,no,no,no]) :-
  decrement_score,
  get_the_gold,
  stench(Stench),
  breeze(Breeze),
  display_action(grab).

execute(shoot,[Stench,Breeze,Glitter,no,Scream]) :-
  decrement_score,
  shoot_arrow(Scream),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(shoot).

execute(climb,[no,no,no,no,no]) :-  % climb works
  agent_location(1,4), !,
  decrement_score,
  agent_gold(G),
  retract(agent_score(S)),
  S1 is (S + (1000 * G)),
  assert(agent_score(S1)),
  retract(agent_in_cave(yes)),
  assert(agent_in_cave(no)),
  display_action(climb),
  format("I am outta here.~n",[]).

execute(climb,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(climb),
  format("You cannot leave the cave from here.~n",[]).


decrement_score :-
  retract(agent_score(S)),
  S1 is S - 1,
  assert(agent_score(S1)).
  
% #########################################################################################################################################
% #########################################################################################################################################
% #########################################################################################################################################
%% Excercise 1:

%% Complete the below codes:

stench(yes) :-

  X1 is X + 1,
  ***
  ( ***
    wumpus_location(X,Y) ),
  !.


breeze(yes) :-

  X1 is X + 1,
  ***
  ( 
	***
    pit(X,Y)  ),
  !.


glitter(yes) :-
  
  ***,
  !.
% #########################################################################################################################################
% #########################################################################################################################################
% #########################################################################################################################################

breeze(no).

glitter(no).

stench(no).

kill_wumpus :-
  retract(wumpus_health(alive)),
  assert(wumpus_health(dead)).



goforward(no) :-
  agent_orientation(Angle),
  agent_location(X,Y),
  new_location(X,Y,Angle,X1,Y1),
  wumpus_world_extent(E),         
  X1 > 0,
  X1 =< E,
  Y1 > 0,
  Y1 =< E,
  !,
  retract(agent_location(X,Y)),   
  assert(agent_location(X1,Y1)).

goforward(yes).     

% #########################################################################################################################################
% #########################################################################################################################################
% #########################################################################################################################################
%% Excercise 2:

%% Complete the below codes:
%% you can use this piece of code as hints:

%% new_location(X,Y,Angle,X1,Y1)
%% display_agent(  0,'>').
%% display_agent( 90,'^').
%% display_agent(180,'<').
%% display_agent(270,'V').

new_location(X,Y,0,X1,Y) :-
  ***

new_location(X,Y,90,X,Y1) :-
  ***

new_location(X,Y,180,X1,Y) :-
  ***

new_location(X,Y,270,X,Y1) :-
  ***

% #########################################################################################################################################
% #########################################################################################################################################
% #########################################################################################################################################

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  wumpus_health(alive),
  wumpus_location(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 1500, %WN
  assert(agent_score(S1)),
  format("You are Wumpus food!~n",[]).

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  pit(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 1500,  %WN
  assert(agent_score(S1)),
  format("Aaaaaaaaaaaaaaaaaaa!~n",[]).

%WNb
update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  pit(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 1500,  %WN
  assert(agent_score(S1)),
  format("Aaaaaaaaaaaaaaaaaaa!~n",[]).
%WNe

update_agent_health.



get_the_gold :-
  agent_location(X,Y),
  gold(X,Y), !,                   
  agent_gold(NGold),              
  NGold1 is NGold + 1,
  retract(agent_gold(NGold)),
  assert(agent_gold(NGold1)),
  format("You now have ~d piece(s) of gold!~n",NGold1),
  retract(gold(X,Y)).             

get_the_gold.



shoot_arrow(Scream) :-
  agent_arrows(Arrows),
  Arrows > 0, !,                  
  Arrows1 is Arrows - 1,          
  retract(agent_arrows(Arrows)),
  assert(agent_arrows(Arrows1)),
  format("You now have ~d arrow(s).~n",Arrows1),
  agent_location(X,Y),
  agent_orientation(Angle),
  propagate_arrow(X,Y,Angle,Scream).

shoot_arrow(no).



propagate_arrow(X,Y,_,yes) :-
  wumpus_location(X,Y), !,
%WNb
  kill_wumpus,
  retract(agent_score(S)),
  S1 is (S + 500),
  assert(agent_score(S1)).
%WNe

propagate_arrow(X,Y,0,Scream) :-
  X1 is X + 1,
  wumpus_world_extent(E),
  X1 =< E,
  !,
  propagate_arrow(X1,Y,0,Scream).

propagate_arrow(X,Y,90,Scream) :-
  Y1 is Y + 1,
  wumpus_world_extent(E),
  Y1 =< E,
  !,
  propagate_arrow(X,Y1,90,Scream).

propagate_arrow(X,Y,180,Scream) :-
  X1 is X - 1,
  X1 > 0,
  !,
  propagate_arrow(X1,Y,180,Scream).

propagate_arrow(X,Y,270,Scream) :-
  Y1 is Y - 1,
  Y1 > 0,
  !,
  propagate_arrow(X,Y1,270,Scream).

propagate_arrow(_,_,_,no).



display_world :-
  nl,
  wumpus_world_extent(E),
%WNb
  display_rows(E,E).
%WNe

display_rows(0,E) :-
  !,
  display_dashes(E).

display_rows(Row,E) :-
  display_dashes(E),
  display_row(Row,E),
  Row1 is Row - 1,
  display_rows(Row1,E).

display_row(Row,E) :-
  display_square(1,Row,E).

display_square(X,_,E) :-
  X > E,
  !,
  format('|~n',[]).

display_square(X,Y,E) :-
  format('|',[]),
  display_info(X,Y),
  X1 is X + 1,
  display_square(X1,Y,E).

display_info(X,Y) :-
%WNb
  agent_orientation(AO),
  display_agent(AO,AC),
  display_location_fact(agent_location,X,Y,AC),
  display_location_fact(gold,X,Y,'G'),
  display_location_fact(pit,X,Y,'P'),
  write(' '),
  wumpus_health(WH),
  display_wumpus(WH,WC),
  display_location_fact(wumpus_location,X,Y,WC).
%WNe

display_location_fact(Functor,X,Y,Atom) :-
  Fact =.. [Functor,X,Y],
  Fact,
  !,
  format('~w',[Atom]). %WN

display_location_fact(_,_,_,_) :-
  format(' ',[]).

display_dashes(E) :-
  RowLen is (E * 6) + 1, %WN
  name('-',[Dash]),
  format('~*c~n',[RowLen,Dash]).

%WNb

display_wumpus(alive,'W').
display_wumpus(dead, 'd').


display_agent(  0,'>').
display_agent( 90,'^').
display_agent(180,'<').
display_agent(270,'V').
%WNe


display_action(Action) :-
  format("~nExecuting ~w~n",[Action]).
%  (((\+ hidden_cave(yes)) -> display_world);true).

