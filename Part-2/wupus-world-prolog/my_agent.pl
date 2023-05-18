%my_agent.pl

:- dynamic
    is_wumpus/2,
    is_pit/2,
    is_gold/1,
    is_wall/1,
    is_dead,
    is_visited/1,
    is_gold_picked/1,
    path.


init_agent :-
	format('\n=====================================================\n'),
	format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),
	format('=====================================================\n\n'),
	retractall(is_wumpus(_, _)),
	retractall(is_pit(_, _)),
	retractall(is_gold(_)),
	retractall(is_wall(_)),
	retractall(is_dead),
	retractall(is_visited(_)),
	retractall(is_gold_picked(_)),
	retractall(path(_)),
	assert(path([])).

run_agent(Percept, goforward) :-
	agent_location(L),
	retractall(is_visited(L)),
	assert(is_visited(L)),
	display_world,
	inform(Percept),
	pick_gold,
	update_orientation,
	display_world,
	format('=====================================================\n\n').

push(X) :-
	path(P),
	retractall(path(P)),
	append([X], P, P1),
	assert(path(P1)),
	!.

pop(X) :-
	path([X|P]),
	retractall(path(_)),
	assert(path(P)),
	format('\nBACK\n'),
	!.

pick_gold :-
	agent_location(L),
	is_gold(L),
	execute(grab, _),
	assert(is_gold_picked(yes)),
	pop(_),
	!.

pick_gold.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agent_orientation(L) :-
	wumpus: agent_orientation(L).

update_orientation :-
	\+ is_gold_picked(yes),
	update_orientation_first,
	!.

update_orientation :-
	agent_location([X, Y]),
	X =:= 1,
	Y =:= 4,
	execute(climb, _),
	format('\n'),
	format('######################################## \n'),
	format('######################################## \n'),
	format('######################################## \n'),
	format('########$$$$ You  Succeeded $$$$######## \n'),
	format('######################################## \n'),
	format('######################################## \n'),
	format('######################################## \n'),
	format('\n'),
	!.

update_orientation :-
	agent_location(L),
	pop(L1),
	location_toward(L, O, L1),
	rotate_to(O),
	!.

rotate_to(O) :-
	agent_orientation(O1),
	O1 =:= O,
	!.

rotate_to(O) :-
	execute(turnleft, _),
	rotate_to(O),
	!.

update_orientation_first :-
	location_ahead(L1, 0),
	\+ is_wall(L1),
	\+ is_wumpus(yes, L1),
	\+ is_pit(yes, L1),
	\+ is_visited(L1),
	!.

update_orientation_first :-
	location_ahead(L1, 90),
	\+ is_wall(L1),
	\+ is_wumpus(yes, L1),
	\+ is_pit(yes, L1),
	\+ is_visited(L1),
	execute(turnleft, _),
	!.

update_orientation_first :-
	location_ahead(L1, 180),
	\+ is_wall(L1),
	\+ is_wumpus(yes, L1),
	\+ is_pit(yes, L1),
	\+ is_visited(L1),
	wumpus: execute(turnleft, _),
	wumpus: execute(turnleft, _),
	!.

update_orientation_first :-
	location_ahead(L1, 270),
	\+ is_wall(L1),
	\+ is_wumpus(yes, L1),
	\+ is_pit(yes, L1),
	\+ is_visited(L1),
	wumpus: execute(turnright, _),
	!.

update_orientation_first :-
	location_ahead(L1, 0),
	\+ is_wall(L1),
	\+ is_wumpus(yes, L1),
	\+ is_pit(yes, L1),
	!.

update_orientation_first :-
	location_ahead(L1, 90),
	\+ is_wall(L1),
	\+ is_wumpus(yes, L1),
	\+ is_pit(yes, L1),
	execute(turnleft, _),
	!.

update_orientation_first :-
	location_ahead(L1, 180),
	\+ is_wall(L1),
	\+ is_wumpus(yes, L1),
	\+ is_pit(yes, L1),
	wumpus: execute(turnleft, _),
	wumpus: execute(turnleft, _),
	!.

update_orientation_first :-
	location_ahead(L1, 270),
	\+ is_wall(L1),
	\+ is_wumpus(yes, L1),
	\+ is_pit(yes, L1),
	wumpus: execute(turnright, _),
	!.

update_orientation_first :-
	location_ahead(L1, 0),
	\+ is_wall(L1),
	!.

update_orientation_first :-
	location_ahead(L1, 90),
	\+ is_wall(L1),
	execute(turnleft, _),
	!.

update_orientation_first :-
	location_ahead(L1, 180),
	\+ is_wall(L1),
	wumpus: execute(turnleft, _),
	wumpus: execute(turnleft, _),
	!.

update_orientation_first :-
	location_ahead(L1, 270),
	\+ is_wall(L1),
	wumpus: execute(turnright, _),
	!.

location_ahead(Ahead, Turn) :-
	agent_location(L),
	agent_orientation(O),
	O1 is O + Turn,
	O_New is mod(O1, 360),
	location_toward(L, O_New, Ahead).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inform([_, _, _, yes, _]) :- 
	add_wall(yes),
	!.

inform([Stench, Bleeze, Glitter, no, Scream]) :-
	trace_path,
	add_wumpus(Stench),
	add_pit(Bleeze),
	add_gold(Glitter),
	add_scream(Scream).

trace_path :-
	\+ is_gold_picked(yes),
	agent_location(L),
	push(L),
	!.

trace_path.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agent_location([X, Y]) :-
	wumpus: agent_location(X, Y).

add_wumpus(no) :-
	agent_location(L1),
	assume_wumpus(no, L1),
	location_toward(L1, 0, L2),
	assume_wumpus(no, L2),
	location_toward(L1, 90, L3),
	assume_wumpus(no, L3),
	location_toward(L1, 180, L4),
	assume_wumpus(no, L4),
	location_toward(L1, 270, L5),
	assume_wumpus(no, L5),
	!.

add_wumpus(yes) :-
	execute(shoot, _),
	agent_location(L1),
	location_toward(L1, 0, L2),
	assume_wumpus(yes, L2),
	location_toward(L1, 90, L3),
	assume_wumpus(yes, L3),
	location_toward(L1, 180, L4),
	assume_wumpus(yes, L4),
	location_toward(L1, 270, L5),
	assume_wumpus(yes, L5).
	
assume_wumpus(no, L) :-
	retractall(is_wumpus(_, L)),
	assert(is_wumpus(no, L)),
	!.

assume_wumpus(yes, L) :-
	is_wumpus(no, L),
	!.
	
assume_wumpus(yes, L) :- 		
	is_wall(L),

	retractall(is_wumpus(_, L)),
	assert(is_wumpus(no, L)),
	!.

assume_wumpus(yes, L) :- 
	wumpus:wumpus_health(alive),
	retractall(is_wumpus(_, L)),
	assert(is_wumpus(yes, L)),
	!.

assume_wumpus(yes, L) :-
	retractall(is_wumpus(_, L)),
	assert(is_wumpus(no, L)).

add_pit(no) :-
	agent_location(L1),
	assume_pit(no, L1),
	location_toward(L1, 0, L2),
	assume_pit(no, L2),
	location_toward(L1, 90, L3),	
	assume_pit(no, L3),
	location_toward(L1, 180, L4),	
	assume_pit(no, L4),
	location_toward(L1, 270, L5),	
	assume_pit(no, L5),
	!.

add_pit(yes) :-	
	agent_location(L1),
	location_toward(L1, 0, L2),
	assume_pit(yes, L2),
	location_toward(L1, 90, L3),	
	assume_pit(yes, L3),
	location_toward(L1, 180, L4),	
	assume_pit(yes, L4),
	location_toward(L1, 270, L5),	
	assume_pit(yes, L5).
	
assume_pit(no, L) :-
	retractall(is_pit(_, L)),
	assert(is_pit(no, L)),
	!.
	
assume_pit(yes, L) :-
	is_pit(no, L),
	!.
	
assume_pit(yes, L) :- 
	is_wall(L),
	retractall(is_pit(_, L)),
	assert(is_pit(no, L)),
	!.
	
assume_pit(yes, L) :- 
	retractall(is_pit(_, L)),
	assert(is_pit(yes, L)).	

add_gold(yes) :-
	agent_location(L),
	retractall(is_gold(L)),
	assert(is_gold(L)),
	!.

add_gold(no).		

add_wall(yes) :-
	location_ahead(L, 0),
	retractall(is_wall(L)),	
	assert(is_wall(L)),
	!.					

add_wall(no).

add_scream(yes) :-
	retractall(is_dead),
	assert(is_dead),
	!.
add_scream(no).

location_toward([X, Y], 0,[New_X, Y]) :- New_X is X+1.
location_toward([X, Y], 90,[X, New_Y]) :- New_Y is Y+1.
location_toward([X, Y], 180,[New_X, Y]) :- New_X is X-1.
location_toward([X, Y], 270,[X, New_Y]) :- New_Y is Y-1.

adjacent(L1, L2) :- location_toward(L1, _, L2).
