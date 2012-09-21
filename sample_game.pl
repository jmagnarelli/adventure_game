/*
SWI-Prolog requires us to declare as dynamic any predicates that may change.
Also retract all existing statements in the knowledge base, for easier restart.
*/

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), 
	retractall(i_am_at(_)), 
	retractall(holding(_)).
	
/*
start is the first rule we will run when grading your game. Anything you want to be
seen first should go in here. This one just prints out where you currently are in
the world.
*/

start :- look.

/*
World Setup
*/	
path(bridge, e, office).	%there is a path from the bridge to the office moving east
at(office, communicator).	%there is a communicator object in the office

%of is just like at, but serves to map descriptions to location
of(bridge, 'You are in the office!').


i_am_at(bridge). 			%player's initial location

/*
Verbs
*/

%Move - if the movement is valid, move the player.
move(Dir) :-
	i_am_at(X),
	path(X, Dir, Y),
	retract(i_am_at(X)),
	assert(i_am_at(Y)),
	write('You have moved to '), write(Y), write('.'), nl,
	look,
	!.
	
%Move - otherwise, tell the player they can't move.
move(_) :-
	write('You cannot move that direction.'), nl.
	
%Pick up object - if already holding the object, can't pick it up!
pickup(X) :-
	holding(X),
	write('You are already holding it!'), nl,
	!.

%Pick up object - if in the right location, pick it up and remove it from the ground.
pickup(X) :-
	i_am_at(Place),
	at(Place, X),
	retract(at(Place, X)),
	assert(holding(X)),
	write('You have picked it up.'), nl,
	!.
	
%Pick up object - otherwise, cannot pick up the object.
pickup(_) :-
	write('I do not see that here.'), nl.


%Look - describe where in space the player is, list objects at the location
look :-
	i_am_at(X),
	describe(X),
	list_objects_at(X).	

%List objects - these two rules effectively form a loop that go through every object
%				in the location and writes them out.
list_objects_at(X) :-
	at(X, Obj),
	write('There is a '), write(Obj), write(' on the ground.'), nl,
	fail.

list_objects_at(_).
	
describe(_) :- 
	i_am_at(X),
	of(X, Des),
	write(Des), nl,
	fail.

