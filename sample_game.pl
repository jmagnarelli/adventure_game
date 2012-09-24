/*
SWI-Prolog requires us to declare as dynamic any predicates that may change.
Also retract all existing statements in the knowledge base, for easier restart.
*/

/*NOTE:  I know it was mentioned in class that you wanted us to write "the 
function that figures out whether two rooms are connected" (paraphrasing). 
I decided to represent my rooms differently than the sample game does, so my
function is simpler.  Just so you know I can reason this through, I wanted to
show how I would have written that function if I were using the 
path(bridge, e, office) representation for rooms:

route :-

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

start :- write('Welcome to Prism.  This game has three difficulty settings. \
				If you are playing this as a grader, and you are looking to finish \
				the game very quickly, you may want to play on easy.  \
				See the README for a list of features you\'ll miss out on \
				when playing on easy. \n\n  Hints are available at any time \
				through the help(). rule.  You are encouraged to play on a hard \
				difficulty setting, if you have time.  Set the difficulty now by 
				calling the set_diff(x) rule with an integer argument 0-2.  Higher \
				is harder.  If you do not call set_diff, difficulty 0 is assumed.\n\n\
				This is not quite the same as other text adventure games \
				you may have played.  \n\n You may need a pencil and paper.'), nl,
		look.

/*
World Setup
*/	
path(0, 1, e).
path(0, 3, s).
path(0, 9, u).
path(1, 2, e).
path(1, 4, s).
path(1, 10, u).
path(2, 5, s).
path(2, 11, u).
path(3, 4, e).
path(3, 6, s).
path(3, 12, u).
path(4, 5, e).
path(4, 7, s).
path(4, 13, u).
path(5, 8, s).
path(5, 14, u).
path(6, 7, e).
path(6, 15, u).
path(7, 8, e).
path(7, 16, u).
path(8, 17, u).
path(9, 10, e).
path(9, 12, s).
path(10, 11, e).
path(10, 13, s).
path(11, 14, s).
path(12, 13, e).
path(12, 15, s).
path(13, 14, e).
path(13, 16, s).
path(14, 17, s).
path(15, 16, e).
path(15, 18, u).
path(16, 17, e).
path(16, 19, u).



opp_dir(e, w).
opp_dir(w, e).
opp_dir(n, s).
opp_dir(s, n).
opp_dir(u, d).
opp_dir(d, u).


movement_string(u, 'You climb up the small ladder running up the wall and across the ceiling. \
					with great effort, you heave yourself through the hatch in the ceiling, emerging \
					through the floor of another room.').
movement_string(d, 'You gingerly lower yourself through the hatch in the floor, straining as you \
					make your way to the wall and down the rungs of the ladder there.').
movement_string(e, 'You open the hatch to the east, and crawl through.').
movement_string(w, 'You open the hatch to the west, and crawl through.').
movement_string(n, 'You open the hatch to the north, and crawl through.').
movement_string(s, 'You open the hatch to the south, and crawl through.').


of(_, rungs, 'They run up the center of each wall, directly to the hatches in the walls and ceiling. \
				The rungs are small, but large enough to support a single hand or foot. \
				You could climb them.').
of(_, hatch, 'It's a square hatch, about 3 feet on each side.  There is a handle in the center \
				that looks like it can be turned.  A small inscription is below the hatch.').
of(_, panel, 'They are made of a thick material.  Through them, you can make out faint \
				outlines of electrical components.').


of(0, inscription, 'The inscription reads 0 0 0.  It appears to be laser-etched.').
of(1, inscription, 'The inscription reads 1 0 0.  It appears to be laser-etched.').
of(2, inscription, 'The inscription reads 2 0 0.  It appears to be laser-etched.').
of(3, inscription, 'The inscription reads 0 1 0.  It appears to be laser-etched.').
of(4, inscription, 'The inscription reads 1 1 0.  It appears to be laser-etched.').
of(5, inscription, 'The inscription reads 2 1 0.  It appears to be laser-etched.').
of(6, inscription, 'The inscription reads 0 2 0.  It appears to be laser-etched.').
of(7, inscription, 'The inscription reads 1 2 0.  It appears to be laser-etched.').
of(8, inscription, 'The inscription reads 2 2 0.  It appears to be laser-etched.').
of(9, inscription, 'The inscription reads 0 0 1.  It appears to be laser-etched.').
of(10, inscription, 'The inscription reads 1 0 1.  It appears to be laser-etched.').
of(11, inscription, 'The inscription reads 2 0 1.  It appears to be laser-etched.').
of(12, inscription, 'The inscription reads 0 1 1.  It appears to be laser-etched.').
of(13, inscription, 'The inscription reads 1 1 1.  It appears to be laser-etched.').
of(14, inscription, 'The inscription reads 2 1 1.  It appears to be laser-etched.').
of(15, inscription, 'The inscription reads 0 2 1.  It appears to be laser-etched.').
of(16, inscription, 'The inscription reads 1 2 1.  It appears to be laser-etched.').
of(17, inscription, 'The inscription reads 2 2 1.  It appears to be laser-etched.').
of(18, inscription, 'The inscription reads 0 2 2.  It appears to be laser-etched.').
of(19, inscription, 'The inscription reads 1 2 2.  It appears to be laser-etched.').

i_am_at(4). 			%player's initial location

at(office, communicator).	%there is a communicator object in the office

%of is just like at, but serves to map descriptions to location
of(bridge, 'You are in the office!').

game_difficulty(0).
max_x_coord(2).
max_y_coord(1).
max_z_coord(1).
moving_rooms(0).

cur_x_coord(1).
cur_y_coord(0).
cur_z_coord(0).



/*
Verbs
*/


	
	

	

%Path - is there a path between the two rooms in the given direction?
%Each room is a 3-element list representing the room's x, y, z coordinates.
%A is source, B is dest, dir is one of N, S, E, W, U, D
path(A, B, Dir) :-
	path(A, B, Dir).
path(A, B, Dir) :-
	opp_dir(Dir, Opp_Dir),
	path(B, A, Opp_Dir).


%Move - if the movement is valid, move the player.
move(Dir) :-
	i_am_at(X),
	path(X, Dir, Y),
	retract(i_am_at(X)),
	assert(i_am_at(Y)),
	movement_string(Dir, Str),
	write(Str), nl,
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

examine(X) :-
	i_am_at(Loc),
	of(Loc, X, Des),
	write(Des), nl.
	



%Look - describe where in space the player is, list objects at the location
look :-
	i_am_at(X),
	write('You are standing in a perfectly cubic room.  Its walls are gleaming, \
			translucent, white panels.  There is a hatch in the center of each wall, the \
			ceiling, and the floor.  There is a series of rungs set into the \
			 middle of  each wall, the ceiling, and the floor.'), nl,
	list_objects_at(X).	

%List objects - these two rules effectively form a loop that go through every object
%				in the location and writes them out.
list_objects_at(X) :-
	at(X, Obj),
	write('There is a '), write(Obj), write(' on the ground.'), nl,
	fail.

list_objects_at(_).

%list features - these two rules loop through each feature of the rooms in question
list_features_of(X) :-
	of(X, Feat, Des),
	write(Des), nl,
	fail.

list_features_of(_).
	
describe(_) :- 
	i_am_at(X),
	of(X, Des),
	write(Des), nl,
	fail.

