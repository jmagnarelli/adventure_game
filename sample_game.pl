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
%path(0, 9, u).
path(1, 2, e).
path(1, 4, s).
%path(1, 10, u).
path(2, 5, s).
%path(2, 11, u).
path(3, 4, e).
path(3, 6, s).
path(3, 12, u).
path(4, 5, e).
path(4, 7, s).
path(4, 13, u).
path(5, 8, s).
path(5, 14, u).
path(6, 7, e).
%path(6, 15, u).
path(7, 8, e).
%path(7, 16, u).
%path(8, 17, u).
%path(9, 10, e).
%path(9, 12, s).
%path(10, 11, e).
%path(10, 13, s).
%path(11, 14, s).
path(12, 13, e).
%path(12, 15, s).
path(13, 14, e).
%path(13, 16, s).
path(14, 17, s).
%path(15, 16, e).
%path(15, 18, u).
%path(16, 17, e).
%path(16, 19, u).
%path(17, 26, u).



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
%of(9, inscription, 'The inscription reads 0 0 1.  It appears to be laser-etched.').
%of(10, inscription, 'The inscription reads 1 0 1.  It appears to be laser-etched.').
%of(11, inscription, 'The inscription reads 2 0 1.  It appears to be laser-etched.').
of(12, inscription, 'The inscription reads 0 1 1.  It appears to be laser-etched.').
of(13, inscription, 'The inscription reads 1 1 1.  It appears to be laser-etched.').
of(14, inscription, 'The inscription reads 2 1 1.  It appears to be laser-etched.').
%of(15, inscription, 'The inscription reads 0 2 1.  It appears to be laser-etched.').
%of(16, inscription, 'The inscription reads 1 2 1.  It appears to be laser-etched.').
of(17, inscription, 'The inscription reads 2 2 1.  It appears to be laser-etched.').
%of(18, inscription, 'The inscription reads 0 2 2.  It appears to be laser-etched.').
%of(19, inscription, 'The inscription reads 1 2 2.  It appears to be laser-etched.').
of(26, inscription, 'The inscription reads 2 2 2.  It appears to be laser-etched.').

of(_, rubber_ducky, 'A small, cute, rubber ducky.  It squeaks when squeezed.').
of(_, crazed_man, 'He is smelly, and is wearing tattered clothing.  He seems \
					to be frothing at the mouth.  May be dangerous.').
of(_, flashlight, 'Upon closer inspection, it doesn't seem to be working.').
%of(_, book_of_matches, 'Wooden matches.  You count them, and find that .').
of(_, spam, 'A delicious canned meat product!  The package claims that it is \'Family-sized\'.').
of(_, computer, 'A large desktop computer is attached to the wall in the corner.  It is running.').
of(_, black_floppy_disk, 'A black floppy disk.  It is not labeled.').
of(_, green_floppy_disk, 'A green floppy disk.  It is not labeled.').
of(_, white_floppy_disk, 'A white floppy disk.  It is not labeled.').
of(_, pink_floppy_disk, 'A pink floppy disk.  It is not labeled.').
of(_, slip_of_paper, 'The note is singed around the edges.  It reads \'NO WAY OUT\'').
of(_, charred_remains, 'You cannot bring yourself to look at them - the stench is bad enough.').
of(_, ripe_banana, 'Yellow and nutricious!').
of(_, suspicious_can, 'It seems to be an old gas can.  A sniff confirms this suspicion.').
of(_, extension_cord, 'About 100 feet of 3-pronged extension cord.').
of(_, old_woman, 'She is muttering to herself, sitting against the far wall.').
of(_, rusty_spoon, 'An old, rusty, tablespoon.').
%of(_, matches, 'A small, cute, rubber ducky.  It squeaks when squeezed.').
of(_, box_of_toothpicks, 'A box of countless toothpicks.  Mint flavored.').
of(_, man, 'A large man, perhaps in his late twenties.  He has just \
			begun to pick his nose. \n\n  He appears confused.').

i_am_at(4). 			%player's initial location

%descriptions for the rooms

%generic description
room_des(_, 'You are standing in a perfectly cubic room, about 15 feet to a side.  Its walls are gleaming, \
			translucent, white panels.  There is a hatch in the center of each wall, the \
			ceiling, and the floor.  There is a series of rungs set into the \
			 middle of  each wall, the ceiling, and the floor.').

at(0, rubber_ducky).
at(2, crazed_man).
at(3, flashlight). %there is a flashlight in room 4
at(4, book_of_matches).
at(6, spam).
at(7, computer).
at(7, black_floppy_disk).
at(8, slip_of_paper).
at(8, charred_remains).
at(8, green_floppy_disk).
at(8, ripe_banana).
at(8, suspicious_can).
at(12, extension_cord).
at(12, pink_floppy_disk).
at(12, old_woman).
at(13, rusty_spoon).
at(13, box_of_matches).
at(14, box_of_toothpicks).
at(12, man).
at(17, white_floppy_disk).

%item characteristics
total_consumption(spam, man).
total_consumption(rubber_ducky, old_woman).
total_consumption(green_floppy_disk, computer).
total_consumption(pink_floppy_disk, computer).
total_consumption(white_floppy_disk, computer).
total_consumption(black_floppy_disk, computer).
total_consumption(ripe_banana).
total_consumption(rusty_spoon, crazed_man).
total_consumption(box_of_toothpicks, man).

no_consumption(spam).
no_consumption(rubber_ducky).
no_consumption(crazed_man).
no_consumption(computer).
no_consumption(extension_cord).
no_consumption(old_woman).
no_consumption(man).
no_consumption(box_of_toothpicks).

partial_consumption(book_of_matches).
partial_consumption(box_of_matches).

cant_pick_up(computer, 'That is too bulky for you to carry.').
cant_pick_up(crazed_man, 'He snarls at you, and shoves you away with \
						calloused, grubby hands').
cant_pick_up(charred_remains, 'You cannot bring yourself to do that.').
cant_pick_up(old_woman, 'She mumbles as you try to pick her up, but you find that\
						 she is too heavy').
cant_pick_up(dillon, 'He quite large and muscular, probably too large for you to carry. \
					You try to pick him up anyway.  He whimpers and cries, so you put him down.').

%what can be used with what?
usable(rubber_ducky).
usable(spam, man).
usable(rubber_ducky, man).
usable(green_floppy_disk, computer).
usable(white_floppy_disk, computer).
usable(black_floppy_disk, computer).
usable(pink_floppy_disk, computer).
usable(extension_cord, scaffold_apparatus).
usable(ripe_banana).
usable(rusty_spoon, crazed_man).
usable(spam).
usable(rubber_ducky).
usable(crazed_man).
usable(computer).
usable(extension_cord).
usable(old_woman).
usable(man).
usable(box_of_toothpicks).

usable(box_of_toothpicks, man) :=
	fed_spam_to_man().

%what happens when we use these things with each other?
use_item(ripe_banana, Des) :-
	Des is 'You eat the banana.  Yum!'.

use_item(rusty_spoon, crazed_man, Des) :-
	assert((killed_crazed_man()),
	Des is 'You fight off the crazed man using only your rusty spoon. \n\n\
			The details are best left unspecified.'.

use_item(spam, Des) :-
	Des is 'You eat a little of the Spam.  It tastes about as you would expect it to.'.

use_item(rubber_ducky, Des) :-
	Des is 'QUACK!'.

use_item(crazed_man, Des) :-
	assert(kill_chase()),
	Des is 'The man lunges at you. He is attacking!'.

use_item(computer, Des) :-
	Des is 'Awaiting input media...'.

use_item(extension_cord, scaffold_apparatus, Des) :-
	Des is 'You plug in the apparatus, which begins deploying a walkway to the outer wall.  \
			When it has reached the far side, you and Dillon eagerly make your way across the chasm \
			and to the exterior door beyond which, hopefully, freedom awaits...\n\n\n YOU WIN!'.
use_item(old_woman, Des) :-
	Des is 'She begins ranting: \'Nowayoutnowayout NO WAY OUT. Why? WHERE?  ROOMSroomsrooms so many rooms...\''.

use_item(man, Des) :-
	(man_transport()
	-> 
		(i_am_at(17)
		->retract(i_am_at(17)),
		retract(at(man, 17)),
		assert(i_am_at(26)),
		assert(at(man, 26)),
		write('The man exclaims, \'Wheeeeeeee!\''), nl,
		look,
		!
		; retract(i_am_at(26)),
		retract(at(man, 26)),
		assert(i_am_at(17)),
		assert(at(man, 17)),
		write('The man exclaims, \'Wheeeeeeee!\''), nl,
		look,
		!)
	; Des is 'He looks up at you, index finger knuckle deep in his left nostril, and speaks: \'\
			Hi! My name\'s Dillon. Nice ta meet ya I guess.  \n\n I have a special place I like to \
			go to when I feel happy.  But I don\'t feel happy now...'
	).
use_item(box_of_toothpicks, Des) :-
	Des is 'You take one of the toothpicks and put it in your mouth.  You work it between your teeth \
			as you walk.'.
	
use_item(spam, man, Des) :-
	assert(fed_spam_to_man()),
	Des is 'With an ear-to-ear grin, he tears open the can and slides the entire \
			brick of spam into his mouth.  Happily, he chews if for a minute or so. \n\n\
			After swallowing, he begins picking at his teeth.  He seems to be growing more \
			frustrated by the second.'.
use_item(box_of_toothpicks, man, Des) :-
	assert(man_transport()),
	Des is 'The man says to you \'I am happy now.  Use me to get to the different place\''.

use_item(rubber_ducky, old_woman, Des) :-
	Des is 'She snatches the ducky from you and stuffs it into her pocket. \
			Her look makes it clear that you will not be getting it back.'.

use_item(

/*
Verbs
*/

reachable(A) :-
	at(inventory, A).
reachable(A) :-
	i_am_at(X),
	at(X, A).

	
consume(A) :-
	(total_consumption(A)
	->reachable(A),
	at(X, A),
	retract(at(X, A))
	;(partial_consumption(A)
	->reachable(A),
	count(A, N),
	n > 0,
	M is N-1,
	retract(count(A, N)),
	assert(count(A, M))
	;no_consumption(A)
	)
	).

consume(A, B) :-
	(total_consumption(A, B)
	->reachable(A),
	at(X, A),
	retract(at(X, A))
	;(partial_consumption(A, B)
	->reachable(A),
	count(A, N),
	n > 0,
	M is N-1,
	retract(count(A, N)),
	assert(count(A, M))
	;no_consumption(A, B)
	)
	).

	
	
	
use(A, B) :-
	(usable(A, B),
	->reachable(A),
	reachable(B),
	use_item(A, B, Des),
	consume(A, B),
	consume(B, A),
	write(Des), nl
	;write('You cannot use '), write(A), write(' with '), write(B), nl
	).

use(A) :-
	(usable(A),
	->reachable(A),
	use_item(A, Des),
	consume(A),
	write(Des), nl
	;write('You cannot use '), write(A), nl
	).
	
	

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
	(kill_chase()
	->
		(at(crazed_man, X)
		%we're where the crazy man is 
		->write('You were caught by the crazed man, who proceeds to kill you. \n\nGAME OVER. Please restart.')
		;at(crazed_man, A),
		 retract(at(crazed_man, A)),
		 assert(at(crazed_man, X)).
		)
	;look,
	!
	).
	
%Move - otherwise, tell the player they can't move.
move(_) :-
	write('Try as you might to turn the handle, it won\t budge. You can\'t \
			go this way.'), nl.
	
%Pick up object - if already holding the object, can't pick it up!
pickup(X) :-
	holding(X),
	write('You are already holding it!'), nl,
	!.


%Pick up object - if in the right location, but not movable, print reason
pickup(X) :-
	i_am_at(Place),
	at(Place, X),
	cant_pick_up(X, Des),
	write(Des), nl,
	!.

%Pick up object - if in the right location and movable, pick it up and remove it from the ground.
pickup(X) :-
	i_am_at(Place),
	at(Place, X, Des0),
	\+ cant_pick_up(X, Des1),
	retract(at(Place, X, Des0)),
	assert(at(inventory, X, Des0)),
	write('You have picked up the'), write(X), nl,
	!.
	
%Pick up object - otherwise, cannot pick up the object.
pickup(_) :-
	write('I do not see that here.'), nl.

examine(X) :-
	i_am_at(Loc),
	at(Loc, X, Des),
	write(Des), nl.
%if it is in the player's inventory
examine(X) :-
	at(-1, X, Des),
	write(Des), nl.

examine(X) :-
	i_am_at(Loc),
	of(Loc, X, Des),
	write(Des), nl.
	



%Look - describe where in space the player is, list objects at the location
look :-
	i_am_at(X),
	room_des(X, Des),
	write(Des), nl,
	list_objects_at(X).	

i() :-
	inventory().

inventory() :-
	at(inventory, Obj),
	write('You are carrying a(n) '), write(Obj), nl.

%List objects - these two rules effectively form a loop that go through every object
%				in the location and writes them out.
list_objects_at(X) :-
	at(X, Obj),
	write('There is a(n) '), write(Obj), write(' on the ground.'), nl,
	fail.

list_objects_at(_).

%list features - these two rules loop through each feature of the rooms in question
list_features_of(X) :-
	of(X, Feat, Des),
	write(Des), nl,
	fail.

list_features_of(_).
	

