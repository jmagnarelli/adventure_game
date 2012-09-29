/*
Source code for Prism, submitted as a response for CS4100's Assignment 1 by James Magnarelli.
jmagnare@ccs.neu.edu, jmagnare@gmail.com
*/


%These will be changed at runtime
:- dynamic i_am_at/1, at/2, holding/1, match_lit/1, count/2, kill_chase/1, path/3, fed_spam_to_man/1,
			man_transport/1.
:- retractall(at(_, _)), 
	retractall(i_am_at(_)), 
	retractall(holding(_)).
	
/*
start is the first rule we will run when grading your game. Anything you want to be
seen first should go in here. This one just prints out where you currently are in
the world.
*/

start :- write('Welcome to Prism, a simple adventure game written in Prolog. \
You can move about with the \'move(<direction>)\' rule, \
where direction is one of n, s, e, w, u, or d.  Movement is allowed in six directions in Prism. \
You can examine just about anything in the game with the \'examine(<item>)\' rule, and you can pick \
up items with the \'take(<item>)\' rule.'), write('  Your inventory can be printed with the \'i\' rule or the \
\'inventory\' rule.  You can use items with the \'use(<item>)\' rule, and can use an item that you \
are carrying with another item via the \'use(<item1>, <item2>)\' rule, so long as the inventory item \
is given as the first parameter.  The use rule also allows you to talk to characters in the game. \
You can look about with the \'look\' rule.  Have fun.\n\n\n'), nl,
		look.

/*
World Setup
*/	
%connected rooms.  Each room has a number. 
path(0, 1, e). %get from room 0 to 1 by going east
path(0, 3, s).
path(1, 2, e).
path(1, 4, s).
path(2, 5, s).
path(3, 4, e).
path(3, 6, s).
path(4, 5, e).
path(4, 7, s).
path(5, 8, s).
path(6, 7, e).
path(7, 8, e).
path(12, 13, e).
path(13, 14, e).
path(14, 17, s).


%Map directions to their opposites.  This cuts down on the nubmer of path rules
opp_dir(e, w).
opp_dir(w, e).
opp_dir(n, s).
opp_dir(s, n).
opp_dir(u, d).
opp_dir(d, u).

%strings desctibing movement in particular directions, printed by move rule.
movement_string(u, 'You climb up the small ladder running up the wall and across the ceiling. \
with great effort, you heave yourself through the hatch in the ceiling, emerging \
through the floor of another room.').
movement_string(d, 'You gingerly lower yourself through the hatch in the floor, straining as you \
make your way to the wall and down the rungs of the ladder there.').
movement_string(e, 'You open the hatch to the east, and crawl through.').
movement_string(w, 'You open the hatch to the west, and crawl through.').
movement_string(n, 'You open the hatch to the north, and crawl through.').
movement_string(s, 'You open the hatch to the south, and crawl through.').


%of maps objects in particular rooms to descriptions.  used by the examine rule. 
of(_, hatch, 'It\'s a square hatch, about 3 feet on each side.  There is a handle in the center \
that looks like it can be turned.  A small inscription is below the hatch.').
of(_, panel, 'They are made of a thick material.  Through them, you can make out faint \
outlines of electrical components.').
of(_, rungs, 'They run up the center of each wall, directly to the hatches in the walls and ceiling. \
The rungs are small, but large enough to support a single hand or foot. \
You could climb them.').
of(0, inscription, 'The inscription reads 0 0 0.  It appears to be laser-etched.').
of(1, inscription, 'The inscription reads 1 0 0.  It appears to be laser-etched.').
of(2, inscription, 'The inscription reads 2 0 0.  It appears to be laser-etched.').
of(3, inscription, 'The inscription reads 0 1 0.  It appears to be laser-etched.').
of(4, inscription, 'The inscription reads 1 1 0.  It appears to be laser-etched.').
of(5, inscription, 'The inscription reads 2 1 0.  It appears to be laser-etched.').
of(6, inscription, 'The inscription reads 0 2 0.  It appears to be laser-etched.').
of(7, inscription, 'The inscription reads 1 2 0.  It appears to be laser-etched.').
of(8, inscription, 'The inscription reads 2 2 0.  It appears to be laser-etched.').
of(12, inscription, 'The inscription reads 0 1 1.  It appears to be laser-etched.').
of(13, inscription, 'The inscription reads 1 1 1.  It appears to be laser-etched.').
of(14, inscription, 'The inscription reads 2 1 1.  It appears to be laser-etched.').
of(17, inscription, 'The inscription reads 2 2 1.  It appears to be laser-etched.').
of(26, inscription, 'The inscription reads 2 2 2.  It appears to be laser-etched.').
of(_, rubber_ducky, 'A small, cute, rubber ducky.  It squeaks when squeezed.').
of(_, flashlight, 'Upon closer inspection, it doesn\'t seem to be working.').
of(_, crazed_man, 'He is smelly, and is wearing tattered clothing.  He seems \
to be frothing at the mouth.  May be dangerous.').
of(_, box_of_matches, 'Wooden matches.').
of(_, spam, 'A delicious canned meat product!  The package claims that it is \'Family-sized\'.').
of(_, computer, 'A large desktop computer is attached to the wall in the corner.  It is running.').
of(_, floppy_disk, 'A floppy disk.  It is not labeled.').
of(_, slip_of_paper, 'The note is singed around the edges.  It reads \'NO WAY OUT\'').
of(_, charred_remains, 'You cannot bring yourself to look at them - the stench is bad enough.').
of(_, ripe_banana, 'Yellow and nutricious!').
of(_, suspicious_can, 'It seems to be an old gas can.  A sniff confirms this suspicion.').
of(_, extension_cord, 'About 100 feet of 3-pronged extension cord.').
of(_, old_woman, 'She is muttering to herself, sitting against the far wall.').
of(_, rusty_spoon, 'An old, rusty, tablespoon.').
of(_, book_of_matches, 'A regular book of matches.  The bendy kind.').
of(_, box_of_toothpicks, 'A box of countless toothpicks.  Mint flavored.').
of(_, man, 'A large man, perhaps in his late twenties.  He has just \
begun to pick his nose. \n\n  He appears confused.').
of(26, scaffold_apparatus, 'A large, telescoping scaffold.  It takes up most of this room.  A control \
panel on the side has a short electrical cord emerging from it, not quite long enough to reach the \
outlet across the room.'). 

%which rooms are dark
lights_off(6).
lights_off(7).
lights_off(8).
lights_off(12).
lights_off(13).
lights_off(14).

%player's current location
i_am_at(4).

fed_spam_to_man(0). %have we fed the man yet?
killed_crazed_man(0). %has the crazed man been killed yet?
kill_chase(0). %is the crazed man chasing us?
man_transport(0). %is the magic teleportation device (the man in room 17) on?
match_lit(0). %is the player holding a lit match?

%descriptions for the rooms
room_des(8, 'You are standing in a perfectly cubic room, about 15 feet to a side.  Its walls are \
translucent, white panels. There is a foul odor in the room.  There is a hatch in the center of \
each wall, the ceiling, and the floor.  There is a series of rungs set into the \
middle of  each wall, the ceiling, and the floor.').
room_des(2, 'You are standing in a perfectly cubic room, about 15 feet to a side.  Its walls are \
translucent, red panels.  There is a hatch in the center of each wall, the \
ceiling, and the floor.  There is a series of rungs set into the \
middle of  each wall, the ceiling, and the floor.').
room_des(1, 'You are standing in a perfectly cubic room, about 15 feet to a side.  Its walls are \
translucent, purple panels.  There is a hatch in the center of each wall, the \
ceiling, and the floor.  There is a series of rungs set into the \
middle of  each wall, the ceiling, and the floor.').
room_des(26, 'You are standing in a perfectly cubic room, about 15 feet to a side.  Its three walls are \
aluminum panels.  Beyond the missing West wall is a black abyss stretching up and down as far as you \
can see.  Directly to the West, about a hundred feet away, is a dimly lit door.  In front of the door \
is a small ledge.').
room_des(_, 'You are standing in a perfectly cubic room, about 15 feet to a side.  Its walls are \
translucent, white panels.  There is a hatch in the center of each wall, the \
ceiling, and the floor.  There is a series of rungs set into the \
middle of  each wall, the ceiling, and the floor.').
room_des(_, 'You are standing in a perfectly cubic room, about 15 feet to a side.  Its walls are \
translucent, white panels.  There is a hatch in the center of each wall, the \
ceiling, and the floor.  There is a series of rungs set into the \
middle of  each wall, the ceiling, and the floor.').
%generic description
room_des(_, 'You are standing in a perfectly cubic room, about 15 feet to a side.  Its walls are \
translucent, white panels.  There is a hatch in the center of each wall, the \
ceiling, and the floor.  There is a series of rungs set into the \
middle of  each wall, the ceiling, and the floor.').

%locations of all objects in the game
at(0, rubber_ducky). %there is a rubber ducky in room 0
at(2, crazed_man).
at(3, flashlight). %there is a flashlight in room 4
at(4, book_of_matches).
at(6, spam).
at(7, computer).
at(8, slip_of_paper).
at(8, charred_remains).
at(8, floppy_disk).
at(8, ripe_banana).
at(8, suspicious_can).
at(12, extension_cord).
at(12, old_woman).
at(13, rusty_spoon).
at(13, box_of_matches).
at(14, box_of_toothpicks).
at(17, man).
at(26, scaffold_apparatus).

%item characteristics
total_consumption(spam, man). %using spam on the man consumes the spam
total_consumption(rubber_ducky, old_woman).
total_consumption(floppy_disk, computer).
total_consumption(ripe_banana).
total_consumption(rusty_spoon, crazed_man).
total_consumption(box_of_toothpicks, man).

no_consumption(spam). %using the spam by itself doesn't consume the spam
no_consumption(rubber_ducky).
no_consumption(crazed_man).
no_consumption(computer).
no_consumption(extension_cord).
no_consumption(old_woman).
no_consumption(man).
no_consumption(box_of_toothpicks).

partial_consumption(book_of_matches). %using the book_of_matches consumes some matches
partial_consumption(box_of_matches).

%the following 3 rules ensure that, unless specified, using two objects 
%together consumes neither
partial_consumption(_, _) :- fail.
total_consumption(_, _) :- fail.
no_consumption(_, _).

%how many of the partially_consumable objects are there?
count(book_of_matches, 5).
count(box_of_matches, 7).

%which objects can't be picked up?
cant_pick_up(computer, 'That is too bulky for you to carry.').
cant_pick_up(crazed_man, 'He snarls at you, and shoves you away with \
calloused, grubby hands').
cant_pick_up(charred_remains, 'You cannot bring yourself to do that.').
cant_pick_up(old_woman, 'She mumbles as you try to pick her up, but you find that\
she is too heavy').
cant_pick_up(man, 'He quite large and muscular, probably too large for you to carry. \
You try to pick him up anyway.  He whimpers and cries, so you put him down.').

%what can be used with what?
usable(rubber_ducky). %rubber_ducky can be used by itself
usable(spam, man). %spam can be used on man
usable(rubber_ducky, old_woman).
usable(floppy_disk, computer).
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
usable(book_of_matches) :-
	count(book_of_matches, A),
	A > 0, !.
usable(box_of_matches) :-
	count(box_of_matches, A),
	A > 0, !.
usable(box_of_toothpicks, man) :-
	fed_spam_to_man(1).


%what happens when we use these things with each other?
use_item(ripe_banana, Des) :-
	Des = 'You eat the banana.  Yum!'.
use_item(rusty_spoon, crazed_man, Des) :-
	retract(kill_chase(1)),
	assert(kill_chase(0)),
	at(A, crazed_man),
	retract(at(A, crazed_man)), 
	Des = 'You fight off the crazed man using only your rusty spoon. \n\n\
The details are best left unspecified.'.
use_item(spam, Des) :-
	Des = 'You eat a little of the Spam.  It tastes about as you would expect it to.'.
use_item(rubber_ducky, Des) :-
	Des = 'QUACK!'.
use_item(crazed_man, Des) :-
	retract(kill_chase(0)),
	assert(kill_chase(1)),
	Des = 'The man lunges at you. He is attacking!'.
use_item(computer, Des) :-
	Des = 'Awaiting input media...'.
use_item(extension_cord, scaffold_apparatus, Des) :-
	Des = 'You plug in the apparatus, which begins deploying a walkway to the outer wall.  \
When it has reached the far side, you and Dillon eagerly make your way across the chasm \
and to the exterior door beyond which, hopefully, freedom awaits...\n\n\n YOU WIN! \
You may continue to explore the map if you wish, but the game is over. Congratulations.'.
use_item(old_woman, Des) :-
	Des = 'She begins ranting: \'Nowayoutnowayout NO WAY OUT. Why? WHERE?  ROOMSroomsrooms so many rooms...\''.
use_item(man, Des) :-
	(man_transport(1)
	-> 
		(i_am_at(17)
		->retract(i_am_at(17)),
		retract(at(17, man)),
		assert(i_am_at(26)),
		assert(at(26, man)),
		write('The man exclaims, \'Wheeeeeeee!\''), nl, Des = '',
		look,
		!
		; retract(i_am_at(26)),
		retract(at(26, man)),
		assert(i_am_at(17)),
		assert(at(17, man)),
		write('The man exclaims, \'Wheeeeeeee!\''), nl, Des = '',
		look,
		!)
	; Des = 'He looks up at you, index finger knuckle deep in his left nostril, and speaks: \
\'Hi! My name\'s Dillon. Nice ta meet ya I guess.  \n\n I have a special place I like to \
go to when I feel happy.  But I don\'t feel happy now...\''
	).
use_item(box_of_toothpicks, Des) :-
	Des = 'You take one of the toothpicks and put it in your mouth.  You work it between your teeth \
as you walk.'.
use_item(spam, man, Des) :-
	retract(fed_spam_to_man(0)),
	assert(fed_spam_to_man(1)),
	Des = 'With an ear-to-ear grin, he tears open the can and slides the entire \
brick of spam into his mouth.  Happily, he chews it for a minute or so. \n\n\
After swallowing, he begins picking at his teeth.  He seems to be growing more \
frustrated by the second.'.
use_item(box_of_toothpicks, man, Des) :-
	fed_spam_to_man(1),
	retract(man_transport(0)),
	assert(man_transport(1)),
	Des = 'The man says to you \'I am happy now.  Use me to get to the different place\''.
use_item(rubber_ducky, old_woman, Des) :-
	Des = 'She snatches the ducky from you and stuffs it into her pocket. \
Her look makes it clear that you will not be getting it back.'.
use_item(floppy_disk, computer, Des) :-
	assert(path(3, 12, u)),
	assert(path(4, 13, u)),
	assert(path(5, 14, u)),	
	Des = 'The computer beeps, and you hear the distant noise of metal panels sliding.'.
use_item(box_of_matches, Des) :-
	retract(match_lit(0)),
	assert(match_lit(1)),
	Des = 'You light the match, and the room comes into view.',
	look.
use_item(book_of_matches, Des) :-
	retract(match_lit(0)),
	assert(match_lit(1)),
	Des = 'You light the match, and the room comes into view.',
	look.


/*
Verbs
*/

%is item A reachable from the current location?
reachable(A) :-
	at(inventory, A).
reachable(A) :-
	i_am_at(X),
	at(X, A).
reachable(crazed_man).

%do whatever is appropriate when using A, whether it is consumed or not
consume(A) :-
	(total_consumption(A)
	->reachable(A),
	at(X, A),
	retract(at(X, A))
	;(partial_consumption(A)
	->reachable(A),
	count(A, N),
	N > 0,
	M is N-1,
	retract(count(A, N)),
	assert(count(A, M)),
	write(A), write(': '), write(M), write(' remaining.'), nl
	;no_consumption(A)
	)
	),!.

%do whatever is appropriate when using A on B, whether it is consumed or not
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

	
	
%use A on B, if possible
use(A, B) :-
	usable(A, B),
	reachable(A),
	reachable(B),
	use_item(A, B, Des),
	consume(A, B),
	consume(B, A),
	write(Des), nl,
	!.
use(A, B) :- write('You cannot use '), write(A), write(' with '), write(B), nl, !.
use(A) :-
	usable(A),
	reachable(A),
	use_item(A, Des),
	consume(A),
	write(Des), nl,
	!.
use(A) :- write('You cannot use '), write(A), nl. 
	

%Path - is there a path between the two rooms in the given direction?
%Each room is a 3-element list representing the room's x, y, z coordinates.
%A is source, B is dest, dir is one of N, S, E, W, U, D
is_path(A, B, Dir) :-
	path(A, B, Dir).
is_path(A, B, Dir) :-
	opp_dir(Dir, Opp_Dir),
	path(B, A, Opp_Dir).
is_path(_, _, _) :- fail.

%Move - if the movement is valid, move the player.
move(Dir) :-
	
	i_am_at(X),
	is_path(X, Y, Dir),
	retract(i_am_at(X)),
	assert(i_am_at(Y)),
	movement_string(Dir, Str),
	write(Str), nl,
	(kill_chase(1)
	->
		at(C, crazed_man),
		( Y == C
		%we're where the crazy man is 
		->write('\n\n\nYou were caught by the crazed man, who proceeds to kill you. \nGAME OVER. PLEASE RESTART.\n\n\n\
Note: you may still explore the map if you wish, but THE GAME IS OVER.')
		;at(A, crazed_man),
		 retract(at(A, crazed_man)),
		 assert(at(X, crazed_man)),
		 write('\nThe crazed man is chasing you! He is only a room behind!\n'), nl
		)
	;!),
	(match_lit(1)
	-> retract(match_lit(1)),
	assert(match_lit(0)),
	write('Your match burns out as you move between rooms.'), nl
	;!),
	look,
	!.


	
%Move - otherwise, tell the player they can't move.
move(_) :-
	write('Try as you might to turn the handle, it won\'t budge. You can\'t \
go this way.'), nl.
	
%Pick up object - if already holding the object, can't pick it up!
take(X) :-
	holding(X),
	write('You are already holding it!'), nl,
	!.


%Pick up object - if in the right location, but not movable, print reason
take(X) :-
	i_am_at(Place),
	at(Place, X),
	cant_pick_up(X, Des),
	write(Des), nl,
	!.

%Pick up object - if in the right location and movable, pick it up and remove it from the ground.
take(X) :-
	i_am_at(Place),
	at(Place, X),
	\+ cant_pick_up(X, Des1),
	retract(at(Place, X)),
	assert(at(inventory, X)),
	write('You have picked up the '), write(X), nl,
	!.
	
%Pick up object - otherwise, cannot pick up the object.
take(_) :-
	write('I do not see that here.'), nl.

examine(X) :-
	i_am_at(Loc),
	of(Loc, X, Des),
	write(Des), nl, !.

examine(X) :-
	i_am_at(Loc),
	of(Loc, X, Des),
	write(Des), nl, !.
	


room_is_dark(X) :-
	lights_off(X),
	match_lit(0).

%Look - describe where in space the player is, list objects at the location
look :-
	i_am_at(X),
	(room_is_dark(X)
	-> write('The room is pitch black - you cannot see!'), nl
	;room_des(X, Des),
	write(Des), nl,
	list_objects_at(X),	
	list_paths_out(X)
	), !.

i :-
	inventory.

inventory :-
	at(inventory, Obj),
	write('You are carrying a(n) '), write(Obj), nl,
	fail.

%List objects - these two rules effectively form a loop that go through every object
%				in the location and writes them out.
list_objects_at(X) :-
	at(X, Obj),
	write('There is a '), write(Obj), write(' in the room.'), nl,
	fail.

list_objects_at(_).

%List objects - these two rules effectively form a loop that go through every object
%				in the location and writes them out.
list_paths_out(X) :-
	is_path(X, A, Dir),
	write('From here, you can move '), write(Dir), nl,
	fail.

list_paths_out(_).

