Reservoir Research

I implemented this on with an elaborate form of floodfill (hah).  The
seed is a dripping operation, that propagates down.  If it hits solid
ground, it turns into a flooding operation, that propagates sideways
over ground.  If flooding isn't contained, it spills, and can turn to
dripping again.  If flooding is contained it settles and floods
upwards.

Proper cutting avoids the combinatorial explosion of spilling on two
sides of a container.  Using the symbols in the puzzle statement made
part two trivial.
