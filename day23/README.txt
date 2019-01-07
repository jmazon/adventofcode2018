Experimental Emergency Teleportation

Greatest difference of difficulty between both parts ever!  So ok,
part one was trivial.

Part two left me thinking for a very long time.  We've got a thousand
units, the maximum number of intersections between them is
exponential, this isn't starting well.

It was “obvious” to me from the start to use the four ±x±y±z planes as
delimiters.  I considered, and started implementing, an actual
hexadecitree (like an octree in 4D) space partitioning.  But writing
down generic tetrahedron difference is very tedious, error-prone, and
didn't seem like it would evade the combinatorial explosion of number
of intersections that much.

So I put it aside for a while and considered the likeliness of
reducing to max clique.  (from a strict intersection transitivity
point of view, it shouldn't work, but intuitively those nanobots do
share a lot, so even if it didn't settle down to an exact solution, it
could help approach a solution and prove a valuable exploring ground.)
I implemented a half-assed Bron-Kerbosch and let it run for a while.
It found a clique very fast, but no improvement forever, while the
memory use kept increasing.  Meanwhile I was having doubts on the
feasibility, given the algorithm's theoretical complexity that's still
exponential.

Eventually I had the revelation that my problem with intersection
combinatorial explosion had a converse: most nanobot radiuses
represent a sizeable fraction of the complete problem space.  A quick
check confirmed.  Since an axis-aligned tetrahedron can't be missed by
a 3D scan quantized to its radius, I didn't really have to scan the
whole volume, only 1/min_radius³ of it.  That was in the order of two
thousand points, so an easy check, especially since I already had all
of my tetrahedron intersection code set up.

It probably would have been enough to pass to just compute that
volume, and just submit each plane's distance to origin until one
clicked, but I actually motivated myself enough to write proper vertex
extraction and filtering, and submit that with a good 99% reliability.
(figures made up.)  The missing percent being: hey, what if more than
one scan point had equal value of coverage, and they had to be
distinguished by their zones?  Tractable, but I passed without it so I
never implemented it.

This was by far the latest puzzle I solved mentally (I still had three
leftover mentally solved, not-yet-implemented puzzles).
