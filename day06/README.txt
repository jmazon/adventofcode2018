Chronal Coordinates

This is where I've started cutting the most corners.  Also the one
where my part two implementation is way more simple than part one's,
so I'm suspicious of what I've done.

My part one implementation is an adjusted multi-source BFS that keeps
track of ties reaching vertices.  I don't detect pseudo-termination
properly either: during Advent I simply tracked the number of radiants
still active, and stopped when it seemed stable.  Cleaner might be to
either determine in advance which radiants will end up infinite
(through convex hull extraction), or to compute an upper bound on the
max distance where radiants can be absorbed (such as initial
width+height).  Both seem wasteful, but I'll look into it eventually.

Intuitively, the “correct” large input approach would be a Manhattan
Voronoi sweeping, but that's a lot to swallow for a day 6 puzzle.

My part two implementation just brute-forcedly sums distances and
counts.  It would be a prime candidate for a cleaner double sweep-line
approach, but the input size simply didn't warrant that.
