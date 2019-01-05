Mine Cart Madness

Part one was a “straightforward” implementation.  I originally used
Complexes, but then Haskell refused to key a map on them because
they're not Ord; so I considered implementing Ord, thought to myself
“what am I doing” and converted to a Int pair with rotations spelled
out.

Part two I postponed forever, until it was the final star I ever
missed.  My initial code had an unfortunate i̶m̶p̶e̶d̶a̶n̶c̶e̶ m̶i̶s̶m̶a̶t̶c̶h̶ ok
let's call it a bug where at each step I took all carts, then advanced
them, then turned them, then checked for collisions.  But then, when a
collision happens, either the active cart collides with one that has
already moved and all is fine and dandy, or it's colliding with one
that hasn't moved yet, and it's getting tricky to make that position
forbidden when another (or even two other) carts are still
legitimately allowed to stp in that same place.  With only a position
or a complete {position,velocity,switch state} tuple, sorting those
out would be a headache.  For the longest time I considered adding an
id to each cart, but that was messy and I just balked.

In the end I bit the bullet and re-implemented the step using the
specified woefully imperative process, and all was well without
needing to distinguish the carts one from another.  Whew.

So mixed feelings about this one.  Great subject, but the carts'
sequencing, while well-specified, doesn't make sense.
