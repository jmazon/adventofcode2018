A Regular Map

The only hard part was generating the maze without falling to the
combinatorial explosion.  To do that, the only change needed to my
naive approach was to cut when reaching a maze position again at the
same point in the regex.  This happens for instance when multiple
regex alternatives open doors along differnt paths, but still end up
in the same place.

The overoptimization in my code is due to *ahem* other bugs.
