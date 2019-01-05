Subterranean Sustainability

Part one is a straightforward implementation.

For part two, some analysis was necessary, as the usual loop-detection
strategy common to this sort of problem didn't work.  The number and
“width” of active cells stabilized eventually, but it kept marching
forward, Conway-glider style.  So I adjusted my loop detection to
first normalize position, and we're down to simple arithmetic.

In the end, I think the period is one, even if my code probably (hey,
untested!) handles more.

Needless to say, I loved that puzzle.
