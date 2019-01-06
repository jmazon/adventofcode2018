Chronal Conversion

I remember doing this one on a plane, during two separate trips, so I
couldn't really check my results until later.

Control flow analysis of the input code showed that the only way to
exit was to have r3==r0 in the eqrr closest to the end.  Since r0
isn't modified in any way by the code, the easiest way to find out how
to exit fast is to simply run it, and trace the value of r3 at that
point, and that solves part one.

The rest of the analysis showed that if r3!=r0, the program looped
(minus the bitwise operations' test code), so the longest way out is
to find the latest value of r3 before a cycle.  Any r0 out of the
orbit would loop forever.  So I just kept tracing the values of r3 at
the comparison point, and piped it all to an awk oneliner to catch the
valid value.
