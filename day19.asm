if (part2) r4 = 10551361;
 else r4 = 961;
r0 = 0;
ip1:   for (r1 = 1; r1 <= r4; r1++)
 ip2:   for (r2 = 1; r2 <= r4; r2++) 
    if (r1*r2 == r4) r0 += r1;
