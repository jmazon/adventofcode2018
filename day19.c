#include <stdio.h>
static int solve() {
int r0=1, r1=0, r2=0, r3=0, r4=0, r5=0, ip=0;
for (;;) {
switch (ip) {
case 0: r3 = ip; r3 = (r3+16); if (r3 != ip) { ip = r3; break; } else ip++;
case 1: r3 = ip; r1 = 1; if (r3 != ip) { ip = r3; break; } else ip++;
case 2: r3 = ip; r2 = 1; if (r3 != ip) { ip = r3; break; } else ip++;
case 3: r3 = ip; r5 = (r1*r2); if (r3 != ip) { ip = r3; break; } else ip++;
case 4: r3 = ip; r5 = (r5==r4); if (r3 != ip) { ip = r3; break; } else ip++;
case 5: r3 = ip; r3 = (r5+r3); if (r3 != ip) { ip = r3; break; } else ip++;
case 6: r3 = ip; r3 = (r3+1); if (r3 != ip) { ip = r3; break; } else ip++;
case 7: r3 = ip; r0 = (r1+r0); if (r3 != ip) { ip = r3; break; } else ip++;
case 8: r3 = ip; r2 = (r2+1); if (r3 != ip) { ip = r3; break; } else ip++;
case 9: r3 = ip; r5 = (r2>r4); if (r3 != ip) { ip = r3; break; } else ip++;
case 10: r3 = ip; r3 = (r3+r5); if (r3 != ip) { ip = r3; break; } else ip++;
case 11: r3 = ip; r3 = 2; if (r3 != ip) { ip = r3; break; } else ip++;
case 12: r3 = ip; r1 = (r1+1); if (r3 != ip) { ip = r3; break; } else ip++;
case 13: r3 = ip; r5 = (r1>r4); if (r3 != ip) { ip = r3; break; } else ip++;
case 14: r3 = ip; r3 = (r5+r3); if (r3 != ip) { ip = r3; break; } else ip++;
case 15: r3 = ip; r3 = 1; if (r3 != ip) { ip = r3; break; } else ip++;
case 16: r3 = ip; r3 = (r3*r3); if (r3 != ip) { ip = r3; break; } else ip++;
case 17: r3 = ip; r4 = (r4+2); if (r3 != ip) { ip = r3; break; } else ip++;
case 18: r3 = ip; r4 = (r4*r4); if (r3 != ip) { ip = r3; break; } else ip++;
case 19: r3 = ip; r4 = (r3*r4); if (r3 != ip) { ip = r3; break; } else ip++;
case 20: r3 = ip; r4 = (r4*11); if (r3 != ip) { ip = r3; break; } else ip++;
case 21: r3 = ip; r5 = (r5+5); if (r3 != ip) { ip = r3; break; } else ip++;
case 22: r3 = ip; r5 = (r5*r3); if (r3 != ip) { ip = r3; break; } else ip++;
case 23: r3 = ip; r5 = (r5+15); if (r3 != ip) { ip = r3; break; } else ip++;
case 24: r3 = ip; r4 = (r4+r5); if (r3 != ip) { ip = r3; break; } else ip++;
case 25: r3 = ip; r3 = (r3+r0); if (r3 != ip) { ip = r3; break; } else ip++;
case 26: r3 = ip; r3 = 0; if (r3 != ip) { ip = r3; break; } else ip++;
case 27: r3 = ip; r5 = r3; if (r3 != ip) { ip = r3; break; } else ip++;
case 28: r3 = ip; r5 = (r5*r3); if (r3 != ip) { ip = r3; break; } else ip++;
case 29: r3 = ip; r5 = (r3+r5); if (r3 != ip) { ip = r3; break; } else ip++;
case 30: r3 = ip; r5 = (r3*r5); if (r3 != ip) { ip = r3; break; } else ip++;
case 31: r3 = ip; r5 = (r5*14); if (r3 != ip) { ip = r3; break; } else ip++;
case 32: r3 = ip; r5 = (r5*r3); if (r3 != ip) { ip = r3; break; } else ip++;
case 33: r3 = ip; r4 = (r4+r5); if (r3 != ip) { ip = r3; break; } else ip++;
case 34: r3 = ip; r0 = 0; if (r3 != ip) { ip = r3; break; } else ip++;
case 35: r3 = ip; r3 = 0; if (r3 != ip) { ip = r3; break; } else ip++;
default: return r0;
}
ip++;
}
}
int main() {
printf("%d\n", solve());
return 0;
}
