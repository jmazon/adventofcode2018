// Correct answer part1: 3909249

#include <stdio.h>
#include <stdint.h>
int func(uint64_t r0)
{
uint64_t r1=0,r2=0,r3=0,r4=0,r5=0;
puts("in func()");
l0: r3 = 123ul;
l1: r3 &= 456ul;
l2: r3 = (r3 == 72);
l3: if (r3) goto l5;
l4: goto l1;
l5: r3 = 0ul;

l6: r2 = (r3 & 65536ul);
l7: r3 = 1397714ul;

l8: r5 = (r2 & 255ul);
l9: r3 += r5;
l10: r3 &= 16777215ul;
l11: r3 *= 65899ul;
l12: r3 &= 16777215ul;
l13: r5 = (256ul > r2);
l14: if (r5) goto l16;
l15: goto l17;
l16: goto l28;
l17: r5 = 0ul;
l18: r1 = r5 + 1ul;
l19: r1 *= 256ul;
l20: r1 = (r1 > r2);
l21: if (r1) goto l23;
l22: goto l24;
l23: goto l26;
l24: r5++;
l25: goto l18;
l26: r2 = r5;
l27: goto l8;
l28: r5 = (r3 == r0);
  static int done = 0; if (!done) { printf("First: %d\n", r3); ++done; }
l29: if (r5) goto l31;
l30: goto l6;
l31: ;
}

int main()
{
  printf("%d\n", func(0 /* 3909249*/ ));
  return 0;
}



