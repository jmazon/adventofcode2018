#ip 4
l0:     r3 = 123
l1:     r3 &= bani 456
l2:     r3 = r3 == 72
l3:     jeq l5
l4:     jmp l1
l5:     r3 = 0
l6:     r2 = r3 | 65536
l7:     r3 = 1397714
l8:     r5 = r2 & 255
l9:     r3 += r5
l10:    r3 &= 16777215
l11:    r3 *= 65899
l12:    r3 &= 16777215
l13:    r5 = 256 > r2
l14:    jgt l16
l15:    jmp l17
l16:    jmp l28
l17:    r5 = 0
l18:    r1 = r5 + 1
l19:    r1 *= 256
l20:    r1 = 1 > 2
l21:    jgt l23
l22:    jmp l24
l23:    jmp l26
l24:    r5++
l25:    jmp l18
l26:    r2 = r5
l27:    jmp l8
l28:    r5 = r3 == 0
l29:    jeq l31
l30:    jmp l6
        
