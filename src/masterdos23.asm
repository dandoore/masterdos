;          ANDREW J.A. WRIGHT
;
;          COPYRIGHT 1990
;
;          MASTER DOS

               DUMP 1,&0009        ;SAVE "MD23"CODE 32777,15750
               ORG  &4009

Include_files: INCLUDE "a1.asm"
               INCLUDE "b1.asm"
               INCLUDE "c11.asm"
               INCLUDE "c12.asm"
               INCLUDE "d1.asm"
               INCLUDE "e1.asm"
               INCLUDE "f11.asm"
               INCLUDE "f12.asm"
               INCLUDE "g1.asm"
               INCLUDE "move.asm"
               INCLUDE "ser2.asm"
               INCLUDE "time.asm"
               INCLUDE "subd.asm"
               INCLUDE "ramd.asm"
               INCLUDE "hooks.asm"