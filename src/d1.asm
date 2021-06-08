
;D1

;CHECK IS IT END OF LINE

CIEL   CALL GCHR
       CP   0DH    ;CR
       RET  Z
       CP   3AH    ;:
       RET


;CHECK FOR SYNTAX ONLY

CFSO   LD   (SVA),A
       CALL NRRD
       DEFW FLAGS
       AND  80H
       LD   A,(SVA)
       RET


;CHECK FOR END OF SYNTAX

CEOS   CALL CIEL
       JR   NZ,REP0HC

ABORT  CALL CFSO
       RET  NZ

;END OF STATEMENT

ENDS   LD   E,0              ;NO ACTION
       DB 21H                ;"JR+2"

ENDSX  LD   E,1

END1   XOR  A
       CALL NRWR
       DEFW XPTR+1           ;NO ERROR

       CALL BCR
       LD (NRFLG),A          ;RECURSE OK
       LD   SP,(ENTSP)

;FROM END OF HOOKS

RENT   POP HL
       LD (ENTSP),HL         ;OLD VALUE
       RET


;TEST FOR BREAK ROUTINE

BRKTST LD   A,0F7H
       IN   A,(249)
       AND  20H
       RET  NZ

REP3   CALL DERR
       DEFB 84

;INSIST SEPARATOR C

ISEPX  CALL GTNC

ISEP   CP C

REP0HC JP NZ,REP0            ;ERROR OR SKIP CHAR

;GET THE NEXT CHAR.

GTNC   CALL CMR
       DEFW 0020H
       RET


;GET THE CHAR UNDER POINTER

GCHR   CALL CMR
       DEFW 0018H
       RET

;CY IF A LETTER OR DIGIT

ALPHANUM   CALL ALPHA
           RET C

;CY IF A DIGIT

NUMERIC:   CP "9"+1          ;NC IF TOO HIGH
           RET NC

           CP "0"
           CCF
           RET


;READ A DOUBLE ROM WORD IN BC

NRRDD  EX   (SP),HL
       PUSH DE
       CALL GTHL
       PUSH DE
       CALL RDBC
       JR PPXR

;READ A ROM SYSTEM VARIABLE (PARAM WORD) TO A REG

NRRD   EX   (SP),HL
       PUSH DE
       CALL GTHL
       PUSH DE
       CALL RDA
       JR PPXR

;WRITE A DOUBLE WORD FROM A ROM SYS VAR (PARAM WORD) TO BC

NRWRD  EX   (SP),HL
       PUSH DE
       CALL GTHL
       PUSH DE
       CALL WRTBC
       JR PPXR

;WRITE ROM SYSTEM VARIABLE

NRWR   EX   (SP),HL
       PUSH DE
       CALL GTHL
       PUSH DE

;WRITE A TO (HL) IN SYS PAGE

           LD E,A
           IN A,(251)
           LD D,A
           XOR A
           OUT (251),A
           SET 7,H
           RES 6,H
           LD (HL),E
           LD A,D
           OUT (251),A
           LD A,E

PPXR   POP  HL
       POP  DE
       EX   (SP),HL
       RET

;PLACE "NEXTST" ADDR - CALLED BY "OPEN"
;IN CASE NEXTSTAT NOT ON STACK, PLACE IT. TRANSFORMS:
;NEXT STAT/ERRSP TO FOWIA/NEXTSTAT/ERRSP
;   *                *             (*=SP)
;ERRSP TO FOWIA/NEXTSTAT/ERRSP
;  *               *             (*=SP)

PLNS   LD HL,(ENTSP)
       INC HL
       INC HL
       INC HL
       INC HL                ;MAIN ROM STACK PTR ON DOS STACK
       DEC (HL)
       DEC (HL)              ;DEC STORED SP LSB
                             ;(ADD ONE ADDR TO ENSURE INTS DO NOT HIT)
       CALL NRRDD
       DW ERRSP              ;READ ERRSP INTO BC
       LD H,B
       LD L,C
       LD BC,(NEXTST)
       CALL DWRBC            ;DEC HL TO PT TO ADDR BELOW ERRSP, WRITE BC
       DEC HL
       LD BC,FOWIA           ;NULL

DWRBC  DEC HL
       DEC HL

;WRITE BC TO (HL) IN SYS PAGE

WRTBC      IN A,(251)
           EX AF,AF'
           XOR A
           OUT (251),A
           SET 7,H
           RES 6,H
           LD (HL),C
           INC HL
           LD (HL),B
           JR BCRWC

;READ BC FROM HL IN SYS PAGE

RDBC       IN A,(251)
           EX AF,AF'
           XOR A
           OUT (251),A
           SET 7,H
           RES 6,H
           LD C,(HL)
           INC HL
           LD B,(HL)
           JR BCRWC


;REPLACE CALL CMR:DW NRREAD - FASTER

RDA        IN A,(251)
           EX AF,AF'
           XOR A
           OUT (251),A
           SET 7,H
           RES 6,H
           LD A,(HL)

BCRWC      EX AF,AF'
           OUT (251),A
           EX AF,AF'
           RET


GTHL   LD   E,(HL)
       INC  HL
       LD   D,(HL)
       INC  HL
       EX   DE,HL
       RET


;GET THE HALF FLAG3

HLFG   EX   (SP),HL
       PUSH HL               ;ORIG HL ON STACK, THEN RET ADDR
       LD HL,PHLR
       EX (SP),HL
       PUSH HL               ;ORIG HL, PHLR, RET ADDR
       LD   HL,FLAG3
       RET

PHLR   POP HL
       RET

;SET FLAG3 SUBROUTINE

SETF0  CALL HLFG
       SET  0,(HL)
       RET

SETF1  CALL HLFG
       SET  1,(HL)
       RET

SETF2  CALL HLFG
       SET  2,(HL)
       RET

SETF3  CALL HLFG
       SET  3,(HL)
       RET

SETF4  CALL HLFG
       SET  4,(HL)
       RET

SETF5  CALL HLFG
       SET  5,(HL)
       RET

SETF6  CALL HLFG
       SET  6,(HL)
       RET

SETF7  CALL HLFG
       SET  7,(HL)
       RET



;BIT TEST OF FLAG3 ROUTS.

BITF0  CALL HLFG
       BIT  0,(HL)
       RET

BITF1  CALL HLFG
       BIT  1,(HL)
       RET

BITF2  CALL HLFG
       BIT  2,(HL)
       RET

BITF3  CALL HLFG
       BIT  3,(HL)
       RET

BITF4  CALL HLFG
       BIT  4,(HL)
       RET

BITF5  CALL HLFG
       BIT  5,(HL)
       RET

BITF6  CALL HLFG
       BIT  6,(HL)
       RET

BITF7  CALL HLFG
       BIT  7,(HL)
       RET

;BORDER COLOUR CHANGE

BCC    LD   A,(RBCC)
       AND  0FH
       RET  Z

       AND  7
       AND  E
       OUT  (ULA),A
       RET

;BORDER COLOUR RESTORE

BCR    PUSH AF
       CALL NRRD
       DEFW 5C4BH
       OUT  (ULA),A
       POP  AF
       RET


;ERROR REPORT MESSAGES

REP4   LD A,85
       DEFB 21H

REP5   LD A,86
       DEFB 21H

REP6   LD A,87
       DEFB 21H

REP12  LD A,93
       DEFB 21H

REP13  LD A,94
       DEFB 21H

REP18  LD A,99
       DEFB 21H

REP19  LD A,100
       DEFB 21H

REP20  LD A,101
       DEFB 21H

REP22  LD A,103
       DEFB 21H

REP23  LD A,104
       DEFB 21H

REP24  LD A,105
       DEFB 21H

REP25  LD A,106
       DEFB 21H

REP27  LD A,22
       DEFB 21H              ;EOF

REP28  LD A,109
       DEFB 21H

REP30  LD A,111
       DEFB 21H

REP31  LD A,112
       DEFB 21H

REP32  LD A,113
       DEFB 21H

REP33  LD A,114
       DEFB 21H

REP35  LD A,115
       DEFB 21H

REP36  LD A,116

       LD (DERV),A
       CALL DERR
DERV   DB 0


;CONVERT NUMBER IN A TO DIGITS IN ABC

CONR   PUSH DE
       LD   H,0
       LD   L,A
       LD   DE,100
       LD C," "
       CALL CONR1
       PUSH AF               ;HUNDREDS DIGIT
       LD   DE,10
       CALL CONR1
       PUSH AF               ;TENS DIGIT
       LD   A,L
       ADD  A,30H
       LD   B,A
       POP  AF
       LD   C,A
       POP  AF
       POP  DE
       RET


CONR1  XOR  A
CONR2  SBC  HL,DE
       JR   C,CONR3

       INC  A
       JR   CONR2

CONR3  ADD  HL,DE
       AND  A
       JR   NZ,CONR4

       LD   A,C              ;SPACE OR ZERO
       RET

CONR4  LD C,"0"              ;**
       ADD  A,C
       RET



;SAMDOS ERROR PRINT
;DE HOLDS TRACK AND SECTOR

DERR   CALL BCR

       LD   HL,(HKSP)
       LD   A,H
       OR   L
       JR   Z,DERR1

       LD   SP,HL
       RET

DERR1  LD   A,D
       CALL CONR
       LD   (PRTRK),A
       LD   (FMTRK),A
       LD   (PRTRK+1),BC
       LD   (FMTRK+1),BC
       LD   A,E
       CALL CONR
       LD   (PRSEC),BC
       CALL NRRDD
       DEFW CHADD

       CALL NRWRD
       DEFW XPTR

       CALL NRRD
       DEFW CHADP

       CALL NRWR
       DEFW XPTRP

       XOR  A
       LD   (FLAG3),A
       LD (NRFLG),A          ;RECURSE OK
       LD   E,A
       POP  HL
       LD   A,(HL)           ;ERR NO.
       LD SP,7FFAH           ;OK WHETHER CMD OR HOOK
       RET

INVALID    EQU 0
ERROR      EQU 7
TREAM      EQU 8
NO         EQU 11
SNOTS      EQU 17
SNAME      EQU 18
TOOMANY    EQU 20
TATEMENT   EQU 21
FILE       EQU 23

ERRTBL DEFB " "+80h          ;0

       DEFB " "+80h          ;1

       DEFB " "+80h          ;2

       DEFM "Escape requeste" ;3
       DEFB "d"+80h

       DEFM "TRK-"           ;4
PRTRK  DEFB 20h
       DEFB 20h
       DEFB 20h
       DEFM ",SCT-"
PRSEC  DEFB 20h
       DEFB 20h
       DEFM ",Erro"
       DEFB "r"+80h

       DEFM "Format TRK-"    ;5
FMTRK  DEFB 20h
       DEFB 20h
       DEFB 20h
       DEFM " los"
       DEFB "t"+80h

       DEFM "Check disk in " ;6
       DEFM "driv"
       DEFB "e"+80h

       DEFB " "+80h          ;7

       DEFB " "+80h          ;8

       DEFB " "+80h          ;9

       DEFB INVALID
       DEFM "devic"
       DEFB "e"+80h		  ;10

       DEFB " "+80h          ;11

       DEFM "Verify faile"   ;12
       DEFB "d"+80h

       DEFB "Wrong ",FILE," typ" ;13
       DEFB "e"+80h

       DEFB " "+80h          ;14

       DEFB " "+80h          ;15

       DEFB " "+80h          ;16

       DEFB " "+80h          ;17

       DEFM "Reading "       ;18
       DEFM "a write "
       DEFB FILE+80H

       DEFM "Writing "       ;19
       DEFM "a read "
       DEFB FILE+80H

       DEFB NO
       DEFM "AUTO* "
       DEFB FILE+80H ;20

       DEFB " "+80h          ;21

       DEFB NO
       DEFM "such driv"
       DEFB "e"+80H ;22

       DEFM "Disk is write "  ;23
       DEFM "protecte"
       DEFB "d"+80h

       DEFM "Disk ful"
       DEFB "l"+80H     ;24

       DEFM "Directory ful"  ;25
       DEFB "l"+80h

       DEFM "File"
       DEFB SNOTS
       DEFM "foun"
       DEFB "d"+80H ;26

       DEFB " "+80h          ;27

       DEFM "File"
       DEFB SNAME
       DEFM " use"
       DEFB "d"+80H ;28

       DEFB " "+80h          ;29

       DEFB "S",TREAM
       DEFM " use"
       DEFB "d"+80H ;30

       DEFM "Channel use"    ;31
       DEFB "d"+80h

       DEFB "Directory"
       DEFB SNOTS
       DEFM "foun"
       DEFB "d"+80H ;32

       DEFM "Directory"
       DEFB SNOTS
       DEFM "empt"
       DEFB "y"+80H ;33

       DEFB NO
       DEFM "pages fre"
       DEFB "e"+80H      ;34

       DEFM "PROTECTED "
       DEFB FILE+80H    ;35

;NON MASK INT ROUT.

NMI    LD   (STR),SP
       LD   SP,STR
       LD   A,I
       PUSH AF
       PUSH HL
       PUSH BC
       PUSH DE
       EX   AF,AF'
       EXX
       PUSH AF
       PUSH HL
       PUSH BC
       PUSH DE
       PUSH IX
       PUSH IY


;PUSH RETURN ADDRESS ON STACK

       LD   HL,SNAP7
       PUSH HL
       LD   (HKSP),SP

;TEST FOR SNAPSHOT TYPE

SNAP29 LD   A,4
       OUT  (251),A          ;ZX RAM AT 8000H
       IM   1

SNAP3  LD   BC,0F7FEH
       IN   E,(C)            ;BITS 0-4=DIGITS 1-5
       BIT 0,E
       JR Z,SNAP31           ;JR IF "1"

       BIT  4,E
       JR NZ,SNAP32          ;JR IF NOT "5"

SNAP31 LD HL,(NMIKA)
       LD A,(NMIKP)          ;NORMALLY 4
       OUT (251),A
       CALL HLJUMP           ;JUST RET USUALLY (0004H)
       JR SNAP29

SNAP32 BIT  1,E              ;RET IF "2"
       RET  Z

       BIT  2,E
       JR   NZ,SNAP3A        ;JR IF NOT "3"

       LD   A,14H            ;SCREEN$ TYPE
       LD   D,1BH            ;SCREEN LENGTH MSB
       JR   SNAP4

SNAP3A BIT  3,E
       JR   NZ,SNAP3B        ;JR IF NOT "4"

       LD   A,5              ;48K SNAPSHOT TYPE
       LD   D,0C0H           ;LEN MSB
       JR   SNAP4

SNAP3B INC  A
       AND  7
       OUT  (C),A            ;BORDER
       LD   B,0FEH
       IN   E,(C)
       BIT  2,E
       JR   NZ,SNAP3         ;LOOP IF NOT "X"

SNAP3C IN   E,(C)
       BIT  2,E
       JR   Z,SNAP3C         ;LOOP TILL NOT "X"

       CALL 005FH            ;DELAY BC

       LD   A,(SNPRT0)
       OUT  (251),A
       LD   A,(SNPRT2)
       OUT  (252),A
       EI
       JP   ENDS

;SAVE VARIABLES OF FILE

SNAP4  LD   (SNME),A
       LD   HL,8000H         ;ZX RAM STARTS AT 8000H
       LD E,L                ;ZERO E
       LD   (SNLEN),DE
       LD   (SNADD),HL

;TEST FOR DIRECTORY SPACE

       LD   IX,DCHAN
       LD   B,0FEH
       IN   A,(C)
       RRA
       LD   A,1
       JR   C,SNAP4A         ;JR IF NOT "SHIFT"

       INC A                 ;DRIVE 2

SNAP4A CALL CKDRX
       LD   A,40H
       CALL FDHR
       JR NZ,SNAP3           ;LOOP IF NO SPACE

;FORM SNAPSHOT FILE NAME

       LD   A,D
       AND  7
       JR   Z,SNAP5

       ADD  A,30H
       LD   (SNME+5),A

SNAP5  LD   L,E
       SLA  L
       DEC  L
       LD   A,(IX+RPTH)
       ADD  A,L
       ADD  A,40H
       LD   (SNME+6),A

;TRANSFER NAME TO FILE AREA

       LD   HL,SNME
       LD   DE,NSTR1
       LD   BC,24
       LDIR

;OPEN A FILE

       XOR  A
       LD   (FLAG3),A
       LD   (PGES1),A
       CALL OFSM

;SAVE REGISTERS IN DIRECTORY

       LD   HL,STR-20
       LD   DE,FSA+220
       LD   BC,22
       LD   A,(NSTR1)
       CP   5
       JR   Z,SNAP6          ;JR IF 48K SNAP - NO HDR. COPY REGS

       XOR A
       LD (DE),A             ;FLAGS
       INC DE
       LD (DE),A             ;SCREEN MODE
       CALL SVHD
       LD   HL,SNPTAB        ;SCREEN$ START, LEN
       LD   DE,FSA+236
       LD   BC,7

SNAP6  LDIR
       LD   HL,(SNADD)
       LD   DE,(SNLEN)
       CALL DSVBL
       JP CFSM

SNPTAB DEFB 6EH,00H,80H      ;START (IF 256K MACHINE)
       DEFB 00H,00H,1BH      ;LEN
       DEFB 0FFH

;RETURN ADDRESS OF SNAPSHOT

SNAP7  DI
       LD   A,3
       OUT  (251),A          ;SPECTRUM "ROM" AT 8000H

       LD   HL,0
       LD   (HKSP),HL
       LD   SP,STR-20
       POP  IY
       POP  IX
       POP  DE
       POP  BC
       POP  HL
       POP  AF
       EX   AF,AF'
       EXX

       LD   HL,NMI
       LD   (0B8F6H),HL
       LD HL,SNPRT0
       LD DE,0B8F8H
       LD BC,3
       LDIR

  ;    LD   A,(SNPRT0)
   ;   LD   (0B8F8H),A
   ;   LD   A,(SNPRT1)
   ;   LD   (0B8F9H),A
   ;   LD   A,(SNPRT2)
   ;   LD   (0B8FAH),A     ;STORE INFO IN EMULATOR ROM AREA

       POP  DE
       POP  BC
       POP  HL
       POP  AF
       LD   I,A
       AND A
       JR   Z,SNAP8

       CP   3FH
       JR   Z,SNAP8
       IM   2

SNAP8  LD   SP,(STR)
       JP   0B900H




