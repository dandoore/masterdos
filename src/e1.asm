
;E1
;DISC FORMAT ROUTINE

DFMT   DI
       CALL CKDRV
       CALL SELD
       LD   B,10

DFMTA  PUSH BC
       CALL INSTP
       POP  BC
       DJNZ DFMTA

       CALL REST
       DI
       LD C,DRSEC
       CALL SADC
       CALL GTBUF
       CALL RDDATA
       AND 0DH
       JR NZ,DFMTB           ;JR IF ERROR ON READ T0/S1

       CALL PMO6             ;"FORMAT "
  ;    LD HL,DRAM+0D2H
  ;    CALL PNDN2            ;PRINT DISC NAME
       CALL PM7K             ;"Y/N", KEY
       RET NZ

DFMTB  CALL GETSCR
       CALL PMOA             ;PRINT "FORMAT DISK AT TRACK "
       LD DE,0001H           ;T0/S1
       CALL DMT1             ;PREPARE TRACK DATA
       LD HL,FTADD+0176H     ;END OF T0/S1 ENTRY 1
       CALL FESET
       JR FMT1A

FMT1   CALL DMT1             ;PREPARE TRACK DATA

FMT1A  CALL SCTRK            ;DISPLAY TRACK NUMBER
       CALL FMTSR
       BIT  5,A
       JP NZ,REP23           ;JP IF WRITE-PROTECTED

       INC  D
       CALL TSTD
       CP   D
       JR   Z,FMT7           ;JR IF ALL TRACKS DONE

       AND  7FH
       CP   D
       JR   Z,FMT6           ;JR IF TIME FOR SIDE 2

       CALL INSTP

       LD A,(SKEW)           ;FFH GIVES SKEW 1, FEH GIVES SKEW 2, 0 GIVES SKEW 0
       DEC E                 ;E=0-9
       ADD A,E
       JR C,FMT5             ;JR IF E.G. FF+02=01
                             ;           FF+01=00
                             ;CONT IF    FF+00=FF
                             ;JR IF E.G. FE+02=00
                             ;CONT IF    FE+01=FF
                             ;CONT IF    FE+00=FE

       ADD A,10              ;FF->9, FE->8

FMT5   LD E,A
       INC E                 ;1-10
       JR   FMT1

FMT6   CALL REST
       LD   D,80H
       CALL SELD
       JR   FMT1

FMT7   CALL REST             ;DE=0001
       LD   A,(DSTR2)
       CP   0FFH
       JR   Z,FMT11A         ;JR IF SIMPLE FORMAT, NOT DISK COPY

  ;    LD   HL,(BUF)
   ;   LD   (SVBUF),HL

FMT8   CALL PMOB
       CALL SCTRK

       LD   A,(DSTR2)
       CALL CKDRX
       LD   HL,FTADD

FMT9   LD   (BUF),HL
       CALL RSAD
       INC H
       INC H
       CALL ISECT
       JR   NZ,FMT9          ;READ 10 SECTORS TO BUFFER

       LD   A,(DSTR1)
       CALL CKDRX
       LD   HL,FTADD

FMT10  LD   (BUF),HL
       CALL WSAD
       INC H
       INC H
       CALL ISECT
       JR   NZ,FMT10         ;WRITE 10 SECTORS FROM BUFFER

       CALL ITRCK
       JR   NZ,FMT8

   ;   LD   HL,(SVBUF)
    ;  LD   (BUF),HL
       CALL GTIXD
       JR   FMT12

FMT11  CALL RSAD
       CALL ISECT
       JR NZ,FMT11

       CALL INSTP
       JR FMT11B

FMT11A CALL PMOC

FMT11B CALL SCTRK
       CALL ITRCK
       JR NZ,FMT11

FMT12  CALL CLSL
       LD   HL,FTADD
       CALL PUTSCR
       JP   REST

FMTSR  DI
       CALL COMMP            ;GET C=STAT PORT
       LD A,C
       LD (WSA3+1),A         ;SELF-MOD STATUS PORT
       INC C
       INC C
       INC C                 ;DATA PORT
       PUSH BC
       LD C,WTRK
       CALL PRECMP
       POP BC
       LD   HL,FTADD
       JP WSA3               ;KEEP DELAY BETWEEN PRECMP AND WSA3 SMALL

;PRINT TRACK ON SCREEN

SCTRK  PUSH DE
       LD A,21               ;COLUMN 21
       CALL NRWR
       DEFW SPOSNL
       LD   L,D
       LD   H,0
       LD   A,20H
       CALL PNUM3
       DI
       POP DE
       RET


;INCREMENT TRACK. Z IF ALL DONE

ITRCK  INC  D
       CALL TSTD
       CP   D
       RET  Z                ;RET IF FINISHED ALL TRKS

       LD H,A
       CALL TIRD
       LD A,H
       JR C,ITRK2           ;JR IF NOT RAM DISC

       LD A,50H

ITRK2  AND 7FH
       CP   D
       RET  NZ               ;RET IF NOT FINISHED SIDE 1

       CALL REST
       LD   D,7FH
       INC D                 ;NZ
       RET

;DOUBLE DENSITY FORMAT

DMT1   LD HL,FTADD
       LD   BC,3C4EH         ;60 (3CH) TRACK HDR BYTES OF 4EH
       CALL WFM
       LD   B,10             ;10 SECTORS

DMT2   PUSH BC
       LD   BC,0C00H
       CALL WFM
       LD   BC,03F5H
       CALL WFM
       LD   C,0FEH
       CALL WFMIB
       LD C,D
       RES 7,C
       CALL WFMIB            ;TRACK
       LD A,D
       AND 80H
       RLCA
       LD C,A
       CALL WFMIB            ;SIDE
       LD C,E
       CALL ISECT
       CALL WFMIB            ;SECTOR
       LD C,2
       CALL WFMIB            ;512-BYTE SECTS
       LD C,0F7H
       CALL WFMIB
       LD   BC,164EH
       CALL WFM
       LD   BC,0C00H
       CALL WFM
       LD   BC,03F5H
       CALL WFM
       LD   C,0FBH
       CALL WFMIB
       LD   BC,0
       CALL WFM
       CALL WFM              ;512 BYTES FOR DATA
       LD   C,0F7H
       CALL WFMIB
       LD   BC,1B4EH         ;3 BYTES EXTRA FOR MORE TIME BETWEEN SECTORS
       CALL WFM
       POP  BC
       DJNZ DMT2

       LD   C,4EH
       CALL WFM
       CALL WFM
       DEC B                 ;TRACK TAIL OF 768 BYTES (ONLY ABOUT 200 USED)

;WRITE FORMAT IN MEMORY

WFMIB  INC B

WFM    LD   (HL),C
       INC  HL
       DJNZ WFM
       RET

;PLACE DTKS-4 AT (HL), AND RND WORD AT (HL-3), AND NAME

FESET  LD A,(DTKS)
       SUB 4
       LD (HL),A             ;MARK DIR ENTRY 1 (T0,S1) WITH TKS/DIR-4

;CALLED BY RENAME

FESE2  PUSH HL
       DEC HL
       DEC HL
       LD A,R
       LD (HL),A
       DEC HL
       CALL NRRD
       DW 5C78H              ;FRAMES LOW
       LD (HL),A             ;RND NO USES R REG AND FRAMES

       LD BC,-42
       ADD HL,BC             ;PT TO DISC NAME DEST (BYTE D2H)
       PUSH DE
       LD DE,NSTR1+1
       EX DE,HL
       LD A,(HL)
       AND 0DFH
       CP "D"
       JR NZ,FESE3

       CALL C11SP
       JR NZ,FESE3

       LD (HL),"*"           ;ALTER "D      " TO "*       " SO FORMAT "D"
                             ;DOES NOT USE DISC NAME "D"
FESE3  LD BC,10
       LDIR                  ;COPY NAME TO TRACK BUFFER

       POP DE
       POP HL
       RET


;INCREMENT SECTOR ROUTINE

ISECT  INC  E
       LD   A,E
       CP   11
       RET  NZ
       LD   E,1
       RET


;PRINT TYPE OF FILE

PNTYP  AND  1FH
       PUSH AF
       CP 22
       JR C,PNTY1

       LD A,13

PNTY1  LD B,A
       LD HL,DRTAB
       CALL PTV2

       POP  AF               ;TYPE
       CP   16
       JR   NZ,PNTY3

       LD B,242              ;BASIC PROGRAM
       CALL GRPNTB
       LD   A,(HL)
       AND  0C0H
       JR   NZ,PNTY5

       INC  HL
       LD   E,(HL)
       INC  HL
       LD   D,(HL)
       EX   DE,HL
       CALL PNUM5
       JR   PNTY5

PNTY3  CP   19
       JR   NZ,PNTY4

       LD B,236              ;CODE
       CALL GRPNTB
       CALL GTVAL
       INC  C
       EX   DE,HL
       PUSH DE
       LD   A,20H
       CALL PNUM6
       LD   A,","
       CALL PNT
       POP  HL
       CALL GTVAL
       EX   DE,HL
       XOR  A
       CALL PNUM6

PNTY4  CP   4
       JR   NZ,PNTY5

       LD B,215              ;ZX CODE
       CALL GRPNTB
       LD   D,(HL)
       DEC  HL
       LD   E,(HL)
       EX   DE,HL
       PUSH DE
       CALL PNUM5
       LD   A,","
       CALL PNT
       POP  HL
       DEC  HL
       LD   D,(HL)
       DEC  HL
       LD   E,(HL)
       EX   DE,HL
       XOR  A
       CALL PNUM5X

PNTY5  LD A,(DTFLG)
       AND A
       JR Z,PNTY6

       LD A,17H
       CALL PNT              ;TAB
       LD A,35
       CALL PNT
       CALL PNT
       CALL PNDAT            ;PRINT DATE/TIME

PNTY6  JP PNCR


;GET NUMBER FROM HEADER

GTVAL  LD   A,(HL)
       AND  1FH
       LD   C,A
       INC  HL
       LD   E,(HL)
       INC  HL
       LD   A,(HL)
       AND  7FH
       LD   D,A
       INC  HL
       RET


DRTAB  DB 0A0H
       DB ZXS
       DM "BASI"
       DB "C"+80H    ;1
       DB ZXS,"D",ARRAY+80H  ;2
       DB ZXS,"$",ARRAY+80H  ;3
       DB ZXS+80H            ;4
       DB ZXS
       DM "SNP 48"
       DB "K"+80H  ;5
       DM "MD.FIL"
       DB "E"+80H      ;6
       DB ZXS,SCREENS+80H    ;7
       DM "SPECIA"
       DB "L"+80H      ;8
       DB ZXS
       DM "SNP 128"
       DB "K"+80H ;9
       DM "OPENTYP"
       DB "E"+80H     ;10
       DM "N/A EXECUT"
       DB "E"+80H  ;11
       DB WHAT+80H           ;12
       DB WHAT+80H           ;13
       DB WHAT+80H           ;14
       DB WHAT+80H           ;15
       DM "BASI"
       DB "C"+80H        ;16
       DB "D",ARRAY+80H      ;17
       DB "$",ARRAY+80H      ;18
       DB "C"+80H            ;19
       DB SCREENS+80H        ;20
       DM "    DI"
       DB "R"+80H      ;21


;PRINT NUMBER IN HL

PNUM6  LD   (SVA),A
       XOR  A
       LD   DE,0

       RR   C
       RR   D
       RR   C
       RR   D
       LD   A,D
       ADD  A,H
       LD   H,A
       LD   A,C
       ADC  A,E
       LD   B,A
       LD   DE,34464
       LD   C,1       ;65536
       LD   A,(SVA)
       CALL PNM2
       JR   PNUM5Y

PNUM5  LD   A,20H

PNUM5X LD   B,0
PNUM5Y LD   C,0
       LD   DE,10000
       CALL PNM2
PNUM4  LD   DE,1000
       CALL PNM1
PNUM3  LD   DE,100
       CALL PNM1
PNUM2  LD   DE,10
       CALL PNM1
PNUM1  LD   A,L

;PRINT DIGIT

PNTDI  ADD  A,30H
       JR   PNT

PNM1   LD   BC,0
PNM2   PUSH AF
       LD   A,B
       LD   B,0
       AND  A

PNM3   SBC  HL,DE
       SBC  A,C
       JR   C,PNM4
       INC  B
       JR   PNM3
PNM4   ADD  HL,DE
       ADC  A,C
       LD   C,A
       LD   A,B
       LD   B,C
       AND  A
       JR   NZ,PNM5

       POP  DE
       ADD  A,D
       RET  Z
       JR   PNT

PNM5   CALL PNTDI
       POP  DE
       LD   A,30H
       RET

;SEND A SPACE CHARACTER

SPC    LD   A,20H

;OUTPUT A CHAR TO CURRENT CHAN

PNT    PUSH AF
       CALL CMR
       DEFW 0010H

       POP  AF
       RET

;PRINT TEXT MESSAGE

PTM    POP  HL

PTM2   LD   A,(HL)
       AND  7FH
       CP 0DH
       JR NC,PTM4

       PUSH HL
       PUSH DE
       AND A
       JR NZ,PTM3            ;1-12 ARE COMPRESSION CODES

       CALL CLSL             ;0=CLSL
       CP A                  ;Z

PTM3   LD B,A
       LD HL,MCPT
       CALL NZ,PTV2          ;PNT MSG B FROM LIST AT HL

       POP DE
       POP HL
       SCF

PTM4   CALL NC,PNT
       BIT  7,(HL)
       RET NZ

       INC  HL
       JR PTM2

;0=CLEAR LOWER SCREEN

SDISKS     EQU 1
PAK        EQU 2
ATTK       EQU 3
YENO       EQU 4
ENTER      EQU 5
SDOS       EQU 6
SFREE      EQU 7
WHAT       EQU 8
ARRAY      EQU 9
ZXS        EQU 10
SCREENS    EQU 11

MCPT       DB 0A0H
           DM " dis"
           DB "k"+80H
           DM "press a ke"
           DB "y"+80H
           DM "at trac"
           DB "k"+80H
           DM " (y/n"
           DB ")"+80H
           DM "Inser"
           DB "t"+80H
           DM " DO"
           DB "S"+80H
           DM " Fre"
           DB "e"+80H
           DM "WHAT"
           DB "?"+80H
           DM ".ARRA"
           DB "Y"+80H
           DB "Z","X"+80H
           DM "SCREEN"
           DB "$"+80H

;SCREEN ROUTINES

PMO3   CALL PTM
       DEFB 0DH
       DEFB "Number of",SFREE
       DEFB "K-Bytes ="
       DEFB 0A0h

PMO5   CALL PTM
       DB 0
       DM "OVERWRITE "
       DB 34+80H

PMO6   CALL PTM
       DB 0
       DM "FORMAT "
       DB 34+80H

PMO7   CALL PTM
       DEFB 22H,YENO+80H

PMO9   CALL PTM
       DB 0,ENTER
       DM "source"
       DB SDISKS
       DB PAK+80H

PMOA   CALL PTM
       DB 0
       DM "Format"
       DB SDISKS,ATTK+80H

PMOB   CALL PTM
       DB 0
       DM "  Copy"
       DB SDISKS,ATTK+80H

PMOC   CALL PTM
       DB 0
       DM "Verify"
       DB SDISKS,ATTK+80H

PMOD   CALL PTM
       DB 0,ENTER
       DM "target"
       DB SDISKS,PAK+80H

PMOE   CALL PTM
       DEFM " Fil"
       DB "e"+80H

PMOF   CALL PTM
       DB SFREE
       DM "Slo"
       DB "t"+80H

PMOG       CALL PTM
           DB 0
           DM "LOADING"
           DB " "+80H

PMOH       CALL PTM
           DB 0
           DM " SAVING"
           DB " "+80H

PMOSD      CALL PTM
           DB "S","A","M",SDOS
           DM "  "
           DB " "+80H

PMYNAE     CALL PTM
           DB 34
           DM " (y/n/a/e"
           DB ")"+80H

;PRINT DISC NAME

PNDNM  LD HL,DNAME

;FROM FORMAT

PNDN2  LD A,(HL)             ;00 IF OLD DOS, OR FF IF NEV USED IT FOR IBU
                             ;OR FIRST NAME CHAR
       RES 7,(HL)            ;SO NEV CAN USE BIT 7 AS IBU MARKER
       INC A
       CP 2
       JR C,PMOSD            ;PRINT "   SAM DOS " IF OLD DOS

       LD A,(HL)
       CP "*"
       JR NZ,PMO8            ;PRINT DISC NAME IF THERE IS ONE,

                             ;PRINT "MASTER DOS " IF NO NAME
PMOMD      CALL PTM
           DB "MASTER",SDOS+80H

PMO8   CALL PTM
DNAME  DS 10                 ;OVER-WRITTEN
       DB " "+80H

PMOOF      CALL PTM
           DM "OPEN Fil"
           DB "e"+80H

MSGUN      DB "UN ",8        ;SPACE, BACKSPACE CANCELS LEADING SPACE ON KWDS

OHNM   CALL CLSL
       XOR A
       CALL WIQF             ;TOKENS ON
       CALL NRRD
       DW CURCMD
       CP 241
       JR C,OHNM2            ;JR IF ERASE OR COPY

       PUSH AF
       CALL BITF1
       LD HL,MSGUN
       LD B,4
       CALL NZ,PBFHL         ;PRINT "UN" IF PROTECT OR HIDE OFF

       POP AF

OHNM2  CALL PNT              ;PRINT PROTECT, HIDE, ERASE OR COPY
       LD A,22H
       CALL PNT              ;NOW E.G. ERASE "
       LD A,1

WIQF   CALL NRWR
       DW INQUFG             ;TOKENS OFF
       RET

FNMAE  CALL PFNME            ;PRINT FILE NAME
       CALL PMYNAE           ;Y/N/A/E
       CALL CYES1
       RET Z                 ;RET IF "Y"

       CP "E"
       JP Z,ENDS             ;END

       CP "A"
       RET NZ

       LD HL,FLAG3
       RES 7,(HL)            ;"?" OPTION OFF - DO ALL
       CP A                  ;Z - "Y"
       RET

FNM7K  CALL PFNME            ;PRINT FILE NAME

PM7K   CALL PMO7             ;"Y/N"

;COMPARE FOR Y or N

CYES   CALL BEEP

CYES1  CALL RDKY
       JR   NC,CYES1

       AND  0DFH
       CP   "Y"
       PUSH AF

CYES2  CALL RDKY
       JR C,CYES2

       CALL CLSL
       POP  AF
       RET

TSPCE1 CALL BITF5
       RET Z

       CALL PMOD
       JR   TSPC1

TSPCE2 CALL BITF5
       RET Z

       CALL PMO9

TSPC1  CALL RDKY
       JR   NC,TSPC1

TSPC2  CALL RDKY
       JR   C,TSPC2

       CALL CLSL

RDKY   PUSH IX
       CALL CMR
       DEFW RDKEY
       POP IX
       RET


