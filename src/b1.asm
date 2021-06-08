
;B1
       ORG  GND

PTHRD  EQU 40D2H         ;TEMP RAM DISC PATH NAME

       ;THIS AREA (256 BYTES) WILL BE OVER-WRITTEN BY NSAM

START  LD   HL,8000H+511
       LD   E,(HL)
       DEC HL
       LD   D,(HL)           ;T/S OF REST OF FILE

DOS    XOR  A
       LD   (START+FS),A
       LD   (START+1+FS),HL  ;USE AS VARIABLE

       LD   A,E
       OUT  (SECT),A

DOS2   IN   A,(COMM)
       BIT  0,A
       JR   NZ,DOS2

       IN   A,(TRCK)
       CP   D
       JR   Z,DOS4

       LD   A,STPOUT
       JR   NC,DOS3
       LD   A,STPIN

DOS3   OUT  (COMM),A
       LD   B,20
DEL1   DJNZ DEL1
       JR   DOS2

DOS4   DI
       LD   A,DRSEC
       OUT  (COMM),A
       LD   B,20
DEL2   DJNZ DEL2

       LD   HL,(START+1+FS)
       LD   BC,DTRQ
       JR   DOS6

DOS5   INI

DOS6   IN   A,(COMM)
       BIT  1,A
       JR   NZ,DOS5
       RRCA
       JR   C,DOS6

;CHECK DISC ERR COUNT

       AND  0DH
       JR   Z,DOS8

       LD   A,(START+FS)
       INC  A
       LD   (START+FS),A
       PUSH AF
       AND  2
       JR   Z,DOS7

       LD   A,DRES
       OUT  (COMM),A
       LD   B,20

DEL3   DJNZ DEL3

DOS7   POP  AF
       CP   10
       JR   C,DOS2

       RST  8
       DEFB 19

DOS8   DEC  HL
       LD   E,(HL)
       DEC  HL
       LD   D,(HL)
       LD   A,D
       OR   E
       JR   NZ,DOS

       LD HL,SVHDR+FS
       LD B,SNME-SVHDR

SVERL  LD (HL),A
       INC HL
       DJNZ SVERL            ;CLEAR SYS VARS FOR CONSISTENT RESULTS

       LD   A,(5CB4H)        ;LAST PAGE
       LD   (PORT2+FS),A
       DEC  A
       LD   (SNPRT2+FS),A
       IN A,(251)
       AND 1FH
       LD   (DOSFLG),A       ;DOSFLG = DOS PAGE

       LD   H,51H
       LD   L,A
       LD   (HL),60H    ;PAGE ALLOCATION TABLE MARKED "DISK USE"

       LD   HL,0144H    ;DEVICE
       LD   (5A06H),HL

       XOR  A
       LD   (SAMCNT+FS),A
       LD   (5BC3H),A        ;"DOS NOT IN CONTROL"

       LD BC,DTRQ2

TDLP   OUT (C),B
       LD A,20

TDDL   DEC A
       JR NZ,TDDL            ;WAIT 50 USEC

       IN A,(C)
       CP B
       LD A,0                ;"NO SECOND DRIVE"
       JR NZ,TD2             ;JR IF NO SECOND DISC CHIP

       DJNZ TDLP

       LD A,(TRAKS2+FS)
       AND A
       JR NZ,TD3             ;JR IF TRACKS ALREADY SET FOR DISC 2

       LD A,128+80

TD2    LD (TRAKS2+FS),A

TD3    POP HL                ;RETURN TO ROM1 IF "BOOT", NEXT STAT IF "BOOT 1"
       POP DE                ;NEXT STAT IF "BOOT", ERR HANDLER IF "BOOT 1"
       POP BC                ;ERR HAND IF BOOT
       PUSH BC
       PUSH DE
       PUSH HL
       BIT 7,H
       JR Z,TD4              ;JR IF HL=NEXT STAT (BOOT 1)

       EX DE,HL
       LD D,B
       LD E,C

TD4        LD (NEXTST+FS),HL     ;KEEP A RECORD TO USE IN SOME SYNTAX HANDLING
           LD (L1303),DE
           CALL MRINIT+FS        ;MEGA RAM INIT

           LD HL,SERDT+FS
           LD DE,4BA0H
           LD BC,DTEND-SERDT
           LDIR              ;COPY CODE TO SYS PAGE AT 4BA0H

           LD HL,4BB0H
           LD (5ADEH),HL     ;PRTOKV

           LD HL,XTRA+MTV-PFV
           LD (5AFAH),HL     ;MTOKV

           LD HL,4BB0H+EVV-PVECT
           LD (5AF6H),HL     ;EVALUV

           LD HL,4BB0H+SLVP-PVECT
           LD (CMDV),HL
           JP INIP3+FS           ;IN HOOKS.DOS


       DS 4100H-$

FFHL   DEFM "BO"
FFDE   DEFB "O","T"+80H

ENTSP  DEFW 0
SNPRT0 DEFB 1FH
SNPRT1 DEFB 2
SNPRT2 DEFB 0
SNPSVA DEFB 0

;CLEARED FROM HERE ON:

SVHDR  DEFW 0
CCHAD  DEFW 0
CNT    DEFW 0

DSC    DEFB 0
DCT    DEFB 0
DST    DEFB 0
       DEFS 7
NBOT   DEFB 0
RCMR   DEFB 0
COUNT  DEFB 0
SVA    DEFB 0
SVC    DEFB 0
NRFLG  DEFB 0 ;WAS SAMCNT
RMSE   DEFB 0
SMSE   DEFB 0

SVDPT  DEFW 0
SVTRS  DEFW 0
SVBUF  DEFW 0
SVCNT  DEFW 0
HLDI   DEFW 0
PTRSCR DEFW 0

PORT1  DEFB 0
PORT2  DEFB 0
PORT3  DEFB 0
SVCST  DEFB 0

TSTR1  DEFB 0
OSTR1  DEFB 0
CSTR1  DEFB 0
HSTR1  DEFB 0

DSTR1  DEFB 0
FSTR1  DEFB 0
SSTR1  DEFB 0
LSTR1  DEFB 0
NSTR1  DEFB 0
       DEFS 14
HD001  DEFB 0
HD0B1  DEFW 0
HD0D1  DEFW 0
HD0F1  DEFW 0
PGES1  DEFB 0
PAGE1  DEFB 0

DSTR2  DEFB 0
FSTR2  DEFB 0
SSTR2  DEFB 0
LSTR2  DEFB 0
NSTR2  DEFB 0
       DEFS 14
HD002  DEFB 0
HD0B2  DEFW 0
HD0D2  DEFW 0
HD0F2  DEFW 0
PGES2  DEFB 0
PAGE2  DEFB 0

NSTR3  DEFB 0
       DEFS 14

UIFA   DEFB 0
       DEFS 47

DIFA   DEFB 0
       DEFS 47

HKA    DEFB 0
HKHL   DEFW 0
HKDE   DEFW 0
HKBC   DEFW 0

;END OF CLEARED SECTION

SNME   DEFB 13H
       DEFM "SNAP          "
       DEFB 13H
SNLEN  DEFW 49152
SNADD  DEFW 16384
       DEFW 0
       DEFW 0FFFFH

FSLOT  DEFW 0                ;SECT/TRK OF FREE DIR SLOT, OR 00?? IF NONE
FSLTE  DEFB 0                ;0 OR 1 FOR DIR ENTRY IN SECTOR

;MAIN PROGRAM ENTRY POINT

       DS 4200H-$

       JP   HOOK
       JP   SYNTAX
       JP   NMI

TEMPB1 DB 0
DTKSX  DB 0                ;USED BY SNDFL AS DTKS
HKSP   DEFW 0
RDAT   DB 0
SAMDR  DB 0

       DS 4210H-$

       DEFW ERRTBL+FS

TEMPW1 DEFW 0
TEMPW2 DEFW 0
TEMPW3 DEFW 0
TEMPW4 DEFW 0
TCNT   DEFW 0                ;DIR'S 'TOTAL FILES ON DISC' COUNTER
FCNT   DEFW 0                ;DIR'S 'FILES IN CUR. DIRECTORY' COUNTER
NEXTST DEFW 0

       DS 4220H-$

DVAR   EQU  $                ;4220H

RBCC   DEFB 7                ;0
TRAKS1 DEFB 128+80           ;1
TRAKS2 DEFB 0                ;2
STPRAT DEFB 0                ;3
STPRT2 DEFB 0                ;4
CHDIR  DEFB " "              ;5  DIR SPACE CHAR
NSTAT  DEFB 1                ;6
VERS   DEFB 43               ;7  VERSION
DCOLS  DEFB 0                ;8  AUTO-SET FOR DIR COLUMN NUMBER
SRTFG  DEFB 1                ;9  SORTED DIR "ON"
DELIM  DEFB 0DH              ;10 POINT'S DELIMITER
FNSEP  DEFB "."              ;11 SEPARATOR IN FILE NAMES - USED BY PFNME
RTSYM  DB "\/"               ;12 (2) ROOT SYMBOLS
SKEW   DEFB 0FFH             ;14 FOR SKEW 1. 0FEH FOR SKEW 2
ODEF   DEFB 1                ;15 DEFAULT DRIVE
DTKS   DEFB 4                ;16 NUMBER OF DIR TRACKS (4 OR MORE)
CDIRT  DEFW 0                ;17 (2) CURRENT DIR CODE (TEMP) AND EXX VERSION
DTFLG  DEFB 0                ;19 NZ IF DATES TO BE SHOWN IN DIR
SAMCNT DEFB 0                ;20
MAXT   DEFB 0                ;21 MAX TAG VALUE FOR SUB DIRS
       DEFW SAMHK            ;22 (2) ADDR OF HOOKS
MSFLG  DB   0                ;24 INVERT CHARS >127 MOVED TO SCREEN
                             ;   USE 1 TO PRINT ALL EXCEPT 0FFH
MSUPC  DB "."                ;25 CHAR TO USE FOR CHARS <32 OR =255
NMIKP  DB   4                ;26 PAGE AT 8000H IF NMI AND "1" OR "5"
NMIKA  DW 0004H              ;27 (2) ADDR CALLED IF DITTO. E=PORT (BITS 4-0)
DWAI   DB 0                  ;29 NUMBER OF .25 SECS BEFORE SAVE,-1
EXTADD CALL CMR
ONERR  DEFW 0                ;33 (2)
       RET

EAPG   DB 0                  ;36 PAGE USED IF ONERR IS ABOVE 8000H
MSINC  DEFW 0200H            ;37 (2) MULTI-SECTOR INCREMENT

;TABLE OF TRKS/DRIVE FOR EACH RAM DISC

RDDT   DB 0,0,0,0,0          ;(5) 39-43 RAM DISCS 3,4,5,6,7 START AT 0 TKS

;TABLE OF FIRST PAGE FOR EACH RAM DISC

FIPT   DB 0,0,0,0,0          ;(5) 44-48

;TABLE OF DIRECTORY NUMBER FOR EACH DRIVE

CDIT   DB 0,0,0,0,0,0,0      ;(7) 49-55

;TABLE OF PATH LENGTH FOR EACH DRIVE

PLT    DB 2,2,2,2,2,2,2      ;(7) 56-62

;TABLE OF RANDOM WORDS FOR EACH DRIVE

CRWT   DW 0                  ;(14) 63-76
       DW 0
       DW 0
       DW 0
       DW 0
       DW 0
       DW 0

SAMRN      DW 0              ;(2) 77 SAM RND NO. (DISC RND NO. WHEN SAM CREATED)

TDVAR      DW 0              ;(2) 79

DATDT      DB "00/00/00",0DH ;(9) 81-89 DD/MM/YY CR

           DB 31,1,12,1,99,0 ;(6) 90-97 DD/MM/YY HI/LOW LIMITS

TIMDT      DB "00:00:00",0DH ;(9) 98-104 HH:MM:SS CR

           DB 23,0,59,0,59,0 ;(6) 105-110 HH/MM/SS HI/LOW LIMITS

;TABLE OF DRIVES EACH DRIVE PRETENDS TO BE

DRPT       DB 1,2,3,4,5,6,7  ;(7) 111-117

;TABLE OF BITS, 1 PER 16K PAGE IN A MEGA RAM=64 BITS,8 BYTES
;4 POSSIBLE MEGA RAMS=32 BYTES

MRTAB      DS 20H            ;(32) 118-149

CKPT       DB 0EFH           ;150 CLOCK PORT
BEEPT      DW 0085H          ;151 (2) BEEP TIME
XXPTR      DEFW 0            ;(2) 153 XPTR STORE


       DB (SYNTAX-CTAB)/3      ;153 NO. OF CMDS IN CTAB

;CMD VALUE AND ADDR TABLE

CTAB   DB   86H       ;WRITE
       DW WRITE

       DB   90H       ;DIR
       DW DIR

       DB   91H       ;FORMAT
       DW WFOD

       DB   92H       ;ERASE
       DW ERAZ

       DB   93H       ;MOVE
       DW MOVE

       DB   95H       ;LOAD
       DW LOAD

       DB   98H       ;OPEN
       DW OPEN

       DB   99H       ;CLOSE
       DW CLOSE

       DB   0B3H      ;CLEAR
       DW CLEAR

       DB   0B8H      ;READ
       DW READ

       DB   0CFH      ;COPY
       DW COPY

       DB   0E3H      ;RENAME
       DW RENAM

       DB   0E4H      ;CALL
       DW CALL

       DB   0F1H      ;PROTECT
       DW PROT

       DB   0F2H      ;HIDE
       DW HIDE

       DB   0F7H      ;BACKUP
       DW BACKUP

       DB  0F8H       ;TIME
       DW TIME

       DB  0F9H       ;DATE
       DW DATE

       DB 0           ;LAST ENTRY FOR NOT FOUND
       DW CNF

SYNTAX DI
       CP 53                 ;NO DOS
       JR Z,SYNT1

       CP   29               ;NOT UNDERSTOOD
       JR   NZ,ST3HP

SYNT1  LD   (SVCST),A        ;ERROR NUMBER
       LD HL,NRFLG           ;NO RECURSE FLAG
       INC (HL)
       DEC (HL)
       LD (HL),1             ;"NO RECURSE NOW"

ST3HP  JP NZ,SYNT3           ;JP IF E.G. DOS CALLED EXPT1NUM AND GOT VAL"#!"
                             ;GIVING NONSENSE ERROR - DO NOT RECURSIVELY CALL
                             ;DOS!
       CALL SETSTK
       LD BC,ENDS
       PUSH BC
       CALL ZFSP             ;ZERO FLAG3 AND HKSP
       CALL RESREG
       CALL GTIXD

;GET CHADD AND SAVE IT

       CALL NRRDD
       DEFW CHADD
       LD   (CCHAD),BC

;GET START OF STATEMENT

       CALL NRRDD
       DEFW CSTAT
       CALL NRWRD
       DEFW CHADD

       CALL GCHR
       CP ":"
       CALL Z,GTNC    ;SKIP ANY COLON
                      ;CSTAT POINTS TO ":" IF E.G. "ON X: DIR: PRINT..."

       EX DE,HL
       LD HL,CTAB-1
       LD B,(HL)             ;NO. OF CMDS
       DEC HL

LCMDL  INC HL
       INC HL
       CP (HL)
       INC HL
       JR Z,CMDF

       DJNZ LCMDL            ;IF NOT FOUND, END ON "CNF" ROUTINE

CMDF   LD C,(HL)
       INC HL
       LD B,(HL)
       EX DE,HL              ;HL=CHAD
       PUSH BC
       RET

;NO MATCH IN TABLE

CNF    INC A          ;POINT IS FF 3D
       JR   NZ,CKESV

       CALL GTNC
       CP 3DH
       JP Z,POINTC

;CHECK EXTERNAL SYNTAX VECTOR

CKESV  POP BC                ;JUNK ENDS
       EX (SP),HL            ;CHAD TO STACK, GET ENTSP
       LD (ENTSP),HL         ;ORIG
       LD   BC,(CCHAD)
       CALL NRWRD
       DEFW CHADD

           POP BC            ;CHAD PTR TO CMD OR AFTER FF
           LD HL,(ONERR)
           LD A,H
           OR L
           LD A,(SVCST)
           JR Z,SYNT3

           BIT 7,H
           JP Z,EXTADD

           IN A,(251)
           LD D,A            ;ORIG
           LD A,(EAPG)
           OUT (251),A
           LD A,(SVCST)
           JP (HL)

SYNT3  LD   E,0              ;NO ACTION
       LD HL,NRFLG           ;RECURSE OK
       LD (HL),E
       RET                   ;A=ERROR NO.

;SAMDOS HOOK CODE ROUTINE

HOOK   DI
       ADD A,A               ;JUNK BIT 7, MAKE WORD OFFSET
       LD (SVCST),A          ;HOOK CODE OFFSET
       EX   AF,AF'
       LD   (HKA),A
       CALL SETSTK
       LD (SVHDR),IX
       EXX
       LD   (HKHL),HL
       LD   (HKDE),DE
       LD   (HKBC),BC
       EXX
       CALL ZFSP
       CALL NRWR             ;A=0
       DEFW XPTR+1           ;NO ERROR IN CASE PRINT

       LD   HL,(SVCST)       ;H=JUNK
       LD   A,(HKA)
       LD   DE,SAMHK
       CALL INDJP            ;INDEXED JP USING L

;RETURN FROM HOOK CODE O.K

       CALL BCR
       EXX                   ;PASS HL,DE,BC OUT VIA ALT REGS
       XOR A                 ;"NO ERROR"
       LD E,A                ;"NO ACTION"
       JP RENT               ;RESTORE ORIG ENTSP

SETSTK POP IY                ;RET ADDR
       CALL NRRDD
       DW DOSSTK             ;READ - SET TO 7FF0H BY BASIC AT EACH CMD
       LD H,B
       LD L,C
       POP AF
       POP DE
       POP BC                ;GET ALL 3 VALUES FROM STACK AT 8000H
       LD SP,HL              ;NEW STACK SO THAT DOS ->CALL ROM->HOOKS CAN AVOID
                             ;OVERWRITING MAIN DOS STACK
       PUSH BC
       PUSH DE
       PUSH AF
       LD HL,(ENTSP)
       PUSH HL               ;OLD VALUE RESTORED ON EXIT
       LD (ENTSP),SP
       JP (IY)

;ZERO FLAG3, HKSP

ZFSP   XOR A
       LD (FLAG3),A
       LD H,A
       LD L,A
       LD (HKSP),HL
       RET

;RESET ALL REGISTERS

RESREG LD   HL,TSTR1
       LD   B,UIFA-TSTR1
       LD A,0FFH

RESR1  LD   (HL),A
       INC  HL
       DJNZ RESR1

       LD (RDAT),A
       RET

;COMMAND CODE TABLE

SAMHK  DEFW INIT      ;128
       DEFW HGTHD     ;129
       DEFW HLOAD     ;130
       DEFW HVERY     ;131
       DEFW HSAVE     ;132
       DEFW SKSAFE    ;133
       DEFW HOPEN     ;134
       DEFW HCLOS     ;135
       DEFW HAUTO     ;136
       DEFW HSKTD     ;137 SEEK TRACK D
       DEFW S         ;138 FORMAT TRACK UNDER HEAD, USING DE AS 1ST T/S
       DEFW HVAR      ;139
       DEFW HEOF      ;140
       DEFW HPTR      ;141
       DEFW HPATH     ;142
       DEFW HLDPG     ;143 LIKE 130 BUT A=PAGE
       DEFW HVEPG     ;144 LIKE 131 BUT A=PAGE
       DEFW HSDIR     ;145 SET DIR. A=PAGE, DE=START, BC=LEN OF NAME
       DEFW ROFSM     ;146
       DEFW HOFLE     ;147
       DEFW SBYT      ;148
       DEFW HWSAD     ;149
       DEFW HKSB      ;150 SAVE ADE FROM HL
       DEFW HDBOP     ;151 O/P BC FROM DE TO DISC FILE (IX)
       DEFW SCFSM     ;152
       DEFW HORDER    ;153 A=LEN TO SORT ON, BC=STR. LEN, DE=NO., HL=START
       DEFW S         ;154
       DEFW S         ;155
       DEFW S         ;156
       DEFW S         ;157
       DEFW HGFLE     ;158
       DEFW LBYT      ;159
       DEFW HRSAD     ;160
       DEFW HLDBK     ;161
       DEFW HFRSAD    ;162 FAR READ SECTORS
                      ;    READ FROM DSC A, TRK D, SCT E TO PAGE C, OFFSET HL
                      ;    IX SECTORS

       DEFW HFWSAD    ;163 FAR WRITE SECTORS
                      ;    WRITE TO DSC A, TRK D, SCT E FROM PAGE C, OFFSET HL
                      ;    IX SECTORS
       DEFW REST      ;164
       DEFW PCAT      ;165
       DEFW HERAZ     ;166
       DEFW MCHWR     ;167
       DEFW MCHRD     ;168
       DEFW HPTV      ;169 PRINT TOKEN "A"
       DEFW HPFF      ;170 POST FF
       DEFW HGTTK     ;171 GET TOKEN
       DEFW HKLEN     ;172 EVALUATOR PATCH
       DEFW HSLMV     ;173 SAVE/LOAD ETC PATCH
       DEFW RCPTCH    ;174 RUN/CLEAR PATCH


