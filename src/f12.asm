

;F12

;SELECT A FILE IN THE DIRECTORY, KEEPING PLACE IN DIRECTORY SO LATER
;PARTS CAN BE LOOKED AT LATER.
;DRAM IS UNCHANGED AND DOES NOT NEED RE-READING UNLESS NEXT SECTOR NEEDED

SNDFL  CALL BITF2
       JR   NZ,SNDF3

       CALL SETF2
       CALL GTIXD
       XOR  A
       LD (IX+4),A           ;FDHR FLAGS=0 FOR CHKNM
       LD (IX+RPTH),A        ;1ST ENTRY
       CALL REST
       CALL RSAD             ;T0/S1
       CALL SDTKS            ;SET DIR TKS, CHECK RAND NO
       JR SNDF15

SNDF1  CALL RSAD

SNDF15 LD   (SVTRS),DE

SNDF2  CALL POINT
       LD   A,(IX+RPTH)
       LD   (SVDPT),A
       LD   A,(HL)
       AND  A
       JR   Z,SNDF25         ;JR IF FREE SLOT

       CALL CKDIR
       CALL Z,CKNAM

       JR NZ,SNDF3           ;JR IF NOT MATCHED

       SCF
       RET

SNDF25 INC HL
       OR (HL)
       RET Z                 ;RET IF ALL USED DIRECTORY HANDLED. NC

SNDF3  LD   A,(SVDPT)
       LD   (IX+RPTH),A
       DEC A
       JR   Z,SNDF4          ;JR IF PTR WAS TO 2ND ENTRY

       CALL CLRRPT
       INC  (IX+RPTH)
       JR   SNDF2            ;DEAL WITH 2ND ENTRY

SNDF4  LD   DE,(SVTRS)
       CALL ISECT
       JR   NZ,SNDF1

       INC  D
       LD   HL,DTKS
       LD   A,D
       CP 4
       JR NZ,SNDF5

       INC E                 ;SKIP T4,S1

SNDF5  CP   (HL)
       JR   C,SNDF1

       RET

;SEPARATOR REPORT ROUTINE

SEPARX CALL SEPAR
       RET  Z

REP0   CALL DERR
       DB 29

;THE SEPARATOR SUBROUTINE

SEPAR  CP   ","
       JR   Z,SEPA1
       CP   ";"
       JR   Z,SEPA1
       CP   """
       RET

SEPA1  CALL GTNC
  ;    LD   (SVA),A
  ;    XOR  A
  ;    LD   A,(SVA)
       CP A
       RET


;EVALUATE PARAMETERS IN SYNTAX FOR READ AT/WRITE AT D,T,S,ADDR

EVPRM  LD C,87H       ;AT
       CALL ISEPX

;GET DRIVE NUMBER

       CALL EVDNM

;GET TRACK NUMBER

       CALL SEPARX
       CALL EVNUM
       PUSH BC               ;C=TRK IF RUN TIME

;GET SECTOR NUMBER

       CALL SEPARX
       CALL EVNUM
       POP DE                ;E=TRK
       LD  B,E               ;BC=T/S
       PUSH BC

;GET ADDRESS

       CALL SEPARX
       CALL EVADDR           ;Z IF SYNTAX
       PUSH AF
       PUSH HL               ;AHL=ADDR
       CALL CIEL
       LD BC,1               ;DEFAULT=1 SECT
       JR Z,EVPR5            ;JR IF NO "SECTORS" PARAM

       CALL EVNUMX           ;NUMBER OF SECTORS
       JR Z,EVPR5

       DEC HL
       LD A,H
       CP 4
       JP NC,IOOR            ;ALLOW 0000-03FFH IN DECED, 0001-0400H IN ORIG
                             ;BC= 1-1024 SECTORS

EVPR5  POP HL
       POP AF                ;AHL=ADDR, NZ IF RUN
       CALL NZ,SCASD         ;SET/CHECK ADDR IN AHL, SECTORS IN BC

       POP HL
       LD (HKDE),HL          ;T/S
       CALL CEOS

       LD   A,(DSTR1)
       LD   (HKA),A
       RET


;SAVE HEADER INFORMATION

SVHD   LD   HL,HD001
       LD   DE,FSA+211
       LD   B,9

SVHD1  LD   A,(HL)
       LD   (DE),A
       CALL SBYT
       INC  HL
       INC  DE
       DJNZ SVHD1
       RET


;REMOVE FLOATING POINT NUMBERS

REMFP  LD   A,(HL)
       CP   0EH
       JR   NZ,REMP1

       LD   BC,6
       CALL CMR
       DEFW JRECLAIM

REMP1  LD   A,(HL)
       INC  HL
       CP   0DH
       JR   NZ,REMFP

       RET

;LOAD COMMAND (FAILED NORMAL SYNTAX)

LOAD   CALL GTNC             ;CHAR AFTER LOAD

       CALL CFSO
       CALL Z,REMFP          ;REMOVE FP FORMS FROM HL ON IF SYNTAX TIME

       CALL EVNUM
       JP Z,CEOS

       LD (TEMPW4),HL        ;FILE NUM CAN BE 2 BYTES
       LD   A,L
       LD   (FSTR1),A
       CALL GTFLE

AUTOX  LD   A,(DIFA)
       CP   14H
       JR   NZ,DLVM1         ;JR IF NOT SCREEN$ OR FORMER TYPE 5

       CALL BITF7
       JR   Z,DLVM1          ;JR IF SCREEN$

;48K SNAPSHOT IS FOUND

       CALL RSADSV

       LD BC,0300H+250
       LD HL,SNPRT0

RDPRTL IN A,(C)
       LD (HL),A
       INC HL
       INC C
       DJNZ RDPRTL

     ; IN   A,(250)
     ; LD   (SNPRT0),A
     ; IN   A,(251)
     ; LD   (SNPRT1),A
     ; IN   A,(252)
     ; LD   (SNPRT2),A

       LD   A,4
       OUT  (251),A          ;ZX "SCREEN" AT 8000H
       OUT  (252),A          ;DISPLAY PAGE 4, SCREEN MODE 1

       LD   HL,8000H
       LD   DE,4000H
       LD   A,2
       LD   (PGES1),A
       CALL LDBLK            ;LOAD 48K TO ZX IMAGE
       CALL SKSAFE
       JP   SNAP7

DLVM1  CP   10H
       JR   NZ,DLVM2         ;JR IF NOT BASIC

       CALL BITF7
       JP   NZ,REP13         ;"WRONG FILE TYPE" IF ZX BASIC

       CALL NRRDD
       DEFW PROG
       PUSH BC
       POP  HL

       CALL NRRD
       DEFW PROGP
       LD   (UIFA+31),A
       LD   (UIFA+32),HL
       EX   DE,HL
       LD   C,A

       PUSH BC
       CALL NRRDD
       DEFW ELINE
       PUSH BC
       POP  HL

       CALL NRRD
       DEFW ELINP

       POP  BC
       DEC  HL
       BIT  7,H
       JR   NZ,LAB2

       DEC  A
LAB2   PUSH BC
       PUSH DE
       CALL AHLN
       PUSH AF
       EX   DE,HL
       LD   A,C
       CALL AHLN
       EX   DE,HL
       LD   C,A
       POP  AF
       AND  A
       SBC  HL,DE
       SBC  A,C
       POP  DE
       POP  BC
       CALL PAGEFORM
       LD   (UIFA+34),A
       LD   (UIFA+35),HL
       XOR  A
       LD   (UIFA+15),A

DLVM2  LD IX,4B00H
       CALL TXINF
       CALL TXHED
       LD HL,SYSP-1
       SET 6,(HL)            ;ENSURE ROM1 ON ON EXIT FROM DOS BY ALTERING
                             ;PORT 251 STATUS ON STACK
       JP   ENDSX            ;NOW JUMP TO ROM1 TO LOAD IS OK

;GET 19-BIT NUMBER

AHLN   CALL AHLNX
       AND 07H
       RET

;GET 20-BIT NUMBER

AHLNX  RLC  H
       RLC  H
       RRA
       RR   H
       RRA
       RR   H
       AND  0FH
       RET

IOOR   CALL DERR
       DB 30

;WRITE FORMAT ON DISC

WFOD   CALL FDFSR
       LD A,4
       LD (DTKS),A    ;DEFAULT TRACKS/DIR
       XOR A
       LD (TEMPW1),A         ;DEFAULT TKS/DISC
       CALL GTNC
       CP   8EH       ;TO
       JR   Z,WFOD1          ;JR IF 'FORMAT TO "NAME"'

       CALL CIEL
       JR   Z,WFOD2X

       CALL EVNAM
       CP ","
       JR NZ,WFOD0           ;JR IF 'FORMAT "NAME" TO "NAME"'

       CALL EVNUMX
       JR Z,WFOD01

       LD A,B
       AND A
       JR NZ,IOOR

       LD A,C
       LD (DTKS),A

WFOD01 CALL GCHR
       CP ","
       JR NZ,WFOD2

       CALL EVNUMX           ;TOTAL TKS OF RAM DISC
       JR Z,WFOD2

       LD A,B
       AND A
       JR NZ,IOOR

       LD A,C
       LD (TEMPW1),A         ;TKS/DISC
       AND A
       JR NZ,WFOD2

WIOOR  JP IOOR

WFOD0  CP   8EH       ;TO
       JR   NZ,WFOD2

       CALL EVNAM2X          ;SECOND NAME OF TWO
       CALL CEOS

       CALL EVFINS           ;FIRST
       JR WFODTC

WFOD1  CALL EVNAM2X          ;SECOND (ONLY) NAME
       CALL CEOS

WFODTC CALL FTOSR            ;EXDAT AND CHECK DRIVE
       CALL EVFINS           ;SECOND NAME
       CALL FTOSR

WFOD3  JP DFMT

WFOD2X CALL CEOS
       JR WFOD3X

WFOD2  CALL CEOS

       CALL EVFINS           ;FIRST NAME

WFOD3X LD A,(DTKS)
       LD C,A
       CP 40
       JR NC,WIOOR

       LD B,4                ;LOW DTK LIMIT
       LD A,(DSTR1)
       CP 3
       JR C,WFOD00

       LD B,1                ;ALLOW 1 OR MORE DIR TKS FOR RAMD
       LD A,C
       AND A
       JR Z,WFODB            ;JR IF E.G. FORMAT"D3:",0 - ERASE RAM DISC

       LD A,(TEMPW1)
       AND A
       JR Z,WIOOR             ;E.G. FORMAT "D3:",5,0 ILLEGAL
                             ;AS IS FORMAT "D3:",5
       JR WFOD07

WFOD00 LD A,(TEMPW1)
       AND A
       JR NZ,WIOOR           ;E.G. FORMAT "D1:",5,10 IS ILLEGAL. ONLY
                             ;FORMAT "D1",4 ETC O.K.
WFOD07 LD A,C
       CP B
       JR C,WIOOR            ;ALLOW 4-39 DIR TRACKS (80-780 FILES) IF REAL DISC
                             ;OR 1-39 IF RAMDISC

       LD A,(TEMPW1)
       AND A
       JR Z,WFODB            ;JR IF REAL DISC FORMAT

       LD C,A
       LD B,159              ;LIMIT
       LD A,(DTKS)
       SUB C
       JR NC,WIOOR            ;TOTAL TKS MUST BE >DTKS

       ADD A,C
       SUB 4
       JR NC,WFOD02          ;JR IF DIR IS NORMAL OR LARGE

       ADD A,B
       LD B,A                ;LOWER LIMIT - 3 DTK=158, 2=157, 1=156

WFOD02 LD A,C
       SUB 2
       CP B

WIORH  JR NC,WIOOR           ;DTKS=1, TOT TKS 2-157 OK
                             ;DTKS=2, TOT TKS 3-158
                             ;DTKS=3, TOT TKS 4-159
                             ;DTKS=4, TOT TKS 5-160
                             ;DTKS=5, TOT TKS 6-160
WFODB  LD A,(DSTR1)
       CP 3
       JR C,WFOD3

       JP FORMRD             ;FORMAT RAMDISC


TIRDXDCT   XOR A
           LD (DCT),A

;TEST IF RAM DISC - NC IF SO

TIRD       LD A,(DRIVE)
           CP 3
           RET


;EVALUATE DRIVE NO. OR FILE NAME (USED BY DIR)
;EXIT: CY IF DRIVE

EVEXP  CP "*"
       JR Z,EVDN1

       CALL CMR
       DW EXPEXP

       JR Z,EVEXP2           ;JR IF STRING

       CALL EVNU2            ;UNSTACK NUMBER IF RUNNING
       SCF                   ;"DRIVE"
       JR EVDN3              ;RET, OR SET DRIVE

EVEXP2 CALL EVST2            ;UNSTACK STRING IF RUNNING
       SCF
       CCF                   ;NC="NAME"
       JR EVNMR              ;RET, OR EVNAME

;EVALUATE DRIVE NUMBER - WRITE AT *,T,S,A ALLOWED

EVDNM  CALL GCHR
       CP "*"
       JR NZ,EVDN2

EVDN1  CALL GTNC
       PUSH AF
       CALL GTDD             ;DEFAULT NUMBER
       LD A,(DRIVE)
       LD C,A
       POP AF                ;CHAR AFTER "*"
       CALL CFSO
       SCF                   ;"NUMBER"
       JR EVDN3

EVDN2  CALL EVNUM

EVDN3  RET  Z

       PUSH AF
       LD A,C
       CALL CODN             ;CONVERT DRIVE NUMBER IN A TO A
       CALL DRSET            ;SET DRIVE, CDIRT
       POP  AF
       RET

CODN   DEC A
       CP 7
       INC A
       JR NC,CODN2           ;JR IF NOT 1-7 - LEAVE ALONE

       PUSH BC
       PUSH HL
       LD C,A
       LD B,0
       LD HL,DRPT-1          ;DRIVE NUMBER PRETEND TABLE
       ADD HL,BC
       LD A,(HL)
       POP HL
       POP BC

CODN2  LD (DSTR1),A
       RET

HEVNAM LD BC,(HKBC)
       LD DE,(HKDE)
       SET 7,D
       RES 6,D               ;BUFFER ADDR ADJUST 4F10->8F10H
       XOR A
       LD B,A
       LD (SVC),A            ;PAGE 0
       JR EVNM2

REP8   CALL DERR
       DEFB 18               ;INVALID FILE NAME

EVNAMX CALL GTNC

;EVALUATE FILE NAME
;EXIT: Z/NZ FOR SYN/RUN, A=CHAR AFTER

EVNAM  CALL EVSTR

EVNMR  RET  Z                ;RET IF SYNTAX TIME

EVNM2  PUSH AF
       LD   A,C
       OR   B
       JR   Z,REP8           ;"INVALID FILE NAME" IF LEN 0

       LD HL,79
       SBC HL,BC
       JR C,REP8             ;ERROR IF TOO LONG

       IN   A,(251)
       PUSH AF
       PUSH BC               ;REAL LEN
       LD HL,14
       SBC  HL,BC
       JR NC,EVNM0

       LD BC,14

EVNM0  LD   HL,NSTR1
       LD   A,15

EVNM1  LD   (HL),20H
       INC  HL
       DEC  A
       JR   NZ,EVNM1

       LD   HL,NSTR1+1
       EX   DE,HL
       LD   A,(SVC)
       AND  1FH
       OUT  (251),A
       PUSH HL
       LDIR

       POP HL
       POP BC                ;REAL LEN
       LD A,C
       CALL NRWR
       DW 4F60H              ;STORE REAL LEN

       LD DE,4F10H
       CALL CMR
       DW 008FH              ;LDIR TO BUFFER IN SYS PAGE

       POP  AF
       OUT  (251),A

       POP  AF
       RET


EVNAM2X    CALL GTNC

;EVALUATE SECOND FILE NAME
;EXIT: Z/NZ FOR SYNTAX/RUN. A CORUPT

EVNAM2     CALL EXDATX
           CALL EVNAM

;SWOP LONG NAMES IN SYS PAGE BUFFER AS WELL AS OTHER DATA

EXDATX     CALL EXDAT
           IN A,(251)
           PUSH AF
           PUSH BC
           PUSH DE
           PUSH HL
           XOR A
           OUT (251),A
           LD BC,0058H
           LD HL,8F10H
           LD DE,8F68H
           CALL EXDT1
           POP HL
           POP DE
           POP BC
           POP AF
           OUT (251),A
           RET


EXDAT  PUSH AF
       PUSH BC
       PUSH DE
       PUSH HL
       LD HL,(CDIRT)
       LD A,H
       LD H,L
       LD L,A
       LD (CDIRT),HL         ;SWOP MAIN AND EXX DIRECTORIES
       LD   BC,28
       LD   DE,DSTR1
       LD   HL,DSTR2
       CALL EXDT1
       POP  HL
       POP  DE
       POP  BC
       POP  AF
       RET


BSWOP  CALL EXDAT
       LD A,(DTKSX)
       EX AF,AF'
       LD A,(DTKS)
       LD (DTKSX),A
       EX AF,AF'
       LD (DTKS),A           ;SWOP DTKS/DTKSX
       CALL BUDT             ;GET HL=RPT, DE=PAGED IN BUFFER, BC=0306H

;SWOP BC AT HL/DE

EXDT1  LD   A,(DE)
       EX   AF,AF'
       LD   A,(HL)
       EX   AF,AF'
       LD   (HL),A
       EX   AF,AF'
       LD   (DE),A
       INC  DE
       INC  HL
       DEC  BC
       LD   A,B
       OR   C
       JR NZ,EXDT1

       RET

;EVALUATE STRING EXPRESSION

EVSTR  CALL CMR
       DEFW EXPSTR

EVST2  CALL CFSO
       RET  Z

       PUSH AF
       CALL CMR
       DEFW GETSTR
       LD   (SVC),A
       POP  AF
       RET


;EVALUATE STREAM INFORMATION

EVSRM  CALL GTNC
EVSRMX CALL EVNUM
       RET  Z

       PUSH AF
       LD   A,C
       CP   17
       JR   NC,EVSRE

       LD   (SSTR1),A
       POP  AF
       RET

EVSRE  CALL DERR
       DB 21                 ;"INVALID STREAM NUMBER"

EVNUMX CALL GTNC

;EVALUATE NUMBER ROUTINE

EVNUM  CALL CMR
       DEFW EXPNUM

EVNU2  CALL CFSO
       RET  Z

CGTINT PUSH AF
       CALL CMR
       DEFW GETINT
       POP  AF
       RET


;EVALUATE BIG NUMBER ROUTINE

EVBNUM CALL CMR
       DEFW EXPNUM
       CALL CFSO
       RET  Z

       PUSH AF
       IN A,(251)
       PUSH AF
       IN A,(250)
       INC A
       CALL SELURPG          ;DOS AT 8000H TOO

       CALL CMR
       DW FPDT+FS
       POP AF
       OUT (251),A

       CALL CGTINT
       PUSH HL               ;X DIV 64K
       CALL CGTINT           ;BC=X MOD 64K
       POP  DE               ;DE=X DIV 64K
       POP  AF
       RET


FPDT   DB CALC               ;X
       DB DUP                ;X,X
       DB FIVELIT
       DB 91H,0,0,0,0        ;X,X,64K
       DB STO4               ;X,X,64K
       DB MOD                ;X,X MOD 64K
       DB SWOP               ;X MOD 64K,X
       DB RCL4               ;X MOD 64K,X,64K
       DB IDIV               ;X MOD 64K, X DIV 64K
       DB EXIT2

;EVALUATE ADDRESS ROUTINE

EVADDR CALL CMR
       DEFW EXPNUM
       CALL CFSO
       RET  Z

       CALL CMR
       DEFW UNSTLEN          ;AHL=PAGES/ ADDR MOD 16K
       SET 7,H               ;HL=OFFSET
       DEC A
       LD C,0
       INC C                 ;NZ
       RET

;CHECK FOR ALPHA CHAR

ALPHA  CP   41H
       CCF
       RET  NC
       CP   5BH
       RET  C
       CP   61H
       CCF
       RET  NC
       CP   7BH
       RET


;TRANSFER FILE NAMES IN COPY
;E.G. COPY "*" TO "*" (NSTR3) WITH "FROG" (NSTR1) LOADED GIVES "FROG" IN NSTR1
;    COPY "*" TO "?X.BIN" WITH "AB.BIN" LOADED GIVES "AX.BIN"

TRX    LD   HL,DIFA

;CALLED BY RENAME

TRX0   LD   DE,NSTR1
       LD   BC,15
       LDIR                  ;NSTR1=DATA FROM DISC

       LD   DE,NSTR1+1
       LD   HL,NSTR3+1
       LD   B,10

TRX1   LD   A,(HL)
       CP   "*"
       JR   Z,TRX3

       CP   "?"
       JR   Z,TRX2           ;LEAVE CHARS IN TGT OPPOSITE "?" ALONE

       LD   (DE),A           ;TGT=SRC UNLESS SRC="?"

TRX2   INC  HL
       INC  DE
       DJNZ TRX1

       RET

TRX3   INC  HL
       LD   A,(HL)
       CP   "."
       RET  NZ               ;RET IF SRC="*???" UNLESS "*."

TRX4   LD   A,(DE)
       CP   "."
       JR   Z,TRX2

       INC  DE
       DJNZ TRX4             ;LOOK FOR "." IN TGT, THEN MATCH EXTENSIONS

       RET

GDIFA  CALL NMMOV
       LD B,220
       CALL GRPNTB
       LD   BC,33
       LDIR

       LD   HL,DIFA
       LD   DE,UIFA
       LD   C,48
       LDIR

       LD B,13
       CALL GRPNTB
       LD   D,(HL)
       INC  HL
       LD   E,(HL)
       RET

