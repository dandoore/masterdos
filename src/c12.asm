
;C12

;FAST FIND NEXT SCT IF FNFS USED BEFORE

FFNS       LD DE,(FFDE)
           PUSH HL
           PUSH BC
           LD HL,(FFHL)
           JR FNS2

;FIND NEXT FREE SECTOR

FNFS   LD DE,0401H           ;TRK 4,SCT 1
       PUSH HL
       PUSH BC
       LD   HL,SAM-1

FNS1   INC HL

FNS2   LD   A,(HL)
       INC A
       JR   NZ,FNS3          ;JR IF NOT FF (=ALL USED)

       LD   A,E
       ADD  A,8
       LD   E,A
       SUB  11
       INC A                 ;EQUIV. OF SUB 10, BUT CY IF ZERO OR LESS
       JR   C,FNS1           ;JR IF SECTOR WAS OK BEFORE SUB 10

       LD   E,A
       CALL FNS5             ;NEXT TRACK
       JR   FNS1

FNS3   LD   B,1              ;MASK FOR BIT 0
       LD (FFDE),DE
       LD (FFHL),HL

FNS4   LD   A,(HL)
       AND  B
       JR   Z,FNS6           ;JR IF FND FREE

       CALL ISECT
       CALL Z,FNS5           ;NEXT TRK IF NEEDED

       RLC  B
       JR   FNS4

;CALLED BY POINT SR "FITS"

FNS5   INC  D
       CALL TSTD             ;GET TRK LIMIT
       LD C,A
       LD A,(DTKS)
       SUB 4
       JR NC,FNS55           ;JR IF NORMAL DISC

       NEG
       ADD A,C               ;FIDDLE LIMIT TO ALLOW E.G. TRK 42 ON 40 TRK, 1 DTK
       LD C,A                ;RAM DISC SO TKS 4-42 OK.

FNS55  LD A,C                ;LIMIT
       CP   D                ;CP LIMIT, TRK
       JP Z,REP24            ;NOT ENOUGH SPACE

       CALL TIRD
       JR C,FNS56            ;JR IF NOT RAM DISC

       LD A,50H              ;SIDE 1 ON RAM DISC=80 TRK
       DB 0FEH               ;"JR+1"

FNS56  LD A,C

       AND  7FH
       CP   D
       RET  NZ

       LD   D,80H
       RET

;FOUND FREE SECT

FNS6   LD A,(HL)
       OR   B
       LD   (HL),A           ;SET BIT IN SAM
       LD   C,L              ;L=DISP FROM START OF SAM,+0FH
       LD   A,B
       LD   B,0
       PUSH IX
       ADD  IX,BC
       OR   (IX+FSAM-0FH)
       LD   (IX+FSAM-0FH),A
       POP  IX
       CALL ICNT             ;INC SECTS USED COUNT
       POP  BC
       POP  HL
       RET


;TEST TRACKS ON DISC

TSTD   PUSH HL
       LD   HL,TRAKS1
       CALL TIRD
       JR C,TSD0

       CALL RTSTD           ;RAMDISC
       POP HL
       RET

TSD0   DEC A
       JR   Z,TSD1

       INC  HL
TSD1   LD   A,(HL)
       POP  HL
       RET


;PRINT FILE NAME

PFNME  LD B,1
       CALL GRPNTB

;PRINT FILE NAME FROM HL. USES HL,A,B
;ADVANCES HL BY 10

PFNM0  PUSH HL
       LD BC,0A0AH

PFNM1  LD A,(FNSEP)          ;FILE NAME SEPARATOR - USUALLY "."
       CP (HL)
       LD A,(HL)
       JR NZ,PFNM12

       LD A,B
       CP 5
       JR NC,PFNM15          ;PRINT PAD SPACE IF "." AND CHARS STILL TO PRINT
                             ;>=5
       LD A,(FNSEP)

PFNM12 INC HL
       CP   20H
       JR   NZ,PFNM2

PFNM15 LD   A,(CHDIR)

PFNM2  CALL PNT
       DJNZ PFNM1

       POP HL
       ADD HL,BC             ;BC=10
       RET

;FILE DIR HAND/ROUT.

FDHR   DI
       PUSH AF
       CALL GTIXD
       POP AF
       LD   (DCHAN+4),A
       XOR  A
       LD (FSLOT),A          ;NO FREE SLOT
       LD (MAXT),A           ;MAX TAG=0
       CALL REST
       CALL ORSAD
       CALL SDTKS            ;SET DIR TKS, CHECK RAND NO, GET HL=POINT
       PUSH HL
       BIT 2,(IX+4)
       CALL NZ,PDIRH         ;PRINT DIR HDR NOW THAT PATH$ CHECKED, IF COMPLEX
                             ;DIR WANTED

       LD A,(DTKS)
       SUB 5
       JR C,FDH05            ;JR IF SAM OK (4 DIR TRACKS OR LESS)

       INC A
       LD B,A                ;B=EXTRA DTKS (1-35)
       ADD A,A
       ADD A,A
       ADD A,B               ;A=5-175 (EXTRA DIR SECTS/2)
       LD C,A
       RRA
       RRA                   ;EXTRA DIR SECTS/8=BYTES
       AND 3FH               ;A=BYTES TO MARK FF IN SAM (1-43)
       LD B,A
       LD HL,SAM
       LD A,(HL)
       OR 0FEH
       LD  (HL),A            ;T4,S1 NOT RESERVED - BUT KEEP CURRENT STATUS!
       DEC B
       JR Z,FDH03

FDH02  INC HL
       LD (HL),0FFH
       DJNZ FDH02

FDH03  LD A,C
       ADD A,A
       AND 07H               ;EXTRA BITS (0,2,4 OR 6)
       JR Z,FDH05

       LD B,A
       XOR A

FDH04  SCF
       RLA
       DJNZ FDH04

       INC HL
       OR (HL)
       LD (HL),A

FDH05  POP HL
       JR FDH25


FDH1   CALL ORSAD
FDH2   CALL POINT

FDH25  LD   A,(HL)
       AND  A
       JP   Z,FDHF           ;JP IF FREE SLOT

;TEST FOR 'P' NUMBER

       LD A,(DCHAN+4)
       RRA
       JR  NC,FDH3           ;JR IF NOT E.G. LOAD 3

       CALL CONM             ;CONVERT T/S TO P NUMBER IN BC
       LD   A,(FSTR1)        ;LSB
       CP   C

FDHDH  JP NZ,FDHd

       LD A,(TEMPW4+1)       ;MSB
       CP B
       JR NZ,FDHDH

       RET

;GET TRACK AND SECTOR COUNT

FDH3   AND 03H
       JP   Z,FDH9           ;JP IF NEITHER TYPE OF DIRECTORY

       LD A,(HL)             ;TYPE
       EX AF,AF'
       LD B,H
       LD C,L
       INC H
       DEC HL
       DEC HL
       LD A,(HL)             ;DIRECTORY TAG - BYTE FE
       LD HL,11
       ADD HL,BC

       LD   B,(HL)
       INC  HL
       LD   C,(HL)
       LD   (SVBC),BC        ;SECTORS USED BY FILE
       LD   HL,(CNT)
       ADD  HL,BC
       LD   (CNT),HL         ;TOTAL USED SECTORS SO FAR ON DISC
       LD HL,(TCNT)
       INC HL
       LD (TCNT),HL          ;TOTAL FILES SO FAR ON DISC
       LD B,A
       LD A,(CDIRT)          ;CUR DIR
       CP 0FFH
       JR Z,FDH35            ;JR IF "ALL"

       CP B
       JP NZ,FDHd            ;JP IF NOT FILE FROM CURRENT DIR

FDH35  LD HL,(FCNT)
       INC HL
       LD (FCNT),HL          ;TOTAL FILES SO FAR IN THIS DIR

;TEST IF WE SHOULD PRINT NAME

       EX AF,AF'
       RLA                   ;BIT 7 OF FILE TYPE=1 IF "HIDDEN"
       JR   C,FDHH           ;JR IF HIDDEN

       LD A,(NSTR1+2)        ;FF IF NO NAME ASKED FOR
       INC A
       JR Z,FDH4

       CALL CKNAM
       JR   NZ,FDHH          ;JR IF NOT A DIR OF THIS NAME TYPE

FDH4   BIT  1,(IX+4)
       JR   Z,FDH5           ;JR IF COMPLEX DIR, CONTINUE IF SIMPLE

;COPY FILE NAME FROM SECT BUFFER TO SPECIAL BUFFER OF NAMES

       PUSH DE
       LD   A,(DCHAN+RPTH)   ;GET DIR ENTRY (0 OR 1)
       LD   B,A
       LD   C,1
       LD   HL,(DCHAN+BUFL)  ;GET SECT BUFFER ADDR IN HL
       ADD  HL,BC            ;PT TO FILE NAME IN DIR ENTRY
       LD   DE,(PTRSCR)
       LD   BC,10
       LDIR
       LD (PTRSCR),DE
       POP DE

FDHH   JP FDHd

FDH5   CALL POINT
       LD   A,(HL)
       BIT  6,A
       JR   Z,FDH4A          ;JR IF FILE NOT PROTECTED - PRINT FILE NUMBER
                             ;ELSE PRINT " * "
       CALL SPC
       LD   A,"*"
       CALL PNT
       CALL SPC
       JR   FDH4D            ;JR TO PRINT FILE NAME

;PRINT FILE NUMBER AS 2 OR 3 DIGITS E.G. "12 " OR "123"

FDH4A  CALL CONM             ;GET FILE NUMBER IN BC
       PUSH DE
       LD HL,99
       AND A
       SBC HL,BC
       LD H,B
       LD L,C
       LD A,20H
       JR C,FDH4B            ;JR IF >99

       CALL PNUM2
       CALL SPC
       AND A                 ;NC

FDH4B  CALL C,PNUM3

       POP  DE

FDH4D  CALL PFNME            ;PRINT FILE NAME FOR DIR 1/2

;PRINT SECTOR COUNT AS E.G. " 20 "

       PUSH DE
       CALL POIDFT
       JR Z,FDH85            ;JR IF DIR

       PUSH AF
       LD   A,20H
       LD   BC,(SVBC)        ;SECTORS USED
       LD HL,999
       AND A
       SBC HL,BC             ;CY IF >999 SECTS USED
       LD H,B
       LD L,C
       JR C,FDH84

       CALL PNUM3
       CALL SPC
       AND A

FDH84  CALL C,PNUM4          ;USE 4 DIGITS, BUTTING ONTO TYPE, IF NEEDED
                             ;E.G. 1234OPENTYPE

;PRINT TYPE OF FILE E.G. "BASIC ","C "

       POP AF

FDH85  CALL PNTYP
       POP  DE
       JR   FDHd

;TEST FOR SPECIFIC FILE NAME

FDH9   LD A,(DCHAN+4)
       AND 18H
       JR   Z,FDHd           ;JR IF NEITHER BIT 3 NOR 4 ARE SET

       LD A,(HL)
       AND 1FH
       CP DFT                ;DIR
       JR NZ,FDH92

       LD BC,DIRT
       ADD HL,BC             ;PT TO DIR TAG
       LD A,(MAXT)           ;CURRENT MAX
       CP (HL)               ;CP TAG OF THIS DIR
       JR NC,FDH91           ;JR IF NO NEW MAX TAG

       LD A,(HL)
       LD (MAXT),A
       AND A

FDH91  SBC HL,BC             ;PT TO DIR TAG FOR FILE

FDH92  INC H
       DEC HL
       DEC HL
       LD A,(CDIRT)
       CP (HL)
       JR Z,FDH95            ;JR IF RIGHT DIR

       INC A
       JR NZ,FDHd            ;JR UNLESS 0FFH "ANY" DIR

FDH95  CALL CKNAM
       RET  Z

;CALCULATE NEXT DIRECTORY ENTRY

FDHd   LD   A,(DCHAN+RPTH)
       DEC  A
       JR   Z,FDHe           ;JR IF WE HAVE JUST DONE SECOND DIR ENTRY IN SECT

       CALL CLRRPT
       INC  (IX+RPTH)        ;NEXT ENTRY
       JP   FDH2

FDHe   CALL ISECT            ;NEXT SECTOR
       JP   NZ,FDH1          ;JP IF NOT LAST SECT IN TRACK

       INC  D                ;INC TRACK
       LD A,D
       CP 4
       JR NZ,FDHE2

       INC E                 ;SECT=2 IF TRACK=4 (SKIP DOS)

FDHE2  LD   A,(DTKS)
       CP   D
       JP   NZ,FDH1

       AND  A                ;NZ
       RET

;FOUND FREE DIRECTORY SPACE - SEE IF THAT IS WHAT IS WANTED

FDHF   CALL FSLSR
       LD   A,(IX+4)
       CPL
       BIT  6,A
       RET  Z                ;RET IF GOT WHAT WE WANTED

       INC HL
       LD A,(HL)
       AND A                 ;**
       JR NZ,FDHd            ;JR IF DIR ENTRY HAS BEEN USED AT SOME TIME
                             ;(NAME START NOT CHR$ 0)

       INC A                 ;NZ
       RET                   ;USED PART OF DIR DEALT WITH

FSLSR  LD A,(FSLOT)
       AND A
       RET NZ                ;JR IF WE HAVE ALREADY NOTED A "FIRST FREE SLOT"

       LD (FSLOT),DE
       LD A,(DCHAN+RPTH)
       LD (FSLTE),A          ;RECORD 0 OR 1 FOR WHICH ENTRY IN SECTOR IS FREE
       RET

;CHECK FILE NAME IN DIR
;EXIT: Z IF MATCHED

CKNAM  PUSH DE
       CALL POINT
       LD   B,11
       LD   DE,NSTR1
       BIT  3,(IX+4)
       JR   Z,CKNM2          ;JR IF TYPE IRREL - SKIP TYPE

CKNM1  LD   A,(DE)
       CP   "*"
       JR   Z,CKNM5          ;MATCH ANY LEN IF "*"

       CP   "?"
       JR   Z,CKNM2          ;MATCH ANY CHAR IF "?"

       XOR  (HL)
       AND  0DFH
       JR   NZ,CKNM4

CKNM2  INC  DE
       INC  HL
       DJNZ CKNM1

CKNM3  XOR  A                ;MATCH=Z
CKNM4  POP  DE
       RET

CKNM5  INC  DE
       INC  HL
       LD   A,(DE)
       CP   "."
       JR   NZ,CKNM3         ;E.G. "*" OR "A*" MATCHES ANYTHING FROM "*" ON,
                             ;BUT E.G. "*.Z" LOOKS FOR MATCHING "." IN FILE
                             ;NAME
CKNM6  LD   A,(HL)
       CP   "."
       JR   Z,CKNM2          ;IF MATCHING "." FOUND, CHECK SUFFIX VS REQUESTED

       INC  HL
       DJNZ CKNM6

       POP DE
       RET                   ;NO MATCH

;CLEAR SAM

CLSAM  XOR A
       LD   HL,SAM
       LD   B,195

CLSML  LD   (HL),A
       INC  HL
       DJNZ CLSML

       RET

;RND/SERIAL OPEN FILE SECTOR ADDRESS MAP - INC SAMCNT OF LONG-TERM OPEN
;FILES (CHANS BUFFERS)

ROFSM  CALL OFSM
       LD   A,(SAMCNT)
       INC  A
       LD   (SAMCNT),A
       DEC A
       JR NZ,SOFS2           ;JR IF NOT FIRST FILE

       CALL GRWA             ;GET CUR. RAND WORD ADDR IN HL
       LD C,(HL)
       INC HL
       LD B,(HL)
       LD (SAMRN),BC         ;RND NO. OF DISC USED FOR SAM
       LD A,(DRIVE)
       LD (SAMDR),A

SOFS2  XOR  A                ;NC - OK
       RET

GOFSM  CALL GTIXD

;OPEN FILE SECTOR ADDRESS MAP

OFSM   PUSH IX
       LD A,(SAMCNT)
       AND A
       CALL Z,CLSAM          ;NO CLEAR IF OPEN-TYPE FILE(S) HAS SAM IN USE

       LD   A,30H            ;BIT 5=CREATE SAM, BIT 4=TEST FOR NAME
                             ;(BETTER THAN G+DOS BECAUSE EVEN IF DISC CHANGED
                             ;SECTORS USED IN EITHER DISC ARE SET BITS IN SAM.
                             ;G+DOS USED LD A,10H - NO ORING OF BITS)
       CALL FDHR
       JR   NZ,OFM4          ;JR IF NAME NOT FOUND

;FILE NAME ALREADY USED

OFM25  PUSH DE
       CALL BITF4
       JP NZ,REP28           ;"FILE NAME USED" IF "OPEN DIR"

       CALL POIDFT
       JP Z,REP28            ;OR IF DIR FILE

       CALL NRRD
       DEFW OVERF
       AND  A
       JR   Z,OFM3           ;NO "OVERWRITE? Y/N" IF SAVE OVER WANTED

       PUSH HL
       CALL SKSAFE           ;IN CASE "N"
       POP HL
       BIT 6,(HL)
       JP NZ,REP36           ;"PROTECTED FILE"

       CALL PMO5             ;"OVERWRITE"
       CALL FNM7K            ;FILE NAME, "Y/N", KEY
       JR   Z,OFM29          ;JR IF "Y"

       POP DE
       POP IX
       SCF                   ;SIGNAL ERROR
       RET                   ;AND ABORT

OFM29  CALL DWAIT            ;IN CASE STOPPED

OFM3   CALL DDEL
       CALL POINT
       LD BC,000FH
       LD   (HL),B           ;MARK ENTRY IN BUFFER AS "DELETED"
       ADD HL,BC             ;POINT TO SAM OF FILE IN BUFFER
       LD DE,SAM             ;POINT TO BAM FOR DISC SO FAR
       LD B,195

DBAML  LD A,(DE)
       XOR (HL)              ;REVERSE BITS ORED IN BY FDHR
       LD (DE),A             ;UPDATE BAM TO TAKE ACCOUNT OF ERASURE
       INC E
       INC HL
       DJNZ DBAML

       POP DE
       CALL FSLSR
       CALL WSAD             ;WRITE DIR SECT TO DISC
       LD IX,DCHAN
       CALL FDH1             ;COMPLETE BAM BY SCANNING REST OF DIRECTORY
       JR Z,OFM25            ;JR IF SECOND OR OTHER COPY... SHOULD NEVER BE!

;CLEAR AREA FOR NEW DIR ENTRY IN BUFFER

OFM4   POP  IX
       PUSH IX
       LD   B,0

OFM5   LD   (IX+FFSA),0
       INC  IX
       DJNZ OFM5

       POP  IX
       PUSH IX
       LD   HL,NSTR1
       LD   B,11
       CALL OFM6
       POP  IX
       PUSH IX
       LD   BC,220
       ADD  IX,BC
       LD   HL,UIFA+15
       LD   B,48-15
       CALL OFM6
       POP  IX

       CALL BITF4
       CALL Z,FNFS           ;AVOID FNFS IF EXISTING OPENTYPE FILE, OR
                             ;IF "OPEN DIR"
       CALL SVNSR
       LD   (IX+FTRK),D      ;FIRST TRACK
       LD   (IX+FSCT),E      ;AND SECTOR OF FILE IN DIR
       XOR  A                ;NC=OK
       JR CLRRPT

;GET BUFFER ADDRESS

GTIXD  LD   IX,DCHAN
       LD   HL,DRAM
       LD   (BUF),HL

;CLEAR RAM POINTER

CLRRPT LD   (IX+RPTL),0
       LD   (IX+RPTH),0
       RET

OFM6   LD   A,(HL)
       LD   (IX+FFSA),A
       INC  HL
       INC  IX
       DJNZ OFM6
       RET


BEEP   PUSH HL
       PUSH DE
       PUSH BC
       PUSH IX
       LD   HL,036AH
       LD   DE,(BEEPT)
       CALL CMR
       DEFW BEEPR
       POP  IX
       POP  BC
       POP  DE
       POP  HL
       RET

;SPECIAL CFSM USED WHEN SEVERAL BLOCKS MAY BE WRITTEN TO SAME FILE AND LAST
;SECTOR NEEDS SAVING BECAUSE HSVBL USED.

SCFSM  CALL GTNSR
       CALL WSAD             ;LAST SECTOR

;CLOSE FILE SECTOR MAP

CFSM   PUSH IX
       LD DE,(FSLOT)
       INC E
       DEC E
       JR Z,CLOSX            ;DIR PROBABLY FULL - BUT DO A FULL CHECK IN
                             ;CASE WE ARE CLOSING AN "OUT" FILE (FSLOT
                             ;HAS BEEN SET TO ZERO BECAUSE IT MIGHT HAVE BEEN
                             ;OBSOLETE IF SAVE USED SINCE OPEN)

       CALL RSAD             ;READ DIR SECTOR WITH FREE SLOT
       LD A,(FSLTE)
       LD (DCHAN+RPTH),A     ;PT RPT TO FREE SLOT
       JR NCF25

;JPED TO FROM CLOSP

CLOSX  LD A,40H          ;FIND FREE SLOT
       CALL FDHR
       JP NZ,REP25       ;DIRECTORY FULL

;UPDATE DIRECTORY
;JPED TO FROM CLOSP

NCF25  CALL POINT
       LD   (SVIX),IX
       POP  IX
       PUSH IX

       LD A,E
       DEC A
       OR D
       OR B                  ;OFFSET FROM 'POINT'
       LD B,0                ;B=COUNT FOR 256 BYTES
       JR NZ,CFM3            ;JR IF NOT T0,S1,ENTRY1

       LD BC,0D20AH          ;C=0AH
       CALL CFMC             ;COPY BYTES 0-D1 FROM BUFFER
       ADD HL,BC             ;SKIP DISC NAME - KEEP AS READ FROM DISC
       ADD IX,BC             ;(BC=000AH)
       LD B,20H
       CALL CFMC             ;COPY BYTES DC-FB FROM BUFFER
                             ;ONLY COPY UP TO FBH ON 1ST DIR ENTRY
                             ;KEEP RAND WORD, DIR TAG AND 'EXTRA
                             ;DIR TRACKS' BYTE AS READ FROM DISC

       LD A,(IX+FFSA+2)      ;READ DIR TAG
       INC HL
       INC HL
       LD (HL),A             ;USE DIR TAG FROM BUFFER, NOT DISC
       CP A                  ;Z

CFM3   CALL NZ,CFMC

       LD   IX,(SVIX)
       CALL DATSET           ;COPY DATE/TIME TO +F2 TO +FB
       CALL POINT
       INC H
       DEC HL
       DEC HL
       LD A,(CDIRT)
       LD (HL),A             ;TAG FILE WITH DIRECTORY CODE
       CALL WSAD
       CALL SKSAFE
       POP  IX
       RET

CFMC   LD   A,(IX+FFSA)
       LD   (HL),A
       INC  IX
       INC  HL
       DJNZ CFMC

       RET

;GET A FILE FROM DISC

GTFLE  LD   A,(FSTR1)

;TEST FOR FILE NUMBER

       INC A
       JR   Z,GTFL3          ;JR IF NOT 'LOAD N' (FSTR1=FF)

       LD   A,1              ;"NUMBERED FILE"
       CALL FDHR             ;FIND FILE
       JP   NZ,REP26         ;ERROR IF DOESNT EXIST

GTFLX  CALL GTFSR            ;FIDDLE ZX TYPE CODES
       CP 21

       JP NC,REP13           ;ERROR IF DIR OR OPENTYPE OR SNP 128K

       SUB 12H
       ADC A,0
       JP Z,REP13            ;ERROR IF TYPE 11H OR 12H (ARRAYS)

       LD   DE,NSTR1
       LD   BC,11
       LDIR
       LD   B,4
       XOR A
       CALL LCNTB

       CALL B211

       CALL POINT
       LD   DE,UIFA
       LD   BC,11
       LDIR

       LD   B,15
       CALL LCNTA

       LD   B,48-26
       LD   A,0FFH
       CALL LCNTB
       JR   GTFL4

GTFL3  LD   A,10H            ;NAMED FILE, IGNORE TYPE
       CALL FDHR
       JP   NZ,REP26

GTFL4  CALL GTFSR
       LD A,(NSTR1)
       LD C,A
       SUB 20
       ADC A,0
       JR NZ,GTFL5A          ;JR IF DESIRED TYPE <> CODE OR SCREEN$

       LD A,(HL)
       SUB 20
       ADC A,0
       JR NZ,GTFL5A          ;JR IF LOADED TYPE <>CODE/SCREEN$

       LD (HL),C             ;LOADED TYPE=DESIRED TYPE

GTFL5A LD   A,C
       CP   (HL)
       JP   NZ,REP13         ;WRONG FILE TYPE IF NO MATCH

       CALL BITF7

;CALLED BY COPY - Z SET (F7 USED FOR OTHER THINGS)

GTFL2  PUSH AF               ;"FLAG 7" STATUS - NZ IF ZX
       LD   DE,HD002
       CALL B211             ;211-219 ZX FILE DATA TO HD002

       DEC HL
       LD   DE,STR-30
       LD   C,42
       LDIR                  ;210-251 WITH SNP REGS AT STR-20

       CALL NMMOV
       POP AF
       JR   Z,GTFL7          ;JR IF NOT ZX FILE

       LD   B,11
       CALL LCNTA

       LD   B,48-26
       LD   A,0FFH
       CALL LCNTB

       LD   HL,(HD0B2)       ;LEN
       XOR  A
       CALL PAGEFORM
       RES 7,H
       LD   (DIFA+34),A
       LD   (PGES2),A
       LD   (DIFA+35),HL
       LD   (HD0B2),HL

       LD   HL,(HD0D2)       ;ADDR
       XOR  A
       CALL PAGEFORM
       DEC  A
       AND  1FH
       LD   (DIFA+31),A
       LD   (PAGE2),A
       LD   (DIFA+32),HL
       LD   (HD0D2),HL
       JR   GTFL8

GTFL7  LD B,220
       CALL GRPNTB
       LD   BC,48-15
       LDIR

GTFL8  LD B,13
       CALL GRPNTB
       LD   D,(HL)
       INC  HL
       LD   E,(HL)
       LD   (SVDE),DE
       RET

NMMOV  CALL POINT
       LD   DE,DIFA
       LD   BC,11
       LDIR                  ;TYPE/NAME

       LD   B,4

;LOAD (DE) WITH COUNT B

LCNTA  LD   A,20H

LCNTB  LD   (DE),A
       INC  DE
       DJNZ LCNTB

       RET

GTFSR  CALL POIDFT
       CP 07H
       JR NZ,GTFS1

       LD A,04H

GTFS1  CP   10H
       JR   NC,GTFS2         ;JR IF SAM FILE

       DEC  A
       OR   10H
       CALL SETF7            ;"ZX FILE"

GTFS2  LD   (HL),A
       RET


;READ SECTOR SR

RSSR   CALL CTAS
       DI
       LD   C,DRSEC
       CALL SADC

GTBUF  LD   L,(IX+BUFL)
       LD   H,(IX+BUFH)
       RET

 
;FIND A FILE ROUTINE

FINDC  LD   A,10H
       CALL FDHR

;GET RAM AND POINTER TO BUFFER

POINT  LD B,0

GRPNTB LD (IX+RPTL),B

GRPNT  LD   L,(IX+BUFL)
       LD   H,(IX+BUFH)
       LD   B,(IX+RPTH)
       LD   C,(IX+RPTL)
       ADD  HL,BC
       RET


;GET THE NEXT TRACK/SECTOR

GTNSR  LD   D,(IX+NSRH)
       LD   E,(IX+NSRL)
       RET

POIDFT     CALL POINT
           LD A,(HL)
           AND 1FH
           CP DFT            ;DIR
           RET

B211   LD B,211
       CALL GRPNTB
       LD   BC,9
       PUSH HL
       LDIR
       POP HL
       RET
