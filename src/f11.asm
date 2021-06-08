1
;F11
;ZX ENTRY INTO RAM SPACE

CALL   LD C,0AAH      ;MODE
       CALL ISEPX
       CALL EVNUM
       CALL CEOS

       LD   A,C
       CP 2
       JP NC,IOOR

       LD   A,4
       OUT  (252),A          ;SCREEN IN PAGE 4, SPECTRUM MODE
       DEC C
       JP  Z,SNAP7           ;JP IF "CALL MODE 1"

;CALL MODE 0

       DEC A
       OUT  (251),A          ;PAGE 3 (SPECTRUM "ROM") AT 8000H
       DI
       JP   0B914H           ;JP TO "ROM"


FLTOFL CALL EVNAM
       LD C,8EH       ;TO
       CALL ISEP
       CALL EVNAM2
       CALL GCHR
       JR NMQU1

;EVAL NAME WITH "?" OPTION

NMQU   CALL EVNAM

NMQU1  CP "?"
       JR NZ,NMQU2

       CALL GTNC             ;SKIP "?"
       CALL SETF7

NMQU2  JP CEOS

;COPY/BACKUP SR

COBUS  CALL EVFINS
       CALL EXDATX           ;EXDAT AND EXX STRINGS AT 4F00/4F80H IN SYS PG
       CALL EVFINS
       CALL EXDAT
       LD   A,(DSTR1)
       LD   B,A
       LD   A,(DSTR2)
       CP   B
       RET NZ                ;RET IF 2 DISKS IN USE

       CP 3
       JP C,SETF5            ;SET FLAG 5 IF SINGLE-DISC COPY WITH 1 OR 2

       RET

;DISC COPY ROUTINE

COPY   CALL GTNC
       CP 0A6H        ;OVER
       JR NZ,COPYB

       CALL GTNC
       XOR A                 ;0="SAVE OVER"

COPYB  CALL NRWR
       DW OVERF

       CALL FLTOFL

       CALL COBUS            ;COPY/BACKUP SR
       CALL N2TN3            ;COPY NSTR2 TO NSTR3 TO ACT AS SAVED NAME
                             ;TEMPLATE

       CALL BUDT
       LDIR                  ;INIT BUFFER

COPY11 CALL CKDRV
       CALL BITF1
       LD HL,(TEMPW4)
       LD A,(TEMPB1)
       JR NZ,COYP3           ;JR IF STILL PART OF LAST FILE TO DO

       CALL REFBUF           ;REFRESH BUFFER
       LD HL,FLAG3
       RES 3,(HL)            ;"FIRST PASS - OFSM NEEDED"
       CALL SNDFX            ;FIND FILE - ABORT IF NOT FOUND
       JR C,COPY11           ;JR IF DIR TYPE

       CALL OHASR
       JR NZ,COPY11          ;JR IF "?" AND "N"

       LD B,11
       CALL GRPNTB
       LD B,(HL)
       INC HL
       LD C,(HL)
       PUSH BC               ;NO. OF SECTORS IN FILE
       XOR A                 ;Z SO "NOT ZX FILE" TO GTFL2
       CALL GTFL2
       CALL RSADSV
       POP BC
       LD HL,510
       CALL M510             ;GET AHL=HL+BC*510 (MAX FILE SIZE)
       CALL PAGEFORM
       RES 7,H               ;A=PAGES, HL=LEN MOD 16K

;ENTRY HERE IF 2ND OR LATER PASSES

COYP3  PUSH AF
       PUSH HL
       CALL PMOG         ;"LOADING "
       LD HL,DIFA+1
       CALL PFNM0
       CALL FFPG             ;GET IN B NO. OF PAGES IN BIGGEST FREE BLOCK
                             ;PAGE IN E
       INC B
       DEC B
       JP Z,REP35            ;ERROR IF NO FREE PAGES

       LD HL,FLAG3
       RES 1,(HL)            ;"CLOSE AFTER THIS"
       LD L,E                ;PAGE
       POP DE                ;LEN MOD 16K
       POP AF                ;REQUIRED PAGES, MINUS 1
       CP B                  ;FREE PAGES
       JR C,FCP1             ;JR IF WE CAN COMPLETE THE COPY IN THIS PASS

       SUB B
       LD (TEMPB1),A         ;PAGES LEFT TO DO
       LD (TEMPW4),DE        ;LEN MOD 16K LEFT
       CALL SETF1            ;"NO CLOSE"
       LD A,B
       LD DE,0               ;LEN MOD 16K=0

FCP1   LD (TEMPW1),DE        ;LEN MOD 16K TO DO (FOR SVBLK)
       LD H,A
       LD (TEMPW2),HL        ;PGES1/PAGE OF BUFFER
       CALL GCOP
       CALL LDBLK
       CALL TSPCE1
       CALL BSWOP
       CALL TRX
       CALL CKDRV
       CALL BITF3
       JR NZ,CYSV1           ;JR IF NOT FIRST PASS

       CALL OFSM
       JR C,COPY3            ;JR IF "OVERWRITE?" AND N

       CALL SETF3            ;"NO OFSM NEXT TIME"

CYSV1  CALL PMOH             ;"SAVING "
       LD HL,NSTR1+1
       CALL PFNM0
       CALL DWAIT            ;SPIN DISK IF STOPPED
       CALL GCOP
       LD DE,(TEMPW1)        ;LEN
       CALL HSVBK
       CALL BITF1
       JR NZ,COPY3           ;NO CLOSE IF PART STILL TO DO

       LD HL,STR-30
       LD DE,FSA+210
       LD BC,42
       LDIR

       CALL SCFSM

COPY3  CALL BSWOP
       CALL TSPCE2           ;MSG IF SINGLE DRIVE
       JP   COPY11

GCOP   LD HL,(TEMPW2)
       LD A,H
       LD (PGES1),A
       LD A,L
       OUT (251),A
       LD HL,8000H
       RET

;FIND FREE PAGES
;EXIT: SYS PAGE AT 8000H, B=SIZE OF MAX FREE BLOCK IN PAGES, E=PAGE NUMBER

FFPG   XOR A
       OUT (251),A
       LD HL,ALLOCT+FS+31
       LD B,0                ;BIGGEST BLOCK SO FAR=0 PAGES

FFM    INC L

FFPL   DEC L
       RET Z

       LD A,(HL)
       AND A
       JR NZ,FFPL            ;LOOP, LOOKING FOR A FREE PAGE

       LD C,0                ;CURRENT BLOCK=0 PAGES

CFPL   DEC L
       LD A,(HL)
       INC C
       AND A
       JR Z,CFPL             ;LOOP WHILE PAGES FREE, COUNTING

       LD A,B
       CP C
       JR NC,FFM             ;JR IF CURRENT BLOCK NO BIGGER THAN MAX

       LD B,C                ;NEW MAX SIZE
       LD E,L
       INC E                 ;POINT TO FIRST PAGE IN FREE BLOCK
                             ;=NEW START PAGE
       JR FFM

BUDT   CALL GETSCR
       LD HL,RPT
       LD DE,0B800H          ;ALLOWS 1580H BYTES FOR SECTOR LIST AT A280H
       LD BC,0306H           ;LEN OF RPT, BUF, NSR, FSA, DRAM
       RET

CLSL       PUSH IX           ;USED BY MODE 1/2 CLS
           CALL CMR
           DW CLSLOW
           POP IX
           RET

;CALL UP DIRECTORY

DIR    CALL FDFSR            ;CALL GTIXD, GTDEF, NULL NAME
       LD   A,2
       LD   (SSTR1),A        ;STREAM 2

       XOR A
       LD (DTFLG),A          ;"PRINT DATES" OFF
       CALL GTNC
       CP "="
       JP Z,STDIR

       CP   "#"
       JR   NZ,CAT0

       CALL EVSRM
       CALL SEPARX

CAT0   CP 249                ;"DATE" TOKEN
       JR NZ,CAT1

       LD (DTFLG),A          ;"PRINT DATES" ON (NZ)
       CALL GTNC             ;SKIP "DATE"
       CALL CIEL
       JR Z,CAT2             ;"DIR DATE" GIVES DETAILED, DATED DIR

CAT1   CALL ALLSR
       CALL CIEL
       JR   Z,CAT1a          ;JUST "DIR" GIVES SIMPLE DIR

       CALL EVEXP            ;EVAL DRIVE, OR FILE NAME
       JR NC,CAT11           ;JR IF IT WAS A NAME

       CALL SEPAR            ;,/;/"
       JR NZ,CAT12

       LD A,(DSTR1)
       PUSH AF               ;Z
       CALL EVNAM            ;EVAL NAME - ,/;/" FOLLOWED DRIVE NUMBER
       CALL NZ,EVFINS
       POP AF                ;Z
       LD (DSTR1),A          ;KEEP DRIVE NO SET BY NUMBER

CAT11  CALL NZ,EVFINS        ;PROCESS NAME IF RUN TIME

       CALL GCHR

CAT12  CP   "!"
       JR   NZ,CAT2          ;JR IF E.G. DIR 1,"NAME" OR DIR 1 - DETAILED

       CALL GTNC             ;SKIP "!" - E.G. DIR 1,"NAME"! OR DIR 1!-SIMPLE
       CALL ALLSR

CAT1a  CALL CEOS

       LD   A,2              ;SIMPLE DIR
       JR   PCAT

CAT2   CALL ALLSR
       CALL CEOS

       LD A,1                ;WINDOW
       CALL CMR
       DEFW JCLSBL           ;MAIN CLS

       LD   A,4              ;SINGLE COLUMN DETAILED DIR

;SETUP FOR DIRECTORY
;NSTR1+1,*
;SSTR1  ,2
;A =2 FOR SIMPLE, 4 FOR DETAILED DIR

PCAT   PUSH AF
       CALL CKDRV
       LD   A,(SSTR1)
       CALL CMR
       DEFW STREAM

       CALL ZDVS             ;ZERO VARS
       POP  AF               ;2 IF SIMPLE
       CP 2
       JR NZ,PCAT2

       CALL DITOB            ;DIR TO BUFFER
       CALL PDIRH            ;PRINT HEADER
       LD HL,(PTRSCR)
       LD DE,0A000H
       AND A
       SBC HL,DE             ;HL=TEXT LEN (1SL

       LD HL,0A000H          ;HL=START, DE=FILES
       LD A,(SRTFG)
       AND A
       CALL NZ,ORDER

       CALL COLB             ;NAMES/LINE IN B. COULD BE 1,2,3 ETC
       LD C,B

PCNML  PUSH BC               ;B=NAMES/LINE COUNTER, C=RELOAD
       CALL PFNM0            ;PRINT NAME, ADVANCE HL
       POP BC
       DEC DE
       LD A,D
       OR E
       JR Z,PCN3             ;EXIT IF ALL DONE

       LD A,20H
       DJNZ PCN2             ;JR AND USE SPACE UNLESS LAST NAME ON LINE

       LD A,0DH
       LD B,C                ;RELOAD COUNTER

PCN2   CALL PNT              ;CR OR SPACE
       JR PCNML

PCN3   CALL PUTSCR
       CALL PNCR
       JR PCN4

PCAT2  CALL FDHR             ;DO COMPLEX DIRECTORY

PCN4   CALL PMO3             ;"Number of Free K-Bytes = "
       CALL STATS            ;GET HL=FREE SECTS, DE=FREE SLOTS
       PUSH DE
       SRL  H
       RR   L                ;HALVE FREE SECTS
       XOR  A
       CALL PNUM4            ;FREE K
       CALL PNCR             ;CARRIAGE RETURN
       LD HL,(FCNT)
       PUSH HL
       XOR A
       CALL PNUM4            ;PRINT NO. OF FILES IN CURRENT DIR
       CALL PMOE             ;PRINT " File"
       POP HL
       CALL PLUR
       LD A,","
       CALL PNT
       CALL SPC
       POP HL
       PUSH HL
       XOR A
       CALL PNUM4            ;PRINT SLOTS FREE
       CALL PMOF             ;PRINT " Free Slot"
       POP HL
       CALL PLUR

PNCR   LD A,0DH

PTHP   JP PNT

PLUR   DEC HL
       LD A,H
       OR L
       RET Z

       LD A,"s"
       JR PTHP          ;MAKE IT PLURAL UNLESS ONLY 1 FILE

;PRINT DIRECTORY HEADER

PDIRH  CALL PNCR
       CALL PNDNM            ;PRINT DISC NAME
       CALL GPATD            ;GET BC=LEN, HL=START OF PATH DATA
       LD B,C
       CALL PBFHL            ;PRINT B FROM HL
       CALL PNCR
       JR PNCR

ZDVS   LD HL,0
       LD (CNT),HL           ;ZERO "SECTORS USED" COUNTER
       LD (TCNT),HL          ;ZERO "FILES ON DISC" COUNTER
       LD (FCNT),HL          ;ZERO "FILES IN DIR" COUNTER
       RET

DITOB  CALL GETSCR
       LD HL,0A000H
       LD (PTRSCR),HL        ;INIT BUFFER PTR
       LD A,2
       JP FDHR               ;SIMPLE DIR TO SCREEN BUFFER

;GET HL=FREE SECTS, DE=FREE SLOTS

STATS  CALL TSTD             ;TRACKS TO A
       LD C,A
       LD A,(DTKS)
       LD B,A
       CALL TIRD             ;CY IF NOT RAM DISC
       LD A,C
       BIT 7,A
       JR Z,TRK0             ;JR IF SINGLE SIDED

       JR C,TRKM1            ;JR IF NORMAL DISK

       SUB 48                ;E.G 130->82, 168->120
       DB 0FEH               ;"JR+1"

TRKM1  ADD A,A               ;*2 FOR DOUBLE-SIDED NORMAL. 208->160, 168->80

TRK0   SUB B                 ;NUMBER OF NON-DIRECTORY TRACKS
       LD HL,0
       LD B,10               ;10 SECTS/TRK
       LD D,H
       LD E,A

TRKL   ADD HL,DE             ;CALC SECTORS FOR DATA
       DJNZ TRKL

       LD A,(DTKS)
       CP 5
       JR C,TRK1

       INC HL                ;ALLOW FOR UNUSED T4/S1 GIVING EXTRA SPACE
                             ;IF DTKS 5 OR MORE
TRK1   LD   DE,(CNT)
       AND A
       SBC  HL,DE
       PUSH HL               ;FREE SECTS
       LD A,(DTKS)
       LD C,A
       ADD A,A               ;*2
       ADD A,A               ;*4
       ADD A,C               ;*5 (5-195)
       LD L,A
       LD H,0
       ADD HL,HL             ;*10
       LD A,C
       CP 5
       JR C,PCT2             ;JR IF 4-TRACK DIR

       DEC HL                ;ALLOW FOR 2 UNUSABLE T4/S1 ENTRIES

PCT2   ADD HL,HL             ;*20
       LD DE,(TCNT)          ;FILES ON DISC
       AND A
       SBC HL,DE             ;ALL SLOTS-FILES=FREE SLOTS
       EX DE,HL
       POP HL
       RET


;ENTRY: D=STRINGS TO SORT, HL=FIRST STRING

ORDER:        LD BC,10          ;NAME LEN
              LD A,C
              DB 0FEH           ;"JR+1"

;A=LEN TO SORT ON, BC=STRING LEN, DE=NO. OF STRINGS, HL=START

HORDER        EXX

              DEC A
              LD (SLNTG+1),A    ;LEN TO MATCH ON AFTER 1ST
              PUSH DE           ;FILES TO SORT
              PUSH HL           ;START

MAINSORTLP:   PUSH DE           ;FILES
              PUSH DE
              EXX
              POP BC            ;ELEMENTS TO SORT COUNTER
              INC C
              DEC C
              JR Z,NBBMP        ;EG COUNT 0100H LEFT ALONE

              INC B             ;COUNT 0106H->0206H SO MSB/LSB CNT OK

NBBMP         EXX
              PUSH HL           ;START OF CURRENT EL.

;ENTRY: BC"=STRINGS TO EXAMINE
;       HL=TOP OF LIST (CURRENT MAX), BC=STR. LEN

LDMAX:     LD D,H
           LD E,L

FETFIRST:  LD A,(DE)

ALORDLP:   ADD HL,BC
           EXX
           DEC C
           JR Z,SRTMB

SRTME      EXX

FIRSTCP:   CP (HL)
           JR C,ALORDLP      ;IF (HL) LOWER THAN MAX, NO NEW MAX - LOOP

           JR NZ,LDMAX

           PUSH HL
           PUSH DE           ;SAVE STARTS OF BOTH STRINGS

SLNTG      LD B,0            ;SELF-MOD. 9 CHARS LEFT TO MATCH ON, USUALLY

STRH1:     INC DE            ;MAX PTR - IN COMMON MEMORY
           INC HL
           LD A,(DE)
           CP (HL)           ;COMPARE EQUIVALENT LETTERS IN EACH STRING
           JR NZ,STRH4       ;EXIT IF MISMATCH FOUND

STRH3:     DJNZ STRH1        ;LOOP, MATCHING UP TO B' CHARS

STRH4:     POP DE
           POP HL            ;GET BACK STARTS OF BOTH STRINGS
           LD B,0
           JR C,FETFIRST     ;EITHER PICK UP OLD MAX FIRST LETTER

           JR LDMAX          ;OR RELOAD DE WITH NEW MAX PTR, AS NEEDED

SRTMB      DJNZ SRTME        ;DEC MSB

           EXX

;GOT MAX STRING

              POP HL            ;HL PTS CURRENT EL., DE TO MAX
              LD B,C

SORTMOVELP:   LD A,(DE)         ;SWOP TOP OF LIST (HL) WITH MAX (DE)
              EX AF,AF'
              LD A,(HL)
              LD (DE),A
              EX AF,AF'
              LD (HL),A
              INC HL
              INC DE
              DJNZ SORTMOVELP

              POP DE            ;ELEMENTS TO SORT COUNTER
              DEC DE
              LD A,D
              OR E
              JR NZ,MAINSORTLP

              POP HL
              POP DE
              RET

;GET IN B NUMBER OF COLUMNS FOR DIRECTORY

COLB   LD A,(DCOLS)
       LD B,A
       AND A
       RET NZ                ;RET IF FIXED NUMBER
                             ;ELSE DETERMINE ACCORDING TO WINDOW WIDTH
       CALL NRRDD
       DW 5A56H              ;WINDRHS/LHS IN C/B
       LD A,C
       SUB B
       INC A                 ;WINDOW WIDTH (NORMALLY 32, 64 OR 85)
       LD B,1
       SUB 11
       RET C

       INC A
       DEC B

COLBL  INC B                 ;COUNT NAMES THAT WILL FIT ACROSS CURRENT WINDOW
       SUB 11                ;NAME LEN, INC SPACE
       JR NC,COLBL

       RET

;OVER OPTION

OVERO  CALL GTNC
       CP   0A6H      ;OVER
       RET NZ

SF1S   CALL SETF1
       JR GTNCH

;RENAME/ERASE DIR OPTION

REDIR  CALL GTNC

REDI2  CP 144                ;"DIR"
       RET NZ

       CALL SETF4
       JR GTNCH              ;SKIP "DIR"

;ALLOW "?" TO MEAN "ALL FILES"

ALLSR  SUB "?"
       RET NZ

       DEC A
       LD (CDIRT),A          ;CDIRT=FF

GTNCH  JP GTNC               ;SKIP "?"

;ERASE A FILE

ERAZ   CALL OVERO
       CALL REDI2
       CALL NMQU

       CALL EVFINS

ERAZ3  CALL SNDFX
       JR Z,ERAZ33           ;JR IF FLAG=ERASE FILE, NOT DIR

       JR NC,ERAZ3           ;SKIP ERASE IF NOT DIR TYPE

       AND A                 ;NC

ERAZ33 JR C,ERAZ3            ;SKIP RENAME IF DIR TYPE

       LD A,(HL)
       CALL BITF1
       JR   NZ,ERAZ45        ;JR IF "ERASE OVER"

       BIT 6,A
       JR NZ,ERASP           ;JR IF PROTECTED

ERAZ45 CALL OHASR
       JR NZ,ERAZ3           ;JR IF "?" AND "N"

       CALL BITF4
       JR Z,ERAZ46           ;JR IF ERASE FILE, NOT DIR

       LD BC,DIRT
       ADD HL,BC
       LD A,(HL)
       LD (CDIRT+1),A        ;SET TEMP DIR TO SUBJECT OF ERASURE
                             ;(this is EXX version, moved to cdirt by EXDAT)
       CALL EXDAT            ;SAVE CURRENT FILE DETAILS
       LD L,"*"
       LD (NSTR1+1),HL       ;H CANNOT BE "." (2EH) SO NAME IS "*"+NOT "."
       LD A,10H              ;FIND FILE, IGNORE TYPE
       CALL FDHR             ;FIND ANY FILE IN THIS DIRECTORY
       JP Z,REP33            ;"DIRECTORY NOT EMPTY" IF A FILE FOUND

       CALL EXDAT            ;ORIG CDIRT AND NAME BACK
       CALL PTSVT            ;LOAD AND POINT TO DIR FILE ENTRY AGAIN

ERAZ46 LD (HL),0
       CALL WSAD
       JR   ERAZ3

ERASP  CALL BEEP
       CALL SETF3            ;"TRIED TO ERASE PROTECTED FILE"
       JR   ERAZ3

;OPTIONAL HANDLER FOR ERASE, PROTECT AND HIDE

OHAND  CALL OHASR
       RET NZ                ;RET IF "?" OPTION AND "N"

       LD   (HL),A           ;ERASED/PROTECTED/HIDDEN
       JR NWSADH

OHASR  CALL SETF0            ;"GOT ONE"
       CALL BITF7
       RET Z                 ;RET IF NOT "?" OPTION

       PUSH AF
       PUSH DE
       PUSH HL
       CALL OHNM             ;CMD NAME
       CALL FNMAE            ;FILE NAME, Y/N/A/E, KEY. Z IF YES OR ALL
       POP HL
       POP DE
       POP BC
       LD A,B
       RET                   ;Z IF YES

;Z IF DIRECTORY OK

CKDIR  INC H
       DEC HL
       DEC HL
       LD A,(CDIRT)
       CP 0FFH
       RET Z                 ;RET IF "MATCH ANY"

       CP (HL)
       RET

;RENAME A FILE

RENAM  CALL REDIR            ;SKIP ANY "DIR", SET FLAG4
       CP 142                ;"TO"
       JR NZ,RENM1

       CALL EVNAMX
       CALL CEOS

;RENAME DISK

       CALL EVFINS
       LD DE,0001H
       CALL RSAD
       CALL CLRRPT
       LD B,255
       CALL GRPNTB
       CALL FESE2            ;COPY NAME, NEW RND NO.

NWSADH JP WSAD

RENM1  CALL FLTOFL

       CALL COBUS
       CALL N2TN3            ;COPY 2ND NAME TO TEMPLATE AREA
       LD A,(DSTR1)          ;FIRST FILE DRIVE
       CALL CKDRX            ;SO E.G. RENAME "D2:ASD" TO "FROG" OK

RENM2  CALL REFBUF           ;REFRESH BUFFER
       CALL SNDFX            ;FIND 1ST NAME FILE - ABORT IF NOT FOUND
       JR Z,RENM3            ;JR IF FLAG= RENAME FILE, NOT DIR

       JR NC,RENM2           ;SKIP RENAME IF NOT DIR TYPE

       AND A                 ;NC

RENM3  JR C,RENM2            ;SKIP RENAME IF DIR TYPE

       CALL OHASR
       JR NZ,RENM2           ;JR IF "?" AND "N"

       PUSH HL
       CALL EXDAT            ;FIRST FILE NAME TO NSTR2
       POP HL
       CALL TRX0             ;COPY LOADED NAME TO NSTR1, USE TEMPLATE ON IT
       CALL FINDC
       JP   Z,REP28          ;"FILE NAME USED"

       CALL PTSVT            ;PT TO FIRST FILE ENTRY AGAIN
       PUSH DE               ;T/S
       PUSH HL
       INC H
       DEC HL
       DEC HL                ;SUB DIR TAG
       LD A,(CDIRT)          ;SUB DIR VALUE FOR 2ND NAME
       LD (HL),A             ;SET SUB DIR TAG
       POP DE
       INC  DE               ;NAME POSN
       LD   HL,NSTR1+1
       LD   BC,10
       LDIR
       POP  DE               ;T/S
       CALL WSAD
       CALL SETF0            ;"DONE ONE"
       CALL EXDAT
       JR RENM2

REFBUF CALL BITF2
       RET Z                 ;RET IF SNDFL NOT USED YET

RFB2   LD DE,(SVTRS)
       JP RSAD               ;ENSURE DRAM CONTAINS DIR ENTRIES IN CASE 2ND
                             ;ONE WANTED

;PT TO SVTRS ENTRY

PTSVT  CALL RFB2
       LD A,(SVDPT)
       LD (IX+RPTH),A
       JP POINT

;COPY NAME FROM 2 TO 3 TO ACT AS NAME TEMPLATE

N2TN3  LD   HL,NSTR2+1
       LD   DE,NSTR3+1
       LD   BC,14
       LDIR
       RET

;PROTECT ROUTINE

PROT   LD   A,40H
       DB 21H                ;"JR+2"

;HIDE FILE ROUTINE

HIDE   LD   A,80H

SFBT   LD   (HSTR1),A
       CALL GTNC
       CP   89H       ;OFF
       CALL Z,SF1S    ;SET F1, SKIP

       CALL NMQU

       CALL EVFINS

SFB2   CALL SNDFX
       LD   A,(HSTR1)
       LD   C,A
       CPL
       LD   B,A
       LD   A,(HL)
       AND  B
       CALL BITF1
       JR   NZ,SFB4

       OR   C

SFB4   CALL OHAND
       JR   SFB2

;FROM ERASE, HIDE, PROTECT

SNDFX  CALL SNDFL
       JR C,SNDTC            ;JR IF GOT ONE

       POP HL                ;JUNK RETURN
       CALL BITF0
       RET NZ                ;OK IF DONE AT LEAST 1 FILE

       CALL BITF3            ;SET BY ERASE. ALWAYS 0 FROM COPY
       JP NZ,REP36           ;"PROTECTED FILE"

REP26  CALL DERR             ;"FILE NOT FOUND"
       DB 107

;GET INFO FOR ERASE/RENAME
;RETURN CY IF DIR TYPE, Z IF FLAG4="DO FILES", NZ IF IT ="DO DIRS"

SNDTC  CALL POINT
       LD A,(HL)
       AND 1FH               ;TYPE
       SUB DFT
       CP 1                  ;CY IF DIR TYPE
       JP BITF4
       
