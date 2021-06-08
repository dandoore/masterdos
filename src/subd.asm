
;SUBD

;OPEN DIRECTORY - JUMPED TO FROM "OPEN" WHEN NEXT CHAR<>"#"

OPNDIR LD C,144              ;DIR
       CALL ISEP
       CALL EVSYN            ;NAME
       CALL PLNS             ;PLACE NEXT STAT ADDR
       CALL CEOS

       LD A,DFT              ;"DIRECTORY"
       LD (NSTR1),A
       CALL SETF4            ;"NO SECTOR NEEDED"
       CALL GOFSM
       LD A,(MAXT)           ;SET BY FDHR
       INC A                 ;TAG VALUE FOR THIS DIRECTORY'S FILES
       CP 0FFH
       JP Z,REP25            ;ERROR IF TOO MANY DIR FILES

       LD (FSA+DIRT),A       ;COPIED TO SECTOR BUFFER BY CFSM
       JP CFSM

;SET CURRENT DIRECTORY - JUMPED TO FROM "DIR" CMD WHEN "=" FOLLOWS IT

STDIR  CALL GTNC             ;SKIP "="
       CP 5EH                ;UPARROW
       JR Z,STDUQ

       CALL RTCK
       JR NZ,STDQ

STDUQ  EX DE,HL              ;START OF "STRING" TO DE
       CALL GTNC
       CALL CEOS

       LD BC,1               ;LEN
       IN A,(251)
       DB 0FEH               ;"JR+1"

;HOOK SET DIR. A=PAGE, BC=LEN, DE=START, OF NAME

HSDIR  EXX

       LD (SVC),A
       CALL EVNM2              ;EVAL AS NAME, AVOIDING STRING FETCH
       JR STDQC

STDQ   CALL EVNAM
       CALL CEOS

STDQC  LD HL,NSTR1+1
       CALL EVFILE
       CALL CKDISC
       CALL GPLA             ;GET PATH LEN ADDR IN HL
       LD A,(HL)
       LD (TEMPW1),A         ;SAVE PATH LEN TO ALLOW UP ARROW TRIM/NO TRIM

       LD HL,NSTR1+1
       LD A,(HL)
       CALL RTCK             ;CHECK IF ROOT
       JR NZ,STDR0

       CALL C11SP
       JR NZ,STDR0           ;JR IF NOT ROOT BY ITSELF
                             ;ELSE SET ROOT (NO DISC ACCESS)

;SET ROOT=PERM DIR. CALLED IF RAND WORD CHANGES, USED IF DIR=/

SETRT  LD BC,2               ;PATH LEN FOR "1:" OR "2:"
       JP STDR2              ;SELECT ROOT (B=0)

STDR0  CP 94                 ;UP-ARROW
       JR NZ,STDR1

       CALL C11SP
       JR NZ,STDR1           ;JR IF NOT UP-ARROW BY ITSELF

       CALL UPAR2            ;CHANGE CDIRT TO LEVEL OF PARENT DIR IF THERE
                             ;IS A PARENT DIR. SAVE TRIMED PATH LEN IN TEMPW1
       CALL GPLA
       LD A,(TEMPW1)
       LD (HL),A             ;TRIM LENGTH
       CALL GCDIA            ;GET CDIRP ADDR
       LD A,(CDIRT)
       LD (HL),A
       RET

STDR1  IN A,(251)
       PUSH AF
       CALL PRPTH            ;GET B=LEN, HL=START, OF PAGED-IN PATH STRING
       PUSH BC               ;LEN IN B
       PUSH HL
       CALL DTREE            ;DESCEND PATH TILL LAST FILE IN NSTR1+1
       CALL FNDIR            ;FIND DIRECTORY

       CALL GPLA
       POP DE
       POP BC                ;B=LEN

SDFCL  LD A,(DE)
       CALL RTCK
       LD A,2                ;PATH LEN IS 2 IF DIR NAME STARTS WITH ROOT.
                             ;LATER NAMES ADD TO THIS STUMP, E.G.
                             ;DIR="\GAMES\SCAB" SETS PATH$ TO "1:\GAMES\SCRAB"
                             ;WHATEVER CURRENT DIR IS.
                             ;OTHERWISE E.G. IF PATH$="1:\GAMES" AND DIR="SCRAB"
                             ;THEN PATH$ BECOMES "1:\GAMES\SCRAB"

       JR Z,SKIPF            ;JR IF ROOT

       LD A,(DE)
       CP 94                 ;UP-ARROW
       JR NZ,STDNR

       LD A,(TEMPW1)         ;LEN LEFT BY UPAR SR

SKIPF  LD (HL),A             ;SET LENGTH
       INC DE                ;SKIP LEADING ROOT OR ARROW SYMBOL. ROOT SYM IS
                             ;ADDED AUTOMATICALLY AS NAME IS ADDED TO PATH
       DJNZ SDFCL            ;DEC LEN

       CALL DERR
       DB 18

STDNR  PUSH BC
       CALL GPATD            ;GET HL=PATH START, BC=LEN
       ADD HL,BC
       EX DE,HL              ;PT DE TO END OF PATH DATA, HL TO NAME
       POP AF
       LD B,A
       INC B                 ;B=NAME CHARS TO COPY, PLUS 1 FOR DIVIDER
       LD A,(RTSYM)          ;DIVIDER SYMBOL
       JR SDN2

SDNL   LD A,(HL)
       INC HL

SDN2   EX AF,AF'
       INC C
       LD A,C
       CP MPL+1              ;MAX LEN+1
       JR NC,SDTL            ;JR IF NO ROOM LEFT (WOULD BE 33RD CHAR NEXT)

       EX AF,AF'
       CALL RTCK
       JR NZ,SDN3

       LD A,(RTSYM)          ;ALWAYS USE FIRST ALTERNATIVE DIVIDER IN PATH$

SDN3   LD (DE),A
       INC DE
       DJNZ SDNL             ;COPY NAME

       INC C

SDTL   DEC DE
       DEC C
       LD A,(DE)
       CP " "
       JR Z,SDTL             ;TRIM SPACES
                             ;C=NEW PATH LEN
       PUSH BC
       CALL POINT            ;START OF DIR ENTRY
       POP BC
       LD DE,DIRT            ;DISP TO TAG VALUE FOR FILES IN THIS DIR
       ADD HL,DE
       LD B,(HL)
       POP AF
       OUT (251),A

STDR2  CALL GCDIA            ;GET CDIRP ADDR
       LD (HL),B             ;SET CDIRP
       LD A,B
       LD (CDIRT),A          ;AND CDIRT
       CALL GPLA             ;GET PATH LEN ADDR
       LD (HL),C

OSRDPN CALL TIRD
       JP NC,SRDPN           ;STORE RD PATH NAME IF RAM DISC

       RET

;ROOT CHECK - Z IF ROOT SYMBOL

RTCK   PUSH HL
       LD HL,(RTSYM)         ;USUALLY "\" AND "/"
       CP H
       JR Z,RTCK2

       CP L

RTCK2  POP HL
       RET

;FIND DIRECTORY. EXIT WITH HL=POINT

FNDIR  CALL SNDFL
       JR NC,REP32H          ;ERROR IF DIRECTORY NOT FOUND

       LD HL,FLAG3
       RES 2,(HL)            ;"NOT INITIALISED" SO LATER USE OF FNDFL AND
                             ;SNDFL STARTS FROM T0/S1
       CALL POIDFT
       RET Z                 ;RET IF DIR TYPE

REP32H JP REP32              ;ERROR IF NOT RIGHT TYPE

EVFINS LD HL,NSTR1+1
       CALL EVFILE
       CALL CKDISC

;GET LAST NAME

GTLNM  IN A,(251)
       PUSH AF
       CALL PRPTH            ;GET B=LEN, HL=START, OF PAGED-IN PATH STRING
       CALL DTREE            ;DESCEND PATH TILL LAST FILE IN NSTR1+1
       POP AF
       OUT (251),A
       RET

;PREPARE PATH

PRPTH  XOR A
       OUT (251),A           ;PAGE IN STRING BUFFER

       LD A,(4F60H+FS)       ;REAL STRING LENGTH
       LD HL,(TEMPW3)        ;CHARS TRIMMED OFF FRONT - E.G. "D2:"
       SUB L
       JR C,PRP2             ;JR IF E.G. "" OR "D2" ALTERED TO "T:" OR "D2:"
                             ;SO TRIMMED CHARS > ORIG LEN
       JR Z,PRP2             ;JR IF ALL CHARS TRIMMED (E.G. "D2:")

       LD B,A                ;CORRECTED LEN
       LD DE,8F10H           ;SYS PAGE BUFFER
       ADD HL,DE             ;CORRECTED START
       RET                   ;LEN IN B, START IN HL

PRP2   LD HL,PRPD            ;USE "  " IF TAPE
       LD B,1
       LD A,(LSTR1)
       CP "T"
       RET Z

       INC HL                ;ELSE "*"
       RET

PRPD   DB " "
       DB "*"

;DESCEND PATH TREE
;ENTRY: HL PTS TO PATH STRING, B=LEN (<>)
;ACTION: PATH IS BROKEN INTO DIRECTORY NAMES AND EACH DIRECTORY IS SELECTED
;UNTIL A FINAL NAME IS RETURNED IN NSTR1+1

DTREE      CALL UPARW        ;CHECK IF FIRST CHAR IN NAME IS UP-ARROW,
                             ;HANDLE IF SO
           CALL RTCK
           JR NZ,DTREL

           INC HL            ;SKIP "\"
           DEC B
           JR Z,IFNE         ;E.G. LOAD "\" IS AN ERROR

           XOR A
           LD (CDIRT),A      ;ROOT IS TEMP DIRECTORY

DTREL      CALL EFLNM        ;GET FILE NAME
           RET C             ;RET IF LAST NAME

           PUSH HL
           PUSH BC
           CALL FNDIR        ;FIND DIRECTORY FILE
           LD BC,DIRT        ;DISP TO TAG VALUE FOR FILES IN THIS DIR
           ADD HL,BC
           LD A,(HL)
           LD (CDIRT),A
           POP BC
           POP HL
           JR DTREL

;EXTRACT FILE NAME FROM STRING
;
;ENTRY: HL POINTS TO POSN IN SRC STRING, B=SRC LEN LEFT
;EXIT: C=FILE NAME LEN,        NSTR1+1 HOLDS NAME
;      IF CY, NAME IS THE LAST ONE
;HL POINTS TO NEXT NAME START (ROOT SYMBOL IS SKIPPED), B IS UPDATED

;E.G. "UTILS\PDS\TEST" BROKEN DOWN TO: UTILS, PDS, TEST

EFLNM      LD DE,NSTR1+1
           PUSH DE
           LD A," "
           LD C,10

EFCL       LD (DE),A
           INC DE
           DEC C
           JR NZ,EFCL        ;INITIAL CLEAR OF NAME. C BECOMES 0

           POP DE

EFNLP      LD A,(HL)
           INC HL
           DEC B
           CALL RTCK
           RET Z             ;RET IF ROOT. NC

           INC B
           INC C             ;NAME LEN
           EX AF,AF'
           LD A,C
           CP 11
           JR Z,IFNE

           EX AF,AF'
           LD (DE),A
           INC DE
           DJNZ EFNLP

           SCF               ;"LAST NAME"
           RET

IFNE       CALL DERR
           DB 18             ;"INVALID FILE NAME"

;UP-ARROW MEANS "GO UP ONE LEVEL" - SEARCH PATH$ FOR LAST ROOT SYMBOL
;ENTRY: HL POINTS TO STRING, B=LEN
;EXIT: HL POINTS PAST UP-ARROW, B IS LESS

UPARW  LD A,(HL)
       CP 94
       RET NZ

       INC HL                ;SKIP LEADING UP-ARROW
       DEC B
       JR Z,IFNE             ;E.G. LOAD "UP-ARROW" IS AN ERROR

UPAR2  PUSH BC
       PUSH HL
       CALL GPATD            ;GET HL=PATH START, BC=LEN
       ADD HL,BC
       LD A,(RTSYM)

FLRSL  DEC HL
       DEC C
       JR Z,STDUF            ;JR IF NO ROOT FOUND - NO ACTION

       CP (HL)
       JR NZ,FLRSL

       LD A,C
       LD (TEMPW1),A         ;NEW LEN STORED FOR USE BY "DIR="
       PUSH HL
       CALL GPLA             ;PT HL TO LEN
       LD A,(HL)             ;OLD
       POP HL
       INC HL                ;POINT TO START OF TRIMMED DIR NAME
       LD DE,NSTR1+1
       SUB C
       DEC A                 ;LEN OF TRIMMED NAME
       CP 11
       JR C,STDTN            ;SHOULD BE JR ALWAYS....

       LD A,10

STDTN  LD C,A
       LD B,0
       LDIR
       LD C,A
       LD A,10
       SUB C
       JR Z,STDPP            ;JR IF NO PADS NEEDED

       EX DE,HL

SPDL   LD (HL),20H
       INC HL
       DEC A
       JR NZ,SPDL

STDPP  LD HL,CDIRT
       LD A,(HL)
       LD (HL),0FFH          ;"ANY"

STFPL  PUSH AF
       CALL FNDIR            ;FIND PARENT DIRECTORY
       CALL SETF2            ;"RESTART FROM CURRENT T/S"
       LD BC,DIRT
       ADD HL,BC
       POP AF                ;ORIG CDIRT
       CP (HL)
       JR NZ,STFPL           ;JR IF NAME CORRECT BUT NOT REAL PARENT ACCORDING
                             ;TO TAG
       LD BC,254-DIRT
       ADD HL,BC
       LD A,(HL)
       LD (CDIRT),A          ;MAKE CURRENT LEVEL SAME LEVEL AS PARENT=UP 1
       LD HL,FLAG3
       RES 2,(HL)            ;START FROM T0/S1
       CALL OSRDPN           ;STORE RD PATH NAME

STDUF  POP HL
       POP BC
       LD A,(HL)
       RET

;GET BC=PATH LEN, HL=PATH, FOR DRIVE

GPATD  CALL GPLA
       LD B,0
       LD C,(HL)             ;BC=PATH LEN
       CALL TIRD             ;A=DRIVE, NC IF RAM DISC
       JR NC,GPATD2

       LD HL,PTH1
       DEC A
       RET Z

       LD HL,PTH2
       RET

GPATD2 PUSH BC
       PUSH DE
       CALL MRDPN            ;CALLED WITH NC - FETCH NAME TO PTHRD
       LD HL,PTHRD
       POP DE
       POP BC
       RET


;RETURN TRACKS/DISC FOR CURRENT RAM DISC IN A

RTSTD      LD HL,RDDT-3      ;DRIVE 3 IS FIRST ENTRY
           CALL GPLA2        ;GET ADDR
           LD A,(HL)         ;T/DISK
           RET

;GET FIRST PAGE ADDR FOR RAM DISC

GFPA       LD HL,FIPT-3
           JR GPLA2

;GET RND WORD ADDR

GRWA   LD HL,CRWT-2
       LD A,(DRIVE)
       ADD A,A
       JR GPLA3

;GET CDIRP ADDR

GCDIA  LD HL,CDIT-1
       JR GPLA2

;GET PATH LEN ADDR

GPLA   LD HL,PLT-1

GPLA2  LD A,(DRIVE)

GPLA3  ADD A,L
       LD L,A
       RET NC

       INC H
       RET

;SET DIR TKS FROM SECTOR JUST READ. COMPARE RAND WORD ON DISC WITH CURRENT,
;ALTER DIR TO ROOT IF A NEW DISC, UPDATE CURRENT RAND WORD IN MEMORY

SDTKS  PUSH DE
       CALL POINT
       PUSH HL
       INC H
       DEC HL
       LD A,(HL)
       ADD A,4
       LD (DTKS),A
       DEC HL
       DEC HL
       LD C,(HL)
       DEC HL
       LD B,(HL)             ;BC=RND WORD FROM DISC
       CALL GRWA             ;GET CUR. RAND WORD ADDR IN HL
       LD A,C
       CP (HL)
       JR NZ,SDTK2           ;JR IF NEW DISC

       INC HL
       LD A,B
       CP (HL)               ;NZ IF NEW DISC
       DEC HL

SDTK2  LD (HL),C
       INC HL
       LD (HL),B
       POP HL                ;POINT VALUE
       PUSH HL
       PUSH BC
       PUSH HL
       CALL NZ,SETRT         ;SET ROOT IF NEW DISC

       POP HL                ;POINT VALUE
       LD BC,00D2H
       ADD HL,BC             ;DISC NAME ON DISC
       LD DE,DNAME
       LD C,10
       LDIR                  ;COPY NAME TO MSG BUFFER
       POP BC                ;CURRENT RND NO.

       LD A,(SAMDR)
       LD HL,DRIVE
       CP (HL)
       JR NZ,SDTK4

       LD A,(SAMCNT)
       AND A
       JR Z,SDTK4            ;JR IF NO OPEN FILES TO WARN ABOUT

       LD HL,(SAMRN)         ;SAM RND NO.
       SBC HL,BC
       JR Z,SDTK4            ;JR IF SAME DISC AS WHEN SAM OPENNED

       CALL CLSL
       CALL PMOOF            ;"OPEN file"
       CALL BEEP
       LD A,(SSTR1)
       CALL CMR
       DW STREAM

SDTK4  POP HL
       POP DE
       RET

