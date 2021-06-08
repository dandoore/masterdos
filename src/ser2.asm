
;SER2

;HKHL=ADDR OF MSB OF PTR IN STRMS (IN SECTION B)
;HKC=FILE NAME LEN
;HKDE=FILE NAME START (USUALLY IN BUFFER)
;CHAD PTS TO 0D/: OR IN OR OUT

HOPEN  CALL GCHR
       CALL OPSR
       CALL HEVSY
       LD HL,(HKHL)
       DEC HL
       LD BC,5C16H
       AND A
       SBC HL,BC
       LD A,L
       SRL A
       LD (SSTR1),A
       JR OPEX

OPSR   LD (FSTR1),A
       CP 0DH
       RET Z

       CP ":"
       RET Z

       CP 0FFH
       JR NZ,OPSR0

       CALL GTNC
       LD C,MIN              ;USE ZX "IN" CODE
       CP 60H                ;FF 60 IS "IN" CODE
       JR Z,OPSR1

       LD C,MRND             ;ZX "RND" CODE
       CP 3CH                ;FF 3C IS "RND" CODE
       JR Z,OPSR1

OPSR0  LD C,MOUT             ;ZX OUT CODE
       CP 0E0H               ;OUT TOKEN
       JP NZ,REP0

OPSR1  LD A,C
       LD (FSTR1),A
       JP GTNC


;OPEN # COMMAND SYNTAX ROUTINE

OPEN   CALL CFSO
       CALL Z,REMFP          ;REMOVE ANY FP FORMS IN SYNTAX TIME

       CALL GTNC
       CP "#"
       JP NZ,OPNDIR

       CALL EVSRM            ;SKIP, EVAL STREAM
       CALL SEPARX           ;INSIST ,/; (SKIPPED) OR QUOTE

       CALL EVSYN            ;NAME
       CALL OPSR             ;DEAL WITH CR/:/IN/OUT
       CALL PLNS             ;PLACE NEXT STAT ADDR
       CALL CEOS

OPEX   LD A,(SSTR1)
       CALL STRMD            ;BC=CURRENT DISP IN STRMS
       CP 4
       JR C,OPEN25           ;DON'T WORRY IF STREAMS 0-3 ALREADY OPEN

       LD HL,5*5+1           ;DISP OF LAST STANDARD CHANNEL
       AND A
       SBC HL,BC
       JP C,REP30            ;"STREAM USED" IF STREAM OPEN TO NON-STANDARD NOW

OPEN25 LD A,(LSTR1)
       AND 0DFH
       CP "D"
       JP NZ,REP0            ;"NONSENSE"

       CALL CKDRV
       LD A,10
       LD (NSTR1),A          ;FILE TYPE

;OPEN A STREAM TO 'D' CHANNEL

OPDST  LD A,(SSTR1)
       ADD A,A
       LD HL,5C16H+FS
       LD E,A
       LD D,0
       ADD HL,DE
       PUSH HL               ;ADDR OF PTR IN STRMS
       CALL OPEND
       POP DE
       RET C                 ;RET IF ERROR

   ;   BIT 0,(IX+MFLG)
    ;  JR Z,OPDST1           ;JR IF OPEN IN OR RND

    ;  CALL COMMP
    ;  IN A,(C)
    ;  BIT  6,A
    ;  JR Z,OPDST1      ;NEED TO WRITE TO GET THIS BIT TO WORK!

    ;  CALL RCLMX
     ; JP REP23              ;"DISC IS WRITE PROTECTED"

OPDST1 RES 7,(IX+4)          ;"D" NOT "D"+80H - PERM CHANNEL
       EX DE,HL
       LD (HL),E
       INC HL
       LD (HL),D             ;STRM PTR
       RET

;OPEN A  'D' DISC CHANNEL

OPEND  XOR A
       OUT (251),A
       LD IX,(CHANS)
       LD DE,6*5+FS
       ADD IX,DE             ;SKIP 6 STANDARD CHANNELS

OPND1  LD A,(IX+0)
       CP 0DH
       JR Z,OPND4            ;JR IF CHANS TERMINATOR FOUND - NEW CHANNEL

;FOUND AN OPEN CHANNEL

       LD A,(IX+4)
       AND 5FH
       CP "D"
       JR NZ,OPND3

       LD A,(DSTR1)
       CP (IX+MDRV)
       JR NZ,OPND3

;CHECK NAME OF CHANNEL

       PUSH IX
       POP HL
       LD DE,NAME
       ADD HL,DE
       EX DE,HL
       LD HL,NSTR1+1
       LD B,10

OPND2  LD A,(DE)
       XOR (HL)
       AND 0DFH
       JR NZ,OPND3

       INC HL
       INC DE
       DJNZ OPND2

       JP REP31              ;"CHANNEL USED"

;GET THE LENGTH OF CHANNEL

OPND3  LD E,(IX+9)
       LD D,(IX+10)
       ADD IX,DE
       JR OPND1

;IT IS A NEW CHANNEL - NOW TEST DIRECTORY FOR FILENAME

OPND4  PUSH IX
       LD A,10H
       CALL FDHR             ;Z IF FOUND
       POP HL                ;ADDR OF CHANS TERMINATOR - LOCN FOR NEW CHAN
       PUSH HL
       LD A,(FSTR1)
       JP NZ,OPND45          ;JR IF NOT FOUND

           PUSH AF
           CALL CRMCH        ;CREATE CHANNEL
           CALL POINT        ;HL=DIR ENTRY  (USES IX=DCHAN)
           POP AF

           POP IX            ;CHANS PTR
           PUSH HL           ;DIR ENTRY PTR
           LD C,4+0          ;BITS 1 AND 0 SHOW READ, BIT 2 SHOWS EXISTS
           CP MIN
           JR Z,OPND44

           CP MOUT
           JP Z,REP19        ;WRITING A READ FILE IF EXISTS AND OUT

           CP MRND
           JR NZ,OPND44      ;JR IF DEFAULT - IN

           LD C,4+2          ;BITS 1 AND 0 SHOW RND, BIT 2 SHOWS EXISTS

OPND44     LD A,C
           CALL RAMST
           LD DE,FFSA
           LD BC,0100H
           PUSH IX
           POP HL            ;CHANNEL
           ADD HL,DE
           EX DE,HL          ;DE=TYPE/NAME ETC DEST IN CHANNEL
           POP HL            ;DIR ENTRY
           PUSH HL           ;DIR ENTRY
           PUSH DE           ;TYPE IN CHANNEL
           LDIR              ;COPY 256 BYTES FROM DIR ENTRY TO CHANNEL
                             ;INCLUDES TYPE, NAME, ????, BAM, LENGTH
           POP DE            ;TYPE IN CHANNEL
           POP HL            ;DIR ENTRY
           LD BC,11
           LD A,(HL)
           LD (NSTR1),A
           LDIR

           INC HL
           INC HL
           LD D,(HL)
           INC HL
           LD E,(HL)
           PUSH DE           ;FIRST T/S
           XOR A
           LD (IX+CNTL),A
           LD (IX+CNTH),A    ;FILE'S SECTOR=0 TO START WITH
           CALL RSADM        ;INC SECTOR, LOAD AND MARK FIRST SECTOR WITH T/S
           CALL OPND8        ;GET STREAM OFFSET IN HL
           POP DE
           PUSH HL
           LD (IX+FTRK),D    ;TRK
           LD (IX+FSCT),E    ;SECT

           PUSH IX
           POP HL
           LD BC,FFSA+0F2H
           ADD HL,BC         ;PT TO EXECUTION ADDR OF FFFFFF UNLESS G+DOS
           LD BC,9           ;HDR LEN
           LD A,(IX+FFSA)    ;TYPE
           BIT 6,A           ;PROTECT BIT
           JR Z,OPNDUP

           LD (IX+MFLG),4    ;ENSURE PROTECTED FILES ARE READ-ONLY

OPNDUP     AND 1FH
           CP 0AH
           JR NZ,NOTF        ;JR IF NOT OPENTYPE

           LD A,(HL)
           INC A
           JR NZ,OOTF2       ;JR IF G+DOS OPEN-TYPE FILE

           LD C,A            ;C=0. NO HDR IN OPENTYPE FILE
           DEC A             ;A=FF SO NO JR

NOTF       CP 10H
           JR C,OOTF         ;JR IF ZX TYPE

           DEC HL            ;PT TO LEN DATA FOR ALL FILE TYPES
           LD D,(HL)
           DEC HL
           LD E,(HL)
           DEC HL
           LD A,(HL)
           EX DE,HL
           CALL AHLNX        ;GET 20-BIT LEN
           ADD HL,BC         ;HDR LEN OR ZERO
           ADC A,0
           LD B,A
           JR GT19B

OOTF       LD HL,0C000H
           LD B,L
           CP 5
           JR Z,GT19B        ;JR IF 48K SNAP

OOTF2      CALL PTLEN
           DEC HL
           LD D,(HL)
           DEC HL
           LD E,(HL)
           DEC HL
           DEC HL
           LD B,(HL)
           EX DE,HL          ;BHL=LEN IN ZX FORM

GT19B      CALL D510         ;GET HL=LEN MOD 510, HL'=DIV 510
           EX DE,HL
           CALL PTLEN        ;NC
           LD (HL),E
           INC HL
           LD (HL),D         ;LEN MOD 510
           INC HL
           EXX
           PUSH HL           ;LEN DIV 510 (LEN CNT)
           EXX
           POP DE
           LD (HL),E
           INC HL
           LD (HL),D         ;LEN DIV 510
           XOR A
           LD (NSTR1+1),A    ;NO NAME SO NO "OVERWRITE?"
           PUSH IX
           LD IX,DCHAN       ;ENSURE CLEARING HITS DCHAN, NOT CHANNEL
           CALL SETF4        ;"NO SECTOR NEEDED"
           CALL ROFSM        ;SET UP SAM, CLEAR, INC SAMCNT
           POP IX

;ENSURE EXTENDING RND FILE NEVER USES EARLIER SECTORS. LOOK BACKWARDS THRU
;SAM, FORCE TO FFS AFTER LAST USED SECTOR SEEN

           LD HL,SAM+195

CSL1       DEC L
           LD A,(HL)
           AND A             ;(NC)
           JR Z,CSL1         ;LOOK FOR LAST BYTE WITH ANY BIT (SECT) USED

           INC L

CSL2       DEC L
           LD (HL),0FFH
           JR NZ,CSL2

           POP HL            ;STREAM OFFSET
           AND A             ;NC=OK (NOT NEEDED...)
           RET


;FILE NOT FOUND
;OPEN A NEW "RND" FILE OR AN "OUT" FILE

OPND45     CP MIN
           JP Z,REP26        ;"FILE NOT FOUND" IF "IN" WITH NEW FILE

           PUSH AF
           CALL CRMCH
           POP AF
           POP IX

           CP MRND
           LD A,2
           JR Z,OPND6        ;JR IF RND (BITS 1-0 = 10) BIT 2=0 (NEW FILE)

           DEC A             ;BITS 1-0 SHOW OUT (01). OUT IS DEFAULT.

OPND6      CALL OPND7        ;SETS FTRK/FSCT, CNT=1
                             ;EXIT WITH HL=STRM PTR
           JP NC,SETLEN

           RET               ;C=ABORTED

OPND7  CALL RAMST            ;SET MFLG ETC
       CALL DDEL             ;DELAY IF NOT RAMDISC IN CASE 1ST SCT WRITTEN TOO
                             ;SOON

       CALL ROFSM            ;CREATE SAM FOR DRIVE, INIT AREA AT IX, INC SAMCNT.
                             ;AS SECTORS ARE WRITTEN, SAM ACCUMULATES USED SECTS
                             ;FOR DRIVE, FSAM DITTO FOR FILE. BUT IF 2 OUT FILES
                             ;OPEN TO DIFFERENT DRIVES, 1 SAM=DISASTER! EVEN 2
                             ;OUT FILES ON 1 DRIVE MUST ENSURE 2ND DOES NOT
                             ;CLEAR SAM OR BITS REFLECTING FSAM ARE LOST!

       JR NC,OPND75          ;JR IF NOT ABORT DUE "OVERWRITE" AND "N"

       LD BC,787
       PUSH IX
       POP HL
       CALL CMR
       DW JRECLAIM
       SCF                   ;"OPEND ABORTED"
       RET

OPND75 CALL BITF2
       JR Z,OPND8            ;JR IF NOT MOVE

       PUSH IX
       POP HL
       LD BC,239
       ADD HL,BC
       EX DE,HL              ;DEST=BYTE 220 IN FFSA (DEST DISC CHANNEL)

       LD HL,(NSTR2)
       LD B,FS/256
       ADD HL,BC             ;DITTO FOR SRC (IF DISC CHANNEL)
       LD BC,36
       LDIR                  ;COPY 220-255

;CALCULATE STREAM OFFSET

OPND8  PUSH IX
       POP HL
       LD DE,FS
       AND A
       SBC HL,DE
       LD DE,(CHANS)
       SBC HL,DE
       INC HL
       RET                   ;NC - OK


;CREATE A 'D'+80H CHANNEL AT HL

CRMCH  PUSH HL
       XOR A
       LD BC,787
       CALL CMR
       DEFW JMKRBIG          ;OPEN SPACE FOR CHANNEL

       POP DE
       LD HL,MTBLS
       LD BC,11
       LDIR                  ;O/P,I/P,D+80H,0,0,0,0,LEN
       EX DE,HL
       LD BC,787-11

;CLEAR THE NEW CHANNEL AREA

CRMC1  LD (HL),0
       INC HL
       DEC BC
       LD A,B
       OR C
       JR NZ,CRMC1

       RET

;DISC 'D' CHANNEL DATA

MTBLS  DEFW 4BA0H
       DEFW 4BA9H
       DEFB "D"+80H
       DW 0
       DW 0
       DW 787                ;CHAN LEN (IX+9/10)


;HOOK 151 - DISC BLOCK O/P BC FROM DE. FASTER THAN REPEATED USE OF RST 08H
;FOR SINGLE CHARS

HDBOP      LD DE,(HKDE)      ;SRC
           LD HL,(HKBC)      ;COUNT
           BIT 7,D
           JR NZ,DBOL        ;JR IF SRC IN SECT C

           SET 7,D
           RES 6,D           ;ELSE ADJUST SYS PAGE SRC (E.G. STR$) TO SECT C
           XOR A
           OUT (251),A       ;SYS PAGE IN SECT C (0013H RESETS ORIG PG AT END)

DBOL       LD A,H
           OR L
           RET Z

           DEC HL            ;DEC CHAR COUNT
           PUSH DE
           PUSH HL
           LD A,(DE)
           CALL MCHWR
           POP HL
           POP DE
           INC DE
           BIT 6,D
           CALL NZ,3FEBH     ;INCPAGDE

           JR DBOL

;CLOSE# STREAMS COMMAND

CLOSE  LD C,"*"
       CALL ISEPX            ;INSIST "*"
       CALL CIEL
       JR Z,CLOS1            ;JR IF CR/0D - CLOSE * IS CLOSE ALL

       CALL EVSRMX           ;EVAL STREAM NO.
       CALL CEOS

       LD A,(SSTR1)
       JR CLSRM

CLOS1  CALL CEOS
       JR CLRS1

;CLEAR# STREAMS COMMAND

CLEAR  LD C,"#"
       CALL ISEPX
       CALL CEOS

       CALL SETF1            ;"CLEAR#"

CLRS1  XOR A

CLRS2  PUSH AF
       CALL CLSRM
       POP AF
       INC A
       CP 16
       JR C,CLRS2

       CALL CLTEMP
       XOR A
       LD (SAMCNT),A
       LD (FLAG3),A
       RET

HCLOS  LD HL,(HKDE)
       LD BC,5C16H
       AND A
       SBC HL,BC
       LD A,L
       SRL A
       LD (SSTR1),A

;CLEAR STREAM AND CLOSE CHANNEL

CLSRM  CALL STRMD
       LD A,C
       OR B
       RET Z                 ;RET IF CLOSED ALREADY

;CLOSE THE STREAM

       LD (SVTRS),BC
       PUSH HL               ;PTR TO STRMS
       LD HL,(CHANS)
       DEC HL
       ADD HL,BC
       SET 7,H
       RES 6,H               ;PT TO CHANNEL SWITCHED IN AT 8000-BFFF
       EX (SP),HL            ;HL=PTR TO STRMS

       LD BC,0
       LD DE,-9C1EH
       EX DE,HL
       ADD HL,DE             ;ADD -9C1EH, STRMS PTR
       JR C,CLOSE1           ;JR IF STREAM >3

       LD BC,TABLE+8
       ADD HL,BC
       LD C,(HL)
       INC HL
       LD B,(HL)

CLOSE1 EX DE,HL
       SET 7,H
       RES 6,H               ;ADDR IN STREAMS
       LD (HL),C
       INC HL
       LD (HL),B
       POP IX                ;CHANNEL START
       LD A,B
       OR C
       RET NZ                ;RET IF STREAM 0-3 JUST CLOSED

;TEST FOR DISC 'D' CHANNEL

       LD A,(IX+4)
       AND 5FH
       CP "D"
       RET NZ


CLRCHD     LD A,(IX+MFLG)
           AND 03H
           JR NZ,CLRC2       ;JR IF ITS NOT AN "IN" FILE

           CALL DECSAM       ;(DECSAM DONE BY SDCM FOR RND/OUT)
           JR RCLAIM

DECSAM     LD   A,(SAMCNT)
           AND A
           RET Z             ;NO DEC BELOW ZERO!

           DEC  A
           LD   (SAMCNT),A
           RET

CLRC2      CALL BITF1
           CALL Z,SDCM       ;CALL IF NOT CLEAR #

;RECLAIM THE CHANNEL

RCLAIM CALL RCLMX

;CLOSE AND UPDATE STREAM DISP

       XOR A
       LD HL,5C16H+FS

RCLM1  LD (SVHL),HL
       LD E,(HL)
       INC HL
       LD D,(HL)             ;DISP OF A STREAM
       LD HL,(SVTRS)
       AND A
       SBC HL,DE
       JR NC,RCLM4           ;JR IF NO NEED TO ALTER IT

       EX DE,HL
       AND A
       SBC HL,BC
       EX DE,HL
       LD HL,(SVHL)
       LD (HL),E
       INC HL
       LD (HL),D             ;REDUCED DISP REPLACED
       DEC DE
       LD HL,(CHANS)
       ADD HL,DE
       LD DE,FS+4
       ADD HL,DE
       LD D,A
       LD A,(HL)
       AND 5FH
       CP "D"
       LD A,D
       JR NZ,RCLM4

       LD DE,BUFL-4
       ADD HL,DE
       LD E,(HL)
       INC HL
       LD D,(HL)             ;OLD MBUFF
       EX DE,HL
       SBC HL,BC
       EX DE,HL
       LD (HL),D
       DEC HL
       LD (HL),E             ;ADJUSTED MBUFF

RCLM4  LD HL,(SVHL)
       INC HL
       INC HL
       INC A
       CP 16
       JR C,RCLM1            ;LOOP FOR STREAMS 0-15

       RET

SDCM       CALL SSDRV
           XOR A
           LD (FSLOT),A      ;ENSURE NO USE OF FSLOT
                             ;DE=T/S
           CALL WRIF         ;WRITE CURRENT SECTOR IF IT HAS BEEN ALTERED
           CALL DECSAM
           BIT 5,(IX+MFLG)
           RET Z             ;RET IF FILE WAS NOT ALTERED - NO NEED TO ALTER
                             ;DIRECTORY ENTRY
           CALL GLEN         ;GET FILE LEN IN AHL, SECTS IN BC
           LD D,A
           LD A,(IX+FFSA)
           AND 1FH
           CP 10H
           LD A,D
           JR C,SDCM2        ;JR IF OPENTYPE AND ZX FILES

           LD DE,-9
           ADD HL,DE
           ADC A,0FFH        ;AHL=AHL-9 (HDR NOT INCLUDED IN LEN)

SDCM2      LD (IX+CNTL),C    ;OVER-WRITE PTR
           LD (IX+CNTH),B
           EX DE,HL
           CALL PTLEN
           DEC HL
           LD (HL),D         ;OLD FORMAT MID LEN - SO G+DOS CAN READ
                             ;(UNLESS FIRST DIR ENTRY - NAME USES AREA)
           DEC HL
           LD (HL),E         ;LOW
           DEC HL
           DEC HL
           LD (HL),A         ;HI - BYTE D2
           EX DE,HL
           CALL PAGEFORM
           EX DE,HL
           LD BC,1DH
           ADD HL,BC         ;PT TO BYTE EF
           LD (HL),A
           INC HL
           LD (HL),E
           INC HL
           LD (HL),D         ;NEW FORM OF LEN
           INC HL
           LD (HL),0FFH      ;"EXEC ADDR" OF FFXXXX

           PUSH IX
           BIT 2,(IX+MFLG)
           JP Z,CLOSX        ;JR IF FILE IS A NEW ONE - MAKE NEW DIR ENTRY

                             ;ELSE DEALING WITH AN ALTERED EXISTING FILE
           PUSH IX
           POP HL
           LD BC,FFSA        ;19
           ADD HL,BC
           LD DE,NSTR1
           LD C,11
           LDIR              ;COPY NAME TO NSTR1

           LD A,8
           CALL FDHR         ;LOOK FOR FILE ENTRY
           JP NZ,REP26       ;ERROR IF NOT FOUND

           JP NCF25          ;UPDATE DIRECTORY - OVERWRITING OLD ENTRY


TABLE  DB 1,0,1,0,6,0,16,0

;HOOK ROUTINE TO READ BYTE FROM DISC. USED BY "D" CHANNEL

MCHRD  CALL MCHIN
       RET C                 ;RET IF GOT CHAR

       RET Z                 ;RET IF NOT EOF

       CALL BITF2
       JP Z,REP27            ;"EOF" IF NOT A MOVE CMD

       OR 1                  ;NC,NZ - EOF
       RET


;DISC 'D' CHANNEL INPUT SUBROUTINE

MCHIN  IN A,(251)
       PUSH AF
       XOR A
       OUT (251),A
       PUSH IX               ;KEEP FPC HAPPY DURING INKEY$
       LD HL,TVFLAG+FS
       RES 3,(HL)            ;"NO NEED TO COPY LINE TO LS"- NEEDED?
       LD IX,(CURCHL)
       LD BC,FS
       ADD IX,BC
       BIT 0,(IX+MFLG)
       JP NZ,REP18           ;"READING A WRITE FILE" IF TYPE="OUT"

       CALL CPPTR
       JR NZ,MCHN1           ;JR IF NOT EOF

       ADD A,0DH             ;NC+NZ=EOF (WHEN PTR=LEN)
       JR MCHN2

MCHN1  CALL LBYT
       SCF                   ;"GOT KEY"

MCHN2  PUSH AF
       POP BC                ;AF IN BC FOR TRANSMISSION OUT
       POP IX
       EX AF,AF'
       POP AF
       OUT (251),A
       EX AF,AF'
       RET

;HOOK ROUTINE TO WRITE BYTE IN A TO DISC. USED BY "D" CHANNEL

MCHWR      LD D,A
           IN A,(251)
           PUSH AF
           XOR A
           OUT (251),A
           LD IX,(CURCHL)
           LD BC,FS
           ADD IX,BC
           LD A,(IX+MFLG)
           AND 03H
           JP Z,REP19            ;"WRITING A READ FILE" IF "IN"

           LD A,(CURCMD+FS)
           CP 0C6H           ;VALUE FOR "INPUT"
           JR Z,MCHW2        ;NO WRITE IF SO

           CALL CPPTR        ;Z IF PTR=LEN
           PUSH AF
           LD A,D
           CALL NSBYT        ;SAVE BYTE
           SET 3,(IX+MFLG)   ;"SECTOR WRITTEN TO"
           SET 5,(IX+MFLG)   ;"FILE WRITTEN TO"
           POP AF
           CALL Z,SETLEN     ;COPY PTR TO LEN IF WRITING TO EXTEND FILE

           LD A,(5C4BH+FS)   ;RESTORE BORDER COLOUR - QUICKLY
           OUT (ULA),A

MCHW2      POP AF
           OUT (251),A
           RET

SBCSR  CALL FNFS
       LD   (HL),D           ;T
       INC  HL
       LD   (HL),E           ;S COMPLETE BUFFER OF 512
       EX   DE,HL

;SWAP THE NEXT TRACK/SECTOR

SWPNSR CALL GTNSR
       LD   (IX+NSRH),H
       LD   (IX+NSRL),L
       RET


SBYT   PUSH BC
       PUSH HL
       PUSH AF
       CALL TFBF             ;HL=ADDR OF WRITE POINT
                             ;BC=RPT (DISP FROM BUFFER START TO WRITE POINT)
                             ;Z IF BC=510
       JR   NZ,SBT1

SBT2   PUSH DE
       CALL SBCSR            ;PLACE NEXT T/S IN IX+, GET DE=CURRENT T/S
       CALL WSAD             ;EXITS WITH HL POINTING TO BUFFER START, RPT
                             ;RESET
       JR SBT0

;NEW SAVE BYTE TO DISC - SERIAL FILES

NSBYT      PUSH BC
           PUSH HL
           PUSH AF
           CALL TFBF
           JR NZ,SBT1        ;JR IF BUFFER NOT FULL

           PUSH DE
           PUSH HL
           CALL CPPTR        ;CP PTR WITH FILE LEN
           POP HL
           JR NZ,SBT3        ;JR IF WE ARE NOT AT FILE END

           CALL SBCSR
           CALL SSDRV        ;SELECT DRIVE
           PUSH BC           ;PREV DRIVE
           CALL NWSAD        ;EXIT WITH HL AT BUFFER START
           POP BC
           LD A,C
           CALL SSDRV2       ;PREV
           PUSH HL
           LD D,H
           LD E,L
           INC DE
           LD BC,01FFH
           XOR A             ;Z
           LD (HL),A
           LDIR              ;BLANK NEW SECTOR
           POP HL

SBT3       CALL NZ,RDWRSR

SBT0       POP DE

SBT1       POP AF
           LD   (HL),A
           POP  HL
           POP  BC

;INCREMENT RAM POINTER

INCRPT INC  (IX+RPTL)
       RET  NZ

       INC  (IX+RPTH)
       RET

SETLEN     PUSH HL
           CALL PTLEN        ;NC
           LD A,(IX+RPTL)
           LD (HL),A
           INC HL
           LD A,(IX+RPTH)
           LD (HL),A
           INC HL
           LD A,(IX+CNTL)
           LD (HL),A
           INC HL
           LD A,(IX+CNTH)
           LD (HL),A
           POP HL
    ;      SET 4,(IX+MFLG)   ;"FILE HAS BEEN EXTENDED"
           RET

;CP PTR WITH LEN. Z IF MATCH (AND HL=LEN MSB OF 4)

CPPTR      CALL PTLEN
           LD A,(IX+RPTL)
           CP (HL)           ;HL PTS TO LEN LOW (LIKE RPT)
           RET NZ

           INC HL
           LD A,(IX+RPTH)
           CP (HL)
           RET NZ

           INC HL
           LD A,(IX+CNTL)
           CP (HL)
           RET NZ

           INC HL
           LD A,(IX+CNTH)
           SUB (HL)
           RET               ;A=0 IF EOF, Z FLAG


;LOAD BYTE FROM DISC

LBYT   PUSH BC
       PUSH DE
       PUSH HL
       CALL TFBF
       CALL Z,RDWRSR         ;CALL IF BUFFER FULL.
                             ;WRITE THIS SECTOR IF ALTERED, READ NEXT
LBT1   LD   A,(HL)
       POP  HL
       POP  DE
       POP  BC
       JR   INCRPT

;PTR/EOF SR
;EXIT: AHL=PTR VALUE, DE=CHANNEL START

PESR   CALL CMR
       DW GETINT
       INC H
       DEC H
       JR NZ,INVST

       CP 16
       JR NC,INVST

PESR2  CALL STRMD
       LD A,B
       OR C
       JR Z,SNOP             ;ERROR IF STREAM NOT OPEN

       DEC BC
       LD HL,(CHANS)
       ADD HL,BC
       LD BC,FS
       ADD HL,BC             ;CHANNEL START
       PUSH HL
       POP IX
       LD A,(IX+4)
       CP "D"
       JP NZ,REP10           ;"INVALID DEVICE"

FPTR   LD C,(IX+CNTL)
       LD B,(IX+CNTH)        ;PTR IN 510'S
       LD L,(IX+RPTL)
       LD H,(IX+RPTH)        ;PTR MOD 510
       JP M510

INVST  CALL DERR
       DB 21                 ;"INVALID STREAM NUMBER"

SNOP   CALL DERR
       DB 47                 ;"STREAM IS NOT OPEN"


;STREAM DISPLACEMENT
;ENTRY: A=STREAM NO.
;EXIT: A UNCHANGED, HL=ADDR IN STREAMS, BC=PTR VALUE FROM STREAMS

STRMD  PUSH AF
       ADD A,A
       LD C,A
       XOR A
       OUT (251),A
       LD B,A
       LD HL,5C16H+FS        ;STREAM ZERO
       ADD HL,BC
       LD C,(HL)
       INC HL
       LD B,(HL)             ;BC=CURRENT DISP IN STRMS
       DEC HL
       POP AF
       RET

;SET IX+MFLG, IX+BUFL/H USING A AND BC
;(SET TYPE=IN (0), OUT (1) OR RND (2), SET BUFFER LOCN=IX+BC)

RAMST  PUSH HL
       LD (IX+MFLG),A
       PUSH IX
       POP HL
       LD BC,WRRAM
       ADD HL,BC
       LD (IX+BUFL),L
       LD (IX+BUFH),H
       LD A,(DSTR1)
       LD (IX+MDRV),A
       POP HL
       RET

;POINT #S,X OR POINT #S,OVER X

POINTC     LD C,"#"
           CALL ISEPX
           CALL EVSRMX       ;EVAL STREAM
           CALL SEPARX       ;,/; (SKIPPED) OR QUOTE
           CP 0A6H           ;OVER
           JP Z,PTREC

           CALL EVBNUM       ;BC=X MOD 64K, DE=X DIV 64K
           CALL CEOS

           PUSH BC
           PUSH DE
           LD A,(SSTR1)
           CALL PESR2        ;PT IX TO CHANNEL, CHECK OPEN "D" IN
           CALL GLEN
           POP BC            ;X DIV 64K
           LD B,C
           POP DE
           SBC HL,DE
           SBC A,B
           JR C,REP27H

           EX DE,HL
           CALL D510
           PUSH HL
           EXX
           LD (IX+CNTL),L
           LD (IX+CNTH),H
           EXX

;FIND T/S OF DE'TH SECTOR IN A FILE, USING FSAM

;ENTRY: IX PTS TO FILE AREA, HL'=SECT (1=FIRST SECTOR)
;EXIT: D=TRACK, E=SECT, NZ, OR Z=NOT FOUND (NOT THAT MANY SECTORS IN FSAM)
;USES AF,BC,DE,HL, HL', D'

FITS       PUSH IX
           POP HL
           LD BC,FSAM
           ADD HL,BC         ;PT HL TO FSAM
           PUSH HL
           LD B,195

FTSL       LD A,(HL)
           INC HL
           AND A
           JR NZ,FTS2        ;JR IF MAP BYTE HAS ANY USED SECTORS IN IT

FTDB       DJNZ FTSL

REP27H     JP REP27          ;EOF IF RAN OUT OF MAP BITS

FTS2       LD E,8            ;8 BITS TO EXAMINE

FTSBL      RRA
           JR NC,FTS3        ;JR IF SECTOR NOT USED

           EXX
           DEC HL            ;DEC 'DESIRED SECTOR' COUNTER
           LD D,A
           LD A,H
           OR L
           LD A,D
           EXX
           JR Z,FTS4         ;JR IF GOT THE ONE WE WANTED

FTS3       DEC E
           JR NZ,FTSBL       ;LOOP FOR ALL BITS

           JR FTDB           ;NEXT BYTE IF ALL BITS DONE

FTS4       POP BC            ;FSAM START
           SCF
           SBC HL,BC         ;HL=MAP BYTE
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL         ;MAP SECTOR (DIV 8)
           LD D,0
           LD A,8
           SUB E
           LD E,A
           ADD HL,DE         ;D=0, E=BIT (7-0 FOR BIT 7-0 IN BYTE)
           LD D,03H          ;FIRST DATA TRACK=4

FTSSL      CALL FNS5         ;INC D, CHECK FOR END OF SIDE
           LD BC,-10
           ADD HL,BC
           JP C,FTSSL

           SBC HL,BC
           LD H,D
           INC L

           CALL GTNSR
           EX DE,HL
           AND A
           SBC HL,DE
           JR Z,PTLX5

           PUSH DE
           CALL WRIF
           POP DE
           CALL RSADM2

PTLX5      POP DE

SETRPT     LD (IX+RPTL),E
           LD (IX+RPTH),D
           RET

PTREC      CALL EVNUMX
           CALL CEOS

           PUSH BC
           LD A,(SSTR1)
           CALL PESR2        ;AHL=CURRENT PTR
           CALL GRPNT        ;HL=BUFF PTR, BC=DISP IN BUFFER (0-509)
           EX DE,HL
           LD HL,510
           AND A
           SBC HL,BC
           LD B,H
           LD C,L            ;BC=BYTES LEFT IN BUFFER
           EX DE,HL          ;HL=BUFF PTR
           POP DE            ;ITEMS TO SKIP
           LD A,D
           OR E
           RET Z

PTRCSL     LD A,B
           OR C
           JR Z,PTRSL        ;IF SECTOR SEARCHED, ENTER SECT LOOP
                             ;(NEVER ON FIRST PASS)
           LD A,(DELIM)
           CPIR
           JR NZ,PTRSL       ;JR IF DELIM NOT FOUND - SEARCH NEXT SECT

           DEC DE
           LD A,D
           OR E
           JR NZ,PTRCSL      ;LOOP UNTIL SKIPPED RIGHT NUMBER OF DELIMS
                             ;IF BC=0, USE NEXT SECT
           LD HL,510
           JR PTRCE

PTRSL      LD (TEMPW1),DE    ;DELIM COUNT
           XOR A
           LD (TEMPW2+1),A   ;"NOT FOUND"
           CALL WRIF
           CALL GTNSC
           CALL SRSAD        ;READ NEXT SECTOR, LOOKING FOR DELIM
           CALL MSECT        ;MARK SECT WITH CUR T/S
           CALL ICNT
           LD DE,(TEMPW3)    ;COUNTER
           PUSH HL           ;BUFFER START
           PUSH IX
           POP HL            ;CHANNEL START
           LD BC,NTRK
           ADD HL,BC         ;PT TO NTRK
           LD BC,(TEMPW2)    ;RECORD PTR - IF 00XX, NO MATCH
                             ;             IF NTRK OR LESS, REAL MATCH
                             ;             IF NSECT, SPURIOUS MATCH ON TRK
                             ;             IF NSECT+1, SPURIOUS MATCH ON SCT
           LD A,B
           AND A
           JR Z,PTRC3        ;JR IF DE NOT COUNTED DOWN YET, NO ADDR IN BC

           SBC HL,BC
           ADD HL,BC
           JR NC,PTRCOK      ;JR IF OK

PTRC3      POP AF            ;JUNK BUFFER START
           LD A,(DELIM)
           CP (HL)
           JR NZ,PTRC4

           INC DE            ;CORRECT COUNTER FOR SPURIOUS MATCH ON TRACK

PTRC4      INC HL
           CP (HL)
           JR NZ,PTRSL       ;JR IF SECTOR WASN'T A SPURIOUS MATCH

           INC DE            ;CORRECT COUNTER

           JR PTRSL

PTRCOK     POP BC            ;BUFFER START
           LD HL,(TEMPW2)    ;PTR WITHIN BUFFER

PTRCE      SBC HL,BC         ;RECORD PTR-BUFFER START (0001-01FEH)
                             ;(WHEN ENTRY AT PRCE, HL=510, BC=BYTES LEFT)
           EX DE,HL
           LD HL,510
           AND A
           SBC HL,DE
           JP NZ,SETRPT       ;JP IF RECORD IN THIS SECTOR

PTRC5      CALL GTNSC
           JR RSADM

;READ NEXT T/S FROM IX+NTRK

GTNSC      PUSH IX
           POP HL
           LD DE,NTRK
           ADD HL,DE
           LD D,(HL)
           INC HL
           LD E,(HL)         ;DE=NEXT T/S
           LD A,D
           OR E
           JP Z,REP27        ;EOF IF LAST SECTOR

           RET

;CALLED WHEN PTR HAS REACHED BUFFER END ON READ OR WRITE (NOT AT FILE END)
;WRITES CURRENT SECTOR IF IT HAS BEEN WRITTEN TO, BEFORE READING NEXT ONE

RDWRSR     LD D,(HL)         ;TRACK
           INC HL
           LD E,(HL)         ;SECT FROM END OF CURRENT BUFFER
           PUSH DE
           EX DE,HL
           CALL SWPNSR       ;SET NEW CUR. T/S FROM HL, GET DE=OLD CUR. T/S
           CALL WRIF2        ;IF SECTOR HAS BEEN WRITTEN TO, SAVE IT
           POP DE

RSADM      CALL ICNT

RSADM2     CALL SSDRV        ;SELECT DRIVE
           PUSH BC           ;PREV DRIVE
           CALL RSAD
           POP BC
           LD A,C
           CALL SSDRV2       ;PREV

MSECT      RES 3,(IX+MFLG)   ;"NOT WRITTEN TO YET"
                             ;MARK NSRL/H WITH DE

;SAVE THE NEXT TRACK/SECTOR

SVNSR  LD   (IX+NSRH),D
       LD   (IX+NSRL),E
       RET

;INC CNT (CURRENT SECTOR NUMBER)

ICNT       INC (IX+CNTL)
           RET NZ

           INC (IX+CNTH)
           RET

;ENTRY: BHL=24-BIT NUMBER
;EXIT: HL=NUMBER MOD 510, HL'=NUMBER DIV 510

D510       XOR A
           LD D,A            ;D MUST BE ZERO LATER
           EXX
           LD L,A
           LD H,A            ;SECTOR COUNT INITED
           EXX
           LD A,B
           LD BC,510         ;DBC=510

D510L      EXX
           INC HL
           EXX

           SBC HL,BC
           SBC A,D
           JR NC,D510L

           ADD HL,BC         ;HL=DISP IN SECTOR
           RET               ;HL=SECTOR

;GET FILE LEN IN AHL. NC FROM M510

GLEN       CALL PTLEN        ;HL PTS TO LEN LOW (LIKE RPT)
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC HL
           LD C,(HL)
           INC HL
           LD B,(HL)         ;BC=LEN DIV 510
           EX DE,HL          ;HL=LEN MOD 510

;LET AHL=HL+BC*510

M510       PUSH BC           ;NEEDED BY 'GLEN'
           XOR A
           LD DE,510
           JR PTAD2

PTADL      EX AF,AF'
           ADD HL,DE
           ADC A,0

PTAD2      EX AF,AF'
           DEC BC
           LD A,B
           OR C
           JR NZ,PTADL       ;LOOP UNTIL A'HL=PTR

           EX AF,AF'         ;NC
           POP BC
           RET

PTLEN      PUSH IX
           POP HL
           LD BC,LENL
           ADD HL,BC         ;HL PTS TO LEN LOW (LIKE RPT)
           RET


