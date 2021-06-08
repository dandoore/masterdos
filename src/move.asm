
;MOVE
;MOVE COMMAND

MOVE   CALL EVMOV
       CP 142                ;"TO"
       JP NZ,REP0            ;"NONSENSE"

       CALL EXDAT
       CALL EVMOV
       CALL EXDAT
       CALL CEOS

       CALL SETF2            ;"MOVE"
       XOR A
       OUT (251),A

       LD A,MIN
       LD (FSTR1),A
       CALL OPMOV            ;OPEN "IN" TEMP CHANNEL - GET NSTR1=ADDR
       JP C,SNOP             ;IF OPMOV RETURNS C WHEN CREATING "IN", IT
                             ;WAS DUE TO A NON-OPEN STREAM BEING USED

       LD A,(FSTR1)          ;DRIVE NO. FOR 2ND CHANNEL IS SAME AS FIRST??
       LD (NSTR2),A
       CALL EXDAT
       LD A,MOUT
       LD (FSTR1),A
       LD IX,DCHAN
       LD HL,FLAG3
       RES 4,(HL)            ;SO OFSM NORMAL (PREVIOUS OPMOV MIGHT HAVE SET IT)
       CALL OPMOV            ;OPEN "OUT" TEMP CHANNEL
       JR NC,MOVA            ;JR IF OK
                             ;IF "OVERWRITE?" AND "N", OR STREAM NOT OPEN, CONT

       PUSH AF               ;Z IF STRM NOT OPEN
       LD IX,(NSTR2)         ;ADDR OF FIRST CHANNEL (IN NSTR1 AFTER EXDAT)
       LD BC,FS
       ADD IX,BC
       LD A,(IX+4)
       CP "D"+80H
       JR NZ,MVNRC           ;ONLY RECLAIM FIRST CHANNEL IF IT IS TEMP "D"

       CALL DECSAM
       CALL RCLMX

MVNRC  POP AF
       JP Z,SNOP             ;STREAM NOT OPEN ERROR

       RET


;RECLAIM A CHANNEL PTED TO BY IX IN SECT C

RCLMX  LD C,(IX+9)
       LD B,(IX+10)
       PUSH BC
       PUSH IX
       POP HL
       CALL CMR
       DEFW JRECLAIM
       POP BC
       RET

MOVA   CALL EXDAT            ;FIRST CHAN ADDR IN NSTR1 AGAIN
       CALL TOSCQ
       JP NZ,MOVE1           ;JR IF NOT MOVE D TO S/T/K

       XOR A
       LD (TVFLAG+FS),A      ;NOT AUTO-LIST
       INC A
       LD (INQUFG+FS),A      ;IN QUOTES SO NO KEYWORDS
       LD A,D
       AND 1FH
       CP 0AH
       JR Z,MOVE1            ;JR IF OPENTYPE

       PUSH AF
       LD B,9
       CALL MOVJ             ;JUNK HEADER
       POP AF
       CP 10H
       JR NZ,MOVCD           ;IF NOT PROGRAM, SUPPRESS UNPRINTABLES

       XOR A
       LD (INQUFG+FS),A      ;NOT IN QUOTES - KEYWORDS ON

MOVLN  CALL MOVRC            ;LINE NO. MSB
       CP 0FFH
       JR Z,MEOF             ;END NOW IF END OF PROG - IGNORE VARS

       PUSH AF
       CALL MOVRC            ;LINE NO. LSB
       LD HL,(NSTR2)
       LD (CURCHL),HL
       POP HL
       LD L,A                ;HL=LINE NO
       CALL PNUM5
       LD B,2
       CALL MOVJ             ;JUNK LINE LEN DATA

MVSLP  CALL MOVRC
       CP 0EH
       CALL Z,MOVJ6          ;JUNK 5-BYTE FORMS

       PUSH AF
       CALL MOVWC
       POP AF
       CP 0DH
       JR NZ,MVSLP

       JR MOVLN

MOVJ6  LD B,6

MOVJ   PUSH BC
       CALL MOVRC
       POP BC
       DJNZ MOVJ

       RET

;MOVE NON-OPENTYPE, NON-PROGRAM TO S/K/T

MOVCD  CALL MOVRC            ;READ CHAR
       JR NC,MEOF            ;JR IF EOF

       BIT 7,A
       JR Z,MCD1

       LD C,A
       LD A,(MSFLG)
       AND A
       LD A,C
       JR Z,MCD0             ;JR IF >127 TO BE INVERTED

       CP 0FFH
       JR NZ,MCD2

       JR MCD15

MCD0   LD HL,INVERT+FS
       LD (HL),0FFH          ;INVERSE

MCD1   AND 7FH
       CP 20H
       JR NC,MCD2

MCD15  LD A,(MSUPC)          ;USUALLY "."

MCD2   CALL MOVWC            ;WRITE PRINTABLE CHAR
       XOR A
       LD (INVERT+FS),A
       JR MOVCD

MOVE1  CALL MOVRC            ;READ CHAR
       JR NC,MEOF            ;JR IF EOF

       CALL MOVWC            ;WRITE CHAR
       JR MOVE1

MEOF   XOR A
       LD (FLAG3),A
       CALL EXDAT
       CALL CLMOV
       CALL EXDAT
       CALL CLMOV
       JP CLTEMP

;DISC TO SCREEN? Z IF SO (D TO S/K/P). EXIT WITH D=SRC FILE TYPE

TOSCQ  LD HL,(NSTR1)
       LD BC,FS+4
       ADD HL,BC             ;SYS PAGE IS AT 8000H. PT TO CHAN LETTER
       LD A,(HL)
       CP "D"+80H
       RET NZ                ;RET IF DISC NOT SRC

       LD BC,FFSA-4
       ADD HL,BC
       LD D,(HL)             ;SRC FILE TYPE

       LD HL,(NSTR2)
       LD BC,FS+4
       ADD HL,BC             ;PT TO CHAN LETTER
       LD A,(HL)
       CP "S"
       RET Z

       CP "P"
       RET Z

       CP "K"
       RET


;MOVE - READ CHAR
;EXIT: CY IF GOT CHAR IN A, NC IF EOF

MOVRC  LD HL,(NSTR1)
       LD (CURCHL),HL

MOVRC2 LD HL,(CURCHL)
       LD DE,FS+2
       ADD HL,DE             ;SYS PAGE IS AT 8000H
       LD E,(HL)
       INC HL
       LD D,(HL)
       EX DE,HL              ;HL=INPUT ADDRESS
       LD A,H
       CP 4BH
       JR Z,DOSIP            ;JR IF DOS

       LD (MTARG),HL
       CALL CMR
MTARG  DW 0
       JR GIPC

DOSIP  CALL MCHRD

GIPC   RET C                 ;RET IF GOT CHAR

       JR Z,MOVRC2           ;LOOP UNLESS EOF

       RET

;MOVE - WRITE CHAR IN A

MOVWC  LD HL,(NSTR2)
       LD (CURCHL),HL
       LD DE,FS+1
       ADD HL,DE
       LD D,A
       LD A,(HL)
       CP 4BH
       LD A,D
       JP Z,MCHWR

       JP PNT

;EVALUATE A MOVE SYNTAX

EVMOV  CALL GTNC
       CP "#"
       JP Z,EVSRM

EVSYN  CALL EVNAM
       RET Z                 ;RET IF SYNTAX TIME

EVSY2  PUSH AF
       CALL EVFINS
       POP AF
       RET

HEVSY  CALL HEVNAM
       JR EVSY2


;OPEN A MOVE CHANNEL. RETURN NSTR1=CHANNEL ADDR IN CHANNELS (SECT B)
;CY IF ERROR, (AND Z IF STREAM NOT OPEN), NC IF OK

OPMOV  LD A,(SSTR1)
       INC A
       JR Z,OPMV1            ;JR IF NOT A STREAM

       DEC A
       CALL STRMD
       LD D,A
       LD A,B
       OR C
       SCF
       RET Z                 ;RET IF STREAM NOT OPEN, WITH CY

       LD A,D
       CALL CMR
       DEFW STREAM           ;SET STREAM
       LD IX,(CURCHL)
       JR OPMV2

OPMV1  LD A,(LSTR1)
       AND 0DFH
       CP "D"
       JP NZ,REP0

       CALL CKDRV
       CALL OPEND
       LD A,1
       INC A                 ;NZ = ERROR NOT DUE STREAM NOT OPEN
       RET C                 ;RET IF ABORTED (OVERWRITE?+N)

       LD A,(NSTR1)
       LD (FSTR1),A
       LD BC,-FS
       ADD IX,BC

OPMV2  LD (NSTR1),IX
       AND A                 ;NC=OK
       RET

;CLOSE A MOVE CHANNEL

CLMOV  LD A,(SSTR1)
       INC A
       RET NZ                ;RET IF THERE WAS A STREAM - DON'T CLOSE IT

       LD IX,(NSTR1)
       LD BC,FS
       ADD IX,BC

;ENTRY: IX POINTS TO CHANNEL IN SECT C

DELD   PUSH IX
       POP HL
       LD DE,-FS
       ADD HL,DE             ;CORRECT TO SECT B, LIKE CHANS
       LD DE,(CHANS)
       OR A
       SBC HL,DE
       INC HL
       LD (SVTRS),HL         ;CHANNEL DISP
       JP CLRCHD


;RECLAIM TEMPORARY CHANNELS

CLTEMP LD IX,(CHANS)
       LD DE,6*5+FS
       ADD IX,DE

CLTM1  LD A,(IX+0)
       CP 0DH
       RET Z

       LD A,(IX+4)
       CP "D"+80H
       JR NZ,CLTM2

       CALL DELD
       JR CLTEMP

CLTM2  CALL BITF1
       JR Z,CLTM3            ;JR IF NOT CLEAR #

       CALL RCLMX
       JR CLTEMP

CLTM3  LD E,(IX+9)
       LD D,(IX+10)
       ADD IX,DE
       JR CLTM1


BACKUP     CALL GTNC
           CALL FLTOFL       ;EVAL FILE TO FILE

           CALL COBUS        ;EVFILES, SET SINGLE-DISC FLAG AS NEEDED
           CALL CKDRV
           LD HL,8000H
           LD (HKHL),HL      ;SRC/DEST
           CALL CLSAM        ;CLEAR SAM
           LD A,20H
           CALL FDHR         ;CREATE SAM
           LD B,195          ;BYTES IN SAM
           LD HL,SAM+194

SBKSL      LD A,(HL)
           AND A
           JR NZ,BKU2        ;EXIT WHEN FIRST USED MAP BYTE FOUND

           DEC HL
           DJNZ SBKSL

BKU2       LD L,B
           LD H,0
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL         ;HL=SECTORS USED
           LD BC,40
           ADD HL,BC         ;ALLOW FOR DIRECTORY TKS 0-3
           INC B             ;NZ
           LD DE,0001H
           LD (HKDE),DE      ;START AT T0/S1

BKUL       PUSH DE           ;T/S TO WRITE TO
           PUSH HL           ;SECTS LEFT TO DO
           CALL Z,TSPCE2     ;PROMPT FOR SRC DISC IF 1-DRIVE BACKUP
                             ;(Z IF ENTRY FROM LOOP)
           CALL FFPG
           LD A,B
           AND A
           JP Z,REP35        ;ERROR IF NO PAGES FREE

           LD (HKBC),DE      ;HKBC=PAGE
           LD L,A
           LD H,0
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           EX DE,HL          ;DE=SECTORS FREE AT 32/PAGE
           POP HL            ;LEFT TO DO
           SBC HL,DE         ;SUB LEFT TO DO,FREE
           PUSH HL           ;NEW LEFT TO DO, UNLESS LAST PASS
           JR C,BKU3         ;JR IF CAN FINISH IN THIS PASS
           JR Z,BKU3

           EX DE,HL          ;HL=FREE
           JR BKU4

BKU3       ADD HL,DE         ;HL=LEFT TO DO
           CALL SETF1        ;"LAST PASS"

BKU4       LD (SVHDR),HL     ;IX=SECTS TO DO
           LD A,(DSTR1)
           LD (HKA),A        ;DRIVE
           CALL HFRSAD       ;READ SECTS
           POP HL            ;LEFT TO DO
           EX (SP),HL
           LD (HKDE),HL      ;T/S TO WRITE TO NEXT
           PUSH DE           ;T/S REACHED ON SRC
           CALL TSPCE1       ;PROMPT FOR DEST DISC IF 1-DRIVE BACKUP
           CALL EXDAT
           LD A,(DSTR1)
           LD (HKA),A        ;DRIVE
           CALL BITF6
           JR NZ,BKU5        ;JR IF NOT FIRST PASS

           LD A,(HKBC)
           OUT (251),A
           LD HL,80FFH
           CALL FESE2        ;SET RND WORD AND NAME
           CALL SETF6        ;"NOT FIRST PASS"

BKU5       CALL HFWSAD       ;WRITE SECTS
           POP HL
           LD (HKDE),HL      ;T/S TO READ FROM NEXT IF MORE PASSES
                             ;DE=T/S TO WRITE TO NEXT (DIFFERENT, IF RAMD)
           CALL EXDAT
           POP HL            ;LEFT TO DO OR JUNK
           CALL BITF1
           JR Z,BKUL         ;LOOP UNTIL LAST PASS

           RET



