
;G1
;HOOK CODE ROUTINES

;INPUT A HEADER FROM IX

RXHED  CALL RXSS
       JR NZ,REP10H

       RET

RXSS   CALL RXHSR
       CALL HCONR
       LD A,(LSTR1)
       CP "D"
       RET

;INPUT A HEADER FROM IX, ALLOW DEVICE D/T/N. RETURNS NC IF D, CY IF T/N

RXHED2 CALL RXSS
       RET  Z

       CP "T"
       JR Z,EVFL75

       CP "N"

REP10H JP  NZ,REP10          ;INVALID DEVICE

EVFL75 CALL NRWR
       DEFW 5BB7H            ;TEMP DEVICE

       LD   A,(DSTR1)
       CP   8
       JR   NC,EVFL8         ;JR IF NO SENSIBLE TAPE SPEED SPECIFIED

       LD   A,112            ;DEFAULT TAPE SPEED (IRREL FOR NET)

EVFL8  CALL NRWR
       DEFW 5BB8H            ;TEMP SPEED

       LD   HL,NSTR1+1
       LD   A,(HL)
       SUB  20H
       JR   NZ,EVFL8A

       DEC A                 ;A=FF
       LD   (UIFA+1),A       ;NULL NAME
       JR   EVFL8B

EVFL8A LD   DE,UIFA+1
       LD   BC,14
       LDIR

;OUTPUT UIFA TO ROMUIFA

TXINF
EVFL8B LD BC,0               ;COPY TO ROM HDR
       LD   DE,UIFA
       PUSH IX
       JR TXRM               ;ENDS WITH SCF

;CONVERT NEW HDR TO OLD

HCONR  CALL RESREG

       LD   HL,UIFA+1
       CALL EVFILE
       LD A,(7FFFH)          ;ENTRY LRPORT VALUE ON STACK
       BIT 6,A
       CALL NZ,GTLNM         ;IF ROM 1 USED HGTHD THEN USE LONG NAME

       LD   A,(UIFA)
       LD   (NSTR1),A
       LD   (HD001),A

       LD   A,(UIFA+31)
       LD   (PAGE1),A

       LD   HL,(UIFA+32)
       LD   (HD0D1),HL

       LD   A,(UIFA+34)
       AND  1FH
       LD   (PGES1),A

       LD   HL,(UIFA+35)
       RES  7,H
       LD   (UIFA+35),HL
       LD   (HD0B1),HL

       RET

;HOOK TO OPEN A FILE FOR LOAD/VERIFY - IX=HDR

HGTHD  PUSH IX
       CALL RXHED2
       JR C,HGTH2            ;JR IF TAPE OR NET

       CALL CKDRV
       CALL GTFLE
       POP IX

;OUTPUT DIFA TO ROMDIFA (DIFA TO IX+80)

TXHED  PUSH IX
       POP HL
       PUSH HL
       LD BC,0024H
       ADD HL,BC             ;IX+24H
       CALL RDA              ;MSB OF LEN MOD 16K (HDR) TO A
       LD HL,DIFA+24H
       XOR (HL)
       AND 80H               ;TAKE BIT 7 FROM HDR (SOMETIMES HI, SOMETIMES LO)
       XOR (HL)              ;VERIFY DEPENDS ON EQUALITY!
       LD (HL),A
       LD DE,DIFA
       LD BC,80

;OUTPUT A HEADER

TXRM   POP HL                ;WAS IX
       ADD HL,BC             ;NC. DEST IN HL=IX OR IX+80
                             ;SRC IN DE=DIFA OR UIFA
       JR RXTXC

RXHSR  LD DE,UIFA
       PUSH IX
       POP HL                ;SRC=HL, DEST=UIFA
       SCF

RXTXC  BIT 7,H
       IN A,(251)
       LD BC,251
       JR NZ,RXH1            ;JR IF NO PAGING REQUIRED

       SET 7,H
       RES 6,H
       OUT (C),B             ;SYSTEM PAGE

RXH1   JR C,RXH2

       EX DE,HL

RXH2   LD C,48
       LDIR

       OUT (251),A
       SCF                   ;FOR TXINF
       RET

HGTH2  POP HL                ;JUNK
       LD HL,(ENTSP)
       INC HL
       INC HL
       INC HL
       INC HL
       INC (HL)              ;STORED SP LSB (EVEN)
       INC (HL)              ;DISCARD ONE ADDR - RET TO LOAD FILE
       LD E,3                ;LOAD/MERGE VERIFY ENTIRE FILE FROM T/N
       JP END1

DSCHD  CALL GTIXD
       CALL LDHDX

       LD   HL,(HKHL)
       LD   (HD0D1),HL       ;START

       LD   A,(HKBC)         ;CDE WAS LEN
       LD   C,A
       LD   (PGES1),A

       LD   DE,(HKDE)
       RES  7,D
       LD   (HD0B1),DE

       RET

NETPA  LD A,(LSTR1)
       CP "N"
       RET NZ

       POP HL                ;JUNK RET ADDR
       CALL OWSTK
       EXX                   ;START, LEN TO HL, CDE FOR PASS OUT
       RET

HVEPG  OUT (251),A

;VERIFY FILE ALREADY OPENED BY HGTHD. HL=START, CDE=LEN, PAGED IN.

HVERY  LD BC,4BB0H+HVEP-PVECT
       CALL NETPA
       CALL DSCHD
       LD   (IX+RPTL),9

HVER1  LD   A,D
       OR   E
       JR   NZ,HVER2

       LD   A,C
       AND  A
       JP Z,SKSAFE

       DEC  C
       LD   DE,16384

HVER2  CALL LBYT
       CP   (HL)
       JP   NZ,REP12         ;VERIFY FAILED

       DEC  DE
       INC  HL
       LD A,H
       CP 0C0H
       JR C,HVER1

       CALL INCURPAGE
       JR   HVER1

HSAVE  CALL RXHED2           ;GET HEADER, ALLOW DEVICES D/T/N
       JR C,HSAVE2           ;JR IF T/N

       CALL CKDRV

       IN   A,(251)
       LD   (PORT3),A
       LD   A,(UIFA+31)
       CALL SELURPG

       CALL GOFSM
       JR C,HSAVE1           ;JR IF "OVERWRITE?" AND N

       CALL SVHD
       LD   HL,(HD0D1)
       LD   DE,(HD0B1)
       CALL DSVBL
       CALL CFSM

HSAVE1 LD   A,(PORT3)
       OUT  (251),A
       RET

HSAVE2 LD   E,2              ;SAVE ENTIRE FILE TO TAPE OR NET
       JP   END1

DDEL   CALL TIRD
       RET NC                ;RET IF RAM DISC

       LD A,(DWAI)
       INC A

DDLP   PUSH AF
       XOR A
       CALL STPDX            ;DELAY ABOUT 0.25 SEC
       POP AF
       DEC A
       JR NZ,DDLP

       RET

HVAR   CALL CGTINT           ;GET DVAR PARAM

       LD   HL,DVAR
       ADD  HL,BC            ;ADD DVAR BASE
       IN A,(250)
       AND 1FH
       ADD A,2               ;A=DOS PAGE+1
       ADD  HL,HL
       ADD  HL,HL            ;LEFT JUSTIFY 16K OFFSET - JUNK UPPER BITS
       LD   B,96H            ;INIT EXPONENT

HVAR1  DEC  B
       ADD  HL,HL
       RLA
       BIT  7,A
       JR   Z,HVAR1

       RES  7,A              ;RES SGN BIT
       LD   E,A
       LD   A,B              ;EXPONENT

HVAR2  LD   D,H
       LD   C,L
       LD   B,0
       JP HSTKS

FNLN2  LD A,2
       DB 21H                ;"JR+2"

HPTR   LD A,1
       DB 0FEH               ;"JR+1"

HEOF   XOR A

       PUSH IX
       LD C,251
       IN B,(C)
       PUSH BC
       PUSH AF               ;0 IF EOF, 1 IF PTR
       CALL PESR             ;AHL=PTR
       POP DE
       DEC D
       JR Z,EPCOM            ;JR IF PTR

       DEC D
       JR NZ,HEOF2

       CALL GLEN             ;GET FILE LEN IN AHL
       JR EPCOM

HEOF2  CALL CPPTR
       LD HL,0
       LD A,H
       JR NZ,EPCOM

       INC HL                ;HL=1, EOF=TRUE

EPCOM  POP BC
       OUT (C),B
       POP IX

;STACK AHL AS A NUMBER

STKFP  LD B,A
       OR H
       OR L
       JR Z,STKHL            ;ZERO IS A SPECIAL CASE

       LD A,B
       LD B,98H              ;INIT EXPONENT
       JR HVAR1

;STKHL ON FPCS. USED BY EOF

STKHL  LD A,H
       LD H,L
       LD L,A
       XOR A
       LD E,A
       JR HVAR2

AUTNAM DEFB 1
       DEFB 0FFH
       DEFB 0FFH
       DEFB "D"
       DEFB 10H
       DEFM "AUTO*     "
       DEFM "    "
       DEFB 0
       DEFW 0FFFFH
       DEFW 0FFFFH
       DEFW 0FFFFH
       DEFW 0FFFFH


;LOOK FOR AN AUTO FILE

HAUTO  CALL AUINSR
       JP   NZ,REP20         ;ERROR IF NOT FOUND

AUINC  CALL GTFLX
       JP   AUTOX

AUINSR LD   A,95H            ;LOAD TOK
       CALL NRWR
       DEFW CURCMD

       LD   HL,AUTNAM
       LD   DE,DSTR1
       LD   BC,28
       LDIR
       CALL GTDEF
       CALL CKDRV
       LD   A,10H            ;"LOOK FOR NAME"
       JP FDHR


INIT   CALL AUINSR
       RET NZ                ;RET IF NOT FOUND

       JR AUINC

;HOOK OPEN FILE

HOFLE  CALL RXHED
       CALL CKDRV
       CALL GOFSM
       JP NC,SVHD            ;JP IF NOT "OVERWRITE?"+N

       RET

HGFLE  CALL RXHED
       CALL GTFLE

LDHDX  CALL RSADSV

;LOAD HEADER INFORMATION

LDHD   LD   B,9
LDHD1  CALL LBYT
       DJNZ LDHD1
       RET

HERAZ  CALL RXHED
       CALL CKDRV
       CALL FINDC
       JP   NZ,REP26

       LD   (HL),0
       JP   WSAD


;WRITE AT A TRACK AND SECTOR

WRITE  CALL EVPRM
       JR HFWSAD

;READ AT A TRACK AND SECTOR

READ   CALL EVPRM
       JR HFRSAD

;HOOK READ 1 SECTOR AT DE

HRSAD  CALL CALS

;HOOK FAR READ MANY SECTORS AT DE

HFRSAD LD IY,RSAD
       JR FRWSR

;HOOK WRITE 1 SECTOR AT DE

HWSAD  CALL CALS

;HOOK FAR WRITE MANY SECTORS AT DE

HFWSAD CALL HFWCD            ;CONVERT HKA AND CHECK DRIVE
       CALL SELD
       CALL DWAIT
       LD IY,WSAD

FRWSR  IN A,(251)
       PUSH AF
       XOR A
       LD (RDAT),A           ;SO RAM DISC KEEPS TRACK UNFIDDLED
       CALL HFWCD
       CALL GTIXD
       LD HL,(HKHL)          ;ADDR
       LD BC,(SVHDR)         ;SECTORS
       LD A,(HKBC)           ;ADDR PAGE
       AND 1FH
       OUT (251),A
       LD DE,(HKDE)          ;T/S

FRWL   PUSH BC
       CALL CHKHL
       LD (BUF),HL
       CALL IYJUMP
       LD BC,(MSINC)         ;USUALLY 0200H
       ADD HL,BC
       CALL ISECT
       JR NZ,FRW2

       PUSH HL
       CALL ITRCK
       POP HL

FRW2   POP BC
       DEC BC
       LD A,B
       OR C
       JR NZ,FRWL

       POP AF
       OUT  (251),A
       JP GTIXD              ;NORMAL BUF AGAIN

;CALCULATE ADDRESS SECTION

CALS   LD   HL,(HKHL)
       XOR A
       CALL PAGEFORM
       DEC A                 ;PAGE 0-2 FOR SECTS B/C/D
       LD BC,1               ;1 SECT TO DO

;CALLED BY EVPRM. AHL=ADDR, BC=SECTS

SCASD  LD (HKBC),A
       INC A
       JP Z,IOOR             ;0-3FFFH ILLEGAL ADDR

       LD (HKHL),HL          ;OFFSET
       LD (SVHDR),BC         ;SECTORS TO DO
S      RET


;CALLED BY DIR, DIR$

FDFSR  LD   A,"*"
       LD   (NSTR1+1),A      ;NULL NAME
       LD   (NSTR1+2),A      ;ENSURE NOT "." IF DIR$

;GET DEFAULT LETTER AND NUMBER

GTDEF  CALL NRRD
       DEFW DEVL
       LD   (LSTR1),A

;GET DEFAULT NUMBER

GTDD   CALL NRRD
       DEFW DEVN
       AND A
       JR Z,GTDF2

       CP RDLIM+1
       JR C,GTDF3            ;JR IF LEGAL DRIVE - ELSE PROB. T SPEED

GTDF2  LD A,(ODEF)           ;USE "OTHER" DEFAULT

GTDF3  PUSH HL
       LD (ODEF),A
       CALL CODN             ;CONVERT DRV NUM
       CALL DRSET
       POP HL
       RET

;EVALUATE FILE INFORMATION AT (HL). 14 CHARS IF HL=NSTR1+1

EVFILE CALL GTDEF
       LD   (SVHL),HL
       LD   A,(HL)
       CP 0FFH
       JR NZ,EVFL0           ;JR UNLESS NULL NAME

       LD A,"T"              ;LOAD "" BECOMES LOAD "T:"
       LD (HL),A
       INC HL
       LD (HL),":"
       DEC HL

EVFL0  AND  0DFH

;CHECK FOR FIRST DIGIT

EVFL1  LD   C,A
       INC  HL
       LD   A,(HL)
       CP   ":"
       JR   Z,EVFL3

       SUB  30H
       CP   10
       JR   NC,EVFL4

;CHECK FOR SECOND DIGIT

       LD   D,A              ;SAVE FIRST DIGIT
       LD A,C                ;FIRST CHAR
       CP "D"
       INC  HL
       LD   A,(HL)           ;CHAR AFTER DIGIT
       JR NZ,EVFL12          ;JR IF NOT "D1XXXXX"

       CP   ":"
       JR  NZ,EVFL11         ;JR IF NOT "D1:XXXX"

       CALL C11SP
       JR NZ,EVFL2           ;JR IF NOT "D1:       "

       JR EVFL14

EVFL11 CP " "
       JR NZ,EVFL12          ;JR IF NOT E.G. "D2 XXXXX"

       CALL C11SP
       JR NZ,EVFL4           ;JR IF NOT "D1      "

EVFL14 LD (HL),":"
       INC HL
       LD (HL),"*"           ;E.G. "D1" OR "D1:" BECOME "D1:*"
       DEC HL                ;PT TO ":"
       JR EVFL2

EVFL12 SUB  30H
       CP   10
       JR   NC,EVFL4         ;JR IF 2ND DIGIT NOT FOUND

;EVALUATE 2 DIGIT NUMBER

       LD E,A
       LD   A,D
       ADD  A,A
       ADD  A,A
       ADD  A,D
       ADD  A,A
       ADD  A,E
       LD   D,A
       INC  HL

;CHECK FOR ":"

       LD   A,(HL)
       CP   ":"
       JR   NZ,EVFL4

EVFL2  LD   A,D
       CALL CODN

EVFL3  LD   A,C
       LD   (LSTR1),A        ;LETTER
       INC  HL               ;SKIP ":"
       JR   EVFL5

EVFL4  LD   HL,(SVHL)

;FILE NAME START

EVFL5  LD DE,(SVHL)
       AND A
       SBC HL,DE
       LD (TEMPW3),HL        ;STORE NO. OF CHARS TRIMMED OFF FRONT (0-4)
       ADD HL,DE
       LD   BC,10
       LD   DE,NSTR1+1
       LDIR                  ;LEFT JUSTIFY NAME (OR COPY TO NSTR1+1)

       LD   B,4
       CALL LCNTA            ;PAD WITH 4 SPACES...NOT NEEDED..
       LD A,(DSTR1)

DRSET  LD (DRIVE),A
       CALL GCDIA            ;GET CDIRP ADDR
       LD A,(HL)
       LD (CDIRT),A
       RET

;CHECK FOR 11 SPACES

C11SP  LD B,11
       PUSH HL

C11LP  INC HL
       LD A,(HL)
       CP " "
       JR NZ,C11E

       DJNZ C11LP

C11E   POP HL
       RET

