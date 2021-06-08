
;HOOKS

;HOOK 169 - PRINT TOKEN A

HPTV       EXX               ;GET HL=PASSED IN XPTR
           CP 0FFH
           JR Z,HPFN

           SUB 246-FUNO
           PUSH AF
           PUSH HL           ;XPTR
           CALL NRRD
           DW FLAGS
           RRA
           CALL NC,SPC

           POP BC            ;XPTR
           CALL NRWRD
           DW XPTR
           POP BC

;ENTRY HERE AVOIDS LEADING SPACE. B=CHAR

PTV1       LD HL,KWMT

PTV2       CALL PTVS
           JP SPC

PTVS       LD A,(HL)
           INC HL
           RLA
           JR NC,PTVS

           DJNZ PTVS

           JP PTM2           ;PRINT MSG FROM HL


HPFN       LD (XXPTR),HL     ;SAVE XPTR TILL LATER - AVOID PRINT HOOKS KILLING
                             ;ERROR MARKER
           LD BC,251
           IN E,(C)
           OUT (C),B
           LD B,E
           LD HL,(CURCHL)
           SET 7,H
           RES 6,H
           LD E,(HL)
           INC HL
           LD D,(HL)
           LD (OPSTORE),DE
           LD DE,XTRA        ;XTRA VECTORS - POST FF HOOK
           LD (HL),D
           DEC HL
           LD (HL),E
           OUT (C),B
           RET

;HOOK 170 - POST FF PRINT

HPFF       PUSH AF           ;CHAR AFTER FF
           LD BC,(XXPTR)
           CALL NRWRD
           DW XPTR
           POP AF
           CP 30H
           JR C,HPF3

           CP 30H+FUNO
           CCF
           JR C,HPF3         ;JR IF NOT NEW FN
                             ;ELSE DEAL WITH 30H+
           SUB 2FH
           LD BC,251
           IN E,(C)
           OUT (C),B
           LD B,E
           LD HL,(CURCHL)
           SET 7,H
           RES 6,H
           LD DE,(OPSTORE)
           LD (HL),E
           INC HL
           LD (HL),D         ;RESTORE NORMAL O/P ADDR
           OUT (C),B
           LD B,A
           LD HL,KWMT
           CALL PTVS         ;PRINT NEW FN NAME - NO LEADING SPACE
           AND A             ;NC=DONE IT

HPF3       PUSH AF
           POP BC
           RET

FUNO       EQU 7
CMNO       EQU 3

KWMT:      DB 0A0H
           DM "TIME"
           DB "$"+80H    ;30H $
           DM "DATE"
           DB "$"+80H    ;31H $
           DM "INP"
           DB "$"+80H     ;32H $
           DM "DIR"
           DB "$"+80H     ;33H $

           DM "FSTA"
           DB "T"+80H    ;34H N
           DM "DSTA"
           DB "T"+80H    ;35H N
           DM "FPAGE"
           DB "S"+80H   ;36H N
      ;    DB "SCRAD"+80H    ;37H N

           DM "BACKU"
           DB "P"+80H   ;247
           DM "TIM"
           DB "E"+80H     ;248
           DM "DAT"
           DB "E"+80H     ;249

    ;      DB "ALTE"
    ;      DB "R"+80H    ;250

    ;      DB "SORT"+80H     ;251

;HOOK 171 -  GET TOKEN

HGTTK      LD C,251
           IN B,(C)
           PUSH BC
           XOR A
           OUT (251),A
           LD HL,GTDT
           LD DE,8F00H
           LD BC,GTDTE-GTDT
           LDIR              ;EXTRA CODE TO BUFFER

           IN A,(250)
           INC A
           CALL SELURPG      ;PAGE KEYWORD LIST IN AT 8000H
           EXX
   ;       LD DE,(HKDE)
           LD HL,KWMT-1+FS
           LD A,FUNO+CMNO+1  ;ITEMS IN LIST+1
           CALL CMR
           DW JGTTOK

           POP BC
           OUT (C),B
           JR Z,HGT2         ;RET IF NO MATCH

           CP FUNO+1
           JR NC,HGT1        ;JR IF CMD

           EX DE,HL
           AND A
           SBC HL,DE
           ADD A,2FH         ;FNS=30H+
           SCF
           JR HGT2

HGT1       ADD A,0BBH-FUNO   ;247+

HGT2       PUSH AF
           POP BC
           RET

;COPIED TO 4F00H TO DEAL WITH FNS

GTDT       POP IY
           LD BC,17
           ADD IY,BC         ;PT TO 17 BYTES FURTHER ON IN TOKENISE SR
           POP DE
           ADD HL,DE
           EX DE,HL
           LD (HL),0FFH      ;FN LEADER PLACED IN BASIC LINE
           JP (IY)

GTDTE

;HOOK 172 - LENGTH FUNCTION PATCH

HKLEN      EXX               ;HL=RET ADDR TO SCANNING
           INC HL
           INC HL
           INC HL
           LD C,(HL)         ;DISP TO "IMMED CODES" (PART OF "JR")
           LD B,0
           ADD HL,BC
           PUSH HL           ;IMMED CODES
           LD C,17
           ADD HL,BC         ;PT TO NUMCONT
           LD C,(HL)
           INC HL
           LD B,(HL)         ;BC=NUMCONT
      ;    LD A,(HKA)
           CP 34H-1AH
           JR NC,HEVV2       ;JR IF NUMERIC RESULT

           LD BC,6
           ADD HL,BC
           LD C,(HL)
           INC HL
           LD B,(HL)         ;BC=STRCONT

HEVV2      PUSH AF
           CALL OWSTK        ;OVER-WRITE ADDR AFTER HOOK CODE ON MAIN ROM
                             ;STACK WITH NUMCONT OR STRCONT ACCORDING TO
                             ;TYPE OF RESULT
           CALL GTIXD
           POP AF
           POP HL            ;IMMED CODES
           CP 3FH-1AH        ;LENGTH
           JR Z,FNLENG

           SUB 30H-1AH
           JP C,REP0

           CP FUNO
           JP NC,REP0

           ADD A,A
           LD L,A
           LD DE,DFNT

;USED BY MAIN HOOK ROUTINE. DE=TABLE, L=ENTRY

INDJP      LD H,0
           ADD HL,DE
           LD E,(HL)
           INC HL
           LD D,(HL)
           EX DE,HL
           JP (HL)

FNLENG     LD BC,10
           ADD HL,BC
           LD C,(HL)
           INC HL
           LD B,(HL)         ;IMFNTAB
           LD HL,8
           ADD HL,BC
           LD E,(HL)
           INC HL
           LD D,(HL)         ;IMLENGTH
           IN A,(250)
           OR 40H
           OUT (250),A       ;ROM1 ON
           IN A,(251)
           PUSH AF
           XOR A
           OUT (251),A       ;SYS PAGE AT 8000H
           EX DE,HL
           LD DE,8D80H
           LD BC,0078H
           LDIR

           LD HL,PDATA
           LD C,15
           LDIR

           LD A,0BH
           LD (8D80H+6CH),A
           LD HL,4D80H+36H
           LD (8D80H+24H),HL
           POP AF
           OUT (251),A       ;ORIG
           CALL NRRDD
           DW CHADD
           INC BC
           LD A,(BC)
           CP "#"
           JR NZ,NRML

           CALL GTNC         ;PT TO "#"
           CALL GTNC         ;SKIP "#"

           CALL CMR
           DW EXPNUM
           CALL FABORT

;FNLENGTH
           JP FNLN2

NRML       CALL CMR
           DW 4D80H          ;CALL MODIFIED "LENGTH" CODE IN CDBUF
           RET


;CODE TO PATCH "LENGTH" FOR LENGTH(0,A$) ERROR ON PAGE BOUNDARY

PDATA:     BIT 6,H
           JR Z,PDAT2

           RES 6,H
           LD A,(5123H)
           INC A
           LD (5123H),A

PDAT2:     JR $-56H

DFNT       DW FNTIME
           DW FNDATE
           DW INPST          ;INP$
           DW FNDIRS
           DW FSTAT
           DW DSTAT
           DW FPAGES
     ;     DW SCRAD

FNDIRS     CALL FDFSR        ;GET "ANY" NAME, GTDEF
           CALL GTNC
           CP "("
           JR Z,FNDI2

           CALL FABORT
           JR FNDI3

FNDI2      CALL EVNAMX
           LD C,")"
           CALL ISEP
           CALL FABORT

           CALL EVFINS

FNDI3      CALL DITOB        ;DIR TO BUFFER
           LD HL,(PTRSCR)
           LD DE,0A000H
           AND A
           SBC HL,DE         ;HL=TEXT LEN (10 PER NAME)
           LD B,H
           LD C,L
           IN A,(251)
           CALL HSTKS
           JP PUTSCR


;DSTAT(D,X) - STATUS OF DISC D. IF D=*, USE DEFAULT DRIVE

;X=1 GIVES 0 IF WRITE PROTECTED OR NO FREE SLOTS, ELSE GIVES FREE SPACE
;X=2 GIVES 1 IF WRITE PROTECTED
;X=3 GIVES FREE SPACE (SECTS*510-9)
;X=4 GIVES FREE SLOTS
;X=5 GIVES TOTAL FILES
;X=6 GIVES FILES IN CURRENT DIR
;X=7 GIVES DTKS
;X=8 GIVES CURRENT DRIVE

DSTAT      CALL SIBKS        ;INSIST "("
           CALL EVDNM        ;DRIVE NUMBER TO DSTR1
           CALL CNB          ;SKIP COMMA, GET X IN BC. NO RET IF SYNTAX
           LD A,C            ;PARAM
           CP 8
           LD A,(DSTR1)      ;DRIVE
           JR Z,NSTKAH       ;STACK CURRENT DRIVE IF PARAM=8

           SUB 2
           JR NZ,DST0

           LD A,(DVAR+2)
           AND A
           JR NZ,DST0        ;JR IF DRIVE 2 EXISTS

           CALL HOC0         ;GET AEHL=-1

HVAR2H     JP HVAR2          ;STACK-1

DST0       CALL CKDRV
           LD B,A
           PUSH BC
           CALL HOCHK
           POP BC
           JR Z,HVAR2H       ;IF NO DISC, STACK -1

           LD A,B            ;DRIVE
           DEC C
           JR NZ,DST1        ;JR UNLESS SPECIAL WP/FREE SLOT/SPACE

           CALL WPCHK
           LD A,0
           JR NZ,STKA        ;ZERO SPACE FREE IF WRITE PROT

           JR DST15

DST1       DEC C
           JR Z,DST3         ;JR TO CHECK WRITE PROTECT

           LD A,C
           CP 6
           LD A,(DRIVE)

NSTKAH     JR Z,STKA

           LD A,C
           JP NC,IOOR

DST15      PUSH AF
           IN A,(251)
           PUSH AF
           CALL ZDVS         ;ZERO VARS
           CALL DITOB        ;DIR TO BUFFER
           CALL STATS        ;GET HL=FREE SECTS, DE=FREE SLOTS
           ADD HL,HL         ;FREE 256-BYTES
           POP AF
           OUT (251),A
           POP AF
           EX DE,HL
           AND A
           JR NZ,DST17

           LD A,H
           OR L

SHLHP      JP Z,STKHL        ;NO SPACE IF NO SLOTS

           JR DST18

DST17      CP 2
           JR Z,SHLHP        ;STACK FREE SLOTS

           DEC A
           JR NZ,DST2

DST18      LD A,D
           OR E
           JR Z,STKA         ;JR IF NO FREE SECTORS

           LD L,0
           LD A,D
           LD H,E            ;AHL=SECTS*512, DE=SECTS*2
           AND A
           SBC HL,DE
           SBC A,0           ;AHL=SECTS*510
           LD DE,9
           SBC HL,DE
           SBC A,0           ;ALLOW FOR HEADER
           JP STKFP

DST2       LD HL,(TCNT)
           SUB 2
           JR Z,DSSTK

           LD HL,(FCNT)
           DEC A

           JR Z,DSSTK

           LD A,(DTKS)
           JR STKA

DST3       CALL WPCHK

STKA       LD L,A
           LD H,0

DSSTK      JP STKHL

;FREE PAGES FN

FPAGES     CALL GTNC
           CALL FABORT

           CALL CNTFP
           JR STKA

;ENTRY: A=DRIVE

WPCHK      CP 3
           LD A,0
           JR NC,WPC2        ;JR IF RAM DISC

           CALL SELD
           ADD A,2
           LD C,A
           OUT (C),A         ;IMPOSSIBLE SECTOR
           CALL PRECMX
           CALL BUSY
           RLCA
           RLCA              ;BIT 6 (WRITE PROT) TO BIT 0

WPC2       AND 01H
           RET

SIBKS      LD C,"("
           JP ISEPX

;INP$(#STREAM,NO. OF CHARS) FUNCTION

INPST      CALL SIBKS        ;INSIST "("
           LD C,"#"
           CALL ISEP
           CALL NNB          ;GET CHARS IN BC, STREAM IN DE. NO RET IF
                             ;SYNTAX TIME
           LD HL,16
           AND A
           SBC HL,DE
           JP C,INVST        ;LIMIT STREAM TO 0-16

           DEC BC            ;Z->FFFF
           LD A,B
           CP 40H
           JP NC,IOOR

           PUSH BC
           CALL NRRDD
           DW CURCHL-FS
           POP HL            ;CHARS
           PUSH BC           ;CURCHL
           IN A,(251)
           PUSH AF
           INC HL            ;HL=1-4000H
           PUSH HL
           LD A,E
           CALL CMR
           DW STREAM

           POP BC
           CALL CMR
           DW WKROOM
           PUSH BC
           PUSH DE

INPSL      PUSH BC
           PUSH DE
           IN A,(251)
           PUSH AF
           XOR A
           OUT (251),A       ;SYS PAGE
           CALL MOVRC2
           JP NC,REP27       ;EOF

           LD D,A
           POP AF
           OUT (251),A       ;WKROOM
           LD A,D
           POP DE
           POP BC
           LD (DE),A
           INC DE
           DEC BC
           LD A,B
           OR C
           JR NZ,INPSL

           POP DE            ;START
           POP BC            ;LEN
           IN A,(251)
           EX AF,AF'
           POP AF
           OUT (251),A       ;ORIG
           EX AF,AF'
           CALL HSTKS        ;STACK ADEBC
           POP BC            ;ORIG CURCHL
           CALL NRWRD
           DW CURCHL-FS
           RET

;EVAL NUMBER, COMMA, NUMBER, BRACKET. ABORT IF SYNTAX, ELSE
;RETURN 1ST NUM IN DE, 2ND IN BC

NNB        CALL EVNUM        ;GET NUM IN HL, Z IF SYNTAX

CNB        PUSH HL
           LD C,","
           CALL ISEP
           CALL EVNUM
           PUSH HL
           LD C,")"
           CALL ISEP
           POP BC            ;2ND
           POP DE            ;1ST

FABORT     CALL CFSO
           RET NZ

           POP HL
           RET

;FILE STATUS FUNCTION
;FSTAT("NAME",1)=FILE NUMBER
;FSTAT("NAME",2)=FILE LENGTH
;FSTAT("NAME",3)=FILE TYPE
;FSTAT("NAME",4)=FILE TYPE AND PROTECT/HIDE BITS

FSTAT      CALL GTDEF
           CALL SIBKS        ;INSIST ON "("
           CALL EVNAM
           CALL CNB          ;COMMA,NUMBER, ")", ABORT

           PUSH BC           ;NUMBER
           DEC BC            ;LEGAL VALUES NOW 0-3
           LD HL,3
           AND A
           SBC HL,BC
           JP C,IOOR

           CALL EVFINS
           CALL HOCHK
           POP BC
           JP Z,HVAR2          ;STACK -1 IF NO HOLE OR NON-FORMATED RAMDISC

           PUSH BC
           CALL FINDC        ;LOOK FOR NAMED FILE, POINT
           POP BC
           LD A,0
           JR NZ,STKAH       ;JR IF NOT FOUND - STACK 0

           DEC C
           JR Z,FST2

           LD A,(HL)         ;TYPE
           DEC C
           JR Z,FST3

           DEC C
           JR NZ,STKAH       ;JR IF PARAM WAS 4

           AND 1FH

STKAH      JP STKA

FST2       CALL CONM         ;CONVERT T/S IN D/E TO A NUMBER IN BC
           LD H,B
           LD L,C

STHLH      JP STKHL

FST3       LD DE,0
           AND 1FH
           CP 5
           LD A,3
           JR Z,FST4         ;JR IF 48K SNAP, ADE=48K

           LD BC,00EFH
           ADD HL,BC         ;POINT TO LEN DATA IN BUFFER (DIR ENTRY)
           LD A,(HL)
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)

FST4       EX DE,HL          ;AHL=PAGE FORM

STK20B     CALL AHLNX        ;GET 20-BIT NUMBER
           JP STKFP

;SCRAD      CALL GTNC
 ;         CALL FABORT

  ;        CALL NRRD
   ;       DW CUSCRNP
    ;      AND 1FH
     ;     INC A
      ;    LD HL,8000H
       ;   JR STK20B

HOCHK      LD A,(DRIVE)
           CP 3
           JR C,HOC1

           CALL RTSTD
           AND A
           RET NZ            ;RET IF RAM DISC FORMATTED

HOC0       LD H,A
           LD L,A
           JR HOC2

HOC1       LD D,A            ;SIDE 1 (TRACK <80)
           CALL TFIHO        ;TEST FOR INDEX HOLE. Z IF NONE, A=0, HL=0
           RET NZ            ;RET IF HOLE FOUND

HOC2       DEC HL            ;VALUE
           LD E,H            ;SGN BYTE. A=0 AND EHL=FFFFFF IF NO HOLE
           RET               ;Z

FNTIME     LD HL,TIMDT       ;TIME START
           DB 0FDH           ;"JR+3"

FNDATE     LD HL,DATDT       ;DATE START

;TIME/DATE FN COMMON

TDFNC      PUSH HL
           CALL GTNC
           POP HL            ;STR START
           CALL FABORT

           CALL RDCLK
           LD BC,8           ;STR LEN
           JR HPTH2

;HOOK CALL FOR PATH$

HPATH      CALL GTDEF
           CALL GPATD

HPTH2      SET 7,H
           RES 6,H
           EX DE,HL
           IN A,(250)
           INC A                 ;DOS PAGE

HSTKS      CALL CMR
           DW STKSTR
           RET

;HOOK 173 - VIA CMDV - SAVE/LOAD/MERGE/VERIFY

HSLMV      PUSH AF
           CALL NRRDD
           DW COMAD          ;CMD ADDRT TO BC
        ;  LD A,(HKA)
           POP AF
           CALL NRWR
           DW CURCMD         ;WRITE CURCMD
           SUB 90H
           ADD A,A
           LD L,A
           LD H,0
           ADD HL,BC
           LD C,250
           IN B,(C)
           SET 6,B
           OUT (C),B         ;ROM1 ON
           LD E,(HL)
           INC HL
           LD D,(HL)
           IN A,(251)
           PUSH AF
           XOR A
           OUT (251),A
           PUSH DE           ;SLVM ADDR

           LD HL,MGPT
           LD DE,9000H-MGPE+MGPT ;DEST IN BUFFER FOR MERGE PATCH CODE
           LD BC,MGPE-MGPT
           LDIR              ;DE ENDS AT 9000H

           POP HL
           LD C,58H
           LDIR              ;DE ADV TO 8158H, SRC TO SLVM+58H

           PUSH HL
           LD HL,SVDT
           LD C,MGPT-SVDT
           LDIR

           POP HL
           LD (9000H+59H),HL ;PATCH JP TO SLMV+58H

           LD HL,(9000H+25H) ;READ SBFSR ADDR
           LD (9000H+5CH),HL ;PATCH CALL SBFSR REPLACEMENT
           LD HL,5000H+5BH
           LD (9000H+25H),HL ;PATCH CALL SBFSR
           POP AF
           OUT (251),A       ;ORIG
           LD BC,5000H-MGPE+MGPT ;START OF MERGE PATCH IN BUFFER

;OVER WRITE MAIN ROM STACK - FIRST ADDR WITH BC.

OWSTK   ;  LD HL,(ENTSP)
       ;   INC HL
        ;  INC HL
        ;  LD E,(HL)
        ;  INC HL
        ;  LD D,(HL)
        ;  EX DE,HL          ;HL=MAIN ROM STACK PTR READ FROM DOS STACK
           LD HL,(7FFCH)
           JP WRTBC

SVDT       JP 0
           CALL 0            ;SBFSR
           LD A,C
           LD DE,4F60H
           LD (DE),A
           DEC DE            ;4F5F
           LD HL,4F4FH
           LD BC,50H         ;FOR 4F00-4F4F COPIED TO 4F10-4F5F
           LDDR              ;COPY TO SAFETY IN CASE E.G. "S" CODE UDG " "

           INC DE            ;4F10H
           LD C,A
           CP 15
           RET C

           LD C,14
           RET

;CORRECT MERGE BUG (IF TOO MANY NUMS MERGE FOR GAP SIZE BETWEEN NUMEND AND
;AND SAVARS)

MGPT       LD A,(CURCMD)
           CP 96H            ;MERGE
           JR NZ,MGP2

           LD HL,(SAVARS)
           LD DE,(NUMEND)
           LD A,(NUMENDP)
           LD C,A
           LD A,(SAVARSP)
           CP C
           JR Z,MGP1         ;JR IF SAME PAGE

           RES 7,D
           SET 6,D           ;SUB 4000H
           AND A

MGP1       SBC HL,DE         ;GET GAP SIZE
           LD A,H
           CP 6
           JR NC,MGP2        ;JR IF LOTS OF SPACE TO MERGE NUMS (1.5K OR MORE)

           IN A,(251)
           PUSH AF
           LD A,C
           OUT (251),A       ;PAGE IN NUMENDP
           XOR A
           LD BC,0610H       ;ABC=SPACE
           LD HL,(NUMEND)    ;MAKE ROOM HERE
           CALL JMKRBIG
           POP AF
           OUT (251),A

MGP2       RST 20H
           LD BC,5FFAH
           OUT (C),B         ;ROM1 ON (CMR TURNS OFF ROM1)
MGPE

;HOOK 174 - RUN/CLEAR PATCH

RCPTCH     CALL FABORT

           IN A,(251)
           PUSH AF
           CALL NRRDD
           DW NVARS
           LD A,B
           CP 0BCH
           JR C,RCP3
                             ;NVARS IS TOO CLOSE TO END OF PAGE - EXPAND
           CALL FPGE         ;POST PROG GAP TO PUT NVARS IN NEXT PAGE
           XOR A
           LD BC,0400H
           CALL CMR
           DW   JMKRBIG      ;NVARS NOW AT LEAST C000H->8000H IN NEXT PAGE
           JR RCP5

RCP3       CP 85H
           JR C,RCP5         ;RET IF NVARS TOO CLOSE TO PAGE START FOR ANY POST-
                             ;PROG GAP TO BE CLOSED.

           PUSH BC           ;NVARS
           CALL FPGE
           EX DE,HL          ;DE=PROG END
           CALL NRRD
           DW NVARSP
           POP HL
           LD C,A            ;CHL=NVARS
           IN A,(251)        ;PROG END P
           XOR C
           AND 1FH
           JR Z,RCP4         ;JR IF NORMAL SUBTRACT OK - PAGES MATCH

           SET 6,H           ;ADD 4000H TO NVARS

RCP4       SCF               ;NORMAL GAP=1, SO SCF FOR NORMAL=0
           SBC HL,DE
           JR Z,RCP5         ;RET IF NO GAP TO CLOSE

           LD B,H
           LD C,L
           EX DE,HL          ;RECLAIM AT PROG END
           CALL CMR
           DW JRECLAIM
           LD (HL),0FFH      ;ENSURE PROG TERMINATED

RCP5       POP AF
           OUT (251),A
           RET

FPGE       CALL NRRDD
           DW PROG           ;GAP AFTER PROGRAM TO MOVE NVARS TO NEXT PAGE
           PUSH BC
           CALL NRRD
           DW PROGP
           OUT (251),A
           POP HL

FPEL       LD D,(HL)
           INC D
           RET Z             ;RET IF TERMINATOR

           INC HL
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC HL
           ADD HL,DE
           CALL CHKHLR
           JR FPEL


;CALL MAIN ROM - REWRITTEN TO ALLOW HOOKS TO BE USED BY ROUTINES CALLED BY CMR

CMR:       EXX
           POP HL
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC HL
           PUSH HL
           LD HL,(7FFCH)     ;MAIN ROM SP ON ENTRY TO DOS
           PUSH HL
           LD HL,CMRF
           PUSH HL
           LD C,A
           IN A,(251)
           LD H,A
           IN A,(250)
           LD B,A
           INC A
           AND 1FH
           OUT (251),A       ;DOS AT 8000H ALSO
           JP CMR2+FS

CMR2       LD A,1FH
           DI
           OUT (250),A       ;SYS PAGE AT 4000H
           LD IY,(DOSSTK)
           LD A,H            ;ORIG URPORT
           LD HL,0
           ADD HL,SP
           LD (DOSSTK),HL    ;SO CALLS TO DOS DO NOT HIT CURRENT STACK
           LD SP,(7FFCH+FS)  ;NEW STACK JUST BELOW MAIN ROM STACK
           EI
           PUSH HL           ;ORIG SP
           PUSH BC           ;B=ORIG LRPORT
           PUSH IY           ;ORIG DOSSTK
           LD HL,XTRA+L58D4-PFV   ;RET ADDR TO PAGING SR AFTER DEFKEYS
           PUSH HL
           PUSH DE           ;ROUTINE ADDR TO CALL
           JP XTRA+CMR3-PFV  ;PAGE IN ORIG URPORT, SET A REG, RET

CMRF       POP IY
           LD (7FFCH),IY     ;ORIG IN CASE HOOK USE CHANGED IT
           RET               ;AVOID STACK CREEP

;JUMPED TO AT BOOT TIME (INIP3+FS)

INIP3      LD A,5BH
           LD BC,0D65BH      ;LOOK FOR 5BD65B OF LD DE,(UMSG)
           LD HL,3D00H       ;AT 3DB1 IN ROM 30
           CALL FTHREE+FS
           LD DE,-4
           ADD HL,DE         ;POSTFF ADDR
           PUSH HL

           LD HL,PFV+FS
           LD DE,XTRA
           LD BC,LDEND-PFV
           LDIR              ;DATA TO AFTER DKLIM

           POP HL
           LD (XTRA+PFTRG+1-PFV),HL

           LD HL,3A31H       ;:/1
           LD (PTH1+FS),HL
           INC L
           LD (PTH2+FS),HL   ;INIT PATH FOR DRIVE 2
           LD A,(CKPT+FS)
           LD C,A
           LD B,0F0H         ;CONTROL REG
           LD A,05H          ;0101 NOT TEST/24H/RUN /REST
           OUT (C),A         ;REST HI
           OUT (C),A         ;WE CAN SET 24H NOW
           LD A,04H          ;0100 NOT TEST/24H/RUN /NOT REST
           OUT (C),A
           LD HL,SYSP
           LD (DOSSTK),HL
           LD A,1
           LD (DRIVE+FS),A

           LD A,0CFH
           LD BC,82C9H
           LD HL,0E200H          ;E2B6 IN ROM 30
           CALL FTHREE+FS
           LD DE,4
           ADD HL,DE
           LD (4BB0H+HLVTG-PVECT+1),HL  ;LOAD/VERIFY HOOK PATCH
           RET

FTHREE     LD DE,0

F3OL       PUSH BC           ;CHARS 2 AND 3
           LD B,A

F3IL       LD A,D
           LD D,E
           INC HL
           LD E,(HL)
           CP B
           JR NZ,F3IL

           POP BC
           EX DE,HL
           SBC HL,BC
           EX DE,HL
           JR NZ,F3OL

           DEC HL
           DEC HL
           RET

;MEGA RAM INIT - USED AT BOOT TIME WHEN DOS AT 8000H. CALL MRINIT+FS

MRINIT     DI
           LD A,(DOSFLG)
           DEC A
           OUT (250),A       ;DOS PAGED IN AT 4000H AS WELL AS 8000H (BOOT)
           LD (TEMPW1),SP
           LD SP,8000H
           CALL MRIP2        ;CALL SR IN SECT B. EXIT WITH DE=MR PAGES
           LD HL,(TEMPW1)
           LD BC,5FFAH
           OUT (C),B         ;NORMAL PAGE AT 4000H (ROM1 ON)
           LD SP,HL
           LD A,D
           OR E
           RET Z             ;RET IF NO FREE PAGES

           EX DE,HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL         ;*16=K (16-4096)
           LD (TEMPW1+FS),HL

           RST 28H
           DB 2AH            ;LKADDRW
           DW TEMPW1+FS
           DB EXIT

           LD A,2
           CALL STREAM

           CALL JSTRS
           CALL 0013H

           LD DE,XMMSG+FS
           LD BC,MRIP2-XMMSG
           JP 0013H          ;PRINT BC FROM DE

XMMSG      DB "K External Memory",0DH

MRIP2      IN A,(251)
           PUSH AF
           OR 80H            ;EXTERNAL MEM BIT ON
           OUT (251),A

           LD C,0            ;START AT PAGE 0

MRIL       LD A,C
           OUT (MRPRT),A     ;SELECT MEGA RAM (IF PRESENT)
           LD HL,8000H
           XOR A
           LD (HL),A
           CP (HL)
           JR NZ,MRI2        ;JR IF CANNOT BE SET TO ZERO

           INC A
           LD (HL),A
           CP (HL)
           JR NZ,MRI2        ;JR IF CANNOT BE SET TO 0FFH

           LD A,C
           CALL RMRBIT       ;RES MEGA RAM BIT - FREE
           LD HL,0
           ADD HL,SP

           EXX
           LD SP,0C000H
           LD HL,0
           LD BC,0004H

CMRL       PUSH HL
           PUSH HL
           PUSH HL
           PUSH HL
           PUSH HL
           PUSH HL
           PUSH HL
           PUSH HL           ;CLEAR 16 BYTES
           DJNZ CMRL         ;DO IT 256 TIMES=4K

           DEC C
           JR NZ,CMRL        ;DO 4K FOUR TIMES

           EXX
           LD SP,HL
           JR MRI3

MRI2       LD A,C
           CALL SMRBIT       ;SET MEGA RAM BIT A - NOT PRESENT

MRI3       INC C
           JR NZ,MRIL        ;DO PAGES 00-FF

           POP AF
           OUT (251),A       ;EXTERNAL MEM BIT OFF

           LD HL,MRTAB
           LD E,32           ;BYTES TO LOOK AT
           JP CFMI           ;COUNT FREE PAGES


;SERIAL I/O SRS FOR OPENTYPE FILES
;COPIED TO 4BA0H

SERDT  DB 40H                ;"BLOCK MOVE SUPPORTED"
       JR SERD2

       RST 08H
       DB 151                ;O/P BC FROM DE
       RET

SERD2  RST 08H
       DB HCHRWR
       RET

       RST 08H
       DB HCHRRD
       EXX
       PUSH BC
       POP AF
       RET

       NOP

;4BB0H
;PRINT TOKENS

PVECT:     CP 247
           RET C

           POP HL
           LD HL,(XPTR)
           RST 08H
           DB 169            ;HPTV - DOS PRINT TOKEN
           RET


;EVALUATOR PATCH

EVV:       CP 3FH-1AH
           JR Z,EVV2         ;RET UNLESS "LENGTH"

           CP 3BH-1AH
           RET NC            ;DEAL WITH <3BH (PI)

EVV2       POP HL
           RST 08H
           DB 172            ;HKLEN - NO RETURN

;CMDV PATCH FOR SLVM

SLVP       LD HL,SYSP        ;7FF0H
           LD (DOSSTK),HL
           CP 94H            ;SAVE
           RET C             ;RET IF LESS THAN "SAVE"

           CP 98H
           JR NC,RCP         ;JR IF NOT SLMV

           POP HL

           RST 08H
           DB 173            ;HSLMV - "RET" TO MODIFIED CODE IN BUFFER

RCP        CP 0B3H           ;CLEAR
           JR Z,RCP2

           CP 0B0H           ;RUN
           RET NZ

RCP2       PUSH AF
           RST 08H
           DB 174            ;RUN/CLEAR PATCH
           POP AF
           RET

;NET PATCHES

HVEP       AND A             ;NC
           DB 3EH            ;"JR+1"

HLDP       SCF

           EXX
           LD A,0FFH
HLVTG      JP 0              ;ROM LOAD/VERIFY

DTEND:

;O/P ADDR USED WHEN CHAR AFTER FF IS PRINTED
;AT XTRA

PFV        RST 08H
           DB 170            ;HPFF
           EXX
           PUSH BC
           POP AF
PFTRG      JP C,0            ;JP POSTFF IF DOS DIDN'T HANDLE IT

           RET               ;JUST RET IF DONE IT

CMR3       OUT (251),A       ;ORIG URPORT
           LD A,C            ;ORIG A
           EXX
           RET

L58D4      EX AF,AF'
           POP IY
           LD (DOSSTK),IY
           POP AF            ;ORIG LRPORT
           POP IY            ;ORIG SP
           DI
           LD SP,IY
           JP 0286H          ;OUT (250),A: EI: EX AF,AF': RET

;MATCH TOKENS

MTV:       RST 08H
           DB 171
           EXX
           PUSH BC
           POP AF
           RET Z

           RET NC            ;NEEDED?

           JP 4F00H          ;EXTENSION TO DEAL WITH FNS

LDEND





