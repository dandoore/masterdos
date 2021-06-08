
;C11

COMMP  PUSH AF
       LD   A,(DSC)
       LD   C,A
       POP  AF
       RET

TRCKP  CALL COMMP
       INC  C
       RET

COMMR  PUSH BC
       CALL COMMP
       IN   A,(C)
       POP  BC
       RET

;RETURN Z IF PGES1/DE IS ZERO

CKDE   LD   A,D
       OR   E
       RET  NZ

       LD   A,(PGES1)
       AND  A
       RET  Z

       DEC  A
       LD   (PGES1),A
       LD   DE,16384
       JR   CKDE

;PRECOMPENSATION CALCULATOR

PRECMX LD   C,DWSEC

PRECMP CALL TSTD             ;GET TRACKS ON DISC
       RRA
       AND 3FH               ;** MASK TOP BIT (SIDEDNESS), HALVE TRK/SIDE
       LD   B,A              ;USUALLY 40D
       LD   A,D
       AND  7FH
       SUB  B                ;E.G. TRACK 0-40=CY, SO JR LEAVING BIT 1 SET
                             ;(PRECOMP=DISABLED)
                             ;E.G. TRACK 70-40=NC, NO JR, RES 1=ENABLE

       JR   C,SADC           ;JR IF CURRENT TRK IN D IS AN INNER TRACK

       RES  1,C              ;ELSE TURN *ON* PRECMP

;SEND A DISC COMMAND

SADC   CALL BUSY
SDCX   LD   A,C
       CALL COMMP
       OUT  (C),A
       LD   B,20
SDC1   DJNZ SDC1
       RET


DWAIT      CALL TIRD
           RET NC

           LD C,0C0H         ;READ ADDRESS CMD CODE
           CALL SADC         ;SPIN UP DISC IN CASE IT IS OFF
                             ;(OR STEP IN/OUT WILL START DRIVE, THEN WSAD WILL
                             ;WRITE TOO SOON 'COS "DRIVE RUNNING")
                             ;WAIT TILL FINISHED CMD (ALTERS SECT REG)
;TEST FOR CHIP BUSY

BUSY   CALL COMMR
       BIT  0,A
       RET  Z
       CALL BRKTST
       JR   BUSY

;WRITE SECTOR IF ALTERED

WRIF       CALL GTNSR        ;GET DE=CUR SECTOR

WRIF2      BIT 3,(IX+MFLG)
           RET Z             ;RET IF SECTOR HAS NOT BEEN WRITTEN TO

NWSAD  CALL DWAIT

;WRITE SECTOR AT DE

WSAD   CALL TIRDXDCT
       JP NC,RDWSCT

       DI

WSA1   CALL CTAS
       CALL PRECMX           ;WRITE SECTOR CMD
       LD A,(DSC)
       LD (WSA3+1),A         ;SELF-MOD STATUS PORT
       ADD A,3
       LD C,A                ;DATA PORT
       CALL GTBUF
       CALL WSA3
       RLCA
       BIT  6,A
       JP NZ,REP23           ;ERROR IF WRITE-PROTECTED

       CALL CDEC
       JR   WSA1

WSA2   OUTI

WSA3   IN A,(0)
       RRCA
       RET NC                ;RET IF DONE

       RRCA
       JR C,WSA2             ;JR IF DISK READY FOR BYTE

       JR WSA3


RSADSV LD DE,(SVDE)

;READ SECTOR AT DE

RSAD   CALL TIRDXDCT
       JP NC,RDRSCT

RSA1   CALL RSSR
       CALL RDDATA
       RLCA
       CALL CDEC
       JR   RSA1

RDDATA LD A,(DSC)           ;STATUS PORT
       LD (RSA3+1),A
       ADD A,3
       LD (RSA2+1),A
       JR   RSA3

RSA2   IN A,(0)
       LD (HL),A
       INC HL

RSA3   IN A,(0)
       RRCA
       RET NC                ;RET IF READ SECTOR CMD FINISHED

       RRA
       JR NC,RSA3            ;JR IF NO BYTE IS READY TO READ

       JR RSA2


;OPTIONAL RSAD OR NRSAD (IF SAM WANTED)

ORSAD  BIT 5,(IX+4)
       JR Z,RSAD

;READ SECTOR AT DE

NRSAD  CALL TIRDXDCT
       JP NC,NRDRSCT

NRSA1  CALL RSSR
       EXX
       LD L,0FFH
       LD D,NSAM/256
       EXX
       PUSH DE
       CALL NRDDATA
       POP DE
       RLCA
       CALL CDEC
       JR  NRSA1


NRDDATA LD A,(DSC)           ;STATUS PORT
       LD (TGT1+1),A
       LD (NRSA3+1),A
       ADD A,3
       LD (NRSA2+1),A
       LD B,2                ;MASK FOR BIT 1
       JR  NRSA3

NRS22  LD H,D                ;NSAM MSB
       AND A                 ;Z IF DIR ENTRY IS ERASED OR UNUSED
       JR NZ,NRS25

       LD H,A                ;DUMP DATA TO ROM

NRS25  EXX

TGT1   IN A,(0)
       AND B
       JR Z,NRSA3           ;JR IF NO BYTE IS READY TO READ

NRSA2  IN A,(0)
       LD (HL),A
       INC HL
       EXX
       INC L
       JR Z,NRS22            ;JR IF START OF DIR ENTRY

       OR (HL)
       LD (HL),A
       EXX

NRSA3  IN A,(0)
       RRCA
       RET NC                ;RET IF READ SECTOR CMD FINISHED

       RRA
       JR NC,NRSA3           ;JR IF NO BYTE IS READY TO READ

       JR NRSA2

;SEARCH VERSION OF RSAD
;ENTRY: DE=T/S, (TEMPW1)=DELIM COUNT, (DELIM)=DELIM
;EXIT: TEMPW3 HOLDS NEW DELIM COUNT, TEMPW2=LOCN

SRSAD  CALL TIRDXDCT
       JP NC,RDSSAD

SRSA1  CALL RSSR
       LD A,(DSC)            ;STATUS PORT
       LD (SRSA3+1),A
       ADD A,3
       LD (SRSA2+1),A
       PUSH DE
       LD DE,(TEMPW1)        ;DELIM COUNT
       LD A,(DELIM)
       LD B,A
       CALL SRSA3
       LD (TEMPW3),DE        ;NEW COUNT
       POP DE
       RLCA
       CALL CDEC
       JR  SRSA1

SRSA2  IN A,(0)
       LD (HL),A
       INC HL
       CP B
       JR Z,SRSA4

SRSA3  IN A,(0)
       RRCA
       RET NC                ;RET IF READ SECTOR CMD FINISHED

       RRA
       JR NC,SRSA3           ;JR IF NO BYTE IS READY TO READ

       JR SRSA2

SRSA4  DEC DE
       LD A,D
       OR E
       JR NZ,SRSA3

       LD (TEMPW2),HL
       JR SRSA3


;CHECK DISC ERR COUNT

CDEC   AND  1CH
       JR   NZ,CDE1          ;JR IF AN ERROR WAS DETECTED

       CALL CLRRPT
       POP  HL               ;JUNK RET ADDR
       JP   GTBUF

CDE1   PUSH AF
       LD   A,(DCT)
       INC  A
       LD   (DCT),A
       CP   10
       JP   NC,REP4

       POP  AF
       BIT  4,A
       JR   NZ,CTSL          ;JR IF RECORD NOT FOUND

       CALL INSTP
       CALL OUTSTP
       CALL OUTSTP
       JP   INSTP


;CONFIRM TRACK/SECTOR LOCATION

CTSL   LD   C,RADD
       CALL SADC
       LD   HL,DST
       CALL RDDATA
       AND  1CH
       JR   NZ,CTS1          ;JR IF ERROR

       CALL TRCKP
       LD   A,(DST)
       OUT  (C),A
       RET

CTS1   LD   A,(DCT)
       INC  A
       LD   (DCT),A
       CP   8
       JP   NC,REP5

       AND  2
       JR   Z,CTS2

       PUSH DE
       CALL REST
       POP  DE
       JR   CTSL

CTS2   CALL INSTP
       JR   CTSL

;AHL=START, CDE=LEN

HLDPG  OUT (251),A

;LOAD FILE ALREADY OPENED BY HGTHD. HL=START, CDE=LEN, PAGED IN.

HLOAD  LD BC,4BB0H+HLDP-PVECT
       CALL NETPA
       CALL DSCHD
       CALL LDBLK

;SEEK LAST TRACK IN DIRECTORY, OR TRACK 3 IF LAST TRACK IS 4 (AVOID 1ST FILE!)
;USED TO LEAVE HEAD SOMEWHERE MORE SAFE IN CASE OF RESET

SKSAFE     CALL TIRD
           RET NC

           LD A,(DTKS)
           DEC A
           LD D,A
           LD E,1            ;PROB NOT NEEDED
           CP 4
           JR NZ,SEEKD

           DEC D
           JR SEEKD

;CONFIRM TRACK AND SEEK

CTAS   LD   A,D
       OR   E
   ;   JR   NZ,CTA1

  ;    CALL BITF2
       JP   Z,REP27          ;END OF FILE

;      LD   SP,(ENTSP)
 ;     XOR  A
  ;    LD   E,A
   ;   RET

CTA1   CALL BCC              ;BORDER COLOUR CHANGE
       EXX

HSKTD  EXX

;SEEK TRACK D. USED BY FORMAT, SKSAFE

SEEKD  CALL SELD
       INC  A
       INC  A
       LD   C,A              ;SECTOR PORT
       OUT  (C),E

CTA2   LD   A,D
       AND  7FH
       LD   B,A
       CALL BUSY
       CALL TRCKP
       IN   A,(C)
       CP   B
       RET  Z

;CHECK IF PAGE OVER C000H EVERY TIME TRACK ALTERS

       PUSH AF
       CALL BITF6
       JR   Z,CTA3           ;JR IF NOT LOAD/SAVE BLOCK

CTA25  LD   A,(SVHL+1)
       CP   0C0H
       JR   C,CTA3

       RES  6,A
       LD   (SVHL+1),A
       IN   A,(251)
       INC  A
       LD   (PORT1),A
       OUT  (251),A

CTA3   POP  AF
       CALL NC,OUTSTP
       CALL C,INSTP
       JR   CTA2

OUTSTP LD   C,STPOUT
       JR   STEP

INSTP  LD   C,STPIN

STEP   CALL SADC

;STEP DELAY ROUTINE

STPDEL PUSH HL
       LD   HL,STPRAT
       LD   A,(DSC)
       BIT  4,A
       JR   Z,STPD1

       INC  HL
STPD1  LD   A,(HL)
       POP  HL
       AND  A

STPD2  RET  Z

STPDX  PUSH AF
       LD   BC,150

STPD4  DEC  BC
       LD   A,B
       OR   C
       JR   NZ,STPD4

       POP  AF
       DEC  A
       JR   STPD2


;RESTORE DISC DRIVE

RESTX
REST   LD   DE,0001H
       CALL TIRD
       RET NC

       CALL TFIHO            ;TEST FOR INDEX HOLE
       JP Z,REP6             ;"CHECK DISC IN DRIVE" IF NO HOLE

;TEST FOR TRACK 00

RSLP4  CALL COMMR
       BIT  2,A
       JP   NZ,BUSY

;STEP OUT ONE TRACK

       CALL OUTSTP
       JR   RSLP4

;TEST FOR INDEX HOLE
;EXIT: NZ=OK, Z=TIMED OUT, NO HOLE

TFIHO  CALL SELD
       LD   C,0D0H
       CALL SDCX             ;RESET DISC CHIP
       LD   B,0

TFIHL  DJNZ TFIHL

       LD   H,80H            ;LOOPS BEFORE TIME-OUT

TFI2   CALL COMMR
       BIT  1,A
       JR Z,TFI3

       DEC  HL
       LD   A,H
       OR   L
       JR NZ,TFI2

       RET

TFI3   CALL COMMR
       BIT  1,A
       RET NZ

       DEC  HL
       LD   A,H
       OR   L
       JR NZ,TFI3

       RET

;FORMAT...TO... SR

FTOSR  CALL TIRD
       JP C,EXDAT

REP10  CALL DERR
       DB 91                 ;INVALID DEVICE IF RAM DISC

HFWCD  LD A,(HKA)            ;DRIVE
       CALL CODN             ;CONVERT
       JR CKDRX

;CHECK VALID SPECIFIER DISC

CKDISC LD   A,(LSTR1)
       CP   "D"
       JR NZ,REP10           ;"INVALID DEVICE"

;CHECK DRIVE NUMBER

CKDRV  LD   A,(DSTR1)

CKDRX  CP   1
       JR   Z,CKDV1

       CP   2
       JR   Z,CKDV0

       DEC A
       CP RDLIM-1
       JP NC,REP22

       INC A                 ;ALLOWS 3 TO RDLIM-1
       JR CKDV1

CKDV0  LD   A,(RBCC+2)
       CP   0
       JP   Z,REP22          ;"NO SUCH DRIVE"

       LD A,2

CKDV1  LD   (DRIVE),A
       RET

;SERIAL SET DRIVE, RETURN CURRENT ONE IN C

SSDRV  LD A,(DRIVE)
       LD C,A
       LD A,(IX+MDRV)

SSDRV2 LD (DRIVE),A

;SELECT DISC AND SIDE - GET PORT BASE ADDR (224/228/240/244 FOR DISC 1 SIDE 1/2,
;DISC 2 SIDE 1/2) IN A AND DSC VAR

SELD   CALL TIRD             ;TEST IF RAM DISK
       RET NC                ;RET IF IT IS

       CP   2
       LD   B,11100000B      ;224
       JR   NZ,SEL1

       LD   B,11110000B      ;240

SEL1   LD   A,D
       AND  80H
       JR   Z,SEL2

       LD   A,00000100B

SEL2   OR   B
       LD   (DSC),A
       RET

;CONVERT T/S IN D/E INTO NUMBER IN BC. A=C

CONM   PUSH HL
       LD H,0
       LD L,D                ;HL=TRK
       LD B,H
       LD C,L
       ADD HL,HL             ;*2
       ADD HL,HL             ;*4
       ADD HL,BC             ;*5
       ADD HL,HL             ;*10
       LD C,E
       DEC C                 ;0-9
       LD A,D
       CP 4                  ;NC IF ON TRACK 4 OR MORE
       ADC HL,BC             ;HL=SECTOR (1 OR MORE, SUB 1 IF T4 OR MORE)
       ADD HL,HL             ;FILE NUMBER DIV 2+1
       DEC HL                ;FILE NUMBER DIV 2
       LD C,(IX+RPTH)        ;DIR ENTRY 0/1
       ADD HL,BC             ;FILE NUMBER
       LD B,H
       LD C,L
       LD A,C
       POP HL
       RET

;TEST FOR BUFFER FULL

TFBF   CALL GRPNT
       LD   A,C
       CP   254
       RET  NZ
       LD   A,B
       DEC A
       RET


;LOAD DATA BLOCK FROM DISK TO HL.

HLDBK  LD (PGES1),A
       EXX

;PGES1/DE=COUNT

LDBLK  CALL SETF6
       JR   LBLOK

LDB1   LD   A,(HL)
       CALL INCRPT
       LD   HL,(SVHL)
       LD   (HL),A
       INC  HL
       DEC  DE

LBLOK  LD   (SVHL),HL
       CALL CKDE
       RET  Z

LDB2   CALL TFBF
       JR   NZ,LDB1

       LD   (SVDE),DE
       LD   D,(HL)
       INC  HL
       LD   E,(HL)
       DI

LDB3   CALL CCNT
       JP   C,LDB8

       INC  HL
       LD   (SVDE),HL
       CALL TIRD
       JR C,LDB35

       CALL RDLB             ;RAM DISC
       JR LDB3               ;DE=T/S, SVHL=OK

LDB35  XOR  A
       LD   (DCT),A
       CALL SVNSR

LDB4   CALL CTAS
       LD   C,DRSEC
       CALL SADC
       EXX
       LD   DE,2
       CALL GTBUF
       LD A,(DSC)
       LD (LDB6+1),A         ;STATUS PORT
       ADD A,3
       LD (LDB5+1),A         ;DATA PORT
       EXX
       LD   DE,510
       LD   HL,(SVHL)
       JR   LDB6

LDB5   IN A,(0)
       LD (HL),A
       INC HL
       DEC  DE
       LD   A,D
       OR   E
       JR   Z,LDB65

LDB6   IN   A,(0)
       BIT  1,A
       JR   NZ,LDB5

       RRCA
       JR   C,LDB6

       AND  0DH
       JR   Z,LDB7

       CALL GTNSR
       CALL CDE1
       JR   LDB4

LDB65  EXX
       JR LDB6

LDB7   LD   (SVHL),HL
       CALL GTBUF
       LD   D,(HL)
       INC  HL
       LD   E,(HL)
       JP   LDB3

LDB8 ; CALL LDINT
       CALL RSAD
       LD   DE,(SVDE)
       JP   LDB2


;CALCULATE COUNT

CCNT   LD   HL,(SVDE)        ;BYTES LEFT IN THIS 16K BLOCK
       LD   BC,510
       SCF
       SBC  HL,BC
       RET  NC               ;RET IF 511 OR MORE LEFT

       LD   A,(PGES1)
       AND  A
       SCF                   ;SIGNAL "LAST SECTOR NOW"
       RET Z                 ;RET IF IT'S TRUE

       DEC  A
       LD   (PGES1),A        ;DEC PAGES TO DO
       LD   HL,(SVDE)
       LD   BC,16384
       ADD  HL,BC
       LD   (SVDE),HL        ;ADJUST REMAINDER
       JR   CCNT


;GET SCREEN MEMORY AND POINTER

GETSCR IN   A,(251)
       LD   (PORT1),A
       LD   A,(PORT2)
       OUT  (251),A
       LD   HL,(PTRSCR)
       RET

;PUT SCREEN MEMORY AND POINTER

PUTSCR LD   (PTRSCR),HL
       LD   A,(PORT1)
       OUT  (251),A
       RET

;REAL HOOK

HKSB   LD (PGES1),A
       EXX

;SAVE DATA BLOCK ON DISC. LEAVE LAST SECTOR IN BUFFER, PADDED WITH
;SPACES, BUT WITH RPT PTING PAST LAST REAL DATUM
;SAVE PGES1/DE BYTES FROM HL

HSVBK  CALL DDEL             ;DELAY 0.25 SEC IN CASE OFSM TOO FAST

HSVB2  CALL SBLOK
       RET C                 ;RET IF SINGLE SECTOR IN BUFFER, PADDED

       CALL SVNSR            ;SAVE DE TO NEXT T/S VARIABLES
       LD DE,(SVDE)          ;BYTES LEFT TO DO
       LD HL,(SVHL)
       JR HSVB2

;DELAY, SAVE BLOCK

DSVBL  CALL DDEL             ;DELAY ABOUT 0.25 SEC IF NOT RAMDISC

;SAVE DATA BLOCK ON DISC
;SAVE PGES1/DE BYTES FROM HL, PADDING LAST SECTOR WITH SPACES

SVBLK  CALL SBLOK
       JP C,WSAD             ;WRITE SINGLE SECTOR

       JP SVBF               ;OR LAST SECTOR OF BLOCK (HL=0)

SVB1   LD   (HL),D
       CALL INCRPT           ;INC IX+RPTL/H
       LD   HL,(SVHL)
       INC  HL
       POP  DE
       DEC  DE               ;DEC BYTES-TO-SAVE

;TEST FOR ZERO BLOCK COUNT

SBLOK  CALL SETF6
       CALL CKDE
       JP Z,SVBSI            ;JP IF ALL BYTES NOW IN BUFFER - PAD BUFFER
                             ;AND SAVE IT

;SAVE CHAR IN REGISTER D

       PUSH DE
       LD   D,(HL)

       PUSH DE
       CALL GETSCR           ;SCREEN PAGED IN
       LD   HL,FTADD
;      LD (HL),D
       LD   BC,0             ;SECT COUNT=0
       JR   SVB2A

SVB2   PUSH HL               ;SCRN PTR
       CALL CCNT             ;CHECK BYTES REMAINING, SUB 511
       EX DE,HL
       POP  HL               ;SCRN PTR
       JR   C,SVB3           ;JR IF 510 OR LESS BYTES TO GO - START SAVING
                             ;SECTORS
       INC  DE               ;CORRECT FOR SUB 511 ->SUB 510 NOW
       LD   (SVDE),DE
 ;     LD D,(HL)
       CALL FFNS             ;FIND NEXT FREE SECT IN SAM
       LD   (HL),D
       INC  HL
       LD   (HL),E           ;STORE IN SCREEN TABLE
       INC  HL
  ;    LD (HL),D
       LD   BC,(SVCNT)
       INC  BC               ;INC SECTOR COUNT

SVB2A  LD   (SVCNT),BC
       JR   SVB2

SVB3   XOR A
       LD (HL),A
       LD D,H
       LD E,L
       INC DE
       LD BC,01FFH           ;ALLOW UP TO 512 BYTES OF ZEROS FOR LAST SECTOR
       LDIR

       LD   HL,FTADD
       CALL PUTSCR           ;SET PTR VAR TO START OF LIST OF T/S, SWITCH SCREEN
                             ;OFF
       POP DE
       CALL WSAD             ;WRITE FIRST SECTOR, SETUP DSC
       DI
       XOR A
       LD   (DCT),A          ;ZERO ERRORS
       CALL GTNSR            ;GET T/S IN D/E

       EXX
       LD HL,(PTRSCR)        ;SOURCE OF LAST 2 BYTES (OR MORE, IF LAST SECT)
       EXX

;LOOP HERE FOR EACH SECTOR IN SVCNT

SVB4   LD HL,(SVCNT)
       LD A,H
       OR L
       RET Z                 ;RET IF LAST TO DO NEXT. NC

       BIT 7,H
       RET NZ                ;RET IF NEGATIVE (FINAL SECTOR JUST DONE)

SVBF   DEC HL
       LD (SVCNT),HL
       PUSH DE               ;T/S
       CALL CTAS             ;CONFIRM TRACK
       CALL PRECMX           ;SEND "WRITE SECTOR" CMD
       LD A,(PORT2)          ;SCREEN PORT
       EX AF,AF'             ;STORED IN A' FOR SPEED
       LD A,(DSC)            ;STATUS PORT
       LD (SVB6+1),A         ;SELF-MOD CODE
       ADD A,3
       LD C,A                ;DATA PORT
       EXX
       LD C,A                ;C'=DATA PORT TOO
       LD DE,0300H           ;DE IS A LARGE NUMBER SO EXX HAPPENS JUST ONCE
       EXX
       LD DE,02FEH           ;510 BYTE COUNTER
       LD HL,(SVCNT)
       INC HL
       LD A,H
       OR L
       JR NZ,SVB45           ;JR IF NOT LAST SECT

       LD   DE,(SVDE)        ;REMAINING BYTES
       LD A,E
       AND A
       JR Z,SVB45            ;NO INC IF E=0 SO COUNTER OK

       INC D

SVB45  LD   HL,(SVHL)        ;SRC
       JR   SVB6

SVB5   OUTI
       DEC E                 ;LSB OF BYTE COUNT
       JR Z,SVBL2

SVB6   IN   A,(0)            ;SELF-MOD STATUS PORT
       BIT  1,A
       JR   NZ,SVB5          ;JR IF DISK READY FOR BYTE

       RRCA
       JR C,SVB6             ;JR IF BUSY - DATA STILL TO BE SENT

       EXX                   ;HL=MAIN PTR
       POP DE                ;T/S
       AND  0DH
       JR  NZ,SVB7           ;JR IF ERROR

;SCREEN IS STILL IN

       LD   (SVHL),HL        ;UPDATE SRC PTR (IN 510 BYTE STEPS)

       EXX
       DEC  HL
       LD   E,(HL)
       DEC  HL
       LD   D,(HL)           ;D/E=BYTES JUST SAVED (NEXT T/S)
       INC HL
       INC HL
       LD   A,(PORT1)
       OUT  (251),A          ;NON-SCREEN
       PUSH DE
       EXX

       POP DE                ;T/S
       JP SVB4               ;LOOP FOR ALL SECTORS


;ENTERED WHEN AN ERROR OCCURRED DURING SAVE

SVB7   LD   A,(PORT1)
       OUT  (251),A          ;NON-SCREEN
       CALL CDE1             ;CHECK/SET T/S
       EXX
       DEC HL
       DEC HL
       EXX
       JP   SVB4             ;TRY AGAIN - MAX 10 ERRORS IN ENTIRE BLOCK

;ENTERED WHEN E COUNTS TO 0

SVBL2  DEC D                 ;DEC MSB OF COUNT (PRE-INCED)
       JR NZ,SVB6

       EXX                   ;SWITCH TO HL' - PTS TO T/S OR ZEROS
       EX AF,AF'
       OUT (251),A           ;SCREEN ON
       JR SVB6

;PAD A SINGLE SECTOR FOR A SMALL BLOCK

SVBSI  CALL GRPNT

SVBS1  LD   A,B
       SUB 2
       OR C
       JR   Z,SVBS3

SVBS2  LD   (HL),0           ;PAD WITH ZEROS
       INC HL                ;NO CHANGE OF RPT
       INC BC
       JR   SVBS1

SVBS3  CALL GTNSR            ;GET T/S
       SCF                   ;"SINGLE SECTOR"
       RET





