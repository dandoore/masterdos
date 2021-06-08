
;RAMD

;RAM DISC WRITE SECTOR AT DE. D=TRACK, E=SECTOR.

RDWSCT     CALL GTBUF        ;GET SRC ADDR IN HL. MAY BE 8000H+ IF WRITE AT
                             ;OR SAVE BLOCK
           LD A,DRAM/256
           CP H
           JR Z,RDW2         ;JR IF SRC=DRAM (FIRST SECTORS, SERIAL FILES)

           PUSH DE           ;T/S
           LD DE,DRAM
           LD BC,0200H
           PUSH DE
           LDIR
           POP HL            ;SRC=DRAM SO PAGING OF DEST POSSIBLE
                             ;(used by WRITE AT)
           POP DE            ;T/S

RDW2       PUSH DE           ;T/S
           PUSH HL           ;SRC
           CALL RDADR        ;GET RD ADDR IN HL
           EX DE,HL          ;DEST IN RD INTO DE
           POP HL            ;SRC IN RAM
           LD BC,0200H
           LDIR

RDW3       OUT (251),A

RDW4       POP DE            ;T/S
           CALL CLRRPT
           JP GTBUF

;RAM DISC SEARCH SECTOR AT DE
;ENTRY: DE=T/S, (TEMPW1)=DELIM COUNT, (DELIM)=DELIM
;EXIT: TEMPW3 HOLDS NEW DELIM COUNT, TEMPW2=LOCN
;COULD BE FASTER IF SECTOR ONLY READ TO BUFFER WHEN MATCHED AND COUNT=0: ELSE
;JUST READ LAST 2 BYTES??

RDSSAD CALL RDRSCT           ;HL=DATA START
       PUSH DE
       PUSH HL
       LD DE,(TEMPW1)
       LD BC,0002H

RDS1   LD A,(DELIM)

RDSLP  CP (HL)
       INC HL
       JR Z,RDS2

RDS12  DJNZ RDSLP

       DEC C
       JR NZ,RDSLP

       JR RDS3

RDS2   DEC DE
       LD A,D
       OR E
       LD A,(DELIM)
       JR NZ,RDS12

       LD (TEMPW2),HL        ;LOCN OF MATCH AND COUNT=0

RDS3   LD (TEMPW3),DE        ;NEW COUNT
       POP HL
       POP DE
       RET

;RAM DISC PREPARE SAM FROM DIR ENTRY READ

NRDRSCT    CALL RDRSCT
           PUSH DE
           PUSH HL
           EX DE,HL
           LD HL,NSAM
           LD B,2

NRDROL     LD A,(DE)
           INC D
           AND A
           JR Z,NRDR2        ;JR IF AN ERASED ENTRY

           DEC D

NRDRL      LD A,(DE)
           OR (HL)
           LD (HL),A
           INC DE
           INC L
           JR NZ,NRDRL

NRDR2      DJNZ NRDROL

           POP HL
           POP DE
           RET


;RAM DISC READ SECTOR AT DE. D=TRACK, E=SECTOR.

RDRSCT     CALL GTBUF
           LD BC,0200H

RDRS2      PUSH DE           ;T/S
           PUSH HL           ;MAIN MEM PTR
           PUSH BC
           CALL RDADR        ;GET SRC ADDR IN HL
           LD BC,0200H
           LD DE,DRAM
           LDIR              ;COPY RAMDISC TO DRAM

           OUT (251),A       ;ORIG URPORT
           POP BC
           POP HL
           LD A,DRAM/256
           CP H
           JR Z,RDW4         ;RET IF DEST=DRAM - DONE IT

           EX DE,HL          ;DE=BUF
           LD HL,DRAM
           LDIR

           JR RDW4

;RAM DISC ADDRESS - GET HL=ADDR, PAGE SELECTED, FROM D/E (T/S)
;GET A=CUR. URPORT. BC, DE USED

RDADR      LD A,(RDAT)
           AND A
           JR Z,RDAD2

           CALL SELFP
           LD HL,(82FFH)
           OUT (251),A
           LD A,L
           ADD A,3
           CP D              ;CP DTKS-1,TRK
           JR NC,RDAD2       ;JR IF TRK IN DIRECTORY - NO FIDDLE
                             ;(ALWAYS JR ON TRACK 0 - WHEN DTKS IS OBSOLETE!)

           SUB 3
           JR NC,RDAD2       ;LEAVE TRACK ALONE IF 4 DIR TKS OR MORE
                             ;A=-1 IF DTKS=3, -2 IF 2, -3 IF 1 DTK
           ADD A,D
           LD D,A            ;E.G. TRACK 4 ACCESS BECOMES TRACK 1 IF DTKS=1
                             ;SO IF DTKS=1, DISK=40 TRK, TRK 0=0
                             ;TRKS 4-42 BECOME 1-39

RDAD2      CALL RTSTD        ;GET A=TRACKS/DRIVE
           AND A
           JP Z,REP22        ;"NO SUCH DRIVE" IF TKS=0

           LD C,A
           LD A,D
           CP C

REP4H      JP NC,REP4        ;TRK/SCT ERROR IF TRACK >=(LIMIT)

           AND 7FH
           CP 80
           JR NC,REP4H       ;ERROR IF E.G. TRK 81

           BIT 7,D
           JR Z,RDAD3        ;JR IF "SIDE 1"

           LD A,D
           SUB 48            ;128->80
           LD D,A

RDAD3      CALL CPFTS        ;GET PAGEFORM OF DISC ADDR
           PUSH HL
           CALL PTRD2        ;PT TO ENTRY "A" IN LIST
           LD L,(HL)         ;GET PAGE VALUE
           OUT (251),A
           LD A,L
           POP HL
           JR SELRDP

;SEL FIRST PAGE

SELFP      CALL GFPA
           LD A,(HL)         ;A=FIRST PAGE

;SELECT RAM DISC PAGE. ENTRY: A=PAGE. 00-1F=INTERNAL RAM, 20-FFH=EXTERNAL
;EXIT: A=ORIG URPORT VALUE

SELRDP     DI
           PUSH DE           ;SAVE THROUGHOUT
           LD D,A            ;40-7F=MEGA RAM 1,80-BF=MEGA RAM 2,C0-FF=MEGA 3
                             ;20-3F=MEGA RAM 0 SECOND HALF
           CP 20H
           JR C,SRDP2        ;JR IF MAIN RAM

           OUT (MRPRT),A     ;SELECT MEGA RAM
           LD D,80H          ;XMEM BIT HIGH

SRDP2      IN A,(251)
           PUSH AF
           LD A,D
           OUT (251),A
           POP AF            ;ORIG PAGE
           POP DE            ;ORIG
           RET

;CALCULATE PAGE FORM FROM T/S IN DE

CPFTS      LD L,D
           LD H,0
           LD B,H
           LD C,L
           ADD HL,HL
           ADD HL,HL
           ADD HL,BC
           ADD HL,HL         ;TRACK*10
           LD C,E
           DEC C
           LD A,C
           CP 10
           JP NC,REP27       ;"END OF FILE" IF SECTOR NOT 1-10

           ADD HL,BC         ;SECTOR NUMBER 0 TO DISC LIMIT-1
           PUSH HL
           LD C,31
           XOR A             ;NC
           DEC A             ;A=0FFH

DIV31L     INC A
           SBC HL,BC
           JP NC,DIV31L

           POP HL            ;SECT NO 0 TO LIMIT-1
           LD C,A
           ADD HL,BC         ;SECT=SECT+INT(SECT/31) TO AVOID EVERY 32ND SECT
           INC HL            ;AVOID SECT 0,32,64,96 ETC
           ADD HL,HL         ;HL=SECT NO.*2
           LD A,H
           LD H,L
           LD L,B            ;AHL=20-BIT DISPLACEMENT (512*SECT)
                             ;NOW GET A=PG DISP, HL=OFFSET

;TRANSFORM 20-BIT NUMBER IN AHL TO PAGE, ADDR (8000-BFFF)

PAGEFORM:  RL H
           RLA
           RL H
           RLA              ;NC. PAGE NOW OK
           RR H
           SCF
           RR H             ;ADDR NOW OK IN 8000-BFFF FORM
           RET

;RAM DISC SAVE BLOCK

RDSB       CALL WSAD         ;SAVE FIRST BLOCK
           LD   BC,0         ;SECT COUNT=0
           JR   RDSB2

RDSBL  ;   PUSH DE           ;PREV TRK
           CALL CCNT         ;CHECK BYTES REMAINING, SUB 511
           EX DE,HL
           LD HL,(SVHL)      ;SRC
           JR   C,RDSB3      ;JR IF 510 OR LESS BYTES TO GO - PREPARE LAST SCT

           INC  DE           ;CORRECT FOR SUB 511 ->SUB 510 NOW
           LD   (SVDE),DE
           LD DE,DRAM        ;TEMP STORE
           LD BC,510
           LDIR              ;COPY DATA TO TEMP BUFFER IN COMMON MEMORY

           CALL CHKHL
           LD (SVHL),HL
        ;  POP DE            ;PREV TRK
           CALL FFNS         ;FAST FIND NEXT FREE SECT IN SAM
           LD L,D
           LD H,E
           LD (DRAM+510),HL

           EX DE,HL
           CALL SWPNSR           ;GET T/S IN D/E
           CALL WSAD
           LD   BC,(SVCNT)
           INC  BC               ;INC SECTOR COUNT

RDSB2      LD   (SVCNT),BC
           JR   RDSBL

RDSB3   ;  POP DE            ;PREV TRK
           LD BC,(SVDE)
           LD (IX+RPTH),B
           LD (IX+RPTL),C
           LD DE,DRAM
           PUSH BC
           LDIR              ;LAST PART-BUFFER COPIED

           EX DE,HL          ;BUFFER DEST IN HL
           POP BC            ;PTR
           JP SVBS1

;FORMAT RAM DISC. A=DSTR1

FORMRD     CP RDLIM
           JP NC,REP22       ;"NO SUCH DRIVE"

           DI
           LD (DRIVE),A
           XOR A
           OUT (251),A       ;SYS PAGE AT 8000H
           CALL RTSTD        ;GET TKS/DRIVE
           AND A
           JR Z,FRMRD2       ;JR IF NEW RAMDISC

           CALL PTRD         ;PT TO TABLE START - 50 BYTES OF PAGES USED+00H
                             ;IN FIRST PAGE
           LD DE,DRAM
           PUSH DE
           LD BC,52
           LDIR              ;COPY TO BUFFER

           POP HL
           OUT (251),A       ;ORIG
           LD D,ALLOCT/256+40H

FRMRDL     LD A,(HL)
           INC HL
           LD E,A
           AND A             ;TERMINATOR IS 0
           JR Z,FRMRD2       ;EXIT IF ALL USED PAGES DE-ALLOCTED

           AND 0E0H
           JR NZ,FRMRD1      ;JR IF MEGA RAM PAGE

           LD (DE),A         ;FREE PAGE IN ALLOCT
           JR FRMRDL

FRMRD1     LD A,E
           CALL RMRBIT       ;FREE BIT IN MEGA RAM MAP
           JR FRMRDL

FRMRD2     LD HL,DRAM        ;PT TO TABLE START - 50 BYTES OF PAGES USED
           LD B,52
           XOR A

RDCLL      LD (HL),A
           INC HL
           DJNZ RDCLL        ;CLEAR TABLE

           LD A,(TEMPW1)     ;TKS/DISC
           AND A
           JP Z,FTKPD        ;JUST ERASE ANY EXISTING RAM DISC IF TKS=0

           LD D,A
           DEC D
           LD E,10           ;LAST SECTOR ON DISC
           CALL CPFTS        ;GET PAGE FORM OF T/S ADDR
           INC A
           LD C,A            ;PAGES NEEDED
           CALL CNTFP        ;COUNT FREE PAGES IN A AND B
           CP C
           JP C,OOMERR

           LD A,C
           PUSH AF           ;PAGES NEEDED
           LD B,0
           LD HL,DRAM
           ADD HL,BC         ;LAST REQUIRED POSN IN TABLE,+1
           POP BC            ;B=PAGES NEEDED
           LD DE,ALLOCT+FS+32

FRMRDEL    CALL SFMRP        ;SEARCH FOR FREE MR PAGE AND RESERVE IT
           JR Z,FRMRD6       ;JR IF PAGE FREE (A=PAGE)

           DEC E             ;INTERNAL RAM PAGE NO./ALLOCT PTR
           XOR A
           OUT (251),A       ;SYS PAGE IN AT 8000H
           LD A,(DE)
           AND A
           JR NZ,FRMRDEL     ;JR IF PAGE NOT FREE

FRMRD5     LD A,(DRIVE)
           OR 0D0H
           LD (DE),A         ;RESERVE IN ALLOCT
           LD A,E

FRMRD6     DEC HL            ;NEXT TABLE POSN
           LD (HL),A         ;PAGE NUMBER TO RAM DISC'S LIST
           CALL SELRDP
           EXX

           LD HL,RDCODE
           LD DE,8002H
           LD BC,RDCE-RDCODE+1
           LDIR              ;CREATE MOVER CODE IN UNUSED "SECTOR" OF EACH PAGE

           LD HL,8020H
           LD B,H            ;B=128

FTCCL      LD (HL),0EDH
           INC HL
           LD (HL),0A0H
           INC HL
           DJNZ FTCCL        ;CREATE 128 LDIs

           LD (HL),3DH       ;DEC A
           INC HL
           LD (HL),0C2H      ;JP NZ
           INC HL
           LD (HL),20H
           INC HL
           LD (HL),80H       ;8020H
           INC HL
           LD (HL),0C9H      ;RET

           LD B,3EH          ;MAX NUMBER OF POSSIBLE DIR ENTRIES IN PAGE
           LD HL,8200H       ;FIRST POSSIBLE DIR ENTRY

FZDL       LD (HL),C
           INC L
           LD (HL),C         ;ENSURE IT STARTS 00/00 SO "NEVER USED"
           DEC L
           INC H             ;NEXT ENTRY
           DJNZ FZDL

           EXX
           DJNZ FRMRDEL

           LD HL,82FFH
           CALL FESET        ;SET DTKS AND RND WORD AND NAME
           LD HL,DRAM
           LD DE,8125H       ;AFTER MULTI-LDI CODE
           LD BC,52
           LD A,(HL)
           LDIR              ;COPY PAGE TABLE FROM TEMP BUFFER TO 1ST PAGE

           PUSH AF
           CALL GFPA         ;FIRST PAGE ADDR
           POP AF
           LD (HL),A         ;SET FIRST PAGE VAR

FTKPD      CALL RTSTD        ;GET ADDR OF TKS/DISC
           LD A,(TEMPW1)     ;DESIRED TKS/DISC
           CP 80
           JR C,NRDTF

           ADD A,48          ;E.G. 80->128, 160->208

NRDTF      LD (HL),A         ;NZ SO DISC USABLE NOW, OR ZERO SO NON-EXISTENT
           XOR A
           OUT (251),A       ;ENSURE XMEM BIT LOW
           RET

OOMERR     CALL DERR
           DB 1

PTRD       XOR A

PTRD2      LD C,A
           CALL SELFP        ;SEL FIRST PAGE, EXIT WITH A=ORIG
           LD HL,8125H       ;START OF FULL TABLE
           LD B,0
           ADD HL,BC         ;PT TO REQUIRED ENTRY
           RET

;STORE RAM DISC PATH NAME

SRDPN      SCF

;MOVE RAM DISC PATH NAME
;ENTRY: (DRIVE) SET, NC IF FETCH FROM RD, CY IF STORE

MRDPN      DI
           EX AF,AF'
           CALL RTSTD        ;GET A=TRACKS/DRIVE
           AND A
           JP Z,REP22        ;"NO SUCH DRIVE" IF TKS=0

           CALL SELFP        ;SEL FIRST PAGE, EXIT WITH A=ORIG
           LD HL,8160H
           LD DE,PTHRD+2
           EX AF,AF'
           JR NC,MRDPN2

           EX DE,HL

MRDPN2     LD BC,MPL-2
           LDIR              ;COPY PATH NAME FROM RAM DISC TO/FROM TEMP BUFFER

           LD A,(DRIVE)
           ADD A,30H
           LD L,A
           LD H,":"
           LD (PTHRD),HL
           EX AF,AF'
           OUT (251),A
           RET

;RAM DISC LOAD BLOCK SR

RDLB       LD HL,(SVHL)      ;DEST
           CALL SDCHK2
           JR NC,RDLB2

           LD BC,510
           CALL RDRS2
           LD A,(DRAM+510)
           LD B,A
           LD A,(DRAM+511)
           LD C,A
           JR RDLB3

RDLB2      CALL FRDRD2       ;LOAD 510 BYTES TO DEST FROM T/S D/E

RDLB3      LD HL,(SVHL)
           LD DE,510
           ADD HL,DE
           CALL CHKHL
           LD (SVHL),HL
           LD D,B
           LD E,C            ;NEXT T/S
           JP CLRRPT


CHKHL      BIT 6,H
           RET Z

           CALL INCURPAGE
           LD   (PORT1),A
           RET


FRDRD2     PUSH DE           ;T/S
           PUSH HL           ;MAIN MEMORY PTR - DEST
           CALL RDADR        ;PT HL TO SRC IN RAMD, PAGE IN
           POP DE            ;DEST
           PUSH AF           ;ORIG PAGE
           LD C,A            ;MAIN MEM PAGE
           SCF               ;"510"
           EX AF,AF'
           DEC C             ;TO BE PAGED IN AT 4000H
           IN A,(250)
           LD B,A
           XOR C
           AND 0E0H
           XOR C             ;A=VALUE FOR PORT 250 TO PAGE IN DEST AT 4000H
           CALL 8002H
           LD B,(HL)
           INC HL
           LD C,(HL)         ;NEXT T/S
           POP AF
           OUT (251),A
           POP DE            ;CURRENT T/S
           RET


SDCHK      CALL GTBUF        ;PT HL TO BUFF (EITHER DRAM OR 8000-BFFF)

SDCHK2     LD A,H
           CP 80H
           RET C             ;RET IF HL IN DRAM

           PUSH HL
           LD BC,41FFH
           ADD HL,BC
           POP HL
           RET C             ;CY IF HL WILL CROSS PAGE BOUNDARY - USE BUFF

           RES 7,H
           SET 6,H
           RET               ;ADDR IN 4000-7FFF AREA NOW

;AT 8002H

RDCODE     LD (8000H),SP
           LD SP,8200H
           OUT (250),A       ;PAGE IN DEST IN SECTION B
           PUSH BC           ;ORIG PORT 250 VALUE
           EX AF,AF'
           LD A,4            ;4 MOVES OF 128 BYTES
           JR C,RDC1         ;JR IF 510 BYTES WANTED

           CALL 8020H        ;MOVER - DO 128*4=512
           AND A             ;NC

RDC1       CALL C,8024H      ;MOVE 1FEH BYTES

           POP AF
           OUT (250),A
           LD SP,(8000H)
RDCE       RET

;SEARCH FOR FREE MR PAGE
;EXIT: IF Z, A=PAGE NUMBER, PAGE RESERVED

SFMRP      XOR A

SFMRL      DEC A
           CALL TMRBIT
           JR Z,SMRBIT       ;JR IF FREE

           CP 20H
           JR NZ,SFMRL

           AND A             ;NZ
           RET


;TEST MEGA RAM BIT (PAGE). RETURN Z IF PAGE "A" FREE, NZ IF IN USE OR NON-
;EXISTENT. TABLE SET UP AT BOOT TIME, ALL 0FFHS IF NO MEGARAMS
;ENTRY:A=00-FF

TMRBIT     PUSH HL
           PUSH AF
           CALL MRADDR       ;GET ADDR, MASK
           AND (HL)          ;Z IF BIT LOW (FREE)
           POP HL
           LD A,H
           POP HL
           RET

;SET MEGA RAM BIT A. ALL REGS SAVED

SMRBIT     PUSH AF
           PUSH HL
           CALL MRADDR
           OR (HL)           ;NZ
           JR SRMRC

;RESET MEGA RAM BIT A. ALL REGS SAVED

RMRBIT     PUSH AF
           PUSH HL
           CALL MRADDR
           CPL               ;SET MASK BIT LOW
           AND (HL)

SRMRC      LD (HL),A
           POP HL
           POP AF
           RET

;GET MEGA RAM TABLE ADDR IN HL, MASK IN A (BIT SET HI)
;ENTRY: A=BIT
;EXIT: HL AND A SET, OTHER REGS SAVED

MRADDR     PUSH BC
           LD B,A
           RRA
           RRA
           RRA
           AND 1FH           ;DIV BY 8
           LD C,A
           LD A,B
           AND 07H
           INC A
           LD B,A            ;B=1 TO 8 FOR BIT 7-0
           LD A,1

MRBL       RRCA
           DJNZ MRBL         ;A=80H IF B WAS 1, 40H IF B WAS 2...

           LD HL,MRTAB
           ADD HL,BC         ;PT TO BYTE (B=0)
           POP BC
           RET

CNTFP      CALL CFMRP        ;GET FREE MEGA RAM PAGES IN E (0-224)
           LD B,E
           LD HL,ALLOCT+FS+32

FRMRAL     CALL RDA          ;READ A FROM SYS PAGE
           AND A
           JR NZ,FRMRD4

           INC B             ;INC FREE PG CNT

FRMRD4     DEC L
           JR NZ,FRMRAL

           LD A,B
           RET

;COUNT FREE MEGA RAM PAGES - RESULT IN DE. BC SAVED

CFMRP      LD HL,MRTAB+4     ;AVOID USE OF MEGA RAM 0 FIRST HALF
           LD E,28           ;BYTES TO LOOK AT

;CALLED BY BOOT

CFMI       PUSH BC
           XOR A
           LD D,A

CFPEL      LD C,(HL)
           INC HL
           LD B,8

CFPBL      RR C
           JR C,CFP3

           INC A             ;INC A BY NUMBER OF LOW (FREE) BITS
           JR Z,CFPF         ;JR IF HIT 256 WHEN CALLED FROM BOOT

CFP3       DJNZ CFPBL        ;DO 8 BITS

           DEC E
           JR NZ,CFPEL       ;DO 32 OR 28 BYTES

           DEC D

CFPF       INC D
           LD E,A
           POP BC
           RET



