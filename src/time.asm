
;TIME

DATE       LD HL,DATDT
           DB 0FDH           ;"JR+3"

TIME       LD HL,TIMDT

TIMDC      LD (TDVAR),HL
           CALL RDCLK        ;UPDATE BUFFERS
           CALL GTNC
           CALL CIEL
           JR Z,TIMPR        ;JR IF PRINTING OF DATA, NOT SETTING DATA

           CALL EVSTR        ;DE=START, BC=LEN
           CALL CEOS

           LD A,(SVC)
           CALL SELURPG      ;PAGE STRING IN
           LD HL,(TDVAR)
           LD B,6

TIML       LD A,C
           AND A
           LD A,"0"
           JR Z,TIM1         ;PAD WITH ZEROS ONCE STRING LEN=0

           LD A,(DE)
           INC DE
           DEC C
           CALL NUMERIC
           JR NC,TIML        ;LOOP IF NOT A DIGIT

TIM1       LD (HL),A

TIM2       INC HL
           LD A,(HL)
           CALL NUMERIC
           JR NC,TIM2        ;LOOP IF NOT A DIGIT

           DJNZ TIML         ;LOOP PLACING 6 NUMBERS FROM STRING, OR PADS

           CALL DTVCK        ;CHECK VALUES
           JP C,IOOR

;WRITE DATE AND TIME TO CLOCK HARDWARE FROM BUFFERS

WRCLK      LD IY,SCTWO
           JR CKHW

TIMPR      CALL CEOS

           LD A,2
           CALL CMR
           DW STREAM
           LD B,9
           LD HL,(TDVAR)

;PRINT B FROM HL - USED TO PRINT DATE/TIME/PATH NAME

PBFHL      LD A,(HL)
           INC HL
           CALL PNT
           DJNZ PBFHL

           RET

;READ CLOCK HARDWARE INFO INTO DATE AND TIME BUFFERS

RDCLK      LD IY,PLTWO

CKHW       LD A,(CKPT)       ;CLOCK PORT
           AND A
           RET Z             ;ABORT IF NO CLOCK!

           DI                ;CRASHES IF NOT...
           PUSH HL
           LD C,A
           LD B,0D0H         ;CONTROL REGISTER
           LD HL,2000

CKLP       LD A,1
           OUT (C),A         ;HOLD ON
           IN A,(C)
           BIT 1,A
           JR Z,CKLX         ;EXIT IF NOT BUSY

           XOR A
           OUT (C),A         ;HOLD OFF
           DEC HL
           CP H
           JR NZ,CKLP

           POP HL            ;GIVE UP - NO CLOCK??
           RET

CKLX       LD B,50H          ;HOURS-H REGISTER
           LD HL,TIMDT
           CALL IYJUMP       ;HANDLE HOURS
           CALL IYJUMP       ;HANDLE MINS
           CALL IYJUMP       ;HANDLE SECS
           LD HL,DATDT
           LD B,70H          ;DAY-H
           CALL IYJUMP       ;DAY
           LD B,90H          ;MTH-H
           CALL IYJUMP       ;MONTH
           LD B,0B0H         ;YR-H
           CALL IYJUMP       ;YEAR
           XOR A
           LD B,0D0H
           OUT (C),A         ;HOLD OFF
           POP HL
           RET

;ENTRY: HL PTS TO DEST, B=CLK REGISTER, C=PORT
;EXIT: HL INCED BY 3, REGISTER DECED BY 2, 2  DIGITS PLACED

PLTWO      CALL PLONE
           DEC HL

PLONE      IN A,(C)          ;READ DIGIT
           AND 0FH
           ADD A,30H
           CP 3AH
           JR NC,SCPLC       ;ENSURE HARDWARE ERRORS DO NOT PLACE A NON-DIGIT

           LD (HL),A
           JR SCPLC

;ENTRY: HL PTS TO SRC, B=CLK REGISTER, C=PORT
;EXIT: HL INCED BY 3, REGISTER DECED BY 2, 2  DIGITS SENT TO CLOCK

SCTWO      CALL SCONE
           DEC HL

SCONE      LD A,(HL)         ;READ DIGIT
           SUB 30H
           OUT (C),A

SCPLC      INC HL
           INC HL
           LD A,B
           SUB 10H
           LD B,A            ;NEXT PORT
           RET

;DATE/TIME SR TO CHECK VALUES
;NC IF OK, CY IF >LIMIT

DTVCK      LD DE,(TDVAR)
           LD HL,9
           ADD HL,DE         ;DE PTS TO DATA TO CHECK, HL TO LIMIT
           LD B,3            ;VALUES TO CHECK

VCLP       CALL GDTV         ;GET VALUE IN A
           LD C,A
           LD A,(HL)
           INC HL
           CP C
           RET C             ;RET IF VALUE>HI LIMIT

           LD A,C
           CP (HL)
           RET C             ;RET IF VALUE<LOW LIMIT

           INC HL
           DJNZ VCLP

           RET

;CALLED BY CFSM TO DATE STAMP FILE DIR ENTRY

DATSET     CALL NRRD
           DW CURCMD
           CP 0CFH           ;COPY
           RET Z

           PUSH DE
           CALL RDCLK
           CALL POINT
           LD BC,0F5H
           ADD HL,BC         ;POSN F5 IN DIR ENTRY IS DDMMYYHHSS (BCD)
           LD DE,DATDT       ;SRC
           LD B,3            ;3 PAIRS OF DIGITS

SDTL1      CALL GDTV         ;GET VALUE IN A
           AND A
           JR Z,SDTL3        ;ABORT IF DY, MTH OR YEAR=0 (2000? PITY)
                             ;INITIAL VALUE IS 00/00/00 SO NO STAMPING
           LD (HL),A
           INC HL
           DJNZ SDTL1

           LD DE,TIMDT
           LD B,2            ;(HRS/MINS)

SDTL2      CALL GDTV         ;GET VALUE IN A
           LD (HL),A
           INC HL
           DJNZ SDTL2

SDTL3      POP DE
           RET

;GET DATE/TIME VALUE IN A FROM (DE) AND (DE+1)
;ADVANCES DE BY 3, USES C

GDTV       LD A,(DE)         ;MS DIGIT
           INC DE
           SUB 30H
           LD C,A
           ADD A,A
           ADD A,A
           ADD A,C
           ADD A,A           ;*10

           LD C,A
           LD A,(DE)         ;LS DIGIT
           INC DE
           INC DE            ;SKIP SEPARATOR
           SUB 30H
           ADD A,C
           RET

;PRINT DATE/TIME AS DD/MM/YY HH:MM

PNDAT      LD B,0F5H
           CALL GRPNTB       ;PT HL TO DATE
           LD A,(HL)
           INC A             ;DOS 2.0 FILES HAVE FF ->00
                             ;G+DOS FILES HAVE 00 ->01
                             ;DOS 3.0 FILES HAVE 01 OR MORE ->02
           CP 2
           RET C             ;RET IF NO DATE

           LD C,"/"
           CALL PSIX         ;DATE, WITH "/" SEPARATOR
           LD C,":"
           LD A," "
           JR PFOUR          ;TIME, WITH ":" SEPARATOR


;PRINT SIX DIGITS FROM 3 BYTES AT (HL) AS NN (SEPARATOR) NN (SEPARATOR) NN
;ENTRY: C=SEPARATOR

PSIX       CALL PTWO
           LD A,C

PFOUR      CALL PNT
           CALL PTWO
           LD A,C
           CALL PNT

PTWO       PUSH HL
           PUSH DE
           PUSH BC
           LD L,(HL)
           LD H,0
           LD A,"0"
           CALL PNUM2
           POP BC
           POP DE
           POP HL
           INC HL
           RET

           END
