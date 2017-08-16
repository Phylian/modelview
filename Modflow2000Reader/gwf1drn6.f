C     Last change:  AWH  05JUNE 2000
      SUBROUTINE GWF1DRN6AL(ISUM,LCDRAI,MXDRN,NDRAIN,IN,IOUT,IDRNCB,
     1        NDRNVL,IDRNAL,IFREFM,NPDRN,IDRNPB,NDRNNP,ierror)
C
C-----VERSION 11JAN2000 GWF1DRN6AL
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR DRAINS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      COMMON /DRNCOM/DRNAUX(5)
      CHARACTER*16 DRNAUX
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      ierror=0
C
C1------IDENTIFY PACKAGE AND INITIALIZE NDRAIN.
C###      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'DRN6 -- DRAIN PACKAGE, VERSION 6, 1/11/2000',
     1' INPUT READ FROM UNIT',I3)
      NDRAIN=0
      NDRNNP=0
C
C2------READ MAXIMUM NUMBER OF DRAINS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPDRN,MXPD)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTD,IDRNCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTD,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDRNCB,R,IOUT,IN)
      END IF
C###      WRITE(IOUT,3) MXACTD
    3 FORMAT(1X,'MAXIMUM OF',I5,' ACTIVE DRAINs AT ONE TIME')
C###      IF(IDRNCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
C###         IF(IDRNCB.GT.0) WRITE(IOUT,8) IDRNCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',I3)
C
C3------READ AUXILIARY VARIABLES AND CBC ALLOCATION OPTION.
      IDRNAL=0
      NAUX=0
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'CBCALLOCATE' .OR.
     1   LINE(ISTART:ISTOP).EQ.'CBC') THEN
         IDRNAL=1
C###         WRITE(IOUT,11)
   11    FORMAT(1X,'MEMORY IS ALLOCATED FOR CELL-BY-CELL BUDGET TERMS')
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.5) THEN
            NAUX=NAUX+1
            DRNAUX(NAUX)=LINE(ISTART:ISTOP)
C###            WRITE(IOUT,12) DRNAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY DRAIN VARIABLE: ',A)
         END IF
         GO TO 10
      END IF
      NDRNVL=5+NAUX+IDRNAL
C
C4------ALLOCATE SPACE IN THE RX ARRAY FOR THE DRAI ARRAY.
      IDRNPB=MXACTD+1
      MXDRN=MXACTD+MXPD
      ISOLD=ISUM
      LCDRAI=ISUM
      ISUM=ISUM+NDRNVL*MXDRN
C
C5------PRINT AMOUNT OF SPACE USED BY DRAIN PACKAGE.
      ISP=ISUM-ISOLD
C###      WRITE (IOUT,14)ISP
   14 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED BY DRN')
C
C6------RETURN.
      RETURN
      END
      SUBROUTINE GWF1DRN6RQ(IN,IOUT,NDRNVL,IDRNAL,NCOL,NROW,NLAY,NPDRN,
     1            DRAI,IDRNPB,MXDRN,IFREFM,ITERP,INAMLOC)
C
C-----VERSION 11JAN2000 GWF1DRN6RQ
C     ******************************************************************
C     READ DRAIN PARAMETERS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION DRAI(NDRNVL,MXDRN)
      COMMON /DRNCOM/DRNAUX(5)
      CHARACTER*16 DRNAUX
C     ------------------------------------------------------------------
C
C-------READ NAMED PARAMETERS.
C###      IF (ITERP.EQ.1) WRITE(IOUT,3) NPDRN
    3 FORMAT(1X,//1X,I5,' Drain parameters')
      IF(NPDRN.GT.0) THEN
        NAUX=NDRNVL-5-IDRNAL
        LSTSUM=IDRNPB
        ITERPU = ITERP
        DO 20 K=1,NPDRN
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXDRN,IN,IOUT,IP,'DRN','DRN',ITERP,
     &                   NUMINST,INAMLOC)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
C         ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          ENDIF
C         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 10 I=IB,NUMINST
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,ITERP)
            ENDIF
            CALL ULSTRD(NLST,DRAI,LB,NDRNVL,MXDRN,IDRNAL,IN,IOUT,
     &      'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  STRESS FACTOR',
     &        DRNAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,ITERPU)
            LB=LB+NLST
   10     CONTINUE
   20   CONTINUE
      END IF
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF1DRN6RP(DRAI,NDRAIN,MXDRN,IN,IOUT,NDRNVL,IDRNAL,
     1       IFREFM,NCOL,NROW,NLAY,NDRNNP,NPDRN,IDRNPB,ierror)
C
C-----VERSION 11JAN2000 GWF1DRN6RP
C     ******************************************************************
C     READ DRAIN HEAD, CONDUCTANCE AND BOTTOM ELEVATION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION DRAI(NDRNVL,MXDRN)
      COMMON /DRNCOM/DRNAUX(5)
      CHARACTER*16 DRNAUX
C     ------------------------------------------------------------------
      ierror=0
C
C1------READ ITMP (NUMBER OF DRAINS OR FLAG TO REUSE DATA) AND
C1------NUMBER OF PARAMETERS.
      IF(NPDRN.GT.0) THEN
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(2I10)') ITMP,NP
         ELSE
            READ(IN,*) ITMP,NP
         END IF
      ELSE
         NP=0
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(I10)') ITMP
         ELSE
            READ(IN,*) ITMP
         END IF
      END IF
C
C------CALCULATE SOME CONSTANTS
      NAUX=NDRNVL-5-IDRNAL
C
C2------DETERMINE THE NUMBER OF NON-PARAMETER DRAINS.
      IF(ITMP.LT.0) THEN
C###         WRITE(IOUT,7)
    7    FORMAT(1X,/1X,
     1        'REUSING NON-PARAMETER DRAINS FROM LAST STRESS PERIOD')
      ELSE
         NDRNNP=ITMP
      END IF
C
C3------IF THERE ARE NEW NON-PARAMETER DRAINS, READ THEM.
      MXACTD=IDRNPB-1
      IF(ITMP.GT.0) THEN
         IF(NDRNNP.GT.MXACTD) THEN
C###            WRITE(IOUT,99) NDRNNP,MXACTD
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE DRAINS (',I4,
     1                     ') IS GREATER THAN MXACTD(',I4,')')
	          ierror=20
	          return
C###            STOP
         END IF
         CALL ULSTRD(NDRNNP,DRAI,1,NDRNVL,MXDRN,IDRNAL,IN,IOUT,
     1     'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE',
     2     DRNAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,1)
      END IF
      NDRAIN=NDRNNP
C
C1C-----IF THERE ARE ACTIVE DRN PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('DRN')
      IF(NP.GT.0) THEN
         NREAD=NDRNVL-IDRNAL
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'DRN',IOUT,'DRN',DRAI,NDRNVL,MXDRN,NREAD,
     1                MXACTD,NDRAIN,5,5,
     2     'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE',
     3            DRNAUX,5,NAUX)
   30    CONTINUE
      END IF
C
C3------PRINT NUMBER OF DRAINS IN CURRENT STRESS PERIOD.
C###      WRITE (IOUT,101) NDRAIN
  101 FORMAT(1X,/1X,I5,' DRAINS')
C
C8------RETURN.
  260 RETURN
      END
      SUBROUTINE GWF1DRN6FM(NDRAIN,MXDRN,DRAI,HNEW,HCOF,RHS,IBOUND,
     1              NCOL,NROW,NLAY,NDRNVL)
C
C-----VERSION 11JAN2000 DRN5FM
C     ******************************************************************
C     ADD DRAIN FLOW TO SOURCE TERM
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,EEL
C
      DIMENSION DRAI(NDRNVL,MXDRN),HNEW(NCOL,NROW,NLAY),
     1         RHS(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     1         HCOF(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C
C1------IF NDRAIN<=0 THERE ARE NO DRAINS. RETURN.
      IF(NDRAIN.LE.0) RETURN
C
C2------PROCESS EACH CELL IN THE DRAIN LIST.
      DO 100 L=1,NDRAIN
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.
      IL=DRAI(1,L)
      IR=DRAI(2,L)
      IC=DRAI(3,L)
C
C4-------IF THE CELL IS EXTERNAL SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C5-------IF THE CELL IS INTERNAL GET THE DRAIN DATA.
      EL=DRAI(4,L)
      EEL=EL
C
C6------IF HEAD IS LOWER THAN DRAIN THEN SKIP THIS CELL.
      IF(HNEW(IC,IR,IL).LE.EEL) GO TO 100
C
C7------HEAD IS HIGHER THAN DRAIN. ADD TERMS TO RHS AND HCOF.
      C=DRAI(5,L)
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*EL
  100 CONTINUE
C
C8------RETURN.
      RETURN
      END
      SUBROUTINE GWF1DRN6BD(NDRAIN,MXDRN,VBNM,VBVL,MSUM,DRAI,DELT,HNEW,
     1        NCOL,NROW,NLAY,IBOUND,KSTP,KPER,IDRNCB,ICBCFL,BUFF,IOUT,
     2        PERTIM,TOTIM,NDRNVL,IDRNAL,IAUXSV)
C-----VERSION 05JUNE2000 GWF1DRN6BD
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR DRAINS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      COMMON /DRNCOM/DRNAUX(5)
      CHARACTER*16 DRNAUX
      CHARACTER*16 VBNM(MSUM),TEXT
      DOUBLE PRECISION HNEW,HHNEW,EEL,CC,CEL,RATOUT,QQ
C
      DIMENSION VBVL(4,MSUM),DRAI(NDRNVL,MXDRN),HNEW(NCOL,NROW,NLAY),
     1          IBOUND(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)
C
      DATA TEXT /'          DRAINS'/
C     ------------------------------------------------------------------
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATOR (RATOUT).
      ZERO=0.
      RATOUT=ZERO
      IBD=0
      IF(IDRNCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IDRNCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NDRNVL-5-IDRNAL
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,DRNAUX,IDRNCB,NCOL,NROW,NLAY,
     1          NDRAIN,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C3------CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
50    CONTINUE
C
C4------IF THERE ARE NO DRAINS THEN DO NOT ACCUMULATE DRAIN FLOW.
      IF(NDRAIN.LE.0) GO TO 200
C
C5------LOOP THROUGH EACH DRAIN CALCULATING FLOW.
      DO 100 L=1,NDRAIN
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING REACH.
      IL=DRAI(1,L)
      IR=DRAI(2,L)
      IC=DRAI(3,L)
      Q=ZERO
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 99
C
C5C-----GET DRAIN PARAMETERS FROM DRAIN LIST.
      EL=DRAI(4,L)
      EEL=EL
      C=DRAI(5,L)
      HHNEW=HNEW(IC,IR,IL)
C
C5D-----IF HEAD HIGHER THAN DRAIN, CALCULATE Q=C*(EL-HHNEW).
C5D-----SUBTRACT Q FROM RATOUT.
      IF(HHNEW.GT.EEL) THEN
         CC=C
         CEL=C*EL
         QQ=CEL - CC*HHNEW
         Q=QQ
         RATOUT=RATOUT-QQ
      END IF
C
C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDRNCB<0).
      IF(IBD.LT.0) THEN
C###         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD',I3,'   STEP',I3)
C###         WRITE(IOUT,62) L,IL,IR,IC,Q
   62    FORMAT(1X,'DRAIN',I4,'   LAYER',I3,'   ROW',I4,'   COL',I4,
     1       '   RATE',1PG15.6)
         IBDLBL=1
      END IF
C
C5F-----ADD Q TO BUFFER.
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+Q
C
C5G-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  OR IF
C5G-----RETURNING THE FLOW IN THE DRAI ARRAY, COPY FLOW TO DRAI.
   99 IF(IBD.EQ.2) CALL UBDSVB(IDRNCB,NCOL,NROW,IC,IR,IL,Q,
     1                  DRAI(1,L),NDRNVL,NAUX,6,IBOUND,NLAY)
      IF(IDRNAL.NE.0) DRAI(NDRNVL,L)=Q
  100 CONTINUE
C
C6------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IDRNCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C7------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 ROUT=RATOUT
      VBVL(3,MSUM)=ZERO
      VBVL(4,MSUM)=ROUT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C9------RETURN.
      RETURN
      END
