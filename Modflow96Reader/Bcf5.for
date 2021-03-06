      SUBROUTINE BCF5AL(ISUM,LENX,LCSC1,LCHY,LCBOT,LCTOP,LCSC2,LCTRPY,
     1  IN,ISS,NCOL,NROW,NLAY,IOUT,IBCFCB,LCWETD,IWDFLG,LCCVWD,
     2  WETFCT,IWETIT,IHDWET,HDRY,IAPART,IFREFM,ierror)
C
C-----VERSION 1431 20FEB1996 BCF5AL
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR BLOCK-CENTERED FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      COMMON /FLWCOM/LAYCON(200)
      COMMON /FLWAVG/LAYAVG(200)
      CHARACTER*12 AVGNAM(4)
      DATA AVGNAM/'HARMONIC    ','ARITHMETIC  ',
     1            'LOGARITHMIC ','*UNCONFINED*'/
C     ------------------------------------------------------------------
      ierror=0
C
C1------IDENTIFY PACKAGE
c###      WRITE(IOUT,1) IN
c###    1 FORMAT(1X,/1X,'BCF5 -- BLOCK-CENTERED FLOW PACKAGE, VERSION 5',
c###     1', 9/1/93',' INPUT READ FROM UNIT',I3)
C
C2------READ AND PRINT ISS (STEADY-STATE FLAG), IBCFCB (FLAG FOR
C2------PRINTING OR UNIT# FOR RECORDING CELL-BY-CELL FLOW TERMS), HDRY
C2------(HEAD AT CELLS THAT CONVERT TO DRY), AND WETTING PARAMETERS.
      IF(IFREFM.EQ.0) THEN
         READ(IN,'(2I10,F10.0,I10,F10.0,2I10)',err=90)
     1              ISS,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET
      ELSE
         READ(IN,*,err=90) ISS,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET
      END IF
c###      IF(ISS.EQ.0) WRITE(IOUT,3)
c###    3 FORMAT(1X,'TRANSIENT SIMULATION')
c###      IF(ISS.NE.0) WRITE(IOUT,4)
c###    4 FORMAT(1X,'STEADY-STATE SIMULATION')
c###      IF(IBCFCB.LT.0) WRITE(IOUT,8)
c###    8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',
c###     1     ' WHEN ICBCFL IS NOT 0')
c###      IF(IBCFCB.GT.0) WRITE(IOUT,9) IBCFCB
c###    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',I3)
c###      WRITE(IOUT,11) HDRY
c###   11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',G13.5)
      IF(IWDFLG.NE.0) GO TO 35
c###      WRITE(IOUT,12)
c###   12 FORMAT(1X,'WETTING CAPABILITY IS NOT ACTIVE')
      GO TO 39
C
c###   35 WRITE(IOUT,36)
c**
   35 continue
c****
c###   36 FORMAT(1X,'WETTING CAPABILITY IS ACTIVE')
      IF(IWETIT.LE.0) IWETIT=1
c###      WRITE(IOUT,37)WETFCT,IWETIT
c###   37 FORMAT(1X,'WETTING FACTOR=',F10.5,
c###     1     '     WETTING ITERATION INTERVAL=',I4)
c###      WRITE(IOUT,38)IHDWET
c###   38 FORMAT(1X,'FLAG THAT SPECIFIES THE EQUATION TO USE FOR HEAD',
c###     1    ' AT WETTED CELLS=',I4)
C
C3------STOP THE SIMULATION IF THERE ARE MORE THAN 200 LAYERS.
   39 IF(NLAY.LE.200) GO TO 50
c###      WRITE(IOUT,41)
c###   41 FORMAT(1X,/1X,'YOU HAVE SPECIFIED MORE THAN 200 MODEL LAYERS'/1X,
c###     1 'SPACE IS RESERVED FOR A MAXIMUM OF 200 LAYERS IN ARRAYS LAYCON',
c###     2 ' AND LAYAVG')
      STOP
C
C4------READ LAYCON & PRINT TITLE FOR LAYCON TABLE.
   50 IF(IFREFM.EQ.0) THEN
         READ(IN,'(40I2)') (LAYCON(I),I=1,NLAY)
      ELSE
         READ(IN,*) (LAYCON(I),I=1,NLAY)
      END IF
c###      WRITE(IOUT,52)
c###   52 FORMAT(1X,5X,'LAYER  LAYER-TYPE CODE     INTERBLOCK T',
c###     1      /1X,5X,44('-'))
C
C5------LOOP THROUGH LAYERS CALCULATING LAYAVG, PRINTING THE LAYER-TYPE
C5------CODE, AND COUNTING LAYERS THAT NEED TOP & BOT ARRAYS.
      NBOT=0
      NTOP=0
      DO 100 I=1,NLAY
      IF(LAYCON(I).EQ.30 .OR. LAYCON(I).EQ.32) LAYCON(I)=LAYCON(I)-10
      INAM=LAYCON(I)/10
      LAYAVG(I)=INAM*10
      IF(LAYAVG(I).LT.0 .OR. LAYAVG(I).GT.30) THEN
c###         WRITE(IOUT,53) LAYAVG(I)
c###   53    FORMAT(1X,'INVALID INTERBLOCK T CODE:',I4)
         STOP
      END IF
      LAYCON(I)=LAYCON(I)-LAYAVG(I)
      L=LAYCON(I)
      INAM=INAM+1
c###      WRITE(IOUT,55) I,L,LAYAVG(I),AVGNAM(INAM)
c###   55 FORMAT(1X,I9,I13,I11,' -- ',A)
      IF(LAYCON(I).LT.0 .OR. LAYCON(I).GT.3) THEN
c###         WRITE(IOUT,56) LAYCON(I)
   56    FORMAT(1X,'INVALID LAYER TYPE:',I4)
         STOP
      END IF
C
C5A-----ONLY THE TOP LAYER CAN BE UNCONFINED(LAYCON=1).
      IF(L.NE.1 .OR. I.EQ.1) GO TO 70
c###      WRITE(IOUT,57)
c###   57 FORMAT(1X,/1X,'LAYER TYPE 1 IS ONLY ALLOWED IN TOP LAYER')
      STOP
C
C5B-----LAYER TYPES 1 AND 3 NEED A BOTTOM. ADD 1 TO KB.
   70 IF(L.EQ.1 .OR. L.EQ.3) NBOT=NBOT+1
C
C5C-----LAYER TYPES 2 AND 3 NEED A TOP. ADD 1 TO KT.
      IF(L.EQ.2 .OR. L.EQ.3) NTOP=NTOP+1
C
C5D-----IF LAYAVG=30, BUFF MUST BE SEPARATE FROM RHS (IAPART NOT 0).
      IF(IAPART.EQ.0 .AND. LAYAVG(I).EQ.30) THEN
c###         WRITE(IOUT,75)
c###   75    FORMAT(1X,'IAPART IN BAS PACKAGE MUST BE NONZERO',
c###     1      ' WHEN INTERBLOCK T IS *UNCONFINED*')
         STOP
      END IF
  100 CONTINUE
C
C
C6------COMPUTE THE NUMBER OF CELLS IN THE ENTIRE GRID AND IN ONE LAYER.
      NRC=NROW*NCOL
      ISIZ=NRC*NLAY
C
C7------ALLOCATE SPACE FOR ARRAYS.
      ISOLD=ISUM
      LCSC1=ISUM
      IF(ISS.EQ.0) ISUM=ISUM+ISIZ
      LCSC2=ISUM
      IF(ISS.EQ.0) ISUM=ISUM+NRC*NTOP
      LCTRPY=ISUM
      ISUM=ISUM+NLAY
      LCBOT=ISUM
      ISUM=ISUM+NRC*NBOT
      LCHY=ISUM
      ISUM=ISUM+NRC*NBOT
      LCTOP=ISUM
      ISUM=ISUM+NRC*NTOP
      LCWETD=ISUM
      IF(IWDFLG.NE.0)ISUM=ISUM+NRC*NBOT
      LCCVWD=ISUM
      IF(IWDFLG.NE.0)ISUM=ISUM+NRC*(NLAY-1)
C
C8------PRINT THE AMOUNT OF SPACE USED BY THE BCF PACKAGE.
      ISP=ISUM-ISOLD
c###      WRITE(IOUT,101) ISP
c###  101 FORMAT(1X,I10,' ELEMENTS IN X ARRAY ARE USED BY BCF')
      ISUM1=ISUM-1
c###      WRITE(IOUT,102) ISUM1,LENX
c###  102 FORMAT(1X,I10,' ELEMENTS OF X ARRAY USED OUT OF ',I10)
c###      IF(ISUM1.GT.LENX) WRITE(IOUT,103)
c###  103 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')
C
C9------RETURN.
      RETURN
 90   ierror=7
      return
      END
      SUBROUTINE BCF5RP(IBOUND,HNEW,SC1,HY,CR,CC,CV,DELR,DELC,BOT,TOP,
     1 SC2,TRPY,IN,ISS,NCOL,NROW,NLAY,IOUT,WETDRY,IWDFLG,CVWD,ierror)
C
C-----VERSION 0917 17JULY1992 BCF5RP
C     ******************************************************************
C     READ AND INITIALIZE DATA FOR BLOCK-CENTERED FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME(11)
c###      DOUBLE PRECISION HNEW
C
      DIMENSION HNEW(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),
     1    HY(NCOL,NROW,NLAY),CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     2    CV(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),BOT(NCOL,NROW,NLAY),
     3    TOP(NCOL,NROW,NLAY),SC2(NCOL,NROW,NLAY),TRPY(NLAY),
     4    IBOUND(NCOL,NROW,NLAY),WETDRY(NCOL,NROW,NLAY),
     5    CVWD(NCOL,NROW,NLAY)
C
      COMMON /FLWCOM/LAYCON(200)
C
      DATA ANAME(1) /'    PRIMARY STORAGE COEF'/
      DATA ANAME(2) /'    TRANSMIS. ALONG ROWS'/
      DATA ANAME(3) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(4) /'VERT HYD COND /THICKNESS'/
      DATA ANAME(5) /'                  BOTTOM'/
      DATA ANAME(6) /'                     TOP'/
      DATA ANAME(7) /'  SECONDARY STORAGE COEF'/
      DATA ANAME(8) /'COLUMN TO ROW ANISOTROPY'/
      DATA ANAME(9) /'                    DELR'/
      DATA ANAME(10)/'                    DELC'/
      DATA ANAME(11)/'        WETDRY PARAMETER'/
C     ------------------------------------------------------------------
      ierror=0
C
C1------READ TRPY,DELR,DELC.
      CALL U1DREL(TRPY,ANAME(8),NLAY,IN,IOUT)
      CALL U1DREL(DELR,ANAME(9),NCOL,IN,IOUT)
      CALL U1DREL(DELC,ANAME(10),NROW,IN,IOUT)
C
C2------READ ALL PARAMETERS FOR EACH LAYER.
      KT=0
      KB=0
      DO 200 K=1,NLAY
      KK=K
C
C2A-----FIND ADDRESS OF EACH LAYER IN THREE DIMENSION ARRAYS.
      IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) KB=KB+1
      IF(LAYCON(K).EQ.2 .OR. LAYCON(K).EQ.3) KT=KT+1
C
C2B-----READ PRIMARY STORAGE COEFFICIENT INTO ARRAY SC1 IF TRANSIENT.
      IF(ISS.EQ.0)CALL U2DREL(SC1(1,1,K),ANAME(1),NROW,NCOL,KK,IN,IOUT)
C
C2C-----READ TRANSMISSIVITY INTO ARRAY CC IF LAYER TYPE IS 0 OR 2.
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) GO TO 100
      CALL U2DREL(CC(1,1,K),ANAME(2),NROW,NCOL,KK,IN,IOUT)
      GO TO 110
C
C2D-----READ HYDRAULIC CONDUCTIVITY(HY) AND BOTTOM ELEVATION(BOT)
C2D-----IF LAYER TYPE IS 1 OR 3.
  100 CALL U2DREL(HY(1,1,KB),ANAME(3),NROW,NCOL,KK,IN,IOUT)
      CALL U2DREL(BOT(1,1,KB),ANAME(5),NROW,NCOL,KK,IN,IOUT)
C
C2E-----READ VERTICAL HYCOND/THICK INTO ARRAY CV IF NOT BOTTOM LAYER;
C2E-----MULTIPLIED BY CELL AREA TO CONVERT TO CONDUCTANCE LATER.
  110 IF(K.EQ.NLAY) GO TO 120
      CALL U2DREL(CV(1,1,K),ANAME(4),NROW,NCOL,KK,IN,IOUT)
C
C2F-----READ SECONDARY STORAGE COEFFICIENT INTO ARRAY SC2 IF TRANSIENT
C2F-----AND LAYER TYPE IS 2 OR 3.
  120 IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 130
      IF(ISS.EQ.0)CALL U2DREL(SC2(1,1,KT),ANAME(7),NROW,NCOL,KK,IN,IOUT)
C
C2G-----READ TOP ELEVATION(TOP) IF LAYER TYPE IS 2 OR 3.
      CALL U2DREL(TOP(1,1,KT),ANAME(6),NROW,NCOL,KK,IN,IOUT)
C
C2H-----READ WETDRY CODES IF LAYER TYPE IS 1 OR 3 AND WETTING
C2H-----CAPABILITY HAS BEEN INVOKED (IWDFLG NOT 0).
  130 IF(LAYCON(K).NE.3.AND.LAYCON(K).NE.1)GO TO 200
      IF(IWDFLG.EQ.0)GO TO 200
      CALL U2DREL(WETDRY(1,1,KB),ANAME(11),NROW,NCOL,KK,IN,IOUT)
  200 CONTINUE
C
C3------PREPARE AND CHECK BCF DATA.
      CALL SBCF5N(HNEW,IBOUND,SC1,SC2,CR,CC,CV,HY,TRPY,DELR,DELC,ISS,
     1         NCOL,NROW,NLAY,IOUT,WETDRY,IWDFLG,CVWD)
C
C4------RETURN
      RETURN
      END
      SUBROUTINE SBCF5N(HNEW,IBOUND,SC1,SC2,CR,CC,CV,HY,TRPY,DELR,DELC,
     1    ISS,NCOL,NROW,NLAY,IOUT,WETDRY,IWDFLG,CVWD)
C
C-----VERSION 1456 29JUNE1993 SBCF5N
C     ******************************************************************
C     INITIALIZE AND CHECK BCF DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
c###      DOUBLE PRECISION HNEW,HCNV
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)
     1    ,SC1(NCOL,NROW,NLAY),CR(NCOL,NROW,NLAY)
     2    ,CC(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY)
     3    ,HY(NCOL,NROW,NLAY),TRPY(NLAY),DELR(NCOL),DELC(NROW)
     4    ,SC2(NCOL,NROW,NLAY),WETDRY(NCOL,NROW,NLAY)
     5    ,CVWD(NCOL,NROW,NLAY)
C
      COMMON /FLWCOM/LAYCON(200)
      COMMON /FLWAVG/LAYAVG(200)
C     ------------------------------------------------------------------
C
C1------MULTIPLY VERTICAL LEAKANCE BY AREA TO MAKE CONDUCTANCE.
      ZERO=0.
      IF(NLAY.EQ.1) GO TO 20
      K1=NLAY-1
      DO 10 K=1,K1
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
      CV(J,I,K)=CV(J,I,K)*DELR(J)*DELC(I)
   10 CONTINUE
C
C2------IF WETTING CAPABILITY IS ACTIVATED, SAVE CV IN CVWD FOR USE WHEN
C2------WETTING CELLS.
      IF(IWDFLG.EQ.0) GO TO 20
      DO 15 K=1,K1
      DO 15 I=1,NROW
      DO 15 J=1,NCOL
      CVWD(J,I,K)=CV(J,I,K)
   15 CONTINUE
C
C3------IF IBOUND=0, SET CV=0 AND CC=0.
   20 DO 30 K=1,NLAY
      DO 30 I=1,NROW
      DO 30 J=1,NCOL
      IF(IBOUND(J,I,K).NE.0) GO TO 30
      IF(K.NE.NLAY) CV(J,I,K)=ZERO
      IF(K.NE.1) CV(J,I,K-1)=ZERO
      CC(J,I,K)=ZERO
   30 CONTINUE
C
C4------INSURE THAT EACH ACTIVE CELL HAS AT LEAST ONE NON-ZERO
C4------TRANSMISSIVE PARAMETER.
      HCNV=888.88
      KB=0
      DO 60 K=1,NLAY
      IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) GO TO 50
C
C4A-----WHEN LAYER TYPE IS 0 OR 2, TRANSMISSIVITY OR CV MUST BE NONZERO.
      DO 45 I=1,NROW
      DO 45 J=1,NCOL
      IF(IBOUND(J,I,K).EQ.0) GO TO 45
      IF(CC(J,I,K).NE.ZERO) GO TO 45
      IF(K.EQ.NLAY) GO TO 41
      IF(CV(J,I,K).NE.ZERO) GO TO 45
   41 IF(K.EQ.1) GO TO 42
      IF(CV(J,I,K-1).NE.ZERO) GO TO 45
   42 IBOUND(J,I,K)=0
c###      HNEW(J,I,K)=HCNV
c###      WRITE(IOUT,43) K,I,J
c###   43 FORMAT(1X,'NODE (LAYER,ROW,COL)',3I4,
c###     1      ' ELIMINATED BECAUSE ALL CONDUCTANCES TO NODE ARE 0')
   45 CONTINUE
      GO TO 60
C
C4B-----WHEN LAYER TYPE IS 1 OR 3, HY OR CV MUST BE NONZERO.
   50 KB=KB+1
      DO 59 I=1,NROW
      DO 59 J=1,NCOL
C
C4B1----IF WETTING CAPABILITY IS ACTIVE, CHECK CVWD.
      IF(IWDFLG.EQ.0) GO TO 55
      IF(WETDRY(J,I,KB).EQ.ZERO) GO TO 55
      IF(K.EQ.NLAY) GO TO 51
      IF(CVWD(J,I,K).NE.ZERO) GO TO 59
   51 IF(K.EQ.1) GO TO 57
      IF(CVWD(J,I,K-1).NE.ZERO) GO TO 59
      GO TO 57
C
C4B2----WETTING CAPABILITY IS INACTIVE, SO CHECK CV AT ACTIVE CELLS.
   55 IF(IBOUND(J,I,K).EQ.0) GO TO 59
      IF(K.EQ.NLAY) GO TO 56
      IF(CV(J,I,K).NE.ZERO) GO TO 59
   56 IF(K.EQ.1) GO TO 57
      IF(CV(J,I,K-1).NE.ZERO) GO TO 59
C
C4B3----CHECK HYDRAULIC CONDUCTIVITY.
   57 IF(HY(J,I,KB).NE.ZERO) GO TO 59
C
C4B4----HY AND CV ARE ALL 0, SO CONVERT CELL TO NO FLOW.
      IBOUND(J,I,K)=0
c###      HNEW(J,I,K)=HCNV
      IF(IWDFLG.NE.0) WETDRY(J,I,KB)=ZERO
c###      WRITE(IOUT,43) K,I,J
   59 CONTINUE
   60 CONTINUE
C
C5------CALCULATE HOR. CONDUCTANCE(CR AND CC) FOR CONSTANT T LAYERS.
c###      DO 70 K=1,NLAY
c###      KK=K
c###      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) GO TO 70
c###      IF(LAYAVG(K).EQ.0) THEN
c###         CALL SBCF5C(CR,CC,TRPY,DELR,DELC,KK,NCOL,NROW,NLAY)
c###      ELSE IF(LAYAVG(K).EQ.10) THEN
c###         CALL SBCF5A(CR,CC,TRPY,DELR,DELC,KK,NCOL,NROW,NLAY)
c###      ELSE
c###         CALL SBCF5L(CR,CC,TRPY,DELR,DELC,KK,NCOL,NROW,NLAY)
c###      END IF
c###   70 CONTINUE
C
C6------IF TRANSIENT, LOOP THROUGH LAYERS AND CALCULATE STORAGE
C6------CAPACITY.
c###      IF(ISS.NE.0) GO TO 100
c###      KT=0
c###      DO 90 K=1,NLAY
C
C6A-----MULTIPLY PRIMARY STORAGE COEFFICIENT BY DELR & DELC TO GET
C6A-----PRIMARY STORAGE CAPACITY.
c###      DO 80 I=1,NROW
c###      DO 80 J=1,NCOL
c###      SC1(J,I,K)=SC1(J,I,K)*DELR(J)*DELC(I)
c###   80 CONTINUE
C
C6B-----IF LAYER IS CONF/UNCONF MULTIPLY SECONDARY STORAGE COEFFICIENT
C6B-----BY DELR AND DELC TO GET SECONDARY STORAGE CAPACITY(SC2).
c###      IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 90
c###      KT=KT+1
c###      DO 85 I=1,NROW
c###      DO 85 J=1,NCOL
c###      SC2(J,I,KT)=SC2(J,I,KT)*DELR(J)*DELC(I)
c###   85 CONTINUE
c###   90 CONTINUE
C
C7------RETURN.
  100 RETURN
      END

