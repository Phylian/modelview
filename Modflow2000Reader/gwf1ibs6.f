C     Last change:  ERB  16 Apr 2001    5:22 pm
C   September, 2000 -- updated to work with MODFLOW-2000
C   May, 2000 -- fixed error that caused incorrect critical head values
C   to be written in an external file when the option to write an
C   external file (IHCSV>0) is used.
C   June, 1996 -- 3 statements in the version documented
C   in TWRI 6-A2 have been modified in order to correct a problem.
C   Although subsidence is only meant to be active for layers in which
C   IBQ>0, some of the subroutines performed subsidence calculations when
C   IBQ<0.  Note that this was a problem only if negative IBQ values
C   were specified.  That is, the code has always worked correctly for
C   IBQ=0 and IBQ>0.
      SUBROUTINE GWF1IBS6AL(ISUM,LCHC,LCSCE,LCSCV,LCSUB,
     1        NCOL,NROW,NLAY,IIBSCB,IIBSOC,IN,IOUT,IBSDIM,ierror)
C
C-----VERSION 07JUN1996 GWF1IBS6AL
C-----VERSION 01AUG1996 -- modified to allow 200 layers instead of 80
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR INTERBED STORAGE PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION IBQ1(200)
      COMMON /IBSCOM/ IBQ(200)
C     ------------------------------------------------------------------
      ierror=0
C
C1------IDENTIFY PACKAGE.
c###      WRITE(IOUT,1)IN
    1 FORMAT(1H0,'IBS -- INTERBED STORAGE PACKAGE, VERSION 6,',
     1     ' 09/15/2000',' INPUT READ FROM UNIT',I3)
C
C4------READ FLAG FOR STORING CELL-BY-CELL STORAGE CHANGES AND
C4------FLAG FOR PRINTING AND STORING COMPACTION, SUBSIDENCE, AND
C4------CRITICAL HEAD ARRAYS.
      READ(IN,3) IIBSCB,IIBSOC
    3 FORMAT(2I10)
C
C5------IF CELL-BY-CELL TERMS TO BE SAVED THEN PRINT UNIT NUMBER.
c###      IF(IIBSCB.GT.0) WRITE(IOUT,105) IIBSCB
  105 FORMAT(1X,'CELL-BY-CELL FLOW TERMS WILL BE SAVED ON UNIT',I3)
C
C5A-----IF OUTPUT CONTROL FOR PRINTING ARRAYS IS SELECTED PRINT MESSAGE.
c###      IF(IIBSOC.GT.0) WRITE(IOUT,106)
  106 FORMAT(1X,'OUTPUT CONTROL RECORDS FOR IBS PACKAGE WILL BE ',
     1 'READ EACH TIME STEP.')
C
C6------READ INDICATOR AND FIND OUT HOW MANY LAYERS HAVE INTERBED STORAGE.
      READ(IN,110) (IBQ(K),K=1,NLAY)
  110 FORMAT(40I2)
      NAQL=0
      DO 120 K=1,NLAY
      IF(IBQ(K).LE.0) GO TO 120
      NAQL=NAQL+1
      IBQ1(NAQL)=K
  120 CONTINUE
      IBSDIM=NAQL
      IF(NAQL.LT.1) IBSDIM=1
C
C7------IDENTIFY WHICH LAYERS HAVE INTERBED STORAGE.
c###      WRITE(IOUT,130) (IBQ1(K),K=1,NAQL)
  130 FORMAT(1X,'INTERBED STORAGE IN LAYER(S) ',80I2)
C
C8------ALLOCATE SPACE FOR THE ARRAYS HC, SCE, SCV, AND SUB.
      IRK=ISUM
      NA=NROW*NCOL*IBSDIM
      LCHC=ISUM
      ISUM=ISUM+NA
      LCSCE=ISUM
      ISUM=ISUM+NA
      LCSCV=ISUM
      ISUM=ISUM+NA
      LCSUB=ISUM
      ISUM=ISUM+NA
C
C9------CALCULATE & PRINT AMOUNT OF SPACE USED BY PACKAGE.
  300 IRK=ISUM-IRK
c###      WRITE(IOUT,4)IRK
    4 FORMAT(1X,I8,' ELEMENTS OF X ARRAY USED FOR INTERBED STORAGE')
C
C10-----RETURN.
      RETURN
      END
      SUBROUTINE GWF1IBS6RP(DELR,DELC,HNEW,HC,SCE,SCV,SUB,NCOL,NROW,
     1                  NLAY,NODES,IIBSOC,ISUBFM,ICOMFM,IHCFM,
     2                  ISUBUN,ICOMUN,IHCUN,IN,IOUT,IBSDIM,ierror)
C
C-----VERSION 1117 02JUN1988 GWF1IBS6RP
C-----VERSION 01AUG1996 -- modified to allow 200 layers instead of 80
C     ******************************************************************
C     READ INTERBED STORAGE DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME(4)
      DOUBLE PRECISION HNEW
      DIMENSION HNEW(NCOL,NROW,NLAY),HC(NCOL,NROW,IBSDIM),
     1       SCE(NCOL,NROW,IBSDIM),SCV(NCOL,NROW,IBSDIM),
     2       SUB(NCOL,NROW,IBSDIM),DELR(NCOL),DELC(NROW)
C
      COMMON /IBSCOM/ IBQ(200)
C
      DATA ANAME(1) /'   PRECONSOLIDATION HEAD'/
      DATA ANAME(2) /'ELASTIC INTERBED STORAGE'/
      DATA ANAME(3) /' VIRGIN INTERBED STORAGE'/
      DATA ANAME(4) /'     STARTING COMPACTION'/
C     ------------------------------------------------------------------
      ierror=0
C
C1------READ IN STORAGE AND CRITICAL HEAD ARRAYS
      KQ=0
      DO 60 K=1,NLAY
      IF(IBQ(K).LE.0) GO TO 60
      KQ=KQ+1
      CALL U2DREL(HC(1,1,KQ),ANAME(1),NROW,NCOL,K,IN,IOUT)
      CALL U2DREL(SCE(1,1,KQ),ANAME(2),NROW,NCOL,K,IN,IOUT)
      CALL U2DREL(SCV(1,1,KQ),ANAME(3),NROW,NCOL,K,IN,IOUT)
      CALL U2DREL(SUB(1,1,KQ),ANAME(4),NROW,NCOL,K,IN,IOUT)
   60 CONTINUE
C
C2------LOOP THROUGH ALL CELLS WITH INTERBED STORAGE.
      KQ=0
      DO 80 K=1,NLAY
      IF(IBQ(K).LE.0) GO TO 80
      KQ=KQ+1
      DO 70 IR=1,NROW
      DO 70 IC=1,NCOL
C
C3------MULTIPLY STORAGE BY AREA TO GET STORAGE CAPACITY.
      AREA=DELR(IC)*DELC(IR)
      SCE(IC,IR,KQ)=SCE(IC,IR,KQ)*AREA
      SCV(IC,IR,KQ)=SCV(IC,IR,KQ)*AREA
C
C4------MAKE SURE THAT PRECONSOLIDATION HEAD VALUES
C4------ARE CONSISTANT WITH STARTING HEADS.
      IF(HC(IC,IR,KQ).GT.HNEW(IC,IR,K)) HC(IC,IR,KQ)=HNEW(IC,IR,K)
   70 CONTINUE
   80 CONTINUE
C
C5------INITIALIZE AND READ OUTPUT FLAGS.
      ICOMFM=0
      ISUBFM=0
      IHCFM=0
      ICOMUN=0
      ISUBUN=0
      IHCUN=0
      IF(IIBSOC.LE.0) GO TO 200
      READ(IN,100) ISUBFM,ICOMFM,IHCFM,ISUBUN,ICOMUN,IHCUN
  100 FORMAT(6I10)
c###      WRITE(IOUT,110) ISUBFM,ICOMFM,IHCFM
  110 FORMAT(1H0,'    SUBSIDENCE PRINT FORMAT IS NUMBER',I4/
     1          '     COMPACTION PRINT FORMAT IS NUMBER',I4/
     2          '  CRITICAL HEAD PRINT FORMAT IS NUMBER',I4)
c###      IF(ISUBUN.GT.0) WRITE(IOUT,120) ISUBUN
  120 FORMAT(1H0,'    UNIT FOR SAVING SUBSIDENCE IS',I4)
c###      IF(ICOMUN.GT.0) WRITE(IOUT,130) ICOMUN
  130 FORMAT(1H ,'    UNIT FOR SAVING COMPACTION IS',I4)
c###      IF(IHCUN.GT.0)  WRITE(IOUT,140) IHCUN
  140 FORMAT(1H ,' UNIT FOR SAVING CRITICAL HEAD IS',I4)
C
C6------RETURN
  200 RETURN
      END
