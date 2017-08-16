      SUBROUTINE GWF1DAF1ALP(IERR,LUFLW,LUGW,LUIN,LUOT,IDAFCB,IDAFBK,a
	1 ierror)
	 use mf2kmodule
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Read input and do preliminary computations
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOTS   - Maximum of time steps per ground water step
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
      INCLUDE 'ground.com'
C
C     + + + + + + + + + + + COMMON DEFINTIONS (startdaf.com) + + + + + +
C     IDBG,NBRCH NXSEC(N) VIN(I,N)
C
C     +  + + + + + + + + + + COMMON DEFINTIONS  (ground.com) + + + + + +
C     AQGW(I,N,J) BC(I,N,J) BEL(I,N) BTH(I,N) CND(I,N) NCL(I,N) NLY(I,N)
C     NRW(I,N) VGW(I,N,J)
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER    I,IDAFBK,IDAFCB,IERR,II,INX,J,LUFLW,LUIN,LUGW,LUOT,N,NN
      CHARACTER*80 TITLE
      LOGICAL OPTEST
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     IDAFBK   Central vs backward differencing flag for MODFLOW
C     IDAFCB   Print code for MODFLOW
C     IERR     Error code (0=ok, 20<stop as gracefully as you can)
C     TITLE    Title of program (80 characters max)
C
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
      EXTERNAL STARTDAF
C     + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + + +
 1000 FORMAT (A)
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
c 2000 FORMAT(1X,A)
c 2010 FORMAT(2I6,2F10.2,G15.3,3I6)
c 2020 FORMAT(' Something wrong in subroutine GWF1DAF1AL')
c 2030 FORMAT(1X,'The DAFLOW ground-water file (file type DAFG) was',/
c     1       1X,'not included in the name file')
c 2040 FORMAT(1X,'The DAFLOW output flow file was not included in',/
c     1       1X,'the name file.  This file must be specified as type',/
c     2       1X,'DATA and the unit must be one greater than the',/
c     3       1X,'DAFG file.')
C
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C
C     ***************** zero arrays and preliminaries *******************
c      IF(LUFLW.LE.0) THEN
c        WRITE(LUOT,2040)
c        CALL USTOP(' ')
c      END IF
c      INQUIRE(UNIT=LUFLW,OPENED=OPTEST)
c      IF(.NOT.OPTEST) THEN
c         WRITE(LUOT,2040)
c         CALL USTOP(' ')
c      END IF
      IERR=0
      DO 40 N=1,NOBR
        DO 40 I=1,NOSC
          BEL(I,N)=0.0
          BTH(I,N)=0.0
          CND(I,N)=0.0
          NCL(I,N)=0
          NLY(I,N)=0
          NRW(I,N)=0
          DO 40 J=1,NOTS
            BC(I,N,J)=0.0
            AQGW(I,N,J)=0.0
            VGW(I,N,J)=0.0
   40 CONTINUE
C
C     ********************** Initialize DAFLOW *************************
      CALL STARTDAF (IERR,LUFLW,LUIN,LUOT)
      IF(IERR.GT.20)GO TO 999
C
C     ******************** read ground water input *********************
c      IF(LUGW.LE.0) THEN
c         WRITE(LUOT,2030)
c         CALL USTOP(' ')
c      END IF
      READ(LUGW,1000) TITLE
c      WRITE(LUOT,2000) TITLE
      READ(LUGW,1000) TITLE
c      WRITE(LUOT,2000) TITLE
      READ(LUGW,1000) TITLE
c      WRITE(LUOT,2000) TITLE
C
C     ************* read data for each branch and subreach *************
      idafcount = 0
      DO 60 NN=1,NBRCH
        INX=NXSEC(NN)-1
        VGW(1,NN,1)=VIN(1,NN)
        DO 50 II=2,INX
           VGW(II,NN,1)=VIN(II,NN)
           READ(LUGW,*)N,I,BEL(I,N),BTH(I,N),CND(I,N),NLY(II,NN),
     #                NRW(II,NN),NCL(II,NN)
           idafcount = idafcount + 1
c          WRITE(LUOT,2010)N,I,BEL(I,N),BTH(I,N),CND(I,N),NLY(I,N),
c     #                    NRW(I,N),NCL(I,N)
   50   CONTINUE
   60 CONTINUE
      IDAFCB=0
      IDBG=0
      IDAFBK=0
      READ(LUGW,1000) TITLE
      READ(LUGW,*,END=70) IDAFCB,IDBG,IDAFBK
   70 continue 
c   70 IF(IDAFCB.LT.0) WRITE(LUOT,*)
c     1   ' CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0'
c      IF(IDAFCB.GT.0) WRITE(LUOT,*)
c     1   ' CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',IDAFCB
c      IF(IDBG.NE.1) WRITE(LUOT,*) ' DAF debugging is turned off'
c      IF(IDBG.EQ.1) WRITE(LUOT,*) ' DAF debugging is turned on'
c      IF(IDAFBK.EQ.0) WRITE(LUOT,*)
c     1   ' DAFLOW is using central differencing for ground-water head'
c      IF(IDAFBK.NE.0) WRITE(LUOT,*)
c     1   ' DAFLOW is using backward differencing for ground-water head'
  999 CLOSE(LUGW)
      IF(IERR.GT.20)THEN
	  ierror = 1
	  return
c        WRITE(LUOT,2020)
c        CALL USTOP(' ')
      END IF
      RETURN
      END
C
c      SUBROUTINE GWF1DAF1AD(DELT,IERR,ITMUNI,LUIN,LUOT)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Compute NHRR, set VIN for repeated cycles, and
C     read boundary conditions for NHRR time steps
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
c      INCLUDE 'params.inc'
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
c      INCLUDE 'startdaf.com'
c      INCLUDE 'ground.com'
C
C     + + + + + + + + + + ++ COMMON VARIABLES (startdaf.com) + + + + + +
C     DT F(K,N) FI(K,N) JTS NBRCH NHRR NS(N) NSI(N) NXSEC(N)
C     PX(K,N) PXI(K,N) TF(I,N) TFI(I,N) TIME TRB(I,N) VIN(I,N)
C
C     + + + + + + + + + + + + + COMMON VARIABLES (ground.com)+ + + + + +
C     BC(I,N,J) VGW(I,N)
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
c      INTEGER I,IERR,INX,ITMUNI,J,K,LUIN,LUOT,N
c      REAL    AA,DELT
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     DELT     Unitless time step size in MODFLOW
C     IERR     Error code (0=ok, 20<stop as gracefully as you can)
C     ITMUNI   Code for units of DELT (1=sec,2=min,3=hr,4=day,5=year)
C
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
c      INTRINSIC  ABS, FLOAT, IFIX
C
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C     EXTERNAL GETBC
C
C     + + + + + + + + + + + +  INPUT FORMATS + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
c 2000   FORMAT(' Something wrong in subroutine GWF1DAF1AD for time',I5)
C      + + + + + + + + + + + END SPECIFICATIONS  + + + + + + + + + + + +
C
C     *********************  Set initial conditions  *******************
C       DO 20 N=1,NBRCH
C        NSI(N)=NS(N)
C        INX=NXSEC(N)-1
c        DO 10 I=1,INX
c          TFI(I,N)=TF(I,N)
C          VIN(I,N)=VGW(I,N,NHRR)
C          BC(I,N,1)=BC(I,N,NHRR)
C   10   CONTINUE
C        DO 20 K=1,NS(N)
C          FI(K,N)=F(K,N)
C          PXI(K,N)=PX(K,N)
C   20  CONTINUE
cC    ***  Compute number of daflow time steps per MODFLOW time step ****
C      AA=0.0
C      IF(ITMUNI.EQ.1)AA=DELT/(3600.0*DT)
C      IF(ITMUNI.EQ.2)AA=DELT/(60.0*DT)
C      IF(ITMUNI.EQ.3)AA=DELT/DT
C      IF(ITMUNI.EQ.4)AA=DELT*24.0/DT
C      IF(ITMUNI.EQ.5)AA=DELT*24.*365.0/DT
c      NHRR=IFIX(AA)
C      AA=ABS(AA-FLOAT(NHRR))
C      IF(AA.GT.0.01)THEN
C        WRITE(LUOT,*)' MODFLOW time step is not an even multiple of',
C     #               ' the daflow time step.'
C        IERR=22
C      END IF
c      WRITE(LUOT,*)' No of DAFLOW steps per MODFLOW step = ',NHRR
c      DO 50 J=1,NHRR
C        DO 30 N=1,NBRCH
C          DO 30 I=1,NXSEC(N)
C            IF(J.GT.1) BC(I,N,J)=BC(I,N,J-1)
C            TRB(I,N)=BC(I,N,J)
C   30   CONTINUE
C        CALL GETBC (IERR,J,LUIN,LUOT)
C        DO 50 N=1,NBRCH
c          INX=NXSEC(N)-1
C          DO 50 I=1,INX
C            BC(I,N,J)=TRB(I,N)
C   50 CONTINUE
C      IF(IERR.GT.20)THEN
C        I=IFIX(TIME/DT+0.501)-JTS+1
C        WRITE(LUOT,2000)I
c        CALL USTOP(' ')
C      END IF
C      RETURN
C      END
C
C      SUBROUTINE sGWF1DAF1FM (IERR,ITMUNI,HNEW,HOLD,LUOT,
C     1        IBOUND,HCOF,RHS,NCOL,NROW,NLAY,KITER,IDAFBK)
CC
cC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     *****This subroutine solves daflow for NHRR time steps and computes
CC     the ground water exchange.
CC     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     NOBR   - Maximum number of branches allowed in model
CC
CC     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
c      INCLUDE 'ground.com'
CC
CC     + + + + + + + + COMMON DEFINTIONS  (startdaf.com)  + + + + + + + +
CC     AQ(I,N) NBRCH NHRR NXSEC(N) TIME TRB(I,N) V(I,N) VIN(I,N)
CC
CC     + + + + + + + + COMMON DEFINTIONS  (ground.com)  + + + + + + + + +
CC     AQGW(I,N,J) BC(I,N,J) CCSTR(I,N) NCL(I,N) NLY(I,N) NRW(I,N)
cC     RHSSTR(I,N) SEP(I,N) SSEP(I,N) VGW(I,N)
CC
CC     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
C      INTEGER     I,IERR,INX,ITMUNI,J,JCD(NOBR),JN,K,L,LUOT,N,
C     #            NCD(NOBR),NN
C      INTEGER IDAFBK,NCOL,NROW,NLAY,IBOUND(NCOL,NROW,NLAY),KITER
C      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
c      REAL HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY)
c      REAL AA,HOLD(NCOL,NROW,NLAY),VO
CC
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
CC     HNEW(M,N,L) Ground water head at end of time step in cell M,N,L
CC     HOLD(M,N,L) Ground water head at start of time step in cell M,N,L
CC     IDAFBK   Central vs backward differencing flag for MODFLOW
CC     IERR     Error code IERR<20 for warning, IERR>20 fatal
CC              (1=DL too large, 21=too many shocks)
cC     ITMUNI   Code for units of DELT (1=sec,2=min,3=hr,4=day,5=year)
CC     JCD(M)   Code for junction mixing (0=mixed, 1=not known)
CC     NCD(N)   Branch code (0=routed, 1=not routed)
CC     NCOL     Number of columns in ground-water model
CC     NLAY     Number of layers in ground-water model
CC     NROW     Number of rows in ground-water model
CC     VO       Volume in subreach at beginning of time step
cC
CC     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
C      INTRINSIC FLOAT,IFIX
CC     + + + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + +
C      EXTERNAL PRERTE, SETJNVL, SEEP, RTBR, LIMSEEP, FGQ, SETJV2
CC
CC     + + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + +
cC
cC     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
C 2000 FORMAT(' Warning DL>C DT, Increase DT to smooth.')
C 2010 FORMAT(' Something wrong in subroutine GWF1DAF1FM for time',I5)
C 2020 FORMAT(' Too many waves in branch',I3,' at J=',I5)
CC
CC     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
CC
CC     ********************* Preliminaries ******************************
c      CALL PRERTE
C      DO 10 N=1,NBRCH
C        DO 10 I=1,NXSEC(N)
C          SEP(I,N)=0.0
C          SSEP(I,N)=0.0
C          CCSTR(I,N)=0.0
C          RHSSTR(I,N)=0.0
c   10   CONTINUE
CC     ************************ Start time loop *************************
C      IERRR=0
C      DO 300 J=1,NHRR
C        CALL SETJNVL (JCD,NCD)
C        CALL SEEP (HNEW,HOLD,J,LUOT,IBOUND,NCOL,NROW,NLAY,KITER,
C     1             IDAFBK)
c        IF(IERR.GT.20)GO TO 900
cC       ************* Add seepage to trib flow *************************
C        DO 20 N=1,NBRCH
C          DO 20 I=1,NXSEC(N)
C            TRB(I,N)=BC(I,N,J)+SEP(I,N)
C   20   CONTINUE
CC
CC       ************************* Route branches ***********************
C        DO 200 NN=1,NBRCH
c          CALL RTBR (IERR,LUOT,J,JCD,JN,N,NCD)
C          IF(IERR.EQ.1)IERRR=1
C          IF(IERR.GT.20)GO TO 900
C          AQGW(1,N,J)=AQ(1,N)
C          DO 100 I=1,NXSEC(N)
C            IF(I.LT.NXSEC(N))THEN
C              IF(I.GT.1. AND. NLY(I,N).GT.0 )CALL LIMSEEP (HOLD,I,
c     1                    J,LUOT,N,NCOL,NROW,NLAY,IDAFBK)
C              IF(J.EQ.1)THEN
C                VO=VIN(I,N)
C              ELSE
C                VO=VGW(I,N,J-1)
C              END IF
C              CALL FGQ (I,J,LUOT,N,VO)
c            END IF
c            AQGW(I,N,J)=AQ(I,N)
C            VGW(I,N,J)=V(I,N)
C  100     CONTINUE
C          CALL SETJV2 (JCD,JN,NCD)
C  200   CONTINUE
C  300 CONTINUE
CC     ************************ End of time loop ************************
CC
c      IF(IERRR.EQ.1)WRITE(LUOT,2000)
C      AA=1.0
C      IF(ITMUNI.EQ.2)AA=60.0
C      IF(ITMUNI.EQ.3)AA=3600.0
C      IF(ITMUNI.EQ.4)AA=86400.0
C      IF(ITMUNI.EQ.5)AA=31536000.0
C      DO 400 N=1,NBRCH
c        INX=NXSEC(N)-1
C        DO 400 I=2,INX
C          L=NLY(I,N)
C          IF(L.GT.0) THEN
C            K=NRW(I,N)
C            NN=NCL(I,N)
C            IF(IBOUND(NN,K,L).GT.0) THEN
c              RHS(NN,K,L)=RHS(NN,K,L)+AA*RHSSTR(I,N)/FLOAT(NHRR)
c              HCOF(NN,K,L)=HCOF(NN,K,L)-AA*CCSTR(I,N)/FLOAT(NHRR)
C            END IF
C          END IF
C  400 CONTINUE
C  900 CONTINUE
CC       ************* Nasty error comes to here ************************
C        IF(IERR.GT.20)THEN
C          I=IFIX(TIME/DT+0.501)-JTS+1
c          WRITE(LUOT,2010)I
C          IF(IERR.EQ.21)WRITE(LUOT,2020)N,I
C          CALL USTOP(' ')
C        END IF
C      RETURN
C      END
CC
C      SUBROUTINE SEEP (HNEW,HOLD,J,LUOT,IBOUND,NCOL,NROW,NLAY,
C     1           KITER,IDAFBK)
CCC
cCC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     *****This subroutine computes the seepage to the channel
CC     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
C      INCLUDE 'ground.com'
CC
CC     + + + + + + + + COMMON DEFINTIONS  (startdaf.com)  + + + + + + + +
cC     AO(I,N) A1(I,N) A2(I,N) IDBG NBRCH NHRR NXSEC(N) VIN(I,N)
CC     W1(I,N) W2(I,N) X(I,N)
CC
CC     + + + + + + + + COMMON DEFINTIONS  (ground.com)  + + + + + + + + +
CC     BEL(I,N) BTH(I,N) CND(I,N) CSTR(I,N) NCL(I,N) NLY(I,N)
CC     NRW(I,N) QSTR(I,N) SEP(I,N) STAGE(I,N) VGW(I,N)
CC
cC     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
C      INTEGER I,IDAFBK,INX,J,KITER,LUOT,N
C      INTEGER NCOL,NROW,NLAY,IBOUND(NCOL,NROW,NLAY)
C      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
c      REAL A,AA,AR,DPT,HD,HOLD(NCOL,NROW,NLAY),Q,W
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     A        Cross sectional area of flow in subreach
C     AR       Surface area of water in subreach
C     DPT      Depth of flow (A/W)
C     HNEW(M,N,L) Ground water head at end of time ste in cell M,N,L
C     HD       Head, either water surface elev or seepage head
C     HOLD(M,N,L) Ground water head at start of time step in cell M,N,L
C     IDAFBK   Central vs backward differencing flag for MODFLOW
C     J        Time step
C     W        Top with of water surface in subreach
C
C     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
C     INTRINSIC  FLOAT
C
C     + + + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + +
C
C     + + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + +
C
C     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
C 2000 FORMAT(' For time step ',I5,', branch ',I3,
C     #       ' The requested Seepage =')
C 2010 FORMAT(10G11.4)
cC
cC     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
CC
CC     ******************** Compute seepage terms ***********************
C      DO 20 N=1,NBRCH
C        INX=NXSEC(N)-2
C        DO 10 I=1,INX
C          SEP(I+1,N)=0.
C          QSTR(I+1,N)=0.
c          STAGE(I+1,N)=0.
C          CSTR(I+1,N)=0.
C          IF(NLY(I+1,N).GT.0) THEN
C            IF(IBOUND(NCL(I+1,N),NRW(I+1,N),NLY(I+1,N)).NE.0) THEN
CC             ******** MODFLOW cell is active **************************
C              IF(KITER.EQ.1)THEN
C                IF(J.EQ.1) THEN
c                  A=VIN(I,N)/(X(I+1,N)-X(I,N))
C                ELSE
C                  A=VGW(I,N,J-1)/(X(I+1,N)-X(I,N))
C                END IF
C              ELSE
C                A=VGW(I,N,J)/(X(I+1,N)-X(I,N))
C              END IF
c              IF(A.GT.AO(I,N)) THEN
c                Q=((A-AO(I,N))/A1(I,N))**(1.0/A2(I,N))
C              ELSE
C                Q=0.0
C              END IF
C              IF(Q.GT.0.0) THEN
C                W=W1(I,N)*(Q**W2(I,N))
C                DPT=A/W
C              ELSE
c                W=0.0
C                DPT=0.0
C              END IF
C              AR=W*(X(I+1,N)-X(I,N))
C              IF(IDAFBK.EQ.0) THEN
C                HD=HOLD(NCL(I+1,N),NRW(I+1,N),NLY(I+1,N))
C              ELSE
c                HD=HNEW(NCL(I+1,N),NRW(I+1,N),NLY(I+1,N))
C              END IF
C              AA=HNEW(NCL(I+1,N),NRW(I+1,N),NLY(I+1,N))
C              HD=HD+(FLOAT(J)-0.5)*(AA-HD)/FLOAT(NHRR)
C              HD=HD-BEL(I+1,N)-DPT
C              AA=DPT+BTH(I+1,N)+HD
CC              IF(IDBG.EQ.1) WRITE(LUOT,*) ' I,DPT,DPT+BTH+HD',
cC     1                                    I+1,DPT,AA
c              IF(AA.LT.0.0) THEN
CC               * GW head does not have hydraulic connection to stream *
C                IF(DPT.GT.0) THEN
C                  HD=-BTH(I+1,N)-DPT
C                  SEP(I+1,N)=CND(I+1,N)*HD*AR/BTH(I+1,N)
C                ELSE
C                  SEP(I+1,N)=0.0
C                END IF
c                QSTR(I+1,N)=SEP(I+1,N)
C                STAGE(I+1,N)=0.
C                CSTR(I+1,N)=0.
C              ELSE
CC               ** GW head  has hydraulic connection too stream ********
C                IF(DPT.LE.0.0 .AND. HD.LE.0.0) THEN
C                  SEP(I+1,N)=0.0
c                  STAGE(I+1,N)=0.0
C                  CSTR(I+1,N)=0.0
C                ELSE
C                  IF(HD.GT.0.0) THEN
C                    AA=2.0*W2(I,N)
C                    IF(A2(I,N).GT.AA)THEN
CC                   ********* Width depth ratio decreases with Q *******
c                      AA=A2(I,N)-W2(I,N)
c                      AA=((HD+DPT)*W1(I,N)/A1(I,N))**(1.0/AA)
C                      AR=(W1(I,N)*AA**W2(I,N))*(X(I+1,N)-X(I,N))
C                      IF(IDBG.EQ.1) WRITE(LUOT,*)
C     1      'Width, Area based on gw head',W1(I,N)*AA**W2(I,N),AR
C                    ELSE
CC                   ******** Width depth ratio increases with Q ********
C                      AR=(W+2.0*HD)*(X(I+1,N)-X(I,N))
C                      IF(IDBG.EQ.1) WRITE(LUOT,*)
c     1     'Width, Area based on GW head',W+2.0*HD,AR
C                    END IF
C                  END IF
C                  CSTR(I+1,N)=CND(I+1,N)*AR/BTH(I+1,N)
C                  SEP(I+1,N)=CSTR(I+1,N)*HD
C                  STAGE(I+1,N)=BEL(I+1,N)+DPT
C                END IF
c                QSTR(I+1,N)=0.0
C              END IF
C            END IF
C          END IF
C          IF(IDBG.EQ.1) WRITE(LUOT,*) ' I,CSTR,STAGE,QSTR,SEP',
C     1             I+1,CSTR(I+1,N),STAGE(I+1,N),QSTR(I+1,N),SEP(I+1,N)
c   10   CONTINUE
c        IF(IDBG.EQ.1)WRITE(LUOT,2000)J,N
c        INX=NXSEC(N)-1
c        IF(IDBG.EQ.1)WRITE(LUOT,2010)(SEP(I,N),I=2,INX)
c   20 CONTINUE
C
c      RETURN
c      END
C
C      SUBROUTINE LIMSEEP (HOLD,I,J,LUOT,N,NCOL,NROW,NLAY,IDAFBK)
CC
CC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     *** This subroutine apportions any increase in TRB (reduction in
cC     *** outflow from the stream) that is calculated by ROUTE between
CC     *** BC and SEP.  Negative BC values are increased to 0 prior to
CC     *** increasing negative SEP values.  Coefficients SSEP, RHSSTR,
CC     *** & CCSTR  are also set.
CC     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
c      INCLUDE 'ground.com'
CC
CC     + + + + + + + + COMMON DEFINTIONS  (startdaf.com)  + + + + + + + +
CC     DT JTS QI IDBG NHRR TIME TRB(I,N)
CC
CC     + + + + + + + + COMMON DEFINTIONS  (ground.com)  + + + + + + + + +
CC     BC(I,N,J) CCSTR(I,N)  CSTR(I,N) NCL(I,N) NLY(I,N) NRW(I,N)
cC     QSTR(I,N) RHSSTR(I,N) SEP(I,N) SSEP(I,N) STAGE(I,N)
CC
CC     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
C      INTEGER I,IDAFBK,J,JJ,LUOT,N,NCOL,NROW,NLAY
C      REAL    AA,BCNEW,HOLD(NCOL,NROW,NLAY),TRBOLD,TRBCHG
CC
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
cC     BCNEW     - Adjusted value of withdrawal caused by lack of water
cC     HOLD(M,N,L) Ground water head at start of time step in cell M,N,L
CC     IDAFBK    - Central vs backward differencing flag for MODFLOW
CC     N         - Branch number
CC     NCOL      - Number of columns in ground-water model
CC     NLAY      - Number of layers in ground-water model
CC     NROW      - Number of rows in ground-water model
CC     J         - Time step number
CC     TRBOLD    - Value of TRB that was sent to ROUTE (BC+SEP).
cC     TRBCHG    - Reduction necessary in the extraction
CC
CC     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
C      INTRINSIC FLOAT,IFIX
CC     + + + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + +
CC     + + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + +
CC     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
c 4030 FORMAT(' Channel dry, reduced BC withdrawal to',G12.3,
C     $        ' at time step',I6,' Branch',I4,' Node',I4)
C 4040 FORMAT(' Channel dry, limited GW seepage to',G12.3,
C     $        ' at time step',I6,' Branch',I4,' Node',I4)
C     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
C
C      JJ=IFIX(TIME/DT+0.501) -JTS +1
C      TRBOLD=SEP(I,N)+BC(I,N,J)
CC     *** ROUTE will have modified TRB only when TRBOLD is negative. ***
c      IF(TRBOLD.LT.0.0) THEN
c        TRBCHG=TRB(I,N)-TRBOLD
CC       * Apportion change in TRB only if there was a significant change
C        IF(TRBCHG.GT. -1.E-6*TRBOLD) THEN
C          IF(BC(I,N,J).LT.0) THEN
C            IF(-BC(I,N,J).GE.TRBCHG) THEN
CC             ********* The entire TRB change is taken from BC *********
C              BCNEW=BC(I,N,J)+TRBCHG
C              IF(IDBG.EQ.1) WRITE(LUOT,4030) BCNEW,J,N,I
c            ELSE
CC             ** Part of TRB change is taken from SEP and part from BC *
C              IF(IDBG.EQ.1) WRITE(LUOT,4030) 0.,J,N,I
C              SEP(I,N)=SEP(I,N)+BC(I,N,J)+TRBCHG
C              QSTR(I,N)=SEP(I,N)
C              CSTR(I,N)=0.0
C              STAGE(I,N)=0.0
c              IF(IDBG.EQ.1) WRITE(LUOT,4040) SEP(I,N),JJ,N,I
C            END IF
C          ELSE
CC           ********* The entire TRB change is taken from SEP **********
C            SEP(I,N)=SEP(I,N)+TRBCHG
C            QSTR(I,N)=SEP(I,N)
C            CSTR(I,N)=0.0
c            STAGE(I,N)=0.0
c             JJ=IFIX(TIME/DT+0.501)-JTS+1
C            IF(IDBG.EQ.1) WRITE(LUOT,4040) SEP(I,N),JJ,N,I
C          END IF
C        END IF
C      END IF
CC
CC     ************** Accumulate SSEP, RHSSTR, and CCSTR ****************
C      IF(IDAFBK.EQ.0) THEN
c        AA=(FLOAT(J)-.5)/FLOAT(NHRR)
C      ELSE
C        AA=1.0
C      END IF
C      SSEP(I,N)=SSEP(I,N)+SEP(I,N)
C      RHSSTR(I,N)=RHSSTR(I,N)
C     1               +CSTR(I,N)*HOLD(NCL(I,N),NRW(I,N),NLY(I,N))*(1.-AA)
c     2               -STAGE(I,N)*CSTR(I,N)+QSTR(I,N)
C      CCSTR(I,N)=CCSTR(I,N)+CSTR(I,N)*AA
C      IF(IDBG.EQ.1)THEN
C        IF(N.EQ.1) WRITE(LUOT,*)'Final I, SEP ',I,SEP(I,N)
C       WRITE(LUOT,*)'I,SSEP,CCSTR,RHSSTR ',
C    1                                I,SSEP(I,N),CCSTR(I,N),RHSSTR(I,N)
c      END IF
c      RETURN
c      END
C
C      SUBROUTINE sGWF1DAF1BD (LUFLW,LUOT,ITMUNI,DELT,VBVL,VBNM,MSUM,
C     #           KSTP,KPER,IDAFCB,ICBCFL,BUFF,PERTIM,TOTIM,NCOL,NROW,
C     #           NLAY,IBOUND)
cC
cC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
CC     *** This subroutine prints the results ***************************
CC     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
C      INCLUDE 'ground.com'
CC
cC     + + + + + +  COMMON DEFINTIONS  (startdaf.com) + + + + + + + + + +
CC     AQ(I,N) NBRCH NHRR NXSEC(N) TRB(I,N) V(I,N)
CC
CC     + + + + + + + + COMMON DEFINTIONS  (ground.com)  + + + + + + + + +
CC     AQGW(I,N,J) BC(I,N,J) NCL(I,N) NRW(I,N) NLY(I,N) SSEP(I,N)
CC     VGW(I,N,J)
CC
cC     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
C      INTEGER NCOL,NROW,NLAY,IBOUND(NCOL,NROW,NLAY)
C      INTEGER I,IBD,ICBCFL,IDAFCB,INX,ITMUNI,J,KPER,KSTP,LUFLW,LUOT,N
C      INTEGER IBDLBL,IC,IL,IR
C      REAL AA,DELT,BUFF(NCOL,NROW,NLAY),TOTIM,VBVL(4,MSUM)
C      REAL RATE,RIN,ROUT,ZERO
C      CHARACTER*16 VBNM(MSUM),TEXT
c      DATA TEXT/'          DAFLOW'/
cC
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
CC     DELT     Unitless time step size in MODFLOW
CC     IDAFCB   Print code for MODFLOW
CC     ITMUNI   Code for units of DELT (1=sec,2=min,3=hr,4=day,5=year)
CC
C      DOUBLE PRECISION RATIN,RATOUT,RRATE
CC     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
cC     INTRINSIC  FLOAT
CC     + + + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + +
CC     EXTERNAL PRTFLW, UBDSV2, UBDSVA, UBUDSV
CC
CC    + + + + + + + + + + + + INPUT FORMATS + + + + + + + + + + + + + + +
CC     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
C 2000 FORMAT(1X,/1X,A,'   PERIOD',I3,'   STEP',I3)
c 2010 FORMAT(1X,'REACH',I4,'   LAYER',I3,'   ROW',I4,'   COL',I4,
C     1       '   RATE',1PG15.6)
CC     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
CC
C     ************************ Write results  **************************
C
C  Count stream nodes
C      NSNODE=0
C      DO 10 N=1,NBRCH
C        NSNODE=NSNODE+NXSEC(N)-2
c  10  CONTINUE
cC
C      DO 50 J=1,NHRR
C        DO 20 N=1,NBRCH
C          DO 20 I=1,NXSEC(N)
C            AQ(I,N)=AQGW(I,N,J)
C            V(I,N)=VGW(I,N,J)
C            TRB(I,N)=BC(I,N,J)
C   20   CONTINUE
c        CALL PRTFLW (LUFLW,LUOT)
C   50 CONTINUE
CC
CC  COMPUTE MODFLOW'S BUDGET TERMS
C        AA=1.0
C        IF(ITMUNI.EQ.2)AA=60.0
C        IF(ITMUNI.EQ.3)AA=3600.0
c        IF(ITMUNI.EQ.4)AA=86400.0
C        IF(ITMUNI.EQ.5)AA=31536000.0
CC
CC1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
C      ZERO=0.
C      RATIN=ZERO
C      RATOUT=ZERO
c      IBD=0
c      IF(IDAFCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
C      IF(IDAFCB.GT.0) IBD=ICBCFL
C      IBDLBL=0
CC
CC2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
C      IF(IBD.EQ.2) CALL UBDSV2(KSTP,KPER,TEXT,IDAFCB,NCOL,NROW,NLAY,
C     1          NSNODE,LUOT,DELT,PERTIM,TOTIM,IBOUND)
CC
cC3------CLEAR THE BUFFER.
C      DO 60 IL=1,NLAY
C        DO 60 IR=1,NROW
C          DO 60 IC=1,NCOL
C            BUFF(IC,IR,IL)=ZERO
C   60 CONTINUE
CC
cC4------IF NO REACHES, SKIP FLOW CALCULATIONS.
C      IF(NBRCH.EQ.0)GO TO 200
CC
CC5------LOOP THROUGH EACH RIVER REACH CALCULATING FLOW.
C        DO 150 N=1,NBRCH
C          INX=NXSEC(N)-1
C          DO 150 I=2,INX
c            IC=NCL(I,N)
c            IR=NRW(I,N)
C            IL=NLY(I,N)
C            RATE=ZERO
C            IF(IBOUND(IC,IR,IL).LE.0.OR.IL.LE.0)GO TO 99
C            RATE=-AA*SSEP(I,N)/FLOAT(NHRR)
C            RRATE=RATE
CC
CC5G-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDAFCB<0).
c        IF(IBD.LT.0) THEN
C          IF(IBDLBL.EQ.0) WRITE(LUOT,2000) TEXT,KPER,KSTP
C          WRITE(LUOT,2010) N,IL,IR,IC,RATE
C          IBDLBL=1
C        END IF
CC
CC5H------ADD RATE TO BUFFER.
c        BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
CC
CC5I-----SEE IF FLOW IS INTO AQUIFER OR INTO RIVER.
C        IF(RATE)94,99,96
C
C5J-----AQUIFER IS DISCHARGING TO RIVER SUBTRACT RATE FROM RATOUT.
C   94 RATOUT=RATOUT-RRATE
C      GO TO 99
CC
cC5K-----AQUIFER IS RECHARGED FROM RIVER; ADD RATE TO RATIN.
c   96 RATIN=RATIN+RRATE
CC
CC5L-----IF SAVING CELL-BY-CELL FLOWS IN LIST, WRITE FLOW.
C   99 IF(IBD.EQ.2) CALL UBDSVA(IDAFCB,NCOL,NROW,IC,IR,IL,RATE,IBOUND,
C     1                        NLAY)
C  150   CONTINUE
CC
CC6------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
cC6------CALL UBUDSV TO SAVE THEM.
C      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IDAFCB,BUFF,NCOL,NROW,
C     1                          NLAY,LUOT)
CC
CC7------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
C  200 RIN=RATIN
C      ROUT=RATOUT
c      VBVL(3,MSUM)=RIN
C      VBVL(4,MSUM)=ROUT
C      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
C      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
C      VBNM(MSUM)=TEXT
CC
CC8------INCREMENT BUDGET TERM COUNTER.
c      MSUM=MSUM+1
c      RETURN
C      END
CC
