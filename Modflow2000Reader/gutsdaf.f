C  Modified 11-30-2002 by Harvey Jobson
C
      SUBROUTINE STARTDAF (IERR,LUFLW,LUIN,LUOT)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     ************  This subroutine starts DAFLOW **********************
C     DAFLOW uses a variable DF=Q^(1-W2)/(2 W1 S) and assumes 
C     A=AO+A1(Q**A2), computes celerity from dQ/dA, conserves mass to
C     compute the average flow at the nodes. All boundry conditions
C     represent the average during the time step. The first BC represents
C     the flow from time 0 to Dt, for example. Dispersion is modeled by
C     mixing at shocks over a dispersion distance.
C     The Q is at the node point with QT occuring just upstream of node.
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOSH   - Maximum number of shocks allowed in branch
C              (NOSH should be at least 4 times NOSC)
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C
C     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
C     AO(I,N) A1(I,N) A2(I,N) F(K,N) FI(K,N) DT IENG IOUTDAF(I,N)
C     JNCD(N) JNCU(N) JGO JTS NBRCH NHR NHRR NJNCT NS(N) NSI(N) NXSEC(N)
C     PF(N) PX(K,N) PXI(K,N) QI SL(I,N) TF(I,N) TFI(I,N) TIME VIN(I,N)
C     VI W1(I,N) W2(I,N) X(I,N) XFACT 
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER     I,IERR,J,K,LUFLW,LUIN,LUOT,N
      REAL        A,AA
C     CHARACTER*64 VERSN
      CHARACTER*80 TITLE
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     IERR     Error code (0=ok, 20<stop as gracefully as you can)
C     TITLE    Title of program (80 characters max)
C
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
      INTRINSIC  FLOAT
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C
C    + + + + + + + + + + + + INPUT FORMATS + + + + + + + + + + + + + + +
 1000 FORMAT (A)
 1010 FORMAT (20X,I10)
 1020 FORMAT (20X,F10.3)
 1030 FORMAT (13X,I3,16X,F5.2,16X,I3,8X,I3)
C
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
c 2000 FORMAT (1X,A,/)
c 2010 FORMAT (' The',I3,' Branch Model with',I3,' Internal Junctions',
c     #        ' is run',I5,' Time Steps each',F5.2,' hours long.')
c 2020 FORMAT ('The Model starts at',F6.2,' hours past midnight.')
c 2030 FORMAT ('The node output is given in "bltm.out" every',I4,
c     #        ' time steps.')
c 2040 FORMAT ('Input units are Metric (Meters & river kilometers')
c 2050 FORMAT ('Input units are English (feet and river miles)')
c 2060 FORMAT (' Width=W1(Q)**W2')
c 2070 FORMAT (' Cross sectional area = AO+A1(Q**A2)')
c 2080 FORMAT (' AO = Cross sectional area at zero flow.')
c 2090 FORMAT (/,28X,'* * *  INITIAL CONDITIONS  * * *')
c 2100 FORMAT (/,' node   Mi/km  Disch       Area      Width  ',
c     $          ' Slope           A1         A2         W1        ',
c     $          ' W2         AO')
c 2110 FORMAT (/,'Branch',I4,' Extends from JNCT',I3,' to JNCT',I3,
c     $        ' and receives',F5.2,' of flow at JNCT',I3,/)
c 2120 FORMAT (I5,F8.2,9G11.4)
c 2130 FORMAT (/,25X,'* * *  OUTPUT  * * *',/)
c 3000 FORMAT (3I5,4E18.5)
C
C      + + + + + + + + + + + END SPECIFICATIONS  + + + + + + + + + + + +
C
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = '@(#)DAFLOW - last modified June 27, 1997 hej '
C     VERSN = Written by HEJ on March 11, 1998
C
C     **************** Zero arrays and preliminaries *******************
      NHRR=1
      DO 40 N=1,NOBR
        JNCU(N)=0
        JNCD(N)=0
        NS(N)=0
        NSI(N)=0
        NXSEC(N)=0
        PF(N)=0.0
        DO 20 I=1,NOSC
          AO(I,N)=0.0
          A1(I,N)=0.0
          A2(I,N)=0.0
          SL(I,N)=0.0
          IOUTDAF(I,N)=0
          X(I,N)=0.0
          TF(I,N)=0.0
          TFI(I,N)=0.0
          W1(I,N)=0.0
          W2(I,N)=0.0
          VIN(I,N)=0.0
C  8-28-2003   AWH
          V(I,N)=0.0
          AQ(I,N)=0.0
   20   CONTINUE
        DO 30 K=1,NOSH
          F(K,N)=0.0
          FI(K,N)=0.0
          PX(K,N)=0.0
          PXI(K,N)=0.0
   30   CONTINUE
   40 CONTINUE
C
C     *********************** Read common input ************************
      READ(LUIN,1000)TITLE
c      WRITE(LUOT,2000)TITLE
      READ(LUIN,1010,ERR=900)NBRCH
      READ(LUIN,1010,ERR=900)NJNCT
      READ(LUIN,1010,ERR=900)NHR
      READ(LUIN,1010,ERR=900)JTS
      READ(LUIN,1010,ERR=900)JGO
      READ(LUIN,1010,ERR=900)IENG
      READ(LUIN,1020,ERR=900)DT
      READ(LUIN,1020,ERR=900)AA
      QI=AA/100000.0
      VI=QI*DT*3600.0
      TIME=(FLOAT(JTS)-0.5)*DT
c      WRITE(LUOT,2010)NBRCH,NJNCT,NHR,DT
      AA=DT*FLOAT(JTS)
c      WRITE(LUOT,2020)AA
c      WRITE(LUOT,2030)JGO
C
C     ************************* Read data for each branch **************
      DO 60 N=1,NBRCH
        READ(LUIN,1030,ERR=900)NXSEC(N),PF(N),JNCU(N),JNCD(N)
        READ(LUIN,1000)TITLE
        DO 50 I=1,NXSEC(N)
          IF(I.LT.NXSEC(N))THEN
            READ(LUIN,*,END=900,ERR=900)K,X(K,N),IOUTDAF(K,N),F(K,N),
     #             A1(K,N),A2(K,N),AO(K,N),SL(K,N),W1(K,N),W2(K,N)
          ELSE
            READ(LUIN,*,END=900,ERR=900)K,X(K,N),IOUTDAF(K,N)
          END IF
   50   CONTINUE
   60 CONTINUE
C
C     ***** Make preliminary computation and write initial conditions **
      IF(IENG.EQ.0)THEN
C       Metric units
c        WRITE(LUOT,2040)
C       XFACT=1609.34
        XFACT=1000.0
      ELSE
C       English units
c        WRITE(LUOT,2050)
        XFACT=5280.00
      END IF
c      WRITE(LUOT,2070)
c      WRITE(LUOT,2080)
c      WRITE(LUOT,2060)
C
c      WRITE(LUOT,2090)
c      WRITE(LUOT,2100)
      J=0
      DO 80 N=1,NBRCH
        NS(N)=NXSEC(N)-1
        NSI(N)=NS(N)
c        WRITE(LUOT,2110)N,JNCU(N),JNCD(N),PF(N),JNCU(N)
        DO 70 I=1,NXSEC(N)
          IF(I.LT.NXSEC(N))THEN
            PX(I,N)=X(I,N)*XFACT
            PXI(I,N)=PX(I,N)
            FI(I,N)=F(I,N)
            IF(F(I,N).GT.0.0)THEN
              A=AO(I,N)+A1(I,N)*(F(I,N)**A2(I,N))
              AA=W1(I,N)*(F(I,N)**W2(I,N))
            ELSE
              A=AO(I,N)
              AA=0.0
            END IF
            VIN(I,N)=A*XFACT*(X(I+1,N)-X(I,N))
            IF(I.GT.1)THEN
              TF(I,N)=F(I,N)-F(I-1,N)
              TFI(I,N)=TF(I,N)
            END IF
c            WRITE(LUOT,2120)I,X(I,N),F(I,N),A,AA,SL(I,N),
c     $                      A1(I,N),A2(I,N),W1(I,N),W2(I,N),AO(I,N)
c            WRITE(LUFLW,3000)J,N,I,F(I,N),A,AA,TF(I,N)
c          ELSE
c            WRITE(LUOT,2120)I,X(I,N),F(I-1,N)
c            WRITE(LUFLW,3000)J,N,NXSEC(N),F(I-1,N)
          END IF
          X(I,N)=X(I,N)*XFACT
   70   CONTINUE
   80 CONTINUE
C
c      WRITE(LUOT,2130)
      GO TO 999
  900   IERR=22
  999 RETURN
      END
C
C      SUBROUTINE GETBC (IERR,J,LUIN,LUOT)
CC
CC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     ******This subroutine reads the boundary conditions for DAFLOW ***
cC
CC     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC
CC     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
CC
CC     + + + + + + + + + + + COMMON VARIABLES (startdaf.com) + + + + + +
cC     DT IDBG JTS TIME TRB(I,N)
CC
CC     + + + + + + + +  + + + LOCAL VARIABLES  + + + + + + + + + + + + +
C      INTEGER I,IERR,J,JJ,K,LUIN,LUOT,N,NBC
CC     CHARACTER*64 VERSN
CC
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
cC     IERR     Error code (0=ok, 20<stop as gracefully as you can)
CC     NBC      Number of boundary conditions to be read
CC
CC     + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + + +
C
C 1000 FORMAT (18X,I3)
C 1010 FORMAT (10X,I3,5X,I3,3X,G14.5)
CC
cC     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
c 2000 FORMAT('Format error on number of boundary condition for time',I5)
C 2010 FORMAT('Format error on boundary condition',I5,' Time step',I5)
CC      + + + + + + + + + + + END SPECIFICATIONS  + + + + + + + + + + + +
CC
CC     unix what information
CC     INCLUDE 'versn.inc'
CC     VERSN = '@(#)DAFLOW - written by HEJ March 16, 1998'
CC
cC     ************************ read boundary conditions ****************
C      READ(LUIN,1000,ERR=900)NBC
C      IF(NBC.GT.0)THEN
CC       ***********  boundary conditons for this time are to be read ***
C        DO 40 K=1,NBC
C          READ(LUIN,1010,ERR=910)N,I,TRB(I,N)
C          JJ=IFIX(TIME/DT+0.501)-JTS+1
c          IF(IDBG.EQ.1) WRITE(LUOT,*)'J,N,I,TRB',JJ,N,I,TRB(I,N)
C   40   CONTINUE
C      END IF
CC
c      GO TO 999
c  900   IERR=22
c        WRITE(LUOT,2000)J
c        GO TO 999
c  910   IERR=22
c        WRITE(LUOT,2010)K,J
c  999 RETURN
c      END
C
C      SUBROUTINE PRERTE
CC
CC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     Prepare for routing NHRR time steps by setting current flow arrays
cC
CC     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
CC
CC     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
CC     F(K,N) FI(K,N) NBRCH NS(N) NSI(N) NXSEC(N) PX(K,N) PXI(K,N)
cC     TF(I,N) TFI(I,N)
CC     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
C      INTEGER  I,N
CC     CHARACTER*64 VERSN
CC
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
CC     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
cC     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
CC     + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + +
CCC     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
CCC     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
CC
cC     unix what information
cC     INCLUDE 'versn.inc'
CC     VERSN = 'Written by HEJ on March 17, 1998'
CC     **************** Zero arrays and preliminaries *******************
C      DO 20 N=1,NBRCH
C        NS(N)=NSI(N)
C        DO 10 I=1,NXSEC(N)
C          TF(I,N)=TFI(I,N)
C   10   CONTINUE
c        DO 20 I=1,NS(N)
C          F(I,N)=FI(I,N)
C          PX(I,N)=PXI(I,N)
C   20 CONTINUE
C  999 RETURN
C      END
CC
C      SUBROUTINE sSETJNVL (JCD,NCD)
CCC
CCC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cCC     Set junction values and mixing codes
cC
CC     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     NOBR   - Maximum number of branches allowed in model
CC
CC     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
CC     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
cC     NBRCH NJNCT 
CC     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
C      INTEGER JCD(NOBR),N,NCD(NOBR)
CC
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
CC     JCD(M)   Code for junction mixing (0=all inflows known, 1=not known)
CC     NCD(N)   Branch code (0=routed, 1=not routed)
cC     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
CC     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
CC     + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + +
CC     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = 'Written by HEJ on March 17, 1998'
C       ** set branch code to not routed and junction code to not mixed*
c        DO 10 N=1,NOBR
c          NCD(N)=1
c          IF(N.LE.NJNCT)THEN
c            JCD(N)=1
c          ELSE
c            JCD(N)=0
c          END IF
c   10   CONTINUE
c  999 RETURN
c      END
C
C
C      SUBROUTINE sRTBR (IERR,LUOT,J,JCD,JN,N,NCD)
CC
CC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     Find branch to route and route it
cC
CC     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     NOBR   - Maximum number of branches allowed in model
CC     NOSC   - Maximum number of cross sections (nodes) allowed in branch
CC     NOSH   - Maximum number of shocks allowed in branch
CC              (NOSH should be at least 4 times NOSC)
CC
cC     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
CC
CC     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
CC     AO(I,N) AQ(I,N) A1(I,N) A2(I,N) DT F(K,N) IDBG JNCD(N) JNCU(N) 
CC     NS(N) NXSEC(N) PX(K,N) QI SL(I,N) TF(I,N) TRB(I,N) VI W1(I,N) 
CC     W2(I,N) X(I,N)
cC
CC     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
C      INTEGER  IERR,JCD(NOBR),J,JN,K,LUOT,N,NCD(NOBR),NSS
C      REAL     DTS,FS(NOSH),PXS(NOSH)
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     DTS      Time step in seconds
C     FS(K)    Flow in shock k of local branch
C     J        Time step  
C     JCD(M)   Code for junction mixing (0=all inflows known, 1=not known)
C     JN       Junction being updated
C     N        Branch being routed
C     NCD(N)   Branch code (0=routed, 1=not routed)
C     PXS(K)   Location of shock K for local branch
C
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C     VERSN = Written by HEJ on March 17, 1998
C     **************** Zero arrays and preliminaries *******************
C      DTS=DT*3600.0
C      N=0
C      IERR=0
c   10 CONTINUE
cC       ***************** Looking for something to route? **************
C        N=N+1
C        IF(NCD(N).EQ.0)GO TO 10
C        IF(JCD(JNCU(N)).NE.0)GO TO 10
C      NSS=NS(N)
C      DO 20 K=1,NSS
C        FS(K)=F(K,N)
C        PXS(K)=PX(K,N)
c   20 CONTINUE
C      CALL ROUTE
C     I      (AO(1,N),A1(1,N),A2(1,N),SL(1,N),DTS,IDBG,IERR,J,LUOT,
C     M      N,NXSEC(N),NSS,FS,PXS,QI,TRB(1,N),TF(1,N),VI,W1(1,N),
C     N      W2(1,N),X(1,N),TIME,JTS)
CC     *********** An error here causes a quick and nasty exit **********
C      IF(IERR.GT.20)GO TO 999
c        NS(N)=NSS
C        DO 30 K=1,NSS
C          F(K,N)=FS(K)
C          PX(K,N)=PXS(K)
c   30   CONTINUE
C       ****************** update junction flows and codes *************
c        NCD(N)=0
c        JN=JNCD(N)
c        JCD(JN)=0
c        AQ(1,N)=TRB(1,N)
c  999 RETURN
c      END
C
C
C      SUBROUTINE sFGQ (I,J,LUOT,N,VO)
CC
CC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     Find volumes in subreaches and compute node flows
cC
CC     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     NOBR   - Maximum number of branches allowed in model
CC     NOSC   - Maximum number of cross sections (nodes) allowed in branch
CC     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
CC     + + + + + + + + + + + + COMMON VARIBLES (startdaf.com) + + + + + +
cC     AO(I,N) AQ(I,N) A1(I,N) A2(I,N) DT F(I,N) IDBG NS(N) NXSEC(N)
CC     PX(I,N) QI TF(I,N) TRB(I,N) V(I,N) X(I,N)
CC
CC     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
C      INTEGER  I,J,K,LUOT,N
C      REAL     AA,BB,VO,XL,XR
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
cC     I     - node of flow or subreach of volume
CC     J     - Time step number
CC     N     - Branch number
CC     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
C      INTRINSIC  ABS
CC     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
CC     + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + +
cC     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
c 2000 FORMAT ('Computed negative flow of',G14.3,' at',
C     $         I4,' branch',I3, ' node',I3)
CC     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
CC     unix what information
CC     INCLUDE 'versn.inc'
CC     VERSN = 'Written by HEJ on March 18, 1998'
CC     
C            XL=X(I,N)
c            XR=X(I+1,N)
C            CALL FKAI(K,NS(N),PX(1,N),X(I,N))
C            CALL FVOL(AO(1,N),A1(1,N),A2(1,N),K,NS(N),NXSEC(N),F(1,N),
C     #                PX(1,N),TF(1,N),V(I,N),X(1,N),XL,XR,AA,BB)
C            AQ(I+1,N)=AQ(I,N)+TRB(I+1,N)+(VO-V(I,N))/(DT*3600.0)
C            IF (AQ(I+1,N).LT.0.0) THEN
CC             ************* Can't have negative flow *******************
c              K=I+1
C              WRITE(LUOT,2000) AQ(K,N),J,N,K
C            END IF
C            IF(AQ(I+1,N).LT.QI)AQ(I+1,N)=0.0
c            IF(IDBG.EQ.1)THEN
C             ************ Debug output ********************************
c              WRITE(LUOT,*)'VO,V(I,N,J),AQ',VO,V(I,N),AQ(I+1,N)
c            END IF
c  999 RETURN
c      END
C
C      SUBROUTINE SETJV2 (JCD,JN,NCD)
CC
CC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     Set junction values after route
cC
CC     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     NOBR   - Maximum number of branches allowed in model
CC     NOSC   - Maximum number of cross sections (nodes) allowed in branch
CC     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
CC     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
cC     AQ(I,N) JNCD(N) JNCU(N) NBRCH NXSEC(N) PF(N) TRB(I,N)
CC
CC     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
C      INTEGER  JCD(NOBR),JN,N,NCD(NOBR)
C      REAL     QJ
CC
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
cC     JCD(M)   Code for junction mixing (0=all inflows known, 1=not known)
CC     JN       Junction in question
CC     NCD(N)   Branch code (0=routed, 1=not routed)
CC     QJ       Flow at junction
C
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
CC     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
CC
CC     unix what information
cC     INCLUDE 'versn.inc'
cC     VERSN = 'Written by HEJ on March 11, 1998'
CC     **************** Set junction codes and flows ********************
C      QJ=0.0
C      DO 10 N=1,NBRCH
C        IF(JNCD(N).EQ.JN)THEN
C          IF(NCD(N).NE.0)JCD(JN)=1
C          QJ=QJ+AQ(NXSEC(N),N)
C        END IF
c   10 CONTINUE
C      IF(JCD(JN).EQ.0) THEN
C        DO 20 N=1,NBRCH
C          IF(JNCU(N).EQ.JN) THEN
C            TRB(1,N)=QJ*PF(N)
C          END IF
C   20   CONTINUE
c      END IF
C  999 RETURN
C      END
CC
C      SUBROUTINE sPRTFLW (LUFLW,LUOT)
CC
CC     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
cC     *** This subroutine prints the results ***************************
cC     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
C      INCLUDE 'params.inc'
CC     NOBR   - Maximum number of branches allowed in model
CC     NOSC   - Maximum number of cross sections (nodes) allowed in branch
CC
CC     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
C      INCLUDE 'startdaf.com'
CC
cC     + + + + + +  COMMON DEFINTIONS  (startdaf.com) + + + + + + + + + +
CC     AO(I,N) AQ(I,N) A1(I,N) A2(I,N) DT IOUTDAF(I,N) JGO JTS NBRCH 
CC     NXSEC(N) TIME TRB(I,N) V(I,N) W1(I,N) W2(I,N) X(I,N) 
CC
CC     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
C      INTEGER I,LUFLW,LUOT,N,NTS
C      REAL    A,AA,Q,W
cC
CC     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
CC     A        Area of flow
CC     NTS      Number of time steps since start of model
C     Q        Discharge
C     W        Top width of channel in subreach
C
C     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
C      INTRINSIC  FLOAT,IFIX,MOD
CC
CC     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
c 2000 FORMAT (' Day',I4,'  Hour',F6.2,' Branch  node  Discharge')
c 2010 FORMAT (20X,I7,I6,G14.4)
C 3000 FORMAT (3I5,4E18.5)
CC     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
CC
CC     ************************ Write results  **************************
C        TIME=TIME+DT
C        I=IFIX(TIME/24.0)+1
C        AA=TIME-FLOAT(I-1)*24.0
c        NTS=IFIX((TIME+DT/2.0+0.0001)/DT)-JTS
C        IF(MOD(NTS,JGO).EQ.0) THEN
C          WRITE(LUOT,2000)I,AA
C        END IF
C        DO 20 N=1,NBRCH
C          DO 10 I=1,NXSEC(N)
C            IF(MOD(NTS,JGO).EQ.0.AND.IOUTDAF(I,N).EQ.1)THEN
c              WRITE(LUOT,2010)N,I,AQ(I,N)
C            END IF
C            IF(I.LT.NXSEC(N))THEN
C              AA=0.0
C              IF(I.GT.1)AA=TRB(I,N)
C              A=V(I,N)/(X(I+1,N)-X(I,N))
C              IF(A.GT.AO(I,N))THEN
c                Q=((A-AO(I,N))/A1(I,N))**(1.0/A2(I,N))
c              ELSE
C                Q=0.0
C              END IF
C              IF(Q.GT.0.0)THEN
C                W=W1(I,N)*(Q**W2(I,N))
C              ELSE
C                W=0.0
C              END IF
c              WRITE(LUFLW,3000)NTS,N,I,AQ(I,N),A,W,AA
C            END IF
C   10     CONTINUE
C          WRITE(LUFLW,3000)NTS,N,NXSEC(N),AQ(NXSEC(N),N)
C   20   CONTINUE
C  999 RETURN
C      END
cC
