! Time of File Save by ERB: 3/23/2004 12:16PM
C     Revised DEP Feb. and 22 March 2004
C     Last change DEP and LK 25 August 2003
C     Last change:  ERB  17 June 2003
C
C       Added subroutine February 9, 2004; dep
C-------SUBROUTINE GWF1SFR1DF
C
      SUBROUTINE GWF1SFR1DF(NLAKES,NLAKESAR,LKACC7,LCSTAG,LSLAKE,LSTGLD,
     &                      ISTRIN,IDSTRT,ISTROT,ISTGNW,LSCOUT,LSCONQ,
     &                      LSCNRN,LSCPPT,LSCNTB,LSSLIN,LSCQIN,LSCGW,
     &                      LSSIN,LSSOUT,LSCOTO)
C     VERSION  4:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- FEB. 2004
C     ******************************************************************
C     INITIALIZE POINTER VARIABLES USED BY SFR1 TO SUPPORT LAKE3 AND
C     GAGE PACKAGES AND THE GWT PROCESS
C     ******************************************************************
C     CONNECTION TO LAKE PACKAGE
            NLAKES=0
            NLAKESAR=1
            LKACC7=1
            LCSTAG=1
            LSLAKE=1
            LSTGLD=1
            ISTRIN=1
            IDSTRT=1
            ISTROT=1
            ISTGNW=1
C     CONNECTION TO THE GWT PROCESS
           LSCOUT=1
           LSCONQ=1
           LSCNRN=1
           LSCPPT=1
           LSCNTB=1
           LSSLIN=1
           LSCQIN=1
           LSCGW=1
           LSSIN=1
           LSSOUT=1
           LSCOTO=1
      RETURN
      END
C-------SUBROUTINE GWF1SFR1ALP
C
      SUBROUTINE GWF1SFR1ALP(NSTRM,IN,IOUT,
     1                 ISTCB1,ISTCB2,NSS,CONST,MAXPTS,DLEAK,IUNITLAK,
     3                 I15,NSOL,NSFRPAR,NSEGDIM,
     7                 NSTRMAR,NSSAR, ierror)
C     VERSION  4:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- AUGUST 2003
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE AND INITIALIZE NSTRM.
c      WRITE(IOUT,1) IN
c    1 FORMAT(1X,/1X,'SFR1 -- STREAMFLOW ROUTING PACKAGE, VERSION 1',
c     1', 8/15/2003',/,9X,'INPUT READ FROM UNIT',I4)
C
C2------READ COMMENT RECORDS, NSTRM, NSS, NSFRPAR, NPARSEG, CONST,
C        DLEAK, ISTCB1, AND ISTCB2.
      IRFG = 0
	ITRFLG = 0
	NUMTAB=0
      CALL URDCOM(IN,IOUT,LINE)
! Check for alternate input (replacement for setting NSTRM<0).
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'REACHINPUT') THEN
         IRFG = 1
      END IF
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'TRANSROUTE') THEN
        ITRFLG = 1
      END IF
      IF ( ITRFLG.EQ.1 .OR. IRFG.EQ.1 ) READ(IN,'(A)') LINE
! Check keyword for tabular inflow rates.
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'TABFILES') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTAB,R,IOUT,IN)
         IF(NUMTAB.LT.0) NUMTAB=0
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
         IF(MAXVAL.LT.0) MAXVAL=0
         READ(IN,'(A)') LINE
      END IF
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSTRM,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSS,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSFRPAR,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPARSEG,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,CONST,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,DLEAK,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISTCB1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISTCB2,R,IOUT,IN)
      IF ( NSTRM.LT.0 ) THEN
!        WRITE(IOUT, 9036)
! 9036   FORMAT (//, 'NSTRM IS NEGATIVE AND THIS METHOD FOR ',
!     +          'SPECIFYING INFORMATION BY REACH HAS BEEN REPLACED BY ',
!     +          'THE KEYWORD OPTION "REACHINPUT"--PROGRAM STOPPING ',/)
!        CALL USTOP(' ')
        IRFG = 1
        NSTRM = ABS(NSTRM)
      END IF
      IF(NSS.LT.0)NSS=0
      IF(NSFRPAR.LE.0) THEN
         NSFRPAR=0
         NPARSEG=0
      END IF
      IF(NPARSEG.LT.0) NPARSEG=0
C
C2A-----PRINT THE INFORMATION THAT WAS READ
c      WRITE(IOUT,2) NSTRM,NSS,NSFRPAR,NPARSEG,DLEAK,CONST
c    2 FORMAT(//1X,'NUMBER OF STREAM NODES IS ',I6//1X,
c     &'NUMBER OF STREAM',
c     1' SEGMENTS IS ',I6//1X,'NUMBER OF STREAM PARAMETERS IS',I5//
c     2 1X,'NUMBER OF STREAM SEGMENTS DEFINED USING PARAMETERS IS ',I6//
c     3 1X,'MAXIMUM DIFFERENCE IN STREAM HEAD BETWEEN ITERATONS IS ',
c     4 1PE10.2//1X,'CONSTANT FOR MANNINGS EQUATION IS ',E11.4///)
c      IF(ISTCB1.GT.0) WRITE(IOUT,3) ISTCB1
c    3 FORMAT(1X,'FLOW TO AND FROM GROUND WATER FOR EACH STREAM REACH ',
c     1       'WILL BE SAVED ON UNIT ',I4)
c      IF(ISTCB2.GT.0) WRITE(IOUT,4) ISTCB2
c    4 FORMAT(1X,'STREAMFLOW OUT OF EACH REACH WILL BE SAVED ON UNIT ',
c     1       I4)
C
C2B-----CHECK FOR ERRORS
      IF(NSTRM.LE.0.OR.NSS.LE.0)THEN
c        WRITE(IOUT,5)
c    5   FORMAT(//1X,'NO STREAM REACHES (NSTRM) AND/OR SEGMENTS (NSS)--'
c     1         //,' SFR PACKAGE BEING TURNED OFF'///)
        IN=0
        NSS=0
        NSTRM=0
        RETURN
      END IF
      IF(NSFRPAR.GT.0 .AND. NPARSEG.LE.0)THEN
c        WRITE(IOUT,6)
c    6   FORMAT(//1X,'NO STREAM SEGMENTS DEFINED BY PARAMETERS--'
c     1         'NSFRPAR GT ZERO AND NPARSEG LE ZERO'//,
c     1         ' SFR PACKAGE BEING TURNED OFF'///)
        IN=0
        NSS=0
        NSTRM=0
        RETURN
      END IF
      NSTRMAR=NSTRM
      NSSAR=NSS
C
C3------SET LCSTRM EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN RX.
      LCSTRM=ISUMRX
C
C4------CALCULATE SPACE NEEDED FOR STRM LIST (X18).
      ISPA=18*NSTRM
      ISUMRX=ISUMRX+ISPA
C
C5------CALCULATE SPACE NEEDED FOR ISTRM LIST (X5).
      ICSTRM=ISUMIR
      ISPB=5*NSTRM
      ISUMIR=ISUMIR+ISPB
C
C6------CALCULATE SPACE NEEDED FOR SEG LIST (X17).
      NSEGDIM=NSS+NPARSEG
      LCSEG=ISUMRX
      ISPC=17*NSEGDIM
      ISUMRX=ISUMRX+ISPC
C
C7------CALCULATE SPACE NEEDED FOR ISEG, NSEGCK, AND IOTSG LISTS.
      ICSEG=ISUMIR
      ISPD=3*NSEGDIM
      ISUMIR=ISUMIR+ISPD
      LCNSEG=ISUMIR
      ISPE=NSS
      ISUMIR=ISUMIR+ISPE
      LCOTSG=ISUMIR
      ISPJ=NSEGDIM
      ISUMIR=ISUMIR+ISPJ
C
C8------CALCULATE SPACE NEEDED FOR SGOTFLW, AND DVRSFLW LISTS.
      LCOTFLW=ISUMRX
      ISPF=NSS
      ISUMRX=ISUMRX+ISPF
      LCDVFLW=ISUMRX
      ISUMRX=ISUMRX+ISPF
C
C9------CALCULATE SPACE NEEDED FOR XSEC LIST (X16).
      LCXSEC=ISUMRX
      ISPG=16*NSEGDIM
      ISUMRX=ISUMRX+ISPG
C
C10-----CALCULATE SPACE NEEDED FOR IDIVAR LIST.
      LCIVAR=ISUMIR
      ISPH=2*NSEGDIM
      ISUMIR=ISUMIR+ISPH
C
C11-----CALCULATE SPACE NEEDED FOR TABULATED DISCHARGE VERSUS FLOW
C        AND WIDTH RELATIONS.
        LCQSTG=ISUMRX
        MAXPTS=3*50
        ISPI=MAXPTS*NSEGDIM
        ISUMRX=ISUMRX+ISPI
C
      ISPRX=ISPA+ISPC+2*ISPF+ISPG+ISPI
      ISPIR=ISPB+ISPD+ISPE+ISPH+ISPJ
C
C12------CALCULATE SPACE FOR CONNECTION TO LAKE PACKAGE.
      IF (IUNITLAK.LE.0) THEN
            LKACC7=ISUMRX
            LCSTAG=ISUMRX+1
            LSLAKE=ISUMRX+2
            ISTGLD=ISUMRX+3
            ISTRIN=ISUMRX+4
            IDSTRT=ISUMRX+5
            ISTROT=ISUMRX+6
            ISTGNW=ISUMRX+7
          ISUMRX=ISUMRX+8
          ISPRX=ISPRX+8
      ELSE
C
C13------CALCULATE AMOUNT OF SPACE NEEDED FOR STRIN LIST.
          ISTRIN=ISUMRX
          ISPL=NSS
          ISUMRX=ISUMRX+ISPL
C
C14------CALCULATE AMOUNT OF SPACE NEEDED FOR STROUT LIST.
          ISTROT=ISUMRX
          ISPM=NSS
          ISUMRX=ISUMRX+ISPM
C
C15------CALCULATE AMOUNT OF SPACE NEEDED FOR DSTROT LIST.
          IDSTRT=ISUMRX
          ISPN=NSS
          ISUMRX=ISUMRX+ISPN
C
          ISPRX=ISPRX+ISPL+ISPM+ISPN
      END IF
C
C16------CALCULATE SPACE NEEDED FOR SOLUTE ROUTING.
      IF (I15.GT.0) THEN
         NSRCHS=NSTRM*NSOL
         LSCOUT=ISUMRX
         ISUMRX=ISUMRX+NSRCHS
         LSCONQ=ISUMRX
         NSEGSL=NSEGDIM*NSOL
         ISUMRX=ISUMRX+NSEGSL
         LSCNRN=ISUMRX
         ISUMRX=ISUMRX+NSEGSL
         LSCPPT=ISUMRX
         ISUMRX=ISUMRX+NSEGSL
         LSCNTB=ISUMRX
         NSSGSL=NSS*NSOL
         ISUMRX=ISUMRX+NSSGSL
         LSSLIN=ISUMRX
         ISUMRX=ISUMRX+NSOL
         LSCQIN=ISUMRX
         ISUMRX=ISUMRX+NSOL
         LSCGW=ISUMRX
         ISUMRX=ISUMRX+NSOL
         LSSIN=ISUMRX
         ISUMRX=ISUMRX+NSOL
         LSSOUT=ISUMRX
         ISUMRX=ISUMRX+NSOL
         LSCOTO=ISUMRX
         ISUMRX=ISUMRX+NSOL
         ISPRX=ISPRX+NSRCHS+(3*NSSGSL)+NSEGSL+(6*NSOL)
      ELSE
C
C17------ELSE, SET FINITE DUMMY VALUES TO ISUM.
           LSCOUT=ISUMRX
           LSCONQ=ISUMRX+1
           LSCNRN=ISUMRX+2
           LSCPPT=ISUMRX+3
           LSCNTB=ISUMRX+4
           LSSLIN=ISUMRX+5
           LSCQIN=ISUMRX+6
           LSCGW=ISUMRX+7
           LSSIN=ISUMRX+8
           LSSOUT=ISUMRX+9
           LSCOTO=ISUMRX+10
         ISUMRX=ISUMRX+11
         ISPRX=ISPRX+11
      END IF
C
C18-----ALLOCATE ADDITIONAL SPACE REQUIRED FOR FLOWS NEEDED BY SEN PROCESS
      LCSFRQ=ISUMRX
      ISUMRX=ISUMRX+5*NSTRM
C
C19-----PRINT SPACE USED BY STREAM PACKAGE.
c      WRITE (IOUT,8)ISPRX
c    8 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED BY SFR')
c      WRITE (IOUT,9)ISPIR
c    9 FORMAT(1X,I10,' ELEMENTS IN IR ARRAY ARE USED BY SFR')
C
C20-----RETURN.
      RETURN
      END
C
C-------SUBROUTINE GWF1SFR1RPP
C
      SUBROUTINE GWF1SFR1RPP(STRM,ISTRM,NSTRM,IN,IOUT,SEG,ISEG,
     1                  NSS,IDIVAR,IOTSG,SGOTFLW,DVRSFLW,
     2                  MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     3                  CONCPPT,NSOL,NSFRPAR,NSEGDIM,ITERP,
     4                  INAMLOC,IBOUND,NCOL,NROW,NLAY,
     5                  ierror)
C
C
C     VERSION  4:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- AUGUST 2003
C     ******************************************************************
C     READ STREAM DATA THAT IS CONSTANT FOR ENTIRE SIMULATION:
C         REACH DATA AND PARAMETER DEFINITIONS
C
C
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION STRM(18,NSTRM),ISTRM(5,NSTRM),SEG(17,NSEGDIM),
     1          ISEG(3,NSEGDIM),IOTSG(NSEGDIM),
     2          IDIVAR(2,NSEGDIM),SGOTFLW(NSS),DVRSFLW(NSS),
     3          XSEC(16,NSEGDIM),QSTAGE(MAXPTS,NSEGDIM),
     4          IBOUND(NCOL,NROW,NLAY)
      DIMENSION CONCQ(NSEGDIM,NSOL),CONCRUN(NSEGDIM,NSOL),
     1          CONCPPT(NSEGDIM,NSOL)
      DIMENSION IDUM(1)
	Integer ILaySearch, ISegIndex
	Logical FoundActiveCell
C
C     ------------------------------------------------------------------
C
C1------INITIALIZE VARIABLES AND LISTS.
      IRCH=1
      DO WHILE (IRCH.LE.NSTRM)
        II=1
        DO WHILE (II.LE.16)
          STRM(II,IRCH)=0.0
          II=II+1
        END DO
        II=1
        DO WHILE (II.LE.5)
          ISTRM(II,IRCH)=0
          II=II+1
        END DO
        IRCH=IRCH+1
      END DO
C
      KSS=1
      DO WHILE (KSS.LE.NSS)
        SGOTFLW(KSS)=0.0
        DVRSFLW(KSS)=0.0
        KSS=KSS+1
      END DO
C
      KSS=1
      DO WHILE (KSS.LE.NSEGDIM)
        ISEG(1,KSS)=0
        ISEG(2,KSS)=0
        ISEG(3,KSS)=0
        IOTSG(KSS)=0
        IDIVAR(1,KSS)=0
        IDIVAR(2,KSS)=0
        II=1
        DO WHILE (II.LE.17)
          SEG(II,KSS)=0.0
          II=II+1
        END DO
        II=1
        DO WHILE (II.LE.16)
          XSEC(II,KSS)=0.0
          II=II+1
        END DO
        II=1
        DO WHILE (II.LE.(MAXPTS))
          QSTAGE(II,KSS)=0.0
          II=II+1
        END DO
        KSS=KSS+1
      END DO
C
C2------READ AND PRINT DATA FOR EACH STREAM REACH.
      NSEG=0
      NREACH=0
c      WRITE(IOUT,1)
c    1 FORMAT(1X,//3X,'STREAM NETWORK DESCRIPTION: ',//
c     1     3X,'LAYER    ROW    COL   SEGMENT   REACH     LENGTH',
c     2     /26X,'NUMBER   NUMBER    IN CELL',/3X,50('-'))
      II=1
	ISegIndex = 0
      DO WHILE (II.LE.NSTRM)
	  ISegIndex = ISegIndex + 1
        READ(IN,*) KRCH,IRCH,JRCH,JSEG,IREACH,STRM(1,ISegIndex)
c        WRITE(IOUT,2)KRCH,IRCH,JRCH,JSEG,IREACH,STRM(1,II)
c    2   FORMAT(2X,I6,2I7,I8,I9,3X,1PE11.3)
C
C3------CHECK RANGE AND ORDER FOR SEGMENTS AND REACHES.
        IF(JSEG.LE.0 .OR. JSEG.GT.NSS) THEN
c          WRITE(IOUT,3)
c    3     FORMAT(1X,'SEGMENT MUST BE GREATER THAN 0 AND LESS THAN NSS')
          ierror = 1
	    return
c          CALL USTOP(' ')
        END IF
        IF(JSEG.NE.NSEG) THEN
           NSEG=NSEG+1
           NREACH=0
           IF(JSEG.NE.NSEG) THEN
c           WRITE(IOUT,4)
c    4      FORMAT(1X,'SEGMENTS MUST BE IN ORDER FROM 1 THROUGH NSS')
          ierror = 1
	    return
c             CALL USTOP(' ')
           END IF
        END IF
        NREACH=NREACH+1
        IF(IREACH.NE.NREACH) THEN
c           WRITE(IOUT,5)
c    5      FORMAT(1X,'EACH SEGMENT MUST START WITH REACH 1, AND',/
c     1            1X,'REACHES MUST BE NUMBERED CONSECUTIVELY')
          ierror = 1
	    return
c          CALL USTOP(' ')
        END IF
C
C4------CHECK TO SEE IF STREAM IS IN ACTIVE CELL.
        ISTRM(1,ISegIndex)=KRCH
        ISTRM(2,ISegIndex)=IRCH
        ISTRM(3,ISegIndex)=JRCH
        ISTRM(4,ISegIndex)=JSEG
        ISTRM(5,ISegIndex)=IREACH
        SEG(1,ISTRM(4,ISegIndex))=SEG(1,ISTRM(4,ISegIndex))
	1    +STRM(1,ISegIndex)
        IF(IBOUND(JRCH,IRCH,KRCH).EQ.0) THEN
	    FoundActiveCell = .FALSE.
	    do ILaySearch = 1, NLAY
	      if (IBOUND(JRCH,IRCH,ILaySearch).GT.0) then
	        FoundActiveCell = .TRUE.
	        ISTRM(1,ISegIndex)=ILaySearch
	        EXIT
	      elseif (IBOUND(JRCH,IRCH,ILaySearch).LT.0) then
	        EXIT
	      endif
c             WRITE(IOUT,6)JSEG,IREACH
c    6        FORMAT(/1X,'****WARNING--STREAM SEGMENT ',I4,' REACH NO.',
c     1             I4,' IS CONNECTED TO AN INACTIVE CELL',/
c     2             'PROGRAM WILL SEARCH FOR UPPERMOST ACTIVE CELL '
c     3             'DURING CALCULATIONS',/1X,'IF THERE ARE NO ACTIVE '
c     4             'CELLS, STREAM INTERACTION WILL NOT BE CALCULATED')
c          ELSE IF (IBOUND(JRCH,IRCH,KRCH).LT.0) THEN
c             WRITE(IOUT,7)JSEG,IREACH
c    7        FORMAT(/1X,'****WARNING--STREAM SEGMENT ',I4,' REACH NO.',
c     1             I4,' IS CONNECTED TO A CONSTANT HEAD CELL.',/1X,
c     2             'NO STREAM INTERACTION WILL BE CALCULATED.',/1X,
c     3             'SUGGEST REMOVING STREAM REACH FROM CELL OR CHANGE '
c     3             'CELL TO VARIABLE HEAD.'/)
          enddo
	    IF (.NOT.FoundActiveCell) THEN
	      ISegIndex = ISegIndex -1
	    ENDIF
        END IF
  
        II=II+1

      END DO
C
C5------READ PARAMETER DEFINITIONS
      IF(NSFRPAR.GT.0) THEN
        LSTSUM=NSS+1
        II=1
        DO WHILE (II.LE.NSFRPAR)
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,NSEGDIM,IN,IOUT,IP,'SFR','SFR',ITERP,
     &                   NUMINST,INAMLOC)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
C6-----ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          END IF
C7-------READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          I=IB
          DO WHILE (I.LE.NUMINST)
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,ITERP)
            END IF
            ICHK=0
            CALL SGWF1SFR1RDSEG(NLST,LB,IN,IOUT,SEG,ISEG,IDIVAR,
     1                  IOTSG,MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     2                  CONCPPT,NSOL,NSEGDIM,IDUM,1,ICHK,NSS,ierror)
            if (ierror.ne.0) return 
            CALL SGWF1SFR1PRSEG(NLST,LB,IOUT,SEG,ISEG,IDIVAR,
     1                  IOTSG,MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     2                  CONCPPT,NSOL,NSEGDIM,ierror)
            if (ierror.ne.0) return 
            LB=LB+NLST
	      I = I + 1
          END DO
          II=II+1
        END DO
      END IF
	NSTRM = ISegIndex;
C

CC45-----READ TABLES FOR SPECIFIED INFLOWS
!      IF ( Kkper.EQ.1 ) THEN
!        IF ( NUMTAB.GT.0 ) THEN
!          DO i=1,NUMTAB
! segment number, number of rows, unit number
!            READ(In,*) ISEGNUM, NUMVAL, IUNIT
!          END DO
!        END IF
!      END IF

c      WRITE (IOUT,10)
c   10 FORMAT(//)
C
C8------RETURN
      RETURN
      END
C
C-------SUBROUTINE GWF1SFR1RPS
C
      SUBROUTINE GWF1SFR1RPS(STRM,ISTRM,KKPER,NSTRM,IN,IOUT,SEG,ISEG,
     1                  NSEGCK,NSS,IDIVAR,IOTSG,MAXPTS,IPTFLG,XSEC,
     2                  QSTAGE,I15,CONCQ,CONCRUN,CONCPPT,NSOL,
     3                  NSFRPAR,NSEGDIM,ierror)
C
C
C     VERSION  4:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- AUGUST 2003
C     ******************************************************************
C     READ STREAM DATA FOR STRESS PERIOD
C
C
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DIMENSION STRM(18,NSTRM),ISTRM(5,NSTRM),SEG(17,NSEGDIM),
     1          NSEGCK(NSS),ISEG(3,NSEGDIM),IOTSG(NSEGDIM),
     2          IDIVAR(2,NSEGDIM),XSEC(16,NSEGDIM),
     3          QSTAGE(MAXPTS,NSEGDIM)
      DIMENSION CONCQ(NSEGDIM,NSOL),CONCRUN(NSEGDIM,NSOL),
     1          CONCPPT(NSEGDIM,NSOL)
C
C     ------------------------------------------------------------------
C
C
C1------READ ITMP FLAG TO REUSE NON-PARAMETER DATA, 2 PRINTING FLAGS,
C1------AND THE NUMBER OF PARAMETERS BEING USED IN CURRENT STRESS PERIOD.
      IF(NSFRPAR.EQ.0) THEN
        READ(IN,*) ITMP,IRDFLG,IPTFLG
        NP=0
      ELSE
        READ(IN,*) ITMP,IRDFLG,IPTFLG,NP
      END IF
C
C2------CHECK FOR TOO MANY SEGMENTS.
      IF(ITMP.GT.NSS) THEN
c        WRITE(IOUT,6)
c    6   FORMAT(/1X,'CANNOT SPECIFY MORE THAN NSS STREAM SEGMENTS')
          ierror = 1
	    return
c        CALL USTOP(' ')
      END IF
C
C3------REUSE NON-PARAMETER DATA FROM LAST STRESS PERIOD IF ITMP<0
      IF(ITMP.LT.0) THEN
        IF(KKPER.EQ.1)THEN
c          WRITE(IOUT,4)
c    4     FORMAT(//1X,' ***  STREAM SEGMENTS MUST BE DEFINED FOR ',
c     1             'FIRST STRESS PERIOD; CODE STOPPING ***')
          ierror = 1
	    return
c          CALL USTOP(' ')
        ELSE IF(NSFRPAR.EQ.0) THEN
c          WRITE(IOUT,5)
c    5     FORMAT(/1X,
c     1     'REUSING STREAM SEGMENT DATA FROM LAST STRESS PERIOD')
          RETURN
        ELSE
C3A----INITIALIZE NSEGCK TO 0 FOR SEGMENTS THAT ARE DEFINED BY CURRENTLY
C3A----USED PARAMETERS.
c          WRITE(IOUT,5)
          IP=1
          DO WHILE (IP.LE.MXPAR)
            IF (PARTYP(IP).EQ.'SFR' .AND. IACTIVE(IP).GT.0) THEN
              IC=IPLOC(1,IP)
              DO WHILE (IC.LE.IPLOC(2,IP))
                NSEGCK(ISEG(3,IC))=0
                IC=IC+1
              END DO
            END IF
            IP=IP+1
          END DO
        END IF
      ELSE
C
C4------NOT REUSING DATA -- INITIALIZE NSEGCK LIST TO ZERO FOR ALL
C4------SEGMENTS.
        KSS=1
        DO WHILE (KSS.LE.NSS)
          NSEGCK(KSS)=0
          KSS=KSS+1
        END DO
      END IF
C
C5------READ NON-PARAMETER STREAM SEGMENT DATA.
      IF(ITMP.GT.0) THEN
          LSTBEG=1
          ICHK=1
          CALL SGWF1SFR1RDSEG(ITMP,LSTBEG,IN,IOUT,SEG,ISEG,IDIVAR,
     1                  IOTSG,MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     2                  CONCPPT,NSOL,NSEGDIM,NSEGCK,NSS,ICHK,NSS,ierror)
            if (ierror.ne.0) return 
      END IF
C
C6------DEACTIVATE ANY PREVIOUSLY USED STREAM PARAMETERS, AND ACTIVATE
C6------PARAMETERS BEING USED IN CURRENT STRESS PERIOD.
      CALL PRESET('SFR')
      JJ=1
      DO WHILE (JJ.LE.NP)
        CALL SGWF1SFR1PARMOV(IN,IOUT,SEG,ISEG,IDIVAR,
     1          IOTSG,MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     2          CONCPPT,NSOL,NSEGDIM,NSEGCK,NSS,ierror)
            if (ierror.ne.0) return 
        JJ=JJ+1
      END DO
C
C7------CHECK FOR ERRORS IN SEGMENT DATA.
      NSEG=1
      DO WHILE (NSEG.LE.NSS)
        IF (NSEGCK(NSEG).LE.0) THEN
c          WRITE (IOUT,8) NSEG
c    8     FORMAT (/5X,'*** WARNING ***  INPUT DATA FOR SEGMENT',
c     1       I6,' WERE NOT DEFINED')
        ELSE IF (NSEGCK(NSEG).GT.1) THEN
c          WRITE (IOUT,9) NSEG,NSEGCK(NSEG)
c    9     FORMAT (/5X,'*** ERROR ***  DATA FOR SEGMENT',I6,
c     1       ' WERE DEFINED ',I2,' TIMES (INSTEAD OF ONCE)')
          ierror = 1
	    return
c          CALL USTOP(' ')
        END IF
        IF (SEG(8,NSEG).LT.SEG(13,NSEG)) THEN
c          WRITE (IOUT,10) NSEG
c   10     FORMAT (/5X,'*** WARNING *** UPSTREAM ELEVATION IS ',
c     1       'LOWER THAN DOWNSTREAM ELEVATION FOR SEGMENT No. ',I6)
        END IF
        IF (IDIVAR(2,NSEG).GT.0) THEN
c          WRITE (IOUT,11) NSEG
c   11     FORMAT (/5X,'*** WARNING *** IPRIOR > 0 FOR NSEG = ',I6,
c     1               /10X,'THIS OPTION NOT YET AVAILABLE; CODE WILL '
c     2           ,   'ASSUME IPRIOR = 0'/)
          IDIVAR(2,NSEG)=0
        ELSE IF (IDIVAR(2,NSEG).LT.-3) THEN
c          WRITE (IOUT,12) NSEG
c   12     FORMAT (/5X,'*** WARNING *** IPRIOR < -3 FOR NSEG = ',
c     1           I6,/10X,'THIS VALUE IS OUT OF RANGE; CODE WILL ',
c     2           'ASSUME IPRIOR = 0'/)
          IDIVAR(2,NSEG)=0
        ELSE IF (IDIVAR(2,NSEG).EQ.-2) THEN
          IF (SEG(2,NSEG).LT.0.0.OR.SEG(2,NSEG).GT.1.0) THEN
c             WRITE (IOUT,13) NSEG
c   13        FORMAT (/5X,'*** WARNING *** IPRIOR = -2 FOR NSEG = ',
c     1                  I6,' & FLOW VALUE IS OUT OF RANGE (.0 - 1.);',
c     2                  /10X,'ASSUME NO DIVERSION OF FLOW'/)
             SEG(2,NSEG)=0.0
          END IF
        END IF
        NSEG=NSEG+1
       END DO
C
C8------PLACE STREAM SEGMENT IDENTITY NUMBERS IN ISEG ARRAY.
C       5 ASSIGNED TO SEGMENTS NOT RECEIVING TRIBUTARY FLOW.
C       6 ASSINGED TO SEGMENTS THAT DIVERT FLOW.
C       7 ASSIGNED TO SEGMENTS RECEIVING TRIBUTARY FLOW.
      K5=0
      K6=0
      K7=0
      NSEG=1
      DO WHILE (NSEG.LE.NSS)
C
C9------IDENTIFY SEGMENTS THAT DIVERT FLOW.
        IF (IDIVAR(1,NSEG).NE.0) THEN
          ISEG(3,NSEG)=6
          K6=K6+1
C
C10-----IDENTIFY SEGMENTS THAT DO NOT DIVERT FLOW.
        ELSE
          JJ=0
C
C11-----IDENTIFY SEGMENTS THAT RECEIVE TRIBUTARY FLOW.
          II=1
          DO WHILE (II.LE.NSS)
            IF(IOTSG(II).EQ.NSEG) JJ=1
            II=II+1
          END DO
C
C12-----IDENTIFY SEGMENTS THAT DO NOT RECEIVE TRIBUTARY FLOW.
          IF (JJ.EQ.0) THEN
            ISEG(3,NSEG)=5
            K5=K5+1
          ELSE
            ISEG(3,NSEG)=7
            K7=K7+1
c              IF (JJ.NE.1) WRITE(IOUT,14) NSEG,JJ
c   14           FORMAT (//5X,'*** WARNING *** ERROR WHILE ',
c     1             'CLASSIFYING SEGMENTS:   NSEG =',I6,4X,'JJ =',I6//)
          END IF
        END IF
        NSEG=NSEG+1
      END DO
C
C13-----TALLY DIFFERENT STREAM SEGMENT TYPES.
      KTOT=K5+K6+K7
c      WRITE (IOUT,15) K5,K6,K7
c   15 FORMAT(///1X,'CLASSIFICATION & COUNT OF STREAM SEGMENTS BASED',
c     1     ' ON SOURCE OF INFLOW:'//16X,'HEADWATER     DIVERSION     ',
c     2     'RECEIVES TRIBUTARY FLOW'/16X,'---------     ---------    ',
c     3     ' -----------------------'/16X,I6,9X,I6,10X,I6/)
C
C14-----PRINT WARNING IF TALLIED SEGMENTS LESS THAN NSS.
      IF (KTOT.NE.NSS) THEN
c        WRITE (IOUT,16) KTOT,NSS
c   16   FORMAT (/5X,'*** WARNING ***  INTERNAL ERROR SUMMING ',
c     1         'TYPES OF STREAM SEGMENTS:  NSEG =',I6,5X,'JJ =',I6//)
          ierror = 1
	    return
c        CALL USTOP(' ')
      END IF
C
C15-----PRINT INPUT DATA IF IRDFLG IS ZERO.
      IF(IRDFLG.LE.0)
     1     CALL SGWF1SFR1PRSEG(NSS,1,IOUT,SEG,ISEG,IDIVAR,
     2                  IOTSG,MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     3                  CONCPPT,NSOL,NSEGDIM,ierror)
            if (ierror.ne.0) return 
C
C16----COMPUTE STREAM REACH VARIABLES.
      NSEG=1
      IRCH=1
      DO WHILE (NSEG.LE.NSS)
        ICALC=ISEG(1,NSEG)
        SEGLEN=SEG(1,NSEG)
        RUNOFF=SEG(3,NSEG)
        ETSW=SEG(4,NSEG)
        PPTSW=SEG(5,NSEG)
        HCSLPE=(SEG(6,NSEG)-SEG(11,NSEG))/SEGLEN
        THKSLPE=(SEG(7,NSEG)-SEG(12,NSEG))/SEGLEN
        ELSLPE=(SEG(8,NSEG)-SEG(13,NSEG))/SEGLEN
        WDSLPE=(SEG(9,NSEG)-SEG(14,NSEG))/SEGLEN
        DPSLPE=(SEG(10,NSEG)-SEG(15,NSEG))/SEGLEN
        SUMLEN=0.D0
        DO WHILE(IRCH.LE.NSTRM)
          IF(NSEG.NE.ISTRM(4,IRCH)) GO TO 100
          RCHLEN=STRM(1,IRCH)
          DIST=SUMLEN+(0.5*RCHLEN)
          AVHC=SEG(6,NSEG)-(HCSLPE*DIST)
          AVTHK=SEG(7,NSEG)-(THKSLPE*DIST)
          AVDPTH=SEG(10,NSEG)-(DPSLPE*DIST)
          STRM(2,IRCH)=ELSLPE
          STRM(3,IRCH)=SEG(8,NSEG)-(ELSLPE*DIST)
          STRM(4,IRCH)=STRM(3,IRCH)-AVTHK
          STRM(6,IRCH)=AVHC
          STRM(8,IRCH)=AVTHK
          STRM(12,IRCH)=RUNOFF*(RCHLEN/SEGLEN)
          IF(ICALC.EQ.0) THEN
            STRM(5,IRCH)=SEG(9,NSEG)-(WDSLPE*DIST)
            STRM(7,IRCH)=AVDPTH
            STRM(13,IRCH)=ETSW*RCHLEN*STRM(5,IRCH)
            STRM(14,IRCH)=PPTSW*RCHLEN*STRM(5,IRCH)
            STRM(15,IRCH)=AVDPTH+STRM(3,IRCH)
            STRM(16,IRCH)=(AVHC*STRM(5,IRCH)*RCHLEN)/AVTHK
          ELSE IF (ICALC.EQ.1) THEN
            STRM(5,IRCH)=SEG(9,NSEG)-(WDSLPE*DIST)
            STRM(7,IRCH)=1.0
            STRM(13,IRCH)=ETSW*RCHLEN*STRM(5,IRCH)
            STRM(14,IRCH)=PPTSW*RCHLEN*STRM(5,IRCH)
            STRM(15,IRCH)=STRM(3,IRCH)
            STRM(16,IRCH)=(AVHC*STRM(5,IRCH)*RCHLEN)/AVTHK
          ELSE IF (ICALC.GE.2.AND.ICALC.LE.4) THEN
            STRM(5,IRCH)=1.0
            STRM(7,IRCH)=1.0
            STRM(13,IRCH)=ETSW*RCHLEN
            STRM(14,IRCH)=PPTSW*RCHLEN
            STRM(15,IRCH)=STRM(3,IRCH)
            STRM(16,IRCH)=(STRM(5,IRCH)*STRM(1,IRCH)*
     1                   STRM(6,IRCH))/STRM(8,IRCH)
          END IF
          SUMLEN=SUMLEN+RCHLEN
          IRCH=IRCH+1
        END DO
100     NSEG=NSEG+1
      END DO
C
C17------CHECK VALUES IN STREAM CROSS SECTION LIST (XSEC).
      NSEG=1
      DO WHILE (NSEG.LE.NSS)
        ICALC=ISEG(1,NSEG)
        IF(ICALC.EQ.2) THEN
          JJ=1
          IF(XSEC(JJ,NSEG).NE.0.0) THEN
c            WRITE(IOUT,37)NSEG
c   37       FORMAT(1X,/1X,'EIGHT POINT CROSS SECTION FOR ',
c     1            'STREAM SEGMENT ',I6,1X,'DOES NOT BEGIN WITH ',
c     2            'ZERO FOR FIRST VALUE -- PROGRAM STOPPING')
             ierror = 1
	       return
c            CALL USTOP(' ')
          END IF
          DO WHILE (JJ.LE.8)
            IF(XSEC(JJ,NSEG).LT.0.0) THEN
c              WRITE(IOUT,38)NSEG,JJ,XSEC(JJ,NSEG)
c   38         FORMAT(1X,/1X,'STREAM SEGMENT ',I6,5X,
c     1             'HAS A NEGATIVE X DISTANCE FOR POINT ',
c     2             I5,3X,'INPUT VALUE IS ',E10.3,//1X,
c     3             'ALL VALUES MUST BE POSITIVE WITH ',
c     4             'FIRST X VALUE STARTING AT EXTREME LEFT ',
c     5             'EDGE OF SECTION LOOKING DOWNSTREAM ',
c     6             'PROGRAM STOPPING')
               ierror = 1
  	         return
c              CALL USTOP(' ')
            END IF
            KK=JJ+8
            IF(XSEC(KK,NSEG).LT.0.0) THEN
c              WRITE(IOUT,39)NSEG,KK,XSEC(KK,NSEG)
c   39              FORMAT(1X,/1X,'STREAM SEGMENT ',I6,5X,
c     1             'HAS A NEGATIVE Z DISTANCE FOR POINT ',
c     2             I5,3X,'INPUT VALUE IS ',E10.3,//1X,
c     3             'ALL VALUES MUST BE POSITIVE RELATIVE ',
c     4             'TO STREAMBED ELEVATION ')
               ierror = 1
  	         return
c              CALL USTOP(' ')
            END IF
            JJ=JJ+1
          END DO
        END IF
        NSEG=NSEG+1
      END DO
C
C18------CHECK ROUGHNESS COEFFICIENTS WHEN ICALC = 1 OR 2.
      NSEG=1
      DO WHILE (NSEG.LE.NSS)
        ICALC=ISEG(1,NSEG)
        IF (ICALC.EQ.1) THEN
          ROUGH=SEG(16,NSEG)
          IF (ROUGH.LE.0.0) THEN
c            WRITE(IOUT,40)ROUGH
c   40       FORMAT('ROUGHNESS COEFFICIENT WHEN ICALC ',
c     1           '=1 IS LESS THAN OR EQUAL TO ZERO',//1X,
c     2           'VALUE IS ',1PE10.3,//1X,'PROGRAM STOPPING')
               ierror = 1
  	         return
c            CALL USTOP(' ')
          END IF
        ELSE IF (ICALC.EQ.2) THEN
          ROUGHCH=SEG(16,NSEG)
          ROUGHBNK=SEG(17,NSEG)
          IF (ROUGHCH.LE.0.0) THEN
c            WRITE(IOUT,41) ROUGHCH
c   41       FORMAT('ROUGHNESS COEFFICIENT FOR CHANNEL ',
c     1           'WHEN ICALC =2 IS LESS THAN OR EQUAL TO ZERO',
c     2           //1X,'VALUE IS ',1PE10.3,//1X,'PROGRAM STOPPING')
               ierror = 1
  	         return
c            CALL USTOP(' ')
          ELSE IF (ROUGHBNK.LE.0.0) THEN
c            WRITE(IOUT,42) ROUGHBNK
c   42       FORMAT('ROUGHNESS COEFFICIENT FOR BANK ',
c     1           'WHEN ICALC =2 IS LESS THAN OR EQUAL TO ZERO',
c     2           //1X,'VALUE IS ',1PE10.3,//1X,'PROGRAM STOPPING')
               ierror = 1
  	         return
c            CALL USTOP(' ')
          END IF
        END IF
        NSEG=NSEG+1
      END DO
C
C19------CHECK VALUES IN TABLE OF FLOW VERSUS DEPTH AND WIDTH
C          WHEN ICALC = 4.
      NSEG=1
      DO WHILE (NSEG.LE.NSS)
        ICALC=ISEG(1,NSEG)
        IF(ICALC.EQ.4)NSTRPTS=ISEG(2,NSEG)
        IF(ICALC.EQ.4) THEN
          FLWLW=QSTAGE(1,NSEG)
          IF(FLWLW.LE.0.0) THEN
c            WRITE(IOUT,43) NSEG
c   43       FORMAT(/1X,'FIRST FLOW VALUE IN TABLE OF FLOW ',
c     1                'VERSUS DEPTH AND WIDTH IS LESS THAN OR EQUAL ',
c     2                'TO ZERO FOR SEGMENT NUMBER ',I6,/1X,'VALUE ',
c     3                'SHOULD BE GREATER THAN ZERO-- IT HAS BEEN ',
c     4                'RESET TO 0.1 BUT MAY CAUSE INSTABILITY')
            QSTAGE(1,NSEG)=0.1
          END IF
          DPTHLW=QSTAGE(1+NSTRPTS,NSEG)
          IF(DPTHLW.LE.0.0) THEN
c            WRITE(IOUT,44) NSEG
c   44       FORMAT(/1X,'FIRST DEPTH VALUE IN TABLE OF FLOW ',
c     1                'VERSUS DEPTH AND WIDTH IS LESS THAN OR EQUAL ',
c     2                'TO ZERO FOR SEGMENT NUMBER ',I6,/1X,'VALUE ',
c     3                'SHOULD BE GREATER THAN ZERO-- IT HAS BEEN ',
c     4                'RESET TO 0.01 BUT MAY CAUSE INSTABILITY')
            QSTAGE(1+NSTRPTS,NSEG)=0.01
          END IF
          WDTHLW=QSTAGE(1+2*NSTRPTS,NSEG)
          IF(WDTHLW.LE.0.0) THEN
c            WRITE(IOUT,46) NSEG
c   46       FORMAT(/1X,'FIRST WIDTH VALUE IN TABLE OF FLOW ',
c     1                'VERSUS DEPTH AND WIDTH IS LESS THAN OR EQUAL ',
c     2                'TO ZERO FOR SEGMENT NUMBER ',I6,/1X,'VALUE ',
c     3                'SHOULD BE GREATER THAN ZERO-- IT HAS BEEN ',
c     4                'RESET TO 1.0 BUT MAY CAUSE INSTABILITY')
            QSTAGE(1+2*NSTRPTS,NSEG)=1.0
          END IF
          IPT=2
          DO WHILE(IPT.LE.NSTRPTS)
            FLW1=QSTAGE(IPT-1,NSEG)
            FLW2=QSTAGE(IPT,NSEG)
            DPTH1=QSTAGE((IPT-1)+NSTRPTS,NSEG)
            DPTH2=QSTAGE(IPT+NSTRPTS,NSEG)
            WDTH1=QSTAGE((IPT-1)+(2*NSTRPTS),NSEG)
            WDTH2=QSTAGE(IPT+(2*NSTRPTS),NSEG)
            IF(FLW2.LE.FLW1) THEN
c              WRITE(IOUT,47) NSEG,FLW2,IPT
c   47         FORMAT(/1X,'SEGMENT NUMBER ',I6,' HAS ',
c     1                  'SPECIFIED FLOW VALUE OF ',1PE10.2,' IN ',
c     2                  'LOCATION ',I5,' THAT IS LESS THAN OR EQUAL ',
c     3                  'TO PRECEDING VALUE',/1X,'FLOW VALUES MUST ',
c     4                  'BE GREATER THAN PRECEDING VALUE IN TABLE--',
c     5                  ' PROGRAM STOPPING')
               ierror = 1
  	         return
c              CALL USTOP(' ')
            END IF
            IF(DPTH2.LE.DPTH1) THEN
c              WRITE(IOUT,48) NSEG,DPTH2,IPT
c   48            FORMAT(/1X,'SEGMENT NUMBER ',I6,' HAS ',
c     1                  'SPECIFIED DEPTH VALUE OF ',1PE10.2,' IN ',
c     2                  'LOCATION ',I5,' THAT IS LESS THAN OR EQUAL ',
c     3                  'TO PRECEDING VALUE',/1X,'DEPTH VALUES MUST ',
c     4                  'BE GREATER THAN PRECEDING VALUE IN TABLE--',
c     5                  ' PROGRAM STOPPING')
               ierror = 1
  	         return
c              CALL USTOP(' ')
            END IF
            IF(WDTH2.LE.WDTH1) THEN
c              WRITE(IOUT,49) NSEG,WDTH2,IPT
c   49         FORMAT(/1X,'SEGMENT NUMBER ',I6,' HAS ',
c     1                  'SPECIFIED WIDTH VALUE OF ',1PE10.2,' IN ',
c     2                  'LOCATION ',I5,' THAT IS LESS THAN OR EQUAL ',
c     3                  'TO PRECEDING VALUE',/1X,'WIDTH VALUES MUST ',
c     4                  'BE GREATER THAN PRECEDING VALUE IN TABLE--',
c     5                  ' PROGRAM STOPPING')
               ierror = 1
  	         return
c              CALL USTOP(' ')
            END IF
            IPT=IPT+1
          END DO
        END IF
        NSEG=NSEG+1
      END DO
C
c      WRITE (IOUT,3)
c    3 FORMAT(//)
C
C20------RETURN.
      RETURN
      END
C
C
      END
      SUBROUTINE SGWF1SFR1RDSEG(NLST,LSTBEG,IN,IOUT,SEG,ISEG,IDIVAR,
     1                  IOTSG,MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     2                  CONCPPT,NSOL,NSEGDIM,ISCHK,NISCHK,ICHK,NSS,
     3                  ierror)
C
C
C     VERSION  4:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- AUGUST 2003
C     ******************************************************************
C     READ STREAM SEGMENT DATA -- parameters or non parameters
C
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION SEG(17,NSEGDIM),ISEG(3,NSEGDIM),IOTSG(NSEGDIM),
     1          IDIVAR(2,NSEGDIM),XSEC(16,NSEGDIM),
     2          QSTAGE(MAXPTS,NSEGDIM),ISCHK(NISCHK)
      DIMENSION CONCQ(NSEGDIM,NSOL),CONCRUN(NSEGDIM,NSOL),
     1          CONCPPT(NSEGDIM,NSOL)
C     ------------------------------------------------------------------
C
C8------READ STREAM SEGMENT DATA.
      IQSEG=LSTBEG
      LSTEND=LSTBEG+NLST-1
      DO WHILE (IQSEG.LE.LSTEND)
C
C9------ONLY READ FIRST 4 VARIABLES TO DETERMINE VALUE OF IUPSEG.
        READ(IN,*) N,ICALC,NOUTSEG,IUPSEG
        IF(N.GT.NSS .OR. N.LT.1) THEN
c           WRITE(IOUT,1) N
c    1      FORMAT(1X,/1X,'SEGMENT NUMBER (NSEG) OUT OF RANGE: ',I6)
c           IF(ICHK.NE.0) THEN
c             WRITE(IOUT,2) IQSEG-LSTBEG+1
c    2        FORMAT(1X,'READING ENTRY ',I6,' OF ITEM 6A')
c           ELSE
c             WRITE(IOUT,3) IQSEG-LSTBEG+1
c    3        FORMAT(1X,'READING ENTRY ',I6,' OF ITEM 4A')
c           END IF
               ierror = 1
  	         return
c           CALL USTOP(' ')
        END IF
C
C10-----DETERMINE WHERE DATA ARE STORED
        IF(ICHK.NE.0) THEN
C  Store data in active segment area
           NSEG=N
           ISCHK(N)=ISCHK(N)+1
        ELSE
C  Store data in parameter area
           NSEG=IQSEG
           ISEG(3,IQSEG)=N
           SEG(1,NSEG)=SEG(1,N)
        END IF
        BACKSPACE IN
C
C11-----READ DATA SET 4A FOR SEGMENTS THAT ARE NOT DIVERSIONS.
        IF (IUPSEG.LE.0) THEN
          IF(ICALC.EQ.0) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         IDIVAR(1,NSEG),(SEG(JJ,NSEG),JJ=2,5)
          ELSE IF (ICALC.EQ.1) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         IDIVAR(1,NSEG),(SEG(JJ,NSEG),JJ=2,5),SEG(16,NSEG)
          ELSE IF (ICALC.EQ.2) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         IDIVAR(1,NSEG),(SEG(JJ,NSEG),JJ=2,5),
     2         (SEG(JK,NSEG),JK=16,17)
          ELSE IF (ICALC.EQ.3) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         IDIVAR(1,NSEG),(SEG(JJ,NSEG),JJ=2,5),SEG(9,NSEG),
     2         SEG(10,NSEG),SEG(14,NSEG),SEG(15,NSEG)
          ELSE IF (ICALC.EQ.4) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         IDIVAR(1,NSEG),ISEG(2,NSEG),(SEG(JJ,NSEG),JJ=2,5)
          END IF
        ELSE
C
C12-----READ DATA 4A FOR SEGMENTS THAT ARE DIVERSIONS FROM STREAMS.
          IF(ICALC.LE.0) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         (IDIVAR(II,NSEG),II=1,2),(SEG(JJ,NSEG),JJ=2,5)
          ELSE IF (ICALC.EQ.1) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         (IDIVAR(II,NSEG),II=1,2),(SEG(JJ,NSEG),JJ=2,5),
     2         SEG(16,NSEG)
          ELSE IF (ICALC.EQ.2) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         (IDIVAR(II,NSEG),II=1,2),(SEG(JJ,NSEG),JJ=2,5),
     2         (SEG(JK,NSEG),JK=16,17)
          ELSE IF (ICALC.EQ.3) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         (IDIVAR(II,NSEG),II=1,2),(SEG(JJ,NSEG),JJ=2,5),
     2         SEG(9,NSEG),SEG(10,NSEG),SEG(14,NSEG),SEG(15,NSEG)
          ELSE IF (ICALC.EQ.4) THEN
            READ(IN,*)IDUM,ISEG(1,NSEG),IOTSG(NSEG),
     1         (IDIVAR(II,NSEG),II=1,2),ISEG(2,NSEG),
     2         (SEG(JJ,NSEG),JJ=2,5)
          END IF
        END IF
C
C13-----READ DATA SET 4B.
        IF (ICALC.LE.0) THEN
          READ(IN,*)(SEG(JJ,NSEG),JJ=6,10)
        ELSE IF (ICALC.EQ.1) THEN
          READ(IN,*)(SEG(JJ,NSEG),JJ=6,9)
        ELSE IF (ICALC.GE.2.AND.ICALC.LE.4) THEN
          READ(IN,*)(SEG(JJ,NSEG),JJ=6,8)
        END IF
C
C14-----READ DATA SET 4C.
        IF (ICALC.LE.0) THEN
          READ(IN,*)(SEG(JJ,NSEG),JJ=11,15)
        ELSE IF (ICALC.EQ.1) THEN
          READ(IN,*)(SEG(JJ,NSEG),JJ=11,14)
        ELSE IF (ICALC.GE.2.AND.ICALC.LE.4) THEN
          READ(IN,*)(SEG(JJ,NSEG),JJ=11,13)
        END IF
C
C15-----READ DATA SET 4D FOR SEGMENT WHEN ICALC IS 2.
        IF (ICALC.EQ.2) THEN
          READ(IN,*)(XSEC(JJ,NSEG),JJ=1,8)
          READ(IN,*)(XSEC(JJ,NSEG),JJ=9,16)
        END IF
C
C16-----READ DATA SET 4E FOR SEGMENT WHEN ICALC IS 4.
        IF (ICALC.EQ.4) THEN
          NSTRPTS=ISEG(2,NSEG)
          IF(NSTRPTS.LT.2) THEN
c            WRITE(IOUT,6)N
c    6       FORMAT(/1X,'NUMBER OF POINTS USED TO RELATE ',
c     1          'STREAMFLOW WITH STREAM DEPTH AND WIDTH FOR ',
c     2          'SEGMENT ',I6,' IS LESS THAN TWO'//1X,
c     3          'PROGRAM STOPPING')
               ierror = 1
  	         return
          ELSE IF(NSTRPTS.GT.MAXPTS/3) THEN
c            WRITE(IOUT,7)N,NSTRPTS
c    7       FORMAT(/1X,'FOR SEGMENT ',I6,' NUMBER OF POINTS',
c     1            'USED TO RELATE STREAMFLOW WITH DEPTH AND ',
c     2            'WIDTH IS ',I5//1X,'WHICH IS MORE THAN ',
c     3            'MAXIMUM NUMBER OF 50 POINTS',//1X,
c     4            'PROGRAM STOPPING'//)
               ierror = 1
  	         return
c             CALL USTOP(' ')
          ELSE
            READ(IN,*)(QSTAGE(JJ,NSEG),JJ=1,NSTRPTS)
            READ(IN,*)(QSTAGE(JJ,NSEG),JJ=NSTRPTS+1,2*NSTRPTS)
            READ(IN,*)(QSTAGE(JJ,NSEG),JJ=2*NSTRPTS+1,3*NSTRPTS)
          END IF
        END IF
C
C17-----READ DATA SET 4F FOR SEGMENT IF SOLUTES SPECIFIED.
        IF (I15.GT.0) THEN
          ISOL=1
          DO WHILE (ISOL.LE.NSOL)
            IF(IDIVAR(1,NSEG).EQ.0) THEN
              READ(IN,*) CONCQ(NSEG,ISOL),CONCRUN(NSEG,ISOL),
     1                   CONCPPT(NSEG,ISOL)
            ELSE
              READ(IN,*) CONCRUN(NSEG,ISOL),CONCPPT(NSEG,ISOL)
            END IF
             ISOL=ISOL+1
          END DO
        END IF
C
        IQSEG=IQSEG+1
      END DO
C
      RETURN
      END
      SUBROUTINE SGWF1SFR1PARMOV(IN,IOUT,SEG,ISEG,IDIVAR,
     1          IOTSG,MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     2          CONCPPT,NSOL,NSEGDIM,NSEGCK,NSS,ierror)
C
C
C     VERSION  4:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- AUGUST 2003
C     ******************************************************************
C     MOVE STREAM PARAMETER DATA INTO ACTIVE SEGMENTS
C
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DIMENSION SEG(17,NSEGDIM),ISEG(3,NSEGDIM),IOTSG(NSEGDIM),
     1          IDIVAR(2,NSEGDIM),XSEC(16,NSEGDIM),
     2          QSTAGE(MAXPTS,NSEGDIM),NSEGCK(NSS)
      DIMENSION CONCQ(NSEGDIM,NSOL),CONCRUN(NSEGDIM,NSOL),
     1          CONCPPT(NSEGDIM,NSOL)
      CHARACTER*4 PACK
      CHARACTER*200 LINE
      CHARACTER*10 PNAME,CTMP3,CTMP4
C     ------------------------------------------------------------------
C
      PACK='SFR '
C1------READ PARAMETER NAME AND FIND IT IN THE PARAMETER LIST.
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      PNAME=LINE(ISTART:ISTOP)
c      WRITE(IOUT,1) PNAME
c    1 FORMAT(/,' Parameter:  ',A)
      CALL UPARFIND(PNAME,'SFR','SFR',IP,IOUT)
C
C         DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE
      NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
      NUMINST=IPLOC(3,IP)
      ILOC=IPLOC(4,IP)
      NI=1
      IF(NUMINST.GT.0) THEN
        NLST=NLST/NUMINST
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
        CTMP3=LINE(ISTART:ISTOP)
        IF(CTMP3.EQ.' ') THEN
c          WRITE(IOUT,1000)PACK,PARNAM(IP)
c 1000     FORMAT(/,1X,'Blank instance name in the ',A,
c     &           ' file for parameter ',A)
               ierror = 1
  	         return
c          CALL USTOP(' ')
        ENDIF
c        WRITE(IOUT,1010) CTMP3
c 1010   FORMAT(3X,'Instance:  ',A)
        CALL UPCASE(CTMP3)
        DO 50 KI=1,NUMINST
          CTMP4=INAME(ILOC+KI-1)
          CALL UPCASE(CTMP4)
          IF(CTMP3.EQ.CTMP4) THEN
            NI=KI
            GOTO 55
          ENDIF
   50   CONTINUE
c        WRITE(IOUT,1020) PACK,CTMP3,PARNAM(IP)
c 1020   FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
c     &         A,'" for parameter ',A)
               ierror = 1
  	         return
c        CALL USTOP(' ')
   55   CONTINUE
      ENDIF
C
      IF (IACTIVE(IP).GT.0) THEN
c        WRITE(IOUT,1030) PARNAM(IP)
c 1030   FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
c     &  '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
c     &  ' -- STOP EXECUTION (SGWF1SFR1PARMOV)')
               ierror = 1
  	         return
c        CALL USTOP(' ')
      ENDIF
C
      IACTIVE(IP)=NI
C
C2------MOVE EACH ENTRY FOR THE PARAMETER
      IQSEG=IPLOC(1,IP)+(NI-1)*NLST
      LSTEND=IQSEG+NLST-1
      DO WHILE (IQSEG.LE.LSTEND)
C
C9------DETERMINE VALUES OF ICALC, NSEG, AND IUPSEG.
        ICALC=ISEG(1,IQSEG)
        NSEG=ISEG(3,IQSEG)
        IUPSEG=IDIVAR(1,IQSEG)
C
C10-----COUNT THE NUMBER OF TIMES A SEGMENT IS DEFINED.
        NSEGCK(NSEG)=NSEGCK(NSEG)+1
C
C11-----MOVE DATA SET 4A
        ISEG(1,NSEG)=ISEG(1,IQSEG)
        IOTSG(NSEG)=IOTSG(IQSEG)
        IDIVAR(1,NSEG)=IDIVAR(1,IQSEG)
        IF(IUPSEG.GT.0) IDIVAR(2,NSEG)=IDIVAR(2,IQSEG)
        SEG(2,NSEG)=SEG(2,IQSEG)
        SEG(3,NSEG)=SEG(3,IQSEG)
        SEG(4,NSEG)=SEG(4,IQSEG)
        SEG(5,NSEG)=SEG(5,IQSEG)
        IF (ICALC.EQ.1) THEN
          SEG(16,NSEG)=SEG(16,IQSEG)
        ELSE IF (ICALC.EQ.2) THEN
          SEG(16,NSEG)=SEG(16,IQSEG)
          SEG(17,NSEG)=SEG(17,IQSEG)
        ELSE IF (ICALC.EQ.3) THEN
          SEG(9,NSEG)=SEG(9,IQSEG)
          SEG(10,NSEG)=SEG(10,IQSEG)
          SEG(14,NSEG)=SEG(14,IQSEG)
          SEG(15,NSEG)=SEG(15,IQSEG)
        ELSE IF (ICALC.EQ.4) THEN
          ISEG(2,NSEG)=ISEG(2,IQSEG)
        END IF
C
C13-----MOVE DATA SET 4B.
        IF (ICALC.LE.0) THEN
          JEND=10
        ELSE IF (ICALC.EQ.1) THEN
          JEND=9
        ELSE IF (ICALC.GE.2.AND.ICALC.LE.4) THEN
          JEND=8
        END IF
        JJ=6
        DO WHILE (JJ.LE.JEND)
          SEG(JJ,NSEG)=SEG(JJ,IQSEG)
          JJ=JJ+1
        END DO
        SEG(6,NSEG)=SEG(6,NSEG)*B(IP)
C
C14-----MOVE DATA SET 4C.
        IF (ICALC.LE.0) THEN
          JEND=15
        ELSE IF (ICALC.EQ.1) THEN
          JEND=14
        ELSE IF (ICALC.GE.2.AND.ICALC.LE.4) THEN
          JEND=13
        END IF
        JJ=11
        DO WHILE (JJ.LE.JEND)
          SEG(JJ,NSEG)=SEG(JJ,IQSEG)
          JJ=JJ+1
        END DO
        SEG(11,NSEG)=SEG(11,NSEG)*B(IP)
C
C15-----MOVE DATA SET 4D FOR SEGMENT WHEN ICALC IS 2.
        IF (ICALC.EQ.2) THEN
          JJ=1
          DO WHILE (JJ.LE.16)
            XSEC(JJ,NSEG)=XSEC(JJ,IQSEG)
            JJ=JJ+1
          END DO
        END IF
C
C16-----MOVE DATA SET 4E FOR SEGMENT WHEN ICALC IS 4.
        IF (ICALC.EQ.4) THEN
          NSTRPTS=ISEG(2,NSEG)
          JJ=1
          DO WHILE (JJ .LE. NSTRPTS*3)
            QSTAGE(JJ,NSEG)=QSTAGE(JJ,IQSEG)
            JJ=JJ+1
          END DO
        END IF
C
C17-----MOVE DATA SET 4F FOR SEGMENT IF SOLUTES SPECIFIED.
        IF (I15.GT.0) THEN
          ISOL=1
          DO WHILE (ISOL.LE.NSOL)
            IF(IDIVAR(1,NSEG).EQ.0) THEN
              CONCQ(NSEG,ISOL)=CONCQ(IQSEG,ISOL)
              CONCRUN(NSEG,ISOL)=CONCRUN(IQSEG,ISOL)
              CONCPPT(NSEG,ISOL)=CONCPPT(IQSEG,ISOL)
            ELSE
              CONCRUN(NSEG,ISOL)=CONCRUN(IQSEG,ISOL)
              CONCPPT(NSEG,ISOL)=CONCPPT(IQSEG,ISOL)
            END IF
            ISOL=ISOL+1
          END DO
        END IF
C
        IQSEG=IQSEG+1
      END DO
C
      RETURN
      END
      SUBROUTINE SGWF1SFR1PRSEG(NLST,LSTBEG,IOUT,SEG,ISEG,IDIVAR,
     1                  IOTSG,MAXPTS,XSEC,QSTAGE,I15,CONCQ,CONCRUN,
     2                  CONCPPT,NSOL,NSEGDIM,ierror)
C
C
C     VERSION  4:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- AUGUST 2003
C     ******************************************************************
C     PRINT STREAM SEGMENT DATA -- parameters or non parameters
C
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION SEG(17,NSEGDIM),ISEG(3,NSEGDIM),IOTSG(NSEGDIM),
     1          IDIVAR(2,NSEGDIM),XSEC(16,NSEGDIM),
     2          QSTAGE(MAXPTS,NSEGDIM)
      DIMENSION CONCQ(NSEGDIM,NSOL),CONCRUN(NSEGDIM,NSOL),
     1          CONCPPT(NSEGDIM,NSOL)
C     ------------------------------------------------------------------
C
      LSTEND=NLST+LSTBEG-1
c      WRITE(IOUT,17)
c   17 FORMAT(1X,//20X,'INPUT DATA FOR EACH STREAM SEGMENT',
c     1            /1X,93('-')/)
C
C27-----PRINT INPUT FLOW RATES FOR EACH STREAM SEGMENT.
c      WRITE(IOUT,18)
c   18 FORMAT(1X,'SEGMENT    SEG.     INFLOW   OVERLAND   ',
c     1        'STREAM    STREAM   ICALC  OUTFLOW  DIVERSION PRIORITY',
c     2        /4X,'NO.    LENGTH     RATE     RUNOFF      ',
c     3        'ET       PPT.    METH.  TO SEG.  FROM SEG.    NO.'/)
      NSEG=LSTBEG
      DO WHILE (NSEG.LE.LSTEND)
        IF(LSTBEG.EQ.1) THEN
          NN=NSEG
        ELSE
          NN=ISEG(3,NSEG)
        END IF
c        WRITE(IOUT,19)NN,(SEG(II,NSEG),II=1,5),ISEG(1,NSEG),
c     1                        IOTSG(NSEG),(IDIVAR(JJ,NSEG),JJ=1,2)
c   19   FORMAT(1X,I6,1X,1P5E10.3,2X,I3,3X,I6,3X,I6,4X,I5)
        NSEG=NSEG+1
      END DO
C
C28-----PRINT STREAMBED PROPERTIES AND STREAM DIMENSIONS.
c      IF(LSTBEG.EQ.1) THEN
c        WRITE(IOUT,20)
c   20   FORMAT (1X,//9X,'STREAMBED PROPERTIES AND STREAM ',
c     1        'DIMENSIONS',//1X,'SEGMENT     BED HYD. COND.',6X,
c     2        'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ',
c     3        'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS',/1X,
c     4        '   No.     UPPER     LOWER     UPPER     ',
c     5        'LOWER     UPPER     LOWER     UPPER     LOWER     ',
c     6        'UPPER     LOWER   CHANNEL      BANK'/)
c      ELSE
c        WRITE(IOUT,210)
c  210   FORMAT (1X,//9X,'STREAMBED PROPERTIES AND STREAM ',
c     1        'DIMENSIONS',//1X,'SEGMENT  BED HYD. COND. FACTOR',2X,
c     2        'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ',
c     3        'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS',/1X,
c     4        '   No.     UPPER     LOWER     UPPER     ',
c     5        'LOWER     UPPER     LOWER     UPPER     LOWER     ',
c     6        'UPPER     LOWER   CHANNEL      BANK'/)
c      END IF
      NSEG=LSTBEG
      DO WHILE (NSEG.LE.LSTEND)
        IF(LSTBEG.EQ.1) THEN
          NN=NSEG
        ELSE
          NN=ISEG(3,NSEG)
        END IF
        ICALC=ISEG(1,NSEG)
c        IF(ICALC.EQ.0) THEN
c          WRITE(IOUT,21)NN,SEG(6,NSEG),SEG(11,NSEG),
c     1                    SEG(7,NSEG),SEG(12,NSEG),SEG(8,NSEG),
c     2                    SEG(13,NSEG),SEG(9,NSEG),SEG(14,NSEG),
c     3                    SEG(10,NSEG),SEG(15,NSEG)
c   21     FORMAT(I6,1X,1P10E10.3)
c        ELSE IF (ICALC.EQ.1) THEN
c          WRITE(IOUT,22)NN,SEG(6,NSEG),SEG(11,NSEG),
c     1                    SEG(7,NSEG),SEG(12,NSEG),SEG(8,NSEG),
c     2                    SEG(13,NSEG),SEG(9,NSEG),SEG(14,NSEG),
c     3                    SEG(16,NSEG)
c   22     FORMAT(I6,1X,1P8E10.3,20X,1PE10.3)
c        ELSE IF (ICALC.EQ.2) THEN
c          WRITE(IOUT,23)NN,SEG(6,NSEG),SEG(11,NSEG),
c     1                    SEG(7,NSEG),SEG(12,NSEG),SEG(8,NSEG),
c     2                    SEG(13,NSEG),SEG(16,NSEG),SEG(17,NSEG)
c   23     FORMAT(I6,1X,1P6E10.3,40X,1P2E10.3)
c        ELSE IF (ICALC.GE.3) THEN
c          WRITE(IOUT,24)NN,SEG(6,NSEG),SEG(11,NSEG),
c     1                    SEG(7,NSEG),SEG(12,NSEG),SEG(8,NSEG),
c     2                    SEG(13,NSEG)
c   24     FORMAT(I6,1X,1P6E10.3)
c        END IF
        NSEG=NSEG+1
      END DO
C
C29-----PRINT CROSS SECTIONAL DATA FOR SEGMENTS WITH ICALC=2.
      NSEG=LSTBEG
      IFLG=0
      DO WHILE (NSEG.LE.LSTEND)
        IF(LSTBEG.EQ.1) THEN
          NN=NSEG
        ELSE
          NN=ISEG(3,NSEG)
        END IF
        ICALC=ISEG(1,NSEG)
        IF(ICALC.EQ.2.AND.IFLG.EQ.0) THEN
c          WRITE(IOUT,25)
c   25     FORMAT(1X,/1X,' EIGHT POINT CROSS SECTION DATA ',
c     1               'FOR SEGMENTS WITH ICALC = 2',/3X,' X VALUES',
c     2               ' X VALUES START FROM LEFT SIDE LOOKING ',
c     3               'DOWNSTREAM',//5X,'SEGMENT NO.',
c     4               '        X1        X2        X3        X4',
c     5               '        X5        X6        X7        X8')
          IFLG=1
        END IF
c        IF(ICALC.EQ.2.AND.IFLG.EQ.1) THEN
c          WRITE(IOUT,26)NN,(XSEC(I,NSEG),I=1,8)
c   26     FORMAT(7X,I6,5X,8(1PE10.3))
c        END IF
        NSEG=NSEG+1
      END DO
      NSEG=LSTBEG
      IFLG=0
      DO WHILE (NSEG.LE.LSTEND)
        IF(LSTBEG.EQ.1) THEN
          NN=NSEG
        ELSE
          NN=ISEG(3,NSEG)
        END IF
        ICALC=ISEG(1,NSEG)
        IF(ICALC.EQ.2.AND.IFLG.EQ.0) THEN
c          WRITE(IOUT,27)
c   27     FORMAT(1X,/3X,' Z VALUES ARE RELATIVE TO STREAM',
c     1                  'BED ELEVATION',//5X,'SEGMENT NO.          ',
c     2                  'Z1        Z2        Z3        Z4        Z5',
c     3                  '        Z6        Z7        Z8')
          IFLG=1
        END IF
c        IF (ICALC.EQ.2.AND.IFLG.EQ.1) THEN
c          WRITE(IOUT,28)NN,(XSEC(I,NSEG),I=9,16)
c   28     FORMAT(7X,I6,5X,8(1PE10.3))
c        END IF
        NSEG=NSEG+1
      END DO
C
C30-----PRINT STREAMFLOW, DEPTH AND WIDTH RELATIONS FOR SEGMENTS
C        WITH ICALC=3.
      NSEG=LSTBEG
      IFLG=0
      DO WHILE (NSEG.LE.LSTEND)
        IF(LSTBEG.EQ.1) THEN
          NN=NSEG
        ELSE
          NN=ISEG(3,NSEG)
        END IF
        ICALC=ISEG(1,NSEG)
        IF(ICALC.EQ.3.AND.IFLG.EQ.0) THEN
c          WRITE(IOUT,29)
c   29     FORMAT(/1X,'STREAMFLOW RELATION WITH DEPTH IS ',
c     1            'BASED ON EQUATION Q = CDPTH*(DEPTH)**FDPTH',
c     2            /1X,'STREAMFLOW RELATION WITH WIDTH IS ',
c     3            'BASED ON EQUATION Q = AWDTH*(WIDTH)**BWDTH',
c     4            //1X,'SEGMENT NO.      CDPTH     FDPTH    ',
c     5            'AWDTH     BWDTH'/)
          IFLG=1
        END IF
        IF(ICALC.EQ.3.AND.IFLG.EQ.1) THEN
c          WRITE(IOUT,30)NN,SEG(9,NSEG),SEG(10,NSEG),
c     1                        SEG(14,NSEG),SEG(15,NSEG)
c   30      FORMAT(5X,I6,1P4E10.3)
        END IF
        NSEG=NSEG+1
      END DO
C
C31-----PRINT TABULATED VALUES FOR COMPUTING STREAM WIDTH AND DEPTH
C         FROM STREAMFLOW FOR SEGMENTS WITH ICALC=4.
      NSEG=LSTBEG
      IFLG=0
      DO WHILE (NSEG.LE.LSTEND)
        IF(LSTBEG.EQ.1) THEN
          NN=NSEG
        ELSE
          NN=ISEG(3,NSEG)
        END IF
        ICALC=ISEG(1,NSEG)
        NSTRPTS=ISEG(2,NSEG)
        IF(ICALC.EQ.4.AND.IFLG.EQ.0) THEN
c           WRITE(IOUT,33)
c   33      FORMAT(1X,/1X,'STREAMFLOW RELATION WITH DEPTH ',
c     1              'AND WIDTH IS BASED ON TABULATED VALUES',//2X,
c     2              'SEGMENT NO.   STREAMFLOW       DEPTH       ',
c     3              'WIDTH',/)
           IFLG=1
        END IF
        IPT=1
        IF(ICALC.EQ.4.AND.IFLG.EQ.1) THEN
          DO WHILE(IPT.LE.NSTRPTS)
c            WRITE(IOUT,34)NN,QSTAGE(IPT,NSEG),
c     1                  QSTAGE(NSTRPTS+IPT,NSEG),
c     2                  QSTAGE(2*NSTRPTS+IPT,NSEG)
c   34       FORMAT(5X,I6,2X,3(3X,1PE10.4))
            IPT=IPT+1
          END DO
        END IF
        NSEG=NSEG+1
      END DO
C
C32-----PRINT SOLUTE DATA FOR EACH STREAM SEGMENT.
      IF(I15.GT.0) THEN
        ISOL=1
        DO WHILE(ISOL.LE.NSOL)
c          WRITE(IOUTS,35)ISOL
c   35     FORMAT(1X,//10X,' DATA FOR EACH STREAM SEGMENT:',
c     1                ' SOLUTE No. ',I2//5X,'SEGMENT          ',
c     2                'SOLUTE CONCENTRATION IN:    ',/5X,
c     3                'NUMBER       SEGMENT INFLOW   OVERLAND FLOW',
c     4                3X,'PRECIPITATION')
          NSEG=LSTBEG
          DO WHILE(NSEG.LE.LSTEND)
            IF(LSTBEG.EQ.1) THEN
              NN=NSEG
            ELSE
              NN=ISEG(3,NSEG)
            END IF
cgzh change to output
c            IF(IDIVAR(1,NSEG).EQ.0) THEN
c              WRITE(IOUTS,36)NN,CONCQ(NSEG,ISOL),
c     1                      CONCRUN(NSEG,ISOL),CONCPPT(NSEG,ISOL)
c            ELSE
c              WRITE(IOUTS,37)NN,
c     1                      CONCRUN(NSEG,ISOL),CONCPPT(NSEG,ISOL)
c            END IF
c   36       FORMAT(1X,/4X,I6,9X,1PE10.3,6X,E10.3,6X,E10.3)
c   37       FORMAT(1X,/4X,I6,9X,'   N/A    ',6X,E10.3,6X,E10.3)
            NSEG=NSEG+1
          END DO
          ISOL=ISOL+1
        END DO
c        WRITE (IOUTS,3)
c    3   FORMAT(//)
      END IF
C
      RETURN
      END
C
