      SUBROUTINE BAS5DF(NCOL,NROW,NLAY,INBAS,IUNIT,CUNIT,INUNIT,
     1    IXSEC,IFREFM,ITMUNI,maxunit,ierror)
C
C-----Modified from VERSION 1030 20FEB1996 BAS5DF
C     ******************************************************************
      implicit none
      CHARACTER*80 HEADNG(2)
      CHARACTER*4 CUNIT(40)
      CHARACTER*80 LINE1,LINE2
      INTEGER IUNIT(40)
      INTEGER NCOL,NROW,NLAY,INBAS,INUNIT,IXSEC,IFREFM
	INTEGER maxunit,ierror
      INTEGER ICHFLG,IOUT,LLOC,ISTART,ISTOP,N,NPER,ITMUNI,IAPART,ISTRT
	REAL R
C     ------------------------------------------------------------------
C0------OPEN FILES AND ASSIGN IUNIT VALUES.
      CALL SBAS5O(INUNIT,INBAS,IOUT,IUNIT,CUNIT,maxunit,ierror)
      if (ierror.ne.0) return
C2------READ HEADING.
      READ(INBAS,'(A)',end=999,err=999) HEADNG(1)
      READ(INBAS,'(A)',end=999,err=999) HEADNG(2)
C
C3------READ LINE SPECIFYING NUMBER OF LAYERS,ROWS,COLUMNS,STRESS
C3------PERIODS AND UNITS OF TIME CODE, BUT DON'T DECODE UNTIL IT IS
C3------DETERMINED THAT FREE OR FIXED FORMAT IS BEING USED.
      READ(INBAS,'(A)',end=999,err=999) LINE1
C
C4------READ OPTIONS RECORD AND LOOK FOR OPTIONS
      READ(INBAS,'(A)',end=999,err=999) LINE2
      IXSEC=0
      ICHFLG=0
      IFREFM=0
      LLOC=1
    5 CALL URWORD(LINE2,LLOC,ISTART,ISTOP,1,N,R,-1,INBAS)
      IF(LINE2(ISTART:ISTOP).EQ.'XSECTION') THEN
         IXSEC=1
      ELSE IF(LINE2(ISTART:ISTOP).EQ.'CHTOCH') THEN
         ICHFLG=1
      ELSE IF(LINE2(ISTART:ISTOP).EQ.'FREE') THEN
         IFREFM=1
      END IF
      IF(LLOC.LT.80) GO TO 5
C
C5------READ NUMBER OF LAYERS, ROWS, COLUMNS, STRESS PERIODS, AND
C5------ITMUNI USING FREE OR FIXED FORMAT.
      IF(IFREFM.EQ.0) THEN
         READ(LINE1,'(5I10)',end=999,err=999) NLAY,NROW,NCOL,NPER,ITMUNI
      ELSE
         LLOC=1
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,NLAY,R,-1,INBAS)
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,NROW,R,-1,INBAS)
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,NCOL,R,-1,INBAS)
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,NPER,R,-1,INBAS)
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,ITMUNI,R,-1,INBAS)
      END IF
c
c-------read iapart and istrt so we can skip calling bas5al
      IF(IFREFM.EQ.0) THEN
         READ(INBAS,'(2I10)',end=999,err=999) IAPART,ISTRT
      ELSE
         READ(INBAS,*,end=999,err=999) IAPART,ISTRT
      END IF
C
C9------RETURN
      RETURN
c     Error while reading data file
 999  ierror=7
      return
      END
      SUBROUTINE BAS5RP(IBOUND,INBAS,NCOL,NROW,NLAY,INOC,IHEDUN,IDDNUN,
     1    IOUT,CHEDFM,CDDNFM,IXSEC,LBHDSV,LBDDSV,IFREFM,hnoflo,
     2    ierror)
C-----Modified from VERSION 1345 20FEB1996 BAS5RP
C     ------------------------------------------------------------------
      implicit none
      CHARACTER*24 ANAME(2)
      CHARACTER*20 CHEDFM,CDDNFM
	INTEGER INBAS,NCOL,NROW,NLAY,INOC,IOUT,IXSEC
	INTEGER IHEDUN,IDDNUN,LBHDSV,LBDDSV,IFREFM,ierror
      INTEGER IBOUND(NCOL,NROW,NLAY)
	INTEGER K,KK,ISTRT,IOFLG,IHEDFM,IDDNFM,IPEROC,ITSOC,IBDOPT
	REAL hnoflo
C     ------------------------------------------------------------------
      ierror=0
C
C2------READ BOUNDARY ARRAY(IBOUND) ONE LAYER AT A TIME.
      IF(IXSEC.EQ.0) THEN
         DO 100 K=1,NLAY
         KK=K
         CALL U2DINT(IBOUND(1,1,KK),ANAME(1),NROW,NCOL,KK,INBAS,IOUT,
     1         ierror)
	   if (ierror.NE.0) return
  100    CONTINUE
      ELSE
         CALL U2DINT(IBOUND(1,1,1),ANAME(1),NLAY,NCOL,-1,INBAS,IOUT,
     1         ierror)
	   if (ierror.NE.0) return
      END IF
C
C3------READ HEAD VALUE FOR NO-FLOW CELLS.
      IF(IFREFM.EQ.0) THEN
         READ(INBAS,'(F10.0)',end=999,err=999) hnoflo
      ELSE
         READ(INBAS,*,end=999,err=999) hnoflo
      END IF
C8------SET UP OUTPUT CONTROL.
      CALL SBAS5I(NLAY,ISTRT,IOFLG,INOC,IOUT,IHEDFM,IDDNFM,IHEDUN,
     1   IDDNUN,IPEROC,ITSOC,CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,IFREFM,
     2   ierror)
 1000 RETURN
 999  ierror=7
      return
      END
      SUBROUTINE SBAS5I(NLAY,ISTRT,IOFLG,INOC,IOUT,IHEDFM,IDDNFM,IHEDUN,
     1   IDDNUN,IPEROC,ITSOC,CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,IFREFM,
     2   ierror)
C
C-----Modified from VERSION 1520 20FEB1996 SBAS5I
C     ******************************************************************
C     SET UP OUTPUT CONTROL.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      implicit none
	integer NLAY,ISTRT,IOFLG,INOC,IOUT,IHEDFM,IDDNFM,IHEDUN,
     1   IDDNUN,IPEROC,ITSOC,IBDOPT,LBHDSV,LBDDSV,IFREFM,ierror
	integer LLOC,ISTART,ISTOP,N
	real R
      DIMENSION IOFLG(NLAY,4)
      CHARACTER*20 CHEDFM,CDDNFM
      CHARACTER*80 LINE
C     ------------------------------------------------------------------
C
C1------ASSIGN DEFAULT VALUES.
      CHEDFM=' '
      CDDNFM=' '
      IHEDFM=0
      IDDNFM=0
      IHEDUN=0
      IDDNUN=0
      IBDOPT=1
      LBHDSV=0
      LBDDSV=0
C
C2------TEST OUTPUT CONTROL INPUT UNIT TO SEE IF OUTPUT CONTROL IS
C2------ACTIVE.
      IF(INOC.LE.0) return
C
C3------OUTPUT CONTROL IS ACTIVE.  READ FIRST RECORD AND DECODE FIRST
C3------WORD.  MUST USE URWORD IN CASE FIRST WORD IS ALPHABETIC.
      READ(INOC,'(A)',end=999,err=999) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
C
C4------TEST FOR NUMERIC OUTPUT CONTROL.  FIRST WORD WILL NOT BE
C4------"PERIOD", "HEAD", "DRAWDOWN", OR "COMPACT".
      IF(LINE(ISTART:ISTOP).NE.'PERIOD' .AND. LINE(ISTART:ISTOP).NE.
     1     'HEAD' .AND. LINE(ISTART:ISTOP).NE.'DRAWDOWN' .AND.
     2     LINE(ISTART:ISTOP).NE.'COMPACT') THEN
C4A-----NUMERIC OUTPUT CONTROL.  DECODE THE INITIAL RECORD ACCORDINGLY.
         IF(IFREFM.EQ.0) THEN
            READ(LINE,'(4I10)') IHEDFM,IDDNFM,IHEDUN,IDDNUN
         ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,INOC)
         END IF
         IPEROC=-1
         ITSOC=-1
      ELSE
C4B-----ALPHABETIC OUTPUT CONTROL.  CALL MODULE TO READ INITIAL RECORDS.
         CALL SBAS5J(INOC,IOUT,IHEDFM,IDDNFM,IHEDUN,IDDNUN,
     1         IPEROC,ITSOC,CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,
     2         LINE,LLOC,ISTART,ISTOP,ierror)
	   if (ierror.ne.0) return
      END IF
	return
C
C-------error encountered
  999 ierror=7
      return
      END
      SUBROUTINE SBAS5J(INOC,IOUT,IHEDFM,IDDNFM,IHEDUN,IDDNUN,
     1         IPEROC,ITSOC,CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,
     2         LINE,LLOC,ISTART,ISTOP,ierror)
C
C-----Modified from VERSION 1433 3JAN1995 SBAS5J
C     ******************************************************************
C     READ INITIAL ALPHABETIC OUTPUT CONTROL RECORDS.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      implicit none
      INTEGER INOC,IOUT,IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,
     1   IBDOPT,LBHDSV,LBDDSV,LLOC,ISTART,ISTOP,ierror,N
	REAL R
      CHARACTER*20 CHEDFM,CDDNFM
      CHARACTER*80 LINE
C     ------------------------------------------------------------------
C
C1------ALPHABETIC OUTPUT CONTROL.  WRITE MESSAGE AND SET INITIAL VALUES
C1------FOR IPEROC AND ITSOC.
C###      WRITE(IOUT,91)
   91 FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED ONLY AT TIME STEPS',
     1    ' FOR WHICH OUTPUT IS DESIRED')
      IPEROC=9999
      ITSOC=9999
C
C2------LOOK FOR ALPHABETIC WORDS:
C2A-----LOOK FOR "PERIOD", WHICH INDICATES THE END OF INITIAL OUTPUT
C2A-----CONTROL DATA.  IF FOUND, DECODE THE PERIOD NUMBER AND TIME
C2A-----STEP NUMBER FOR LATER USE.
  100 IF(LINE(ISTART:ISTOP).EQ.'PERIOD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).NE.'STEP') GO TO 2000
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
C###         WRITE(IOUT,101) IHEDFM,IDDNFM
  101    FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,
     1        '    DRAWDOWN PRINT FORMAT CODE IS',I4)
C###         WRITE(IOUT,102) IHEDUN,IDDNUN
  102    FORMAT(1X,'HEADS WILL BE SAVED ON UNIT',I4,
     1        '    DRAWDOWNS WILL BE SAVED ON UNIT',I4)
         GO TO 1000
C
C2B-----LOOK FOR "HEAD PRINT FORMAT" AND "HEAD SAVE FORMAT".  IF
C2B-----FOUND, SET APPROPRIATE FLAGS.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
         ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,
     1            INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
               CHEDFM=LINE(ISTART:ISTOP)
C###               WRITE(IOUT,103) CHEDFM
  103          FORMAT(1X,'HEADS WILL BE SAVED WITH FORMAT: ',A)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                  LBHDSV=1
C###                  WRITE(IOUT,104)
  104             FORMAT(1X,'SAVED HEADS WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2C-----LOOK FOR "DRAWDOWN PRINT FORMAT" AND "DRAWDOWN SAVE FORMAT".
C2C-----IF FOUND, SET APPROPRIATE FLAGS
      ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
         IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
         ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,
     1                   INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
               CDDNFM=LINE(ISTART:ISTOP)
C###               WRITE(IOUT,105) CDDNFM
  105          FORMAT(1X,'DRAWDOWN WILL BE SAVED WITH FORMAT: ',A)
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
               IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                  LBDDSV=1
C###                  WRITE(IOUT,106)
  106             FORMAT(1X,'SAVED DRAWDOWN WILL BE LABELED')
               END IF
            ELSE
               GO TO 2000
            END IF
         ELSE
            GO TO 2000
         END IF
C
C2D-----LOOK FOR "COMPACT BUDGET FILES" -- "COMPACT" IS SUFFICIENT.
C2D-----IF FOUND, SET APPROPRIATE FLAG.
      ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPACT') THEN
         IBDOPT=2
C###         WRITE(IOUT,107)
  107    FORMAT(1X,'COMPACT CELL-BY-CELL BUDGET FILES WILL BE WRITTEN')
C
C2E-----ERROR IF UNRECOGNIZED WORD.
      ELSE
         GO TO 2000
      END IF
C
C3------FINISHED READING A RECORD.  READ NEXT RECORD, IGNORING BLANK
C3------LINES.  GO BACK AND DECODE IT.
  110 READ(INOC,'(A)',END=1000,err=2000) LINE
      IF(LINE.EQ.' ') GO TO 110
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
      GO TO 100
C
C4------RETURN.
 1000 RETURN
C
C5------ERROR DECODING INPUT DATA.
 2000 continue
      ierror=7
	return
      END
      SUBROUTINE SBAS5O(INUNIT,INBAS,IOUT,IUNIT,CUNIT,
     1      maxunit,ierror)
C
C-----Modified from VERSION 0943 18MAR1996 SBAS5O
C     ------------------------------------------------------------------
      implicit none
      INTEGER IUNIT(40)
      CHARACTER*4 CUNIT(40)
      CHARACTER*80 LINE
      CHARACTER*11 FMTARG
      INTEGER INUNIT,INBAS,IOUT,maxunit,ierror
	INTEGER I,LLOC,ITYP1,ITYP2,N,ISTART,ISTOP,IU,INAM1,INAM2,IRECL
	REAL R
      logical unstructbin
      COMMON /bintype/unstructbin
C     ---------------------------------------------------------------
C
C1------INITIALIZE CONSTANTS.
      ierror=0
      INBAS=0
      DO 5 I=1,40
      IUNIT(I)=0
5     CONTINUE
C
C2------READ A LINE; IGNORE BLANK LINES AND COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000,err=999) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') GO TO 10
C
C3------DECODE THE FILE TYPE AND UNIT NUMBER.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,-1,0)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,-1,0)
C
C4------CHECK FOR A VALID FILE TYPE.
      FMTARG='FORMATTED'
c
c-------Ignore the listing file. We don't want to open it.
      if(line(ityp1:ityp2).eq.'LIST') then
         goto 10
C
C4B-----CHECK FOR "BAS" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'BAS') THEN
         INBAS=IU
C
C4C-----CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA(BINARY)') THEN
	   if (unstructbin) then
            FMTARG='BINARY'
	   else
            FMTARG='UNFORMATTED'
	   end if
C
C4D-----CHECK FOR "FORMATTED FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA') THEN
         FMTARG='FORMATTED'
C
C4E-----CHECK FOR MAJOR OPTIONS.
      ELSE
         DO 20 I=1,40
            IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
               IUNIT(I)=IU
               GO TO 30
            END IF
20       CONTINUE
c        If execution reaches this point, then the file type
c        is not recognized. In this case, we just ignore it
c        and go to the next line.
         goto 10
30       CONTINUE
      END IF
C
C5------DETERMINE FILE NAME AND THE ACCESS METHOD (DIRECT OR
C5------SEQUENTIAL). THEN OPEN THE FILE.
      call urword(line,lloc,inam1,inam2,0,n,r,-1,0)
      call urword(line,lloc,istart,istop,1,n,r,-1,0)
      if(line(istart:istop).eq.'direct') then
         call urword(line,lloc,istart,istop,2,irecl,r,-1,0)
         open(unit=iu,file=line(inam1:inam2),form=fmtarg,
     1         access='direct',recl=irecl,action='read',
     2         err=998)
      else
         open(unit=iu,file=line(inam1:inam2),form=fmtarg,
     1         access='sequential',action='read',err=998)
      end if
c
c     Keep track of the highest unit number
      if (iu.gt.maxunit) maxunit = iu
      GO TO 10
1000  if (inbas.eq.0) goto 997
      return
997   ierror=20
      return
c     error while opening data file
998   ierror=6
      return
c     error while reading name file
999   ierror=4
      return
      END
