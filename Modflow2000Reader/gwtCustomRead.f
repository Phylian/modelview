C  GENERAL GROUND WATER TRANSPORT (GWT, or MOC3D) SUBROUTINES
C  FORCE NONZERO STORAGE FOR PACKAGES NOT USED
C
C  GWT1BAS6DF  DEFINE TRANSPORT PROBLEM
C*************************************************************************
C
      SUBROUTINE GWT1BAS6DF(NCOL,NROW,NLAY,IOUT,IOUTS,IN,
     *   INMOC,JUNIT, DUNIT,
     *   NSCOL,NSROW,NSLAY,NODESS,NPMAX,NLIMBO,
     *   NEWPTS,NUMOBS,LSOBSW,
     *   ICSTRT,ICONLY,NODISP,DECAY,DIFFUS,NCINFL,
     *   IABOVE,IBELOW,
     *   IDIM,NPTPND,MOCTYPE,
     *   IDKTIM,IDKRF,IDKZO,IDKFO,IDKZS,IDKFS,
     *   AGER8,
     *   IDPZO,IDPFO,IDPTIM,IDPPS,maxunit,ierror)
C
C*************************************************************************
C
C     ******************************************************************
C     DEFINE KEY MOC PARAMETERS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*80 HEDMOC(2)
      DIMENSION JUNIT(40)
      CHARACTER*4 DUNIT(40)
C
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION DECAY
      COMMON /GWT/ CDEL,RDEL,CNOFLO,CELDIS,FZERO,NZCRIT
      COMMON /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
cellam
      COMMON /ELLAM/ CINV,RINV,BINV,HCINV,HRINV,HBINV,
     *               STINIT,ADINIT,STMASS,ADMASS,OLMASS,
     *               AZERO,
     *               NSC,NSR,NSL,NT,NCTF,NRTF,NLTF,
     *               NEIGHB(8,2),NSLOPE(3),
     *               IDTOP,IDMAX,NCOEF,NSCH,NSRH,NSLH
cellam
      INCLUDE 'ptwt.inc'
C
C************
      ierror=0
C
C     OPEN FILES
      CALL SMOC6O(IN,INMOC,IOUTS,JUNIT,DUNIT,MOCTYPE,maxunit,ierror)
      if (ierror.ne.0) return
	INIPDL = JUNIT(13)
	INIPDA = JUNIT(14)
C
C2------READ AND PRINT A HEADING.
      READ(INMOC,'(A)',err=999,end=999) HEDMOC(1)
      READ(INMOC,'(A)',err=999,end=999) HEDMOC(2)
      READ(INMOC,*,err=999,end=999) ISLAY1,ISLAY2,ISROW1,ISROW2,
     1                              ISCOL1,ISCOL2
C
      NSLAY=ISLAY2-ISLAY1+1
      IF(NSLAY.LE.0) then
	      ierror=20
	      return
C###        STOP ' ILLEGAL GWT SUBGRID (LAYER)'
	end if
      NSROW=ISROW2-ISROW1+1
      IF(NSROW.LE.0) then
	      ierror=20
	      return
C###        STOP ' ILLEGAL GWT SUBGRID (ROW)'
	end if
      NSCOL=ISCOL2-ISCOL1+1
      IF(NSCOL.LE.0) then
	      ierror=20
	      return
C###        STOP ' ILLEGAL GWT SUBGRID (COLUMN)'
	end if
C
C  DEFINE LIMITS FOR THE C' INFLOW ARRAY (ABOVE+NSLAY+BELOW)
      NCINFL=0
      IF(ISROW1.GT.1.OR.ISROW2.LT.NROW.OR.
     *   ISCOL1.GT.1.OR.ISCOL2.LT.NCOL) NCINFL=NSLAY
      IABOVE=0
      IBELOW=0
      IF (ISLAY1.GT.1) THEN  
           NCINFL=NCINFL+1
           IABOVE=1
      ENDIF
      IF (ISLAY2.LT.NLAY) THEN 
           NCINFL=NCINFL+1
           IBELOW=1
      ENDIF
C
C
C4------PRINT # OF LAYERS, ROWS, COLUMNS FOR SOLUTE TRANSPORT.
C###      WRITE(IOUTS,6) ISLAY1,ISLAY2,ISROW1,ISROW2,ISCOL1,ISCOL2
    6 FORMAT(5X,' MAPPING OF SOLUTE-TRANSPORT SUBGRID IN FLOW GRID:'/
     1' FIRST LAYER FOR SOLUTE TRANSPORT =',I4,5X,
     2' LAST LAYER FOR SOLUTE TRANSPORT  =',I4/
     3' FIRST ROW FOR SOLUTE TRANSPORT   =',I4,5X,
     4' LAST ROW FOR SOLUTE TRANSPORT    =',I4/
     5' FIRST COLUMN FOR SOLUTE TRANSPORT=',I4,5X,
     6' LAST COLUMN FOR SOLUTE TRANSPORT =',I4)
cellam
      IF(MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2) THEN
C###        WRITE(IOUTS,5) NSLAY,NSROW,NSCOL
      ELSE IF(MOCTYPE.EQ.3) THEN
C###        WRITE(IOUTS,55) NSLAY,NSROW,NSCOL
      ENDIF
    5 FORMAT(/1X,'UNIFORM DELCOL AND DELROW IN SUBGRID FOR SOLUTE '
     1 ,'TRANSPORT'//,1X,'NO. OF LAYERS = ',I4,'   NO. OF ROWS = ',I4,
     2 '   NO. OF COLUMNS = ',I4)
   55 FORMAT(/1X,'NONUNIFORM DELCOL AND DELROW ALLOWED '
     1 ,'IN SUBGRID FOR SOLUTE '
     2 ,'TRANSPORT'//,1X,'NO. OF LAYERS = ',I4,'   NO. OF ROWS = ',I4,
     3 '   NO. OF COLUMNS = ',I4)
C
C  SET SUBGRID FLAG, ISUBGD=0, USE FULL FLOW GRID
      ISUBGD=0
      IF(NSCOL.LT.NCOL.OR.NSROW.LT.NROW.OR.NSLAY.LT.NLAY) ISUBGD=1
C
C  NODESS IS NUMBER OF SOLUTE-TRANSPORT BLOCKS
      NODESS=NSCOL*NSROW*NSLAY
C
C  READ GENERAL SOLUTE-TRANSPORT CONDITIONS
C  NOTE ANY FLUID SOURCE IS SOLUTE SOURCE, EVEN IF C=0
C  NODISP=1  NO DISPERSION (OR DIFFUSION)
C  DECAY =   DECAY RATE (1/T), SET TO ZERO FOR NO DECAY
C  DIFFUS = MOLECULAR DIFFUSION COEFFICIENT CONSTANT IN WATER
C
C  ICONLY=1  NO SOLUTE SOURCES DURING SIMULATION, ONLY TRANSPORT OF 
C            INITIAL CONDITION
C  ICSTRT=1  INITIAL CONC WILL BE SAVED SO THAT CHANGE IN CONC MAY BE 
C            PRINTED
C    ICONLY IS SET TO 0 IN CODE; USER MUST CHANGE ICONLY IN CODE AND 
C       RECOMPILE TO USE THIS OPTION
C    ICSTRT IS HANDLED THE SAME WAY AS ICONLY; USER MUST ALSO ADD 
C       CODE TO PRINT THE CHANGE IN CONC; INITIAL CONC SAVED IN 
C       ARRAY NAMED "CONINT"
C
      NUMOBS=0
      LSOBSW=1
      ICONLY=0
      ICSTRT=0
C
C###      IF(ICONLY.EQ.1) WRITE(IOUTS,8)
C###      IF(ICSTRT.EQ.1) WRITE(IOUTS,34)
   34 FORMAT(1X,' SAVE INITIAL CONCENTRATIONS')
    8 FORMAT(' NO FLUID SOURCES--TRANSPORT OF INITIAL CONDITION ONLY'/
     *   '   NO FLUID SOURCES ALLOWED WITHIN SOLUTE-TRANSPORT SUBGRID')
C
      READ(INMOC,*,err=999,end=999) NODISP,DECAY,DIFFUS
      IF(NODISP.EQ.1.AND.MOCTYPE.EQ.2) THEN
C###         WRITE(IOUTS,*) '***ERROR***  NODISP MUST = 0 FOR ',
C###     *                  'GWT MOC IMPLICIT SOLVER'
c###         STOP
          ierror=20
	    return
      ENDIF
      IF(NODISP.EQ.1) THEN
C###         WRITE(IOUTS,9)
      ELSE
         NODISP=0
      END IF
    9 FORMAT(' NO SOLUTE DISPERSION')
      IF(DECAY.EQ.0.0) THEN
C###         WRITE(IOUTS,10)
      ELSE
C###         WRITE(IOUTS,11) DECAY
C###         IF(JUNIT(11).GT.0) WRITE(IOUTS,110)
      END IF
   10 FORMAT(' NO SOLUTE DECAY')
   11 FORMAT(' SOLUTE DECAY RATE (1/T), DECAY=',1PG11.5)
  110 FORMAT('  DK PACKAGE ACTIVE, DECAY MAY BE RESET BELOW')
      IF(DIFFUS.LE.0.0) THEN
         DIFFUS=0.0
C###         WRITE(IOUTS,12)
      ELSE
C###         WRITE(IOUTS,13) DIFFUS
      END IF
   12 FORMAT(' NO MOLECULAR DIFFUSION')
   13 FORMAT(' MOLECULAR DIFFUSION CONSTANT, DIFFUS=',1PG11.5)
C
C  FOR EXPLICIT AND IMPLICIT MOC3D
C  READ MAXIMUM NUMBER OF PARTICLES
C  READ NUMBER OF PARTICLES INITIALLY IN EACH CELL
C
cellam
C  SKIP READING NPTPND AND NPMAX FOR IPDL AND IPDA OPTIONS
      IF(INIPDL.GT.0.OR.INIPDA.GT.0) THEN
cgzh newpts=2 means "nptpnd"=1; only needed for debug output I think,
c need to check all ADDPTVAR calls and other pt stuff when ipdx is on
        NEWPTS=2
	  GO TO 40
	END IF
      IF(MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2) THEN
       READ(INMOC,*,err=999,end=999) NPMAX,NPTPND
C  NPMAX SETTING IS AUTOMATED IF USER SETS TO 0
C  LIMBO ARRAY ALWAYS AUTOMATED 
       NPGRID=ABS(NPTPND)*(NSROW*NSCOL*NSLAY)
       IF (NPMAX.EQ.0) NPMAX=NPGRID*2
       NLIMBO=500
       NTEMP=NPGRID/25
       IF(NTEMP.GT.NLIMBO) NLIMBO=NTEMP
C
C  WRITE NPMAX 
C###       WRITE(IOUTS,23) NPMAX
   23 FORMAT(' MAXIMUM NUMBER OF PARTICLES (NPMAX) = ',I8)
cellam
C
C  FOR ELLAM,
C  READ NUMBER OF SPACIAL SUBINTERVALS PER CELL
C  IN THE COL, ROW, AND LAYER DIRECTIONS AND IN TIME
C  DEFAULT TO 4 FOR INPUT LESS THAN (OR TIME =) ZERO
      ELSEIF (MOCTYPE.EQ.3) THEN
       READ(INMOC,*,err=999,end=999) NSCEXP,NSREXP,NSLEXP,NTEXP
C  WRITE NSC,NSR,NSL,NT 
C###       WRITE(IOUTS,*) 
C###       WRITE(IOUTS,*) 'ELLAM INPUT PARAMETERS:'
C###       WRITE(IOUTS,123) NSCEXP,NSREXP,NSLEXP,NTEXP
  123 FORMAT('  NSCEXP, NSREXP, NSLEXP, NTEXP ',/I7,2I8,I7)
c check dimensions; use appropriate NS values
       IF (NSCEXP.LE.0) THEN
         IF(NSCOL.EQ.1) THEN
           NSC=2
         ELSE
           NSC=4
         ENDIF 
       ELSE
          NSC=2**NSCEXP
       ENDIF
       IF (NSREXP.LE.0) THEN
         IF(NSROW.EQ.1) THEN
           NSR=2
         ELSE
           NSR=4
         ENDIF 
       ELSE
          NSR=2**NSREXP
       ENDIF
       IF (NSLEXP.LE.0) THEN
         IF(NSLAY.EQ.1) THEN
           NSL=2
         ELSE
           NSL=4
         ENDIF 
       ELSE
          NSL=2**NSLEXP
       ENDIF
       IF (NTEXP.LE.0) THEN
          NT=4
       ELSE
          NT=2**NTEXP
       ENDIF
C
       NSCH=0.5D0*NSC
       NSRH=0.5D0*NSR
       NSLH=0.5D0*NSL
C
       NCTF=2*NSC
       NRTF=2*NSR
       NLTF=2*NSL
C
       CINV=1/REAL(NSC)
       RINV=1/REAL(NSR)
       BINV=1/REAL(NSL)
       HCINV=0.5D0*CINV
       HRINV=0.5D0*RINV
       HBINV=0.5D0*BINV
C  WRITE NSC,NSR,NSL,NT calculated values
C###       WRITE(IOUTS,223) NSC,NSR,NSL,NT
  223 FORMAT('  NSC, NSR, NSL,  NT (CALCULATED)',/4I5/)
      END IF
cellam
C  DETERMINE DIMENSIONS
      IF(((NSLAY.EQ.1).AND.(NSROW.EQ.1)).OR.  
     *   ((NSLAY.EQ.1).AND.(NSCOL.EQ.1)).OR.  
     *   ((NSCOL.EQ.1).AND.(NSROW.EQ.1))) THEN 
             IDIM=1
      ELSEIF ((NSROW.EQ.1).OR.(NSCOL.EQ.1).OR.
     *        (NSLAY.EQ.1)) THEN
             IDIM=2 
      ELSEIF ((NSLAY.GT.1).AND.(NSCOL.GT.1).AND.(NSROW.GT.1)) THEN
             IDIM=3 
      ENDIF
C
C  FOR EXPLICIT AND IMPLICIT MOC3D
C  CHECK CONSISTENCY OF NPTPND WITH DIMENSIONS OF SUBGRID
C  COMPARE NPTPND VS. IDIM                                
      IF(MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2) THEN
       ISTOP=0
       IF (NPTPND.EQ.0) ISTOP=1
       IF ((NPTPND.GT.4).AND.(NPTPND.NE.8).AND.(NPTPND.NE.9).
     *                AND.(NPTPND.NE.16).AND.(NPTPND.NE.27))
     *                 ISTOP=1 
C
       IF (NPTPND.GT.1) THEN
C  ONE-D 
         IF ((IDIM.EQ.1).AND.(NPTPND.GT.4)) ISTOP=1          
C  TWO-D
         IF ((IDIM.EQ.2).AND.(NPTPND.NE.4).AND.(NPTPND.NE.9)
     *                  .AND.(NPTPND.NE.16))
     *                 ISTOP=1          
C  THREE-D 
         IF ((IDIM.EQ.3).AND.(NPTPND.NE.8).AND.(NPTPND.NE.27))
     *                 ISTOP=1   
       ENDIF
       IF (ISTOP.EQ.1) THEN
C###            WRITE(IOUTS,38)
            ierror=20
	      return
C###            STOP
       ENDIF
   38 FORMAT(' ***ERROR*** NPTPND NOT CONSISTENT WITH SUBGRID ',
     *'DIMENSIONS OR DEFAULT VALUES')
C  ALLOCATE 1 EXTRA SPACE FOR NEW PARTICLES AT SINKS (CELL CENTER)
           NEWPTS=ABS(NPTPND)+1
      ENDIF
C
 40   continue
      RETURN
999   ierror=7
      return
      END


      SUBROUTINE SMOC6O(INUNIT,INMOC,IOUTS,JUNIT,DUNIT,MOCTYPE,
     1      maxunit,ierror)
C
C-----from VERSION 0818 15JULY1993 SBAS5O
C     ******************************************************************
C     OPEN FILES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION JUNIT(40)
      CHARACTER*4 DUNIT(40)
      CHARACTER*80 LINE
      CHARACTER*11 FMTARG
      logical unstructbin
      COMMON /bintype/unstructbin
      INCLUDE 'ptwt.inc'
C     ---------------------------------------------------------------
C
C1------INITIALIZE CONSTANTS.  ILIST IS SET TO 1 ONCE THE LISTING
C1------FILE HAS BEEN OPENED; UNTIL THEN ERROR MESSAGES ARE WRITTEN
C1------ TO "*" UNIT.
      ILIST=0
      IOUTS=0
      INMOC=0
      MOCTYPE=0
      DO 5 I=1,40
      JUNIT(I)=0
5     CONTINUE
C
C2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000,err=999) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') GO TO 10
C
C3------DECODE THE FILE TYPE AND UNIT NUMBER.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUTS,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUTS,INUNIT)
C
C4------CHECK FOR A VALID FILE TYPE.
      FMTARG='FORMATTED'
C
c-------Ignore the MF2k-GWT listing file. We don't want to open it.
      if(line(ityp1:ityp2).eq.'CLST') then
	   goto 10
C
C4B-----CHECK FOR "GWT" FILE TYPE.
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'MOC'.AND.MOCTYPE.EQ.0) THEN
         INMOC=IU
         MOCTYPE=1
         PTWTON=0
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'MOCIMP'.AND.MOCTYPE.EQ.0) THEN
         INMOC=IU
         MOCTYPE=2
         PTWTON=0
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'ELLAM'.AND.MOCTYPE.EQ.0) THEN
         INMOC=IU
         MOCTYPE=3
         PTWTON=0
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'MOCWT'.AND.MOCTYPE.EQ.0) THEN
         INMOC=IU
         MOCTYPE=1
         PTWTON=1
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'MOCWTI'.AND.MOCTYPE.EQ.0) THEN
         INMOC=IU
         MOCTYPE=2
         PTWTON=1
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
            IF(LINE(ITYP1:ITYP2).EQ.DUNIT(I)) THEN
               JUNIT(I)=IU
C       CHECK FOR BINARY FILE
               IF(I.EQ.3.OR.I.EQ.5.OR.I.EQ.7) then
                  if (unstructbin) then
                     FMTARG='BINARY'
                  else
                     FMTARG='UNFORMATTED'
	            end if
	         ENDIF
               GO TO 30
            END IF
20       CONTINUE
c        If execution reaches this point, then the type type
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
      if(line(istart:istop).eq.'DIRECT') then
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
1000  if(inmoc.eq.0) goto 999
      return
c     error while opening data file
998   ierror=6
      return
c     error while reading gwt package file
999   ierror=5
      return
C
      END


      SUBROUTINE CUSTOMREAD(MOCTYPE,INMOC,NPTPND,NEWPTS,CNOFLO,ierror)
	IMPLICIT NONE
	INTEGER MOCTYPE,INMOC,NPTPND,NEWPTS,ierror
	REAL CNOFLO

	REAL PNEWL,PNEWR,PNEWC,CELDIS,FZERO,FDTMTH,EPSSLV,REMCRIT,GENCRIT
	INTEGER IP,INTRPL,NCXIT,IDIREC,MAXIT,IRAND
      INTEGER NPNTCL,ICONFM,NPNTVL,IVELFM,
     1                NPNTDL,IDSPFM,NPNTPL
      INCLUDE 'ptwt.inc'
	ierror=0
      IF((MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2).AND.(NPTPND.LT.0)) THEN
         DO 79 IP=1,NEWPTS-1
         READ(INMOC,*) PNEWL,PNEWR,PNEWC
   79    CONTINUE
      END IF

      IF(MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2) THEN
        READ(INMOC,*) CELDIS,FZERO,INTRPL
      ELSEIF(MOCTYPE.EQ.3) THEN
        READ(INMOC,*) CELDIS
      ENDIF

      IF (MOCTYPE.EQ.2) THEN
        READ(INMOC,*) FDTMTH,NCXIT,IDIREC,EPSSLV,MAXIT
      END IF

CMOCWT
C  REMCRIT (REMOVAL CRITERIA FOR SINKS: 
C    FRACTION OF INITIAL-PARTICLE-WEIGHT PARTICLE CAN BE BEFORE BEING REMOVED)
C  GENCRIT (CRITERIA FOR STRONG SOURCE CELLS:
C    IF RATIO OF WHAT IS COMING FROM SOURCE TO WHAT IS LEAVING CELL ON FACES 
C    > CRITERIA, THEN SET AS STRONG SOURCE
      IF(PTWTON.EQ.1) THEN
        READ(INMOC,*) REMCRIT,GENCRIT,IRAND
      END IF

      IF(MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2) THEN
        READ(INMOC,*) NPNTCL,ICONFM,NPNTVL,IVELFM,
     1                NPNTDL,IDSPFM,NPNTPL
      ELSEIF(MOCTYPE.EQ.3) THEN
        READ(INMOC,*) NPNTCL,ICONFM,NPNTVL,IVELFM,
     1                NPNTDL,IDSPFM
      ENDIF

      READ(INMOC,*) CNOFLO
      RETURN
	END

C  OBS5 OBSERVATION WELLS  

C
C  GWT1OBS5DF READ NUMBER OF OBSERVATION WELLS 
C
C     ******************************************************************
C
      SUBROUTINE GWT1OBS5DF(INOBS, IERROR)
C
C     ******************************************************************
C
C     READ IN NUMOBS
C     ******************************************************************
C
      use mf2kmodule
      implicit none
	integer IOBSFL, INOBS, IERROR
      READ(INOBS,*) NUMGWTOBS, IOBSFL
	if (Allocated(GwtObs)) Deallocate(GwtObs)
	Allocate(GwtObs(NUMGWTOBS), STAT = IERROR)
      RETURN
      END
C
C
C
C
C  OBS5RP READ OBSERVATION WELL INPUT FILE 
C
C     ******************************************************************
C
      SUBROUTINE GWT1OBS5RP(INOBS, IERROR)
C
C     ******************************************************************
C
C     READ OBSERVATION WELL LOCATIONS
C     ******************************************************************
C
C
      use mf2kmodule
      implicit none
      COMMON /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
	integer inode, i, j, k, IOB, INOBS, ierror
	integer ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      interface
        integer function rcl2i(arow,acol,alayer)
          implicit none
          integer,intent(in)::arow,acol,alayer
        end function rcl2i
      end interface
C
C     ******************************************************************
C
!      WRITE (IOUTS,140) NUMOBS
!      WRITE (IOUTS,150)
C READ THE FIRST RECORD
c      IOB=1
c         READ(INOBS,*) IOBLST(1,IOB),IOBLST(2,IOB),IOBLST(3,IOB),
c     *                 IOBLST(4,IOB)
c         K=IOBLST(1,IOB)
c         I=IOBLST(2,IOB)
c         J=IOBLST(3,IOB)
c         IOBUN=IOBLST(4,IOB)
C
!         WRITE(IOUTS,'(5I8,5X,A40)') IOB,K,I,J,IOBUN            
c         IF(K.LT.ISLAY1.OR.K.GT.ISLAY2.OR. 
c     *      I.LT.ISROW1.OR.I.GT.ISROW2.OR. 
c     *      J.LT.ISCOL1.OR.J.GT.ISCOL2) THEN
!            WRITE(IOUTS,*) '***ERROR***   OBSERVATION WELL OUTSIDE',
!     *                      ' SUBGRID'
c            STOP
c         ENDIF
C CYCLE THROUGH THE REMAINING RECORDS, READ ACCORDING TO IOBSFL
c      DO 139 IOB=2,NUMOBS
      DO 139 IOB=1,NUMGWTOBS
c        IF (IOBSFL.LE.0) THEN 
c         READ(INOBS,*) IOBLST(1,IOB),IOBLST(2,IOB),IOBLST(3,IOB),
c     *                 IOBLST(4,IOB)
c         IOBUN=IOBLST(4,IOB)
c        ELSE
c         READ(INOBS,*) IOBLST(1,IOB),IOBLST(2,IOB),IOBLST(3,IOB)
         READ(INOBS,*,err=999,end=999) K,I,J
c        ENDIF
c         K=IOBLST(1,IOB)
c         I=IOBLST(2,IOB)
c         J=IOBLST(3,IOB)
C
!         WRITE(IOUTS,'(5I8,5X,A40)') IOB,K,I,J,IOBUN            
         IF(K.LT.ISLAY1.OR.K.GT.ISLAY2.OR. 
     *      I.LT.ISROW1.OR.I.GT.ISROW2.OR. 
     *      J.LT.ISCOL1.OR.J.GT.ISCOL2) then
              GOTO 999
!            WRITE(IOUTS,*) '***ERROR***   OBSERVATION WELL OUTSIDE',
!     *                      ' SUBGRID'
c            STOP
         ENDIF
	   inode = rcl2i(I,J,K) 
         GwtObs(IOB) = inode
C
C      FLAG = 0 : SEPARATE OUTPUT FILES
C      FLAG = 1 : ONE OUPUT FILE (HEADER WRITTEN LATER)
  139 CONTINUE
!      IF (IOBSFL.GE.1) THEN
!            IOBUN=IOBLST(4,1)
!            WRITE(IOUTS,160) IOBUN
!            WRITE(IOUTS,'(/)')
!      ELSE
!            WRITE(IOUTS,'(/)')
!            WRITE(IOUTS,*) 'OBSERVATION WELL DATA WILL BE'
!     *                     ,' WRITTEN ON UNIT NUMBERS LISTED ABOVE' 
!      ENDIF
!  140 FORMAT(///'COORDINATES FOR',I4,' OBSERVATION WELLS:')
!  150 FORMAT(/'  WELL #   LAYER     ROW  COLUMN    ',
!     *                           'UNIT')
!  160 FORMAT('ALL OBSERVATION WELL DATA WILL BE WRITTEN ON UNIT ',
!     *        I3)
      RETURN
999   ierror=7
      return
      END       
C
