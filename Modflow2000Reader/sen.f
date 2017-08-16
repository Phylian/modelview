      SUBROUTINE INITSEN(IUSEN,NPLIST,ISENSU,IERROR)
	USE SensModule
	IMPLICIT NONE
	INTEGER,INTENT(IN)::IUSEN
	INTEGER,INTENT(OUT)::NPLIST,IERROR
      INTEGER LLOC,ISTART,ISTOP
	INTEGER ISENALL,IUHEAD,MXSEN
	INTEGER IPRINTS,ISENSU,ISENPU,ISENFM
	REAL DUM,BL,BU,BSCAL
      CHARACTER*200 LINE
C     READ ITEM 1
      CALL URDCOM(IUSEN,0,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPLIST,DUM,0,0)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISENALL,DUM,0,0)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUHEAD,DUM,0,0)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXSEN,DUM,0,0)
      READ (IUSEN,*,ERR=80) IPRINTS, ISENSU, ISENPU, ISENFM
	ISENSUSAVED = ISENSU
	IERROR=0
	RETURN
 80   IERROR=1
      RETURN
	END

	SUBROUTINE READSEN(IU,NPLIST,OUTNAM,IERROR)
	INTEGER,INTENT(IN)::IU,NPLIST
	INTEGER,INTENT(OUT)::IERROR
	CHARACTER*200,INTENT(IN)::OUTNAM
	INTEGER ISENS,LN,I
	REAL BL,BU,BSCAL
      CHARACTER*200 LINE,FN
      logical exists
      INCLUDE 'param.inc'
      CHARACTER*10 pp(MXPAR)
	REAL BB(MXPAR)
c     read from .sen file
      DO I=1,NPLIST
        READ(IU,*,ERR=80) PARNAM(I),ISENS,LN,B(I),BL,BU,BSCAL
      end do
      close(iu)
c     If outname is not 'NONE' then replace parameter values
c     from the ._b file.
      if (outnam.ne.'NONE') then
        LENGNAM = NONB_LEN(OUTNAM,200)
        FN = OUTNAM(1:LENGNAM)//'._b'
        INQUIRE (FILE=FN,EXIST=EXISTS)
	  if (exists) then
	     open(iu,file=FN,form='formatted',
     1                    status='old',ACTION='read',err=70)
 20	     read (iu,'(A)',end=40) line
           DO I=1,NPLIST
             READ(IU,*,ERR=70) PP(I),ISENS,LN,BB(I),BL,BU,BSCAL
           END DO
           goto 20
 40        do i=1,nplist
              parnam(i)=pp(i)
	        b(i)=bb(i)
	     end do
	   end if
	end if
 70	IERROR=0
      RETURN
 80   IERROR=1
      RETURN
      END
