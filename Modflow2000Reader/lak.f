      SUBROUTINE INITLAKE(IN,NLAKES,IFREFM,itrss,iss,IUNITGWT,nsol,
     1                    ierror)
	implicit none
	integer,intent(in)::in,ifrefm,itrss,iss,iunitgwt,nsol
	integer,intent(out)::nlakes,ierror
	integer ilkcb,nssitr,lm,isol,IRDTAB,lloc,IOUT,NPP,MXVL
	integer ISTART,ISTOP,I
	real theta,sscncr,stages,ssmn,ssmx,clake,R
      CHARACTER*200 LINE
	ierror=0
      
	
	!
      lloc = 1
      IRDTAB = 0
      CALL URDCOM(In, IOUT, line)
! Check for alternate option to specifiy stage/vol/area tables.
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'TABLEINPUT') THEN
         IRDTAB = 1
!         WRITE(IOUT,32)
!   32  FORMAT(1X,I10,' Stage, volume and area relationship specified ',
!     +                'based on an external tabular input file')
      ELSE
        BACKSPACE IN
!        WRITE(IOUT,'(A)') ' Model grid will be used to develop ',
!     +                     ' volume and area relationship. '
      END IF

	IF(IFREFM.EQ.0) THEN
         READ(IN,'(2I10)')NLAKES,ILKCB
         IF (ITRSS.LE.0) THEN 
            READ(IN,'(F10.2,I10,F10.2)') THETA,NSSITR,SSCNCR
         ELSE IF(ITRSS.GT.0) THEN
            READ(IN,'(F10.2)') THETA
         ELSE
	      ierror=1
	      return
         END IF
      ELSE
         READ(IN,*) NLAKES,ILKCB
         IF (ITRSS.LE.0) THEN 
            READ(IN,*) THETA,NSSITR,SSCNCR
         ELSE IF(ITRSS.GT.0) THEN
            READ(IN,*) THETA
         ELSE
            ierror=1
	      return
         END IF
      END IF
C
      IF(NLAKES.LE.0) then
         NLAKES=0
         RETURN
	end if
C1A1----READ INITIAL CONDITIONS FOR ALL LAKES
      IF (IUNITGWT.EQ.0) THEN
         DO 30 LM=1,NLAKES
            IF (IFREFM.EQ.0) THEN
               IF(ISS.NE.0) READ (IN,'(3F10.4)') STAGES,SSMN,
     1           SSMX
               IF(ISS.EQ.0) READ (IN,'(3F10.4)') STAGES
            ELSE
               IF(ISS.NE.0) READ (IN,*) STAGES,SSMN,SSMX
               IF(ISS.EQ.0) READ (IN,*) STAGES
            END IF
 30      CONTINUE
      ELSE
         DO 35 LM=1,NLAKES 
            IF (IFREFM.EQ.0) THEN 
              IF(ISS.NE.0) READ(IN,'(100F10.4)') STAGES,SSMN, 
     1           SSMX,(CLAKE,ISOL=1,NSOL) 
              IF(ISS.EQ.0) READ (IN,'(100F10.4)') STAGES, 
     1                     (CLAKE,ISOL=1,NSOL) 
            ELSE 
              IF(ISS.NE.0) READ (IN,*) STAGES,SSMN,SSMX, 
     1                     (CLAKE,ISOL=1,NSOL) 
              IF(ISS.EQ.0) READ (IN,*) STAGES, 
     1                     (CLAKE,ISOL=1,NSOL) 
            END IF 
  35     continue
      END IF
      RETURN

      END

	subroutine GETLAKE(in,nlakes,IFREFM,iunitgwt,nsol,
     1              lkarr,ncol,nrow,nlay,iend,ierror)
      implicit none
      integer in,nlakes,ifrefm,iunitgwt,nsol,ncol,nrow,nlay
	integer ierror,iend
	real lkarr(ncol*nrow*nlay)
      REAL SILLVT,PRCPLK,EVAPLK,RNF,WTHDRW,CPPT,CRNF,CAUG
	integer itmp,itmp1,lwrt,ncr,i,j,k,kk,loc,numlakecell
	integer NSLMS,IS,ISUB,IC,LM,ISOL
      CHARACTER*24 ANAME
	real,allocatable::bdlknc(:)
	ierror=0
	iend=0
      IF(IFREFM.EQ.0) THEN
         READ(IN,'(3I10)',end=80,err=90) ITMP, ITMP1, LWRT
      ELSE
         READ(IN,*,end=80,err=90) ITMP, ITMP1, LWRT
      END IF
	if (itmp.eq.0) RETURN
      IF(ITMP.Gt.0) then
C
C       READ ARRAY OF LAKE ID'S, LAYER BY LAYER (Record 5)
C
        NCR = NCOL*NROW
        DO 125 K=1,NLAY
          KK = K
          LOC = 1 + (K-1)*NCR
          CALL U2DINT(LKARR(LOC),ANAME,NROW,NCOL,KK,IN,0)
  125   CONTINUE
C
C       READ ARRAY OF BED LEAKANCES, LAYER BY LAYER (Record 6)
C
        allocate(bdlknc(ncol*nrow))
        DO 135 K=1,NLAY
          CALL U2DREL(BDLKNC,ANAME,NROW,NCOL,0,IN,0)
  135   CONTINUE
        deallocate(bdlknc)
C
C--     READ LINKAGE PARAMETERS FOR COALESCING LAKES
C
        IF(IFREFM.EQ.0) THEN
          READ(IN,'(I5)') NSLMS
        ELSE
          READ(IN,*) NSLMS
        END IF
        IF(NSLMS.LE.0) GO TO 760
        DO 700 IS=1,NSLMS
          IF(IFREFM.EQ.0) THEN
            READ(IN,'(16I5)',END=750) IC,(ISUB,I=1,IC)
          ELSE
            READ(IN,*,END=750) IC,(ISUB,I=1,IC)
          END IF
        IF(IC.LE.0) GO TO 750
        IF(IFREFM.EQ.0) THEN
          READ(IN,'(100F10.2)') (SILLVT,I=1,IC-1)
        ELSE
          READ(IN,*) (SILLVT,I=1,IC-1)
        END IF
  700   CONTINUE
  750   CONTINUE
  760   CONTINUE
	end if
      if(itmp1.ge.0) then
        DO 300 LM=1,NLAKES
          IF(IFREFM.EQ.0) THEN
            READ(IN,'(4F10.4)') PRCPLK,EVAPLK,RNF,WTHDRW
          ELSE
            READ(IN,*) PRCPLK,EVAPLK,RNF,WTHDRW
          END IF
          IF(IUNITGWT.LE.0) GO TO 300
          DO 850 ISOL=1,NSOL
            IF(IFREFM.EQ.0) THEN
              IF(WTHDRW.LT.0.0) THEN
                READ(IN,'(3F10.4)')CPPT,CRNF,CAUG
              ELSE
                READ(IN,'(2F10.4)')CPPT,CRNF
              END IF
            ELSE
              IF(WTHDRW.LT.0.0) THEN
                READ(IN,*) CPPT,CRNF,CAUG 
              ELSE
                READ(IN,*) CPPT,CRNF
              END IF
            END IF
  850     CONTINUE
  300   CONTINUE
	end if
	return
80    iend=1
      return
90    ierror=1
	return
	end
