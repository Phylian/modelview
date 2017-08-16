      subroutine mc3df(in,junit,nscol,nsrow,nslay,
     1      maxunit,ierror)
      implicit none
      integer in,junit(40),nscol,nsrow,nslay,maxunit,ierror
c     named common block shared with other subroutines
      integer inmoc,moctype,
     *  iscol1,iscol2,isrow1,isrow2,islay1,islay2,isubgd
      common /mc3/inmoc,moctype
      COMMON /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     locally defined variables
      integer iouts
      character*80 hedmoc
      character*4 DUNIT(40)
      DATA DUNIT/'CRCH','CNCA','CNCB','PRTA','PRTB','VELA','VELB',
     1           'OBS ','AGE ','DP  ','DK  ','    ','    ','    ',
     2           '    ','    ','    ','    ','    ','    ','    ',
     3           '    ','    ','    ','    ','    ','    ','    ',
     4           '    ','    ','    ','    ','    ','    ','    ',
     5           '    ','    ','    ','    ','    '/
c     ------------------------------------------------------------------
      ierror=0
      iouts=0
      call SMOC6O(in,inmoc,iouts,junit,dunit,moctype,
     1    maxunit,ierror)
      if (ierror.ne.0) return
      read(inmoc,'(a)',err=999,end=999) hedmoc
      read(inmoc,'(a)',err=999,end=999) hedmoc
      read(inmoc,*,err=999,end=999) islay1,islay2,isrow1,isrow2,
     1    iscol1,iscol2
      nslay=islay2-islay1+1
      nsrow=isrow2-isrow1+1
      nscol=iscol2-iscol1+1
      return
c     Error while reading data file
999   ierror=7
      return
      end

      SUBROUTINE Mc3Read(THCK,NSCOL,NSROW,NSLAY,NCOL,NROW,NLAY,
     *  cnoflo,ierror)
      implicit none
      real THCK(*),cnoflo
      integer nscol,nsrow,nslay,ncol,nrow,nlay,ierror
C
      integer inmoc,moctype,
     *  iscol1,iscol2,isrow1,isrow2,islay1,islay2,isubgd
      common /mc3/inmoc,moctype
      COMMON /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
C
      CHARACTER*24 ANAME
      integer nscr,nodess,ncinfl,nrdummy,kk,ks,loc,ip,iz, iidummy
      integer nodisp,NPMAX,NPTPND,NEWPTS,INTRPL,NCXIT,IDIREC,MAXIT
      integer NPNTCL,ICONFM,NPNTVL,IVELFM,NPNTDL,IDSPFM,NPNTPL,nzones
      integer iouts,iconly,IZONE
      real decay,diffus,CELDIS,FZERO,FDTMTH,EPSSLV,zoncon,dummy
      real, allocatable :: rdummy(:)
      integer, allocatable :: idummy(:)
C     ------------------------------------------------------------------
      ierror=0
      NSCR=NSCOL*NSROW
      NODESS=NSCOL*NSROW*NSLAY
      NCINFL=0
      IF(ISROW1.GT.1.OR.ISROW2.LT.NROW.OR.
     *   ISCOL1.GT.1.OR.ISCOL2.LT.NCOL) NCINFL=NSLAY
      IF (ISLAY1.GT.1) THEN  
           NCINFL=NCINFL+1
      ENDIF
      IF (ISLAY2.LT.NLAY) THEN 
           NCINFL=NCINFL+1
      ENDIF
c
      ICONLY=0
      READ(INMOC,*,err=90) NODISP,DECAY,DIFFUS
      IF(MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2) THEN
        READ(INMOC,*,err=90) NPMAX,NPTPND
        NEWPTS=ABS(NPTPND)+1
  430   IF (NPTPND.LT.0) THEN
         DO 79 IP=1,NEWPTS-1
         READ(INMOC,*) dummy,dummy,dummy
   79    CONTINUE
        END IF
      ELSEIF (MOCTYPE.EQ.3) THEN
       READ(INMOC,*) iidummy,iidummy,iidummy,iidummy
      END IF
      IF(MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2) THEN
        READ(INMOC,*) CELDIS,FZERO,INTRPL
      ELSEIF(MOCTYPE.EQ.3) THEN
        READ(INMOC,*) CELDIS
        INTRPL=1
      ENDIF
      IF (MOCTYPE.EQ.2) THEN
	  newpts = 1
        READ(INMOC,*) FDTMTH,NCXIT,IDIREC,EPSSLV,MAXIT
      END IF
      READ(INMOC,*) NPNTCL,ICONFM,NPNTVL,IVELFM,NPNTDL,IDSPFM,
     1              NPNTPL
      READ(INMOC,*) CNOFLO
      nrdummy = nodess
      if (NCINFL.gt.nrdummy) nrdummy=ncinfl
      if (newpts.gt.nrdummy) nrdummy=newpts
      allocate(rdummy(nrdummy), idummy(nodess))
C4------READ STARTING CONCENTRATIONS.
      DO 300 KS=1,NSLAY
C  KK IS NUMBER OF FLOW LAYER
      KK=KS+ISLAY1-1
      LOC=1+(KS-1)*NSCR
      CALL U2DREL(rdummy,ANAME,NSROW,NSCOL,KK,INMOC,IOUTS)
  300 CONTINUE
      IF(NCINFL.GT.0) THEN
         CALL U1DREL(rdummy,ANAME,NCINFL,INMOC,IOUTS)
      ENDIF
C4------READ FIXED HEAD BOUNDARY CONCENTRATIONS                
      IF(ICONLY.NE.1) THEN
         READ(INMOC,*) NZONES
C
         DO 305 IZ=1,NZONES
          READ(INMOC,*) IZONE, ZONCON
305      continue
      END IF
C  READ FLAG TO TREAT SINK/SOURCE AS STRONG FOR PARTICLE REGENERATION AND 
C      REMOVAL
      IF(MOCTYPE.EQ.1.OR.MOCTYPE.EQ.2) THEN
      DO 306 KS=1,NSLAY
C  KK IS NUMBER OF FLOW LAYER
      KK=KS+ISLAY1-1
      LOC=1+(KS-1)*NSCR
      CALL U2DINT(idummy,ANAME,NSROW,NSCOL,KK,
     *  INMOC,IOUTS,ierror)
	if (ierror.NE.0) return
  306 CONTINUE
      ENDIF
C
C  READ DISPERSIVITIES BY LAYER
      IF(NODISP.NE.1) THEN
         CALL U1DREL(rdummy,ANAME,NSLAY,INMOC,IOUTS)
         CALL U1DREL(rdummy,ANAME,NSLAY,INMOC,IOUTS)
         CALL U1DREL(rdummy,ANAME,NSLAY,INMOC,IOUTS)
      END IF
C
C  READ RETARDATION FACTOR BY LAYER
      CALL U1DREL(rdummy,ANAME,NSLAY,INMOC,IOUTS)
C
C  READ PROPERTIES, ONE LAYER AT A TIME
C
      DO 309 KS=1,NSLAY
C  KK IS NUMBER OF FLOW LAYER
      KK=KS+ISLAY1-1
      LOC=1+(KS-1)*NSCR
C
C4------READ CELL THICKNESSES
      CALL U2DREL(THCK(LOC),ANAME,NSROW,NSCOL,KK,INMOC,IOUTS)
C
C4------READ CELL POROSITIES
      CALL U2DREL(rdummy,ANAME,NSROW,NSCOL,KK,INMOC,IOUTS)
  309 CONTINUE
      deallocate(rdummy,idummy)
      close (inmoc)
 1000 RETURN
c     error while reading data files
  90  ierror=7
      deallocate(rdummy,idummy)
      close (inmoc)
      return
      END

      SUBROUTINE SMOC6O(INUNIT,INMOC,IOUTS,JUNIT,DUNIT,MOCTYPE,
     1      maxunit,ierror)
C
C-----Modfied from MOC3D
C     ------------------------------------------------------------------
      implicit none
      INTEGER JUNIT(40)
      CHARACTER*4 DUNIT(40)
      CHARACTER*80 LINE
      CHARACTER*11 FMTARG
      INTEGER INUNIT,INMOC,IOUTS,MOCTYPE,maxunit,ierror
      INTEGER I,LLOC,ITYP1,ITYP2,N,ISTART,ISTOP,IU,INAM1,INAM2,IRECL
      REAL R
      logical unstructbin
      COMMON /bintype/unstructbin
C     ---------------------------------------------------------------
C
C1------INITIALIZE CONSTANTS
      ierror=0
      INMOC=0
      MOCTYPE=0
      DO 5 I=1,40
      JUNIT(I)=0
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
c-------Ignore the MOC3D listing file. We don't want to open it.
      if(line(ityp1:ityp2).eq.'CLST') then
         goto 10
C
C4B-----CHECK FOR "MOC" FILE TYPE.
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'MOC'.AND.MOCTYPE.EQ.0) THEN
         INMOC=IU
         MOCTYPE=1
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'MOCIMP'.AND.MOCTYPE.EQ.0) THEN
         INMOC=IU
         MOCTYPE=2
      ELSEIF(LINE(ITYP1:ITYP2).EQ.'ELLAM'.AND.MOCTYPE.EQ.0) THEN
         INMOC=IU
         MOCTYPE=3
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
1000  if (inmoc.eq.0) goto 999
      return
c     error while opening data file
998   ierror=6
      return
c     error while reading moc3d package file
999   ierror=5
      return
      END

