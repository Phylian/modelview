      subroutine rewindparticlefile(ierror)
      use mf2kmodule
      implicit none
      integer inunit
      integer,intent(out)::ierror
      inunit = 0
      if (JUNIT(4).gt.0) inunit = JUNIT(4)
      if (JUNIT(5).gt.0) inunit = JUNIT(5)
        call commonrewindparticlefile(inunit,ierror)
      return
      end subroutine rewindparticlefile
c
c
c
      subroutine readparticlecount(NP,ierror,istep)
      use mf2kmodule
      implicit none
      integer inunit
      logical binary
      integer,intent(out)::ierror,NP
      integer,intent(in)::istep
      logical isopen
      integer KPER,KSTP,IMOV
      real SUMTCH 
      inunit = 0
      if (JUNIT(4).gt.0) then
        inunit = JUNIT(4)
        binary = .false.
      endif 
      if (JUNIT(5).gt.0) then
        inunit = JUNIT(5)
        binary = .true.
      endif 
      call commonreadparticlecount(inunit,binary,NP,ierror,istep)
      return
      end subroutine readparticlecount
c
c
c
      subroutine readparticles(NP,ierror,coord,scalars, 
     1  delr, delc, elev)
      use mf2kmodule
      implicit none
      integer,intent(inout)::NP
      integer,intent(out)::ierror
      real,intent(inout):: coord, scalars
      real, intent(in):: delr, delc, elev
      dimension delr(NCOL), delc(NROW), elev(*)
      integer inunit
      logical binary
      dimension coord(NP*3), scalars(NP)
      integer IP
      COMMON /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      integer ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      logical isopen
      inunit = 0
      if (JUNIT(4).gt.0) then
        inunit = JUNIT(4)
        binary = .false.
      endif 
      if (JUNIT(5).gt.0) then
        inunit = JUNIT(5)
        binary = .true.
      endif 
      call commonreadparticles(inunit,binary,NP,ierror,coord,
     1  scalars, NCOL,NROW,NLAY, ISCOL1,ISCOL2,ISROW1,ISROW2,
     2  ISLAY1,ISLAY2, delr, delc, elev)
      return
      end subroutine readparticles
c
c
c
      subroutine dims(ierror,nc,nr,nl,igwt,unstruct,ITimeUnit,namefile)
      use mfcommonmodule
      use mf2kmodule
      implicit none
      integer,intent(in)::igwt,unstruct
      integer,intent(out)::ierror, nc, nr, nl
      character*256,intent(in)::namefile
c     Named common block shared with Modflow 2k and gwt
      logical unstructbin
      common /bintype/unstructbin
c     locally defined variables
      logical isopen, SHOWPROG
      integer inunit, NOTICECOUNT
	integer ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      COMMON /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      CHARACTER*200 LINE
      character*40 VERSION
      character*4 CUNIT(NIUNIT)
      data CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', '    ', 'GHB ',
     &           'RCH ', 'SIP ', 'DE4 ', 'SOR ', 'OC  ', 'PCG ', '    ',
     &           'GWT ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',
     &           'LAK ', 'LPF ', 'DIS ', 'SEN ', 'PES ', 'OBS ', 'HOB ',
     &           'ADV2', 'COB ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',
     &           'STOB', 'HUF2', 'CHOB', 'ETS ', 'DRT ', 'DTOB', '    ',
     &           'HYD ', 'SFR ', '    ', 'GAGE', 'LVDA', '    ', '    ',
     &           'MNW1', 'DAF ', 'DAFG', 'KDEP', 'SUB ', 'PVAL', 
     &           44*'    ',
     & 'HUF '/
      integer ISUMGX,ISUMGZ,ISUMIG
      integer LCBOTM,LCDELR,LCDELC,LCHNEW,LCIBOU,LCCR,
     1  LCCC,LCCV,LCRHS,LCHCOF,LCHOLD,LCBUFF,LCSTRT,LCRMLT,
     2  LCIZON
      integer ISEN,IOBS,IPES,ISENALL,ITMXP,IPAR
      character*4 DUNIT(NIUNIT)
      data DUNIT/'CRCH', 'CNCA', 'CNCB', 'PRTA', 'PRTB', 'VELA', 'VELB',
     1           'OBS ', 'AGE ', 'DP  ', 'DK  ', 'CHFB', 'IPDL', 'IPDA',
     2           'BFLX', '    ', '    ', '    ', '    ', '    ', '    ',
     3           '    ', '    ', '    ', '    ', '    ', '    ', '    ',
     4           '    ', '    ', '    ', '    ', '    ', '    ', '    ',
     5           '    ', '    ', '    ', '    ', '    ', '    ', '    ',
     6           '    ', '    ', '    ', '    ', '    ', '    ', '    ',
     &           '    ', 50*'    '/
      integer IOUTS,NODESS,NPMAX,NLIMBO,NUMOBS,LSOBSW,ICSTRT,ICONLY,
     *    NODISP,NCINFL,IABOVE,IBELOW,IDIM,IDKTIM,IDKRF,IDKZO,
     *    IDKFO,IDKZS,IDKFS,IDPZO,IDPFO,IDPTIM,IDPPS
      real DIFFUS,AGER8
      double precision DECAY
	integer ITimeUnit
      INCLUDE 'param.inc'
c     --------------------------------------------
      ISLAY1 = 1
      ISLAY2 = 0
      IXSEC = 0 
      NSOL=1
      if (igwt.eq.0) then
        gwt=.FALSE.
      else
        gwt=.TRUE.
      end if
      if (unstruct.eq.0) then
        unstructbin=.FALSE.
      else
        unstructbin=.TRUE.
      end if
      ierror=0
c     open the name file
      inunit=99
      open(unit=inunit,file=namefile,form='formatted',
     1    status='old',action='read',err=90)
90    inquire (unit=inunit,opened=isopen)
      if (.not.isopen) then
        ierror=2
        return
      end if
c     read modflow basic simulation data
      mxunit=99
      CALL GLO1BAS6DF(INUNIT,IUNIT,CUNIT,IREWND,NIUNIT,IOUTG,IOUT,
     &                VERSION,NCOL,NROW,NLAY,NPER,ITMUNI,ISUMGX,
     &                MXPER,ISUMIG,ISUMGZ,INBAS,LENUNI,ISUMX,ISUMZ,
     &                ISUMIX,LAYHDT,24,IFREFM,INAMLOC,IPRTIM,IBDT,
     &                SHOWPROG,NOTICECOUNT,mxunit,ierror)
      close(inunit,err=15)
	ITimeUnit = ITMUNI
15    continue
      if (ierror.ne.0) return
      CALL GLO1BAS6AL(IUNIT(24),NCNFBD,NBOTM,NCOL,NROW,NLAY,LCBOTM,
     &                LCDELR,LCDELC,ISUMGX,IOUTG,LCHNEW,LCIBOU,LCCR,
     &                LCCC,LCCV,LCRHS,LCHCOF,LCHOLD,LCBUFF,LCSTRT,
     &                ISUMGZ,ISUMIG,ISEN,IOBS,IPES,ISENALL,ITMXP,IPAR,
     &                IUNIT(31),IUNIT(32),NMLTAR,NZONAR,NML,NZN,LCRMLT,
     &                LCIZON,IUNIT(15),ierror)
      if (ierror.ne.0) return
      if (gwt) then
        if(iunit(15).gt.0) then
           call gwt1bas6df(ncol,nrow,nlay,ioutg,iouts,iunit(15),
     *      inmoc,junit,dunit,nscol,nsrow,nslay,nodess,npmax,
     *                nlimbo,
     *    newpts,numobs,lsobsw,icstrt,iconly,nodisp,decay,
     *    diffus,ncinfl,iabove,ibelow,idim,nptpnd,moctype,
     *    idktim,idkrf,idkzo,idkfo,idkzs,idkfs,ager8,idpzo,
     *    idpfo,idptim,idpps,mxunit,ierror)
        else
           ierror=11
        end if
        if (ierror.ne.0) return
c       islay1,islay2,isrow1,isrow2,iscol1,iscol2 are read in above subroutine
c       and is available in the common block subgrd
        nc=NSCOL
        nr=NSROW
        nl=NSLAY
CGWT----READ NUMBER OF GWT OBSERVATION LOCATIONS
C  THIS IS NOT A PART OF THE PARAMETER ESTIMATION OBSERVATIONS
        IF(IUNIT(15).GT.0.AND.JUNIT(8).GT.0) then
          CALL GWT1OBS5DF(JUNIT(8),ierror)
          if (ierror.ne.0) return
          CALL GWT1OBS5RP(JUNIT(8),ierror)
          if (ierror.ne.0) return
	  else
	    NUMGWTOBS = 0
	  endif
      else
        nc=NCOL
        nr=NROW
        nl=NLAY+NCNFBD
	  NUMGWTOBS = 0
c       Note that nlay is the number of model layers in modflow
c       nl is the number of layers for model viewer. 
      end if
      return
      end subroutine dims

      subroutine grid(ierror,delr,delc,elev,ibnd,conductivity,
     1      vnull1,vnull2,sunit,vunit,xoffset,yoffset,isMfLayer)
      use mf2kmodule
	use mf2kSubModule
      use mfcommonmodule
	use SUBARRAYS
	use SavedUnits
      implicit none
      integer,intent(out)::ierror,ibnd(*),sunit(*),vunit,isMfLayer(*)
      real, intent(out) :: delr(*),delc(*),elev(*),conductivity(*),
     1  vnull1,vnull2,xoffset,yoffset
c     Named common block shared with Modflow 2000
      integer LAYCON
      common /BCFCOM/LAYCON(999)
C     Named common block shared with mf2kgwt
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      common /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     Named common block for subroutines to read data
      common /mfcommon/ IBSDIM,IBSNULL,hhnoflo,hhdry,SUBNULL
c     Locally defined variables
      logical isopen, RESETDD, RESETDDNEXT
      character*20 CHEDFM,CDDNFM,CBOUFM
      character*80 HEADNG(2)
      CHARACTER*200 LINE,OUTNAM, FN
      integer NCR,OFFSET,i,j,k,INDEX,KK,nscr,is,js,ks,lengnam,kb
      INTEGER LLOC,ISTART,ISTOP,IDUM
      integer ISUMRX,IBCFCB,IWDFLG,IWETIT,IHDWET,ISUMIR
      integer ICHFLG,LCIOFL,ISTRT,IAPART,IHEDFM,IDDNFM,IHEDUN,
     1  IDDNUN,IPEROC,ITSOC,IBDOPT,LBHDSV,LBDDSV
      integer LCSC1,LCHY,LCSC2,LCTRPY,LCWETD,LCCVWD
      integer LCHK,LCVKA,LCHANI,LCVKCB,ILPFCB,NPLPF,LCLAYF,LCSV,ISEN
      integer IBOUUN,LBBOSV,ITS,IAUXSV
      integer IHUFCB,LCHGUF,NHUF,NPHUF,LCHUFTHK,LCHKCC,IOHUF,LCHUFTMP
      integer IOHUFHDS,IOHUFFLWS,LCVDHD,LCDVDH,LCVDHT,NPLVDA,LCA9
      integer LCGS,NPKDEP,IFKDEP
      integer LCHC,LCSCE,LCSCV,LCSUB,IIBSCB,IIBSOC,IBSDIM
      integer ISUBUN,ICOMUN,IHCUN,ISUBFM,ICOMFM,IHCFM,ISENSU
      real TOTIM,WETFCT,HDRY,HNOFLO,CNOFLO,VBVL(4,NIUNIT)
      real thickness,DUM,IBSNULL,hhnoflo,hhdry,SUBNULL
	integer MaxIsLay
      integer,allocatable :: IOFLG(:,:),ISSFLG(:),IZON(:,:,:),NSTP(:),
     1  LAYFLG(:,:),IHGUFLG(:,:),ISENS(:)
      real,allocatable :: BOTM(:,:,:),DC(:),DR(:),HNEW(:),DUMMY(:),
     1  PERLEN(:),RMLT(:,:,:),TRPY(:),TSMULT(:),WETDRY(:,:,:),
     2  HK(:,:,:),HANI(:,:,:),HUFTHK(:,:,:,:),HUFTMP(:,:,:),
     3  CC(:,:,:),VKA(:,:,:),VKCB(:,:,:),
     4  VDHD(:,:,:),VDHT(:,:),GS(:,:),A9(:,:)
      INCLUDE 'param.inc'
c     --------------------------------------------
      interface
        integer function NONB_LEN(CHARVAR,LENGTH)
          implicit none
          CHARACTER*(*) CHARVAR
          INTEGER LENGTH
        end function NONB_LEN
      end interface
c     --------------------------------------------
      ierror=0
      ITERPK=0
      ibsdim=0
      IBSNULL=0
	SUBNULL=1e30
	ISENSU = 0
      allocate(IBOUND(NCOL, NROW, NLAY))
      allocate(IOFLG(NLAY,5))
      allocate(ISSFLG(NPER))
      allocate(IZON(NCOL,NROW,NZONAR))
      allocate(NSTP(NPER))
      allocate(BOTM(NCOL,NROW,0:NBOTM))
      allocate(DC(NROW))
      allocate(DR(NCOL))
      allocate(HNEW(2*NCOL*NROW*NLAY))
      allocate(DUMMY(NCOL*NROW*NLAY))
      allocate(STRT(NCOL,NROW,NLAY))
      allocate(PERLEN(NPER))
      allocate(RMLT(NCOL,NROW,NMLTAR))
      allocate(TRPY(NLAY))
      allocate(TSMULT(NPER))
      allocate(LAYFLG(6,NLAY))
      allocate(WETDRY(NCOL,NROW,NLAY))
      allocate(HK(NCOL,NROW,NLAY))
      allocate(HANI(NCOL,NROW,NLAY))
      allocate(CC(NCOL,NROW,NLAY))
      allocate(VKA(NCOL,NROW,NLAY))
      allocate(VKCB(NCOL,NROW,NLAY))
	MaxIsLay = NLAY + NCNFBD - (ISLAY1 -1)
	if (ISLAY2.ne.0) then
	  MaxIsLay = MaxIsLay - (nlay-ISLAY2)
	endif
      j = MaxIsLay
	if (ISLAY2.eq.0) then 
	  MaxIsLay = nlay 
	else
	  MaxIsLay = ISLAY2 
	endif
      do i=ISLAY1,MaxIsLay
        isMfLayer(j) = 1
        if (LAYCBD(i).NE.0) then
          j = j-1
          isMfLayer(j) = 0
        end if
        j = j-1
      end do
c  GLOBAL READ AND PREPARE (RP) PROCEDURE
      CALL GLO1BAS6RP(IUNIT(24),NCOL,NROW,NLAY,BOTM,NBOTM,IOUTG,
     1                DR,DC,NPER,PERLEN,NSTP,TSMULT,
     2                ISSFLG,ITRSS,IUNIT(31),IUNIT(32),NMLTAR,NZONAR,
     3                RMLT,IZON,NML,NZN,55,IUNIT,NIUNIT,ierror)
      if (ierror.ne.0) goto 90

      if(IUNIT(23).gt.0)
     1    CALL GWF1LPF1ALG(ISUMX,LCHK,LCVKA,LCSC1,LCSC2,LCHANI,LCVKCB,
     2                     IUNIT(23),NCOL,NROW,NLAY,IOUTG,ILPFCB,LCWETD,
     3                     HDRY,NPLPF,NCNFBD,LCLAYF,IREWND(23),ISUMIX,
     4                     LAYHDT,ITRSS,LCSV,ISEN,ierror)
c     The HUF2 code will be used for reading HUF1 data
      if ((IUNIT(100).gt.0).and.(IUNIT(37).eq.0)) then
         IUNIT(37)=IUNIT(100)
         IUNIT(100)=0
      end if
c     HUF2 package
      IF(IUNIT(37).GT.0) THEN
        CALL GWF1HUF2ALG(ISUMX,LCHK,LCVKA,LCSC1,IUNIT(37),ITRSS,NCOL,
     &                   NROW,NLAY,IOUTG,IHUFCB,LCWETD,HDRY,NPER,
     &                   ISSFLG,LCHGUF,IREWND(37),
     &                   NHUF,NPHUF,LCHUFTHK,LCHKCC,ISUMIX,IOHUFHDS,
     &                   IOHUFFLWS,LAYHDT,LCHUFTMP,ierror)
        if (ierror.eq.0)
     &    CALL GWF1HUF2LVDA1ALG(ISUMX,ISUMIX,IUNIT(47),IOUTG,NCOL,
     &                        NROW,NLAY,LCVDHD,LCDVDH,LCVDHT,NPLVDA,
     &                        LCA9,ierror)
        if (ierror.eq.0)
     &    CALL GWF1HUF2KDEP1ALG(ISUMX,IUNIT(53),IOUTG,NCOL,NROW,
     &                        LCGS,NPKDEP,IFKDEP,ierror)
        if (ierror.eq.0) then
         allocate(IHGUFLG(5,NHUF))
         allocate(HUFTHK(NCOL,NROW,NHUF,2))
         allocate(HUFTMP(NCOL,NROW,NHUF))
         allocate(VDHD(NCOL,NROW,NLAY))
         allocate(VDHT(NCOL*NROW*NLAY,3))
         allocate(GS(NCOL,NROW))
         allocate(A9(NCOL*NROW*NLAY,5))
        end if
      ENDIF
      if (ierror.ne.0) goto 90
C     read first 2 lines of sen file if invoked
      if (iunit(25).gt.0) then
         call initsen(iunit(25),nplist,ISENSU,ierror)
         if (ierror.ne.0) goto 90
      end if
c     get the OUTNAM if estimation and observation processes are invoked
      if (iunit(26).gt.0.and.iunit(27).gt.0) then
        CALL URDCOM(iunit(27),-1,LINE)
        LLOC = 1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,DUM,0,0)
        OUTNAM = LINE(ISTART:ISTOP)
      else
        OUTNAM='NONE'
      end if
c     
      if (iunit(25).gt.0.and.nplist.gt.0) then
         call readsen(iunit(25),nplist,outnam,ierror)
         if (ierror.ne.0) goto 90
      end if

c-----READ AND PREPARE FOR PACKAGES 
      WETDRY=0
      if(IUNIT(23).gt.0) then
          ALLOCATE (ISENS(NPLIST))
          CALL GWF1LPF1RPGD(HK,VKA,VKCB,HANI,
     2                      DUMMY,DUMMY,IUNIT(23),ITRSS,NCOL,NROW,
     3                      NLAY,IOUTG,WETDRY,NPLPF,WETFCT,IWETIT,
     4                      IHDWET,LAYFLG,BOTM,NBOTM,
     5                      DR,DC,1,INAMLOC,
     6                      ISENS,ISEN,NPLIST,ierror)
          DEALLOCATE(ISENS)
          if (ierror.ne.0) goto 90
      end if
      IF(IUNIT(37).GT.0) then
          CALL GWF1HUF2RPGD(IUNIT(37),NCOL,NROW,NLAY,IOUTG,WETDRY,
     &                    WETFCT,IWETIT,IHDWET,IHGUFLG,
     &                    1,NHUF,NPHUF,HUFTHK,
     &                    ITRSS,ierror)
          if (ierror.ne.0) goto 90
      end if
      IF(IUNIT(47).GT.0) then
          CALL GWF1HUF2LVDA1RPGD(IUNIT(47),IOUTG,1,NHUF,NPLVDA,NLAY,
     &                           ISEN,ierror)
          if (ierror.ne.0) goto 90
      end if
      IF(IUNIT(53).GT.0) then
          CALL GWF1HUF2KDEP1RPGD(IUNIT(53),IOUTG,1,NPKDEP,IFKDEP,NROW,
     &                           NCOL,GS,BOTM,NHUF,ierror)
          if (ierror.ne.0) goto 90
      end if
C-------BEGIN ITERATION LOOP FOR PARAMETER ESTIMATION
	ITERPK=1
C4------ALLOCATE SPACE IN RX AND IR ARRAYS.
      CALL GWF1BAS6ALP(HEADNG,NPER,TOTIM,NCOL,NROW,NLAY,NODES,INBAS,
     1                   IOUT,IXSEC,ICHFLG,IFREFM,ISUMRX,ISUMIR,LCIOFL,
     2                   ISTRT,IAPART,ierror)
      if (ierror.ne.0) goto 90
      if(IUNIT(1).gt.0)
     1      CALL GWF1BCF6ALP(ISUMRX,LCSC1,LCHY,LCSC2,LCTRPY,ITRSS,ISS,
     2                       IUNIT(1),NCOL,NROW,NLAY,IOUT,IBCFCB,LCWETD,
     3                       IWDFLG,LCCVWD,WETFCT,IWETIT,IHDWET,HDRY,
     4                       IAPART,IFREFM,LAYHDT,ierror)
      if (ierror.ne.0) goto 90
      IF(IUNIT(19).GT.0)
     1    CALL GWF1IBS6AL(ISUMRX,LCHC,LCSCE,LCSCV,LCSUB,NCOL,
     2       NROW,NLAY,IIBSCB,IIBSOC,IUNIT(19),IOUT,IBSDIM,ierror)
      if (ierror.ne.0) goto 90
        IF(IUNIT(54).GT.0)
     1      CALL GWF1SUB1ALP(NROW,NCOL,NLAY,1,ISUBCB,ISUBOC,AC1,AC2,
     2                  ITMIN,NNDB,NDB,NPZ,NN,NND1,ND1,ND2,IDSAVE,
     3                  IDREST,ISSFLG,NPER,NSTP,NSTPT,IUNIT(54),IOUT,
     4                  IUNIT(9),LCV,ISEN,ierror)
      if (ierror.ne.0) goto 90
C6------READ AND PREPARE INFORMATION FOR ENTIRE SIMULATION.
      CALL GWF1BAS6RPP(IBOUND,HNEW,STRT,INBAS,HEADNG,
     1                   NCOL,NROW,NLAY,VBVL,IOFLG,IUNIT(12),
     2                   IHEDFM,IDDNFM,IHEDUN,IDDNUN,IOUT,IPEROC,ITSOC,
     3                   CHEDFM,CDDNFM,IBDOPT,IXSEC,LBHDSV,LBDDSV,
     4                   IFREFM,IBOUUN,LBBOSV,CBOUFM,HNOFLO,NIUNIT,ITS,
     5                   IAUXSV,RESETDD,RESETDDNEXT,ierror)
      if (ierror.ne.0) goto 90
	HeadUnit = IHEDUN
	DrawdownUnit = IDDNUN


      if(IUNIT(1).gt.0)
c         VKCB --> CVWD, VKA --> CV
     1      CALL GWF1BCF6RPP(IBOUND,HNEW,DUMMY,HK,
     2                       DUMMY,CC,VKA,DR,
     3                       DC,DUMMY,TRPY,IUNIT(1),
     4                       ISS,NCOL,NROW,NLAY,IOUT,WETDRY,IWDFLG,
     5                       VKCB,ierror)
      if(IUNIT(23).gt.0)
     1      CALL GWF1LPF1SP(IBOUND,HNEW,DUMMY,DUMMY,
     2                      DUMMY,DR,DC,BOTM,
     3                      HK,VKA,VKCB,HANI,
     4                      DUMMY,DUMMY,ITRSS,NCOL,NROW,NLAY,IOUT,
     5                      WETDRY,NPLPF,NBOTM,RMLT,IZON,
     6                      NMLTAR,NZONAR,LAYFLG,DUMMY,ITERPK,ierror)
      IF(IUNIT(37).GT.0) then
           CALL GWF1HUF2SP(IBOUND,HNEW,DUMMY,DUMMY,
     2                      DUMMY,DR,DC,BOTM,
     3                      HK,VKA,DUMMY,ITRSS,NCOL,NROW,
     4                      NLAY,IOUT,WETDRY,NHUF,NBOTM,RMLT,
     5                      IZON,NMLTAR,NZONAR,HUFTHK,
     6                      DUMMY,HDRY,0,0,0,IHGUFLG,
     7                      HUFTMP,IUNIT(47),
     8                      VDHD,VDHT,IWETIT,
     9                      IHDWET,WETFCT,GS,A9,ierror)
          deallocate(IHGUFLG)
          deallocate(HUFTHK)
          deallocate(HUFTMP)
          deallocate(VDHD)
          deallocate(VDHT)
          deallocate(GS)
          deallocate(A9)
      end if
      if (ierror.ne.0) goto 90 
      IF(IUNIT(19).GT.0) then
         CALL GWF1IBS6RP(DR,DC,HNEW,DUMMY,
     2          DUMMY,DUMMY,DUMMY,NCOL,NROW,
     3          NLAY,NODES,IIBSOC,ISUBFM,ICOMFM,IHCFM,
     4          ISUBUN,ICOMUN,IHCUN,IUNIT(19),IOUT,IBSDIM,ierror)
         if (ierror.ne.0) goto 90 
         IBSNULL=hnoflo
      end if
      if(IUNIT(54).GT.0) then
            CALL GWF1SUB1RPP(DR,DC,HNEW,
     2                  DUMMY,NCOL,NROW,NLAY,NODES,NPER,NSTP,
     3                  ISUBOC,NND1,ND1,ND2,NDB,NNDB,NPZ,NN,IDSAVE,
     4                  IDREST,NSTPT,IUNIT(54),IOUT, ierror)
         if (ierror.ne.0) goto 90 
      end if
      if (gwt) then
        call customread(moctype,inmoc,nptpnd,newpts,CNOFLO,ierror)
c       construct subgrid delc to return (do not assume uniform grid)
        do i=1,nsrow
          delc(i)=dc(ISROW2-i+1)
        end do
c       construct subgrid delr to return
        do i=1,nscol
          delr(i)=DR(ISCOL1+i-1)
        end do
        SubColumnOffset = 0
        do i=1,ISCOL1-1
          SubColumnOffset=SubColumnOffset + dr(i)
        end do
        SubRowOffset = 0
        do i=NROW,ISROW2+1,-1
          SubRowOffset=SubRowOffset + dc(i)
        end do

        nscr=nscol*nsrow
        do ks=1,nslay
          k=ks+islay1-1
          offset=(nslay-ks)*nscr
          do is=1,nsrow
            i=is+isrow1-1
            do js=1,nscol
              j=js+iscol1-1
              index=offset+(nsrow-is)*nscol+js
              if (wetdry(j,i,k).ne.0.and.ibound(j,i,k).eq.0) 
     1               ibound(j,i,k) = 1
              ibnd(index)=ibound(j,i,k)
              elev(index+nscr)=botm(j,i,k-1)
            end do
          end do
        end do 
        do is=1,nsrow
          i=is+isrow1-1
          do js=1,nscol
            j=js+iscol1-1
            elev((nsrow-is)*nscol+js)=botm(j,i,islay2)
          end do
        end do
        sunit(1)=0
        vunit=0
        if (junit(3).gt.0) then
          inquire(unit=junit(3),opened=isopen)
          if (isopen) then
            sunit(1)=junit(3)
          end if
        end if
        if (sunit(1).eq.0.and.junit(2).gt.0) then
          inquire(unit=junit(2), opened=isopen)
          if (isopen) then
            sunit(1)=-junit(2)
          end if
        end if
        inquire(unit=ihedun, opened=isopen)
        if (isopen.and.(.not.gwt)) then
          i=2
          if (CHEDFM.eq.' ') then
            sunit(i)=ihedun
          else
            sunit(i)=-ihedun
          end if
        end if
c       make sure the drawdown unit is not same as head unit
        if (ihedun.ne.iddnun) then
          inquire(unit=iddnun, opened=isopen)
          if (isopen.and.(.not.gwt)) then
            i=i+1
            if (CDDNFM.eq.' ') then
              sunit(i)=iddnun
            else
              sunit(i)=-iddnun
            end if
          end if
        end if
        if (junit(7).gt.0) then
          inquire(unit=junit(7),opened=isopen)
          if (isopen) then
            vunit=junit(7)
          end if
        end if
        if (vunit.eq.0.and.junit(6).gt.0) then
          inquire(unit=junit(6),opened=isopen)
          if (isopen) then
            vunit=-junit(6)
          end if
        end if
        xoffset=0
        if (iscol1.gt.1) then
          do i=1,iscol1-1
            xoffset=xoffset + DR(i)
          end do
        end if
        yoffset=0
        if (isrow2.lt.nrow) then
          do i=isrow2+1,nrow
            yoffset=yoffset + dc(i)
          end do
        end if
        vnull1=CNOFLO
        vnull2=CNOFLO
      else
c       Copy DC to delc in reverse direction
        do i=1,NROW
          delc(i)=DC(NROW-i+1)
        end do
c       Copy DR to delr
        do i=1,NCOL
          delr(i)=DR(i)
        end do
        NCR=NCOL*NROW
c       copy BOTM into elev in reordered format
        do k=0,NBOTM
          OFFSET=(NBOTM-k)*NCR
          do i=1,NROW
            do j=1,NCOL
              INDEX=OFFSET+(NROW-i)*NCOL+j
              elev(INDEX)=BOTM(j,i,k)
            end do
          end do
        end do 
c       copy ibound and conductivity in reordered and extended format
        kk=0
        kb=0
        do k=1,NLAY
          IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) KB=KB+1
c         work on the model layer
          kk=kk+1
          offset=(nbotm-kk)*ncr
          do i=1,NROW
            do j=1,NCOL
              index=offset+(NROW-i)*NCOL+j
              if (wetdry(j,i,k).ne.0.and.ibound(j,i,k).eq.0) 
     1               ibound(j,i,k) = 1
              ibnd(index)=IBOUND(j,i,k)
              if (IUNIT(1).gt.0) then
                if (LAYCON(k).eq.0.or.LAYCON(k).eq.2) then
                  thickness=botm(j,i,kk-1) - botm(j,i,kk);
                  if (thickness.gt.0) then
                    conductivity(index)=cc(j,i,k)/thickness
                  else
                    conductivity(index)=0
                  end if
                else
                  conductivity(index)=HK(j,i,kb)
                end if
              else
                conductivity(index)=HK(j,i,k)
              end if
            end do
          end do
c         work on the underlying confining layer if it exists
c         set the (extended) ibnd to the same as model layer
c         set the conductivity to zero.
c         MODIFIED in version 1.1
c         The cell in a confining layer is assumed active only
c         if both the overlying and underlying cells (in aquifer
c         layer) are active.
          if (LAYCBD(k).ne.0) then
            kk=kk+1
            offset=(nbotm-kk)*ncr
            do i=1,NROW
              do j=1,NCOL
                index=offset+(NROW-i)*NCOL+j
c               The following line in version 1.0 is commented out
c                ibnd(index)=IBOUND(j,i,k)
c               The following 5 lines are used in version 1.1
                if (IBOUND(j,i,k).ne.0.and.IBOUND(j,i,k+1).ne.0) then
                    ibnd(index) = 1
                else
                    ibnd(index) = 0
                end if
                conductivity(index)=0
              end do
            end do
          end if
        end do 
        i=0
        inquire(unit=ihedun, opened=isopen)
        if (isopen) then
          i=1
          if (CHEDFM.eq.' ') then
            sunit(1)=ihedun
          else
            sunit(1)=-ihedun
          end if
        end if
c       make sure the drawdown unit is not same as head unit
        if (ihedun.ne.iddnun) then
          inquire(unit=iddnun, opened=isopen)
          if (isopen) then
            i=i+1
            if (CDDNFM.eq.' ') then
              sunit(i)=iddnun
            else
              sunit(i)=-iddnun
            end if
          end if
        end if
        IF(IUNIT(19).GT.0) then
          inquire(unit=icomun, opened=isopen)
          if (isopen) then
            i=i+1
            sunit(i)=icomun
          end if
          inquire(unit=ihcun, opened=isopen)
          if (isopen) then
            i=i+1
            sunit(i)=ihcun
          end if
        end if
        if(IUNIT(54).GT.0) then
          inquire(unit=ISBOCU(2), opened=isopen)
          if (isopen.AND.(ISBOCU(2)>0)) then
            i=i+1
            sunit(i)=ISBOCU(2)
          end if
          inquire(unit=ISBOCU(4), opened=isopen)
          if (isopen.AND.(ISBOCU(4)>0)) then
            i=i+1
            sunit(i)=ISBOCU(4)
          end if
          inquire(unit=ISBOCU(5), opened=isopen)
          if (isopen.AND.(ISBOCU(5)>0)) then
            i=i+1
            sunit(i)=ISBOCU(5)
          end if
          inquire(unit=ISBOCU(6), opened=isopen)
          if (isopen.AND.(ISBOCU(6)>0)) then
            i=i+1
            sunit(i)=ISBOCU(6)
          end if
        end if
        if (ISENSU.gt.0) then
          inquire(unit=ISENSU, opened=isopen)
          if (isopen) then
            i=i+1
            if (CHEDFM.eq.' ') then
              sunit(i)=ISENSU
            else
              sunit(i)=-ISENSU
            end if
          end if
        end if
        vunit=0
        if (IUNIT(1).gt.0.and.IBCFCB.gt.0) then
          vunit=IBCFCB
        else if (IUNIT(23).gt.0.and.ILPFCB.gt.0) then
          vunit=ILPFCB
        else if(IUNIT(37).gt.0.and.IHUFCB.gt.0) then
          vunit=IHUFCB
        end if
        if (vunit.gt.0) then
          inquire(unit=vunit, opened=isopen)
          if (.not.isopen) vunit=0
        end if
        xoffset=0
        yoffset=0
        vnull1=HNOFLO
        vnull2=HDRY
        islay1=1
        islay2=NLAY
        isrow1=1
        isrow2=NROW
        iscol1=1
        iscol2=NCOL
      end if
 90   continue
      hhnoflo=hnoflo
      hhdry=hdry
      call GWF1SUB1DA
      deallocate(IOFLG)
      deallocate(ISSFLG)
      deallocate(IZON)
      deallocate(NSTP)
      deallocate(BOTM)
      deallocate(DC)
      deallocate(DR)
      deallocate(HNEW)
      deallocate(DUMMY)
      deallocate(RMLT)
      deallocate(PERLEN)
      deallocate(TRPY)
      deallocate(TSMULT)
      deallocate(LAYFLG)
      deallocate(WETDRY)
      deallocate(HK)
      deallocate(CC)
      deallocate(VKA)
      deallocate(VKCB)
      deallocate(HANI)
      if (ierror.ne.0) deallocate(IBOUND)
      if (ierror.ne.0) deallocate(STRT)
	if (ierror.ne.0) then
	      IF(ALLOCATED(LDN)) DEALLOCATE (LDN)
	endif
      return
      end subroutine grid

      subroutine countfeatures(ierror,ibsize,nfeat,ibnd)
      use mf2kmodule
      implicit none
	include 'param.inc'
      integer,intent(out)::ierror,ibsize,nfeat,ibnd(nbotm*ncol*nrow)
c     locally defined variables
      real,allocatable::dummy(:,:),BDTIM(:),SBHED(:,:),FLWRAT(:,:)
      integer,allocatable::IFLLOC(:,:),IHDLOC(:,:)
      integer i,j,k,ISUM,ISUMI,LENX
      integer IWELCB,IDRNCB,IRIVCB,IGHBCB,IRESCB,IFHBCB,
     1      IDRTCB
      integer LCWELL,LCDRAI,LCRIVR,LCBNDS,LCDRTF
      integer LCIRES,LCIRSL,LCBRES,LCCRES,LCBBRE,LCHRES,LCHRSE
      integer LCSTRM,LCTBAR,LCTRIB,LCIVAR,LCFGAR
      integer LCCHDS,LCFLLC,LCBDTM,LCFLRT,LCBDFV,LCBDHV,LCHDLC,LCSBHD
      integer ICSTRM,IRESPT
      INTEGER IERR, IDAFCB, IDAFBK
      integer NBDTIM,NFHBX1,NFHBX2,IFHBD3,IFHBD4,IFHBD5,IFHBSS
      integer numlakecell,maxlakecell,iend,nn
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      common /SUBGRD/
     1  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     --------------------------------------------
      interface
        integer function rcl2i(arow,acol,alayer)
          implicit none
          integer,intent(in)::arow,acol,alayer
        end function rcl2i
      end interface
c     --------------------------------------------
!     The purpose of this countfeatures is to determine the size of the array
!     required to store the locations of the "Model Features" that will be
!     displayed in model viewer.  That size is returned in ibsize.  The number
!     of types of features is returned in nfeat.  The final array will have
!     section devoted to each type of model feature.  The first entry in each
!     section will be the number of model features of that type that are contained
!     in the rest of the section.  The remainder will be node numbers for the 
!     model features.  The function rcl2i can be used to convert from row, column
!     and layer numbers to node numbers.
c     --------------------------------------------
      ierror=0
C-----Below is a temporary hack to get around all the additional checks 
c     coded into MF2K version 1.14 to check parameter definition variables.
c     These variables are re-initialized below at the start of the countfeature
c     subroutine.
c     TO DO: Make a thorough check on how the parameter definition variables are
c     implemented in MF2K so that model features are correctly shown in Model Viewer.
      SFR_HasBeenRead = .FALSE.
      IPSUM=0
      ICLSUM=0
      IDEFPAR=0
	INAMLOC=1
      do i=1,MXPAR
        PARNAM(i)=' '
        PARTYP(i)=' '
        IPLOC(1,i)=0
        IPLOC(2,i)=0
        IACTIVE(i)=0
      end do
c-----end of hack
      LENX=9999999
      allocate (DUMMY(NCOL,NROW))
      nfeat=0
      ibousz=0
c Fixed Heads (negative values in IBOUND array)
      nfeat=nfeat + 1
      ibousz=ibousz + 1
      nfixedhead=0
      do k=islay1,islay2
        do i=isrow1,isrow2
          do j=iscol1,iscol2
            if (IBOUND(j,i,k).lt.0) then
                nfixedhead=nfixedhead + 1
            end if
          enddo
        enddo
      enddo
      ibousz=ibousz + nfixedhead
c Well Package
      if(IUNIT(2).gt.0) then
        rewind(IUNIT(2))
        call GWF1WEL6AL(ISUM,LCWELL,MXWELL,NWELLS,IUNIT(2),IOUT,
     1       IWELCB,NWELVL,IWELAL,IFREFM,NPWEL,IPWBEG,NNPWEL,ierror)
        if (ierror.ne.0) goto 10
        if (IUNIT(2).eq.0) then
          ibousz=ibousz + 1
        else
          if (.not.allocated(well)) then
            allocate (well(NWELVL,MXWELL))
          end if
          call GWF1WEL6RQ(IUNIT(2),IOUTG,NWELVL,IWELAL,NCOL,NROW,NLAY,
     2       NPWEL,well,IPWBEG,MXWELL,IFREFM,1,INAMLOC)
          ibousz=ibousz + MXWELL + 1
        end if
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c Drain Package
      if(IUNIT(3).gt.0) then
        rewind(IUNIT(3))
        call GWF1DRN6AL(ISUM,LCDRAI,MXDRN,NDRAIN,IUNIT(3),IOUT,
     1       IDRNCB,NDRNVL,IDRNAL,IFREFM,NPDRN,IDRNPB,NDRNNP,ierror)
        if (ierror.ne.0) goto 10
        if (.not.allocated(drain)) then
          allocate (drain(NDRNVL,MXDRN))
        end if
        call GWF1DRN6RQ(IUNIT(3),IOUTG,NDRNVL,IDRNAL,NCOL,NROW,NLAY,
     2       NPDRN,drain,IDRNPB,MXDRN,IFREFM,1,INAMLOC)
        ibousz=ibousz + MXDRN + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c River Package
      if(IUNIT(4).gt.0) then
        rewind(IUNIT(4))
        call GWF1RIV6AL(ISUM,LCRIVR,MXRIVR,NRIVER,IUNIT(4),IOUT,
     1       IRIVCB,NRIVVL,IRIVAL,IFREFM,NPRIV,IRIVPB,NRIVNP,ierror)
        if (ierror.ne.0) goto 10
        if (.not.allocated(river)) then
          allocate (river(NRIVVL,MXRIVR))
        end if
        call GWF1RIV6RQ(IUNIT(4),IOUTG,NRIVVL,IRIVAL,NCOL,NROW,NLAY,
     2       NPRIV,river,IRIVPB,MXRIVR,IFREFM,1,INAMLOC)
        ibousz=ibousz + MXRIVR + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c Stream Package
      if(IUNIT(18).gt.0) then
        rewind(IUNIT(18))
        call GWF1STR6AL(ISUM,ISUMI,LCSTRM,ICSTRM,MXSTRM,                     
     1                 NSTREM,IUNIT(18),IOUT,ISTCB1,ISTCB2,NSS,NTRIB,   
     2                 NDIV,ICALC,CONST,LCTBAR,LCTRIB,LCIVAR,LCFGAR,
     3                 NPSTR,ISTRPB,ierror)   
        if (ierror.ne.0) goto 10
        if (.not.allocated(stream)) then
          allocate (stream(5,MXSTRM))
        end if
        if (.not.allocated(ISTRM)) then
          allocate (ISTRM(5,MXSTRM))
        end if
        if (.not.allocated(STRM)) then
          allocate(STRM(11,MXSTRM))
        end if
        if (.not.allocated(ITRBAR)) then
          allocate(ITRBAR(NSS,NTRIB))
        end if
        if (.not.allocated(IDIVAR)) then
          allocate(IDIVAR(NSS))
        end if
        call GWF1STR6RQ(IUNIT(18),IOUTG,NCOL,NROW,NLAY,NPSTR,
     2       STRM,ISTRM,ISTRPB,MXSTRM,1,INAMLOC,ierror)
        ibousz=ibousz + MXSTRM + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c Reservoir Package
      if(IUNIT(17).gt.0) then
        rewind(IUNIT(17))
        if (.not.allocated(IRES)) allocate (IRES(NCOL,NROW))
        if (.not.allocated(IRESL)) allocate (IRESL(NCOL,NROW))
        call RES1AL(ISUM,LENX,LCIRES,LCIRSL,LCBRES,
     1    LCCRES,LCBBRE,LCHRES,LCHRSE,IUNIT(17),IOUT,NRES,IRESCB,
     2    NRESOP,IRESPT,NPTS,NCOL,NROW,ierror)
        if (ierror.ne.0) goto 10
        call RES1RP1(IRES,IRESL,DUMMY,DUMMY,DUMMY,IBOUND,NRES,
     2    NRESOP,NPTS,NCOL,NROW,NLAY,IUNIT(17),IOUT,NCELL,ierror)
        if (ierror.ne.0) goto 10
        ibousz=ibousz + NCELL + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c GHB Package
      if(IUNIT(7).gt.0) then
        rewind(IUNIT(7))
        call GWF1GHB6AL(ISUM,LCBNDS,MXBND,NBOUND,IUNIT(7),IOUT,
     1       IGHBCB,NGHBVL,IGHBAL,IFREFM,NPGHB,IGHBPB,NGHBNP,ierror)
        if (ierror.ne.0) goto 10
        if (.not.allocated(ghb)) then
          allocate (ghb(NGHBVL,MXBND))
        end if
        call GWF1GHB6RQ(IUNIT(7),IOUTG,NGHBVL,IGHBAL,NCOL,NROW,NLAY,
     2       NPGHB,ghb,IGHBPB,MXBND,IFREFM,1,INAMLOC)
        ibousz=ibousz + MXBND + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c CHD Package
      if(IUNIT(20).gt.0) then
        rewind(IUNIT(20))
        call GWF1CHD6AL(ISUM,LCCHDS,NCHDS,MXCHD,IUNIT(20),IOUT,    
     1           NCHDVL,IFREFM,NPCHD,IPCBEG,NNPCHD,ierror)                                        
        if (ierror.ne.0) goto 10
        if (.not.allocated(chd)) then
          allocate (chd(NCHDVL,MXCHD))
        end if
        call GWF1CHD6RQ(IUNIT(20),IOUTG,NCHDVL,NCOL,NROW,NLAY,NPCHD,
     2                      chd,IPCBEG,MXCHD,IFREFM,1,INAMLOC)
        ibousz=ibousz + MXCHD + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c FHB Package
      if(IUNIT(16).gt.0) then
        rewind(IUNIT(16))
        call FHB1AL(ISUM,LENX,LCFLLC,LCBDTM,LCFLRT,
     1          LCBDFV,LCBDHV,LCHDLC,LCSBHD,NBDTIM,NFLW,NHED,IUNIT(16),
     2          IOUT,IFHBCB,NFHBX1,NFHBX2,IFHBD3,IFHBD4,IFHBD5,
     3          IFHBSS,ISS,ierror)
        if (ierror.ne.0) goto 10
        if (IUNIT(16).eq.0) then
          ibousz=ibousz + 2
        else
          ibousz=ibousz + NFLW + NHED + 2
          if (.not.allocated(fhbflow)) allocate(fhbflow(NFLW))
          if (.not.allocated(fhbhead)) allocate(fhbhead(NHED))
          allocate(IFLLOC(4,NFLW))
          allocate(IHDLOC(4,NHED))
          allocate(BDTIM(NBDTIM))
          allocate(SBHED(IFHBD5,NHED))
          allocate(FLWRAT(IFHBD3,NFLW))
          call FHB1RP(IBOUND,NROW,NCOL,NLAY,
     &          IFLLOC,BDTIM,NBDTIM,FLWRAT,NFLW,NHED,
     &          IHDLOC,SBHED,IUNIT(16),IOUT,
     &          NFHBX1,NFHBX2,IFHBD3,IFHBD5,ierror)
          if (ierror.ne.0) goto 5
          call storefhb(IFLLOC,IHDLOC,ierror)
5         continue
          deallocate(IFLLOC)
          deallocate(IHDLOC)
          deallocate(BDTIM)
          deallocate(SBHED)
          deallocate(FLWRAT)
          if (ierror.ne.0) goto 10
        end if
      else
        ibousz=ibousz + 2
      endif
      nfeat=nfeat + 2
c     Drain with return flow package
      if(IUNIT(40).gt.0) then
        rewind(IUNIT(40))
        CALL GWF1DRT1AL(ISUM,LCDRTF,MXDRT,NDRTCL,IUNIT(40),IOUT,
     &                      IDRTCB,NDRTVL,IDRTAL,IFREFM,NPDRT,IDRTPB,
     &                      NDRTNP,IDRTFL,ierror)
        if (ierror.ne.0) goto 10
        if (.not.allocated(drtf)) then
          allocate (drtf(NDRTVL,MXDRT))
        end if
        CALL GWF1DRT1RQ(IUNIT(40),IOUTG,NDRTVL,IDRTAL,NCOL,NROW,
     &       NLAY,NPDRT,drtf,IDRTPB,MXDRT,IFREFM,
     &       1,IDRTFL,INAMLOC)
        ibousz=ibousz + MXDRT + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c DAF package
      IF(IUNIT(51).GT.0) then
        rewind(IUNIT(51))
        rewind(IUNIT(52))
        rewind(IUNIT(52)+1)
        CALL GWF1DAF1ALP(IERR,IUNIT(52)+1,IUNIT(52),IUNIT(51),IOUT,
     2                      IDAFCB,IDAFBK, ierror)
        if (ierror.ne.0) goto 10
        ibousz=ibousz + idafcount + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c MNW package
      IF(IUNIT(50).GT.0) THEN
        rewind(IUNIT(50))
        CALL GWF1MNW1AL(ISUM,LCWEL2,MXWEL2,NWELL2,LCHREF,NODES,
     &                     KSPREF,IUNIT(50),IOUT,IWL2CB,IOWELL2,
     &                     NOMOITER,PLOSSMNW,MNWNAME,FNAME,ierror)
        if (ierror.ne.0) goto 10
        if (.not. allocated(well2)) then
          allocate(well2(17,mxwel2+1))
        end if
        ibousz=ibousz + MXWEL2 + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c SFR package
      IF(IUNIT(44).GT.0) THEN
        rewind(IUNIT(44))
          CALL GWF1SFR1ALP(NSTRM,
     &        IUNIT(44),IOUT,ISTCB1,ISTCB2,NSSSFR,CONST,MAXPTS,DLEAK,
     &        IUNIT(22),IUNIT(15),NSOLSFR,NSTRPAR,NSEGDIM,NSTRMAR,
     &        NSSAR, ierror)
        if (ierror.ne.0) goto 10
        ibousz=ibousz + NSTRM + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c Lake
      if(IUNIT(22).gt.0) then
        rewind(IUNIT(22))
        call INITLAKE(IUNIT(22),NLAKES,IFREFM,itrss,iss,iUNIT(15),nsol,
     1                    ierror)
        if (ierror.ne.0) goto 10
        if (nlakes.eq.0) then
            iunit(22)=0
            ibousz=ibousz + 1
        else
          if (.not.allocated(lkarr1)) 
     1        allocate(lkarr1(ncol,nrow,nlay))
          iend=0
          maxlakecell=0
20        call GETLAKE(iunit(22),NLAKES,IFREFM,iunit(15),nsol,
     1              lkarr1,ncol,nrow,nlay,iend,ierror)
          if (ierror.ne.0) goto 10
          if (iend.ne.0) goto 30
          numlakecell=0
          DO 130 K=1,NLAY
          DO 130 I=1,NCOL
          DO 130 J=1,NROW
            IF(LKARR1(I,J,K).GT.0.AND.LKARR1(I,J,K).LE.NLAKES) THEN
               numlakecell=numlakecell+1
               nn=rcl2i(j,i,k)+1
               ibnd(nn)=22
            end if
  130     CONTINUE
          if (numlakecell.gt.maxlakecell) maxlakecell=numlakecell
          goto 20
  30      ibousz=ibousz + maxlakecell + 1
          rewind(IUNIT(22))
          call INITLAKE(IUNIT(22),NLAKES,IFREFM,itrss,iss,iUNIT(15),
     1                  nsol,ierror)
        end if
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
!	GWT observation wells
!     If GWT is not used NUMGWTOBS is set to 0.
      ibousz=ibousz + NUMGWTOBS + 1
      nfeat=nfeat + 1

10    continue
      deallocate (DUMMY)
      ibsize=ibousz
      return
      end subroutine countfeatures

      subroutine getfeatures(ierror,ibnode)
      use mf2kmodule
      use simtime
      implicit none
      integer,intent(out)::ibnode(*),ierror
c     locally defined variables
      integer position,i,j,k,iptflg,p0,imnw,n, II, NN
      integer NRFLOW,iend
c     Named common block shared with GWT
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      common /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     --------------------------------------------
      interface
        integer function rcl2i(arow,acol,alayer)
          implicit none
          integer,intent(in)::arow,acol,alayer
        end function rcl2i
      end interface
c     --------------------------------------------
!     The purpose of this getfeatures is to  store the locations of the 
!     "Model Features" that will be displayed in model viewer. The features will
!     be stored in ibnode. The final array will have
!     section devoted to each type of model feature.  The first entry in each
!     section will be the number of model features of that type that are contained
!     in the rest of the section.  The remainder will be node numbers for the 
!     model features.  The function rcl2i can be used to convert from row, column
!     and layer numbers to node numbers.
c     --------------------------------------------
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOTS   - Maximum of time steps per ground water step
C
c     --------------------------------------------
      INCLUDE 'startdaf.com'
C     + + + + + + + + + + + COMMON DEFINTIONS (startdaf.com) + + + + + +
C     IDBG,NBRCH NXSEC(N) VIN(I,N)
      INTEGER INX
C
c     --------------------------------------------
      INCLUDE 'ground.com'
C     +  + + + + + + + + + + COMMON DEFINTIONS  (ground.com) + + + + + +
C     AQGW(I,N,J) BC(I,N,J) BEL(I,N) BTH(I,N) CND(I,N) NCL(I,N) NLY(I,N)
C     NRW(I,N) VGW(I,N,J)
c     --------------------------------------------
      do i=1, ibousz
        ibnode(i)=0
      enddo
      position=0    
c Fixed Heads
      if (nfixedhead.gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          position=position + 1
          ibnode(position)=nfixedhead
          do k=islay1,islay2
            do i=isrow1,isrow2
              do j=iscol1,iscol2
                if (Ibound(j,i,k).lt.0) then
                  position=position + 1
                  ibnode(position)=rcl2i(i,j,k)
                end if
              enddo
            enddo
          enddo
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        ibnode(position)=-1
      end if
c Well Package
      if(IUNIT(2).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          call GWF1WEL6RP(well,NWELLS,MXWELL,IUNIT(2),IOUT,NWELVL,
     1        IWELAL,IFREFM,NCOL,NROW,NLAY,NNPWEL,NPWEL,IPWBEG,ierror)
          if (ierror.ne.0) goto 100
          call setboundary(well,NWELVL,NWELLS,position,ibousz,
     1          ibnode,ierror)
100       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c Drain Package
      if(IUNIT(3).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          call GWF1DRN6RP(drain,NDRAIN,MXDRN,IUNIT(3),IOUT,NDRNVL,
     1        IDRNAL,IFREFM,NCOL,NROW,NLAY,NDRNNP,NPDRN,IDRNPB,ierror)
          if (ierror.ne.0) goto 200
          call setboundary(drain,NDRNVL,NDRAIN,position,ibousz,
     1          ibnode,ierror)
200       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c River Package
      if(IUNIT(4).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          call GWF1RIV6RP(river,NRIVER,MXRIVR,IUNIT(4),IOUT,NRIVVL,
     1        IRIVAL,IFREFM,NCOL,NROW,NLAY,NRIVNP,NPRIV,IRIVPB,ierror)
          if (ierror.ne.0) goto 300
          call setboundary(river,NRIVVL,NRIVER,position,ibousz,
     1        ibnode,ierror)
300       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c Stream Package
      if(IUNIT(18).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          call GWF1STR6RP(STRM,ISTRM,NSTREM,MXSTRM,IUNIT(18),IOUT,        
     1         ITRBAR,NDIV,NSS,NTRIB,IDIVAR,ICALC,IPTFLG,NCOL,NROW,
     2         NLAY,NPSTR,ISTRPB,ierror)                
          if (ierror.ne.0) goto 500
          do i=1,5
            do j=1, MXSTRM
              stream(i,j)=ISTRM(i,j)
            end do
          end do
c         NOTE THAT THE ARRAY stream MIGHT CONTAIN THE SAME CELL
c         MULTIPLE TIMES if THE CELL PARTICIPATES IN MORE THAN ONE BRANCH.
          call setboundary(stream,5,NSTREM,position,ibousz,
     1          ibnode,ierror)
500       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c Reservoir Package
      if(IUNIT(17).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          call setreservior(position,ibnode,ierror)
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c GHB Package
      if(IUNIT(7).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          call GWF1GHB6RP(ghb,NBOUND,MXBND,IUNIT(7),IOUT,NGHBVL,IGHBAL,
     1        IFREFM,NCOL,NROW,NLAY,NGHBNP,NPGHB,IGHBPB,ierror)
          if (ierror.ne.0) goto 400
          call setboundary(ghb,NGHBVL,NBOUND,position,ibousz,
     1        ibnode,ierror)
400       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c CHD Package
      if(IUNIT(20).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          call GWF1CHD6RP(chd,NCHDS,MXCHD,IBOUND,   
     1            NCOL,NROW,NLAY,IUNIT(20),IOUT,NCHDVL,IFREFM,NNPCHD,
     2            NPCHD,IPCBEG,ierror)
          if (ierror.ne.0) goto 600
          call setboundary(chd,NCHDVL,NCHDS,position,ibousz,ibnode,
     1          ierror)
600       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c FHB Package
      if(IUNIT(16).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          call setfhb(position,ibnode,ierror)
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c Drain with return flow package
      if(IUNIT(40).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then
          CALL GWF1DRT1RP(drtf,NDRTCL,MXDRT,IUNIT(40),
     &        IOUT,NDRTVL,IDRTAL,IFREFM,NCOL,NROW,NLAY,
     &        NDRTNP,NPDRT,IDRTPB,IDRTFL,NRFLOW,ierror)
          if (ierror.ne.0) goto 700
          call setboundary(drtf,NDRTVL,NDRTCL,position,ibousz,
     1          ibnode,ierror)
700       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c DAF package
      IF(IUNIT(51).GT.0) THEN
        if (.not.gwt.or.cperiod.gt.0) then
C       store the number of DAF cells
          position=position + 1
          ibnode(position)=idafcount
c       store the node number of each DAF cell
          DO 60 NN=1,NBRCH
            INX=NXSEC(NN)-1
            DO 50 II=2,INX
              k = NLY(II,NN);
              j = NRW(II,NN)
              i = NCL(II,NN)
              position=position + 1
              ibnode(position)=rcl2i(j,i,k)
   50       CONTINUE
   60     CONTINUE
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
c       store the fact that there are no DAF cells.   
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c MNW package
      IF(IUNIT(50).GT.0) THEN
        if (.not.gwt.or.cperiod.gt.0) then
          CALL GWF1MNW1RP(well2,NWELL2,MXWEL2,
     &                         NODES,NROW,NCOL,KSPREF,IUNIT(50),
     &                         IOUT,IOWELL2,
     &                         NLAY,PLOSSMNW, ierror)
          if (ierror.ne.0) goto 10
          position=position + 1
          if (position.gt.ibousz) then
            ierror=19
            goto 10
          endif
          ibnode(position)=NWELL2
          do 120 imnw=1, NWELL2
            n = INT(well2(1,imnw))
            k = (n-1)/(ncol*nrow) + 1
            j = mod((n-1),ncol*nrow)/ncol + 1
            i = mod((n-1),ncol) + 1
            position=position + 1
            if (position.gt.ibousz) then
              ierror=19
              goto 10
            endif
            ibnode(position)=rcl2i(j,i,k)
  120     continue
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c     SFR package
      IF(IUNIT(44).GT.0) THEN
        if (.not.gwt.or.cperiod.gt.0) then
          if (.not. allocated(SFRSTRM)) then
            allocate(SFRSTRM(18,NSTRM))
          end if
          if (.not. allocated(ISTRMSFR)) then
            allocate(ISTRMSFR(5,NSTRM))
          end if
          if (.not. allocated(SEG)) then
            allocate(SEG(17,NSEGDIM))
          end if
          if (.not. allocated(ISEG)) then
            allocate(ISEG(3,NSEGDIM))
          end if
          if (.not. allocated(IOTSG)) then
            allocate(IOTSG(NSEGDIM))
          end if
          if (.not. allocated(ISFRDIVAR)) then
            allocate(ISFRDIVAR(2,NSEGDIM))
          end if
          if (.not. allocated(SGOTFLW)) then
            allocate(SGOTFLW(NSSSFR))
          end if
          if (.not. allocated(DVRSFLW)) then
            allocate(DVRSFLW(NSSSFR))
          end if
          if (.not. allocated(XSEC)) then
            allocate(XSEC(16,NSEGDIM))
          end if
          if (.not. allocated(QSTAGE)) then
            allocate(QSTAGE(MAXPTS,NSEGDIM))
          end if
          if (.not. allocated(CONCQ)) then
            allocate(CONCQ(NSEGDIM,NSOLSFR))
          end if
          if (.not. allocated(CONCRUN)) then
            allocate(CONCRUN(NSEGDIM,NSOLSFR))
          end if
          if (.not. allocated(CONCPPT)) then
            allocate(CONCPPT(NSEGDIM,NSOLSFR))
          end if
          if (.not. SFR_HasBeenRead) then
             CALL GWF1SFR1RPP(SFRSTRM,ISTRMSFR,
     2         NSTRM,IUNIT(44),IOUTG,SEG,ISEG,NSSSFR,
     3       ISFRDIVAR,IOTSG,SGOTFLW,DVRSFLW,MAXPTS,
     4       XSEC,QSTAGE,IUNIT(15),CONCQ,
     5       CONCRUN,CONCPPT,NSOLSFR,NSTRPAR,NSEGDIM,ITERPK,
     &       INAMLOC,IBOUND,NCOL,NROW,NLAY,ierror)
	       SFR_HasBeenRead = .TRUE.
	    endif
          if (ierror.ne.0) goto 10
          position=position + 1
          if (position.gt.ibousz) then
            ierror=19
            goto 10
          endif
          ibnode(position)=NSTRM
          DO 160 NN=1,NSTRM
              k = ISTRMSFR(1,NN);
              i = ISTRMSFR(2,NN)
              j = ISTRMSFR(3,NN)
              position=position + 1
              if (position.gt.ibousz) then
                ierror=19
                goto 10
              endif
              ibnode(position)=rcl2i(i,j,k)
  160     CONTINUE
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
          position=position + 1
          if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c     lake package
      if(IUNIT(22).gt.0) then
        if (.not.gwt.or.cperiod.gt.0) then

          call GETLAKE(iunit(22),NLAKES,IFREFM,iunit(15),nsol,
     1              lkarr1,ncol,nrow,nlay,iend,ierror)
          if (ierror.ne.0) goto 10
          position=position+1
          p0=position
          DO 130 K=1,NLAY
          DO 130 I=1,NCOL
          DO 130 J=1,NROW
            IF(LKARR1(I,J,K).GT.0.AND.LKARR1(I,J,K).LE.NLAKES) THEN
              position=position + 1
              ibnode(position)=rcl2i(j,i,k)
            end if
  130     CONTINUE
          ibnode(p0)=position-p0
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      end if
	if (NUMGWTOBS .GT. 0) then
        position=position + 1
        ibnode(position)=NUMGWTOBS
	  do 140 i=1,NUMGWTOBS
          position=position + 1
          ibnode(position)=GwtObs(i)
  140   continue
	else
        position=position + 1
        ibnode(position)=-1
	endif
10    continue
      return
      end subroutine getfeatures

      subroutine cleanup
      use mf2kmodule
	use mf2kReadSub
	use SUBARRAYS
      implicit none
      integer i
      if (allocated(IRES)) deallocate (IRES)
      if (allocated(IRESL)) deallocate (IRESL)
      if (allocated(fhbflow)) deallocate (fhbflow)
      if (allocated(fhbhead)) deallocate (fhbhead)
      if (allocated(IBOUND)) deallocate(IBOUND)
      if (allocated(STRT)) deallocate(STRT)
      if (allocated(well)) deallocate(well)
      if (allocated(drain)) deallocate(drain)
      if (allocated(river)) deallocate(river)
      if (allocated(ghb)) deallocate(ghb)
      if (allocated(stream)) deallocate(stream)
      if (allocated(chd)) deallocate(chd)
      if (allocated(drtf)) deallocate(drtf)
      if (allocated(STRM)) deallocate(STRM)
      if (allocated(ISTRM)) deallocate(ISTRM)
      if (allocated(ITRBAR)) deallocate(ITRBAR)
      if (allocated(IDIVAR)) deallocate(IDIVAR)
      if (allocated(lkarr1)) deallocate(lkarr1)
      if (allocated(GwtObs)) deallocate(GwtObs)
      if (ALLOCATED(LDN)) DEALLOCATE (LDN)
      do i=1,mxunit
         close(i,err=20)
20       continue
      end do
	call clearsubsidencerecord
      return 
      end subroutine cleanup

      subroutine storefhb(iflloc,ihdloc,ierror)
      use mf2kmodule
      implicit none
      integer position, layer, row, column,ierror 
      integer iflloc(4,nflw), ihdloc(4,nhed)
      interface
        integer function rcl2i(arow,acol,alayer)
          implicit none
          integer,intent(in)::arow,acol,alayer
        end function rcl2i
      end interface
      do position=1,nflw
        layer=iflloc(1,position)
        row=iflloc(2,position)
        column=iflloc(3,position)
        fhbflow(position)=rcl2i(row,column,layer)
        if (ierror.ne.0) goto 10
      enddo 
      do position=1,nhed
        layer=ihdloc(1,position)
        row=ihdloc(2,position)
        column=ihdloc(3,position)
        fhbhead(position)=rcl2i(row,column,layer)
        if (ierror.ne.0) goto 10
      enddo 
   10 continue
      return
      end subroutine storefhb

      subroutine setboundary(boundary,firstlimit,secondlimit,position,
     1   ibousz,ibnode,ierror)
      implicit none
      interface
        integer function rcl2i(arow,acol,alayer)
          implicit none
          integer,intent(in)::arow,acol,alayer
        end function rcl2i
      end interface
      integer firstlimit,secondlimit,position,index,ierror
      real boundary(firstlimit,secondlimit)
      integer ibousz
      integer ibnode(ibousz)
      integer layer,row,column
      position=position + 1
      ibnode(position)=secondlimit
      do index=1,secondlimit
        layer =boundary(1,index)
        row   =boundary(2,index)
        column=boundary(3,index)
        position=position + 1
        ibnode(position)=rcl2i(row,column,layer)
        if (ierror.ne.0) goto 10
      enddo
10    continue
      return
      end subroutine setboundary

      subroutine setreservior(position,ibnode,ierror)
      use mf2kmodule
      implicit none
      interface
        integer function rcl2i(arow,acol,alayer)
          implicit none
          integer,intent(in)::arow,acol,alayer
        end function rcl2i
      end interface
      integer position,ierror
      integer ibnode(ibousz)
      integer row,column,layer,index
      ierror=0
      if (.not.(allocated(ires))) ierror=19
      if ((nresop.eq.2).and..not.(allocated(iresl))) ierror=19
      if ((nresop.eq.3).and..not.(allocated(ibound))) ierror=19
      if (ierror.ne.0) goto 10
      layer=0
      position=position + 1
      ibnode(position)=ncell
      do row=1, nrow
        do column=1, ncol
          if (ires(column,row).ne.0) then
            if (nresop.eq.1) then 
              layer=1
            elseif (nresop.eq.2) then 
              layer=iresl(column,row)
            elseif (nresop.eq.3) then 
              layer=0
              do index=1, nlay
                if (ibound(column,row,index).ne.0) then 
                  layer=index
                  exit
                endif 
              enddo
            endif
            if (layer.gt.0) then
              position=position + 1
              ibnode(position)=rcl2i(row,column,layer)
              if (ierror.ne.0) goto 10
            endif
          endif
        enddo
      enddo
10    continue
      return
      end subroutine setreservior

      subroutine setfhb(position,ibnode,ierror)
      use mf2kmodule
      implicit none
      integer ierror
      integer ibnode(ibousz)
      integer position,index
      ierror=0
      position=position + 1
      if (position.gt.ibousz) then
        ierror=19
        goto 10
      endif
      if (nflw.gt.0) then
        ibnode(position)=nflw
        do index=1,nflw
          position=position + 1
          if (position.gt.ibousz) then
            ierror=19
            goto 10
          endif
          ibnode(position)=fhbflow(index)
        enddo
      else
        ibnode(position)=-1
      end if
      position=position + 1
      if (position.gt.ibousz) then
        ierror=19
        goto 10
      endif
      if (nhed.gt.0) then
        ibnode(position)=nhed
        do index=1,nhed
          position=position + 1
          if (position.gt.ibousz) then
            ierror=19
            goto 10
          endif
          ibnode(position)=fhbhead(index)
        enddo
      else
        ibnode(position)=-1
      end if
10    continue
      return
      end subroutine setfhb

      integer function rcl2i(arow,acol,alayer) 
c     returns a node number based on a row column and layer position.
      use mf2kmodule
      implicit none
      integer,intent(in)::arow,acol,alayer
      integer layer,i
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      common /subgrd/
     1  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     --------------------------------------------
      if ((alayer.lt.1).or.(alayer.gt.nlay).or.(arow.lt.1).or.
     1      (arow.gt.nrow).or.(acol.lt.1).or.(acol.gt.ncol)) then
        rcl2i=-1
        return
      endif
      if (gwt) then
        if ((alayer.lt.islay1).or.(alayer.gt.islay2).or.
     1        (arow.lt.isrow1).or.(arow.gt.isrow2).or.
     2        (acol.lt.iscol1).or.(acol.gt.iscol2)) then
          rcl2i=-1
          return
        else
          rcl2i=(islay2-alayer)*nsrow*nscol 
     1       + (isrow2-arow)*nscol + (acol-iscol1)
          return
        endif 
      else
        layer=0
        do i=1,alayer
          layer=layer + 1
          if ((i.gt.1).and.(laycbd(i-1).ne.0)) then
            layer=layer + 1
          endif
        end do
        rcl2i=(nbotm-layer)*nrow*ncol 
     1      + (nrow-arow)*ncol + (acol-1)
        return
      end if
      end function rcl2i
