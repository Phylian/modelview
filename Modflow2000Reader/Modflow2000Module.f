      module mf2kmodule
c     basic simulation variables
      logical gwt
	integer NCOL,NROW,NLAY
      integer LAYHDT(999),IOUTG,IOUT,NPER,ITMUNI,INBAS,LENUNI,
     1   ISUMX,ISUMZ,ISUMIX,IFREFM,ISS,ITRSS,NSOL,NPLIST
	integer, allocatable :: IBOUND(:,:,:)
      integer numflow ! number of cell by cell flow components saved to file
      integer NCNFBD  ! number of confining beds
      integer NBOTM   ! number of layer bottoms = NLAY + ncnfbd
	integer nfixedhead ! number of fixed head cells (for which IBOUND < 0)
	integer mxunit  ! highest unit number
      integer NIUNIT,MXPER,NMLTAR,NZONAR,NML,NZN,IPRTIM,IBDT
      parameter (NIUNIT=100)
      parameter (MXPER=1000)
	integer IUNIT(NIUNIT)
      integer IREWND(NIUNIT)
c     named common block shared with modflow 2k
      integer LBOTM,LAYCBD
      common /DISCOM/LBOTM(999),LAYCBD(999)
c     variables used by well package
      integer MXWELL,NWELLS,NWELVL,IWELAL,NPWEL,IPWBEG,NNPWEL
c     variables used by drain package
	integer MXDRN,NDRAIN,NDRNVL,IDRNAL,NPDRN,IDRNPB,NDRNNP
c     variables used by river package
      integer MXRIVR,NRIVER,NRIVVL,IRIVAL,NPRIV,IRIVPB,NRIVNP
c     variables used by general head boundary package
	integer MXBND,NBOUND,NGHBVL,IGHBAL,NPGHB,IGHBPB,NGHBNP
c     variables used by reservior package
	integer NRES,NRESOP,NPTS,NCELL
	integer,allocatable::IRES (:,:),IRESL(:,:)
c     variables used by stream package
	integer MXSTRM,NSTREM,NSS,NTRIB,NDIV,ICALC,NPSTR,ISTRPB,NUMTAB
	integer,allocatable::ISTRM(:,:),ITRBAR(:,:),IDIVAR(:)
	real,allocatable::STRM(:,:)
c     variables used by chd package
      integer NCHDS,MXCHD,NCHDVL,NPCHD,IPCBEG,NNPCHD
c     variables used by fhb package
      integer NFLW,NHED
	integer, allocatable:: fhbflow (:),fhbhead(:)
c     variables used by DRT package
      integer MXDRT,NDRTCL,NDRTVL,IDRTAL,NDRTNP,NPDRT,IDRTPB,IDRTFL
c     variables used the MNW package
      integer MXWEL2, LCWEL2, NWELL2, NODES, LCHREF, KSPREF, IWL2CB, 
     +  IOWELL2, NOMOITER
	REAL PLOSSMNW
	CHARACTER*200 MNWNAME
	CHARACTER*200 FNAME
c     variables used by LAK package
      integer NLAKES
	integer,allocatable::LKARR1(:,:,:)
c     variables used to store package information
	integer ibousz
      real, allocatable :: well(:,:),drain(:,:),river(:,:),
	1      ghb(:,:),stream(:,:),chd(:,:),drtf(:,:), well2(:,:)
c     variables used by GWT
	integer moctype,inmoc,NPTPND,NEWPTS
	integer JUNIT(NIUNIT),NSCOL,NSROW,NSLAY
c     variables used by DAF
      integer idafcount
c	variables used by SFR package
      integer NSTRM, NSEGDIM, NSSSFR, MAXPTS, NSOLSFR, NSTRPAR, 
	1   ITERPK, INAMLOC, ISTCB1,ISTCB2,NSTRMAR,NSSAR
	real CONST,DLEAK
      real,allocatable::  SFRSTRM(:,:),SEG(:,:),SGOTFLW(:),DVRSFLW(:),
     1          XSEC(:,:),QSTAGE(:,:),CONCQ(:,:),CONCRUN(:,:),
     2          CONCPPT(:,:)
	integer, allocatable:: ISTRMSFR(:,:),ISEG(:,:),IOTSG(:),
	1          ISFRDIVAR(:,:)
      logical SFR_HasBeenRead
c     variables used for GWT observations wells
      integer, allocatable:: GwtObs (:)
	integer NUMGWTOBS
	integer, allocatable:: STRT(:,:,:)
	end module mf2kmodule

      module mf2kSubModule
C     variables used by SUB
      integer ITERP, ISUBCB, ISUBOC, ITMIN, NNDB, NDB, NPZ, NN, NND1
	INTEGER ND1, ND2, IDSAVE, IDREST, NSTPT, LCV
	real AC1, AC2
	end module mf2kSubModule

