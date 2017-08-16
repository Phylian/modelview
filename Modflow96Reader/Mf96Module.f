      module mf96module
c     basic simulation variables
      logical moc
      integer NCOL,NROW,NLAY
      integer INBAS,IOUT,IUNIT(40),IFREFM,ISS
	integer, allocatable :: IBOUND(:,:,:)
	integer lunit   ! unit number for elevation file
	integer ncnfbd  ! number of confining beds
	integer nbotm   ! number of layer bottoms = NLAY + ncnfbd
	integer nfixedhead ! number of fixed head cells (for which IBOUND < 0)
	integer maxunit  ! highest unit number
	integer laycbd(200)
c     variables used by well package
	integer MXWELL,NWELLS,NWELVL,IWELAL
c     variables used by drain package
	integer MXDRN,NDRAIN,NDRNVL,IDRNAL
c     variables used by river package
	integer MXRIVR,NRIVER,NRIVVL,IRIVAL
c     variables used by general head boundary package
	integer MXBND,NBOUND,NGHBVL,IGHBAL
c     variables used by reservior package
	integer NRES,NRESOP,NPTS,NCELL
	integer,allocatable::IRES (:,:),IRESL(:,:)
c     variables used by stream package
	integer MXSTRM,NSTREM,NSS,NTRIB,NDIV,ICALC
	integer,allocatable::ISTRM(:,:),ITRBAR(:,:),IDIVAR(:)
	real,allocatable::STRM(:,:)
c     variables used by chd package
	integer NCHDS,MXCHD
c     variables used by fhb package
	integer NFLW,NHED
	integer,allocatable::fhbflow(:),fhbhead(:)
c     variables used to store package information
	integer ibousz
      real,allocatable::well(:,:),drain(:,:),river(:,:), 
	1      ghb(:,:),stream(:,:),chd(:,:)
c     variables used by MOC3D
	integer JUNIT(40),NSCOL,NSROW,NSLAY
      end module mf96module
