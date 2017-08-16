      subroutine mf2krewindparticlefile0(ierror)
c filetype
c 0 = binary (unstructured, non formatted)
c 1 = unformatted - Compaq, Visual Fortran
c 2 = unformatted - Lahey Fortran 90 (F90)
c 3 = Big Endian - Unix
      implicit none
!DEC$ attributes dllexport::mf2krewindparticlefile0
      integer,intent(out)::ierror
	  call rewindparticlefile(ierror)
	return
	end
c
c
c
	subroutine mf2kreadparticlecount0(NP,ierror,istep)
      implicit none
!DEC$ attributes dllexport::mf2kreadparticlecount0
      integer,intent(out)::ierror,NP
      integer,intent(in)::istep
	  call readparticlecount(NP,ierror,istep) 
	return
	end
c
c
c
	subroutine mf2kreadparticles0(NP,ierror,coord,scalars, 
	1 delr, delc, elev)
      implicit none
!DEC$ attributes dllexport::mf2kreadparticles0
	real, intent(in):: delr, delc, elev
	dimension delr(*), delc(*), elev(*)
      integer,intent(inout)::NP
      integer,intent(out)::ierror
	real,intent(inout):: coord, scalars
	dimension coord(NP*3), scalars(NP)
	  call readparticles(NP,ierror,coord,scalars, delr, delc, elev)
	return
	end
c
c
c
      subroutine mf2kdims0(ierror,nc,nr,nl,gwt,unstruct,ITimeUnit, 
	1  namefile)
      implicit none
!DEC$ attributes dllexport::mf2kdims0
      integer ierror,nc,nr,nl,gwt,unstruct
      character*256 namefile
	integer ITimeUnit
      call dims(ierror,nc,nr,nl,gwt,unstruct,ITimeUnit,namefile)
      return
      end

      subroutine mf2kgrid0(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      implicit none
!DEC$ attributes dllexport::mf2kgrid0
      integer ierror,ibound(*),sunit(*),vunit,q3dcl(*)
      real delr(*),delc(*),elev(*),conductivity(*),hnoflo,hdry
      real xoffset,yoffset
      call grid(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      return
      end

      subroutine mf2kcountscalars0(ierror,funit,ndatasets,datatype)
      use mf2kmodule
	use SUBARRAYS
	use SavedUnits
      implicit none
!DEC$ attributes dllexport::mf2kcountscalars0
      integer ierror,funit,ndatasets
      character*17 datatype
	ierror = 0
	if (gwt) then
!	Attempting to read the head and drawdown data with GWT always fails because
!     the concentration contains data for stress period zero (initial conditions)
!	and the head and drawdown data do not.

!	if ((abs(funit).eq.HeadUnit).or.(abs(funit).eq.DrawdownUnit)) then
!          call countheads(ierror,funit,ncol,nrow,nlay,
!     1        ndatasets,datatype)
!	  else
	    call countconc(ierror,funit,nscol,nsrow,nslay,
     1        ndatasets,datatype)
!	  endif 
	else
	  if ((funit.eq.ISBOCU(2)).or.(funit.eq.ISBOCU(5))
	1     .or.(funit.eq.ISBOCU(6))) then
          call countsubsidence(ierror,funit,ncol,nrow,nlay,
     1        ndatasets,datatype)
	  else
          call countheads(ierror,funit,ncol,nrow,nlay,
     1        ndatasets,datatype)
	  endif
	endif
      return
      end

      subroutine mf2kcountvectors0(ierror,funit)
      use mf2kmodule
      implicit none
!DEC$ attributes dllexport::mf2kcountvectors0
      integer ierror,funit
	if (gwt) then
	  call countvel(ierror,funit,nscol,nsrow,nslay)
	else
        call countcbc(ierror,funit,ncol,nrow,nlay)
	endif
      return
      end

      subroutine mf2kcountfeatures0(ierror,ibsize,nfeat,ibnd)
      implicit none
!DEC$ attributes dllexport::mf2kcountfeatures0
      integer ibsize,nfeat,ierror,ibnd(*)
      call countfeatures(ierror,ibsize,nfeat,ibnd)
      return
      end

      subroutine mf2kgettimepoints0(timepoints,periods,steps,moves,
     1      numtimepoints)
      implicit none
!DEC$ attributes dllexport::mf2kgettimepoints0
      integer numtimepoints
	integer periods(numtimepoints),steps(numtimepoints),
     1  moves(numtimepoints)
      real timepoints(numtimepoints)
      call gettimepoints(timepoints,periods,steps,moves,numtimepoints)
      return
      end

      subroutine mf2kgetscalars0(ierror,funit,a,istep,kper)
      use mf2kmodule
	use SUBARRAYS
	use SavedUnits
      implicit none
!DEC$ attributes dllexport::mf2kgetscalars0
      integer ierror,funit,istep,kper
	! RBW It shouldn't be necessary to make dummy an allocatable array.
	! However, in one test case, Model Viewer will crash here if it isn't
	! allocatable.  This may mean that there is memory corruption somewhere.
	!real dummy(NCOL,NROW,NLAY)
	real, allocatable, dimension(:,:,:) :: dummy
      real a(*)
	allocate(dummy(NCOL,NROW,NLAY))
	ierror = 0
	if (gwt) then
!	  if ((abs(funit).eq.HeadUnit).or.(abs(funit).eq.DrawdownUnit)) 
!	1    then
!          call getgwtheads(ierror,a,kper,funit,istep,
!     1        ncol,nrow,nlay,nbotm,laycbd)
!	  else
	    call getconc(ierror,a,kper,funit,istep,
     1        nscol,nsrow,nslay)
!	  endif
	else
	  if (funit.eq.ISBOCU(2)) then
	    dummy = 0
	    call getsubsidence(ierror,a,kper,funit,istep,
     1        ncol,nrow,nlay,nbotm,laycbd,IBOUND,dummy)
	  elseif ((funit.eq.ISBOCU(5)).or.(funit.eq.ISBOCU(6))) then
		call getsubsidence(ierror,a,kper,funit,istep,
     1        ncol,nrow,nlay,nbotm,laycbd,IBOUND,STRT)
	  else
          call getheads(ierror,a,kper,funit,istep,
     1        ncol,nrow,nlay,nbotm,laycbd)
	  endif
	endif
	deallocate(dummy)
      return
      end

      subroutine mf2kgetvectors0(ierror,funit,array,istep)
      use mf2kmodule
      implicit none
!DEC$ attributes dllexport::mf2kgetvectors0
      integer ierror,funit,istep
      real array(*)
	if (gwt) then
	  call getvel(ierror,funit,array,istep,
     1        nscol,nsrow,nslay)
	else
        call getcbc(ierror,funit,array,istep,
     1        ncol,nrow,nlay,nbotm,laycbd)
	endif
      return
      end

      subroutine mf2kgetfeatures0(ierror,ibnode)
      implicit none
!DEC$ attributes dllexport::mf2kgetfeatures0
      integer ierror
      integer ibnode(*)
      call getfeatures(ierror,ibnode)
      return
      end

      subroutine mf2kcleanup0
      implicit none
!DEC$ attributes dllexport::mf2kcleanup0
      call cleanup
      return
      end
