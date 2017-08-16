      subroutine mf2krewindparticlefile2(ierror)
c filetype
c 0 = ASCII
c 1 = binary (unstructured, non formatted)
c 2 = unformatted - Compaq, Visual Fortran
c 3 = unformatted - Lahey Fortran 90 (F90)
c 4 = Big Endian - Unix
      implicit none
!DEC$ attributes dllexport::mf2krewindparticlefile2
      integer,intent(out)::ierror
	  call rewindparticlefile(ierror)
	return
	end
c
c
c
	subroutine mf2kreadparticlecount2(NP,ierror,istep)
      implicit none
!DEC$ attributes dllexport::mf2kreadparticlecount2
      integer,intent(out)::ierror,NP
      integer,intent(in)::istep
	  call readparticlecount(NP,ierror,istep) 
	return
	end
c
c
c
	subroutine mf2kreadparticles2(NP,ierror,coord,scalars, 
	1 delr, delc, elev)
      implicit none
!DEC$ attributes dllexport::mf2kreadparticles2
      integer,intent(inout)::NP
      integer,intent(out)::ierror
	real,intent(inout):: coord, scalars
	real, intent(in):: delr, delc, elev
	dimension delr(*), delc(*), elev(*)
	dimension coord(NP*3), scalars(NP)
	  call readparticles(NP,ierror,coord,scalars,delr, delc, elev)
	return
	end
c
c
c      
	subroutine mf2kdims2(ierror,nc,nr,nl,gwt,unstruct,ITimeUnit,
	1  namefile)
      implicit none
!DEC$ attributes dllexport::mf2kdims2
      integer ierror,nc,nr,nl,gwt,unstruct
      character*256 namefile
	integer ITimeUnit
      call dims(ierror,nc,nr,nl,gwt,unstruct,ITimeUnit,namefile)
      return
      end

      subroutine mf2kgrid2(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      implicit none
!DEC$ attributes dllexport::mf2kgrid2
      integer ierror,ibound(*),sunit(*),vunit,q3dcl(*)
      real delr(*),delc(*),elev(*),conductivity(*),hnoflo,hdry
      real xoffset,yoffset
      call grid(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      return
      end

      subroutine mf2kcountscalars2(ierror,funit,ndatasets,datatype)
      use mf2kmodule
	use SUBARRAYS
      implicit none
!DEC$ attributes dllexport::mf2kcountscalars2
      integer ierror,funit,ndatasets
      character*17 datatype
	ierror = 0
	if (gwt) then
	  call countconc(ierror,funit,nscol,nsrow,nslay,
     1        ndatasets,datatype)
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

      subroutine mf2kcountvectors2(ierror,funit)
      use mf2kmodule
      implicit none
!DEC$ attributes dllexport::mf2kcountvectors2
      integer ierror,funit
	if (gwt) then
	  call countvel(ierror,funit,nscol,nsrow,nslay)
	else
        call countcbc(ierror,funit,ncol,nrow,nlay)
	endif
      return
      end

      subroutine mf2kcountfeatures2(ierror,ibsize,nfeat,ibnd)
      implicit none
!DEC$ attributes dllexport::mf2kcountfeatures2
      integer ibsize,nfeat,ierror,ibnd(*)
      call countfeatures(ierror,ibsize,nfeat,ibnd)
      return
      end

      subroutine mf2kgettimepoints2(timepoints,periods,steps,moves,
     1          numtimepoints)
      implicit none
!DEC$ attributes dllexport::mf2kgettimepoints2
      integer numtimepoints
	integer periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
      real timepoints(numtimepoints)
      call gettimepoints(timepoints,periods,steps,moves,numtimepoints)
      return
      end

      subroutine mf2kgetscalars2(ierror,funit,a,istep,kper)
      use mf2kmodule
	use SUBARRAYS
      implicit none
!DEC$ attributes dllexport::mf2kgetscalars2
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
	  call getconc(ierror,a,kper,funit,istep,
     1        nscol,nsrow,nslay)
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

      subroutine mf2kgetvectors2(ierror,funit,array,istep)
      use mf2kmodule
      implicit none
!DEC$ attributes dllexport::mf2kgetvectors2
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

      subroutine mf2kgetfeatures2(ierror,ibnode)
      implicit none
!DEC$ attributes dllexport::mf2kgetfeatures2
      integer ierror
      integer ibnode(*)
      call getfeatures(ierror,ibnode)
      return
      end

      subroutine mf2kcleanup2
      implicit none
!DEC$ attributes dllexport::mf2kcleanup2
      call cleanup
      return
      end
