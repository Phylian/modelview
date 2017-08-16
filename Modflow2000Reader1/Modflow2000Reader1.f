      subroutine mf2krewindparticlefile1(ierror)
      implicit none
      dll_export mf2krewindparticlefile1
      integer,intent(out)::ierror
        call rewindparticlefile(ierror)
      return
      end

      subroutine mf2kreadparticlecount1(NP,ierror,istep)
      implicit none
      dll_export mf2kreadparticlecount1
      integer,intent(out)::ierror,NP
      integer,intent(in)::istep
        call readparticlecount(NP,ierror,istep)
      return
      end

      subroutine mf2kreadparticles1(NP,ierror,coord,scalars,
     1  delr, delc, elev)
      implicit none
      dll_export mf2kreadparticles1
      integer,intent(inout)::NP
      integer,intent(out)::ierror
      real,intent(inout):: coord, scalars
      dimension coord(NP*3), scalars(NP)
      real, intent(in):: delr, delc, elev
      dimension delr(*), delc(*), elev(*)
        call readparticles(NP,ierror,coord,scalars,delr, delc, elev)
      return
      end

      subroutine mf2kdims1(ierror,nc,nr,nl,gwt,unstruct,ITimeUnit,
     1  namefile)
      implicit none
      dll_export mf2kdims1
      integer ierror,nc,nr,nl,gwt,unstruct
      character*256 namefile
      integer ITimeUnit
      close(unit=5)
      close(unit=6)
      call dims(ierror,nc,nr,nl,gwt,unstruct,ITimeUnit,namefile)
      return
      end

      subroutine mf2kgrid1(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      implicit none
      dll_export mf2kgrid1
      integer ierror,ibound(*),sunit(*),dunit,vunit,q3dcl(*)
      real delr(*),delc(*),elev(*),conductivity(*),hnoflo,hdry
      real xoffset,yoffset
      call grid(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      return
      end

      subroutine mf2kcountscalars1(ierror,funit,ndatasets,datatype) 
      use mf2kmodule
	use SUBARRAYS
      implicit none
      dll_export mf2kcountscalars1
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

      subroutine mf2kcountvectors1(ierror,funit)
      use mf2kmodule
      implicit none
      dll_export mf2kcountvectors1
      integer ierror,funit
	if (gwt) then
	  call countvel(ierror,funit,nscol,nsrow,nslay)
	else
        call countcbc(ierror,funit,ncol,nrow,nlay)
	endif
      return
      end

      subroutine mf2kcountfeatures1(ierror,ibsize,nfeat,ibnd)
      implicit none
      dll_export mf2kcountfeatures1
      integer ibsize,nfeat,ierror,ibnd(*)
      call countfeatures(ierror,ibsize,nfeat,ibnd)
      return
      end

      subroutine mf2kgettimepoints1(timepoints,periods,steps,moves,
     1           numtimepoints)
      implicit none
      dll_export mf2kgettimepoints1
      integer numtimepoints
	integer periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
      real timepoints(numtimepoints)
      call gettimepoints(timepoints,periods,steps,moves,numtimepoints)
      return
      end

      subroutine mf2kgetscalars1(ierror,funit,a,istep,kper)
      use mf2kmodule
	use SUBARRAYS
      implicit none
      dll_export mf2kgetscalars1
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

      subroutine mf2kgetvectors1(ierror,funit,array,istep)
      use mf2kmodule
      implicit none
      dll_export mf2kgetvectors1
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

      subroutine mf2kgetfeatures1(ierror,ibnode)
      implicit none
      dll_export mf2kgetfeatures1
      integer ierror
      integer ibnode(*)
      call getfeatures(ierror,ibnode)
      return
      end

      subroutine mf2kcleanup1
      implicit none
      dll_export mf2kcleanup1
      call cleanup
      return
      end
