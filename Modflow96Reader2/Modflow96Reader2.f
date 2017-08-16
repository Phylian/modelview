      subroutine mf96rewindparticlefile2(ierror)
c filetype
c 0 = ASCII
c 1 = binary (unstructured, non formatted)
c 2 = unformatted - Compaq, Visual Fortran
c 3 = unformatted - Lahey Fortran 90 (F90)
c 4 = Big Endian - Unix
      implicit none
!DEC$ attributes dllexport::mf96rewindparticlefile2
      integer,intent(out)::ierror
	  call rewindparticlefile(ierror)
	return
	end subroutine mf96rewindparticlefile2
c
c
c
	subroutine mf96readparticlecount2(NP,ierror,istep)
      implicit none
!DEC$ attributes dllexport::mf96readparticlecount2
      integer,intent(out)::ierror,NP
      integer,intent(in)::istep
	  call readparticlecount(NP,ierror,istep) 
	return
	end
c
c
c
	subroutine mf96readparticles2(NP,ierror,coord,scalars, 
	1  delr, delc, elev)
      implicit none
!DEC$ attributes dllexport::mf96readparticles2
      integer,intent(inout)::NP
      integer,intent(out)::ierror
	real,intent(inout):: coord, scalars
	real, intent(in):: delr, delc, elev
	dimension delr(*), delc(*), elev(*)
	dimension coord(NP*3), scalars(NP)
	  call readparticles(NP,ierror,coord,scalars, delr, delc, elev)
	return
	end
c
c
c
      subroutine mf96dims2(ierror,nc,nr,nl,moc,unstruct,ITimeUnit,
     1      namefile,elevfile)
	use mfcommonmodule
      implicit none
!DEC$ attributes dllexport::mf96dims2
      integer ierror,nc,nr,nl,moc,unstruct
      character*256 namefile,elevfile
	integer ITimeUnit
      call dims(ierror,nc,nr,nl,moc,unstruct,ITimeUnit,namefile,
	1  elevfile,IXSEC)
      return
      end

      subroutine mf96grid2(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      implicit none
!DEC$ attributes dllexport::mf96grid2
      integer ierror,ibound(*),sunit(*),vunit,q3dcl(*)
      real delr(*),delc(*),elev(*),conductivity(*),hnoflo,hdry
      real xoffset,yoffset
      call grid(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      return
      end

      subroutine mf96countscalars2(ierror,funit,ndatasets,
     1      datatype)
      use mf96module
      implicit none
!DEC$ attributes dllexport::mf96countscalars2
      integer ierror,funit,ndatasets
      character*17 datatype
	if (moc) then
	  call countconc(ierror,funit,nscol,nsrow,nslay,
     1        ndatasets,datatype)
	else
        call countheads(ierror,funit,ncol,nrow,nlay,
     1        ndatasets,datatype)
	endif
      return
      end

      subroutine mf96countvectors2(ierror,funit)
      use mf96module
      implicit none
!DEC$ attributes dllexport::mf96countvectors2
      integer ierror,funit
	if (moc) then
	  call countvel(ierror,funit,nscol,nsrow,nslay)
	else
        call countcbc(ierror,funit,ncol,nrow,nlay)
	endif
      return
      end

      subroutine mf96countfeatures2(ierror,ibsize,nfeat)
      implicit none
!DEC$ attributes dllexport::mf96countfeatures2
      integer ibsize,nfeat,ierror
      call countfeatures(ierror,ibsize,nfeat)
      return
      end

      subroutine mf96gettimepoints2(timepoints,periods,steps,moves,
     1           numtimepoints)
      implicit none
!DEC$ attributes dllexport::mf96gettimepoints2
      integer numtimepoints
	integer periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
      real timepoints(numtimepoints)
      call gettimepoints(timepoints,periods,steps,moves,numtimepoints)
      return
      end

      subroutine mf96getscalars2(ierror,funit,a,istep,kper)
      use mf96module
      implicit none
!DEC$ attributes dllexport::mf96getscalars2
      integer ierror,funit,istep,kper
      real a(*)
	if (moc) then
	  call getconc(ierror,a,kper,funit,istep,
     1        nscol,nsrow,nslay)
	else
        call getheads(ierror,a,kper,funit,istep,
     1        ncol,nrow,nlay,nbotm,laycbd)
	endif
      return
      end

      subroutine mf96getvectors2(ierror,funit,array,istep)
      use mf96module
      implicit none
!DEC$ attributes dllexport::mf96getvectors2
      integer ierror,funit,istep
      real array(*)
	if (moc) then
	  call getvel(ierror,funit,array,istep,
     1        nscol,nsrow,nslay)
	else
        call getcbc(ierror,funit,array,istep,
     1        ncol,nrow,nlay,nbotm,laycbd)
	endif
      return
      end

      subroutine mf96getfeatures2(ierror,ibnode)
      implicit none
!DEC$ attributes dllexport::mf96getfeatures2
      integer ierror
      integer ibnode(*)
      call getfeatures(ierror,ibnode)
      return
      end

      subroutine mf96cleanup2
      implicit none
!DEC$ attributes dllexport::mf96cleanup2
      call cleanup
      return
      end
