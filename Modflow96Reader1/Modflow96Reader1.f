      subroutine mf96rewindparticlefile1(ierror)
      implicit none
      dll_export mf96rewindparticlefile1
      integer,intent(out)::ierror
        call rewindparticlefile(ierror)
      return
      end

      subroutine mf96readparticlecount1(NP,ierror,istep)
      implicit none
      dll_export mf96readparticlecount1
      integer,intent(out)::ierror,NP
      integer,intent(in)::istep
        call readparticlecount(NP,ierror,istep)
      return
      end

      subroutine mf96readparticles1(NP,ierror,coord,scalars,
     1  delr, delc, elev)
      implicit none
      dll_export mf96readparticles1
      integer,intent(inout)::NP
      integer,intent(out)::ierror
      real,intent(inout):: coord, scalars
      dimension coord(NP*3), scalars(NP)
      real, intent(in):: delr, delc, elev
      dimension delr(*), delc(*), elev(*)
        call readparticles(NP,ierror,coord,scalars)
      return
      end

      subroutine mf96dims1(ierror,nc,nr,nl,moc,unstruct,ITimeUnit,
     1      namefile,elevfile)
	use mfcommonmodule
      implicit none
      dll_export mf96dims1
      integer ierror,nc,nr,nl,moc,unstruct
      character*256 namefile,elevfile
      integer ITimeUnit
      close(unit=5)
      close(unit=6)
      call dims(ierror,nc,nr,nl,moc,unstruct,ITimeUnit,namefile,
     1  elevfile,IXSEC)
      return
      end

      subroutine mf96grid1(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      implicit none
      dll_export mf96grid1
      integer ierror,ibound(*),sunit(*),vunit,q3dcl(*)
      real delr(*),delc(*),elev(*),conductivity(*),hnoflo,hdry
      real xoffset,yoffset
      call grid(ierror,delr,delc,elev,ibound,conductivity,
     1      hnoflo,hdry,sunit,vunit,xoffset,yoffset,q3dcl)
      return
      end

      subroutine mf96countscalars1(ierror,funit,ndatasets,
     1      datatype)
      use mf96module
      implicit none
      dll_export mf96countscalars1
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

      subroutine mf96countvectors1(ierror,funit)
      use mf96module
      implicit none
      dll_export mf96countvectors1
      integer ierror,funit
	if (moc) then
	  call countvel(ierror,funit,nscol,nsrow,nslay)
	else
        call countcbc(ierror,funit,ncol,nrow,nlay)
	endif
      return
      end

      subroutine mf96countfeatures1(ierror,ibsize,nfeat)
      implicit none
      dll_export mf96countfeatures1
      integer ibsize,nfeat,ierror
      call countfeatures(ierror,ibsize,nfeat)
      return
      end

      subroutine mf96gettimepoints1(timepoints,periods,steps,moves,
     1           numtimepoints)
      implicit none
      dll_export mf96gettimepoints1
      integer numtimepoints
	integer periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
      real timepoints(numtimepoints)
      call gettimepoints(timepoints,periods,steps,moves,numtimepoints)
      return
      end

      subroutine mf96getscalars1(ierror,funit,a,istep,kper)
      use mf96module
      implicit  none
      dll_export mf96getscalars1
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

      subroutine mf96getvectors1(ierror,funit,array,istep)
      use mf96module
      implicit none
      dll_export mf96getvectors1
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

      subroutine mf96getfeatures1(ierror,ibnode)
      implicit none
      dll_export mf96getfeatures1
      integer ierror
      integer ibnode(*)
      call getfeatures(ierror,ibnode)
      return
      end

      subroutine mf96cleanup1
      implicit none
      dll_export mf96cleanup1
      call cleanup
      return
      end
