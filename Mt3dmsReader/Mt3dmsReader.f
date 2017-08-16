      subroutine mtdims0(ierror,cnfFile,ucnFile,ftlFile,unstruct,
     1   nc,nr,nl)
      implicit  none
!DEC$ attributes dllexport :: mtdims0
      character*256 cnfFile,ucnFile,ftlFile
      integer ierror,nc,nr,nl,unstruct
	call dims(ierror,cnfFile,ucnFile,ftlFile,unstruct,nc,nr,nl)
      return
	end

      subroutine mtgrid0(ierror,delr,delc,z,cinact)
	implicit none
!DEC$ attributes dllexport :: mtgrid0
      integer ierror
	real delr(*),delc(*),z(*),cinact
	call grid(ierror,delr,delc,z,cinact)
	return
	end

	subroutine mtcountscalars0(ierror,ndatasets,datatype)
      implicit none
!DEC$ attributes dllexport :: mtcountscalars0
	integer  ierror,ndatasets
	character*17 datatype
	call countscalars(ierror,ndatasets,datatype)
      return
	end

      subroutine mtcountvectorsandfeatures0(ierror,ibsize,nfeat)
      implicit none
!DEC$ attributes dllexport::mtcountvectorsandfeatures0
      integer ibsize,nfeat,ierror
      call countvectorsandfeatures(ierror,ibsize,nfeat)
      return
      end

      subroutine mtgettimepoints0(timepoints,periods,steps,moves,
     1                            numtimepoints)
      implicit none
!DEC$ attributes dllexport::mtgettimepoints0
      integer numtimepoints
      real timepoints(numtimepoints)
	integer periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
      call gettimepoints(timepoints,periods,steps,moves,numtimepoints)
      return
      end

      subroutine mtgetscalars0(ierror,a,istep)
      implicit  none
!DEC$ attributes dllexport :: mtgetscalars0
	integer   ierror,istep
      real a(*)
      call getscalars(ierror,a,istep)
	return
	end

      subroutine mtgetvectorsandfeatures0(update,a,istep,ibnode)
      implicit  none
!DEC$ attributes dllexport :: mtgetvectorsandfeatures0
      integer   update,istep
      integer ibnode(*)
      real a(*)
	call getvectorsandfeatures(update,a,istep,ibnode)
      return
      end

      subroutine mtcleanup0
      implicit none
!DEC$ attributes dllexport::mtcleanup0
      call cleanup
      return
      end
