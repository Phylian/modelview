      subroutine mtdims2(ierror,cnfFile,ucnFile,ftlFile,unstruct,
     1      nc,nr,nl)
      implicit  none
!DEC$ attributes dllexport :: mtdims2
      character*256 cnfFile,ucnFile,ftlFile
      integer ierror,nc,nr,nl,unstruct
	call dims(ierror,cnfFile,ucnFile,ftlFile,unstruct,nc,nr,nl)
      return
	end

      subroutine mtgrid2(ierror,delr,delc,z,cinact)
	implicit none
!DEC$ attributes dllexport :: mtgrid2
      integer ierror
	real delr(*),delc(*),z(*),cinact
	call grid(ierror,delr,delc,z,cinact)
	return
	end

	subroutine mtcountscalars2(ierror,ndatasets,datatype)
      implicit none
!DEC$ attributes dllexport :: mtcountscalars2
	integer  ierror,ndatasets
	character*17 datatype
	call countscalars(ierror,ndatasets,datatype)
      return
	end

      subroutine mtcountvectorsandfeatures2(ierror,ibsize,nfeat)
      implicit none
!DEC$ attributes dllexport::mtcountvectorsandfeatures2
      integer ibsize,nfeat,ierror
      call countvectorsandfeatures(ierror,ibsize,nfeat)
      return
      end

      subroutine mtgettimepoints2(timepoints,periods,steps,moves,
     1                            numtimepoints)
      implicit none
!DEC$ attributes dllexport::mtgettimepoints2
      integer numtimepoints
      real timepoints(numtimepoints)
	integer periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
      call gettimepoints(timepoints,periods,steps,moves,numtimepoints)
      return
      end

      subroutine mtgetscalars2(ierror,a,istep)
      implicit  none
!DEC$ attributes dllexport :: mtgetscalars2
	integer   ierror,istep
      real a(*)
      call getscalars(ierror,a,istep)
	return
	end

      subroutine mtgetvectorsandfeatures2(update,a,istep,ibnode)
      implicit  none
!DEC$ attributes dllexport :: mtgetvectorsandfeatures2
      integer   update,istep
      integer ibnode(*)
      real a(*)
	call getvectorsandfeatures(update,a,istep,ibnode)
      return
      end

      subroutine mtcleanup2
      implicit none
!DEC$ attributes dllexport::mtcleanup2
      call cleanup
      return
      end
