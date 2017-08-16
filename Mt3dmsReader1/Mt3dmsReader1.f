      subroutine mtdims1(ierror,cnfFile,ucnFile,ftlFile,unstruct,
     1     nc,nr,nl)
      implicit  none
      dll_export mtdims1
      integer ierror,nc,nr,nl,unstruct
      character*256 cnfFile,ucnFile,ftlFile
	call dims(ierror,cnfFile,ucnFile,ftlFile,unstruct,nc,nr,nl)
      return
	end

      subroutine mtgrid1(ierror,delr,delc,z,cinact)
      implicit none
      dll_export mtgrid1
      integer ierror
	real delr(*),delc(*),z(*),cinact
	call grid(ierror,delr,delc,z,cinact)
	return
	end

      subroutine mtcountscalars1(ierror,ndatasets,
     &                   datatype)
      implicit none
      dll_export mtcountscalars1
	integer  ierror,ndatasets
	character*17 datatype
	call countscalars(ierror,ndatasets,datatype)
      return
	end

      subroutine mtcountvectorsandfeatures1(ierror,ibsize,nfeat)
      implicit none
      dll_export mtcountvectorsandfeatures1
      integer ibsize,nfeat,ierror
      call countvectorsandfeatures(ierror,ibsize,nfeat)
      return
      end

      subroutine mtgettimepoints1(timepoints,periods,steps,moves,
     1                            numtimepoints)
      implicit none
      dll_export mtgettimepoints1
      integer numtimepoints
      real timepoints(numtimepoints)
	integer periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
      call gettimepoints(timepoints,periods,steps,moves,numtimepoints)
      return
      end

      subroutine mtgetscalars1(ierror,a,istep)
      implicit  none
      dll_export mtgetscalars1
	integer   ierror,istep
      real a(*)
      call getscalars(ierror,a,istep)
	return
	end

      subroutine mtgetvectorsandfeatures1(update,a,istep,ibnode)
      implicit  none
      dll_export mtgetvectorsandfeatures1
      integer   update,istep
      real a(*)
      integer ibnode(*)
	call getvectorsandfeatures(update,a,istep,ibnode)
      return
      end

      subroutine mtcleanup1
      implicit none
      dll_export mtcleanup1
      call cleanup
      return
      end
