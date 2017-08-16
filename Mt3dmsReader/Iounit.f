      subroutine fortopen(ierror,iunit,fmt,filename)
c*****opens a unit for reading
c     ierror    returns 0 if no error, 1 if unable to open file
c     iunit      file unit
c     fmt        set to 0 if unformatted file, set to 1 if formatted
c     filename   name of file to be opened
      implicit none
      integer iunit,fmt,ierror
      character*80 filename
	call fortclose(iunit)
      if (fmt.eq.0) then
        open(unit=iunit,file=filename,form='unformatted',err=99,
     &       status='old',action='read')
      else
        open(unit=iunit,file=filename,form='formatted',err=99,
     &       status='old',action='read')
      end if
      ierror=0
      return
 99   ierror=1
      return
      end

      subroutine fortclose(iunit)
      implicit none
c*****close the io unit
      integer iunit
      logical isopen
      inquire (unit=iunit,opened=isopen,err=99)
      if (isopen) close(iunit)
 99   return
      end

      subroutine fortrewind(iunit)
      implicit none
c*****rewinds the io unit
      integer iunit
      logical isopen
      inquire (unit=iunit,opened=isopen)
      if (isopen) rewind(iunit,err=99)
 99   return
      end
