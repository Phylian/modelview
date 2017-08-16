      module mfcommonmodule
	  real SubColumnOffset, SubRowOffset
	  integer IXSEC
      end module mfcommonmodule

	module simtime
      integer cperiod, ctimestep, vperiod, vtimestep
	end module simtime

	module timepointlist
	type timestruct
	   real::time
	   integer::period
	   integer::step
	   integer::move
	   type(timestruct),pointer::next_time
	end type timestruct
	type (timestruct), pointer :: begin
	integer size
	contains
	   subroutine clear
	   type(timestruct),pointer::traverse
	   type(timestruct),pointer::temp
         if (size.eq.0) return
         traverse=>begin
         do i=1,size
            temp=>traverse%next_time
            deallocate(traverse)
            traverse=>temp
         end do
         size=0
	   nullify(begin)
	   end subroutine clear
	end module timepointlist

      subroutine countheads(ierror,iunit,ncol,nrow,nlay,
     1                      ndatasets,datatype)
	use mfcommonmodule
	use timepointlist
      implicit none
	integer, intent(in) :: iunit,ncol,nrow,nlay
	integer, intent(out) :: ierror, ndatasets
	character*17, intent(out) ::datatype
c     locally defined variables
      character*16 text,text0
      character*20 fmt
      integer i,j,k,kstp,kper,nc,nr,ilay,ncr,iu,kmax
      logical binary,docheck
      real pertim,totim
	type (timestruct), pointer :: traverse
      real, dimension(:), allocatable :: buff
	integer ibsdim
      real IBSNULL,hhnoflo,hhdry
      common /mfcommon/ IBSDIM,IBSNULL,hhnoflo,hhdry
c     --------------------------------------------
      if (iunit.lt.0) then
	  binary=.false.
	  iu=-iunit
	else
	  binary=.true.
	  iu=iunit
	end if
      ndatasets=0
	text0=' '
	if (IXSEC .eq. 1) then
 	  ncr = ncol*nlay
      else
 	  ncr = ncol*nrow
	endif
      allocate(buff(ncr))
c     If begin has been allocated then we check the
c     period and time step against the previously saved values
	if (associated(begin)) then
	  docheck=.true.
	  traverse=>begin
	else
	  docheck=.false.
	end if
 10   continue
      k=0
	if (IXSEC .eq. 1) then
	  kmax=1  
	else
	  kmax=nlay  
	endif
	
 20   continue
        k=k+1
	  if (.not.binary) then
           read(iu,800,end=80,err=90) kstp,kper,pertim,totim,
     &                            text,nc,nr,ilay,fmt
	     if (IXSEC .eq. 1) then
	       if (nc.ne.ncol.or.nr.ne.nlay.or.ilay.ne.-1) goto 90
	       do i=1,nr
               read(iu,fmt,end=80,err=90) (buff(j),j=1,nc)
 	       end do
	     else
	       if (nc.ne.ncol.or.nr.ne.nrow.or.ilay.ne.k) goto 90
	       do i=1,nr
               read(iu,fmt,end=80,err=90) (buff(j),j=1,nc)
 	       end do
	     endif
	  else
           read(iu,end=80,err=90) kstp,kper,pertim,totim,
     &                            text,nc,nr,ilay
	     if (IXSEC .eq. 1) then
	       if (nc.ne.ncol.or.nr.ne.nlay) goto 90
	       if (text.eq.'      COMPACTION'.or.
     &           text.eq.'   CRITICAL HEAD') then
                 kmax=ibsdim
	       else if (ilay.ne.-1) then
               goto 90
	       end if
             read(iu,end=80,err=90) (buff(i),i=1,ncr)
	     else
	       if (nc.ne.ncol.or.nr.ne.nrow) goto 90
	       if (text.eq.'      COMPACTION'.or.
     &           text.eq.'   CRITICAL HEAD') then
                 kmax=ibsdim
	       else if (ilay.ne.k) then
               goto 90
	       end if
             read(iu,end=80,err=90) (buff(i),i=1,ncr)
	     endif
	  end if
	  if (text0.eq.' ') then
          text0=text
          datatype=text
	  else if (text.ne.text0) then
          goto 90
	  end if
      if (k.lt.kmax) goto 20
      ndatasets=ndatasets+1
	if (docheck) then
        if (traverse%period.ne.kper.or.traverse%step.ne.kstp) then
	     ierror=102
	     goto 100
	  end if
	  if (ndatasets.eq.size) then
	     ierror=0
           goto 100
	  end if
	  traverse=>traverse%next_time
	else
	   if (size.eq.0) then
            allocate(begin)
	      size=1
	      traverse=>begin
	   else
            allocate(traverse%next_time)
	      size=size+1
	      traverse=>traverse%next_time
	   end if
	   traverse%time=totim
	   traverse%period=kper
	   traverse%step=kstp
	   traverse%move=0
	   nullify(traverse%next_time)
	end if
      goto 10
 80   if (docheck.and.(ndatasets.ne.size)) then
	   ierror=102
	else
         ierror=0
	end if
      goto 100
 90   ierror=101
      if (.not.docheck) then
         call clear
	end if
 100  deallocate(buff)
      rewind(iu)
 99   return
 800  format(1x,2i5,1p,2e15.6,1x,a,3i6,1x,a)
      end

      subroutine getheads(ierror,a,kper,iunit,istep,
     1                    ncol,nrow,nlay,nbotm,laycbd)
	use mfcommonmodule
      implicit none
	integer, intent(in) :: iunit,istep,ncol,nrow,nlay,
     1                       nbotm,laycbd(nlay)
	integer, intent(out) :: ierror,kper
	real, intent(out) ::  a(*)
c     locally defined variables
      character text*16
      character*20 fmt
      integer   j,i,k,m,kstp,nc,nr,ilay,ncr,kk,offset,iu,kmax
	logical binary
      real pertim,totim
      real, dimension(:,:,:), allocatable :: buff
	integer ibsdim
      real IBSNULL,hhnoflo,hhdry
      common /mfcommon/ IBSDIM,IBSNULL,hhnoflo,hhdry
c     -------------------------------------------
      if (iunit.lt.0) then
	  binary=.false.
	  iu=-iunit
	else
	  binary=.true.
	  iu=iunit
	end if
      ncr = ncol*nrow
      allocate(buff(ncol,nrow,nlay))
c     rewind if we are not reading the next time step
      if (istep.ne.-1) then
	  rewind(iu)
	end if
c     skip ahead if necessary
      if (istep.gt.0) then
        do m=0,istep-1
	    if (.not.binary) then
            if (IXSEC .eq. 1) then
                read(iu,800,end=90,err=90) kstp,kper,pertim,totim,
     &                                text,nc,nr,ilay,fmt
	          do k=1,nlay
                  read(iu,fmt,end=90,err=90) (buff(j,1,k),j=1,nc)
  	          end do
	      else
              do k=1,nlay
                read(iu,800,end=90,err=90) kstp,kper,pertim,totim,
     &                                text,nc,nr,ilay,fmt
	          do i=1,nr
                  read(iu,fmt,end=90,err=90) (buff(j,i,k),j=1,nc)
  	          end do
              end do
	      endif
	    else
	      k=0
	      kmax=nlay
20          k=k+1
              read(iu,end=90,err=90) kstp,kper,pertim,totim,
     &                                text,nc,nr,ilay
            if (text.eq.'      COMPACTION'.or.
     &          text.eq.'   CRITICAL HEAD') kmax=ibsdim
            if (IXSEC .eq. 1) then
              read(iu,end=90,err=90) ((buff(j,1,k),j=1,nc),k=1,kmax)
	      else
              read(iu,end=90,err=90) ((buff(j,i,k),j=1,nc),i=1,nr)
	      endif
            if (k.lt.kmax) goto 20
	    end if
        end do
      end if
c     read data
	buff=IBSNULL
	if (.not.binary) then
        if (IXSEC .eq. 1) then
            read(iu,800,end=90,err=90) kstp,kper,pertim,totim,
     &                            text,nc,nr,ilay,fmt
	      do k=1,nlay
              read(iu,fmt,end=90,err=90) (buff(j,1,k),j=1,nc)
	      end do
	  else
          do k=1,nlay
            read(iu,800,end=90,err=90) kstp,kper,pertim,totim,
     &                            text,nc,nr,ilay,fmt
	      do i=1,nr
              read(iu,fmt,end=90,err=90) (buff(j,i,k),j=1,nc)
	      end do
          end do
	  endif
      else
	  k=0
        kmax=nlay
40      k=k+1
          read(iu,end=90,err=90) kstp,kper,pertim,totim,text,
     &                            nc,nr,ilay
	    if ((text.eq.'      COMPACTION'.or.
     &        text.eq.'   CRITICAL HEAD')) then
            kmax=ibsdim
	    end if
	    if (IXSEC .eq. 1) then
            read(iu,end=90,err=90) ((buff(j,1,k),j=1,nc),k=1,kmax)
		else
            read(iu,end=90,err=90) ((buff(j,i,ilay),j=1,nc),i=1,nr)
		endif
        if (k.lt.kmax) goto 40
	end if
c     reorder the data
	kk=0
      do k=1,nlay
	  kk = kk + 1
	  offset = (nbotm-kk)*ncr;
        do i=1,nrow
          do j=1,ncol
            a(offset + (nrow-i)*ncol + j) = buff(j,i,k)
          end do
        end do
	  if (laycbd(k).ne.0) then
          kk = kk + 1
	    offset = (nbotm-kk)*ncr;
          do i=1,nrow
            do j=1,ncol
	        if (buff(j,i,k).eq.hhnoflo.or.
     &            buff(j,i,k).eq.hhdry.or.
     &            buff(j,i,k+1).eq.hhnoflo.or.
     &            buff(j,i,k+1).eq.hhdry) then
                a(offset + (nrow-i)*ncol + j) = hhnoflo
	        else
                a(offset + (nrow-i)*ncol + j) = 
     &              (buff(j,i,k) + buff(j,i,k+1))/2
	        end if
            end do
          end do
	  end if
      end do
      ierror=0
	goto 100
 90   ierror=1
 100  deallocate(buff)
      return
 800  format(1x,2i5,1p,2e15.6,1x,a,3i6,1x,a)
      end

      subroutine gettimepoints(timepoints, periods, steps,moves, 
     1          numtimepoints)
	use timepointlist
      implicit none
	integer, intent(in) :: numtimepoints
	real, intent(out) :: timepoints(numtimepoints)
	integer,intent(out)::periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
	integer i
	type (timestruct), pointer :: traverse
	traverse=>begin
      do i=1,numtimepoints
         timepoints(i)=traverse%time
	   periods(i)=traverse%period
	   steps(i)=traverse%step
	   moves(i)=traverse%move
         traverse=>traverse%next_time
	end do
	call clear
	return
	end

      subroutine countcbc(ierror,iu,ncol,nrow,nlay)
	use timepointlist
      implicit none
	integer, intent(in) :: iu,ncol,nrow,nlay
	integer, intent(out) :: ierror
c     locally defined variables
      character*16 text,auxtxt
      integer  i,j,k,kstp,kper,nc,nr,nl,ncr,ncrl,method,nlist
	integer  naux1,naux,n,ndatasets
	real pertim,q,totim,delt,val
	logical vx, vy, vz
      real, dimension(:), allocatable :: buff
      integer, dimension(:), allocatable :: ibuff
	type (timestruct), pointer :: traverse
c     --------------------------------------------
	if (.not.associated(begin)) then
	  ierror=1
	  return
	end if
	traverse=>begin
	ncr=ncol*nrow
      ncrl=ncol*nrow*nlay
      allocate(buff(ncrl))
      allocate(ibuff(ncr))
	vx = .false.
	vy = .false.
	vz = .false.
      ndatasets=0
 10   continue
        read(iu,end=90,err=90) kstp,kper,text,nc,nr,nl
	  if (nc.ne.ncol.or.nr.ne.nrow.or.abs(nl).ne.nlay) goto 90
        method=0
	  if (nl.lt.0) then
	    nl = -nl
          read(iu,end=90,err=90) method,delt,pertim,totim
	  end if
	  if (method.le.1) then
          read(iu,end=90,err=90) (buff(i),i=1,ncrl)
        else if (method.eq.2) then
          read(iu,end=90,err=90) nlist
          do i=1,nlist
            read(iu,end=90,err=90) k,q
	    end do
	  else if (method.eq.3) then
          read(iu,end=90,err=90) (ibuff(i),i=1,ncr)
          read(iu,end=90,err=90) (buff(i),i=1,ncr)
	  else if (method.eq.4) then
          read(iu,end=90,err=90) (buff(i),i=1,ncr)
	  else if (method.eq.5) then
          read(iu,end=90,err=90) naux1
	    naux=naux1-1
	    if (naux.gt.0) then
            read(iu,end=90,err=90) (auxtxt,n=1,naux)
	    end if
	    read(iu,end=90,err=90) nlist
	    do i=1,nlist
            if (naux.gt.0) then
              read(iu,end=90,err=90) k,q,(val,n=1,naux)
	      else
              read(iu,end=90,err=90) k,q
	      end if
          enddo
	  end if
        if (adjustl(text).eq.'FLOW RIGHT FACE') then
          vx = .true.
	  else if (adjustl(text).eq.'FLOW FRONT FACE') then
          vy = .true.
	  else if (adjustl(text).eq.'FLOW LOWER FACE') then
	    vz = .true.
	  end if
        if ((vx.or.nc.eq.1).and.(vy.or.nr.eq.1).and.
     &      (vz.or.nl.eq.1)) then
          ndatasets=ndatasets+1
	    vx = .false.
	    vy = .false.
	    vz = .false.
c         Check that the period and time step of the cbc data are
c         identical to those of the flow data
          if (traverse%period.ne.kper.or.traverse%step.ne.kstp) then
	       ierror=1
	       goto 100
	    end if
	    if (ndatasets.eq.size) then
	       ierror=0
	       goto 100
	    end if
	    traverse=>traverse%next_time
	  end if
      goto 10
 90   ierror=1
 100  deallocate(buff)
      deallocate(ibuff)
	rewind(iu)
 99   return
      end

      subroutine getcbc(ierror,iu,a,istep,ncol,nrow,nlay,
     1                    nbotm,laycbd)
      implicit  none
	integer, intent(in) :: iu,istep,ncol,nrow,nlay,nbotm,
     1                       laycbd(nlay)
	integer, intent(out) :: ierror
	real, intent(out) ::  a(*)
c     locally defined variables
      character*16 text,auxtxt
      integer   ncr,m, offset, d
      integer   j,i,k,l,method,nlist,kstp,kper,nc,nr,nl,kk
	integer  naux1,naux,n,count
	real pertim,q,totim,delt,val
      real, dimension(:,:,:), allocatable :: buff
      integer, dimension(:,:), allocatable :: ibuff
	logical vx, vy, vz
c     --------------------------------------------
      ncr = ncol*nrow
      allocate(buff(ncol,nrow,nlay))
      allocate(ibuff(ncol,nrow))
c     rewind if we are not reading the next time step
      if (istep.ne.-1) then
	  rewind(iu)
	end if
	vx = .false.
	vy = .false.
	vz = .false.
c     skip ahead if needed
	if (istep.gt.0) then
        count=0
10      method=0
        read(iu,end=90,err=90) kstp,kper,text,nc,nr,nl
	  if (nl.lt.0) then
	    nl = - nl
          read(iu,end=90,err=90) method,delt,pertim,totim
	  end if
	  if (method.le.1) then
          read(iu,end=90,err=90) (((buff(j,i,k),j=1,nc),
     &                                  i=1,nr), k=1,nl)
        else if (method.eq.2) then
          read(iu,end=90,err=90) nlist
          do i=1,nlist
            read(iu,end=90,err=90) k,q
	    end do
	  else if (method.eq.3) then
          read(iu,end=90,err=90) ((ibuff(j,i),j=1,nc),i=1,nr)
          read(iu,end=90,err=90) ((buff(j,i,1),j=1,nc),i=1,nr)
	  else if (method.eq.4) then
          read(iu,end=90,err=90) ((buff(j,i,1),j=1,nc),i=1,nr)
	  else if (method.eq.5) then
          read(iu,end=90,err=90) naux1
	    naux=naux1-1
	    if (naux.gt.0) then
            read(iu,end=90,err=90) (auxtxt,n=1,naux)
	    end if
	    read(iu,end=90,err=90) nlist
	    do i=1,nlist
            if (naux.gt.0) then
              read(iu,end=90,err=90) k,q,(val,n=1,naux)
	      else
              read(iu,end=90,err=90) k,q
	      end if
          enddo
	  end if
        if (adjustl(text).eq.'FLOW RIGHT FACE') then
          vx = .true.
	  else if (adjustl(text).eq.'FLOW FRONT FACE') then
          vy = .true.
	  else if (adjustl(text).eq.'FLOW LOWER FACE') then
	    vz = .true.
	  end if
        if ((vx.or.nc.eq.1).and.(vy.or.nr.eq.1).and.
     &      (vz.or.nl.eq.1)) then
          count=count+1
	    vx = .false.
	    vy = .false.
	    vz = .false.
	  end if
	  if (count.lt.istep+1) goto 10
	end if
c     zero out the a array. This is needed for 2 or 1 dimensional
c     problems because flow across some faces are not saved
      do i=1,ncr*nlay*3
	  a(i)=0
	end do
c     read flow for the specified step
20	method=0
      read(iu,end=90,err=90) kstp,kper,text,nc,nr,nl
	if (nl.lt.0) then
	  nl = - nl
        read(iu,end=90,err=90) method,delt,pertim,totim
	end if
	if (method.le.1) then
        read(iu,end=90,err=90) (((buff(j,i,k),j=1,nc),i=1,nr),
     &                              k=1,nl)
        if (adjustl(text).eq.'FLOW RIGHT FACE') then
	    kk=0
          do k=1,nl
	      kk=kk+1
            do i=1,nr
              do j=1,nc
	          offset = 3*((nbotm-kk)*ncr + (nr-i)*nc + j-1)
                a(offset + 1) = buff(j,i,k)
              end do
            end do
	      if (laycbd(k).ne.0) then
	        kk=kk+1
              do i=1,nr
                do j=1,nc
	            offset = 3*((nbotm-kk)*ncr + (nr-i)*nc + j-1)
                  a(offset + 1) = 0
                end do
              end do
            endif
          end do
          vx = .true.
	  else if (adjustl(text).eq.'FLOW FRONT FACE') then
	    kk=0
          do k=1,nl
	      kk=kk+1
            do i=1,nr
              do j=1,nc
	          offset = 3*((nbotm-kk)*ncr + (nr-i)*nc + j-1)
                a(offset + 2) = -buff(j,i,k)
              end do
            end do
	      if (laycbd(k).ne.0) then
	        kk=kk+1
              do i=1,nr
                do j=1,nc
	            offset = 3*((nbotm-kk)*ncr + (nr-i)*nc + j-1)
                  a(offset + 2) = 0
                end do
              end do
            endif
          end do
          vy = .true.
	  else if (adjustl(text).eq.'FLOW LOWER FACE') then
	    kk=0
          do k=1,nl
	      kk=kk+1
            do i=1,nr
              do j=1,nc
	          offset = 3*((nbotm-kk)*ncr + (nr-i)*nc + j-1)
                a(offset + 3) = -buff(j,i,k)
              end do
            end do
	      if (laycbd(k).ne.0) then
	        kk=kk+1
              do i=1,nr
                do j=1,nc
	            offset = 3*((nbotm-kk)*ncr + (nr-i)*nc + j-1)
                  a(offset + 3) = -buff(j,i,k)
                end do
              end do
            endif
          end do
          vz = .true.
	  end if
      else if (method.eq.2) then
        read(iu,end=90,err=90) nlist
        do i=1,nlist
          read(iu,end=90,err=90) k,q
	  end do
	else if (method.eq.3) then
        read(iu,end=90,err=90) ((ibuff(j,i),j=1,nc),i=1,nr)
        read(iu,end=90,err=90) ((buff(j,i,1),j=1,nc),i=1,nr)
	else if (method.eq.4) then
        read(iu,end=90,err=90) ((buff(j,i,1),j=1,nc),i=1,nr)
	else if (method.eq.5) then
        read(iu,end=90,err=90) naux1
	  naux=naux1-1
	  if (naux.gt.0) then
          read(iu,end=90,err=90) (auxtxt,n=1,naux)
	  end if
	  read(iu,end=90,err=90) nlist
	  do i=1,nlist
          if (naux.gt.0) then
            read(iu,end=90,err=90) k,q,(val,n=1,naux)
	    else
            read(iu,end=90,err=90) k,q
	    end if
        enddo
	end if
      if ((vx.or.nc.eq.1).and.(vy.or.nr.eq.1).and.
     &    (vz.or.nl.eq.1)) then
        ierror=0
	  goto 100
      else 
        goto 20
	end if
 90   ierror=1
 100  deallocate(buff)
      deallocate(ibuff)
      return
      end

      subroutine countconc(ierror,iunit,ncol,nrow,nlay,
     1                      ndatasets,datatype)
	use mfcommonmodule
	use timepointlist
      implicit none
	integer, intent(in) :: iunit,ncol,nrow,nlay
	integer, intent(out) :: ierror, ndatasets
	character*17, intent(out) ::datatype
c     locally defined variables
	character*16 text,text0
      integer i,j,k,imov,kstp,kper,nc,nr,ilay,ncr,iu
      real pertim,totim,sumtch,buff
	logical binary
	type (timestruct), pointer :: traverse
c     --------------------------------------------
      if (iunit.lt.0) then
	  binary=.false.
	  iu=-iunit
	else
	  binary=.true.
	  iu=iunit
	end if
      ndatasets=0
	text0=' '
      ncr = ncol*nrow
	if (size.ne.0) call clear
 10   continue
      do k=1,nlay
	  if (.not.binary) then
             read(iu,800,end=80,err=90) text,ilay,imov,kstp,kper,sumtch
             read(iu,*,end=80,err=90) (buff,i=1,ncr)
	  else
           read(iu,end=80,err=90) kstp,kper,sumtch,totim,
     &                            text,nc,nr,ilay
	     if (nc.ne.ncol.or.nr.ne.nrow) goto 90
           read(iu,end=80,err=90) (buff,i=1,ncr)
	  end if
	  if (text0.eq.' ') then
          text0=text
          datatype=text
	  else if (text.ne.text0) then
          goto 90
	  end if
      end do
      ndatasets=ndatasets+1
	if (size.eq.0) then
	   allocate(begin)
	   size=1
	   traverse=>begin
	else
         allocate(traverse%next_time)
	   size=size+1
	   traverse=>traverse%next_time
      end if 
c     the next check is to fix a bug in moc3d version 3.5
c     because sumtch for initial conc is not properly saved 
      if ((kper.eq.0).and.(kstp.eq.0).and.(imov.eq.0)) sumtch = 0 
	traverse%time=sumtch
	traverse%period=kper
	traverse%step=kstp
	traverse%move=imov
	nullify(traverse%next_time)
      goto 10
 80   ierror=0
	goto 100
 90   ierror=101
      call clear
 100  rewind(iu)
 99   return
 800  format(a13,27x,i5,7x,i5,7x,i5,7x,i5,9x,f10.0)
	end

      subroutine getconc(ierror,a,kper,iunit,istep,
     1                    ncol,nrow,nlay)
	use mfcommonmodule
	use simtime
      implicit none
	integer, intent(in) :: iunit,istep,ncol,nrow,nlay
	integer, intent(out) :: ierror,kper
	real, intent(out) ::  a(*)
	character*16 text
      integer   j,i,k,m,kstp,nc,nr,ilay,ncr,kk,offset,iu
      real pertim,totim,sumtch
      real, dimension(:,:,:), allocatable :: buff
	logical binary
c     -------------------------------------------
      if (iunit.lt.0) then
	  binary=.false.
	  iu=-iunit
	else
	  binary=.true.
	  iu=iunit
	end if
      ncr = ncol*nrow
      allocate(buff(ncol,nrow,nlay))
c     rewind if we are not reading the next time step
      if (istep.ne.-1) then
	  rewind(iu)
	end if
c     skip ahead if necessary
      if (istep.gt.0) then
        do m=0,istep-1
	    if (.not.binary) then
            do k=1,nlay
              read(iu,800,end=90,err=90) kstp,kper,sumtch
	        do i=1,nrow
                read(iu,*,end=90,err=90) (buff(j,i,k),j=1,ncol)
	        end do
            end do
	    else
            do k=1,nlay
              read(iu,end=90,err=90) kstp,kper,sumtch,totim,
     &                                text,nc,nr,ilay
              read(iu,end=90,err=90) ((buff(j,i,k),j=1,nc),i=1,nr)
            end do
	    end if
        end do
      end if
c     read data
	if (.not.binary) then
        do k=1,nlay
          read(iu,800,end=90,err=90) kstp,kper,sumtch
	    do i=1,nrow
            read(iu,*,end=90,err=90) (buff(j,i,k),j=1,ncol)
	    end do
        end do
      else
        do k=1,nlay
          read(iu,end=90,err=90) kstp,kper,sumtch,totim,text,
     &                            nc,nr,ilay
          read(iu,end=90,err=90) ((buff(j,i,k),j=1,nc),i=1,nr)
        end do
	end if
c     reorder the data
      do k=1,nlay
	  offset = (nlay-k)*ncr;
        do i=1,nrow
          do j=1,ncol
            a(offset + (nrow-i)*ncol + j) = buff(j,i,k)
          end do
        end do
      end do
	cperiod = kper
	ctimestep = kstp
      ierror=0
	goto 100
 90   ierror=1
 100  deallocate(buff)
      return
 800  format(64x,i5,7x,i5,9x,f10.0)
      end

      subroutine countvel(ierror,iunit,ncol,nrow,nlay)
	use timepointlist
	use simtime
      implicit none
	integer, intent(in) :: iunit,ncol,nrow,nlay
	integer, intent(out) :: ierror
	character*16 text
      integer i,j,k,m,kstp,kper,nc,nr,ilay,ncr,iu
      real pertim,totim,sumtch,buff
	logical binary
	type (timestruct), pointer :: traverse
c     -------------------------------------------
c     determine if data file is binary or text
      if (iunit.lt.0) then
	  binary=.false.
	  iu=-iunit
	else
	  binary=.true.
	  iu=iunit
	end if
c     initialize variables
      vperiod = 0
	vtimestep = 0
      ncr = ncol*nrow
c     set the traverse pointer to the beginning of the time point list
	traverse=>begin
c     if the first period is zero, advance to the next
	if (traverse%period.eq.0) then
	  traverse=>traverse%next_time
	end if
10    continue
      do k=1,nlay
	  if (.not.binary) then
	    do m=1,3
            read(iu,20,end=90,err=90) ilay,kstp,kper,totim
            read(iu,*,end=90,err=90) (buff,j=1,ncr)
	    end do
        else
	    do m=1,3
            read(iu,end=90,err=90) kstp,kper,pertim,totim,text,
     &                            nc,nr,ilay
	      if (nc.ne.ncol.or.nr.ne.nrow) goto 90
            read(iu,end=90,err=90) (buff,j=1,ncr)
          end do
        end if
      end do
c     if the velocity data are saved at the period/time step later
c     than the concentration data, then we have a mismatch.
      if (.not.Associated(traverse).or.(kper.gt.traverse%period).or.
     1    (kper.eq.traverse%period.and.kstp.gt.traverse%step)) then
	  ierror=1
	  return
	end if
c     if the velocity data are saved at the period/time step earlier
c     than the concentration data, then read the next velocity data.
      if (traverse%period.ne.kper.or.traverse%step.ne.kstp) then
	  goto 10
	end if
c     if executation reaches this point, we have a period/time step 
c     match. If we are at the end of the time poinst list, we are done
 70	if (.not.associated(traverse%next_time)) then
        ierror=0
	  rewind(iu)
	  return
	else
c       move the traverse pointer ahead if the period/time step are
c       both same (only imove changes)
        if (traverse%next_time%period.eq.kper.and.
     1      traverse%next_time%step.eq.kstp) then
          traverse=>traverse%next_time
	    goto 70
	  end if
	end if
c     if executation reaches here, we found the next period/time step
      traverse=>traverse%next_time
	goto 10
 90   ierror=1
      return
 20   format(52x,i5,7x,i5,7x,i5,9x,f10.0)
	end

      subroutine getvel(ierror,iunit,a,istep,ncol,nrow,nlay)
	use simtime
      implicit  none
	integer, intent(in) :: iunit,istep,ncol,nrow,nlay
	integer, intent(out) :: ierror
	real, intent(out) ::  a(*)
	character*16 text
      integer   j,i,k,m,ncr,dir,nc,nr,ilay,index,kstp,kper,iu
	real totim,pertim
	character*20 fmt
	logical binary
      real, allocatable :: v(:,:,:,:)
c     -------------------------------------------
c     determine if we are reading text or binary data
      if (iunit.lt.0) then
	  binary=.false.
	  iu=-iunit
	else
	  binary=.true.
	  iu=iunit
	end if
c     if concentration is for period zero (initial conc), then
c     set velocity to zero
      if (cperiod.eq.0) then
	  do i=1,ncol*nrow*nlay*3
	    a(i)=0
	  end do
	  vperiod = 0
	  vtimestep = 0
	  rewind(iu)
        ierror=0
	  return
	end if
c     if concentration data are for the same period and time step
c     then use the same velocity as before.
      if ((cperiod.eq.vperiod).and.(ctimestep.eq.vtimestep)) then
	   ierror=0
	   return
	end if
c     rewind if we are not reading the next time step
      if (istep.ne.-1) then
	  rewind(iu)
	end if
	ncr = ncol*nrow
      allocate(v(3,ncol,nrow,nlay))
10    if (.not.binary) then
        do k=1,nlay
	    do dir=1,3
          read(iu,20,end=90,err=90) fmt, kstp, kper
            do i=1,nrow
              read(iu,*,end=90,err=90) (v(dir,j,i,k),j=1,ncol)
	      end do
          end do
        end do
	else
	  do dir=1,3
          do k=1,nlay
          read(iu,end=90,err=90) kstp,kper,pertim,totim,text,
     &                            nc,nr,ilay
          read(iu,end=90,err=90) ((v(dir,j,i,k),j=1,ncol),i=1,nrow)
          end do
        end do
	end if
      if (kper.ne.cperiod.or.kstp.ne.ctimestep) goto 10
	vperiod = kper
	vtimestep = kstp
      do k=1,nlay
	  do i=1,nrow
	    do j=1,ncol
	      index = 3*((nlay-k)*ncr + (nrow-i)*ncol + j - 1);
            a(index+1) = v(1,j,i,k)
            a(index+2) = -v(2,j,i,k)
            a(index+3) = -v(3,j,i,k)
	    end do
	  end do
	end do
	ierror=0
      deallocate(v)
	return
 90   ierror=1
      deallocate(v)
  	return
20    format(29x, a7, 28x, i5, 7x, i5)
	end
c
c
c
      subroutine commonrewindparticlefile(inunit,ierror)
      implicit none
      integer,intent(in):: inunit
      integer,intent(out)::ierror
	logical isopen
      ierror = 0
	if (inunit.gt.0) then
        inquire (unit=inunit,opened=isopen)
        if (.not.isopen) then
          ierror=2
	    return
        end if
	  rewind(inunit,err=90)
      end if
	return
 90   ierror = 100
	return
	end subroutine commonrewindparticlefile
c
c
c
	subroutine commonreadparticlecount(inunit,binary,NP,ierror,istep)
      implicit none
      integer,intent(in):: inunit
	logical,intent(in):: binary
      integer,intent(out)::ierror,NP
      integer,intent(in)::istep
	real coordinates(3), concentrations
	integer size, IP, i 
	real TIMV
      logical isopen
	integer KPER,KSTP,IMOV
	real SUMTCH 
	ierror = 0
	if (inunit .eq.0) then
	  NP = 0
	  return
	endif
      inquire (unit=inunit,opened=isopen)
      if (.not.isopen) then
        ierror=2
	  return
      end if
c	rewind and skip ahead if required
	if (istep.ne.-1) then
	  call commonrewindparticlefile(inunit,ierror)
	  if (ierror.ne.0) then
	    return
	  end if
	  do i=0,istep-1
		if (binary) then
			read(inunit,err=90) IMOV,NP,TIMV,SUMTCH
			read(inunit,err=90) (coordinates(1),
	1		  coordinates(2),coordinates(3), 
     2          concentrations,IP=1,NP)
		else
			read(inunit,11,err=90) KPER,KSTP,IMOV,NP,TIMV,SUMTCH 
			read(inunit,12,err=90) (coordinates(1),
	1			coordinates(2),coordinates(3),
     2            concentrations,IP=1,NP)
  12			FORMAT(4E12.4)
		endif
	  end do
	end if
	if (binary) then
        read(inunit,err=90) IMOV,NP,TIMV,SUMTCH
	else
        read(inunit,11,err=90) KPER,KSTP,IMOV,NP,TIMV,SUMTCH 
  11    FORMAT(4I10,1PE12.4,1PE12.4)
	endif
	return
 90   ierror = 100
	return
	end subroutine commonreadparticlecount
c
c
c
	subroutine commonreadparticles(inunit,binary,NP,ierror,coord,
	1  scalars, NCOL,NROW,NLAY, ISCOL1,ISCOL2,ISROW1,ISROW2,
     2  ISLAY1,ISLAY2, delr, delc, elev)
	use mfcommonmodule
      implicit none
      integer,intent(inout)::NP
      integer,intent(out)::ierror
	real,intent(inout):: coord, scalars
      integer,intent(in):: inunit
	logical,intent(in):: binary
	dimension coord(NP*3), scalars(NP)
	integer, intent(in):: NCOL,NROW,NLAY, ISCOL1,ISCOL2,
	1  ISROW1,ISROW2,ISLAY1,ISLAY2
	real, intent(in):: delr, delc, elev
	dimension delr(NCOL), delc(NROW), elev(*)
	integer IP, IROW, ICOL
      logical isopen
	Integer Layer, ILAY, NewLayer, Row, Column
	Integer SLayer, SRow, SColumn
	integer nslay, nsrow, nscol, nscr, elevoffset
	real LayerOffset, RowOffset, ColumnOffset
	real XCoord, YCoord, temp
c	integer ZLayer
	dimension XCoord(NCOL+1), YCoord(NROW+1)
	integer itemp, indexB, IndexT
	integer NewNP, i
	ierror = 0
      inquire (unit=inunit,opened=isopen)
      if ((.not.isopen).or.(inunit.eq.0)) then
        ierror=2
	  return
      end if
	if (binary) then
        read(inunit,err=90) (coord(IP*3-2),coord(IP*3-1),coord(IP*3),
	1    scalars(IP),IP=1,NP)
	else
        read(inunit,12,err=90) (coord(IP*3-2),coord(IP*3-1),coord(IP*3),
	1    scalars(IP),IP=1,NP)
  12    FORMAT(4E12.4)
	endif

      i=0
	do IP=1, NP
	  i=i+1
	  if (int(coord(IP*3)+0.5).eq.0) then
	    i = i-1
	  elseif (i.ne.ip) then
	    coord(i*3-2) = coord(IP*3-2)
	    coord(i*3-1) = coord(IP*3-1)
	    coord(i*3)   = coord(IP*3)
	    scalars(i)   = scalars(IP)
	  endif
	enddo
	NP = i;

      nslay=islay2-islay1+1
      nsrow=isrow2-isrow1+1
      nscol=iscol2-iscol1+1
      nscr=nscol*nsrow

	temp = 0
	XCoord(1) = 0
	do ICOL =1,NSCOL
	  temp = temp + delr(ICOL)
	  XCoord(ICOL+1) = temp
	end do
	
	temp = 0
	YCoord(1) = 0
	do IROW =1,NSROW
	  temp = temp + DELC(NSROW-IROW+1)
	  YCoord(IROW+1) = temp
	end do

c	itemp = 0
c	do ILAY = ISLAY1,ISLAY2
c	  itemp = itemp + 1
c	  ZLayer(ILAY-ISLAY1+1) = itemp
c	end do

	do IP=1, NP
	  Layer = int(coord(IP*3)+0.5)
! Layeroffset should be from 0 to 1.
	  LayerOffset = -(coord(IP*3) - Layer)+0.5
c	  if (abs(fraction(coord(IP*3)+0.5)).eq.0)
	  SLayer = ISLAY2-Layer+1
c       SLayer is now the layer number counting from the bottom of the GWT subgrid up. 
c       SLayer = 1 in the new ordering is equivalent to ISLAY2 in the original order.
c       No confining beds are in the subgrid

	  Row = int(coord(IP*3-1)+0.5)
	  RowOffset = -(coord(IP*3-1) - Row)+0.5
	  if (Row.gt.ISROW2) then
	    Row = Row-1
	    RowOffset = RowOffset-1
	  endif
	  SRow = ISROW2-Row+1
c       SROW is the subrow of the GWT subgrid in reverse order as the original rows.

	  Column = int(coord(IP*3-2)+0.5)
	  ColumnOffset = coord(IP*3-2) - Column + 0.5
	  SColumn = Column-ISCOL1+1
c       SColumn is the subcolumn of the GWT subgrid in same order as the original columns.
        
	  indexB = (SLayer-1)*nscr + (SRow-1)*nscol + SColumn
c       IndexB is the position in elev of the bottom of the current cell
        indexT = indexB + nscr
c       indexT is the position in elev of the top of the current cell
         
c	  coord(IP*3-2) = XCoord(Column) + 
c	1    ColumnOffset*(XCoord(Column+1)-XCoord(Column))
c	   
c	  coord(IP*3-1) = YCoord(NRow+1-Row) + 
c	1    RowOffset*(YCoord(NRow+2-Row)-YCoord(NRow+1-Row))
c
c	  coord(IP*3) = elev(IndexB) 
c	1    + LayerOffset*(elev(IndexT)-elev(IndexB))

	  coord(IP*3-2) = XCoord(SColumn) + 
	1    ColumnOffset*(XCoord(SColumn+1)-XCoord(SColumn)) 
     2    + SubColumnOffset 
	   
	  coord(IP*3-1) = YCoord(SRow) + 
	1    RowOffset*(YCoord(SRow+1)-YCoord(SRow))
	2    + SubRowOffset

	  coord(IP*3) = elev(IndexB) 
	1    + LayerOffset*(elev(IndexT)-elev(IndexB))

	end do
	return
 90   ierror = 100
	return
	end subroutine commonreadparticles
c
c
c
