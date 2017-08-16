      module mtmodule
c     period = stress period for hydraulic data
c     flowstep = flow time step for hydraulic data
c     kper = stress period for concentration
c     kstp = flow time step for concentration
	integer period, flowstep, kper, kstp
	integer   mtchd,mtwel,mtdrn,mtrch,mtevt,mtriv,mtghb, 
     &          mtiss,mtnper,mtstr,mtres,mtfhb,mtdrt,mtets,
     &          mtibs,mttlk,mtlak,mtmaw,mtusr1,mtusr2,mtusr3
	integer maxchd,maxwel,maxdrn,maxriv,maxghb,maxstr,maxres,maxfhb,
     &          maxmnw
	integer cnfunit, ucnunit, ftlunit
      integer ncol,nrow,nlay,ibousz
! precision = -1 -> error
! precision =  0 -> single precision
! precision =  1 -> double precision
	integer precision
	end module mtmodule

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
	   if (.not.associated(begin)) then
	      size=0
	      return
	   end if
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

      subroutine dims(ierror,cnfFile,ucnFile,ftlFile,unstruct,
     1      nc,nr,nl)
      use mtmodule
	use timepointlist
      implicit  none
      integer,intent(in)::unstruct
      integer,intent(out)::ierror,nc,nr,nl
      character*256,intent(in)::cnfFile,ucnFile,ftlFile
c     locally defined variables
	character*20 fmtarg
c     -------------------
      cnfunit=10
	ucnunit=11
	ftlunit=12
	open (unit=cnfunit,file=cnfFile,form='formatted',
     1      action='read',err=92)
	if (unstruct.eq.1) then
         fmtarg='binary'
	else
	   fmtarg='unformatted'
	end if
	open (unit=ucnunit,file=ucnFile,form=fmtarg,
     1      action='read',err=93)
	if (ftlFile(1:1).ne.' ') then
	   open(unit=ftlunit,file=ftlFile,form=fmtarg,
     1        action='read',err=94)
      end if
      read(cnfunit,*,end=95,err=95) nlay,nrow,ncol
	nc=ncol
	nr=nrow
	nl=nlay
	ierror=0
	return
 92   ierror=2
      return
 93   ierror=3
	return
 94   ierror=4
	return
 95   ierror=5
      return
	end

      subroutine grid(ierror,delr,delc,z,cinact)
      use mtmodule
	implicit none
      integer,intent(out)::ierror
	real,intent(out)::delr(*),delc(*),z(*),cinact
c     locally defined variables
      integer ncr,roffset,zoffset,allocstat,i,j,k
	real, allocatable :: thick(:,:,:)
c     ----------------------
	ierror=0
c--read cell width along rows (or the x axis)
      read(cnfunit,*,end=90,err=90) (delr(j),j=1,ncol)
c--read cell width along columns (or the y axis) in reverse direction
      read(cnfunit,*,end=90,err=90) (delc(nrow-i+1),i=1,nrow)
c--read top elevation of 1st model layer into array using xyz indexing
      ncr=ncol*nrow
      zoffset=nlay*ncr
      read(cnfunit,*,end=90,err=90) ((z(zoffset+(nrow-i)*ncol+j),
     1                               j=1,ncol),i=1,nrow)
c--read cell thickness into array using modflow indexing
      allocate(thick(ncol,nrow,nlay),stat=allocstat)
      read(cnfunit,*,end=90,err=90) (((thick(j,i,k),j=1,ncol),
     1                               i=1,nrow),k=1,nlay)
c--read value at inactive cell
	read(cnfunit,*,end=90,err=90) cinact
c--calculate layer bottom elevations, using xyz indexing. work from
c--top to bottom
      do k=1,nlay
	  zoffset=(nlay-k)*ncr
	  do i=1,nrow
	    roffset=(nrow-i)*ncol+zoffset
	    do j=1,ncol
	      z(roffset+j) = z(roffset+j+ncr)-thick(j,i,k)
	    end do
	  end do
	end do
	goto 100
  90  ierror=5
 100  if (allocated(thick)) deallocate(thick)
      close(cnfunit)
	return
	end

	subroutine get_precision(ierror)
      use mtmodule
      implicit none
	integer, intent(out)::ierror
	integer i,k,ntrans,nc,nr,ilay,ncr
	real time
	real (KIND = 8) timedbl
	character*16 text
	ierror = 0
      read(ucnunit,err=90) ntrans,kstp,kper,time,text,
     &                            nc,nr,ilay
      if (nc.ne.ncol.or.nr.ne.nrow.or.ilay.ne.1) then
        rewind(ucnunit)
        read(ucnunit,err=90) ntrans,kstp,kper,timedbl,text,
     &                            nc,nr,ilay
        if (nc.ne.ncol.or.nr.ne.nrow.or.ilay.ne.1) then
          precision = -1
	    ierror = 1
	  else
          precision = 1
	  endif
	else
	  precision = 0
	endif
      rewind(ucnunit)
	return
 90   ierror=1
      precision = -1
      rewind(ucnunit)
      return
	end

	subroutine countscalars(ierror,ndatasets,datatype)
      use mtmodule
      implicit none
	integer, intent(out)::ierror,ndatasets
	character*17,intent(out)::datatype
	call get_precision(ierror)
	if (ierror.ne.0) return
	if (precision.eq.0) then
	  call countscalars_single(ierror,ndatasets,datatype)
	elseif (precision.eq.1) then
	  call countscalars_double(ierror,ndatasets,datatype)
	else
	  ierror = 1
	endif
	return
	end

	subroutine countscalars_single(ierror,ndatasets,datatype)
      use mtmodule
	use timepointlist
      implicit none
	integer, intent(out)::ierror,ndatasets
	character*17,intent(out)::datatype
c     locally defined variables
	integer i,k,ntrans,nc,nr,ilay,ncr
	real time,buff
	character*16 text
	type (timestruct), pointer :: traverse
c     -------------------
      call clear
	ierror=0
	ncr=ncol*nrow
 10   continue
      do k=1,nlay
        read(ucnunit,end=80,err=90) ntrans,kstp,kper,time,text,
     &                            nc,nr,ilay
	  if (nc.ne.ncol.or.nr.ne.nrow.or.ilay.ne.k) goto 90
        read(ucnunit,end=80,err=90) (buff,i=1,ncr)
      end do
	if (size.eq.0) then
        allocate(begin)
	  size=1
	  traverse=>begin
	else
        allocate(traverse%next_time)
	  size=size+1
	  traverse=>traverse%next_time
	end if
	datatype=text
	text=trim(adjustl(text))
	traverse%time=time
	if (text.eq.'HEAD'.or.text.eq.'DRAWDOWN') then
	  traverse%period=kstp
	  traverse%step=ntrans
	  traverse%move=0
	else
	  traverse%period=kper
	  traverse%step=kstp
	  traverse%move=ntrans
	end if
	nullify(traverse%next_time)
      goto 10
 80   ierror=0
      period=-1
	flowstep=-1
	ndatasets=size
      return
 90   ierror=1
      call clear
      return
	end

	subroutine countscalars_double(ierror,ndatasets,datatype)
      use mtmodule
	use timepointlist
      implicit none
	integer, intent(out)::ierror,ndatasets
	character*17,intent(out)::datatype
c     locally defined variables
	integer i,k,ntrans,nc,nr,ilay,ncr
	real (KIND = 8) time,buff
	character*16 text
	type (timestruct), pointer :: traverse
c     -------------------
      call clear
	ierror=0
	ncr=ncol*nrow
 10   continue
      do k=1,nlay
        read(ucnunit,end=80,err=90) ntrans,kstp,kper,time,text,
     &                            nc,nr,ilay
	  if (nc.ne.ncol.or.nr.ne.nrow.or.ilay.ne.k) goto 90
        read(ucnunit,end=80,err=90) (buff,i=1,ncr)
      end do
	if (size.eq.0) then
        allocate(begin)
	  size=1
	  traverse=>begin
	else
        allocate(traverse%next_time)
	  size=size+1
	  traverse=>traverse%next_time
	end if
	datatype=text
	text=trim(adjustl(text))
	traverse%time=time
	if (text.eq.'HEAD'.or.text.eq.'DRAWDOWN') then
	  traverse%period=kstp
	  traverse%step=ntrans
	  traverse%move=0
	else
	  traverse%period=kper
	  traverse%step=kstp
	  traverse%move=ntrans
	end if
	nullify(traverse%next_time)
      goto 10
 80   ierror=0
      period=-1
	flowstep=-1
	ndatasets=size
      return
 90   ierror=1
      call clear
      return
	end

      subroutine countvectorsandfeatures(ierror,ibsize,nfeat)
      use mtmodule
      implicit none
      integer,intent(out)::ierror,ibsize,nfeat
c     locally defined variables
      integer ncr,ncrl,j,i,k,m,nc,nr,nl,nq,ibuff,icount
	real q,buff
	integer IDWell
	real QSW
      character label*16,version*11
c     ---------------------
      nfeat=9
	ibsize=9
      ncr=ncol*nrow
	ncrl=ncr*nlay
	icount=0
c     read header information
      read(ftlunit,end=90,err=90) version,mtwel,mtdrn,mtrch,mtevt,
     &          mtriv,mtghb,mtchd,mtiss,mtnper
      if (version.eq.'MT3D4.00.00') then
         read(ftlunit,end=90,err=90) mtstr,mtres,mtfhb,mtdrt,mtets,
     &          mtibs,mttlk,mtlak,mtmaw,mtusr1,mtusr2,mtusr3
	else
         mtstr=0
         mtres=0
         mtfhb=0
         mtdrt=0
         mtets=0
         mtibs=0
         mttlk=0
         mtlak=0
         mtmaw=0
         mtusr1=0
         mtusr2=0
         mtusr3=0
	end if
	maxchd=0
	maxwel=0
	maxdrn=0
	maxriv=0
	maxghb=0
	maxstr=0
      maxres=0
      maxfhb=0
	maxmnw=0
10    continue
c     saturated thickness
      read(ftlunit,end=80,err=90) period,flowstep,nc,nr,nl,label
	if (nc.ne.ncol.or.nr.ne.nrow.or.nl.ne.nlay) goto 90
      read(ftlunit,end=90,err=90) (buff,i=1,ncrl)
c     x component of flow
	if (nc.gt.1) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (buff,i=1,ncrl)
	end if
c     y component of flow
	if (nr.gt.1) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (buff,i=1,ncrl)
	end if
c     z component of flow
	if (nl.gt.1) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (buff,i=1,ncrl)
	end if
c     storage
      if (mtiss.eq.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (buff,i=1,ncrl)
	end if
c     constant head (cells where ibound < 0)
      read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	if (nq.gt.0) then
        do m=1,nq
          read(ftlunit,end=90,err=90) k, i, j, q
	  end do
	  if (nq.gt.maxchd) maxchd=nq
	end if
c     wells
      if (mtwel.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
          end do
	    if (nq.gt.maxwel) maxwel=nq
	  end if
	end if
c     drains
      if (mtdrn.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
          end do
	    if (nq.gt.maxdrn) maxdrn=nq
	  end if
	end if
c     recharge
      if (mtrch.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (ibuff,i=1,ncr)
        read(ftlunit,end=90,err=90) (buff,i=1,ncr)
	end if
c     evapotranspiration
      if (mtevt.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (ibuff,i=1,ncr)
        read(ftlunit,end=90,err=90) (buff,i=1,ncr)
	end if
c     rivers
      if (mtriv.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
          end do
	    if (nq.gt.maxriv) maxriv=nq
	  end if
	end if
c     general head boundary
      if (mtghb.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
          end do
	    if (nq.gt.maxghb) maxghb=nq
	  end if
	end if
c     stream
      if (mtstr.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
          end do
	    if (nq.gt.maxstr) maxstr=nq
	  end if
	end if
c     reservoir
      if (mtres.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
          end do
	    if (nq.gt.maxres) maxres=nq
	  end if
	end if
c     fhb
      if (mtfhb.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
          end do
	    if (nq.gt.maxfhb) maxfhb=nq
	  end if
	end if
      if (mtmaw.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q, IDWell, QSW
          end do
	    if (nq.gt.maxmnw) maxmnw=nq
	  end if
	end if

      icount=icount+1
      goto 10
 80   if (icount.lt.1) goto 90
      ierror=0
      ibsize=9+maxchd+maxwel+maxdrn+maxriv+maxghb+maxstr+maxres+maxfhb
     &   +maxmnw
	ibousz=ibsize
	rewind(ftlunit)
      period=-1
	flowstep=-1
      return
 90   ierror=1
	return
	end

      subroutine gettimepoints(timepoints,periods,steps,moves,
     1                         numtimepoints)
	use timepointlist
      implicit none
	integer, intent(in) :: numtimepoints
	real, intent(out) :: timepoints(numtimepoints)
	integer,intent(out)::periods(numtimepoints),steps(numtimepoints),
     1   moves(numtimepoints)
c     locally defined variables
	integer i
	type (timestruct), pointer :: traverse
c     -----------
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

      subroutine getscalars(ierror,a,istep)
      use mtmodule
      implicit  none
	integer, intent(out) :: ierror
      integer, intent(in) :: istep
      real, intent(out) ::  a(*)
c     locally defined variables
      character text*16
	integer   ncr,j,i,k,m
      integer   ntrans,nc,nr,ilay
	real time
	real, dimension(:,:,:), allocatable :: buff
	real (kind=8) timedbl
	real (kind=8), dimension(:,:,:), allocatable :: buffdbl
c     --------
      if ((precision.ne.0).and.(precision.ne.1)) then
	  ierror = 1
	  return
	endif
	ncr = ncol*nrow
	if (precision.eq.0) then
        allocate(buff(ncol,nrow,nlay))
	else
	  allocate(buffdbl(ncol,nrow,nlay))
	endif
c     rewind if we are not reading the next time step
      if (istep.ne.-1) then
	  rewind(ucnunit)
	end if
c     skip ahead if necessary
	if (istep.gt.0) then
        do m=0,istep-1
          do k=1,nlay
	      if (precision.eq.0) then 
              read(ucnunit,end=90,err=90) ntrans,kstp,kper,time,text,
     &	                           nc,nr,ilay
              read(ucnunit,end=90,err=90) ((buff(j,i,k),j=1,ncol),
     $                                    i=1,nrow)
	      else
              read(ucnunit,end=90,err=90) ntrans,kstp,kper,timedbl,text,
     &	                           nc,nr,ilay
              read(ucnunit,end=90,err=90) ((buffdbl(j,i,k),j=1,ncol),
     $                                    i=1,nrow)
	      endif
          end do
	  end do
	end if
c     read data
      do k=1,nlay
        if (precision.eq.0) then 
          read(ucnunit,end=90,err=90) ntrans,kstp,kper,time,text,
     1                              nc,nr,ilay
          read(ucnunit,end=90,err=90) ((buff(j,i,k),j=1,ncol),i=1,nrow)
	  else
          read(ucnunit,end=90,err=90) ntrans,kstp,kper,timedbl,text,
     1                              nc,nr,ilay
          read(ucnunit,end=90,err=90) ((buffdbl(j,i,k),j=1,ncol),
	1	   i=1,nrow)
	  endif
      end do
c     reorder the data array
      do k=1,nlay
	  do i=1,nrow
	    do j=1,ncol 
	      if (precision.eq.0) then
              a((nlay-k)*ncr + (nrow-i)*ncol + j) = buff(j,i,k)
	      else
              a((nlay-k)*ncr + (nrow-i)*ncol + j) = buffdbl(j,i,k)
	      endif
	    end do
	  end do
	end do
	if (text.ne.'CONCENTRATION  ') then
        kper = kstp
	  kstp = ntrans
	end if
	ierror=0
	goto 100
 90   ierror=1
100	if (precision.eq.0) then
        deallocate(buff)
	else
	  deallocate(buffdbl)
	endif
	return
	end

      subroutine getvectorsandfeatures(update,array,istep,ibnode)
c*****Read flow components in the ftl file
      use mtmodule
      implicit  none
      interface
        integer function rcl2i(arow,acol,alayer)
          implicit none
          integer,intent(in)::arow,acol,alayer
        end function rcl2i
      end interface
	integer,intent(in)::istep
      integer,intent(out)::update,ibnode(*)
      real,intent(out)::array(*)
c     locally defined variables
      integer ncr,j,i,k,m,nc,nr,nl,offset,nq,position
	integer IDWell
	real q, QSW
      real,dimension(:,:,:),allocatable::buff
      integer,dimension(:,:),allocatable::ibuff
      character label*16, version*11
c     ----------------------
c     return if we don't need to update flow data
      if (kper.eq.period.and.kstp.eq.flowstep) then
        update=0
        return
	end if
	update=1
      ncr = ncol*nrow
      allocate(buff(ncol,nrow,nlay))
      allocate(ibuff(ncol,nrow))
      if (istep.ne.-1) then
	  rewind(ftlunit)
c       read header information for entire simulation
        read(ftlunit,end=90,err=90) version,mtwel,mtdrn,mtrch,mtevt,
     &                         mtriv,mtghb,mtchd,mtiss,mtnper
        if (version.eq.'MT3D4.00.00') then
           read(ftlunit,end=90,err=90) mtstr,mtres,mtfhb,mtdrt,mtets,
     &          mtibs,mttlk,mtlak,mtmaw,mtusr1,mtusr2,mtusr3
	  end if
	end if
10    continue
      position = 0
c     Fill the array with zeros
      do i=1, 3*ncol*nrow*nlay
	  array(i) = 0
	end do
	do i=1,ibousz
	  ibnode(i)=0
	end do
c     saturated thickness
      read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
      read(ftlunit,end=90,err=90) (((buff(j,i,k),j=1,ncol),i=1,nrow),
     &                              k=1,nlay)
c     x component of flow
	if (nc.gt.1) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (((buff(j,i,k),j=1,ncol),i=1,nrow),
     &                              k=1,nlay)
        do k=1,nlay
          do i=1,nrow
            do j=1,ncol
	        offset = 3 * ((nlay-k)*ncr + (nrow-i)*ncol + j - 1) + 1
              array(offset) = buff(j,i,k)
            end do
          end do
        end do
	end if
c     y component of flow
	if (nr.gt.1) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (((buff(j,i,k),j=1,ncol),i=1,nrow),
     &                              k=1,nlay)
        do k=1,nlay
          do i=1,nrow
            do j=1,ncol
	        offset = 3 * ((nlay-k)*ncr + (nrow-i)*ncol + j - 1) + 2
              array(offset) = -buff(j,i,k)
            end do
          end do
        end do
	end if
c     z component of flow
	if (nl.gt.1) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (((buff(j,i,k),j=1,ncol),i=1,nrow),
     &                              k=1,nlay)
        do k=1,nlay
          do i=1,nrow
            do j=1,ncol
	        offset = 3 * ((nlay-k)*ncr + (nrow-i)*ncol + j - 1) + 3
              array(offset) = -buff(j,i,k)
            end do
          end do
        end do
	end if
c     storage
      if (mtiss.eq.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) (((buff(j,i,k),j=1,ncol),i=1,nrow),
     &                              k=1,nlay)
	end if
c     constant head (ibound <0)
      read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	position=position+1
	ibnode(position)=nq
	if (nq.gt.0) then
        do m=1,nq
          read(ftlunit,end=90,err=90) k, i, j, q
	    position=position+1
	    ibnode(position)=rcl2i(i,j,k)
	  end do
	else if (maxchd.eq.0) then
	  ibnode(position)=-1
	end if
c     wells
      position=position+1
      if (mtwel.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  ibnode(position)=nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
	      position=position+1
	      ibnode(position)=rcl2i(i,j,k)
          end do
	  end if
	else
	  ibnode(position)=-1
	end if
c     drains
      position=position+1
      if (mtdrn.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  ibnode(position)=nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
	      position=position+1
	      ibnode(position)=rcl2i(i,j,k)
          end do
	  end if
	else
	  ibnode(position)=-1
	end if
c     recharge
      if (mtrch.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) ((ibuff(j,i),j=1,ncol),i=1,nrow)
        read(ftlunit,end=90,err=90) ((buff(j,i,1),j=1,ncol),i=1,nrow)
	end if
c     evapotranspiration
      if (mtevt.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label
        read(ftlunit,end=90,err=90) ((ibuff(j,i),j=1,ncol),i=1,nrow)
        read(ftlunit,end=90,err=90) ((buff(j,i,1),j=1,ncol),i=1,nrow)
	end if
c     rivers
      position=position+1
      if (mtriv.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  ibnode(position)=nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
	      position=position+1
	      ibnode(position)=rcl2i(i,j,k)
          end do
	  end if
	else
	  ibnode(position)=-1
	end if
c     general head boundary
      position=position+1
      if (mtghb.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  ibnode(position)=nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
	      position=position+1
	      ibnode(position)=rcl2i(i,j,k)
          end do
	  end if
	else
	  ibnode(position)=-1
	end if
c     stream
      position=position+1
      if (mtstr.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  ibnode(position)=nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
	      position=position+1
	      ibnode(position)=rcl2i(i,j,k)
          end do
	  end if
	else
	  ibnode(position)=-1
	end if
c     reservoir
      position=position+1
      if (mtres.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  ibnode(position)=nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
	      position=position+1
	      ibnode(position)=rcl2i(i,j,k)
          end do
	  end if
	else
	  ibnode(position)=-1
	end if
c     specified flow
      position=position+1
      if (mtfhb.ne.0) then
        read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  ibnode(position)=nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q
	      position=position+1
	      ibnode(position)=rcl2i(i,j,k)
          end do
	  end if
	else
	  ibnode(position)=-1
	end if
c	 multinode wells
      position=position+1
      if (mtmaw.ne.0) then
	  read(ftlunit,end=90,err=90) period,flowstep,nc,nr,nl,label,nq
	  ibnode(position)=nq
	  if (nq.gt.0) then
          do m=1,nq
            read(ftlunit,end=90,err=90) k, i, j, q, IDWell, QSW
	      position=position+1
	      ibnode(position)=rcl2i(i,j,k)
          end do
	  end if
	else
	  ibnode(position)=-1
	end if
c     repeat if we have not reached period and time step for concentration
      if (period.lt.kper.or.flowstep.lt.kstp) goto 10
	goto 100
 90   continue
100   deallocate(buff)
	deallocate(ibuff)
      return
      end
      integer function rcl2i(arow,acol,alayer) 
c     returns a node number based on a row column and layer position.
      use mtmodule
      implicit none
      integer,intent(in)::arow,acol,alayer
c     --------------------------------------------
      if ((alayer.lt.1).or.(alayer.gt.nlay).or.(arow.lt.1).or.
     1      (arow.gt.nrow).or.(acol.lt.1).or.(acol.gt.ncol)) then
        rcl2i=0
        return
      endif
      rcl2i=(nlay-alayer)*nrow*ncol + (nrow-arow)*ncol + (acol-1)
      return
      end function rcl2i
      
      subroutine cleanup
      use mtmodule
      implicit none
	logical isopen
c      if (allocated(IRES)) deallocate (IRES)
      inquire (unit=cnfunit,opened=isopen)
      if (isopen) close(cnfunit)
      inquire (unit=ucnunit,opened=isopen)
      if (isopen) close(ucnunit)
      inquire (unit=ftlunit,opened=isopen)
      if (isopen) close(ftlunit)
      return
      end subroutine cleanup
