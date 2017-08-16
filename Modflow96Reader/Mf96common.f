      subroutine rewindparticlefile(ierror)
      use mf96module
      implicit none
      integer inunit
      integer,intent(out)::ierror
	if (JUNIT(4).gt.0) inunit = JUNIT(4)
	if (JUNIT(5).gt.0) inunit = JUNIT(5)
	  call commonrewindparticlefile(inunit,ierror)
	return
	end subroutine rewindparticlefile
c
c
c
	subroutine readparticlecount(NP,ierror,istep)
      use mf96module
      implicit none
      integer inunit
	logical binary
      integer,intent(out)::ierror,NP
      integer,intent(in)::istep
      logical isopen
	integer KPER,KSTP,IMOV
	real SUMTCH 
	inunit = 0
	if (JUNIT(4).gt.0) then
	  inunit = JUNIT(4)
	  binary = .false.
      endif 
	if (JUNIT(5).gt.0) then
	  inunit = JUNIT(5)
	  binary = .true.
      endif 
      call commonreadparticlecount(inunit,binary,NP,ierror,istep)
	return
	end subroutine readparticlecount
c
c
c
	subroutine readparticles(NP,ierror,coord,scalars, 
	1  delr, delc, elev)
      use mf96module
      implicit none
      integer,intent(inout)::NP
      integer,intent(out)::ierror
	real,intent(inout):: coord, scalars
	real, intent(in):: delr, delc, elev
	dimension delr(NCOL), delc(NROW), elev(*)
	integer inunit
	dimension coord(NP*3), scalars(NP)
	integer IP
      logical isopen
	logical binary
	integer ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      COMMON /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
	inunit = 0
	if (JUNIT(4).gt.0) then
	  inunit = JUNIT(4)
	  binary = .false.
      endif 
	if (JUNIT(5).gt.0) then
	  inunit = JUNIT(5)
	  binary = .true.
      endif 
	call commonreadparticles(inunit,binary,NP,ierror,coord,
	1  scalars, NCOL,NROW,NLAY, ISCOL1,ISCOL2,ISROW1,ISROW2,
     2  ISLAY1,ISLAY2, delr, delc, elev)
	return
	end subroutine readparticles
c
c
c
      subroutine dims(ierror,nc,nr,nl,imoc,unstruct,ITimeUnit,
	1  namefile,elevfile, IXSEC)
      use mf96module
      implicit none
      integer,intent(in)::imoc,unstruct
      integer,intent(out)::ierror,nc,nr,nl,IXSEC, ITimeUnit
      character*256,intent(in)::namefile,elevfile
c     Named common block shared with Modflow 96 and Moc3d
      logical unstructbin
      COMMON /bintype/unstructbin
c     Locally defined variables
      logical isopen
      integer inunit,i,ITMUNI
      character*4 CUNIT(40)
	integer ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      COMMON /SUBGRD/
     *  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      DATA CUNIT/'BCF ','WEL ','DRN ','RIV ','EVT ','TLK ','GHB ',
     1           'RCH ','SIP ','DE4 ','SOR ','OC  ','PCG ','GFD ',
     2           'CONC','HFB ','RES ','STR ','IBS ','CHD ','FHB ',
     3           '    ','    ','    ','    ','    ','    ','    ',
     4           '    ','    ','    ','    ','    ','    ','    ',
     5           '    ','    ','    ','    ','    '/
c     --------------------------------------------
      ISLAY1 = 1
      ISLAY2 = 0
      if (imoc.eq.0) then
        moc=.FALSE.
      else
        moc=.TRUE.
      end if
      if (unstruct.eq.0) then
        unstructbin=.FALSE.
      else
        unstructbin=.TRUE.
      end if
      ierror=0
c     open the name file
      inunit=99
      open(unit=inunit,file=namefile,form='formatted',
     &      status='old',action='read',err=90)
90    inquire (unit=inunit,opened=isopen)
      if (.not.isopen) then
        ierror=2
        return
      end if
c     open the elevation file
      lunit=98
      open(unit=lunit,file=elevfile,form='formatted',
     &      status='old',action='read',err=91)
91    inquire (unit=lunit,opened=isopen)
      if (.not.isopen) then
        ierror=3
        return
      end if
c     read modflow basic simulation data
      maxunit=99
      call BAS5DF(NCOL,NROW,NLAY,INBAS,IUNIT,CUNIT,INUNIT,IXSEC,
	1  IFREFM,ITMUNI,maxunit,ierror)
      close(unit=inunit)
      if (ierror.ne.0) return
	ITimeUnit = ITMUNI
      if (moc) then
        if(IUNIT(15).gt.0) then
          call mc3df(IUNIT(15),junit,nscol,nsrow,nslay,
     1          maxunit,ierror)
	  else
	    ierror=11
        end if
        if (ierror.ne.0) return
        nc=nscol
        nr=nsrow
        nl=nslay
      else
        read(lunit,*,err=999) (laycbd(i),i=1,NLAY)
        ncnfbd=0
        do i=1,NLAY
          if (laycbd(i).ne.0) ncnfbd=ncnfbd+1
        end do
        nbotm=NLAY+ncnfbd
        nc=NCOL
        nr=NROW
        nl=nbotm
      end if
      return
  999 ierror=7
      return
      end subroutine dims

      subroutine grid(ierror,delr,delc,elev,ibnd,conductivity,
     1      vnull1,vnull2,sunit,vunit,xoffset,yoffset,isMfLayer)
      use mf96module
	use mfcommonmodule
      implicit none
      integer,intent(out)::ierror,ibnd(*),sunit(*),vunit,isMfLayer(*)
      real,intent(out)::delr(*),delc(*),elev(*),conductivity(*),
     1  vnull1,vnull2,xoffset,yoffset
c     Named common block shared with Modflow 96
      integer LAYCON
      COMMON /FLWCOM/LAYCON(200)
c     named common block shared with Moc3d
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      COMMON /SUBGRD/
     1  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     Named common block for subroutines to read data
      common /mfcommon/ IBSDIM,IBSNULL,hhnoflo,hhdry
c     Named common block for ibs package
      COMMON /IBSCOM/ IBQ(200)
c     Locally defined variables
      logical isopen
      integer ISUM,LENX,IAPART
      integer LCSC1,LCHY,LCBOT,LCTOP,LCSC2,LCTRPY,
     1        IBCFCB,LCWETD,IWDFLG,LCCVWD,IWETIT,IHDWET
      integer IHEDUN,IDDNUN,LBHDSV,LBDDSV
	integer LCHC,LCSCE,LCSCV,LCSUB,IIBSCB,IIBSOC,IBSDIM
      integer ISUBUN,ICOMUN,IHCUN,ISUBFM,ICOMFM,IHCFM,IBQ
      real WETFCT,IBSNULL,hhnoflo,hhdry,SUBNULL
      real hdry,hnoflo,cnoflo
      real fdtmth,epsslv,sbvl
      character*24 ANAME
      character*20 CHEDFM,CDDNFM
      integer ncr,offset,i,j,k,kk,index
      integer nscr,ks,is,js,koffset,ioffset,kb
      real,allocatable::dc(:),dr(:),TRPY(:),top(:,:),thick(:,:,:),
     1      cc(:,:,:),hy(:,:,:),cv(:,:,:),wetdry(:,:,:),cvwd(:,:,:),
     2      dummy(:),buffer(:,:) 
      real thickness
c     --------------------------------------------
      ierror=0     
      LENX=9999999
	ibsdim=0
	IBSNULL=0
	SUBNULL=1e30
      allocate(IBOUND(NCOL,NROW,NLAY))
      allocate(TRPY(NLAY))
      allocate(buffer(NCOL,NROW))
      allocate(dummy(NCOL*NROW*NLAY))
      allocate(cc(NCOL,NROW,NLAY))
      allocate(hy(NCOL,NROW,NLAY))
      allocate(cv(NCOL,NROW,NLAY))
      allocate(wetdry(NCOL,NROW,NLAY))
      allocate(cvwd(NCOL,NROW,NLAY))
      allocate(dc(NROW))
      allocate(dr(ncol))
	j = NLAY + NCNFBD 
	do i=1,nlay
	  isMfLayer(j) = 1
	  if (LAYCBD(i).NE.0) then
	    j = j-1
	    isMfLayer(j) = 0
	  end if
	  j = j-1
	end do
c     read layer elevation
      if (moc) then
c       read top elevation and layer thicknesses for moc        
        allocate(top(nscol,nsrow))
        allocate(thick(nscol,nsrow,nslay))
        call u2drl1(top,aname,nsrow,nscol,0,lunit,IOUT,ierror)
	  if (ierror.gt.0) goto 100
        call mc3read(thick,nscol,nsrow,nslay,
     1        ncol,nrow,nlay,cnoflo,ierror)
	  if (ierror.gt.0) goto 100
      else
c       read layer elevations for modflow 96
        ncr=NCOL*NROW
        do k=0,nbotm
          offset=(nbotm-k)*ncr
          call u2drl1(buffer,ANAME,NROW,NCOL,0,lunit,IOUT,ierror)
	    if (ierror.ne.0) goto 100
          do i=1,NROW
            do j=1,NCOL
              elev(offset+(NROW-i)*NCOL+j)=buffer(j,i)
            end do
          end do
        end do
      end if

      if(IUNIT(1).gt.0) call BCF5AL(ISUM,LENX,LCSC1,LCHY,
     1     LCBOT,LCTOP,LCSC2,LCTRPY,IUNIT(1),ISS,
     2     NCOL,NROW,NLAY,IOUT,IBCFCB,LCWETD,IWDFLG,LCCVWD,
     3     WETFCT,IWETIT,IHDWET,HDRY,IAPART,IFREFM,ierror)
      if (ierror.ne.0) goto 100

      IF (IUNIT(19).GT.0) then
          CALL IBS1AL(ISUM,LENX,LCHC,LCSCE,LCSCV,LCSUB,NCOL,NROW,NLAY,       
     1        IIBSCB,IIBSOC,ISS,IUNIT(19),IOUT,ierror) 
          if (ierror.ne.0) goto 100
	    if (IUNIT(19).ne.0) then
            ibsdim=0
            DO 120 K=1,NLAY
              IF(IBQ(K).LE.0) GO TO 120
              ibsdim=ibsdim+1
  120       CONTINUE
          end if
	end if

      call BAS5RP(IBOUND,INBAS,NCOL,NROW,NLAY,IUNIT(12),
     2       ihedun,iddnun,IOUT,CHEDFM,
     3       CDDNFM,IXSEC,lbhdsv,lbddsv,IFREFM,hnoflo,ierror)
      close(INBAS)
      if (ierror.ne.0) goto 100

      if(IUNIT(1).gt.0) call BCF5RP(IBOUND,dummy,dummy,hy,dummy,
     1          cc,cv,dr,dc,dummy,dummy,dummy,TRPY,IUNIT(1),
     2          ISS,NCOL,NROW,NLAY,IOUT,wetdry,IWDFLG,cvwd,ierror)
      if (ierror.ne.0) goto 100

      IF(IUNIT(19).GT.0) then
        CALL IBS1RP(dr,dc,dummy,     
     1      dummy,dummy,dummy,dummy,NCOL,NROW,NLAY,NCOL*NROW*NLAY,          
     2      IIBSOC,ISUBFM,ICOMFM,IHCFM,ISUBUN,ICOMUN,IHCUN,       
     3      IUNIT(19),IOUT,ierror) 
     	   IBSNULL=hnoflo
         if (ierror.ne.0) goto 100
      end if

      if (moc) then
c       construct subgrid delc to return (don't assume uniform grid)
        do i=1,nsrow
          delc(i)=dc(ISROW2-i+1)
        end do
c       construct subgrid delr to return
        do i=1,nscol
          delr(i)=dr(ISCOL1+i-1)
        end do

	  SubColumnOffset = 0
        do i=1,ISCOL1-1
          SubColumnOffset=SubColumnOffset + dr(i)
        end do
	  SubRowOffset = 0
        do i=NROW,ISROW2+1,-1
          SubRowOffset=SubRowOffset + dc(i)
        end do

        nscr=nscol*nsrow
        do ks=1,nslay
          k=ks+islay1-1
          offset=(nslay-ks)*nscr
          do is=1,nsrow
            i=is+isrow1-1
            do js=1,nscol
              j=js+iscol1-1
              index=offset+(nsrow-is)*nscol+js
              if (wetdry(j,i,k).ne.0.and.ibound(j,i,k).eq.0) 
     1               ibound(j,i,k) = 1
              ibnd(index)=ibound(j,i,k)
            end do
          end do
        end do 

        koffset=nslay*nscr
        do i=1,nsrow
          ioffset=koffset+(nsrow-i)*nscol
          do j=1,nscol
            elev(ioffset+j)=0
          end do
        end do
        do k=1,nslay
          koffset=(nslay-k)*nscr
          do i=1,nsrow
            ioffset=koffset+(nsrow-i)*nscol
            do j=1,nscol
              elev(ioffset+j)=elev(ioffset+j+nscr)-thick(j,i,k)
            end do
          end do
        end do

        nscr=nscol*nsrow
        do k=0,nslay
          koffset=(nslay-k)*nscr
          do i=1,nsrow
            ioffset=koffset+(nsrow-i)*nscol
            do j=1,nscol
              elev(ioffset+j)=elev(ioffset+j)+top(j,i)
            end do
          end do
        end do

	  sunit(1)=0
	  vunit=0
        if (junit(2).gt.0) then
	    inquire(unit=junit(2),opened=isopen)
	    if (isopen) then
            sunit(1)=-junit(2)
	    end if
	  end if
	  if (sunit(1).eq.0.and.junit(3).gt.0) then
	    inquire(unit=junit(3), opened=isopen)
	    if (isopen) then
            sunit(1)=junit(3)
	    end if
        end if
        if (junit(6).gt.0) then
	    inquire(unit=junit(6),opened=isopen)
	    if (isopen) then
            vunit=-junit(6)
	    end if
	  end if
	  if (vunit.eq.0.and.junit(7).gt.0) then
	    inquire(unit=junit(7),opened=isopen)
	    if (isopen) then
            vunit=junit(7)
	    end if
        end if

        xoffset=0
        if (iscol1.gt.1) then
          do i=1,iscol1-1
            xoffset=xoffset + dr(i)
          end do
        end if
        yoffset=0
        if (isrow2.lt.nrow) then
          do i=isrow2+1,nrow
            yoffset=yoffset + dc(i)
          end do
        end if
        vnull1=cnoflo
        vnull2=cnoflo
      else
c       COPY DC TO DELC IN REVERSE DIRECTION
        do i=1,NROW
          delc(i)=dc(NROW-i+1)
        end do
c       COPY DR TO DELR
        do i=1,NCOL
          delr(i)=dr(i)
        end do
c
c       copy ibound and conductivity in reordered and extended format
        kk=0
        KB=0
        do k=1,NLAY
          IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) KB=KB+1
c         work on the model layer
          kk=kk+1
          offset=(nbotm-kk)*ncr
          do i=1,NROW
            do j=1,NCOL
              index=offset+(NROW-i)*NCOL+j
              if (wetdry(j,i,k).ne.0.and.ibound(j,i,k).eq.0) 
     1               ibound(j,i,k) = 1
              ibnd(index)=IBOUND(j,i,k)
              if (LAYCON(k).eq.0.or.LAYCON(k).eq.2) then
                thickness=elev(index+ncr) - elev(index);
                if (thickness.gt.0) then
                  conductivity(index)=cc(j,i,k)/thickness
                else
                  conductivity(index)=0
                end if
	        else
                conductivity(index)=hy(j,i,kb)
              end if
            end do
          end do
c         work on the underlying confining layer if it exists
c         set the (extended) ibnd to the same as model layer
c         set the conductivity to zero.
c         MODIFIED in version 1.1
c         The cell in a confining layer is assumed active only
c         if both the overlying and underlying cells (in aquifer
c         layer) are active.
          if (laycbd(k).ne.0) then
            kk=kk+1
            offset=(nbotm-kk)*ncr
            do i=1,NROW
              do j=1,NCOL
                index=offset+(NROW-i)*NCOL+j
c               The following line in version 1.0 is commented out
c                ibnd(index)=IBOUND(j,i,k)
c               The following 5 lines are used in version 1.1
                if (IBOUND(j,i,k).ne.0.and.IBOUND(j,i,k+1).ne.0) then
                    ibnd(index) = 1
                else
                    ibnd(index) = 0
                end if
                conductivity(index)=0
              end do
            end do
          end if
        end do   
	  i=0
        inquire(unit=ihedun, opened=isopen)
	  if (isopen) then
	    i=1
          if (CHEDFM.eq.' ') then
            sunit(1)=ihedun
          else
            sunit(1)=-ihedun
          end if
	  end if
c       make sure the drawdown unit is not same as head unit
        if (ihedun.ne.iddnun) then
	    inquire(unit=iddnun, opened=isopen)
	    if (isopen) then
		  i=i+1
		  if (CDDNFM.eq.' ') then
		    sunit(i)=iddnun
		  else
		    sunit(i)=-iddnun
		  end if
	    end if
	  end if
        inquire(unit=icomun, opened=isopen)
	  if (isopen) then
	    i=i+1
          sunit(i)=icomun
	  end if
        inquire(unit=ihcun, opened=isopen)
	  if (isopen) then
	    i=i+1
          sunit(i)=ihcun
	  end if
	  vunit=0
	  if (ibcfcb.gt.0) then
          inquire(unit=ibcfcb, opened=isopen)
	    if (isopen) then
            vunit=ibcfcb
	    end if
	  end if
        xoffset=0
        yoffset=0
        vnull1=hnoflo
        vnull2=hdry
        islay1=1
        islay2=NLAY
        isrow1=1
        isrow2=NROW
        iscol1=1
        iscol2=NCOL
      end if
100   continue
      hhnoflo=hnoflo
	hhdry=hdry
      deallocate(buffer)
      deallocate(TRPY)
      deallocate(dummy)
      deallocate(cc)
      deallocate(hy)
      deallocate(cv)
      deallocate(wetdry)
      deallocate(cvwd)
      deallocate(dc)
      deallocate(dr)
      if (allocated(top)) deallocate(top)
      if (allocated(thick)) deallocate(thick)
      close(unit=lunit)
      return
      end subroutine grid

      subroutine countfeatures(ierror,ibsize,nfeat)
      use mf96module
      implicit none
      integer,intent(out)::ierror,ibsize,nfeat
c     locally defined variables
      real,allocatable::dummy(:,:),BDTIM(:),SBHED(:,:),FLWRAT(:,:)
      integer,allocatable::IFLLOC(:,:),IHDLOC(:,:)
      integer i,j,k,ISUM,LENX
      integer IWELCB,IDRNCB,IRIVCB,IGHBCB,IRESCB,ISTCB1,ISTCB2,IFHBCB
      integer LCWELL,LCDRAI,LCRIVR,LCBNDS
      integer LCIRES,LCIRSL,LCBRES,LCCRES,LCBBRE,LCHRES,LCHRSE
      integer LCSTRM,LCTBAR,LCTRIB,LCIVAR,LCFGAR
      integer LCCHDS,LCFLLC,LCBDTM,LCFLRT,LCBDFV,LCBDHV,LCHDLC,LCSBHD
      integer ICSTRM,IRESPT
      real CONST
      integer NBDTIM,NFHBX1,NFHBX2,IFHBD3,IFHBD4,IFHBD5,IFHBSS
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      COMMON /SUBGRD/
     1  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     --------------------------------------------
      ierror=0
      LENX=9999999
      allocate (DUMMY(NCOL,NROW))
      nfeat=0
      ibousz=0
c fixed heads (negative values in IBOUND array)
      nfeat=nfeat + 1
      ibousz=ibousz + 1
      nfixedhead=0
      do k=islay1,islay2
        do i=isrow1,isrow2
          do j=iscol1,iscol2
            if (IBOUND(j,i,k).lt.0) then
                nfixedhead=nfixedhead + 1
            end if
          enddo
        enddo
      enddo
      ibousz=ibousz + nfixedhead
c Well Package
      if(IUNIT(2).gt.0) then
        rewind(IUNIT(2))
        call WEL5AL(ISUM,LENX,LCWELL,MXWELL,NWELLS,
     1    IUNIT(2),IOUT,IWELCB,NWELVL,IWELAL,IFREFM,ierror)
        if (ierror.ne.0) goto 10
        if (.not.allocated(well)) then
          allocate (well(NWELVL,MXWELL))
        end if
        ibousz=ibousz + MXWELL + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c Drain Package
      if(IUNIT(3).gt.0) then
        rewind(IUNIT(3))
        call DRN5AL(ISUM,LENX,LCDRAI,NDRAIN,MXDRN,
     1    IUNIT(3),IOUT,IDRNCB,NDRNVL,IDRNAL,IFREFM,ierror)
        if (ierror.ne.0) goto 10
        if (.not.allocated(drain)) then
          allocate (drain(NDRNVL,MXDRN))
        end if
        ibousz=ibousz + MXDRN + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c River Package
      if(IUNIT(4).gt.0) then
        rewind(IUNIT(4))
        call RIV5AL(ISUM,LENX,LCRIVR,MXRIVR,NRIVER,
     1            IUNIT(4),IOUT,IRIVCB,NRIVVL,IRIVAL,IFREFM,ierror)
        if (ierror.ne.0) goto 10
        if (.not.allocated(river)) then
          allocate (river(NRIVVL,MXRIVR))
        end if
        ibousz=ibousz + MXRIVR + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c Stream Package
      if(IUNIT(18).gt.0) then
        rewind(IUNIT(18))
        call STR1AL(ISUM,LENX,LCSTRM,ICSTRM,MXSTRM,                    
     1                 NSTREM,IUNIT(18),IOUT,ISTCB1,ISTCB2,NSS,NTRIB,  
     2                 NDIV,ICALC,CONST,LCTBAR,LCTRIB,LCIVAR,LCFGAR,
     3                 ierror)   
        if (ierror.ne.0) goto 10
        if (.not.allocated(stream)) then
          allocate (stream(5,MXSTRM))
        end if
        if (.not.allocated(ISTRM)) then
          allocate (ISTRM(5,MXSTRM))
        end if
        if (.not.allocated(STRM)) then
          allocate(STRM(11,MXSTRM))
        end if
        if (.not.allocated(ITRBAR)) then
          allocate(ITRBAR(NSS,NTRIB))
        end if
        if (.not.allocated(IDIVAR)) then
          allocate(IDIVAR(NSS))
        end if
        ibousz=ibousz + MXSTRM + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c Reservoir Package
      if(IUNIT(17).gt.0) then
        rewind(IUNIT(17))
        if (.not.allocated(IRES)) allocate (IRES(NCOL,NROW))
        if (.not.allocated(IRESL)) allocate (IRESL(NCOL,NROW))
        call RES1AL(ISUM,LENX,LCIRES,LCIRSL,LCBRES,
     1    LCCRES,LCBBRE,LCHRES,LCHRSE,IUNIT(17),IOUT,NRES,IRESCB,
     2    NRESOP,IRESPT,NPTS,NCOL,NROW,ierror)
        if (ierror.ne.0) goto 10
        call RES1RP1(IRES,IRESL,DUMMY,DUMMY,DUMMY,IBOUND,NRES,
     2    NRESOP,NPTS,NCOL,NROW,NLAY,IUNIT(17),IOUT,NCELL,ierror)
        if (ierror.ne.0) goto 10
        ibousz=ibousz + NCELL + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c GHB Package
      if(IUNIT(7).gt.0) then
        rewind(IUNIT(7))
        call GHB5AL(ISUM,LENX,LCBNDS,NBOUND,MXBND,
     1            IUNIT(7),IOUT,IGHBCB,NGHBVL,IGHBAL,IFREFM,ierror)
        if (ierror.ne.0) goto 10
        if (.not.allocated(ghb)) then
          allocate (ghb(NGHBVL,MXBND))
        end if
        ibousz=ibousz + MXBND + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c CHD Package
      if(IUNIT(20).gt.0) then
        rewind(IUNIT(20))
        call CHD1AL(ISUM,LENX,LCCHDS,NCHDS,MXCHD,     
     1           IUNIT(20),IOUT,ierror)                                        
        if (ierror.ne.0) goto 10
        if (.not.allocated(chd)) then
          allocate (chd(5,MXCHD))
        end if
        ibousz=ibousz + MXCHD + 1
      else
        ibousz=ibousz + 1
      endif
      nfeat=nfeat + 1
c FHB Package
      if(IUNIT(21).gt.0) then
        rewind(IUNIT(21))
        call FHB1AL(ISUM,LENX,LCFLLC,LCBDTM,LCFLRT,
     1          LCBDFV,LCBDHV,LCHDLC,LCSBHD,NBDTIM,NFLW,NHED,IUNIT(21),
     2          IOUT,IFHBCB,NFHBX1,NFHBX2,IFHBD3,IFHBD4,IFHBD5,
     3          IFHBSS,ISS,ierror)
        if (ierror.ne.0) goto 10
	  if (iunit(21).eq.0) then
          ibousz=ibousz + 2
	  else
          ibousz=ibousz + NFLW + NHED + 2
          if (.not.allocated(fhbflow)) allocate(fhbflow(NFLW))
          if (.not.allocated(fhbhead)) allocate(fhbhead(NHED))
          allocate(IFLLOC(4,NFLW))
          allocate(IHDLOC(4,NHED))
          allocate(BDTIM(NBDTIM))
          allocate(SBHED(IFHBD5,NHED))
          allocate(FLWRAT(IFHBD3,NFLW))
          call FHB1RP(IBOUND,NROW,NCOL,NLAY,
     &          IFLLOC,BDTIM,NBDTIM,FLWRAT,NFLW,NHED,
     &          IHDLOC,SBHED,IUNIT(21),IOUT,
     &          NFHBX1,NFHBX2,IFHBD3,IFHBD5,ierror)
          if (ierror.ne.0) goto 5
          call storefhb(IFLLOC,IHDLOC,ierror)
5         continue
          deallocate(IFLLOC)
          deallocate(IHDLOC)
          deallocate(BDTIM)
          deallocate(SBHED)
          deallocate(FLWRAT)
          if (ierror.ne.0) goto 10
	  end if
      else
        ibousz=ibousz + 2
      endif
      nfeat=nfeat + 2
10    continue
      deallocate (DUMMY)
      ibsize=ibousz
      return
      end subroutine countfeatures

      subroutine getfeatures(ierror,ibnode)
      use mf96module
      use simtime
      implicit none
      integer,intent(out)::ibnode(*),ierror
c     locally defined variables
      integer position,i,j,k,IPTFLG,NSTP
      real DELT,PERLEN,TSMULT
c     Named common block shared with Moc3d
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      COMMON /SUBGRD/
     1  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     --------------------------------------------
      interface
        integer function rcl2i(arow,acol,alayer,ierror)
          implicit none
          integer,intent(in)::arow,acol,alayer
          integer,intent(out)::ierror
        end function rcl2i
      end interface
c     --------------------------------------------
      do i=1,ibousz
        ibnode(i)=0
      enddo
      position=0    
c Fixed Heads
      if (nfixedhead.gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          position=position + 1
          ibnode(position)=nfixedhead
          do k=islay1,islay2
            do i=isrow1,isrow2
              do j=iscol1,iscol2
                if (IBOUND(j,i,k).lt.0) then
                  position=position + 1
                  ibnode(position)=rcl2i(i,j,k,ierror)
                end if
              enddo
            enddo
          enddo
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        ibnode(position)=-1
      end if
c Well Package
      if(IUNIT(2).gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          call WEL5RP(well,NWELLS,MXWELL,IUNIT(2),
     1          IOUT,NWELVL,IWELAL,IFREFM)
          if (ierror.ne.0) goto 100
          call setboundary(well,NWELVL,NWELLS,position,ibousz,
     1          ibnode,ierror)
100       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c Drain Package
      if(IUNIT(3).gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          call DRN5RP(drain,NDRAIN,MXDRN,IUNIT(3),
     1          IOUT,NDRNVL,IDRNAL,IFREFM,ierror)
          if (ierror.ne.0) goto 200
          call setboundary(drain,NDRNVL,NDRAIN,position,ibousz,
     1          ibnode,ierror)
200       continue
          if (ierror.ne.0) goto 10
        else
           position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c River Package
      if(IUNIT(4).gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          call RIV5RP(river,NRIVER,MXRIVR,IUNIT(4),
     1          IOUT,NRIVVL,IRIVAL,IFREFM,ierror)
          if (ierror.ne.0) goto 300
          call setboundary(river,NRIVVL,NRIVER,position,ibousz,
     1          ibnode,ierror)
300       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c Stream Package
      if(IUNIT(18).gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          call STR1RP(STRM,ISTRM,NSTREM,       
     1                     MXSTRM,IUNIT(18),IOUT,ITRBAR,NDIV,NSS,   
     2                     NTRIB,IDIVAR,ICALC,IPTFLG,ierror)                
          if (ierror.ne.0) goto 500
          do i=1,5
            do j=1,mxstrm
              stream(i,j)=ISTRM(i,j)
            end do
          end do
c         Note that the array stream might contain the same cell
c         multiple times if the cell participates in more than one branch.
          call setboundary(stream,5,NSTREM,position,ibousz,
     1          ibnode,ierror)
500       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c Reservoir Package
      if(IUNIT(17).gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          call setreservior(position,ibnode,ierror)
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c GHB Package
      if(IUNIT(7).gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          call GHB5RP(ghb,NBOUND,MXBND,IUNIT(7),
     1          IOUT,NGHBVL,IGHBAL,IFREFM,ierror)
          if (ierror.ne.0) goto 400
          call setboundary(ghb,NGHBVL,NBOUND,position,ibousz,
     1          ibnode,ierror)
400       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c CHD Package
      if(IUNIT(20).gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          call CHD1RP(chd,NCHDS,MXCHD,IBOUND,  
     1            NCOL,NROW,NLAY,PERLEN,DELT,NSTP,TSMULT,IUNIT(20),IOUT,
     2            ierror)
          if (ierror.ne.0) goto 600
          call setboundary(chd,5,NCHDS,position,ibousz,ibnode,
     1          ierror)
600       continue
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
c FHB Package
      if(IUNIT(21).gt.0) then
        if (.not.moc.or.cperiod.gt.0) then
          call setfhb(position,ibnode,ierror)
          if (ierror.ne.0) goto 10
        else
          position=position + 1
          ibnode(position)=0
          position=position + 1
          ibnode(position)=0
        end if
      else
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
        position=position + 1
        if (position.gt.ibousz) then
          ierror=19
          goto 10
        endif
        ibnode(position)=-1
      endif
10    continue
      return
      end subroutine getfeatures

      subroutine cleanup
      use mf96module
      implicit none
      integer i
      if (allocated(IRES)) deallocate (IRES)
      if (allocated(IRESL)) deallocate (IRESL)
      if (allocated(fhbflow)) deallocate (fhbflow)
      if (allocated(fhbhead)) deallocate (fhbhead)
      if (allocated(IBOUND)) deallocate(IBOUND)
      if (allocated(well)) deallocate(well)
      if (allocated(drain)) deallocate(drain)
      if (allocated(river)) deallocate(river)
      if (allocated(ghb)) deallocate(ghb)
      if (allocated(stream)) deallocate(stream)
      if (allocated(chd)) deallocate(chd)
      if (allocated(STRM)) deallocate(STRM)
      if (allocated(ISTRM)) deallocate(ISTRM)
      if (allocated(ITRBAR)) deallocate(ITRBAR)
      if (allocated(IDIVAR)) deallocate(IDIVAR)
      do i=1,maxunit
         close(i,err=20)
20    continue
      end do
      return
      end subroutine cleanup

      subroutine storefhb(iflloc,ihdloc,ierror)
      use mf96module
      implicit none
      integer position,layer,row,column,ierror 
      integer iflloc(4,nflw),ihdloc(4,nhed)
      interface
        integer function rcl2i(arow,acol,alayer,ierror)
          implicit none
          integer,intent(in)::arow,acol,alayer
          integer,intent(out)::ierror
        end function rcl2i
      end interface
      do position=1,nflw
        layer=iflloc(1,position)
        row=iflloc(2,position)
        column=iflloc(3,position)
        fhbflow(position)=rcl2i(row,column,layer,ierror)
        if (ierror.ne.0) goto 10
      enddo 
      do position=1,nhed
        layer=ihdloc(1,position)
        row=ihdloc(2,position)
        column=ihdloc(3,position)
        fhbhead(position)=rcl2i(row,column,layer,ierror)
        if (ierror.ne.0) goto 10
      enddo 
   10 continue
      return
      end subroutine storefhb

      subroutine setboundary(boundary,firstlimit,secondlimit,position,
     1   ibousz,ibnode,ierror)
      implicit none
      interface
        integer function rcl2i(arow,acol,alayer,ierror)
          implicit none
          integer,intent(in)::arow,acol,alayer
          integer,intent(out)::ierror
        end function rcl2i
      end interface
      integer firstlimit,secondlimit,position,ibousz,ierror
      real boundary(firstlimit,secondlimit)
      integer ibnode(ibousz)
      integer layer,row,column,counter,i,index
      position=position + 1
      counter=position
      ibnode(counter)=0
      do i=1,secondlimit
        layer =boundary(1,i)
        row   =boundary(2,i)
        column=boundary(3,i)
        index=rcl2i(row,column,layer,ierror)
        if (ierror.ne.0) goto 10
c       if index is -1,this means the cell is outside the transport subgrid
        if (index.ge.0) then
          position=position + 1
          ibnode(position)=index
          ibnode(counter)=ibnode(counter) + 1
        end if
      enddo
10    continue
      return
      end subroutine setboundary

      subroutine setreservior(position,ibnode,ierror)
      use mf96module
      implicit none
      interface
        integer function rcl2i(arow,acol,alayer,ierror)
          implicit none
          integer,intent(in)::arow,acol,alayer
          integer,intent(out)::ierror
        end function rcl2i
      end interface
      integer position,ierror
      integer ibnode(ibousz)
      integer row,column,layer,index
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      COMMON /SUBGRD/
     1  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      ierror=0
      if (.not.(allocated(ires))) ierror=19
      if ((nresop.eq.2).and..not.(allocated(iresl))) ierror=19
      if ((nresop.eq.3).and..not.(allocated(ibound))) ierror=19
      if (ierror.ne.0) goto 10
      layer=0
      position=position + 1
      ibnode(position)=ncell
      do row=isrow1,isrow2
        do column=iscol1,iscol2
          if (ires(column,row).ne.0) then
            if (nresop.eq.1) then 
              layer=1
            elseif (nresop.eq.2) then 
              layer=iresl(column,row)
            elseif (nresop.eq.3) then 
              layer=0
              do index=1,nlay
                if (ibound(column,row,index).ne.0) then 
                  layer=index
                  exit
                endif 
              enddo
            endif
            if (layer.ge.islay1.and.layer.le.islay2) then
              position=position + 1
              ibnode(position)=rcl2i(row,column,layer,ierror)
              if (ierror.ne.0) goto 10
            endif
          endif
        enddo
      enddo
10    continue
      return
      end subroutine setreservior

      subroutine setfhb(position,ibnode,ierror)
      use mf96module
      implicit none
      integer ierror
      integer ibnode(ibousz)
      integer position,i,index,counter
      ierror=0
      position=position + 1
      if (position.gt.ibousz) then
        ierror=19
        goto 10
      endif
      if (nflw.gt.0) then
        counter=position
        ibnode(counter)=0
        do i=1,nflw
          index=fhbflow(i)
          if (index.ge.0) then
            position=position + 1
            if (position.gt.ibousz) then
              ierror=19
              goto 10
            endif
            ibnode(position)=index
            ibnode(counter)=ibnode(counter) + 1
	    end if
        enddo
      else
        ibnode(position)=-1
      end if
      position=position + 1
      if (position.gt.ibousz) then
        ierror=19
        goto 10
      endif
      if (nhed.gt.0) then
        counter=position
        ibnode(counter)=0
        do i=1,nhed
          index=fhbhead(i)
          if (index.ge.0) then
            position=position + 1
            if (position.gt.ibousz) then
              ierror=19
              goto 10
            endif
            ibnode(position)=index
            ibnode(counter)=ibnode(counter) + 1
          end if
        enddo
      else
        ibnode(position)=-1
      end if
10    continue
      return
      end subroutine setfhb

      integer function rcl2i(arow,acol,alayer,ierror) 
c     returns a node number based on a row column and layer position.
      use mf96module
      implicit none
      integer,intent(in)::arow,acol,alayer
      integer,intent(out)::ierror
      integer layer,i
      integer  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
      common /subgrd/
     1  ISCOL1,ISCOL2,ISROW1,ISROW2,ISLAY1,ISLAY2,ISUBGD
c     --------------------------------------------
      if ((alayer.lt.1).or.(alayer.gt.nlay).or.(arow.lt.1).or.
     1      (arow.gt.nrow).or.(acol.lt.1).or.(acol.gt.ncol)) then
        ierror=19
        rcl2i=0
        return
      endif
      ierror=0
      if (moc) then
        if ((alayer.lt.islay1).or.(alayer.gt.islay2).or.
     1        (arow.lt.isrow1).or.(arow.gt.isrow2).or.
     2        (acol.lt.iscol1).or.(acol.gt.iscol2)) then
          rcl2i=-1
          return
        else
          rcl2i=(islay2-alayer)*nsrow*nscol 
     1       + (isrow2-arow)*nscol + (acol-iscol1)
          return
        endif	
      else
        layer=0
        do i=1,alayer
          layer=layer + 1
          if ((i.gt.1).and.(laycbd(i-1).ne.0)) then
            layer=layer + 1
          endif
        end do
        rcl2i=(nbotm-layer)*nrow*ncol 
     1      + (nrow-arow)*ncol + (acol-1)
        return
	end if
      end function rcl2i

      SUBROUTINE U2DRL1(A,ANAME,II,JJ,K,IN,IOUT,ierror)
c     Does the same thing as U2DREL except "external" files are rewound
c     after reading. This is used for reading the elevation file
C
C
C-----VERSION 1539 22JUNE1993 U2DREL
C     ******************************************************************
C     ROUTINE TO INPUT 2-D REAL DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       II IS NO. OF ROWS
C       JJ IS NO. OF COLS
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
C              if K=0,NO LAYER IS PRINTED
C              if K<0,CROSS SECTION IS PRINTED)
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION A(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*80 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*80 FNAME
      CHARACTER*11 FMTARG
      logical unstructbin
      COMMON /bintype/unstructbin
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
      ierror=0
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)',err=500,end=500) CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING if FREE FORMAT OR FIXED FORMAT.
      irewnd=0
      ICLOSE=0
      IFREE=1
      ICOL=1
      call URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      if (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') then
         LOCAT=0
      else if(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') then
         LOCAT=IN
      else if(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') then
         call URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
         irewnd=1
      else if(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') then
         call URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
c###         WRITE(IOUT,15) LOCAT,FNAME
c###   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT',I4,':',/1X,A)
         ICLOSE=1
      else
C
C2A-----DID NOT FIND A RECOGNIZED WORD,SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      end if
C
C3------FOR FREE FORMAT CONTROL RECORD,READ REMAINING FIELDS.
      if(IFREE.ne.0) then
         call URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         if(LOCAT.ne.0) then
            call URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            if(ICLOSE.ne.0) then
               if(FMTIN.EQ.'(BINARY)') then
	            if (unstructbin) then
	                FMTARG='BINARY'
	            else
	                FMTARG='UNFORMATTED'
	            end if
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FMTARG,action='read')
               else
                  OPEN(UNIT=LOCAT,FILE=FNAME,action='read')
               end if
            end if
            if(LOCAT.gt.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            call URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         end if
      end if
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      if(LOCAT) 200,50,90
C
C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
   50 do 80 I=1,II
      do 80 J=1,JJ
   80 A(J,I)=CNSTNT
c###      if(K.gt.0) WRITE(IOUT,2) ANAME,CNSTNT,K
c###    2 FORMAT(1X,/1X,A,' =',G15.7,' FOR LAYER',I4)
c###      if(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
c###    3 FORMAT(1X,/1X,A,' =',G15.7)
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 if(K.gt.0) then
c###         WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
c###   94    FORMAT(1X,///11X,A,' FOR LAYER',I4,/
c###     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      else if(K.EQ.0) then
c###         WRITE(IOUT,95) ANAME,LOCAT,FMTIN
c###   95    FORMAT(1X,///11X,A,/
c###     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      else
c###         WRITE(IOUT,96) ANAME,LOCAT,FMTIN
c###   96    FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
c###     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      end if
      do 100 I=1,II
      if(FMTIN.EQ.'(FREE)') then
         READ(LOCAT,*,err=500,end=500) (A(J,I),J=1,JJ)
      else
         READ(LOCAT,FMTIN,err=500,end=500) (A(J,I),J=1,JJ)
      end if
      if (irewnd.eq.1) rewind(locat)
  100 CONTINUE
      GO TO 300
C
C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
  200 LOCAT=-LOCAT
c###      if(K.gt.0) then
c###         WRITE(IOUT,201) ANAME,K,LOCAT
c###  201    FORMAT(1X,///11X,A,' FOR LAYER',I4,/
c###     1    1X,'READING BINARY ON UNIT',I4)
c###      else if(K.EQ.0) then
c###         WRITE(IOUT,202) ANAME,LOCAT
c###  202    FORMAT(1X,///1X,A,/
c###     1    1X,'READING BINARY ON UNIT',I4)
c###      else
c###         WRITE(IOUT,203) ANAME,LOCAT
c###  203    FORMAT(1X,///1X,A,' FOR CROSS SECTION',/
c###     1    1X,'READING BINARY ON UNIT',I4)
c###      end if
      READ(LOCAT,err=500,end=500) KSTP,KPER,PERTIM,TOTIM,TEXT,
     1                            NCOL,NROW,ILAY
      READ(LOCAT,err=500,end=500) A
C
C5------if CNSTNT NOT ZERO then MULTIPLY ARRAY VALUES BY CNSTNT.
  300 if(ICLOSE.ne.0) CLOSE(UNIT=LOCAT)
      ZERO=0.
      if(CNSTNT.EQ.ZERO) GO TO 320
      do 310 I=1,II
      do 310 J=1,JJ
      A(J,I)=A(J,I)*CNSTNT
  310 CONTINUE
C
C6------if PRINT CODE (IPRN) >0 OR =0 then PRINT ARRAY VALUES.
c**
  320 return
c****
c###  320 if(IPRN.GE.0) call ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
C
C7------RETURN
c      RETURN
C
C8------CONTROL RECORD ERROR.
  500 ierror=10
      return
      end

