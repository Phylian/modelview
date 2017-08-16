C                  KJH  20030327      -- Patched Hyd.K term in LPF option -- cel2wel function
C     Last change: KJH  20030717      -- Patched budget output switch -- subroutine GWF1MNW1bd
c                                        Cleaned output so outrageous pointers are not printed
c      SUBROUTINE GWF1MNW1DF(LCHANI,LCHK,LCHKCC,LCHUFTHK,LCHY,LCSSHMN,
c     &                      LCTRPY,NHUFAR)
C     VERSION 20020129 ERB
C     ******************************************************************
C     INITIALIZE POINTER VARIABLES USED BY MNW1 TO SUPPORT MULTIPLE FLOW
C     PACKAGES
C     ******************************************************************
c      LCHANI = 1
c      LCHK = 1
c      LCHKCC = 1
c      LCHUFTHK = 1
c      LCHY = 1
c      LCSSHMN = 1
c      LCTRPY = 1
c      NHUFAR = 1
c      RETURN
c      END
c
c-------------------------------------------------------------------------
c
      SUBROUTINE GWF1MNW1al(isum, lcwel2, mxwel2, nwell2, lchref,
     +       nodes, kspref, in, iout, iwl2cb,iowell2, NoMoIter, PLoss,
     +        MNWname, Fname, ierror)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford        1/31/98
c     ******************************************************************
c     allocate array storage for well package
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      common /rev23/ iwelpt
      dimension rn(25), iowell2(3), icF(3)
      character*6 ftag(3)
      character*200 FNAME, MNWname                          !!08/19/02KJH-MODIFIED
      character*256 txt, tx2
	INTEGER ierror
      data ftag/'WEL1  ','BYNODE','QSUM  '/
      data  icF/4,6,4/
c
      iowell2(1) = 0
      iowell2(2) = 0
      iowell2(3) = 0
c
c1------identify package and initialize nwell2
c      write(iout,1)in
c    1 format(/,1x,'MNW1 -- MULTI-NODE WELL PACKAGE, VERSION 1,',
c     +' 8/13/2002.',/,4X,'INPUT READ FROM UNIT ',i3)
      nwell2=0
c
c2------read max number of wells and
c2------unit or flag for cell-by-cell flow terms.
      call ncread(in,txt,ierr, ierror)
	if (ierror .ne. 0) return
      call UPCASE(txt)
c
      ki = index(txt,'REF')
      if( ki.gt.0 ) then  
        tx2 = txt(ki:256)
        call qread(rn,1,tx2,ierr)
        if( ierr.eq.0 ) kspref = ifrl( rn(1) )
        txt(ki:256) = '                                '
      else
        kspref = 1
      endif  
c
      call qread(rn,4,txt,ierr)
      mxwel2 = ifrl( rn(1) )
      iwl2cb = 0
      if(ierr.le.2) iwl2cb = ifrl( rn(2) )
      iwelpt = 0
      if(ierr.eq.1) iwelpt = ifrl( rn(3) )
      NoMoIter   = 9999
      if(ierr.eq.0) NoMoIter = ifrl( rn(4) )
c
c      write(iout,3) mxwel2
c    3 format(1h ,'MAXIMUM OF',i5,' WELLS')
c      if(iwl2cb.gt.0) write(iout,9) iwl2cb
c    9 format(1x, 'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT', i3)
c      if(iwl2cb.lt.0) write(iout,8)
c    8 format(1x,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
c      write(iout,'(2x,33hThe heads at the beginning of SP:,i4,
c     +      1x,41hwill be the default reference elevations.,/)') kspref
c      write(iout,7) NoMoIter
c    7 format(1x,'Flow rates will not be estimated after the',i4,'th',
c     +          ' iteration')
c
c   Define well model to be used
c
      call ncread(in,txt,ierr, ierror)
	if (ierror .ne. 0) return
      call UPCASE(txt)
      PLoss = 0.0   !!  Default use of Skin so linear loss varies with T
      if( index(txt,'LINEAR').gt.0 ) then
        PLoss = 1.0 !!  ADD THIS LINE to make sure that the power term is 1 for the linear model
        ki = index(txt,':') + 1
        tx2 = txt(ki:256)
        call qread(rn,1,tx2,ierr)
        if(ierr.eq.0) PLoss = rn(1)
c   Add error checking to shut down MODFLOW
        BS = 3.6           !!   Maximum limit on power term
        if( PLoss .gt. BS ) then
c          write(*,*)'Power term of',PLoss,' exceeds maximum of', BS
c          write(iout,*)'Power term of',PLoss,' exceeds maximum of',BS
C
C         When compiling MNW with Modflow-96, comment out the call to
C         USTOP and uncomment the STOP statement
          ierror = 1
	    return
c          CALL USTOP(' ')
C          STOP
C
        endif
c
      endif
c
c   Test for a specified PREFIX NAME  for time series output from MNW1OT
c
      call ncread(in,txt,ierr, ierror)
	if (ierror .ne. 0) return
      tx2 = txt
      call UPCASE(tx2)
      kf = index(tx2,'PREFIX:')
      if( kf.gt.0 ) then
        MNWname = txt(kf+7:256)
        ke = index(MNWname,' ')
        MNWname(ke:200) = '               '
        tx2 = MNWname
        call UPCASE(tx2)
        if( index(tx2,'FILEPREFIX').gt.0 ) then
          MNWname = Fname
          ke = index(MNWname,'.')
          MNWname(ke:200) = '               '
        endif
      else
        MNWname = 'OUTput_MNW'
        backspace(in)
      endif
c
c     Test for creation of a WEL1 package and auxillary output files
c
      iok = 1
      do while( iok.eq.1 )
        call ncread(in,txt,ierr, ierror)
  	  if (ierror .ne. 0) return
        tx2 = txt
        call UPCASE(tx2)
        kf = index(tx2,'FILE:')
        if( kf.gt.0 ) then
          kio = 0
          jf = 0
          do while( kio.eq.0 .and. jf.lt.3 )
            jf = jf + 1
            kio = index(tx2,ftag(jf)(1:icF(jf)))
            if( kio .gt.0 ) then
              tx2 = txt(kio+1+icF(jf):256)
              call qread(rn,1,tx2,ierr)
              if( ierr.eq.0 ) then
                iowell2(jf) = ifrl( rn(1) )
c            OC over ride is ALLTIME
                if(index(tx2,'ALLTIME').gt.0) iowell2(jf) = -iowell2(jf)
c            Find and use file name
                tx2 = txt(kf+5:256)
                kf = index(tx2,' ') - 1
c                close( abs(iowell2(jf)) )
c                open(abs(iowell2(jf)), file=tx2(1:kf) )
c                write(tx2(253:256),'(i4)') abs(iowell2(jf))
c                txt =' A '//ftag(jf)//' data input file will be written'
c     +          //' to '//tx2(1:kf)//' on unit '//tx2(253:256)
c                write(iout,'(/1x,a79)') txt
c                if( jf.eq.1 )
c     +          write(abs(iowell2(jf)),'(3i10)') mxwel2, iwl2cb, iwelpt
              endif
            endif
          enddo
        else
          backspace(in)
          iok = 0
        endif
      enddo
c
c3------set lcwel2 equal to location of well list in x array.
      lcwel2 = isum
c
c4------add amount of space used by well list to isum.
cerb      isp  = 16 * mxwel2
cerb  Change made 7/11/2003 - ERB
      isp  = 17 * (mxwel2 + 1)    !!7/13/2003 - CZ: increased to 17 from 16
      isum = isum+isp
c  set aside a single precision array for a set of reference heads
      lchref = isum
      isp  = nodes
      isum = isum+isp
c
c5------print number of spaces in x array used by well package.
c      write(iout,4) isp
c    4 format(1x,i6,' ELEMENTS IN X ARRAY ARE USED FOR MNW1')
c
c  Write header in Auxillary BYNODE file if KPER=1 & IO>0
c
      if ( iowell2(2).ne.0 ) then
        io = abs(iowell2(2))
c        write(io,'(6hSiteID,26x,6h Entry,6h  NODE,5x,10hTotal_Time,
c     +        8x,1hQ,5x,6hH-Well,5x,6hH-Cell,5x,6hQW-Avg)')
      endif
c
c  Write header in Auxillary QSUM file if KPER=1 & IO>0
c
      if ( iowell2(3).ne.0 ) then
        io = abs(iowell2(3))
c        write(io,'(6hSiteID,30x,6h Entry,5x,10hTotal_Time,
c     +     10x,3hQin,10x,4hQout,10x,4hQsum,5x,6hH-Well,5x,6hQW-Avg)')
      endif
c
c7------return
      return
      end
c
c_________________________________________________________________________________
c
      SUBROUTINE GWF1MNW1RP(well2,nwell2,mxwel2,
     +  nodes,nrow,
     +  ncol,kspref,in,iout,iowell2,
     &  NLAY,PLoss, ierror)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford        1/31/98
c     ******************************************************************
c     read new well locations, stress rates, conc, well char., and limits

c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
c      dimension MNWsite(mxwel2)
      dimension well2(17,mxwel2+1)
c      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
c      dimension hy(nodes)
      dimension rn(25), iowell2(3)
      common /rev23/ iwelpt
      COMMON /BCFCOM/LAYCON(200)
c      dimension hnew(nodes)
c      double precision hnew
      character*1  tab
      character*32 MNWSITE, TempSite
      CHARACTER*200 FNAME, MNWname                          !!08/19/02KJH-MODIFIED
      character*256 txt, tx2, txtRAW
c      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
c     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
c     &          LAYHDT(NLAY), TRPY(NLAY)
      SAVE HMAX
C
      tab = char(9)
      zero = 1.e-25
      Qfrcmn = zero
      Qfrcmx = zero
      Qreject  = 0.00000000000
      NQreject = 0
      NL = 0
      if( PLoss.gt.1.001 ) NL = 1  !!  Read NL loss Coefficient after Skin
c      small = hclose
c
c  Check for setting the HREFerence array
CERB     IN FIRST STRESS PERIOD, HOLD IS UNDEFINED, SO USE HNEW INSTEAD
c      IF (KPER.EQ.1) THEN
c        hmax = HNEW(1)
c        do n = 1, nodes
c          href(n) = HNEW(n)
c          if( abs(href(n)).gt.hmax ) hmax = abs(href(n))
c        enddo
c      ELSEif( kper.le.kspref ) then
c        hmax = hold(1)
c        do n = 1, nodes
c          href(n) = hold(n)
c          if( abs(href(n)).gt.hmax ) hmax = abs(href(n))
c        enddo
c      endif
c
c------------------------------------------------------------------
c     The 16 rows of the well array store:
c      Row #  = Description
c------------------------------------------------------------------
c         1   = Well node locator
c         2   = Desired flow rate
c         3   = Actual flow rate used
c         4   = Water Quality attribute to be averaged by flow
c         5   = Radius of wellbore
c         6   = Skin associated with well completion
c         7   = Minimum/Maximum head or drawdown
c         8   = Elevation of reference head for computing lift costs
c         9   = Water Quality Group identifier
c        10   = Water level in wellbore
c        11   = HCOF value / QWaverage
c        12   = RHS  value
c        13   = Minimum flow rate -  to turn off
c        14   = Minimum flow rate -- to turn on
c        15   = Reserve Desired flow rate
c        16   = Non-linear loss term
c        17   = Actual flow rate to individual nodes of a multi-node well
c               kept for transport or other purposes !!7/13/2003 - CZ
c------------------------------------------------------------------
c
c1------read itmp(number of wells or flag saying reuse well data)
      call ncread(in,txtRAW,ierr, ierror)
	if (ierror .ne. 0) return
      txt = txtRAW
      call UPCASE(txt)
      call qread(rn,1,txt,ierr)
      itmp  = rn(1)
c
      if( itmp.lt.0 ) then
c        if itmp less than zero reuse data. print message and return.
c        write(iout,6)
c    6   format(1h0,'REUSING MNW1  FROM LAST STRESS PERIOD')
        return
      else
c  If itmp > 0,  Test if wells are to replace old ones or be added.
c
        if( index(txt,'ADD').eq.0 ) nwell2 = 0
c
c   return if there are no wells to read ........
        if( itmp .eq. 0 ) return
c
c  Redundant well information is allowed in MNW1
c
c   Read additional well info
        nstart = nwell2
        do m = 1, itmp
          call ncread(in,txtRAW,ierr, ierror)
	    if (ierror .ne. 0) return
          txt = txtRAW
          call UPCASE(txt)
c   Attempt read with QREAD first
          call qread(rn,4,txt,ierr)
          if( ierr.eq.0 .and. rn(5).lt.0.5 ) then
            k = ifrl( rn(1) )
            j = ifrl( rn(2) )
            i = ifrl( rn(3) )
            q = rn(4)
            irmx = ifrl( rn(6) ) + 1
          else
c  Use fixed form reader if errors were detected
            read (txt(1:40),'(3i10,f10.0)') k, j, i, q
            irmx = 41
          endif
          node = (k-1)*ncol*nrow + (j-1)*ncol + i
c    Test for if well is in active grid ......
          iok = 1
          if(i.gt.ncol .or. j.gt.nrow .or. node.gt.nodes) iok = 0
c          DryTest = Hnew(node) - Hdry
c          if( iok.gt.0 .and. DryTest**2.gt.zero) iok = ibound(node)
c
c  Should MNW wells be allowed in specified-head cells?
          if( iok .ne. 0 ) then     !! Allow SH now, "gt" for no SH
c    Test for redundant info ......
            ipt = 0
c    The commented statements prevent having multiple MNW sites in the same cells
c            nt  = 0
c            do while (nt.lt.nwell2 .and. ipt.eq.0 )
c              nt = nt + 1
c              if( well2(1,nt).eq.node ) ipt = nt
c            enddo
            if( ipt.eq.0 ) then
              nwell2 = nwell2 + 1
              ipt    = nwell2
            endif
c
c    Assign data now that the pointer is set
            well2(1,ipt) = node
            well2(2,ipt) = q
            IPOLE = 0
            if( abs(q).gt.zero )  ipole = q / abs(q)
            well2(3,ipt) = well2(2,ipt)
            well2(13,ipt) = Qfrcmn        ! default lower limit
            well2(14,ipt) = Qfrcmx
c
c    Look for limit modifications
            kqc = index(txt,'QCUT')
            kpc = index(txt,'%CUT')
            if( kqc+kpc.gt.0 .and. abs(q).gt.zero) then
              tx2 = txt(kqc+kpc+5:256)
              call qread(rn,2,tx2,ierr)
              if( kqc.gt.0 ) then          !!  Absolute value was provided
                rn(1) = 100.* rn(1) / q    !!  Convert to percentage
                rn(2) = 100.* rn(2) / q
              endif
              if( ierr.ge.1 ) rn(2) = rn(1)
              well2(13,ipt) = rn(1) * 0.01   !! convert percentages to fractions
              well2(14,ipt) = rn(2) * 0.01
              if( index(tx2,'DEFAULT').gt.0 ) then
                Qfrcmn = rn(1) * 0.01        !!  New default lower limit
                Qfrcmx = rn(2) * 0.01        !!  New default upper limit
              endif
            endif
c
c    Look for NonLinear coefficient
            well2(16,ipt) = 0.000000      !!  NonLinear Loss Coefficient
            kCp = index(txt,'CP:')
            if( kCp.gt.0 .and. NL.gt.0 ) then
              tx2 = txt(kCp+3:256)
              call qread(rn,1,tx2,ierr)
              if( ierr.eq.0 ) then
                well2(16,ipt) = rn(1)
c         Could reset default C-term here to a non-zero value
              endif
            endif
c
c   Look for Site Identifier   -- Set to NO-PRINT  if not present.
            kSiteID = index(txt,'SITE')
            if( kSiteID.gt.0 ) then
              MNWsite = txtRAW(kSiteID+5:256)
              kblk = index(MNWsite,' ')
              ktab = index(MNWsite,tab)
              if( kblk.gt.0 ) Kfini = kblk
              if( ktab.gt.0 .and. ktab.lt.kblk) Kfini = ktab
              if(Kfini.le.32) then
                MNWsite(kFini:32)='                 '
              else
                Kfini = 32
              endif
              txt(kSiteID:kSiteID+kFini+4) = '                        '
            else
              MNWsite = 'NO-PRINT                     '
            endif
c
c    Read remaining info from card to set MNW1 specific parameters
            tx2 = txt(irmx:256)
            ki = index(tx2,'ZONE')
            if(ki.gt.0 )  tx2(ki:256)= '                         '
            call qread(rn,6,tx2,ierr)
c
c   Move from well data from temp to permanent locations
            do ip = 1, 6-ierr
              well2(ip+3,ipt) = rn(ip)
            enddo
            if( ierr.ge.1 ) well2(9,ipt) = ipt
c            if( ierr.ge.2 .or. abs(well2(8,ipt)).gt.hmax )
c     +          well2(8,ipt) = href(node)
c  Compute HLIM relative to reference elevation if HLIM read was a DrawDown (DD)
            if( index(txt,'DD').gt.0 )
     +        well2(7,ipt) = ipole*well2(7,ipt) + well2(8,ipt)
            if( ierr.ge.3 ) well2(7,ipt) = ipole * 1.0E+26
            if( ierr.ge.4 ) well2(6,ipt) =  0.0000
            if( ierr.ge.5 ) well2(5,ipt) =  0.0000
            if( ierr.ge.6 ) well2(4,ipt) = -1.0000
c  Flag as 2-point definition of a multi-node well if MULTI is detected.
            if( index(tx2,'MULTI') .gt. 0  .and.
     +          abs(well2(5,ipt))  .gt. zero  ) then
c  Define direction and # of points in well
              well2(2,ipt-1) = well2(2,ipt) + well2(2,ipt-1)
              n1 = ifrl( well2(1,ipt-1) )
              mstep = idirect( n1, node, ncol, nrow )
              do nn = n1+mstep, node, mstep
                ipt = ipt + 1
                nwell2 = nwell2 + 1
                well2(1,ipt) = nn
                well2(2,ipt) = 0.0000
                well2(3,ipt) = well2(2,ipt)
                well2(4,ipt) = well2(4,ipt-1)
                well2(5,ipt) = well2(5,ipt-1)
                well2(6,ipt) = well2(6,ipt-1)
                well2(16,ipt)= well2(16,ipt-1)  !!  NonLinear Loss Coefficient
                well2(9,ipt) = well2(9,ipt-1)
                well2(8,ipt) = -1.0E31
                well2(13,ipt) = 0.0000
                well2(14,ipt) = 0.0000
                icmn = icmn + 1
                well2(7,ipt) = icmn
              enddo
c  Flag as part of a multi-node well if MN is detected.
            elseif( index(tx2,'MN')  .gt. 0 .and.
     +              abs(well2(5,ipt)).gt.zero) then
c  Set to very large -value to flag MN status
                well2(8,ipt) = -1.0E31
              icmn = icmn + 1
                well2(7,ipt) = icmn
            else
              icmn = 1
            endif
          else
c   Sum details on rejected wells
            Qreject  = Qreject  + q
            NQreject = NQreject + 1
          endif    !   IBOUND test statement
        enddo      !   end of well entry loop
c
c   Process wells that are screened across multiple nodes
c
c Check for extreme contrast in conductance
c
        well2(8,nwell2+1) = 0.0000
        if( nstart.lt.1 )      nstart = 1
        if( nstart.gt.nwell2 ) nstart = nwell2 - itmp + 1
        do i = nstart, nwell2
          if( well2(8,i).lt.-1.E30 .and. well2(8,i+1).gt.-1.E30 .or.
     +        well2(8,i).lt.-1.E30 .and. i.eq.nwell2          ) then
            ngrp = ifrl( well2(7,i)      )
            ne   = i
            nb   = ne - ngrp + 1
            hlim = well2(7,nb)
            hrfw = well2(8,nb)
            qsum = 0.0000
            TempSite = 'NO-PRINT                     '
            do iin = nb, ne
              qsum = qsum + well2(2,iin)
c              if( MNWsite(iin)(1:8).ne. 'NO-PRINT' ) then
c                TempSite = MNWsite(iin)
c              endif
              well2(2,iin) = 0.000000
              well2(7,iin) = 1.0E31
              well2(8,iin) = 1.0E31
c   Set to very large +value to flag MN status
            enddo
c   Set All SiteIDs in a multinode well to a common tag
c            do iin = nb, ne
c              MNWsite(iin) = TempSite
c            enddo
            well2(7,nb) = ne
            well2(2,ne) = qsum
            well2(7,ne) = hlim
            well2(8,ne) = hrfw
          endif
        enddo   !   end of multi-well pointer setting
c
      endif
c
c  nwell2>mxwel2.  print message. stop.
      if( nwell2 .gt. mxwel2) then
c        write(iout,99) nwell2, mxwel2
c   99   format(1h0,'nwell2(',i4,') IS GREATER THAN mxwel2(',i4,')')
C
C       When compiling MNW with Modflow-96, comment out the call to
C       USTOP and uncomment the STOP statement
        CALL USTOP(' ')
C        STOP
C
      endif
c
c   Place desired flow rates in a reserved location
c
      do m = 1, nwell2
        well2(15,m) = well2(2,m)
      enddo
c
c   Echo input to iout file
c
      if (iwelpt.eq.0) then
c        if (NQreject.gt.0) then
c          txt = ' wells were outside of the model domain.'
c          write (iout,'(1h0,5x,i5,a50)')NQreject, txt
c          txt = 'The rejected pumpage totaled: '
c          write (iout,'(1h0,a34,g14.5)') txt, Qreject
c        endif
c
c        write (iout,'(1h0,10x,i5,10h MNW WELLS)') nwell2
c        write(iout,'(3x,4h No.,3x,3hLay,3x,3hRow,3x,3hCol,4x,6hStress,
c     +        3x,8hQW param,6x,2hRw,7x,4hSkin,4x,8hWL Limit,
c     +        4x,8hWL Refer,3x,12hNonLinear Cp,2x,8hQW Group,2x,
c     +        12hCell-To-Well,2x,8hMin-Qoff,2x,8hMin-Qon,2x,
c     +        16hSite Identifier  )')
c
        do m = 1, nwell2
          n = INT(well2(1,m))
          k = (n-1)/(ncol*nrow) + 1
          j = mod((n-1),ncol*nrow)/ncol + 1
          i = mod((n-1),ncol) + 1
          igrp = INT(well2(9,m))
c
          rw = well2(5,m)
          if( rw .lt. -zero ) then
            cond = -rw
          else
            Qact = well2(3,m)
            sk = well2(6,m)
            Cf = well2(16,m)
            cond = 0
c            cond = cel2wel(delr,delc,cr,cc,hy,hnew,ncol,nrow,nodes,n,rw,
c     &                 sk,Qact,Cf,PLoss,small,Hdry,LAYHDT,BOTM,NBOTM,HK,
c     &                     IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,HANI)
c            if( rw .lt. zero ) cond = cond * 1.0E3
          endif
          well2(11,m) = cond
c
c ---------Modified OUTPUT to hide internal pointers that "Look Funny" --KJH-- July 10, 2003
          if( well2(8,m) .gt. 1.0e30 )then
            if( well2(7,m) .lt. 1.0e30 )then
              ne = ifrl(well2(7,m))
              hlim = well2(7,ne)
              hrfw = well2(8,ne)
            else
            endif
          else
            hlim = well2(7,m)
            hrfw = well2(8,m)
          endif
c          write (iout,'(1x,4i6,6(1x,g10.4),g13.6,i10,g13.6,
c     +          2f10.3,2x,a32)')
c     +          m, k,j,i, (well2(ii,m),ii=3,6),hlim, hrfw,
c     +          well2(16,m), igrp, well2(11,m),
c     +          (well2(ii,m)*100.0, ii = 13,14), MNWsite(m)
c
        enddo
      else
c        write (iout,*) 'WELLS WILL NOT BE PRINTED'
      endif
c
c  Write blank fields in Auxillary BYNODE file if KPER=1 & IO>0
c
c      if (totim.lt.1e-26 .and. iowell2(2).ne.0 ) then
c        io = abs(iowell2(2))
c        do m = 1, nwell2
c          n = ifrl( well2(1,m) )
c          write (abs(iowell2(2)),'(a32,1x,2i8)')MNWsite(m),m, n
c        enddo
c      endif
c
c  Write blank fields in Auxillary QSUM file if KPER=1 & IO>0
c
c      if (totim.lt.1e-26 .and. iowell2(3).ne.0 ) then
c        io = abs(iowell2(3))
c        m = 0
c        do while( m .lt. nwell2 )
c          m = m + 1
c          if( well2(8,m) .gt. 1.0E30 ) then
c            ne  = ifrl( well2(7,m) )
cc            write (abs(iowell2(3)),'(a32,1x,i5.5,1h-,i5.5)')
c     +             MNWsite(m),m,ne
c            m = ne
c          endif
c        enddo
c      endif
c
      return
      end
c
c_________________________________________________________________________________
c
c      SUBROUTINE GWF1MNW1ad(nwell2,mxwel2,well2,ibound,delr,delc,cr,cc,
c     +               hy,small,Hdry,hnew, ncol, nrow, nodes,LAYHDT,BOTM,
c     &                NBOTM,HK,IUBCF,IULPF,IUHUF,NLAY,PLoss,TRPY,HKCC,
c     &                HANI)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford
c
c     ******************************************************************
c     Update Qact for wells that were constrained
c     ******************************************************************
c
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
c      dimension well2(17,mxwel2), ibound(nodes)
c      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
c      dimension hy(nodes)
c      dimension hnew(nodes)
c      COMMON /BCFCOM/LAYCON(200)
c      double precision hnew
c      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
c     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
c     &          LAYHDT(NLAY), TRPY(NLAY)
C
c      zero = 1.0E-8
c
c1------if number of wells <= 0 then return.
c      if(nwell2.le.0) return
c
c   Compute cell-to-well conductance for each well node
c
c      do m = 1, nwell2
c        n = ifrl( well2(1,m) )
c        qres = well2(15,m)
c-----if the cell is inactive or specified then bypass processing.
c        if( ibound(n).ne.0 ) then
c          rw = well2(5,m)
c          if( rw .lt. -zero ) then
c            cond = -rw
c          else
c            Qact = well2(3,m)
c            sk = well2(6,m)
c            Cf = well2(16,m)
c            cond = cel2wel(delr,delc,cr,cc,hy,hnew,ncol,nrow,nodes,n,rw,
c     &                 sk,Qact,Cf,PLoss,small,Hdry,LAYHDT,BOTM,NBOTM,HK,
c     &                     IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,HANI)
c            if( rw .lt. zero ) cond = cond * 1.0E3
c          endif
c          well2(11,m) = cond
c        endif
c      enddo
c
c   Allow constrained wells a new chance with the next time step
c
c      m = 0
c      do while( m .lt. nwell2 )
c        m = m + 1
c        qoff = well2(13,m)
c        qon  = well2(14,m)
c        qact = well2(3,m)
c        Qsmall = small
c
c   A very large # in WL reference array (8,m) triggers multi-node calculation
c
c        if( well2(8,m) .gt. 1.0E30 ) then
c     Compute hwell / Qpot for multi-node well
c          ne  = ifrl( well2(7,m) )
c          qdes = well2(15,ne)
c          csum = 0.000
c          chsum = 0.000
c          qact = 0.0000
c          Qsmall = small*abs(qdes)
c          do iin = m, ne
c            n = ifrl( well2(1,iin) )
c            if( ibound(n) .ne. 0 ) then
c              csum  = csum  + well2(11,iin)
c              chsum = chsum + well2(11,iin)*hnew(n)
c              qact  = qact  + well2( 3,iin)
c            else
c              qact  = 0.0000
c            endif
c          enddo
c---div0 ---  CSUM could go to zero if the entire well is dry
c          if( csum .gt. zero ) then
c            hwell = ( qdes + chsum ) / csum
c          else
c            hwell = hnew(n)
c          endif
c          m = ne
c   Test DD constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
c          ipole = 0
c          if( abs(qdes).gt.zero ) ipole = qdes / abs(qdes)
c          hlim = well2(7,ne)
c          href = well2(8,ne)
c          ddmax = ipole*( hlim - href )
c          ddsim = ipole*( hwell - href )
c          qpot = hlim*csum - chsum
c          if( ddsim .gt. ddmax ) then
c            hwell = hlim
c            qpot = hwell*csum - chsum
c          endif
c          cond = csum
c        else       !  End of multi-node conditioning IF statement
c     Compute hwell / Qpot for single-node well
c          n = ifrl( well2(1,m) )
c          cond = well2(11,m)
c          hlim = well2(7,m)
c          qpot = ( hlim - hnew(n) )*cond
c          qdes = well2(15,m)
c        endif
c
c  Compute ratio of potential/desired flow rates
c        ratio = 1.00
c        if( abs(qdes) .gt. small ) ratio =  qpot / qdes
c        if( ratio .gt. 0.9999 ) then
c          ratio =  1.000
c          Qpot = Qdes
c        endif
c  Check if potential flow rate is below cutoff
c        if( ratio .lt. Qoff ) then
c          Qact = 0.000
c          Qdes = Qact
c          well2(2,m) = Qdes
c          well2(3,m) = Qact
c  Check if potential flow rate is above restart threshold
c        elseif( ratio.gt.Qon .and. abs(qact).lt.Qsmall ) then
c          Qdes = well2(15,m)
c          well2(2,m) = Qdes
c          well2(3,m) = Qpot
c        else
c  Otherwise leave the flow rate alone
c        endif
c
c      enddo   ! End of overall test loop
c
c      return
c      end
c
c_________________________________________________________________________________
c
c      SUBROUTINE GWF1MNW1fm(nwell2,mxwel2,well2,ibound,delr,delc,cr,cc,
c     +    hy,small,Hdry, hcof, rhs, hnew, ncol, nrow, nodes,kiter,
c     +    NoMoIter,LAYHDT,BOTM,NBOTM,HK,IUBCF,IULPF,IUHUF,NLAY,
c     &    PLoss,TRPY,HKCC,HANI)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford
c
c     ******************************************************************
c     add well flow to source term
c     ******************************************************************
c
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
c      dimension well2(17,mxwel2), ibound(nodes)
c      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
c      dimension hy(nodes)
c      dimension hcof(nodes), rhs(nodes)
c      dimension hnew(nodes)
c      double precision hnew
c      COMMON /BCFCOM/LAYCON(200)
c      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
c     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
c     &          LAYHDT(NLAY), TRPY(NLAY)
C
c      zero = 1.0E-20
c
c                 CR( i, j, k)    ------>   CR  i + 1/2
c                 CC( i, j, k)    ------>   CC  j + 1/2
c                 CV( i, j, k)    ------>   CV  k + 1/2
c
c1------if number of wells <= 0 then return.
c      if(nwell2.le.0) return
c
c   Compute cell-to-well conductance for each well node
c
c      do m = 1, nwell2
c        n = ifrl( well2(1,m) )
c        qres = well2(15,m)
c-----if the cell is inactive or specified then bypass processing.
c        if( ibound(n).ne.0 ) then
c          rw = well2(5,m)
c          if( rw .lt. -zero ) then
c            cond = -rw
c          else
c            Qact = well2(3,m)
c            sk = well2(6,m)
c            Cf = well2(16,m)
c            cond = cel2wel(delr,delc,cr,cc,hy,hnew,ncol,nrow,nodes,n,rw,
c     &                 sk,Qact,Cf,PLoss,small,Hdry,LAYHDT,BOTM,NBOTM,HK,
c     &                     IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,HANI)
c            if( rw .lt. zero ) cond = cond * 1.0E3
c          endif
c          well2(11,m) = cond
c        endif
c      enddo
c
c   Prepare components and limits of a multi-node well
c      m = 0
c      do while( m .lt. nwell2 )
c        m = m + 1
c        well2(10,m) = 1.0E31
c
c   A very large # in WL reference array (8,m) triggers multi-node calculation
c
c        if( well2(8,m) .gt. 1.0E30 ) then
c          ne  = ifrl( well2(7,m) )
c          qdes = well2(2,ne)
c          qact = qdes
c          csum = 0.000
c          chsum = 0.000
c          do iin = m, ne
c            n = ifrl( well2(1,iin) )
c            if( ibound(n) .ne. 0 ) then
c              csum  = csum  + well2(11,iin)
c              chsum = chsum + well2(11,iin)*hnew(n)
c            else
c              well2(3,iin) = 0.00000000
c            endif
c          enddo
c---div0 ---  CSUM could go to zero if the entire well is dry
c          if( csum .gt. zero ) then
c            hwell = ( qact + chsum ) / csum
c          else
c            hwell = hnew(n)
c          endif
c
c   Test DD constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
c          ipole = 0
c          if( abs(qdes).gt.zero ) ipole = qdes / abs(qdes)
c          hlim = well2(7,ne)
c          href = well2(8,ne)
c          ddmax = ipole*( hlim - href )
c          ddsim = ipole*( hwell - href )
c
c          if( ddsim .gt. ddmax ) then
c            hwell = hlim
c            qact = hwell*csum - chsum
c      DD constraints that stop production are not tested until after the 2nd iteration
c            if( kiter .gt.2 ) then
c              ratio = 1.00
c              if( abs(qdes) .gt. small ) ratio =  qact / qdes
c              if( ratio .lt. 0.00001 ) then
c                qact  = 0.000
c                hwell = chsum / csum
c              endif
c            endif
c          endif
c
c   Assign flow rates and water levels to individual nodes
c          do iin = m, ne
c            n = ifrl( well2(1,iin) )
c            well2(10,iin) = hwell
c            qact = ( hwell - hnew(n) ) * well2(11,iin)
c            well2(3,iin) = qact
c          enddo
c          m = ne
c        endif       !  End of multi-node conditioning IF statement
c      enddo       ! End of overall multi-node test loop
c
c2------process each well in the well list.
c      m = 0
c      do while( m .lt. nwell2 )
c        m = m + 1
c        n = ifrl( well2(1,m) )
c        qdes = well2(2,m)
cc-----if the cell is inactive then bypass processing.
c        if( ibound(n).gt.0 ) then
c          qact = well2(3,m)
c          cond = well2(11,m)
cc
c          hlim = well2(7,m)
c          href = well2(8,m)
c          if( well2(10,m).gt.1.0E30 .and. cond.gt.zero ) then
c            dhc2w = Qact / cond
cc   Process single-node wells
cc   Test DD constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
c            ipole = 0
c            if( abs(qdes).gt.zero ) ipole = qdes / abs(qdes)
c            hwell = hnew(n) + dhc2w
c            well2(10,m) = hwell
c            ddsim = ipole*( hwell - href )
c            ddmax = ipole*( hlim - href ) - small
c            ratio = 1.00
c            if( abs(qdes) .gt. zero ) ratio =  qact / qdes
c            if( abs(ratio).gt. 1.00 ) qact = qdes
c            if( ratio     .lt. zero ) qact = 0.000000
cc    Well will be simulated as a specified rate or GHB
c            iqslv = 0
c            if( ddsim.gt.ddmax .and. ddmax.gt.zero ) iqslv = 1
c            if((qdes-qact)**2 .gt. small           ) iqslv = 1
c            if(abs(qact).lt.zero .and.  ddsim.gt.ddmax) iqslv = 0
c            if(abs(qact).lt.zero .and.  ddsim.lt.ddmax) iqslv = 1
c            if(abs(qdes).lt.zero .or. ratio.gt.1.0-zero ) iqslv = 0
cc
c          elseif( cond.lt.zero ) then
c            qact = 0.00000
c            iqslv = 0
c          else
cc Process multi-node wells, Constraints were already tested when allocating flow
c            if( mod(kiter,2).eq.0 .and. abs(qact).gt.small ) then
c              hlim = well2(10,m)
c              iqslv = 1
c            else
c              qact = well2(3,m)
c              iqslv = 0
c            endif
c          endif
cc
cc   Modify HCOF and RHS arrays
c          if( iqslv.ne.0 .and. kiter.gt.1 .and. kiter.lt.NoMoIter ) then
c            qact = ( hlim - hnew(n) ) * cond
c            hcof(n) = hcof(n) - cond
c            rhs(n)  = rhs(n)  - cond * hlim
c          else
cc  Specify Q and solve for head;  add Q to RHS accumulator.
c            rhs(n) = rhs(n) - qact
c          endif
c          well2(3,m) = qact
c        endif
c      enddo       !    End of DO WHILE loop
c
c      return
c      end
c
c_________________________________________________________________________________
c
c      SUBROUTINE GWF1MNW1bd(MNWsite,nwell2,mxwel2,vbnm,vbvl,msum,delt,
c     +        well2,ibound,hnew,ncol,nrow,nodes,nstp,kstp,kper,iwl2cb,
c     +             icbcfl,buff,iout,iowell2,totim,PLoss,Hdry)
cC     VERSION 20030710 KJH
cc
cc----- MNW1 by K.J. Halford        1/31/98
cc     ******************************************************************
cc     calculate volumetric budget for wells
cc     ******************************************************************
cc
cc        specifications:
cc     ------------------------------------------------------------------
c      dimension MNWsite(mxwel2)
c      dimension vbvl(4,msum),well2(17,mxwel2),
c     1          ibound(nodes), buff(nodes)
c      dimension iowell2(3)
c      dimension hnew(nodes)
c      double precision hnew
cc
c      character*16 text,vbnm(msum),AUXTXT(5)
c      character*32 MNWsite
cc             ----+----1----+-
c      text = '             MNW'
c      zero = 1.E-25
cc     ------------------------------------------------------------------
cc
cc  clear ratin and ratout accumulators.
c      ratin=0.
c      ratout=0.
c      ibd=0
c      IF(IWL2CB.GT.0) IBD=ICBCFL
cC
cC2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
c      IF(IBD.EQ.2) THEN
c         NAUX = 0   !!   Set to zero -- Change order to dump
cc         IF(IAUXSV.EQ.0) NAUX=0
c         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,auxtxt,IWL2CB,NCOL,NROW,NLAY,
c     1          NWELL2,IOUT,DELT,PERTIM,TOTIM,IBOUND)
c      END IF
cc  clear the buffer.
c      do n = 1, nodes
c        buff(n)=0.000000000
c      enddo
cc -----print the header for individual rates if requested(iwl2cb<0).
cc      if( iwl2cb.lt.0 .and. icbcfl.ne.0 ) then
cc        write(iout,'(/,1x,a16,9h PERIOD =,i5,8h  STEP =,i5)')
cc     +              text, kper,kstp
cc        write(iout,900)
cc      endif
c  900 format(1x,6h Entry,4h LAY,4h ROW,4h COL,
c     + 9x,1hQ,6x,6hH-Well,7x,6hH-Cell,7x,6hDD    ,7x,6hQW-Avg,
c     + 6x,8hs-LINEAR,3x,11hs-NonLINEAR)
c
cc  Create WEL1 file if iowell2(1) > 0
c      if( iowell2(1).gt.0 .and. nstp.eq.kstp )
c     +       write(iowell2(1),'(1i10)') nwell2
c
c2------if there are no wells do not accumulate flow
c      if(nwell2.gt.0) then
cc
cc     Compute flow weighted QW values and store in well2(11,m)
cc
c        do m = 1, nwell2
c          well2(11,m) = well2(3,m) * well2(4,m)
c          well2(12,m) = 0.000
c          if( well2(4,m).lt.0.00 .or. well2(3,m).gt.0.00 ) then
c            well2(11,m) = -1
c            well2(12,m) =  1
c          endif
c        enddo
cc
c        do m = 1, nwell2
c          igrp1 = ifrl( well2(9,m) )
c          if( well2(12,m) .lt. 0.5 ) then
c            qwsum = 0.0000
c            qsum = 0.0000
c            do m2 = m, nwell2
c              igrp2 = ifrl( well2(9,m2) )
c              if( igrp1.eq.igrp2 .and. well2(12,m2).lt.0.5) then
c                qwsum = qwsum + well2(11,m2)
c                qsum  = qsum  + well2(3,m2)
c                well2(12,m2) = 1
c              endif
c            enddo
cc
c            qwbar = qwsum
c            if( qsum**2.gt.zero ) qwbar = qwsum / qsum
c            do m2 = m, nwell2
c              igrp2 = ifrl( well2(9,m2) )
c              if( igrp1.eq.igrp2 .and. well2(4,m2).ge.0.0 )
c     +            well2(11,m2) = QWbar
c            enddo
c          endif
c        enddo
cc
c        imult = 0
c        do m = 1,nwell2
c          n = ifrl( well2(1,m) )
c          DryTest = Hnew(n) - Hdry
c          if(DryTest**2.lt.zero) then
c            well2(3,m) = 0.0000
c          endif
c          q = well2(3,m)
c          well2(17,m)=q     !!7/13/2003 - CZ: preserve q
cc
cc    Report all wells with production less than the desired rate......
c          if(ibound(n).ne.0 .or. DryTest**2.lt.zero) then
c            il = (n-1) / (ncol*nrow) + 1
c            ir = mod((n-1),ncol*nrow)/ncol + 1
c            ic = mod((n-1),ncol) + 1
c            qd = well2(2,m)
c            hlim = well2(7,m)
c -----Modified OUTPUT to hide internal pointers that "Look Funny" in DD column--KJH-- July 10, 2003
c            if( well2(8,m) .gt. 1.0e30 )then
c              imult = 1
c              if( well2(7,m) .lt. 1.0e30 )then
c                ne = ifrl(well2(7,m))
c                href = well2(8,ne)
c              else
c              endif
c            else
c              href = well2(8,m)
c            endif
c            hwell = well2(10,m)
c            QWbar = well2(11,m)
c            dd  = hwell - href
cc
c            ioch = 0
c            if( iwl2cb.lt.0 .and. icbcfl.ne.0 ) ioch = 1
cc -----print the individual rates if requested(iwl2cb<0).
c            if( ioch.eq.1 ) then
c              s   = hnew(n) - hwell
c              IPOLE = 0
c              if( abs(s).gt.zero )  ipole = s / abs(s)
c              sNL = ipole * well2(16,m) * q**PLoss
c              sL  = s - sNL
cc              write(iout,'(1x,i6,3i4,9(1x,g12.6))')
cc     +         m,il,ir,ic,q, hwell,hnew(n), dd, qwbar, sL, sNL
c            endif
cc
c -----print the individual rates to auxillary file if requested(iwl2cb<0).
c            iobynd = abs(iowell2(2))
c            if( iobynd.gt.0 ) then
c              if(  ioch.eq.1 .or. iowell2(2).lt.0)then
cc                write(iobynd,'(a32,1x,2i8,6(1x,g14.8))')
cc     +          MNWsite(m),m,n,totim,q, hwell,hnew(n), qwbar
c              endif
c            endif
cc  Create WEL1 file if iowell2(1) > 0
c            if( iowell2(1).gt.0 .and. nstp.eq.kstp ) then
cc              write(iowell2(1),'(i9,2i10,1x,g10.4,i10,2x,6(1x,g10.4))')
cc     +        il,ir,ic,q,0,qd, hwell, hnew(n), dd,href,qwbar
c            endif
cc
c            buff(n) = buff(n) + q
c            if( q.ge.0.000 ) then
cc -----pumping rate is positive(recharge). add it to ratin.
c              ratin = ratin + q
c            else
cc -----pumping rate is negative(discharge). add it to ratout.
c              ratout = ratout - q
c            endif
c          endif
c        enddo
cc
cc   Sum components of  multi-node wells
cc
cc -----print the header for multi-node rates if requested(iwl2cb<0).
c        if(  ioch.eq.1  .and. imult.eq.1 ) then
c          write(iout,'(/,5x,31h Multi-Node Rates & Average QW )')
c          write(iout,901)
c  901     format(1x,16hSite Identifier ,5x,18hENTRY: Begin - End,
c     +     2x,7hQ-Total,7x,6hH-Well,7x,6hDD    ,7x,6hQW-Avg)
c        endif
c
c        m = 0
c        do while( m .lt. nwell2 )
c          m = m + 1
c          if( well2(8,m) .gt. 1.0E30 ) then
c            ne  = ifrl( well2(7,m) )
c            qwsum = 0.000
c            qwfsum = 0.000
c            qsum = 0.000
c            qin  = 0.000
c            qout = 0.000
c            do iin = m, ne
c              n = ifrl( well2(1,iin) )
c              if( ibound(n).eq.0 ) well2(3,iin) = 0.0
c              if( well2(4,iin).ge.0.0 .and. well2(3,iin).le.0.0 ) then
c                qwfsum  = qwfsum + well2(3,iin)
c                qwsum   = qwsum  + well2(3,iin)*well2(4,iin)
c              endif
c              if( well2(3,iin).le.0.0 ) then
c                qin = qin  + well2(3,iin)
c              else
c                qout = qout  + well2(3,iin)
c              endif
c              qsum  = qsum  + well2(3,iin)
c              well2(3,iin) = 0.00000
c            enddo
c            well2(3,ne) = qsum
cc -----print the summed rates if requested(iwl2cb<0).
c            qwbar = well2(4,ne)
c            if(qwfsum**2 .gt. zero ) qwbar = qwsum / qwfsum
c            href = well2(8,ne)
c            hwell = well2(10,ne)
c            dd  = hwell - href
cc            if(  ioch.eq.1  ) then
cc              write(iout,'(A26,1x,2i6,6(1x,g12.6))')
cc     +         MNWsite(m),m,ne,qsum, hwell, dd, qwbar
cc            endif
cc -----print the summed rates to auxillary file if requested .
c            ioQsum = abs(iowell2(3))
c            if( ioQsum.gt.0 ) then
cc              if(  ioch.eq.1 .or. iowell2(3).lt.0)then
cc                write(ioQsum,102)
cc     +          MNWsite(m),m,ne,totim,qin,qout, qsum, hwell, qwbar
cc  102           format(A32,1x,i5.5,1h-,i5.5,12(1x,g14.8))
cc              endif
c            endif
c            m = ne
c          endif
c        enddo
cc        if(  ioch.eq.1  .and. imult.eq.1 ) then
cc          write(iout,*)
cc        endif
cc
cc  ----- END  MULTI-NODE reporting section -------------
cc
c        nlay = nodes / ncol / nrow
cc6------if cell-by-cell flows will be saved call ubudsv to record them
c        if( abs(iwl2cb).gt.0 .and. icbcfl.ne.0 ) then           !! BooBoo Fix--July 10,2003  KJH
c          ioc = abs(iwl2cb)
c          if( ibd.eq.2 ) then   !!  Write COMPACT budget
c            NWELVL  = 1  !!  Dummy value
c            do m = 1, nwell2
c              n = ifrl( well2(1,m) )
c              q = well2(3,m)
c              call UBDSVB(ioc,ncol, nrow,n,1,1,Q,well2(1,m),
c     +                    NWELVL,NAUX,5,IBOUND,NLAY)
c            enddo
c          else                  !!  Write full 3D array
c            call ubudsv(kstp,kper,text,ioc, buff,ncol,nrow,nlay,iout)
c          endif
c        endif
c      endif
cc
cc7------move rates into vbvl for printing by module bas1ot.
c      vbvl(3,msum)=ratin
c      vbvl(4,msum)=ratout
cc
cc8------move rates times time step length into vbvl accumulators.
c      vbvl(1,msum) = vbvl(1,msum) + ratin*delt
c      vbvl(2,msum) = vbvl(2,msum) + ratout*delt
cc
cc9------move budget term labels into vbnm for printing.
c      vbnm(msum) = text
c
c10-----increment budget term counter(msum).
c      msum = msum + 1
c
c11-----return
c      return
c      end
c
c_________________________________________________________________________________
c
c      subroutine GWF1MNW1OT(MNWsite,well2,nwell2,mxwel2,iowell2,OUTname)
cC     VERSION 20020819 KJH
cc
cc     ******************************************************************
cc    Sort well output into useful tables
cc     ******************************************************************
cc
cc        specifications:
cc     ------------------------------------------------------------------
c      dimension MNWsite(mxwel2)
c      dimension well2(17,mxwel2)
c      dimension iowell2(3)
c      character*1  tab
c      character*32  MNWsite, TempTag,TT, LastTag, EOFtag
c      character*200 OUTname
cc
c      tab = char(9)
c      zero = 1.E-25
c      IOstart = 1000
c      EOFtag = 'EndOfFile__EndOfFile__EndOfFile_'
cc   Set Flag for printing header info once
c      do i = 1, mxwel2
c        well2(16,i)= 0     !! 16 = Header Flag
c      enddo
cc
cc   Site file names are constructed to be OUTname_MNWsite.txt
cc   All info for a well are dumped as tab delimited output
c
c------------------------------------------------------------------
c     The 16 rows of the well array store:
c      Row #  = Description
c------------------------------------------------------------------
c         1   = Well node locator
c         2   = Net Discharge
c         3   = Water level in wellbore
c         4   = Water Quality
c         5   = Qin  ---------------MNW ONLY--------
c         6   = Qout
c         7   = Q-node   / Net Discharge
c         8   = MN-Flag --- Number of nodes / well
c         9   = I/O unit for well output
c        16   = Header Flag   Print= 0 / NoPrint = 1
c------------------------------------------------------------------
c
c   Test auxillary output files for cleaning data sets
c      iobynd = abs(iowell2(2))
c      if( iobynd.gt.0 ) then
cc        write(iobynd,'(a32)') EOFtag
c        rewind(iobynd)
c        read(iobynd,'(a)')
c      else
c        return
c      endif
cc
c      ioQsum = abs(iowell2(3))
c      if( ioQsum.gt.0 ) then
cc        write(ioQsum,'(a32)') EOFtag
c        rewind(ioQsum)
c        read(ioQsum,'(a)')
c      else
c        return
c      endif
cc
c      nwell2= 0
c      iCNT  = 0
c      iSTOP = 0
c      Tnow  = -1.0000
c      LastTag = 'NO-PRINT'
cc
c      do while( iSTOP.eq.0 )
c        read(iobynd,'(a32,1x,2i8,6g15.8)')
c     +     TempTag,mE,node,TimeIN,Q, hwell,hcell, Conc
c
c   Test for output before accumulating INFO
c        if(LastTag(1:8).ne.'NO-PRINT' .and. TempTag.ne.LastTag )then  !! Output
c          IOT = ifrl(well2(9,1))
cc   Write a Header ?????
c          ioPT = IOT - IOstart
c          if( ifrl(well2(16,ioPT)).eq.0 ) then
c            well2(16,ioPT) = 1
cc            if( iCNT.gt.1 ) then
cc              write(IOT,
cc     +        '(a4,a1,a9,a1,a6,a1,a13,a1,a10,a1,a11,99(a1,a4,i7.7))')
cc     +        'TIME',tab,'Discharge',tab,'H-Well',tab,'Concentration',
cc     +         tab,'Net-Inflow',tab,'Net-Outflow',
cc     +         (tab,'Node',ifrl(well2(1,i)), i=1,ICNT)
cc            else
cc              write(IOT,'(a4,a1,a9,a1,a6,a1,a13)')
cc     +        'TIME',tab,'Discharge',tab,'H-Well',tab,'Concentration'
cc            endif
c          endif
cc   END of "Write a Header"  Section
c          Hwell = well2(3,iCNT)
c          Conc  = well2(4,iCNT)
c          if( iCNT.gt.1 ) then
c            Qt   = well2(7,1)
c            Qin  = well2(5,1)
c            Qout = well2(6,1)
cc            write(IOT,'(99(g15.8,a1))')
cc     +      well2(10,1),tab, Qt,tab, Hwell,tab, conc,
cc     +      tab,Qin, tab,Qout, (tab,well2(2,i),i=1,iCNT)
c          else
c            Qt = well2(2,1)
cc            write(IOT,'(99(g15.8,a1))')
cc     +      well2(10,1),tab, Qt,tab, Hwell,tab, conc
cc
c          endif
c          iCNT = 0     !! RESET node counter
c        endif
c
cc   Is this the EOF?
c        if( TempTag .eq. EOFtag ) iSTOP = 1
cc
cc   Skip if this is a NO-PRINT node
c        if( TempTag(1:8).eq.'NO-PRINT' .or. TempTag.eq.EOFtag )then
c          iCNT = 0
c        else
c          iCNT = iCNT + 1
cc   Identify pointer
c          call ioWellOUT(TempTag,OUTname,nwell2,
c     +           mxwel2,MNWsite,IOstart,io)
c          well2(1,iCNT) = node       !!  1 = Well node locator
c          well2(2,iCNT) = Q          !!  2 = Net Discharge
c          well2(3,iCNT) = Hwell      !!  3 = Water level in wellbore
c          well2(4,iCNT) = Conc       !!  4 = Water Quality
c          well2(8,1) = iCNT       !!  8 = MN-Flag --- Number of nodes / well
c          well2(9,1) = io+IOstart !!  9 = IO output
c          well2(10,1)= TimeIN     !! 10 = Time
c
c     Read MN well output if available
c          if( TempTag.eq.LastTag )then    !! MN well
c            if( iCNT.eq.2 .and. ioQsum.gt.0 ) then
c              read (ioQsum,102)
c     +        TT, nb,ne, TimMULT,qin,qout, qsum, Hwell, qwbar
c  102         format(A32,1x,i5.5,1x,i5.5,12(1x,g14.8))
c              well2(4,1) = QWbar       !!  4 = Water Quality
c              well2(5,1) = Qin        !!  5 = Qin  ---------------MNW ONLY--------
c              well2(6,1) = Qout       !!  6 = Qout
c              well2(7,1) = Qsum       !!  7 = Qnet
c            endif
c          endif
c        endif
cc
cc   Save Value of TEMPTAG for comparison
c        LastTag = TempTag
c      enddo
cc
cc   Add IO close routine here if needed
cc
c      return
c      end
cc
cc_________________________________________________________________________________
c
c      subroutine ioWellOUT(TempTag,OUTname,nwell2,
c     +           mxwel2,MNWsite,IOstart,io)
c      dimension MNWsite(mxwel2)
c      character*32  MNWsite, TempTag
c      character*200 OUTname
c      character*256 txt
cc
c      i = 0
c      io = 0
c      do while( i.lt.nwell2 )
c        i = i + 1
c        if( MNWsite(i).eq.TempTag ) then
c          io = i
c          i = nwell2
c        endif
c      enddo
cc
c      if( io.eq.0 ) then
c        nwell2 = nwell2 + 1
c        MNWsite(nwell2) = TempTag
c        io = nwell2
cc   Build output file name and Open file
c        k1 = index(OUTname,' ') - 1
c        k2 = index(TempTag,' ') - 1
c        if( k2.eq.0 .and. TempTag(32:32).ne.' ' ) k2 = 32
c        txt = OUTname(1:k1)//'.'//TempTag(1:k2)//'.txt'
c        open(io+IOstart,file=txt)
c      endif
c
c      return
c      end
c
c_________________________________________________________________________________
c
c      function cel2wel(delr,delc,cr,cc,hy,hnew,
c     +                 ncol,nrow,nodes,n,rw,skin,Q,Cf,PLoss,small,Hdry,
c     &                 LAYHDT,BOTM,NBOTM,HK,IUBCF,IULPF,IUHUF,NLAY,TRPY,
c     &                 HKCC,HANI)
cC     VERSION 20030327 KJH        -- Patched Hyd.K term in LPF solution
cc
cc----- MNW1 by K.J. Halford
cc
cc     ******************************************************************
cc     Compute conductance term to define head loss from cell to wellbore
cc      Methodology is described in full by Peaceman (1983)
cc     ******************************************************************
cC     Note: BCF, when LAYCON=0 or 2, does not save cell-by-cell
cC     Transmissivity (T) values.  Instead, it converts the cell-by-cell
cC     T values to branch conductances CR and CC, using harmonic
cC     averaging.  When BCF is used, the method used in this routine to
cC     generate cell-specific values of Tx and Ty is an approximation
cC     based on CR and CC.  When LPF or HUF is used, cell-by-cell
cC     hydraulic-conductivity values are stored, this approximation is
cC     not needed, and the values generated for Tx and Ty are exact --
cC     ERB 1/29/01.
cC
cC        SPECIFICATIONS:
cC     ------------------------------------------------------------------
c      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
c      dimension hy(nodes)
c      dimension hnew(nodes)
c      double precision hnew
c      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
c     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
c     &          LAYHDT(NLAY), TRPY(NLAY)
c      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
c      COMMON /BCFCOM/LAYCON(200)
c      COMMON /LPFCOM/LAYTYP(200),LAYAVG(200),CHANI(200),LAYVKA(200),
c     1               LAYWET(200)
c      REAL KY
cC     ------------------------------------------------------------------
c 1000 FORMAT(/1X,
c     &'***ERROR: MNW1 PACKAGE DOES NOT SUPPORT HEAD-DEPENDENT',/,
c     &' THICKNESS OPTION OF SELECTED FLOW PACKAGE',/,
c     &' (MNW1 DOES FULLY SUPPORT BCF, LPF, AND HUF PACKAGES)',/,
c     &' -- STOP EXECUTION (CEL2WEL)')
cC
c      pi = 3.141592654
c      zero = 1.e-25
cC
c      ix   = mod((n-1),ncol)+1
c      dx   = delr(ix)
c      iy   = mod((n-1),ncol*nrow)/ncol + 1
c      dy   = delc(iy)
c      iz   = int((n-1)/(ncol*nrow)) + 1
c      top = BOTM(IX,IY,LBOTM(IZ)-1)
c      bot = BOTM(IX,IY,LBOTM(IZ))
cC
C     FIND HORIZONTAL ANISOTROPY, THE RATIO Ky/Kx
c      AH = 1.0
c      IF (IULPF.GT.0) THEN
c        IF (CHANI(IZ).GT.0.0) THEN
c          AH = CHANI(IZ)
c        ELSE
c          AH = HANI(IX,IY,IZ)
c        ENDIF
c      ELSEIF (IUHUF.GT.0) THEN
c        TempKX = HK(N)
c        KY = HKCC(IX,IY,IZ)
c        AH = KY/TempKX
c      ELSEIF (IUBCF.GT.0) THEN
c        AH = TRPY(IZ)
c      ENDIF
cC
c      if (LAYHDT(IZ).EQ.0) then
cC       THICKNESS IS NOT HEAD-DEPENDENT
c        IF (IULPF.EQ.0 .AND. IUHUF.EQ.0) THEN
cC         BCF OR ANOTHER FLOW PACKAGE, OTHER THAN LPF OR HUF, IS ACTIVE
c          dxp  = dx
c          Txp  = 0.00000
c          if( ix .lt. ncol ) then
c            dxp = delr(ix+1)
c            Txp  = cr(n) * (dx+dxp) / 2
c          endif
c          dxm = dx
c          Txm  = Txp
c          if( ix .gt. 1  ) then
c            dxm = delr(ix-1)
c            Txm  = cr(n-1) * (dx+dxm) / 2
c          endif
c          if( Txp.lt.small ) Txp = Txm
c          if( Txm.lt.small ) Txm = Txp
cc
c          dyp  = dy
c          Typ  = 0.00000
c          if( iy .lt. nrow ) then
c            dyp = delc(iy+1)
c            Typ  = cc(n) * (dy+dyp) / 2
c          endif
c          dym = dy
c          Tym  = Typ
c          if( iy .gt. 1 ) then
c            dym = delc(iy-1)
c            Tym  = cc(n-ncol) * (dy+dym) / 2
c          endif
c          if( Typ.lt.small ) Typ = Tym
c          if( Tym.lt.small ) Tym = Typ
c          Txp = Txp / dy
c          Txm = Txm / dy
c          Typ = Typ / dx
c          Tym = Tym / dx
cc
cc  Eliminate zero values .....
c
c          if( Typ.lt.small .or. nrow.lt.2 )  then
c            Typ = Txp
c            Tym = Txm
c          endif
cc
c          if( Txp.lt.small .or. ncol.lt.2 )  then
c            Txp = Typ
c            Txm = Tym
c          endif
cc
cc   Assuming expansion of grid is slight, if present, & that Txx and Tyy of the adjacent
cc  cells are about the same value.
c          Txx = 0.00000000
c          div  = Txp + Txm
c          if( div.gt.small ) Txx  = 2*Txp*Txm / div
c          Tyy = 0.00000000
c          div  = Typ + Tym
c          if( div.gt.small ) Tyy  = 2*Typ*Tym / div
c          if( Txx.gt.small .and. Tyy.lt.small ) Tyy = Txx
c          if( Tyy.gt.small .and. Txx.lt.small ) Txx = Tyy
c        ELSE
cC         LPF OR HUF IS ACTIVE
c          THICK = TOP-BOT
c          TXX = HK(N)*THICK
c          TYY = TXX*AH
c        ENDIF
c      else
C       THICKNESS IS HEAD-DEPENDENT
c  Estimate T to well in an unconfined system
c
c        upper = hnew(n)
c        TempKX = hy(n)        !!  BCF Hydraulic Conductivity array
c        IF (IUBCF.GT.0) THEN
c          if (LAYCON(IZ).EQ.3) then
c            if( upper.gt.top ) upper = top
c          endif
c        ELSEIF (IULPF.GT.0 .OR. IUHUF.GT.0) THEN
c          if( upper.gt.top ) upper = top
c          TempKX = hk(n)        !!  LPF Hydraulic Conductivity array
c        ELSE
c        ENDIF
c        thick = upper - bot
cc   set thickness / conductance to 0 if cell is dry
c        if( (hnew(n)-Hdry )**2 .lt. zero ) thick = 0.000000000000
c        Txx = TempKX * thick
c        if( Txx .lt.zero ) Txx = 0.000000000000000
c        Tyy = Txx * AH
c      endif
cc
c      if( rw.lt.zero .or. Txx.lt.zero .or. Tyy.lt.zero ) then
c        cel2wel = ( Txx * Tyy )** 0.5
c      else
c        yx4 = (Tyy/Txx)**0.25
c        xy4 = (Txx/Tyy)**0.25
c        ro = 0.28 *((yx4*dx)**2 +(xy4*dy)**2)**0.5 / (yx4+xy4)
cc
c        Tpi2 = 2*pi * (Txx*Tyy)**0.5
c        A = alog(ro/rw) / Tpi2
c        if( Ploss .gt. 0.99 ) then
c          B = Skin
c          C = Cf * abs(Q)**(PLoss-1)
c        else
c          B = Skin / Tpi2
c          C = 0.000000000
c        endif
c        cel2wel = A + B + C
c        cel2wel = 1.000000 / cel2wel
c      endif
c
c      return
c      end
c
c_________________________________________________________________________________
c
      integer function idirect(n1, n2, ncol,nrow)
c
c     ******************************************************************
c     Define direction of pointer along a row, column, or layer
c     ******************************************************************
c
      idirect = ncol
      if( abs( n2-n1 ) .gt. ncol*nrow ) idirect = ncol*nrow
      if( abs( n2-n1 ) .lt. ncol      ) idirect = 1
      if( n2 .lt. n1 ) idirect = -idirect
c
      return
      end
c
c_________________________________________________________________________________
c
c
c      integer function itxend( txt )
c      character*256 txt
c      k = 256
c      do while( txt(k:k).eq.' ' .and. k.gt.1 )
c        k = k - 1
c      enddo
c      itxend = k
cc
c      return
c      end
c
c_________________________________________________________________________________
c
      integer function ifrl( r )
      ip = abs(r) + 0.5
      if( r .lt. 0.000 )  ip = -ip
      ifrl = ip
      return
      end
c
c_________________________________________________________________________________
c
c   NCREAD: reads lines of input and ignores lines that begin with a "#" sign.
c          All information after a ! is wiped from the input card.
      subroutine ncread(io,txt,ierr, ierror)
      character*128  afile
      character*256  txt,tx2
      data ioflip,ioalt /69,69/
c
      ierr = 0
    5 read(io,'(a)',end=10)  txt
      if( txt(1:1) .eq. '#' )  goto 5
c
      ki = index(txt,'!')
      if( ki.gt.0 ) then
        txt(ki:256) = '                                                '
      endif
c
      tx2 = txt
      call UPCASE(tx2)
c
c    Test for switching control to an auxillary input file
c
      ki = index(txt,':')
      if( index(tx2,'REDIRECT').gt.0 .and. ki.gt.0 ) then
        afile = txt(ki+1:256)
        ki = index(afile,'  ') - 1
        iohold = io
        io = ioflip
        ioflip = iohold
        open(io,file=afile(1:ki),status='OLD',err=20)
        goto 5
      endif
c
c    Test for returning io control from auxillary input to master input file
c
      if( index(tx2,'RETURN')  .gt.0 .and.
     +    index(tx2,'CONTROL') .gt.0      ) goto 10
c
      ki = index(tx2,'<END>')
      if( ki .gt. 0 ) then
        ierr = 1
        txt(ki+5:256) = '                                           '
      endif
c
      if( index(tx2,'<STOP>') .gt. 0 ) ierr = 2
      return
c
c    Report error in opening auxillary input file and stop
c
   20 Continue 
c   20 write(*,*)
c      write(*,*) '  ERROR opening auxillary input file  '
c      write(*,*)
c      write(*,'(2x,10h The file:,2x,a40,16h does not exist.)')  afile
c      write(*,*)
C
C     When compiling MNW with Modflow-96, comment out the call to
C     USTOP and uncomment the STOP statement
      ierror = 1
	return
c      CALL USTOP(' ')
C      STOP
C
   10 txt(1:3) = 'EOF'
      if( io .eq. ioalt ) then
        close(io)
        iohold = io
        io = ioflip
        ioflip = iohold
        goto 5
      else
        ierr = -1
      endif
c
      return
      end
c
c_________________________________________________________________________________
c
c      subroutine shorten(txt,test,n,tx2)
c      character*256 txt,tx2,test
cc
c      tx2 = '???????'
c      ki = index(txt,test(1:n))
c      if(ki.gt.0) then
c        tx2 = txt(ki:256)
c        kc = index(tx2,':')+1
c        tx2 = tx2(kc:256)
c        kc = index(tx2,':')+1
c        if( kc .gt. 1 ) tx2 = tx2(1:kc-1)
c      endif
c      return
c      end
c
c_________________________________________________________________________________
c  GENERIC UTILITIES that were IN MNWutil.for  file
c_________________________________________________________________________________
c
c      subroutine conden(txt,n)
c      character*256 txt
c      if(n.gt.64) n=64
c      do  i=1,256-n
c        txt(n+i:n+i)=' '
c      enddo
cc
c      l=0
c      do 10 k=1,n*2
c      l=l+1
c      if(txt(l:l).eq.' ')then
c      do m=l,n
c        txt(m:m)=txt(m+1:m+1)
c      enddo
c      l=l-1
c      endif
c   10 continue
c      return
c      end
cc
cc_________________________________________________________________________________
cc
c      subroutine bakped(n,io)
c
c      do i=1,n
c        backspace(io)
c      enddo
c
c      return
c      end
c
c_________________________________________________________________________________
c
      subroutine qread(r,ni,ain,ierr)
      parameter (mrnv=25)
      dimension r(mrnv)
      character*1   tab
      character*16  form
      character*256 a256,ain
      tab = char(9)           ! sets tab delimiter
c
c   r(ni+1) records the number of non-numeric entries that were attempted to be read as a number
c   r(ni+2) records the last column that was read from the card
c
      r(ni+1) = -1.0000
      a256 = ain
      do i = 1, 256
        if( a256(i:i).eq.tab ) a256(i:i) = ' '
        if( a256(i:i).eq.',' ) a256(i:i) = ' '
        if( a256(i:i).eq.':' ) a256(i:i) = ' '
        if( a256(i:i).eq.'=' ) a256(i:i) = ' '
      enddo
      n = 1
      i = 0
   11 r(ni+1) = r(ni+1) + 1
   10 i = i+1
      if( i.ge.256) goto 15
      if( a256(i:i).eq.' ' ) then
        a256(i:i) = '?'
        goto 10
      endif
c
      ki = index(a256,' ')-1
      nd = ki - i + 1
      form ='(F??.0)              '
      write(form(3:4),'(i2.2)') nd
CERB  Fix for bug that caused i to be incremented by only 1 position
CERB  each time the read statement returns an error.  This bug also
CERB  incremented r(ni+1) unnecessarily.  With Lahey-compiled code, the
CERB  buggy version would read a final E in a word (without returning an
CERB  error) as a zero.
CERB      read (a256(i:ki),form,err=11,end=10) r(n)
      READ (A256(I:KI),FORM,ERR=13,IOSTAT=ISTAT) R(N)
   13 CONTINUE
      i = ki
      IF (ISTAT.GT.0) GOTO 11  ! PART OF BUG FIX -- ERB
      n = n+1
      if( n.le.ni .and. i.lt.256) goto 10
c
  15  n  = n-1
      ierr = ni - n
      r(ni+2) = i
      return
      end

