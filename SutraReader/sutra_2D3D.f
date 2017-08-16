C     MAIN PROGRAM     S U T R A _ 2 D 3 D _ 1     SUTRA VERSION 2D3D.1 @021799c !kluge
c     *****************************************************************
c     *****                                                      ******
c     *****  THIS IS A LIMITED-RELEASE BETA VERSION FOR TESTING  ****** !kluge
c     *****                                                      ******
c     *****************************************************************
C_______________________________________________________________________A20.....
C|                                                                     |A30.....
C|                                                                     |A40.....
C|                  UNITED STATES GEOLOGICAL SURVEY                    |A50.....
C|  GROUND-WATER FLOW AND ENERGY OR SOLUTE TRANSPORT SIMULATION MODEL  |@101200
C|                                                                     |A70.....
C|                                                                     |A80.....
C|                                                                     |A90.....
C|                                                                     |A100....
C|                       _______________________                       |A110....
C|                      |                       |                      |A120....
C|                      |   S   U   T   R   A   |                      |A130....
C|                      |_______________________|                      |A140....
C|                                                                     |A150....
C|                                                                     |A160....
C|                 Saturated    Unsaturated    TRAnsport               |A170....
C|                 =            =              ===                     |A180....
C|                                                                     |A190....
C|                                                                     |A200....
C|                                                                     |A210....
    ! newcom -- revised this comment box
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    |
C|    *                                                           *    |
C|    *  PHYSICS OPTIONS:                                         *    |
C|    *  -> Saturated and/or unsaturated ground-water flow        *    |
C|    *  -> Either single species reactive solute transport       *    |
C|    *     or thermal energy transport                           *    |
C|    *  GEOMETRY OPTIONS:                                        *    |
C|    *  -> Two-dimensional areal or cross-sectional simulation   *    |
C|    *  -> Fully three-dimensional simulation                    *    |
C|    *  -> Either (2-D, 3-D) Cartesian or                        *    |
C|    *     (2-D) radial/cylindrical coordinates                  *    |
C|    *  NUMERICAL METHODS:                                       *    |
C|    *  -> Hybrid Galerkin-finite-element method and             *    |
C|    *     integrated-finite-difference method                   *    |
C|    *     with two-dimensional quadrilateral or                 *    |
C|    *     three-dimensional hexahedral finite elements          *    |
C|    *  -> Finite-difference time discretization                 *    |
C|    *  -> Non-linear iterative, sequential or steady-state      *    |
C|    *     solution modes                                        *    |
C|    *  -> Direct and iterative solvers                          *    |
C|    *  OUTPUT OPTIONS:                                          *    |
C|    *  -> Optional fluid velocity calculation                   *    |
C|    *  -> Optional observation well output                      *    |
C|    *  -> Optional fluid mass and solute mass or energy budget  *    |
C|    *  -> Flexible, columnwise output of solution               *    |
C|    *                                                           *    |
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    |
C|                                                                     |A390....
C|                                                                     |A400....
C|                                                                     |A410....
C|       Complete explanation of function and use of this code         |A420....
C|       is given in :                                                 |A430....
C|                                                                     |A440....
c        ++++++ UPDATE THIS REFERENCE ! ++++++++         ! kluge
c                                                        ! kluge
C|       Voss, Clifford I., 1984, SUTRA: A Finite-Element              |A450....
C|            Simulation Model for Saturated-Unsaturated               |A460....
C|            Fluid-Density-Dependent Ground-Water Flow                |A470....
C|            with Energy Transport or Chemically-Reactive             |A480....
C|            Single-Species Solute Transport, U.S. Geological         |A490....
C|            Survey Water-Resources Investigations Report             |A500....
C|            84-4369.                                                 |A510....
C|                                                                     |A520....
C|                                                                     |A530....
C|                                                                     |A540....
C|       Users who wish to be notified of updates of the SUTRA         |A550....
C|       code and documentation may be added to the mailing            |A560....
C|       by sending a request to :                                     |A570....
C|                                                                     |A580....
C|                      Chief Hydrologist - SUTRA                      |A590....
C|                       U.S. Geological Survey                        |A600....
C|                        431 National Center                          |A610....
C|                       Reston, Virginia 20192                        |@061198
C|                                USA                                  |A630....
C|                                                                     |A640....
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    |
C|    *                                                           *    |
C|    *  The SUTRA code and documentation were prepared under a   *    |
C|    *  joint research project of the U.S. Geological Survey,    *    |
C|    *  Department of the Interior, Reston, Virginia, and the    *    |
C|    *  Engineering and Services Laboratory,  U.S. Air Force     *    |
C|    *  Engineering and Services Center, Tyndall A.F.B.,         *    |
C|    *  Florida.  The SUTRA code and documentation are           *    |
C|    *  available for unlimited distribution.                    *    |
C|    *                                                           *    |
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    |
C|                                                                     |
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    |
C|    *                                                           *    |
C|    *  First Revision: June 1990, Version V06902D               *    |
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    |
C|    *                                                           *    |
C|    *  Second Revision: September 1997, Version V09972D         *    |
C|    *  by: C.I. Voss and David Boldt, U.S. Geological Survey    *    |
C|    *                                                           *    |
C|    *  Third Revision: _____ 2002, Version 2D3D.1    ! kluge    *    |
C|    *  by: Alden Provost & C.I. Voss, U.S. Geological Survey    *    |
C|    *                                                           *    |
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    |
C|                                                                     |A750....
C|                                                                     |A760....
C|_____________________________________________________________________|A770....
C                                                                       A780....
C                                                                       A790....
C                                                                       A800....
C RBW START
C IERROR IS SET TO -1 BY SUBROUTINE SUTRA_2D3D_1 IF THE INPUT FILE
C CANNOT BE OPENED
C IERROR IS SET TO 1 BY SUBROUTINE SUTRA_2D3D_1 IF AN ERROR OCCURS
C IF ISTART <= 0 THEN DATA SETS 1 - 4 ARE READ.
C IF ISTART > 0 THEN DATA SETS 1-21 ARE READ.
C IBOUSZ IS THE SIZE OF THE ARRAY IBNODE
C IBNODE IS THE NODE NUMBERS OF THE BOUNDARY CONDITIONS AS WELL AS THE 
C NUMBER OF EACH TYPE OF BOUNDARY CONDITION.
C THE FIRST NUMBER IN THE ARRAY IS THE NUMBER OF BOUNDARY CONDITIONS OF
C THE FIRST TYPE FOLLOWED BY THE NODE NUMBERS FOR THAT TYPE OF BOUNDARY
C CONDITIONS. THAT IS FOLLOWED BY A SIMILAR ARRANGEMENT FOR EACH TYPE 
C OF BOUNDARY CONDITION. 
C NFEAT IS THE NUMBER OF TYPES OF BOUNDARY FEATURES (4 FOR SUTRA)
C ISTEADYFLOW      IS SET TO 1 IF FLOW      IS STEADY.  OTHERWISE IT IS SET TO 0.
C ISTEADYTRANSPORT IS SET TO 1 IF TRANSPORT IS STEADY.  OTHERWISE IT IS SET TO 0.
C FLABELS CONTAINS THE NAMES OF THE BOUNDARY FEATURES. 
C EACH NAME HAS A LENGTH OF 40 CHARACTERS
      SUBROUTINE SUTRA_2D3D(IERROR, ISTART, IBOUSZ, 
     &        IBNODE, NFEAT, ISTEADYFLOW, ISTEADYTRANSPORT, INPFILE)     @021799c
!DEC$ attributes dllexport :: SUTRA_2D3D
C RBW END
        ! alloc -- reorganized nonexecutable statements
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               A810....
C.....PROGRAMMERS SET SUTRA VERSION NUMBER HERE (8 CHARACTERS MAXIMUM)
      CHARACTER*8, PARAMETER :: VERN='2D3D.1'
	CHARACTER*8 VERNUM
      CHARACTER*1 TITLE1(80),TITLE2(80)                                 A900....
      CHARACTER*80 SIMULA(2), MSHTYP(2)                                 @111998
      CHARACTER*80 SIMSTR, MSHSTR                                       @102798
      CHARACTER*80 CUNSAT, CSSFLO ,CSSTRA, CREAD                        @111998
      CHARACTER*80 UNSSTR, SSFSTR ,SSTSTR, RDSTR                        @111998
      CHARACTER*80 UNAME,FNAME
      CHARACTER*80 ERRCOD, CHERR(10)                                    @041300
      CHARACTER*40 SOLNAM(0:10)                                         @102798
      CHARACTER*10 SOLWRD(0:10)                                         @102798
      CHARACTER*10 ADSMOD
      CHARACTER LETTER(8)*1                                             @042500
      CHARACTER INTFIL*1000                                             @042800
      INTEGER RMDIM,RVDIM,RMVDIM,IMVDIM
      LOGICAL ONCEK5, ONCEK6, ONCEK7                                    @030599
      DIMENSION FNAME(0:7),IUNIT(0:7)
      DIMENSION INERR(10), RLERR(10)                                    @041300
      COMMON/FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        @111098
ccc    alloc -- eliminated the COMMON blocks LGEM, LGEV, and LGEMV
      COMMON/DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              @111798b
     1   NSOP,NSOU,NBCN                                                 A860....
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                  @061198
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA
      COMMON/TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       @102398b
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                 @102398b
      COMMON/CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  A870...$
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE            @061198
      COMMON/OBS/ NOBSN,NTOBS,NOBCYC                                    A890....
      COMMON/PLT1/ ONCEK5, ONCEK6, ONCEK7                               @111098
      COMMON/ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      @102398b
      COMMON/KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG      @102398b
      COMMON /SOLVC/ SOLWRD, SOLNAM                                     @021799
      COMMON /SOLVN/ NSLVRS                                             @102798
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                      @040798
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU           @040298
      COMMON /ITSOLR/ TOLP,TOLU                                         @040298
      COMMON /FNAMES/ FNAME                                             @041300
      COMMON/MODSOR/ ADSMOD                                             B190....
      COMMON/PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      B260...$
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2       B270....
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL       @020399
	COMMON /VER/ VERNUM
      DATA (LETTER(M), M=1,8) /'A','B','C','D','E','F','G','H'/         @042500
C....."NSLVRS" AND THE ARRAYS "SOLWRD" AND "SOLNAM" ARE INITIALIZED     @021799
C.....IN THE BLOCK-DATA SUBPROGRAM "BDINIT"                             @021799
      ALLOCATABLE PMAT(:,:),UMAT(:,:)
      ALLOCATABLE PITER(:),UITER(:),PM1(:),UM1(:),UM2(:),PVEL(:),
     1   SL(:),SR(:),X(:),Y(:),Z(:),VOL(:),POR(:),
     2   CS1(:),CS2(:),CS3(:),SW(:),DSWDP(:),RHO(:),SOP(:),
     3   QIN(:),UIN(:),QUIN(:),RCIT(:),RCITM1(:)
      ALLOCATABLE PVEC(:),UVEC(:)
      ALLOCATABLE ALMAX(:),ALMIN(:),ATMAX(:),ATMIN(:),VMAG(:),
     1   VANG1(:),PERMXX(:),PERMXY(:),PERMYX(:),PERMYY(:),
     2   PANGL1(:)
      ALLOCATABLE ALMID(:),ATMID(:),
     1   VANG2(:),PERMXZ(:),PERMYZ(:),PERMZX(:),
     2   PERMZY(:),PERMZZ(:),PANGL2(:),PANGL3(:)
      ALLOCATABLE PBC(:),UBC(:),QPLITR(:)
      ALLOCATABLE GXSI(:,:),GETA(:,:),GZET(:,:)
      ALLOCATABLE FWK(:),B(:)
      ALLOCATABLE IN(:),IQSOP(:),IQSOU(:),IPBC(:),IUBC(:),
     1   IOBS(:),NREG(:),LREG(:),NBI27(:),IWK(:),IA(:),JA(:)
	INTEGER IBOUSZ,NFEAT
	INTEGER IBNODE(IBOUSZ)
      CHARACTER(*) INPFILE
C                                                                       A930....
C                                                                       A940....
       ! newcom -- comment box below revised extensively
C_____________________________________________________________________  A950....
C|* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *|  A960....
C|* *************************************************************** *|  A970....
C|* *                                                             * *|  A980....
C|* *   The main arrays used by SUTRA are dimensioned dynamically * *|
C|* *   in SUBROUTINE SUTRA.  The amount of storage required by   * *|
C|* *   these arrays is as follows:                               * *|
C|* *                                                             * *|
C|* *   RMDIM = 2*NELT*NCBI                                       * *|
C|* *                                                             * *|
C|* *   RVDIM = NNV*NN + NNVA*NNVEC + (NEV+NEVG)*NE + NEVX + NE8X * *|
C|* *               + NBCN*3 + (NOBS+1)*(NTOBS+2)*2 + NTOBS+2     * *|
C|* *               + NDIMXR + NFPARM*2                           * *|
C|* *                                                             * *|
C|* *   IMVDIM = NE*9 + NN + NSOP+1 + NSOU+1 + NBCN*2             * *|
C|* *               + NOBS+1 + NTOBS+2 + NDIMXI + NIPARM*2        * *|
C|* *                                                             * *|
C|* *   where:  ! kluge -- these comments need more updating      * *|
C|* *                                                             * *|  A1130...
C|* *    NNV = 25     ! kluge -- update this                      * *|
C|* *    NEV = 21 for 3-D; 11 for 2-D                             * *|
C|* *    NEVG = 16 for 3-D; 8 for 2-D                             * *|  @061198
C|* *    NEVX = 21 - NEV                                          * *|  @061198
C|* *    NE8X = NE*8 for 3-D; 4 for 2-D                           * *|  @061198
C|* *    NBCN = NPBC + NUBC + 1                                   * *|
C|* *                                                             * *|  A1170...
C|* *   and:                                                      * *|  A1180...
C|* *                                                             * *|  A1190...
C|* *    NN = number of nodes in finite element mesh              * *|  A1200...
C|* *    NE = number of elements in finite element mesh           * *|  A1210...
C|* *    NBI = estimated full bandwidth in finite element mesh    * *|  @031798
C|* *    NCBI = NBI for Gaussian (direct) solver; =27 otherwise   * *|  @031798
C|* *    NDIMXI = integer array storage associated with solver(s),* *|  @030599
C|* *       exclusive of the parameter array (see NIPARM below);  * *|
C|* *       if two different solvers are used, the storage        * *|
C|* *       required is the greater of the two values:            * *|
          ! kluge -- should reduce 2*NN+2 to just 4 for DIRECT; don't need IA, JA arrays ...
C|* *       = 2*NN+2 for Gaussian (direct) solver;                * *|
C|* *       = 2*NELTA+NL+NN+NBI+12 for CG with non-symm. storage; * *|
C|* *       = 3*NELTA+3*NN+NBI+33 for GMRES;                      * *|
C|* *       = 3*NELTA+3*NN+NBI+13 for ORTHOMIN                    * *|
C|* *    NDIMXR = real array storage associated with solver(s),   * *|
C|* *       exclusive of the matrix and parameter arrays          * *|
C|* *       (see NELT and NFPARM below); if two different solvers * *|
C|* *       are used, the storage required is the greater of the  * *|
C|* *       two values:                                           * *|
C|* *       = 2 for Gaussian (direct) solver;                     * *|
C|* *       = NL+6*NN+1 for CG with non-symmetric storage;        * *|
C|* *       = NELT+NN*(NSAVE+7)+NSAVE*(NSAVE+3)+2 for GMRES;      * *|
C|* *       = NELT+NN*(3*NSAVE+10)+NSAVE+1 for ORTHOMIN           * *|
C|* *    NELTA = array storage needed for matrix elements         * *|
C|* *       = NN for Gaussian (direct) solver;                    * *|
C|* *       = 27*NN-6*NN1*(1+3*NN2)-2 for 3-D, using GMRES,       * *|
C|* *         ORTHOMIN, or CG with non-symmetric storage;         * *|
C|* *       = 9*NN-6*NN1-2 for 2-D, using GMRES,                  * *|
C|* *         ORTHOMIN, or CG with non-symmetric storage          * *|
C|* *    NELT = dimensioned size of matrix arrays                 * *|
C|* *       = NELTA for Gaussian (direct), CG with non-symm.      * *|
C|* *         storage, GMRES, and ORTHOMIN                        * *|
C|* *    NIPARM =   ! kluge newcom
C|* *    NFPARM =   ! kluge newcom
C|* *    NL = (NELT + NN)/2 for CG with non-symmetric storage     * *|  @031798
C|* *    NN1 = number of nodes in 1st node numbering direction,   * *|  @111898
C|* *       for iterative solver                                  * *|  @111898
C|* *    NN2 = number of nodes in 2nd node numbering direction,   * *|  @111898
C|* *       for iterative solver                                  * *|  @111898
C|* *    NN3 = number of nodes in 3rd node numbering direction,   * *|  @111898
C|* *       for 3-D using iterative solver                        * *|  @111898
C|* *    NSAVE = maximum number of direction vectors for GMRES    * *|  @040398
C|* *       or ORTHOMIN                                           * *|  @050698
C|* *    NOBS = number of observation nodes in mesh               * *|  A1220...
C|* *    NTOBS = number of time steps with observations           * *|  @031299
C|* *    NSOP = number of fluid mass source nodes in mesh         * *|  A1250...
C|* *    NSOU = number of energy or solute mass source nodes      * *|  A1260...
C|* *    NPBC = number of specified pressure nodes in mesh        * *|  A1270...
C|* *    NUBC = number of specified concentration or temperature  * *|  A1280...
C|* *       nodes in mesh                                         * *|  A1290...
C|* *                                                             * *|  A1300...
C|* *                                                             * *|  A1310...
C|* *   NOTE :                                                    * *|  @021799b
C|* *    Two files must be permanently assigned just below for    * *|  A1330.6$
C|* *    your computer installation.  One file captures error     * *|  A1330.7$
C|* *    output written during subsequent file opening.  The      * *|  @061098
C|* *    other file contains the unit numbers and file names      * *|  A1330.9$
C|* *    to be assigned as SUTRA input and output files           * *|  A1331.0$
C|* *    for each simulation.                                     * *|  A1331.1$
C|* *                                                             * *|  A1331.2$
C|* *    STANDARD ASSIGNMENTS TO BE MADE:                         * *|  A1331.3$
C|* *    for Error Output:                                        * *|  A1331.4$
C|* *        Filename is contained in ENAME    !kluge UPDATE!!!   * *|  A1331.5$
C|* *        Unit Number is contained in K00                      * *|  A1331.6$
C|* *    for Simulation Units and Files:                          * *|  A1331.7$
C|* *        Filename is contained in UNAME                       * *|  A1331.8$
C|* *        Unit Number is contained in K0                       * *|  A1332.9$
C|* *                                                             * *|  A1333.0$
C|* *************************************************************** *|  A1340...
C|* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *|
C                                                                       @030499
ccc alloc      ALLOCATABLE :: RM(:), RV(:), IMV(:)                               @021799b
C                                                                       @102098
C.....COPY PARAMETER VERN (SUTRA VERSION NUMBER) TO VARIABLE VERNUM,
C.....WHICH IS PASSED THROUGH COMMON BLOCK VER.
      VERNUM = VERN
C
C RBW START 
      IERROR = 0 
C RBW END
C     set string for use with RCS ident command                            .
      fname(1) =                                                           .
     &'$Id: sutra_2D3D_1.f,v 2D3D.1 1999/XX/XX XX:XX:XX' //      !kluge
     &' rsregan Exp rsregan $'                                   !kluge
C     strings for Unix what command
      fname(1) =
     &'@(#)SUTRA - Finite-element simulation model for saturated-'
      fname(1) =
     &'@(#)SUTRA - unsaturated fluid-density-dependent ground-water'
      fname(1) =
     &'@(#)SUTRA - flow with energy transport or chemically-reactive'
      fname(1) = '@(#)SUTRA - single-species solute transport'
      fname(1) = '@(#)SUTRA - USGS WRIR 84-4369, C.I. Voss'                .
ckluge   --- Ref to new documentation? ---
      fname(1) = '@(#)SUTRA - Contact: h2osoft@usgs.gov'                   .
      fname(1) = '@(#)SUTRA - Version: 2D3D.1 1999/XX/XX'      !kluge   @022499
C
C                                                                       @102098
C|* *****  S T A N D A R D   F I L E   A S S I G N M E N T S  ***** *|  A1346..$
C|*   S I M U L A T I O N   U N I T S   A N D   F I L E S         * *|  A1350..$
      UNAME = 'SUTRA.FIL'                                               A1351..$
      K0 = 99                                                           A1352DG$
C|*                                                               * *|  A1352.1$
C|*   -------> Required Format of Unit K0 :  ! kluge -- check this* *|  A1352.2$
C|*                                                               * *|  A1352.3$
C|*              V A R I A B L E           F O R M A T            * *|  A1352.4$
C|*                                                               * *|  A1352.5$
C|*              Unit Number for K1         (free format)         * *|  A1352.6$
C|*               File Name for K1           (A80)                * *|  A1352.7$
C|*              Unit Number for K2         (free format)         * *|  A1352.8$
C|*               File Name for K2           (A80)                * *|  A1352.9$
C|*              Unit Number for K3         (free format)         * *|  A1353..$
C|*               File Name for K3           (A80)                * *|  A1353.5$
C|*              Unit Number for K4         (free format)         * *|  A1354..$
C|*               File Name for K4           (A80)                * *|  A1355..$
C|*              Unit Number for K5         (free format)         * *|  @102098
C|*               File Name for K5           (A80)                * *|  @102098
C|*              Unit Number for K6         (free format)         * *|  @102098
C|*               File Name for K6           (A80)                * *|  @102098
C|*                                                               * *|  A1356..$
C|*                                                               * *|  A1357..$
C|*   Some of the last six lines need not be included if          * *|  @102098
C|*   UNIT-K4,K5 and/or K6 will not be used.                      * *|  @102098
C|*     This file has between six and twelve lines.               * *|  @102098
C|* *************************************************************** *|  A1360...
C|* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *|  A1370...
C|___________________________________________________________________|  A1380...
C                                                                       A1390...
C                                                                       A1400...
C ---> Programmers making code changes that affect dimensions must      A1401..$
C --->  check and change the following assignments for NNV and NEV:     A1402..$
C                                                                       A1403..$
C      NNV IS NUMBER OF REAL VECTORS THAT ARE NN LONG                   A1404..$
          NNV = 25                                             ! kluge update
C      NEV IS NUMBER OF REAL VECTORS THAT ARE NE LONG.                  @061198
C         NEV = NEV2 for 2-D;                                           @061198
C         NEV = NEV3 for 3-D                                            @061198
          NEV2 = 11                                                     @061198
          NEV3 = 21
C                                                                       A1408..$
C                                                                       A1409..$
C.....KEEP TRACK IF OUTPUT ROUTINES HAVE BEEN EXECUTED, TO PRINT        @110498
C      HEADERS ONLY ONCE                                                @110498
          ONCEK5 = .FALSE.                                              @110498
          ONCEK6 = .FALSE.                                              @110498
C                                                                       A1410...
C.....ASSIGN UNIT NUMBERS AND OPEN FILE UNITS FOR THIS SIMULATION       A1412..$
c	IF (ISTART.LE.0) THEN
	  IUNIT(1) = 10
	  UNAME = INPFILE
        CALL FOPEN(UNAME,IUNIT,NFILE,IERROR)
    	  IF (IERROR.NE.0) then
	    GOTO 900
	  endif
c      ELSE
c         REWIND(K1)
c	END IF
C                                                                       A1416..$
C.....OUTPUT BANNER                                                     @102398
C                                                                       @102398
C.....INPUT DATASET 1:  OUTPUT HEADING                                  @102398
      ERRCOD = 'REA-INP-S1'                                             @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
    	IF (IERROR.NE.0) then
	  GOTO 900
	endif
      READ(K1,170,IOSTAT=IERROR) TITLE1,TITLE2                          @042500
c      READ(K1,170) TITLE1,TITLE2                                        @042500
    	IF (IERROR.NE.0) GOTO 900
  170 FORMAT(80A1/80A1)                                                 @102398
C                                                                       @102398
C.....INPUT DATASET 2A:  INPUT DATA HEADING -- TYPE OF TRANSPORT        @102398
C.....( SET ME=-1 FOR SOLUTE TRANSPORT, ME=+1 FOR ENERGY TRANSPORT )    A1430...
      ERRCOD = 'REA-INP-S2A'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
    	IF (IERROR.NE.0) GOTO 900
      ERRCOD = 'REA-INP-2A'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
    	IF (IERROR.NE.0) GOTO 900
      READ(INTFIL,*,IOSTAT=IERROR) SIMSTR                                   @042800
c	IERROR = INERR(1)
    	IF (IERROR.NE.0) GOTO 900
      CALL PRSWDS(SIMSTR, ' ', 2, SIMULA, NWORDS)                       @041300
      IF(SIMULA(1).NE.'SUTRA     ') THEN                                @041300
	   IERROR = 1
	   GOTO 900
      END IF                                                            @041300
      IF(SIMULA(2).EQ.'SOLUTE    ') GOTO 120                            @102798
      IF(SIMULA(2).EQ.'ENERGY    ') GOTO 140                            @102798
      IERROR = 1
	GOTO 900
  120 ME=-1                                                             A1680...
      GOTO 160                                                          A1730...
  140 ME=+1                                                             A1740...
  160 CONTINUE                                                          A1790...
C                                                                       A1800...
C.....INPUT DATASET 2B:  INPUT DATA HEADING -- MESH TYPE                @102398
      ERRCOD = 'REA-INP-S2B'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
    	IF (IERROR.NE.0) GOTO 900
c
      ERRCOD = 'REA-INP-2B'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
    	IF (IERROR.NE.0) GOTO 900
c
      READ(INTFIL,*,IOSTAT=IERROR) MSHSTR                                   @042800
    	IF (IERROR.NE.0) GOTO 900
c
      CALL PRSWDS(MSHSTR, ' ', 2, MSHTYP, NWORDS)                       @041300
C.....KTYPE SET ACCORDING TO THE TYPE OF FINITE-ELEMENT MESH:           @111898
C        3-D, IRREGULAR MESH   ==>   KTYPE = +3                         @111898
C        3-D, REGULAR MESH     ==>   KTYPE = -3                         @111898
C        2-D, IRREGULAR MESH   ==>   KTYPE = +2                         @111898
C        2-D, REGULAR MESH     ==>   KTYPE = -2                         @111898
      IF (MSHTYP(1).EQ.'2D        ') THEN                               @102798
         KTYPE = 2                                                      @102798
      ELSE IF (MSHTYP(1).EQ.'3D        ') THEN                          @102798
         KTYPE = 3                                                      @102798
      ELSE                                                              @102798
         IERROR = 1
   	   GOTO 900
      END IF                                                            @102798
      IF (MSHTYP(2).EQ.'IRREGULAR ') THEN                               @102798
         IF (KTYPE.EQ.3) THEN                                           @102798
            IERROR = 1
	      GOTO 900
         END IF                                                         @102798
      ELSE IF ((MSHTYP(2).EQ.'REGULAR   ').OR.
     1         (MSHTYP(2).EQ.'BLOCKWISE ')) THEN
         KTYPE = - KTYPE                                                @102798
         ERRCOD = 'REA-INP-2B'                                          @042800
         IF (KTYPE.EQ.-2) THEN                                          @102798
            READ(INTFIL,*,IOSTAT=IERROR) MSHSTR, NN1, NN2                   @042800
    		  IF (IERROR.NE.0) GOTO 900
            NN3 = 1                                                     @102798
         ELSE                                                           @102798
            READ(INTFIL,*,IOSTAT=IERROR) MSHSTR, NN1, NN2, NN3              @042800
    		  IF (IERROR.NE.0) GOTO 900
         END IF                                                         @102798
         IF ((NN1.LE.0).OR.(NN2.LE.0).OR.(NN3.LE.0)) THEN               @102798
		  IERROR = 1
    		  IF (IERROR.NE.0) GOTO 900
         END IF                                                         @102798   
         IF (MSHTYP(2).EQ.'BLOCKWISE ') THEN
            ERRCOD = 'REA-INP-2B'                                       @042800
            DO 177 I1=1,-KTYPE                                          @042500
               CALL READIF(K1, INTFIL, ERRCOD, IERROR)                          @042800
	    	 IF (IERROR.NE.0) GOTO 900
               READ(INTFIL,*,IOSTAT=IERROR) IDUM1, (IDUM2, I2=1,IDUM1)      @042800
    			 IF (IERROR.NE.0) GOTO 900
177         CONTINUE                                                    @042500
         END IF
      ELSE                                                              @102798
         IERROR = 1
 	   GOTO 900
      END IF                                                            @102798
C                                                                       @102398
C.....INPUT DATASET 3:  SIMULATION CONTROL NUMBERS                      @102398
      ERRCOD = 'REA-INP-S3'                                             @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
    	IF (IERROR.NE.0) GOTO 900
      ERRCOD = 'REA-INP-3'                                              @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c    	IF (IERROR.NE.0) GOTO 900
      READ(K1,*,IOSTAT=IERROR) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS
c      READ(INTFIL,*) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS
    	IF (IERROR.NE.0) GOTO 900
      IF (KTYPE.LT.0) THEN                                              @102798
         NN123 = NN1*NN2*NN3                                            @102798
         IF(NN123.NE.NN) THEN                                           @102798
           IERROR = 1
	     GOTO 900
         END IF                                                         @102798
         IF (IABS(KTYPE).EQ.3) THEN                                     @110498b
            NE123 = (NN1 - 1)*(NN2 - 1)*(NN3 - 1)                       @110498b
         ELSE                                                           @110498b
            NE123 = (NN1 - 1)*(NN2 - 1)                                 @110498b
         END IF                                                         @110498b
         IF(NE123.NE.NE) THEN                                           @110498b
           IERROR = 1
	     GOTO 900
         END IF                                                         @110498b
      ENDIF                                                             @102798
C                                                                       @102398

      IBOUSZ = NPBC + NUBC + NSOP + NSOU + 4
	NFEAT = 4

C.....INPUT AND OUTPUT DATASET 4:  SIMULATION MODE OPTIONS              A1865..$
      ERRCOD = 'REA-INP-S4'                                             @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
    	IF (IERROR.NE.0) GOTO 900
      ERRCOD = 'REA-INP-4'                                              @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
    	IF (IERROR.NE.0) GOTO 900
      READ(INTFIL,*,IOSTAT=IERROR) UNSSTR,SSFSTR,SSTSTR,RDSTR,ISTORE        @042800
c      READ(INTFIL,*) UNSSTR,SSFSTR,SSTSTR,RDSTR,ISTORE                  @042800
    	IF (IERROR.NE.0) GOTO 900
      CALL PRSWDS(UNSSTR, ' ', 1, CUNSAT, NWORDS)                       @041300
      CALL PRSWDS(SSFSTR, ' ', 1, CSSFLO, NWORDS)                       @041300
      CALL PRSWDS(SSTSTR, ' ', 1, CSSTRA, NWORDS)                       @041300
      CALL PRSWDS(RDSTR,  ' ', 1, CREAD, NWORDS)                        @041300
      ISMERR = 0                                                        @111998
      IF (CUNSAT.EQ.'UNSATURATED') THEN                                 @111998
         IUNSAT = +1                                                    @111998
      ELSE IF (CUNSAT.EQ.'SATURATED') THEN                              @111998
         IUNSAT = 0                                                     @111998
      ELSE       
	   IERROR = 1 
	   GOTO 900                                                       
      END IF                                                            @111998
      IF (CSSFLO.EQ.'TRANSIENT') THEN                                   @111998
         ISSFLO = 0                                                     @111998
      ELSE IF (CSSFLO.EQ.'STEADY') THEN                                 @111998
         ISSFLO = +1                                                    @111998
      ELSE                                                              @111998
	   IERROR = 1 
	   GOTO 900                                                       
      END IF                                                            @111998
      IF (CSSTRA.EQ.'TRANSIENT') THEN                                   @111998
         ISSTRA = 0                                                     @111998
      ELSE IF (CSSTRA.EQ.'STEADY') THEN                                 @111998
         ISSTRA = +1                                                    @111998
      ELSE                                                              @111998
	   IERROR = 1 
	   GOTO 900                                                       
      END IF                                                            @111998
      IF (CREAD.EQ.'COLD') THEN                                         @111998
         IREAD = +1                                                     @111998
      ELSE IF (CREAD.EQ.'WARM') THEN                                    @111998
         IREAD = -1                                                     @111998
      ELSE                                                              @111998
	   IERROR = 1 
	   GOTO 900                                                       
      END IF                                                            @111998
      IF(ISSTRA.EQ.1.AND.ISSFLO.NE.1) THEN                              A1920...
	   IERROR = 1 
	   GOTO 900                                                       
      ENDIF  
	
	ISTEADYFLOW = ISSFLO
	ISTEADYTRANSPORT = ISSTRA

      IF (ISTART.LE.0) then
	  CLOSE(K1)
	  RETURN
	endif
C                                                                       @031798
C.....INPUT DATASETS 5 - 7                                              @020399
      CALL INDAT0(IERROR)
	IF (IERROR.NE.0) GOTO 900
C.....KSOLVP AND KSOLVU HAVE BEEN SET ACCORDING TO THE SOLVERS SELECTED:@111898
C        BANDED GAUSSIAN ELIMINATION (DIRECT)   ==>   0                 @111898
C        ILU-PRECONDITIONED GMRES               ==>   1                 @111898
C        ILU-PRECONDITIONED ORTHOMIN            ==>   2                 @111898
C                                                                       @102398b
      IF ((KTYPE.GT.0).AND.(KSOLVP.NE.0)) THEN                          @102798
	   IERROR=1
	   GOTO 900
      ENDIF                                                             @102798
 266  CONTINUE
C                                                                       A2690...
C                                                                       A2700...
C.....CALCULATE THE NUMBER OF TIME STEPS ON WHICH OBSERVATIONS WILL     @031299
C.....BE MADE, NTOBS.  THIS REQUIRES LOOKING AHEAD TO DATASET 8 OF         .
C.....UNIT K1 AND DATASET 1 OF UNIT K2.  THE UNITS ARE THEN "BACKSPACED"   .
C.....SO THAT THESE DATASETS CAN BE READ AGAIN LATER BY SUBROUTINES        .
C.....INDAT1 AND INDAT2.
C
      NTOBS = 0
      IF (NOBS.EQ.0) GOTO 311
      NLSTOT = 0
      DO 307 I=1,4
         ERRCOD = 'REA-INP-S8' // LETTER(I)                             @042500
         CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                @042500
         IF (IERROR.NE.0) GOTO 900
         NLSTOT = NLSTOT + NLSKIP
         ERRCOD = 'REA-INP-8' // LETTER(I)                              @042800
c         CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                @042800
c    	   IF (IERROR.NE.0) GOTO 900
         READ(K1,*,IOSTAT=IERROR) NOBCYC                                @042800
c         READ(INTFIL,*) NOBCYC                                          @042800
         IF (IERROR.NE.0) GOTO 900
307   CONTINUE
C
      DO 309 I=1,NLSTOT+4
309      BACKSPACE(K1)
c      ERRCOD = 'REA-ICS-S1'                                             @042500
c      CALL SKPCOM(K2, NLSKIP, ERRCOD, IERROR)                                   @042500
c    	IF (IERROR.NE.0) GOTO 900
c      ERRCOD = 'REA-ICS-1'                                              @042800
c      CALL READIF(K2, INTFIL, ERRCOD, IERROR)                                   @042800
c    	IF (IERROR.NE.0) GOTO 900
c      READ(K1,*,IOSTAT=IERROR) TSTART                                   @042800
c      READ(INTFIL,*) TSTART                                             @042800
c      IF (IERROR.NE.0) GOTO 900
c      BACKSPACE(K2)
      TSTART = 0
      TS=0                                                         
      JT=0                                                              
      KT=0                                                              
      DELTK=DELT                                                        
310   CONTINUE                                                       
         JT=JT+1                                                     
         IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT    
         IF (DELTK.GT.DTMAX) DELTK=DTMAX                             
         TS=TS+DELTK                                                 
         IF (MOD(JT,NOBCYC).EQ.0 .OR. JT.EQ.1) KT = KT + 1           
      IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 310
      JTMAX = JT                                                           .
      IF(JTMAX.GT.1 .AND. MOD(JT,NOBCYC).NE.0) KT = KT + 1                 .
      NTOBS = KT                                                        @031299
311   CONTINUE
C
C.....CALCULATE ARRAY DIMENSIONS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH
C                                                                       A2720...
C     IF (KSOLVR.EQ.0) THEN                                             @031798
      IF (KSOLVP.EQ.0) THEN                                             @040798
C........SET DIMENSIONS FOR DIRECT SOLVER
         NNNX = 1                                                          .
         NELTA = NN
         NELT = NELTA
         NDIMJA = NELT
         NNVEC = NN
         NWI = 1
         NWF = 1
      ELSE                                                              @050698
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)
         NNNX = NN
         IF (IABS(KTYPE).EQ.2) THEN                                     @102798
            NELTA = 9*NN - 6*NN1 - 2
         ELSE                                                           @061198
            NELTA = 27*NN - 6*NN1*(1 + 3*NN2) - 2
         END IF                                                         @061198
         NELT = NELTA
         NDIMJA = NELT
         NNVEC = NN
	   KSOLVR = KSOLVP
	   NSAVE = NSAVEP
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIP, NWFP)
	   KSOLVR = KSOLVU
	   NSAVE = NSAVEU
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIU, NWFU)
         NWI = MAX(NWIP, NWIU)                                             .
         NWF = MAX(NWFP, NWFU)                                             .
      END IF                                                            @050698
      NBCN=NPBC+NUBC+1                                                  A2730...
      NSOP=NSOP+1                                                       A2740...
      NSOU=NSOU+1                                                       A2750...
      NIN=NE*8                                                          A2780...
      NOBSN=NOBS+1                                                      A2790...
      IF (IABS(KTYPE).EQ.3) THEN                                        @102798
         NEV = NEV3                                                     @061198
         N48 = 8                                                        @061198
         NEX = NE                                                       @061198
      ELSE                                                              @061198
         NEV = NEV2                                                     @061198
         N48 = 4                                                        @061198
         NEX = 1                                                        @061198
      END IF                                                            @061198
C.....NEXV IS THE NUMBER OF VECTORS THAT ARE OF LENGTH NE IN 3-D AND
C.....ARE TO BE DIMENSIONED TO LENGTH 1 BECAUSE THEY ARE NOT NEEDED.
C.....THUS, IN 3-D, NEXV=0; IN 2-D, NEXV=NEV3-NEV2.
      NEVX = NEV3 - NEV                                                 @061198
      NEVG = 2*N48                                                      @061198
      NE8=NE*N48                                                        @061198
      NE8X = NEX*N48                                                    @061198
C                                                                       A2820.2$
C.....ALLOCATE REAL ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH
      ALLOCATE(PITER(NN),UITER(NN),PM1(NN),UM1(NN),UM2(NN),PVEL(NN),
     1   SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),
     3   QIN(NN),UIN(NN),QUIN(NN),RCIT(NN),RCITM1(NN))
      ALLOCATE(PVEC(NNVEC),UVEC(NNVEC))
      ALLOCATE(ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),
     2   PANGL1(NE))
      ALLOCATE(ALMID(NEX),ATMID(NEX),
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX))
      ALLOCATE(PBC(NBCN),UBC(NBCN),QPLITR(NBCN))
      ALLOCATE(GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48))
      ALLOCATE(FWK(NWF),B(NNNX))
C.....ALLOCATE INTEGER ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH
      ALLOCATE(IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),
     1   IOBS(NOBSN),NREG(NN),LREG(NE),IWK(NWI),IA(NELT),JA(NDIMJA))
C
C.....INPUT SIMULATION DATA FROM UNIT-K1 (DATASETS 8 THROUGH 15)        @020399
      CALL INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,
     1   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,
     2   PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG,
     3   IOBS, IERROR)                                                  @020399
         IF (IERROR.NE.0) GOTO 900
C                                                                       B520....
C.....INPUT FLUID MASS, AND ENERGY OR SOLUTE MASS SOURCES               B560....
C        (DATASETS 17 AND 18)                                           B570....
      CALL ZERO(QIN,NN,0.0D0)                                           B580....
      CALL ZERO(UIN,NN,0.0D0)                                           B590....
      CALL ZERO(QUIN,NN,0.0D0)                                          B600....
      IF(NSOP-1.GT.0.OR.NSOU-1.GT.0) then                                   B610....
          CALL SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT,IERROR, 
     2    IBOUSZ, IBNODE, IPOS) 
      ELSE
	  IBNODE(1) = 0
	  IBNODE(2) = 0
	  IPOS = 2
      ENDIF                                                             B620....
	IF (IERROR.NE.0) goto 900
C                                                                       B630....
C.....INPUT SPECIFIED P AND U BOUNDARY CONDITIONS (DATASETS 19 AND 20)  B640....
      IF(NBCN-1.GT.0) then
	  CALL BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT,IERROR, 
     1  IBOUSZ, IBNODE, IPOS) 
      ELSE
	  IBNODE(IPOS+1) = 0
	  IBNODE(IPOS+2) = 0
      ENDIF                                                             B650....
	IF (IERROR.NE.0) goto 900
 900  continue                                                          O720....
c     close files
	CLOSE(K1)
C                                                                       B660....
C                                                                       A3660...
      return                                                              A3680...
      END                                                               A3690...
C
C
C
C
C     SUBPROGRAM        B  D  I  N  I  T           SUTRA VERSION 2D3D.1  BDINIT.........100
C                                                                        BDINIT.........200
C *** PURPOSE :                                                          BDINIT.........300
C ***  BLOCK-DATA SUBPROGRAM FOR INITIALIZING VARIABLES NAMED IN         BDINIT.........400
C ***  COMMON BLOCKS.                                                    BDINIT.........500
C                                                                        BDINIT.........600
      BLOCK DATA BDINIT                                                  BDINIT.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BDINIT.........800
      CHARACTER*40 SOLNAM(0:10)                                          BDINIT.........900
      CHARACTER*10 SOLWRD(0:10)                                          BDINIT........1000
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      BDINIT........1100
      COMMON /SOLVN/ NSLVRS                                              BDINIT........1200
C.....SET THE NUMBER OF SOLVERS AVAILABLE                                BDINIT........1300
      DATA NSLVRS /4/                                                    BDINIT........1400
C.....DEFINE KEYWORDS AND NAMES FOR SOLVERS                              BDINIT........1500
      DATA (SOLWRD(M),SOLNAM(M),M=0,10) /                                BDINIT........1600
     1   'DIRECT', 'BANDED GAUSSIAN ELIMINATION (DIRECT)',               BDINIT........1700
     2   'CG', 'IC-PRECONDITIONED CONJUGATE GRADIENT',                   BDINIT........1800
     3   'GMRES', 'ILU-PRECONDITIONED GMRES',                            BDINIT........1900
     4   'ORTHOMIN', 'ILU-PRECONDITIONED ORTHOMIN',                      BDINIT........2000
     5   '', '',                                                         BDINIT........2100
     6   '', '',                                                         BDINIT........2200
     7   '', '',                                                         BDINIT........2300
     8   '', '',                                                         BDINIT........2400
     9   '', '',                                                         BDINIT........2500
     T   '', '',                                                         BDINIT........2600
     1   '', ''/                                                         BDINIT........2700
      END                                                                BDINIT........2800
C
C     SUBROUTINE        B  O  U  N  D              SUTRA VERSION 2D3D.1 @021799c
C                                                                       F20.....
C *** PURPOSE :                                                         F30.....
C ***  TO READ AND ORGANIZE SPECIFIED PRESSURE DATA AND                 F40.....
C ***  SPECIFIED TEMPERATURE OR CONCENTRATION DATA.                     F50.....
C                                                                       F60.....
      SUBROUTINE BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT,IERROR, 
     1  IBOUSZ, IBNODE, IPOS)                                           F70.....
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               F80.....
      COMMON/FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        @111098
      COMMON/DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              @111798b
     1   NSOP,NSOU,NBCN                                                 F100....
      COMMON/CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  F110...$
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE            @061198
      DIMENSION IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN)               F130....
      CHARACTER*80 ERRCOD, CHERR(10), FNAME(0:7)
      DIMENSION INERR(10), RLERR(10)                                    @041300
      COMMON/FNAMES/FNAME                                               @041300
      CHARACTER INTFIL*1000                                             @042800
	INTEGER IBOUSZ, IPOS
	INTEGER IBNODE(IBOUSZ)
C                                                                       F140....
C                                                                       F150....
      IPBCT=1                                                           F160....
      IUBCT=1                                                           F170....
      IP=0                                                              F195...$
      IPU=0                                                             F200....
      IF(NPBC.EQ.0) THEN
        IPOS = IPOS + 1
  	  IBNODE(IPOS) = NPBC
        GOTO 400                                                         F230....
	ENDIF
C                                                                       F390....
C.....INPUT DATASET 19                                                  F400...$
120   ERRCOD = 'REA-INP-S19'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      IPOS = IPOS + 1
	IBNODE(IPOS) = NPBC
125   IPU=IPU+1                                                         F410....
      ERRCOD = 'REA-INP-19'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) IDUM                                     @042800
c      READ(INTFIL,*) IDUM                                               @042800
	IF (IERROR.NE.0) RETURN
      IF (IDUM.EQ.0) THEN                                               @041300
         GOTO 180                                                       @041300
      ELSE IF (IPU.GT.NPBC) THEN                                        @041300
         GOTO 125                                                       @041300
      END IF                                                            @041300
      IPBC(IPU) = IDUM                                                  @041300
	IF (IPBC(IPU).GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IPBC(IPU) - 1                                                
	ELSE IF (IPBC(IPU).lT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = -IPBC(IPU) - 1                                                
	ENDIF
      IF (IPBC(IPU).GT.0) THEN                                          @102798
         ERRCOD = 'REA-INP-19'                                          @042800
         READ(INTFIL,*,IOSTAT=IERROR) IPBC(IPU),PBC(IPU),UBC(IPU)           @042800
c         READ(INTFIL,*) IPBC(IPU),PBC(IPU),UBC(IPU)                     @042800
	   IF (IERROR.NE.0) RETURN
      ELSE IF (IPBC(IPU).LT.0) THEN                                     @102798
         IPBCT = -1                                                     @102798
      ELSE                                                              @102798
         GOTO 180                                                       @102798
      END IF                                                            @102798
      GOTO 125                                                          @111998
  180 IPU=IPU-1                                                         F500....
      IP=IPU                                                            F510....
      IF(IP.EQ.NPBC) GOTO 200                                           F520....
	IERROR = 1
	return
  200 IF(IPBCT.NE.-1) GOTO 400                                          F540....
  400 IF(NUBC.EQ.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = 0                                                
        GOTO 6000
      ENDIF                                                             F630....
C                                                                       F640....
C                                                                       F750....
C.....INPUT DATASET 20                                                  F760....
1120  ERRCOD = 'REA-INP-S20'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
C
      IPOS = IPOS + 1
	IBNODE(IPOS) = NUBC
1125  IPU=IPU+1                                                         @111998
      ERRCOD = 'REA-INP-20'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) IDUM                                     @042800
c      READ(INTFIL,*) IDUM                                               @042800
	IF (IERROR.NE.0) RETURN
      IF (IDUM.EQ.0) THEN                                               @041300
         GOTO 1180                                                      @041300
      ELSE IF (IPU.GT.NPBC+NUBC) THEN                                   @041300
         GOTO 1125                                                      @041300
      END IF                                                            @041300
      IUBC(IPU) = IDUM                                                  @041300

	IF (IUBC(IPU).GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IUBC(IPU) - 1                                                
c	  IF (KTYPE.EQ.2) THEN
c          IPOS = IPOS + 1
c          IBNODE(IPOS) = IUBC(IPU) - 1 + NN                                                
c	  ENDIF
	ELSE IF (IUBC(IPU).lT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = -IUBC(IPU) - 1                                                
c	  IF (KTYPE.EQ.2) THEN
c          IPOS = IPOS + 1
c          IBNODE(IPOS) = -IUBC(IPU) - 1 + NN                                                
c	  ENDIF
	ENDIF

      IF (IUBC(IPU).GT.0) THEN                                          @102798
         ERRCOD = 'REA-INP-20'                                          @042800
         READ(INTFIL,*,IOSTAT=IERROR) IUBC(IPU),UBC(IPU)                    @042800
c         READ(INTFIL,*) IUBC(IPU),UBC(IPU)                              @042800
	   IF (IERROR.NE.0) RETURN
      ELSE IF (IUBC(IPU).LT.0) THEN                                     @102798
         IUBCT = -1                                                     @102798
      ELSE                                                              @102798
         GOTO 1180                                                      @102798
      END IF                                                            @102798
      GOTO 1125                                                         @111998
 1180 IPU=IPU-1                                                         F850....
      IU=IPU-IP                                                         F860....
      IF(IU.EQ.NUBC) GOTO 6000                                          F870....
	IERROR = 1
	return
C                                                                       F980....
 6000 Continue
                                                                        F1250...
C                                                                       F1260...
      RETURN                                                            F1270...
      END                                                               F1280...
C
C
C     SUBROUTINE        D  I  M  W  R  K           SUTRA VERSION 2D3D.1 @021799c
C
C *** PURPOSE :
C ***  RETURN DIMENSIONS FOR SOLVER WORK ARRAYS.  THE DIMENSIONS DEPEND
C ***  ON THE PARTICULAR SOLVER CHOSEN.
C
      SUBROUTINE DIMWRK(KSOLVR, NSAVE, NN, NELT, NWI, NWF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C.....COMPUTE DIMENSIONS.
C

      IF (KSOLVR.EQ.1) THEN
         NWI = 31 + 2*NELT
         NWF = 2 + NN*(NSAVE + 7) + NSAVE*(NSAVE + 3) + (NELT - NN)
      ELSE
         NWI = 11 + 2*NELT
         NWF = 1 + 3*NN*(NSAVE + 1) + 7*NN + NSAVE + (NELT - NN)
      END IF

      RETURN
      END
C
C
C
C     SUBROUTINE        F  O  P  E  N              SUTRA VERSION 2D3D.1 @021799c
C                                                                       Z20....$
C *** PURPOSE :                                                         Z30....$
C ***  OPENS FILES FOR SUTRA SIMULATION.                                Z40....$
C ***  OPENS ERROR OUTPUT FILE, READS FILE NUMBERS AND NAMES,           Z50....$
C ***  CHECKS FOR EXISTENCE OF INPUT FILES, AND WRITES ERROR MESSAGES.  Z60....$
C                                                                       Z70....$
      SUBROUTINE FOPEN(UNAME,IUNIT,NFILE,IERROR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               @041300
      CHARACTER*80 FT,FN,UNAME,FNAME,ENAME,FTYPE,FTSTR,FNTMP
      LOGICAL IS                                                        Z110...$
      COMMON/FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        @111098
      COMMON/FNAMES/FNAME                                               @041300
      DIMENSION FTYPE(0:7),FNAME(0:7),IUNIT(0:7)
	DIMENSION FTSTR(0:7),FNTMP(0:7),IUTMP(0:7)
      CHARACTER*80 ERRCOD, CHERR(10)                                    @041300
      DIMENSION INERR(10), RLERR(10)                                    @041300
	DATA (FTSTR(NFT),NFT=0,7) /'LOG','INP','ICS','LST','RST',
	1   'NOD','ELE','OBS'/
C
C
C.....IDENTIFY AND OPEN ERROR OUTPUT FILE (IF ASSIGNED), OTHERWISE
C        OPEN DEFAULT ERROR OUTPUT FILE "SUTRA.LOG"
	NFILE = 0
C                                                                       Z260...$
C.....K00 HAS BEEN SET PREVIOUSLY
      K1=IUNIT(1)                                                       Z550...$
      K2=IUNIT(2)                                                       Z560...$
      K3=IUNIT(3)                                                       Z570...$
      K4=IUNIT(4)                                                       Z580...$
      K5=IUNIT(5)                                                       @102098
      K6=IUNIT(6)                                                       @102098
      K7=IUNIT(7)                                                       @111098
C
C.....CHECK FOR EXISTENCE OF INPUT FILES                                Z370...$
C        AND OPEN BOTH INPUT AND OUTPUT FILES (EXCEPT LOG FILE)
c      DO 300 NF=1,1
      IU=IUNIT(1)                                                       Z400...$
      FN=UNAME                                                          Z410...$
	IF (IU.EQ.-1) GOTO 300
c     IF(NF.LE.2) THEN                                                  Z420...$
       INQUIRE(FILE=FN,EXIST=IS)                                        Z430...$
       IF(IS) THEN                                                      Z440...$
        OPEN(UNIT=IU,FILE=FN,STATUS='OLD',FORM='FORMATTED',IOSTAT=KERR) Z450...$
c        OPEN(UNIT=IU,FILE=FN,STATUS='OLD',FORM='FORMATTED')             Z450...$
       ELSE                                                             Z460...$
        GOTO 8000                                                       Z470...$
       ENDIF                                                            Z480...$
c     ENDIF                                                             Z520...$
      IF(KERR.GT.0) GOTO 9000                                           Z530...$
  300 CONTINUE                                                          Z540...$
      RETURN                                                            Z590...$
C                                                                       Z600...$
 8000 CONTINUE
C.....WRITE ERROR MESSAGE AND STOP                                      Z630...$
	IERROR = 1
      return
C                                                                       Z680...$
 9000 CONTINUE
C.....WRITE ERROR MESSAGE AND STOP                                      Z710...$
	IERROR = 1
      return
C                                                                       Z770...$
      END                                                               Z780...$
C
C
C
C     SUBROUTINE        I  N  D  A  T  0           SUTRA VERSION 2D3D.1 @021799c
C                                                                       C20.....
C *** PURPOSE :                                                         C30.....
C ***  TO INPUT ,OUTPUT, AND ORGANIZE A PORTION OF
C ***  UNIT-K1 INPUT DATA (DATASETS 5 THROUGH 7)                        @020399
C                                                                       C60.....
      SUBROUTINE INDAT0(IERROR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               C90.....
      CHARACTER*10 ADSMOD,CDUM10                                        @102298
      CHARACTER SOLWRD(0:10)*10, SOLNAM(0:10)*40                        @021799
      CHARACTER*14 UTYPE(2)                                             C110....
      CHARACTER*6 STYPE(2)                                              C120....
      CHARACTER*10 CSOLVP, CSOLVU                                       @102798
      COMMON/FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        @111098
      COMMON/MODSOR/ ADSMOD                                             C130....
      COMMON/DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              @111798b
     1   NSOP,NSOU,NBCN                                                 C150....
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                  @061198
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA
      COMMON/TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       @081497b
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                 @102098
      COMMON/CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  C180...$
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE            @061198
      COMMON/ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      C200....
      COMMON/GRAVEC/ GRAVX,GRAVY,GRAVZ                                  @090397d
      COMMON/PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      C220...$
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2       C230....
C                                                                       C240...$
      COMMON/KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG      C250....
      COMMON /SOLVC/ SOLWRD, SOLNAM                                     @021799
      COMMON /SOLVN/ NSLVRS                                             @102798
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                      @102398b
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU           @102398b
      COMMON /ITSOLR/ TOLP,TOLU                                         @102398b
      DATA UTYPE(1)/' TEMPERATURES '/,UTYPE(2)/'CONCENTRATIONS'/        C290....
      DATA STYPE(1)/'ENERGY'/,STYPE(2)/'SOLUTE'/                        C300....
      SAVE UTYPE, STYPE                                                 @030599
      CHARACTER*80 ERRCOD, CHERR(10), FNAME(0:7)
      DIMENSION INERR(10), RLERR(10)                                    @041300
      COMMON/FNAMES/FNAME                                               @041300
      CHARACTER INTFIL*1000                                             @042500
C                                                                       C310....
      INSTOP=0                                                          C320....
C                                                                       C330....
C.....INPUT DATASET 5: NUMERICAL CONTROL PARAMETERS                     C340....
      ERRCOD = 'REA-INP-S5'                                             @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-5'                                              @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) UP,GNUP,GNUU                             @042800
c      READ(INTFIL,*) UP,GNUP,GNUU                                       @042800
	IF (IERROR.NE.0) RETURN
C                                                                       C410....
C.....INPUT DATASET 6: TEMPORAL CONTROL AND SOLUTION CYCLING DATA       C420....
      ERRCOD = 'REA-INP-S6'                                             @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-6'                                              @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) ITMAX,DELT,TMAX,ITCYC,DTMULT,DTMAX,      @042800
c      READ(INTFIL,*) ITMAX,DELT,TMAX,ITCYC,DTMULT,DTMAX,                @042800
     1   NPCYC,NUCYC                                                    @042500
	IF (IERROR.NE.0) RETURN
      IF(NPCYC.GE.1.AND.NUCYC.GE.1) GOTO 140                            C560....
	IERROR = 1
	RETURN
  140 IF(NPCYC.EQ.1.OR.NUCYC.EQ.1) GOTO 160                             C610....
	IERROR = 1
	RETURN
  160 IF (DELT.LE.DTMAX) GOTO 180                                       @020499
	IERROR = 1
	RETURN
  180 CONTINUE                                                          @020499
C.....SET MAXIMUM ALLOWED TIME STEPS IN SIMULATION FOR                  C670....
C        STEADY-STATE FLOW AND STEADY-STATE TRANSPORT SOLUTION MODES    C680....
      IF(ISSFLO.EQ.1) THEN                                              C690....
       NPCYC=ITMAX+1                                                    C700....
       NUCYC=1                                                          C710....
       ENDIF                                                            C720....
      IF(ISSTRA.EQ.1) ITMAX=1                                           C730....
C                                                                       C740....
C.....INPUT DATASET 7A: OUTER (PICARD) ITERATION CONTROLS               @020399
      ERRCOD = 'REA-INP-S7A'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-7A'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) ITRMAX                                   INDAT0.......10600
c      READ(INTFIL,*) ITRMAX                                             INDAT0.......10600
	IF (IERROR.NE.0) RETURN
      IF (ITRMAX.GT.1) THEN                                              INDAT0.......10800
         ERRCOD = 'REA-INP-7A'                                           INDAT0.......10900
         READ(INTFIL,*,IOSTAT=IERROR) ITRMAX,RPMAX,RUMAX                    INDAT0.......11000
c         READ(INTFIL,*) ITRMAX,RPMAX,RUMAX                              INDAT0.......11000
	   IF (IERROR.NE.0) RETURN
      END IF                                                             INDAT0.......11200
C                                                                       C1350...
C.....INPUT DATASETS 7B AND 7C:  INNER (SOLVER) ITERATION PARAMETERS    @020399
      ERRCOD = 'REA-INP-S7B'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-7B'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) CSOLVP                                   @042800
c      READ(INTFIL,*) CSOLVP                                              @042800
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-7B'
	IF ((CSOLVP.NE.SOLWRD(0))) 
	1   READ(INTFIL,*,IOSTAT=IERROR) CSOLVP,ITRMXP,TOLP
c	1   READ(INTFIL,*) CSOLVP,ITRMXP,TOLP
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-S7C'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-7C'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) CSOLVU                                   @042800
c      READ(INTFIL,*) CSOLVU                                             @042800
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-7C'
      IF ((CSOLVU.NE.SOLWRD(0)))
	1   READ(INTFIL,*,IOSTAT=IERROR) CSOLVU,ITRMXU,TOLU
c	1   READ(INTFIL,*) CSOLVU,ITRMXU,TOLU
	IF (IERROR.NE.0) RETURN
      KSOLVP = -1                                                       @102798
      KSOLVU = -1                                                       @102798
      DO 250 M=0,NSLVRS-1                                               @102798
         IF (CSOLVP.EQ.SOLWRD(M)) KSOLVP = M                            @102798
         IF (CSOLVU.EQ.SOLWRD(M)) KSOLVU = M                            @102798
250   CONTINUE                                                          @102798
      IF ((KSOLVP.LT.0).OR.(KSOLVU.LT.0)) THEN                          @041300
	   IERROR = 1
	   RETURN
      ELSE IF ((KSOLVP*KSOLVU.EQ.0).AND.(KSOLVP+KSOLVU.NE.0)) THEN      @041300
	   IERROR = 1
	   RETURN
      END IF                                                            @041300
      IF (KSOLVP.EQ.2) THEN
	   ITOLP = 1
	ELSE
	   ITOLP = 0
	END IF
      IF (KSOLVU.EQ.2) THEN
	   ITOLU = 1
	ELSE
	   ITOLU = 0
	END IF
	NSAVEP = 10
	NSAVEU = 10
C
C                                                                       C1190...
 1000 RETURN                                                            C3430...
      END                                                               C3440...
C     SUBROUTINE        I  N  D  A  T  1           SUTRA VERSION 2D3D.1 @021799c
C                                                                       C20.....
C *** PURPOSE :                                                         C30.....
C ***  TO INPUT ,OUTPUT, AND ORGANIZE A MAJOR PORTION OF                C40.....
C ***  UNIT-K1 INPUT DATA (DATASETS 8 THROUGH 15)                       @020399
C                                                                       C60.....
      SUBROUTINE INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,
     1   ATMIN,PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,
     2   PERMYZ,PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG,@020399
     3   IOBS, IERROR)                                                          @020399
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               C90.....
      PARAMETER (NCOLMX=9)                                              @020399
      CHARACTER*10 ADSMOD,CDUM10                                        @102298
      CHARACTER*14 UTYPE(2)                                             C110....
      CHARACTER*6 STYPE(2)                                              C120....
      COMMON/FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        @111098
      COMMON/MODSOR/ ADSMOD                                             C130....
      COMMON/DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              @111798b
     1   NSOP,NSOU,NBCN                                                 C150....
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                  @061198
      COMMON/TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       @081497b
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                 @102098
      COMMON/CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  C180...$
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE            @061198
      COMMON/ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      C200....
      COMMON/GRAVEC/ GRAVX,GRAVY,GRAVZ                                  @090397d
      COMMON/PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      C220...$
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2       C230....
C                                                                       C240...$
      COMMON/KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG      C250....
      CHARACTER K5SYM(7)*1, NCOL(NCOLMX)*1, VARNK5(7)*25                @111998
      CHARACTER K6SYM(7)*2, LCOL(NCOLMX)*2, VARNK6(7)*25                @111998
      CHARACTER*1 CNODAL,CELMNT,CINCID,CVEL,CBUDG                       @111998
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                            @110498
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL       @020399
      COMMON/OBS/ NOBSN,NTOBS,NOBCYC       
      DIMENSION IOBS(NOBSN)                                             @020399
      DIMENSION X(NN),Y(NN),Z(NN),POR(NN),SOP(NN),NREG(NN)              @061198
      DIMENSION PERMXX(NE),PERMXY(NE),PERMXZ(NEX),PERMYX(NE),PERMYY(NE),@061198
     1   PERMYZ(NEX),PERMZX(NEX),PERMZY(NEX),PERMZZ(NEX),PANGL1(NE),    @061198
     2   PANGL2(NEX),PANGL3(NEX),ALMAX(NE),ALMID(NEX),ALMIN(NE),        @061198
     3   ATMAX(NE),ATMID(NEX),ATMIN(NE),LREG(NE)
      DATA UTYPE(1)/' TEMPERATURES '/,UTYPE(2)/'CONCENTRATIONS'/        C290....
      DATA STYPE(1)/'ENERGY'/,STYPE(2)/'SOLUTE'/                        C300....
      DATA (K5SYM(MM), MM=1,7) /'N', 'X', 'Y', 'Z', 'P', 'U', 'S'/      @111998
      DATA (VARNK5(MM), MM=1,7) /'NODE NUMBER',                         @110498
     1   'X-COORDINATE', 'Y-COORDINATE', 'Z-COORDINATE',                @110498
     2   'PRESSURE', 'CONCENTRATION/TEMPERATURE', 'SATURATION'/
      DATA (K6SYM(MM), MM=1,7) /'E', 'X', 'Y', 'Z', 'VX', 'VY', 'VZ'/   @111998
      DATA (VARNK6(MM), MM=1,7) /'ELEMENT NUMBER',                      @110498
     1   'X-COORDINATE OF CENTROID', 'Y-COORDINATE OF CENTROID',        @110498
     2   'Z-COORDINATE OF CENTROID', 'X-VELOCITY', 'Y-VELOCITY',        @110498
     3   'Z-VELOCITY'/                                                  @110498
      SAVE UTYPE, STYPE, K5SYM, VARNK5, K6SYM, VARNK6                   @030599
      CHARACTER*80 ERRCOD, CHERR(10), FNAME(0:7)
      DIMENSION INERR(10), RLERR(10)                                    @041300
      COMMON/FNAMES/FNAME                                               @041300
      CHARACTER INTFIL*1000                                             @042500
C                                                                       C310....
      INSTOP=0                                                          C320....
C                                                                       C330....
C.....INPUT DATASET 8A: OUTPUT CONTROLS AND OPTIONS -- UNIT K3          @020399
      ERRCOD = 'REA-INP-S8A'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
      ERRCOD = 'REA-INP-8A'                                             @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
      READ(K1,*,IOSTAT=IERROR) NPRINT,CNODAL,CELMNT,CINCID,             @042800
c      READ(INTFIL,*) NPRINT,CNODAL,CELMNT,CINCID,                       @042800
     1   CVEL,CBUDG                                                     @042800
	IF (IERROR.NE.0) RETURN
      IF (CNODAL.EQ.'Y') THEN                                           @111998
         KNODAL = +1                                                    @111998
      ELSE IF (CNODAL.EQ.'N') THEN                                      @111998
         KNODAL = 0                                                     @111998
      ELSE                                                              @111998
	   IERROR = 1
	   RETURN
      END IF                                                            @111998
      IF (CELMNT.EQ.'Y') THEN                                           @111998
         KELMNT = +1                                                    @111998
      ELSE IF (CELMNT.EQ.'N') THEN                                      @111998
         KELMNT = 0                                                     @111998
      ELSE                                                              @111998
	   IERROR = 1
	   RETURN
      END IF                                                            @111998
      IF (CINCID.EQ.'Y') THEN                                           @111998
         KINCID = +1                                                    @111998
      ELSE IF (CINCID.EQ.'N') THEN                                      @111998
         KINCID = 0                                                     @111998
      ELSE                                                              @111998
	   IERROR = 1
	   RETURN
      END IF                                                            @111998
      IF (CVEL.EQ.'Y') THEN                                             @111998
         KVEL = +1                                                      @111998
      ELSE IF (CVEL.EQ.'N') THEN                                        @111998
         KVEL = 0                                                       @111998
      ELSE                                                              @111998
	   IERROR = 1
	   RETURN
      END IF                                                            @111998
      IF (CBUDG.EQ.'Y') THEN                                            @111998
         KBUDG = +1                                                     @111998
      ELSE IF (CBUDG.EQ.'N') THEN                                       @111998
         KBUDG = 0                                                      @111998
      ELSE                                                              @111998
	   IERROR = 1
	   RETURN
      END IF                                                            @111998
C                                                                       @041300
      IME=2                                                             C1030...
      IF(ME.EQ.+1) IME=1                                                C1040...
C                                                                       @110498
C.....INPUT DATASET 8B: OUTPUT CONTROLS AND OPTIONS -- UNIT K5          @020399
      ERRCOD = 'REA-INP-S8B'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-8B'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) NCOLPR                                   @042800
c      READ(INTFIL,*) NCOLPR                                             @042800
	IF (IERROR.NE.0) RETURN
      DO 1160 M=1,NCOLMX                                                   .
         READ(INTFIL,*,IOSTAT=IERROR) NCOLPR, (NCOL(MM), MM=1,M)            @042800
c         READ(INTFIL,*) NCOLPR, (NCOL(MM), MM=1,M)                       @042800
	   IF (IERROR.NE.0) RETURN
         IF (NCOL(M).EQ.'-') THEN
            NCOLS5 = M - 1
            GOTO 1162
         END IF
1160  CONTINUE
      NCOLS5 = NCOLMX
1162  CONTINUE
      DO 1250 M=1,NCOLS5
         DO 1200 MM=1,7
            IF (NCOL(M).EQ.K5SYM(MM)) THEN                              @111998
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN
				IERROR = 1
				RETURN
               END IF
               IF ((MM.EQ.4).AND.(IABS(KTYPE).EQ.2)) THEN
				IERROR = 1
				RETURN
               END IF
               J5COL(M) = MM
               GOTO 1250
            END IF
1200     CONTINUE
	   IERROR = 1
	   RETURN
1250  CONTINUE
C
C.....INPUT DATASET 8C: OUTPUT CONTROLS AND OPTIONS -- UNIT K6          @020399
      ERRCOD = 'REA-INP-S8C'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-8C'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) LCOLPR                                   @042800
c      READ(INTFIL,*) LCOLPR                                             @042800
	IF (IERROR.NE.0) RETURN
      DO 2160 M=1,NCOLMX
         READ(INTFIL,*,IOSTAT=IERROR) LCOLPR, (LCOL(MM), MM=1,M)            @042800
c         READ(INTFIL,*) LCOLPR, (LCOL(MM), MM=1,M)                       @042800
	   IF (IERROR.NE.0) RETURN
         IF (LCOL(M).EQ.'-') THEN
            NCOLS6 = M - 1
            GOTO 2162
         END IF
2160  CONTINUE
      NCOLS6 = NCOLMX
2162  CONTINUE
      DO 2250 M=1,NCOLS6
         DO 2200 MM=1,7
            IF (LCOL(M).EQ.K6SYM(MM)) THEN                              @111998
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN
				IERROR = 1
				RETURN
               END IF
               IF ((MM.EQ.4).AND.(IABS(KTYPE).EQ.2)) THEN
				IERROR = 1
				RETURN
               END IF
               IF ((MM.EQ.7).AND.(IABS(KTYPE).EQ.2)) THEN
				IERROR = 1
				RETURN
               END IF
               J6COL(M) = MM
               GOTO 2250
            END IF
2200     CONTINUE
	   IERROR = 1
	   RETURN
2250  CONTINUE                                                             .
C                                                                       @111098
C.....INPUT DATASET 8D: OUTPUT CONTROLS AND OPTIONS -- UNIT K7          @020399
      NOBCYC = ITMAX + 1                                                @021899
      IF (NOBSN-1.EQ.0) GOTO 4399                                       @021899
      ERRCOD = 'REA-INP-S8D'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
C.....NOBS IS ACTUAL NUMBER OF OBSERVATION NODES                        @020399
C.....NTOBS IS MAXIMUM NUMBER OF TIME STEPS WITH OBSERVATIONS           @020399
      NOBS=NOBSN-1                                                      @020399
      ERRCOD = 'REA-INP-8D'                                             @042800
      READ(K1,*,IOSTAT=IERROR) NOBCYC, (IOBS(JJ), JJ=1,NOBSN)
c      READ(K1,*) NOBCYC, (IOBS(JJ), JJ=1,NOBSN)
	IF (IERROR.NE.0) RETURN
      JSTOP=0                                                              .
      IF (IOBS(NOBSN).NE.0) THEN                                        @041300
	   IERROR = 1
	   RETURN
      END IF                                                            @041300                                   
      DO 4200 JJ=1,NOBS                                                  
         IF (IOBS(JJ).LE.0) THEN                                        @041300
		  IERROR = 1
		  RETURN
         END IF                                                         @041300
4200  CONTINUE                                                          
4399  CONTINUE                                                          @042500
C                                                                       @020399
C.....INPUT DATASET 9: FLUID PROPERTIES                                 C1360...
      ERRCOD = 'REA-INP-S9'                                             @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-9'                                              @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) COMPFL,CW,SIGMAW,RHOW0,URHOW0,           @042800
c      READ(INTFIL,*) COMPFL,CW,SIGMAW,RHOW0,URHOW0,                     @042800
     1   DRWDU,VISC0                                                    @042800
	IF (IERROR.NE.0) RETURN
C.....INPUT DATASET 10: SOLID MATRIX PROPERTIES                         C1380...
      ERRCOD = 'REA-INP-S10'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-10'                                             @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) COMPMA,CS,SIGMAS,RHOS                    @042800
c      READ(INTFIL,*) COMPMA,CS,SIGMAS,RHOS                              @042800
	IF (IERROR.NE.0) RETURN
C                                                                       C1790...
C.....INPUT DATASET 11: ADSORPTION PARAMETERS                           C1800...
      ERRCOD = 'REA-INP-S11'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-11'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) ADSMOD                                   @042800
c      READ(INTFIL,*) ADSMOD                                             @042800
	IF (IERROR.NE.0) RETURN
      IF (ADSMOD.NE.'NONE      ') THEN                                  @102798
         READ(INTFIL,*,IOSTAT=IERROR) ADSMOD,CHI1,CHI2                      @042800
c         READ(INTFIL,*) ADSMOD,CHI1,CHI2                                @042800
	   IF (IERROR.NE.0) RETURN
      END IF                                                            @102798
      IF(ME.EQ.+1) GOTO 248                                             C1830...
      IF(ADSMOD.EQ.'NONE      ') GOTO 234                               C1840...
      GOTO 236                                                          C1880...
  234 CONTINUE                                                          C1890..$
  236 IF((ADSMOD.EQ.'NONE ').OR.(ADSMOD.EQ.'LINEAR    ').OR.            C1920...
     1   (ADSMOD.EQ.'FREUNDLICH').OR.(ADSMOD.EQ.'LANGMUIR  ')) GOTO 238 C1930...
	IERROR = 1
	RETURN
  238 continue
      IF(ADSMOD.EQ.'FREUNDLICH'.AND.CHI2.LE.0.D0) THEN                  C2040...
	   IERROR = 1
	   RETURN
      ENDIF                                                             @041300
C                                                                       C2130...
C.....INPUT DATASET 12: PRODUCTION OF ENERGY OR SOLUTE MASS             C2140...
248   ERRCOD = 'REA-INP-S12'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-12'                                             @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) PRODF0,PRODS0,PRODF1,PRODS1              @042800
c      READ(INTFIL,*) PRODF0,PRODS0,PRODF1,PRODS1                        @042800
	IF (IERROR.NE.0) RETURN
C.....SET PARAMETER SWITCHES FOR EITHER ENERGY OR SOLUTE TRANSPORT      C2330...
      IF(ME) 272,272,274                                                C2340...
C     FOR SOLUTE TRANSPORT:                                             C2350...
  272 CS=0.0D0                                                          C2360...
      CW=1.D00                                                          C2370...
      SIGMAS=0.0D0                                                      C2380...
      GOTO 278                                                          C2390...
C     FOR ENERGY TRANSPORT:                                             C2400...
  274 ADSMOD='NONE      '                                               C2410...
      CHI1=0.0D0                                                        C2420...
      CHI2=0.0D0                                                        C2430...
      PRODF1=0.0D0                                                      C2440...
      PRODS1=0.0D0                                                      C2450...
  278 CONTINUE                                                          C2510...
C                                                                       C2520...
      IF (IABS(KTYPE).EQ.3) THEN                                        @102798
C.....READ 3-D INPUT FROM DATASETS 13 - 15.                             @061198
C                                                                       @061198
C.....INPUT DATASET 13: ORIENTATION OF COORDINATES TO GRAVITY           C2530...
      ERRCOD = 'REA-INP-S13'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-13'                                             @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) GRAVX,GRAVY,GRAVZ                        @042800
c      READ(INTFIL,*) GRAVX,GRAVY,GRAVZ                                  @042800
	IF (IERROR.NE.0) RETURN
C                                                                       C2620...
C.....INPUT DATASETS 14A AND 14B: NODEWISE DATA                         C2630...
      ERRCOD = 'REA-INP-S14A'                                           @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-14A'                                            @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) CDUM10,SCALX,SCALY,SCALZ,PORFAC          @042800
c      READ(INTFIL,*) CDUM10,SCALX,SCALY,SCALZ,PORFAC                    @042800
	IF (IERROR.NE.0) RETURN
      IF (CDUM10.NE.'NODE      ') THEN                                  @020999
	   IERROR = 1
	   RETURN
      END IF                                                            @020999
      NRTEST=1                                                          C2655..$
      ERRCOD = 'REA-INP-S14B'                                           @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      DO 450 I=1,NN                                                     C2660...
      ERRCOD = 'REA-INP-14B'                                            @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) II,NREG(II),X(II),Y(II),Z(II),           @042800
c      READ(INTFIL,*) II,NREG(II),X(II),Y(II),Z(II),                     @042800
     1   POR(II)                                                        @042800
	IF (IERROR.NE.0) RETURN
      X(II)=X(II)*SCALX                                                 C2690...
      Y(II)=Y(II)*SCALY                                                 C2700...
      Z(II)=Z(II)*SCALZ                                                 @090297.
      POR(II)=POR(II)*PORFAC                                            C2720...
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                  C2723..$
      NROLD=NREG(II)                                                    C2726..$
C     SET SPECIFIC PRESSURE STORATIVITY, SOP.                           C2730...
  450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                      C2740...
C                                                                       C2850...
C.....INPUT DATASETS 15A AND 15B: ELEMENTWISE DATA                      C2860...
      ERRCOD = 'REA-INP-S15A'                                           @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-15A'                                            @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) CDUM10,PMAXFA,PMIDFA,PMINFA,             @042800
c      READ(INTFIL,*) CDUM10,PMAXFA,PMIDFA,PMINFA,                       @042800
     1   ANG1FA,ANG2FA,ANG3FA,ALMAXF,ALMIDF,ALMINF,                     @042500
     1   ATMXF,ATMDF,ATMNF
	IF (IERROR.NE.0) RETURN
      IF (CDUM10.NE.'ELEMENT   ') THEN                                  @020999
	   IERROR = 1
	   RETURN
      END IF                                                            @020999
C
      LRTEST=1                                                          C2969..$
      ERRCOD = 'REA-INP-S15B'                                           @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      DO 550 LL=1,NE                                                    C2970...
      ERRCOD = 'REA-INP-15B'                                            @042800
c      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
c	IF (IERROR.NE.0) RETURN
      READ(K1,*,IOSTAT=IERROR) L,LREG(L),PMAX,PMID,PMIN,                @042800
c      READ(INTFIL,*) L,LREG(L),PMAX,PMID,PMIN,                          @042800
     1   ANGLE1,ANGLE2,ANGLE3,ALMAX(L),ALMID(L),ALMIN(L),               @042500
     1   ATMAX(L),ATMID(L),ATMIN(L)
	IF (IERROR.NE.0) RETURN
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                  C2990..$
      LROLD=LREG(L)                                                     C2995..$
      PMAX=PMAX*PMAXFA                                                  C3000...
      PMID=PMID*PMIDFA                                                  @090297.
      PMIN=PMIN*PMINFA                                                  C3010...
      ANGLE1=ANGLE1*ANG1FA                                              @090297.
      ANGLE2=ANGLE2*ANG2FA                                              @090297.
      ANGLE3=ANGLE3*ANG3FA                                              @090297.
      ALMAX(L)=ALMAX(L)*ALMAXF                                          @020999
      ALMID(L)=ALMID(L)*ALMIDF                                          @020999
      ALMIN(L)=ALMIN(L)*ALMINF                                          @020999
      ATMAX(L)=ATMAX(L)*ATMXF
      ATMID(L)=ATMID(L)*ATMDF
      ATMIN(L)=ATMIN(L)*ATMNF
C                                                                       C3090...
C.....ROTATE PERMEABILITY FROM MAX/MID/MIN TO X/Y/Z DIRECTIONS.         @101398
C.....THIS SECTION OF CODE (THROUGH THE CALL TO "TENSOR") WAS LIFTED    @090297
C.....FROM DAVE POLLOCK'S "indat13-dwp.f".                              @090297
      D2R=1.745329252D-2                                                @090297
      PANGL1(L)=D2R*ANGLE1                                              @090297
      PANGL2(L)=D2R*ANGLE2                                              @090297
      PANGL3(L)=D2R*ANGLE3                                              @090297
      ZERO = 0D0                                                        @090898
      MSTRUC = 1                                                        @090898
      CALL TENSOR(PMAX,ZERO,ZERO,ZERO,PMID,ZERO,ZERO,ZERO,PMIN,         @090898  ! kluge update this call
     1   PANGL1(L),PANGL2(L),PANGL3(L),PERMXX(L),PERMXY(L),PERMXZ(L),   @090898
     2   PERMYX(L),PERMYY(L),PERMYZ(L),PERMZX(L),PERMZY(L),PERMZZ(L),   @090898
     3   MSTRUC)                                                        @090898
  550 CONTINUE                                                          @090297.
C                                                                       @061198
      ELSE                                                              @061198
C.....READ 2-D INPUT FROM DATASETS 13 - 15.                             @061198
C.....NOTE THAT Z = THICKNESS AND PANGL1 = PANGLE.
C                                                                       @061198
C.....INPUT DATASET 13: ORIENTATION OF COORDINATES TO GRAVITY           C2530...
      ERRCOD = 'REA-INP-S13'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-13'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) GRAVX,GRAVY                          @042800
	IF (IERROR.NE.0) RETURN
      GRAVZ = 0D0
C                                                                       C2620...
C.....INPUT DATASETS 14A AND 14B: NODEWISE DATA                         C2630...
      ERRCOD = 'REA-INP-S14A'                                           @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-14A'                                            @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) CDUM10,SCALX,SCALY,SCALTH,PORFAC     @042800
	IF (IERROR.NE.0) RETURN
      IF (CDUM10.NE.'NODE      ') THEN                                  @041200
	   IERROR = 1
	   RETURN
      END IF                                                            @041200
      NRTEST=1                                                          C2655..$
      ERRCOD = 'REA-INP-S14B'                                           @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      DO 1450 I=1,NN
      ERRCOD = 'REA-INP-14B'                                            @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) II,NREG(II),X(II),Y(II),Z(II),       @042800
     1   POR(II)                                                        @042800
	IF (IERROR.NE.0) RETURN
      X(II)=X(II)*SCALX                                                 C2690...
      Y(II)=Y(II)*SCALY                                                 C2700...
      Z(II)=Z(II)*SCALTH
      POR(II)=POR(II)*PORFAC                                            C2720...
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                  C2723..$
      NROLD=NREG(II)                                                    C2726..$
C     SET SPECIFIC PRESSURE STORATIVITY, SOP.                           C2730...
 1450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL
C                                                                       C2850...
C.....INPUT DATASETS 15A AND 15B: ELEMENTWISE DATA                      C2860...
      ERRCOD = 'REA-INP-S15A'                                           @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      ERRCOD = 'REA-INP-15A'                                            @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
	if ((KTYPE.EQ.2).OR.(KTYPE.EQ.-2)) THEN
        READ(INTFIL,*,IOSTAT=IERROR) CDUM10,PMAXFA,PMINFA,               INDAT1.......47700
     1   ANG1FA,ALMAXF,ALMINF,                                           INDAT1.......47800
     1   ATMXF,ATMNF                                                     INDAT1.......47900
	ELSE 
        READ(INTFIL,*,IOSTAT=IERROR) CDUM10,PMAXFA,PMIDFA,PMINFA,         INDAT1.......47700
     1   ANG1FA,ANG2FA,ANG3FA,ALMAXF,ALMIDF,ALMINF,                      INDAT1.......47800
     1   ATMXF,ATMDF,ATMNF                                               INDAT1.......47900
	ENDIF
      	IF (IERROR.NE.0) RETURN
      IF (CDUM10.NE.'ELEMENT   ') THEN                                  @041200
	   IERROR = 1
	   RETURN
      END IF                                                            @041200
      LRTEST=1                                                          C2969..$
      ERRCOD = 'REA-INP-S15B'                                           @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
      DO 1550 LL=1,NE
      ERRCOD = 'REA-INP-15B'                                            @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
	IF ((KTYPE.EQ.2).OR.(KTYPE.EQ.-2)) THEN
        READ(INTFIL,*,IOSTAT=IERROR) L,LREG(L),PMAX,PMIN,                INDAT1.......52700
     1   ANGLE1,ALMAX(L),ALMIN(L),                                       INDAT1.......52800
     1   ATMAX(L),ATMIN(L)                                               INDAT1.......52900
	ELSE
        READ(INTFIL,*,IOSTAT=IERROR) L,LREG(L),PMAX,PMID,PMIN,             INDAT1.......52700
     1   ANGLE1,ANGLE2,ANGLE3,ALMAX(L),ALMID(L),ALMIN(L),                INDAT1.......52800
     1   ATMAX(L),ATMID(L),ATMIN(L)                                      INDAT1.......52900
	ENDIF
	IF (IERROR.NE.0) RETURN
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                  C2990..$
      LROLD=LREG(L)                                                     C2995..$
      PMAX=PMAX*PMAXFA                                                  C3000...
      PMIN=PMIN*PMINFA                                                  C3010...
      ANGLEX=ANGLEX*ANGFAC                                              C3020...
      ALMAX(L)=ALMAX(L)*ALMAXF                                          C3030...
      ALMIN(L)=ALMIN(L)*ALMINF                                          C3040...
      ATMAX(L)=ATMAX(L)*ATMAXF
      ATMIN(L)=ATMIN(L)*ATMINF
C                                                                       C3090...
C.....ROTATE PERMEABILITY FROM MAXIMUM/MINIMUM TO X/Y DIRECTIONS        C3100...
      RADIAX=1.745329D-2*ANGLEX                                         C3110...
      SINA=DSIN(RADIAX)                                                 C3120...
      COSA=DCOS(RADIAX)                                                 C3130...
      SINA2=SINA*SINA                                                   C3140...
      COSA2=COSA*COSA                                                   C3150...
      PERMXX(L)=PMAX*COSA2+PMIN*SINA2                                   C3160...
      PERMYY(L)=PMAX*SINA2+PMIN*COSA2                                   C3170...
      PERMXY(L)=(PMAX-PMIN)*SINA*COSA                                   C3180...
      PERMYX(L)=PERMXY(L)                                               C3190...
      PANGL1(L)=RADIAX                                                  C3200...
 1550 CONTINUE
C                                                                       @061198
      END IF                                                            @061198
C                                                                       C3320...
 1000 RETURN                                                            C3430...
      END                                                               C3440...
C
C     SUBROUTINE        P  R  S  W  D  S           SUTRA VERSION 2D3D.1 @021799c
C
C *** PURPOSE :
C ***  PARSE A CHARACTER STRING INTO WORDS.  WORDS ARE CONSIDERED TO BE
C ***  SEPARATED BY ONE OR MORE OF THE SINGLE-CHARACTER DELIMITER DELIM @041300
C ***  AND/OR BLANKS.                                                   @041300
C
      SUBROUTINE PRSWDS(STRING, DELIM, NWMAX, WORD, NWORDS)             @041300
      CHARACTER*80 STRING, WORD(NWMAX)                                  @041300
      CHARACTER*1 DELIM                                                 @041300
C
      DO 50 I=1,NWMAX
         WORD(I) = ""
50    CONTINUE
C
      NWORDS = 0
      M2 = 1
C
300   CONTINUE
      DO 350 M=M2,80                                                    @111998
         IF ((STRING(M:M).NE.DELIM).AND.(STRING(M:M).NE.' ')) THEN      @041300
            M1 = M
            GOTO 400
         END IF
350   CONTINUE
      RETURN
C
400   CONTINUE
      DO 450 M=M1+1,80                                                  @111998
         IF ((STRING(M:M).EQ.DELIM).OR.(STRING(M:M).EQ.' ')) THEN       @041300
            M2 = M
            GOTO 500
         END IF
450   CONTINUE
      M2 = 80                                                           @111998
C
500   CONTINUE
      NWORDS = NWORDS + 1
      WORD(NWORDS) = STRING(M1:M2-1)                                    @041300
C
      IF ((M2.LT.80).AND.(NWORDS.LT.NWMAX)) GOTO 300                    @111998
C
      RETURN
      END
C
C
C     SUBROUTINE        R  E  A  D  I  F           SUTRA VERSION 2D3D.1 @042800
C                                                                          .
C *** PURPOSE :                                                            .
C ***  TO READ A LINE FROM AN INPUT FILE INTO THE CHARACTER VARIABLE       .
C ***  INTFIL.
C
      SUBROUTINE READIF(K1, INTFIL, ERRCOD, IERROR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER INTFIL*1000
      CHARACTER*80 ERRCOD, CHERR(10)
      DIMENSION INERR(10), RLERR(10)
C
      READ(K1,'(A)',IOSTAT=IERROR) INTFIL
C                                                                          .
      RETURN                                                               .
      END                                                                  .
C                                                                       @042800
C     SUBROUTINE        R  O  T  M  A  T           SUTRA VERSION 2D3D.1 @021799c
C
C *** PURPOSE :                                                         @101398
C ***  TO COMPUTE A TRANSFORMATION MATRIX, [G], THAT CONVERTS           @101398
C ***  COORDINATES OF A VECTOR, {v}, FROM A COORDINATE SYSTEM (X, Y, Z) @101398
C ***  TO A NEW COORDINATE SYSTEM (X', Y', Z'):  {v'} = [G]{v}.         @101398
C ***  THE OVERALL TRANSFORMATION IS THE RESULT OF THREE ROTATIONS      @090898
C ***  APPLIED CONSECUTIVELY:                                           @090898
C ***  A1 = ROTATION IN THE XY-PLANE, COUNTER-CLOCKWISE FROM THE        @090898
C ***     +X-AXIS (LOOKING DOWN THE +Z-AXIS TOWARD THE ORIGIN),         @090898
C ***  A2 = ROTATION IN THE NEW XZ-PLANE, COUNTER-CLOCKWISE FROM THE    @090898
C ***     NEW +X-AXIS (LOOKING DOWN THE NEW +Y-AXIS TOWARD THE ORIGIN), @090898
C ***  A3 = ROTATION IN THE NEW YZ-PLANE, COUNTER-CLOCKWISE FROM THE    @090898
C ***     NEW +Y-AXIS (LOOKING DOWN THE NEW +X-AXIS TOWARD THE ORIGIN). @090898
C
      SUBROUTINE ROTMAT(A1,A2,A3,G11,G12,G13,G21,G22,G23,G31,G32,G33)   @101498
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      S1= DSIN(A1)
      C1= DCOS(A1)
      S2= DSIN(A2)
      C2= DCOS(A2)
      S3= DSIN(A3)
      C3= DCOS(A3)
C
C.....COMPUTE ROTATION MATRIX.
C
      G11 =  C1*C2                                                      @101498
      G12 =  -C1*S2*S3 - S1*C3                                          @101498
      G13 =  -C1*S2*C3 + S1*S3                                          @101498
      G21 =  S1*C2                                                      @101498
      G22 =  -S1*S2*S3 + C1*C3                                          @101498
      G23 =  -S1*S2*C3 - C1*S3                                          @101498
      G31 =  S2                                                         @101498
      G32 =  C2*S3                                                      @101498
      G33 =  C2*C3                                                      @101498
      RETURN
      END
C
C     SUBROUTINE        S  K  P  C  O  M           SUTRA VERSION 2D3D.1 @021799c
C
C *** PURPOSE :
C ***  IDENTIFY AND SKIP OVER COMMENT LINES IN THE UNIT K1 INPUT FILE
C ***  AND RETURN THE NUMBER OF LINES SKIPPED.
C
      SUBROUTINE SKPCOM(KU, NLSKIP, ERRCOD, IERROR)                             @042500
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                              @042500
      CHARACTER*1 CDUM
      CHARACTER*80 ERRCOD, CHERR(10), FNAME(0:7)
      DIMENSION INERR(10), RLERR(10)                                    @041300
      COMMON/FNAMES/FNAME                                               @041300
C
      NLSKIP = 0
100   READ(KU,111,IOSTAT=IERROR) CDUM
111   FORMAT (A1)
	IF (IERROR.NE.0) RETURN
      IF (CDUM.EQ.'#') THEN
         NLSKIP = NLSKIP + 1
         GOTO 100
      END IF
C
      BACKSPACE(KU)
C
900   RETURN                                                            @042500
      END
C
C                                                                       W970....
C     SUBROUTINE        S  O  U  R  C  E           SUTRA VERSION 2D3D.1 @021799c
C                                                                       E20.....
C *** PURPOSE :                                                         E30.....
C ***  TO READ AND ORGANIZE FLUID MASS SOURCE DATA AND ENERGY OR        E40.....
C ***  SOLUTE MASS SOURCE DATA.                                         E50.....
C                                                                       E60.....
      SUBROUTINE SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT,
     1  IERROR, IBOUSZ, IBNODE, IPOS)                                   E70.....
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               E80.....
      COMMON/FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        @111098
      COMMON/DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              @111798b
     1   NSOP,NSOU,NBCN                                                 E100....
      COMMON/CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  E110...$
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE            @061198
      DIMENSION QIN(NN),UIN(NN),IQSOP(NSOP),QUIN(NN),IQSOU(NSOU)        E130....
      CHARACTER*80 ERRCOD, CHERR(10), FNAME(0:7)
      DIMENSION INERR(10), RLERR(10)                                    @041300
      COMMON/FNAMES/FNAME                                               @041300
      CHARACTER INTFIL*1000                                             @042800
	INTEGER IBOUSZ, IPOS
	INTEGER IBNODE(IBOUSZ)
C                                                                       E140....
C.....NSOPI IS ACTUAL NUMBER OF FLUID SOURCE NODES                      E150....
C.....NSOUI IS ACTUAL NUMBER OF SOLUTE MASS OR ENERGY SOURCE NODES      E160....
      NSOPI=NSOP-1                                                      E170....
      NSOUI=NSOU-1                                                      E180....
      IQSOPT=1                                                          E190....
      IQSOUT=1                                                          E200....
      NIQP=0                                                            E210....
      NIQU=0                                                            E220....
      IF(NSOPI.EQ.0) then
	  ipos = 1
	  ibnode(ipos) = 0
        GOTO 1000          
      end if                                               
C                                                                       E430....
C.....INPUT DATASET 17                                                  E440....
  300 CONTINUE                                                          E450....
      IPOS = 1 
      IBNODE(IPOS) = NSOPI
      ERRCOD = 'REA-INP-S17'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
	IF (IERROR.NE.0) RETURN
305   NIQP=NIQP+1                                                       @041300
      ERRCOD = 'REA-INP-17'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=IERROR) IQCP 
	IF (IERROR.NE.0) RETURN
	IF (IQCP.GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IQCP - 1  
	ELSE IF (IQCP.lT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = -IQCP - 1                                                
	ENDIF
      IF (IQCP.EQ.0) THEN                                               @041300
         GOTO 700                                                       @041300
      ELSE IF (NIQP.GT.NSOPI) THEN                                      @041300
         GOTO 305                                                       @041300
      END IF                                                            @041300
      ERRCOD = 'REA-INP-17'                                             @042800
      IF (IQCP.GT.0) THEN                                               @102798
         READ(INTFIL,*,IOSTAT=IERROR) IQCP,QINC                         @042800 
	   IF (IERROR.NE.0) RETURN
         IF (QINC.GT.0D0) THEN                                          @102798
            READ(INTFIL,*,IOSTAT=IERROR) IQCP,QINC,UINC                 @042800
	      IF (IERROR.NE.0) RETURN
         END IF                                                         @102798
      END IF                                                            @102798
      IQSOP(NIQP)=IQCP                                                  E500....
      IF(IQCP.LT.0) IQSOPT=-1                                           E510....
      IQP=IABS(IQCP)                                                    E520....
      QIN(IQP)=QINC                                                     E530....
      UIN(IQP)=UINC                                                     E540....
  600 GOTO 305                                                          @041300
700   NIQP = NIQP - 1                                                   @041300
      IF(NIQP.EQ.NSOPI) GOTO 890                                        @041300
	   IERROR = 1
	   return
C                                                                       E770....
  890 continue
C                                                                       E780....
 1000 IF(NSOUI.EQ.0) THEN
        IPOS = IPOS + 1 
        IBNODE(IPOS) = 0        
        GOTO 9000  
      ENDIF                                                             E790....
CC                                                                       E960....
C.....INPUT DATASET 18                                                  E970....
1300  ERRCOD = 'REA-INP-S18'                                            @042500
      CALL SKPCOM(K1, NLSKIP, ERRCOD, IERROR)                                   @042500
      IPOS = IPOS + 1 
      IBNODE(IPOS) = NSOUI
1305  NIQU=NIQU+1                                                       @041300
      ERRCOD = 'REA-INP-18'                                             @042800
      CALL READIF(K1, INTFIL, ERRCOD, IERROR)                                   @042800
	IF (IERROR.NE.0) RETURN
c
c
      READ(INTFIL,*,IOSTAT=IERROR) IQCU                                 @042800
	IF (IERROR.NE.0) RETURN
c
	IF (IQCU.GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IQCU - 1   
	ELSE IF (IQCU.LT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = -IQCU - 1                                                
	ENDIF
c
      IF (IQCU.EQ.0) THEN                                               @041300
         GOTO 1700                                                      @041300
      ELSE IF (NIQU.GT.NSOUI) THEN                                      @041300
         GOTO 1305                                                      @041300
      END IF                                                            @041300
      IF (IQCU.GT.0) THEN                                               @102798
         ERRCOD = 'REA-INP-18'                                          @042800
         READ(INTFIL,*,IOSTAT=IERROR) IQCU,QUINC                        @042800 
	   IF (IERROR.NE.0) RETURN
      END IF                                                            @102798
      IQSOU(NIQU)=IQCU                                                  E1020...
      IF(IQCU.LT.0) IQSOUT=-1                                           E1030...
      IQU=IABS(IQCU)                                                    E1040...
      QUIN(IQU)=QUINC                                                   E1050...
 1600 GOTO 1305                                                         @111998
1700  NIQU = NIQU - 1                                                   @041300
      IF(NIQU.EQ.NSOUI) GOTO 9000                                       @041300
	   IERROR = 1
	   return
C                                                                       E1320...
 9000 RETURN                                                            E1330...
C                                                                       E1340...
      END                                                               E1350...
C
C                                                                       @041300
C
C     SUBROUTINE        T  E  N  S  O  R           SUTRA VERSION 2D3D.1 @021799c
C
C *** PURPOSE :                                                         @090898
C ***  TO TRANSFORM A SYMMETRIC TENSOR BETWEEN TWO COORDINATE           @090898
C ***  SYSTEMS.  "T" IS THE MATRIX EXPRESSED IN THE FIRST (INPUT)       @090898
C ***  COORDINATE SYSTEM, AND "P" IS THE MATRIX EXPRESSED IN THE SECOND @090898
C ***  (OUTPUT) COORDINATE SYSTEM.  ANGLE1, ANGLE2, AND ANGLE3 ARE THE  @090898
C ***  "YAW", "PITCH", AND "ROLL" ANGLES THAT ROTATE THE SECOND         @090898
C ***  COORDINATE SYSTEM INTO THE FIRST.                                @090898
C                                                                       @090898
      SUBROUTINE TENSOR(T11,T12,T13,T21,T22,T23,T31,T32,T33,ANGLE1,     @090898
     1   ANGLE2,ANGLE3,P11,P12,P13,P21,P22,P23,P31,P32,P33,MSTRUC)      @090898
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C.....COMPUTE TRANSFORMATION MATRIX.
C
      CALL ROTMAT(ANGLE1,ANGLE2,ANGLE3,G11,G12,G13,G21,G22,G23,         @101498
     1   G31,G32,G33)                                                   @101498
C
C.....COMPUTE TRANSFORMED TENSOR.
C
      IF (MSTRUC.EQ.0) THEN                                             @090898
         PP11 = G11*T11 + G12*T21 + G13*T31                                .
         PP12 = G11*T12 + G12*T22 + G13*T32                                .
         PP13 = G11*T13 + G12*T23 + G13*T33                                .
         PP21 = G21*T11 + G22*T21 + G23*T31
         PP22 = G21*T12 + G22*T22 + G23*T32
         PP23 = G21*T13 + G22*T23 + G23*T33
         PP31 = G31*T11 + G32*T21 + G33*T31
         PP32 = G31*T12 + G32*T22 + G33*T32
         PP33 = G31*T13 + G32*T23 + G33*T33
         P11 = PP11*G11 + PP12*G12 + PP13*G13
         P12 = PP11*G21 + PP12*G22 + PP13*G23
         P13 = PP11*G31 + PP12*G32 + PP13*G33
         P22 = PP21*G21 + PP22*G22 + PP23*G23
         P23 = PP21*G31 + PP22*G32 + PP23*G33
         P33 = PP31*G31 + PP32*G32 + PP33*G33
      ELSE
         P11= T11*G11*G11 + T22*G12*G12 + T33*G13*G13
         P12= T11*G11*G21 + T22*G12*G22 + T33*G13*G23
         P13= T11*G11*G31 + T22*G12*G32 + T33*G13*G33
         P22= T11*G21*G21 + T22*G22*G22 + T33*G23*G23
         P23= T11*G21*G31 + T22*G22*G32 + T33*G23*G33
         P33= T11*G31*G31 + T22*G32*G32 + T33*G33*G33
      END IF                                                               .
      P21= P12                                                             .
      P31= P13                                                             .
      P32= P23                                                          @090898
C
      RETURN
      END
C
C     SUBROUTINE        T  E  N  S  Y  M           SUTRA VERSION 2D3D.1 @021799c ! kluge
C
C *** PURPOSE :                                                         @090898
C ***  TO TRANSFORM A DIAGONAL MATRIX BETWEEN TWO COORDINATE
C ***  SYSTEMS.  "T" IS THE DIAGONAL MATRIX EXPRESSED IN THE FIRST
C ***  (INPUT) COORDINATE SYSTEM; "P" IS THE MATRIX EXPRESSED IN THE
C ***  SECOND (OUTPUT) COORDINATE SYSTEM; AND "Q" IS THE TRANSFORMATION
C ***  MATRIX.
C
C
C
C     SUBROUTINE        Z  E  R  O                 SUTRA VERSION 2D3D.1 @021799c
C                                                                       M20.....
C *** PURPOSE :                                                         M30.....
C ***  TO FILL AN ARRAY WITH A CONSTANT VALUE.                          M40.....
C                                                                       M50.....
      SUBROUTINE ZERO(A,IADIM,FILL)                                     M60.....
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               M70.....
      DIMENSION A(IADIM)                                                M80.....
C                                                                       M90.....
C.....FILL ARRAY A WITH VALUE IN VARIABLE 'FILL'                        M100....
      DO 10 I=1,IADIM                                                   M110....
   10 A(I)=FILL                                                         M120....
C                                                                       M130....
C                                                                       M140....
      RETURN                                                            M150....
      END                                                               M160....
      SUBROUTINE SUTRA_LABELS(FLABELS)
!DEC$ attributes dllexport :: SUTRA_LABELS
	CHARACTER(*) FLABELS
      FLABELS  = 'fluid src/sink                          energy/sol src
     &/sink                     spec. pressure                          
     &spec. conc/temp                         '
      RETURN
      END
