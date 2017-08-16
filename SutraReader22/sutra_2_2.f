C     MAIN PROGRAM       S U T R A _ M A I N       SUTRA VERSION 2.2     SUTRA_MAIN.....100
C_______________________________________________________________________ SUTRA_MAIN.....200
C|                                                                     | SUTRA_MAIN.....300
C|                                                                     | SUTRA_MAIN.....400
C|                   UNITED STATES GEOLOGICAL SURVEY                   | SUTRA_MAIN.....500
C|          MODEL FOR SATURATED-UNSATURATED, VARIABLE-DENSITY          | SUTRA_MAIN.....600
C|          GROUND-WATER FLOW WITH SOLUTE OR ENERGY TRANSPORT          | SUTRA_MAIN.....700
C|                                                                     | SUTRA_MAIN.....800
C|                                                                     | SUTRA_MAIN.....900
C|                                                                     | SUTRA_MAIN....1000
C|                                                                     | SUTRA_MAIN....1100
C|                       _______________________                       | SUTRA_MAIN....1200
C|                      |                       |                      | SUTRA_MAIN....1300
C|                      |   S   U   T   R   A   |                      | SUTRA_MAIN....1400
C|                      |_______________________|                      | SUTRA_MAIN....1500
C|                                                                     | SUTRA_MAIN....1600
C|                                                                     | SUTRA_MAIN....1700
C|                Saturated    Unsaturated    TRAnsport                | SUTRA_MAIN....1800
C|                =            =              ===                      | SUTRA_MAIN....1900
C|                                                                     | SUTRA_MAIN....2000
C|                                                                     | SUTRA_MAIN....2100
C|                                                                     | SUTRA_MAIN....2200
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....2300
C|    *                                                           *    | SUTRA_MAIN....2400
C|    *  PHYSICS OPTIONS:                                         *    | SUTRA_MAIN....2500
C|    *  -> Saturated and/or unsaturated ground-water flow        *    | SUTRA_MAIN....2600
C|    *  -> Either single species reactive solute transport       *    | SUTRA_MAIN....2700
C|    *     or thermal energy transport                           *    | SUTRA_MAIN....2800
C|    *  GEOMETRY OPTIONS:                                        *    | SUTRA_MAIN....2900
C|    *  -> Two-dimensional areal or cross-sectional simulation   *    | SUTRA_MAIN....3000
C|    *  -> Fully three-dimensional simulation                    *    | SUTRA_MAIN....3100
C|    *  -> Either two- or three-dimensional Cartesian or         *    | SUTRA_MAIN....3200
C|    *     two-dimensional radial coordinates                    *    | SUTRA_MAIN....3300
C|    *  NUMERICAL METHODS:                                       *    | SUTRA_MAIN....3400
C|    *  -> Hybrid Galerkin-finite-element method and             *    | SUTRA_MAIN....3500
C|    *     integrated-finite-difference method                   *    | SUTRA_MAIN....3600
C|    *     with two-dimensional quadrilateral or                 *    | SUTRA_MAIN....3700
C|    *     three-dimensional generalized hexahedral              *    | SUTRA_MAIN....3800
C|    *     finite elements                                       *    | SUTRA_MAIN....3900
C|    *  -> Finite-difference time discretization                 *    | SUTRA_MAIN....4000
C|    *  -> Nonlinear iterative, sequential or steady-state       *    | SUTRA_MAIN....4100
C|    *     solution modes                                        *    | SUTRA_MAIN....4200
C|    *  -> Direct and iterative solvers                          *    | SUTRA_MAIN....4300
C|    *  OUTPUT OPTIONS:                                          *    | SUTRA_MAIN....4400
C|    *  -> Optional fluid velocity calculation                   *    | SUTRA_MAIN....4500
C|    *  -> Optional observation well output                      *    | SUTRA_MAIN....4600
C|    *  -> Optional fluid mass and solute mass or energy budget  *    | SUTRA_MAIN....4700
C|    *  -> Flexible, columnwise output of solution               *    | SUTRA_MAIN....4800
C|    *                                                           *    | SUTRA_MAIN....4900
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....5000
C|                                                                     | SUTRA_MAIN....5100
C|                                                                     | SUTRA_MAIN....5200
C|                                                                     | SUTRA_MAIN....5300
C|       Complete explanation of the function and use of this code     | SUTRA_MAIN....5400
C|       is given in :                                                 | SUTRA_MAIN....5500
C|                                                                     | SUTRA_MAIN....5600
C|       Voss, Clifford I., and Provost, Alden M., 2002,               | SUTRA_MAIN....5700
C|            SUTRA - A model for saturated-unsaturated                | SUTRA_MAIN....5800
C|            variable-density ground-water flow with                  | SUTRA_MAIN....5900
C|            solute or energy transport: U.S. Geological              | SUTRA_MAIN....6000
C|            Survey Water-Resources Investigations Report             | SUTRA_MAIN....6100
C|            02-4231, 291 p. (Version of Oct 22, 2009)                | SUTRA_MAIN....6200
C|                                                                     | SUTRA_MAIN....6300
C|                                                                     | SUTRA_MAIN....6400
C|                                                                     | SUTRA_MAIN....6500
C|       Users who wish to be notified of updates of the SUTRA         | SUTRA_MAIN....6600
C|       code and documentation may be added to the mailing list       | SUTRA_MAIN....6700
C|       by sending a request to :                                     | SUTRA_MAIN....6800
C|                                                                     | SUTRA_MAIN....6900
C|                           SUTRA Support                             | SUTRA_MAIN....7000
C|                       U.S. Geological Survey                        | SUTRA_MAIN....7100
C|                        411 National Center                          | SUTRA_MAIN....7200
C|                       Reston, Virginia 20192                        | SUTRA_MAIN....7300
C|                                USA                                  | SUTRA_MAIN....7400
C|                                                                     | SUTRA_MAIN....7500
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....7600
C|    *                                                           *    | SUTRA_MAIN....7700
C|    *  The SUTRA code and documentation were originally         *    | SUTRA_MAIN....7800
C|    *  prepared under a joint research project of the U.S.      *    | SUTRA_MAIN....7900
C|    *  Geological Survey, Department of the Interior, Reston,   *    | SUTRA_MAIN....8000
C|    *  Virginia, and the Engineering and Services Laboratory,   *    | SUTRA_MAIN....8100
C|    *  U.S. Air Force Engineering and Services Center, Tyndall  *    | SUTRA_MAIN....8200
C|    *  A.F.B., Florida.  The SUTRA code and documentation are   *    | SUTRA_MAIN....8300
C|    *  available for unlimited distribution.                    *    | SUTRA_MAIN....8400
C|    *                                                           *    | SUTRA_MAIN....8500
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....8600
C|                                                                     | SUTRA_MAIN....8700
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....8800
C|    *                                                           *    | SUTRA_MAIN....8900
C|    *  Original Release: 1984, Version 1.0                      *    | SUTRA_MAIN....9000
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    | SUTRA_MAIN....9100
C|    *                                                           *    | SUTRA_MAIN....9200
C|    *  First Revision: June 1990, Version 1.1 (V06902D)         *    | SUTRA_MAIN....9300
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    | SUTRA_MAIN....9400
C|    *                                                           *    | SUTRA_MAIN....9500
C|    *  Second Revision: September 1997, Version 1.2 (V09972D)   *    | SUTRA_MAIN....9600
C|    *  by: C.I. Voss and David Boldt, U.S. Geological Survey    *    | SUTRA_MAIN....9700
C|    *                                                           *    | SUTRA_MAIN....9800
C|    *  Third Revision: September 2003, Version 2.0 (2D3D.1)     *    | SUTRA_MAIN....9900
C|    *  by: A.M. Provost & C.I. Voss, U.S. Geological Survey     *    | SUTRA_MAIN...10000
C|    *                                                           *    | SUTRA_MAIN...10100
C|    *  Fourth Revision: June 2008, Version 2.1                  *    | SUTRA_MAIN...10200
C|    *  by: A.M. Provost & C.I. Voss, U.S. Geological Survey     *    | SUTRA_MAIN...10300
C|    *                                                           *    | SUTRA_MAIN...10400
C|    *  Fifth Revision: October 2009, Version 2.2                *    | SUTRA_MAIN...10500
C|    *  by: A.M. Provost & C.I. Voss, U.S. Geological Survey     *    | SUTRA_MAIN...10600
C|    *                                                           *    | SUTRA_MAIN...10700
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN...10800
C|                                                                     | SUTRA_MAIN...10900
C|                                                                     | SUTRA_MAIN...11000
C|_____________________________________________________________________| SUTRA_MAIN...11100
C                                                                        SUTRA_MAIN...11200
C                                                                        SUTRA_MAIN...11300
C                                                                        SUTRA_MAIN...11400
C RBW START
C
C IERROR IS SET TO a non zero value IF AN ERROR OCCURS
C
C IF ISTART <= 0 THEN DATA SETS 1 - 4 ARE READ.
C IF ISTART > 0 THEN DATA SETS 1-21 ARE READ.
C
C IBOUSZ IS THE SIZE OF THE ARRAY IBNODE
C
C IBNODE IS THE NODE NUMBERS OF THE BOUNDARY CONDITIONS AS WELL AS THE 
C NUMBER OF EACH TYPE OF BOUNDARY CONDITION.
C THE FIRST NUMBER IN THE ARRAY IS THE NUMBER OF BOUNDARY CONDITIONS OF
C THE FIRST TYPE FOLLOWED BY THE NODE NUMBERS FOR THAT TYPE OF BOUNDARY
C CONDITIONS. THAT IS FOLLOWED BY A SIMILAR ARRANGEMENT FOR EACH TYPE 
C OF BOUNDARY CONDITION. 
C
C NFEAT IS THE NUMBER OF TYPES OF BOUNDARY FEATURES (4 FOR SUTRA)
C
C ISTEADYFLOW      IS SET TO 1 IF FLOW      IS STEADY.  OTHERWISE IT IS SET TO 0.
C ISTEADYTRANSPORT IS SET TO 1 IF TRANSPORT IS STEADY.  OTHERWISE IT IS SET TO 0.
C
C ElementValues contains space for data values for each element.
C
C IElementValueCount is the size of the ElementValues array.
C
C NodeValues contains space for data values for each node.
C
C INodeValueCount is the size of the NodeValues array.
C
C INPFILE is the name of the main SUTRA input file.
C
C Incidence lists the nodes for each element.  The node numbers have been 
C reduced by 1 because in the Model Viewer C++ code, arrays start at zero 
C instead of at 1.
C 
C MeshInfo(1) is set to 2 for 2D models and 3 for 3D models
C MeshInfo(2) indicates the type of mesh.
C        IRREGULAR MESH      ==>   MeshInfo(2) = 0  
C        LAYERED MESH ACROSS ==>   MeshInfo(2) = 1  
C        REGULAR MESH        ==>   MeshInfo(2) = 2  
C        BLOCKWISE MESH      ==>   MeshInfo(2) = 3  
C        LAYERED MESH WITHIN ==>   MeshInfo(2) = 4  
C MeshInfo(3) is set to the number of node layers if the mesh is a layered mesh.
C Otherwise it is set to 0.
C RBW end
      SUBROUTINE SUTRA_22(IERROR, ISTART, IBOUSZ, 
     &        IBNODE, NFEAT, ISTEADYFLOW, ISTEADYTRANSPORT,
     &        ElementValues, IElementValueCount, Incidence, NodeValues, 
     &        INodeValueCount, MeshInfo, INPFILE)     
!DEC$ attributes dllexport :: SUTRA_22
!      PROGRAM SUTRA_MAIN                                                 SUTRA_MAIN...11500
      USE ALLARR                                                         SUTRA_MAIN...11600
      USE PTRDEF                                                         SUTRA_MAIN...11700
      USE EXPINT                                                         SUTRA_MAIN...11800
      USE SCHDEF                                                         SUTRA_MAIN...11900
      USE BCSDEF                                                         SUTRA_MAIN...12000
      USE FINDEF                                                         SUTRA_MAIN...12100
      USE LLDEF                                                          SUTRA_MAIN...12200
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTRA_MAIN...12300
      PARAMETER (NCOLMX=9)                                               SUTRA_MAIN...12400
C                                                                        SUTRA_MAIN...12500
C.....PROGRAMMERS SET SUTRA VERSION NUMBER HERE (8 CHARACTERS MAXIMUM)   SUTRA_MAIN...12600
      CHARACTER*8, PARAMETER :: VERN='2.2'                               SUTRA_MAIN...12700
C                                                                        SUTRA_MAIN...12800
      CHARACTER*8 VERNUM, VERNIN                                         SUTRA_MAIN...12900
      CHARACTER*1 TITLE1(80),TITLE2(80)                                  SUTRA_MAIN...13000
      CHARACTER*80 SIMULA(5),MSHTYP(2),LAYNOR(2),SIMSTR,MSHSTR,LAYSTR    SUTRA_MAIN...13100
      CHARACTER*80 CUNSAT, CSSFLO ,CSSTRA, CREAD                         SUTRA_MAIN...13200
      CHARACTER*80 UNSSTR, SSFSTR ,SSTSTR, RDSTR                         SUTRA_MAIN...13300
      CHARACTER*80 UNAME,FNAME,FNINP,FNICS,FNBCS                         SUTRA_MAIN...13400
      CHARACTER*80 ERRCOD,CHERR(10)                                      SUTRA_MAIN...13500
      CHARACTER*40 SOLNAM(0:10)                                          SUTRA_MAIN...13600
      CHARACTER*10 SOLWRD(0:10)                                          SUTRA_MAIN...13700
      CHARACTER*10 ADSMOD                                                SUTRA_MAIN...13800
      CHARACTER INTFIL*1000                                              SUTRA_MAIN...13900
      CHARACTER*10 BCSSCH                                                SUTRA_MAIN...14000
      CHARACTER*80 CDUM80                                                SUTRA_MAIN...14100
      INTEGER RMVDIM,IMVDIM,CMVDIM,PMVDIM,LMVDIM                         SUTRA_MAIN...14200
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                SUTRA_MAIN...14300
      LOGICAL ONCEK10,ONCEK11,ONCEK12,ONCEK13                            SUTRA_MAIN...14400
      LOGICAL ONCEFO                                                     SUTRA_MAIN...14500
      LOGICAL ONCEBCS, SETBCS                                            SUTRA_MAIN...14600
      LOGICAL ALCBCS,ALCFIN,ALCOBS                                       SUTRA_MAIN...14700
      DIMENSION FNAME(0:13),IUNIT(0:13)                                  SUTRA_MAIN...14800
      DIMENSION INERR(10), RLERR(10)                                     SUTRA_MAIN...14900
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             SUTRA_MAIN...15000
      ALLOCATABLE :: FNBCS(:), IUBCS(:)                                  SUTRA_MAIN...15100
      DIMENSION KTYPE(2)                                                 SUTRA_MAIN...15200
      TYPE (LLD), POINTER :: DENB                                        SUTRA_MAIN...15300
      COMMON /ALC/ ALCBCS,ALCFIN,ALCOBS                                  SUTRA_MAIN...15400
      COMMON /BCSL/ ONCEBCS                                              SUTRA_MAIN...15500
      COMMON /CLAY/ LAYSTR                                               SUTRA_MAIN...15600
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SUTRA_MAIN...15700
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,           SUTRA_MAIN...15800
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE                                       SUTRA_MAIN...15900
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  SUTRA_MAIN...16000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTRA_MAIN...16100
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            SUTRA_MAIN...16200
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        SUTRA_MAIN...16300
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        SUTRA_MAIN...16400
      COMMON /FNAMES/ UNAME,FNAME                                        SUTRA_MAIN...16500
      COMMON /FO/ONCEFO                                                  SUTRA_MAIN...16600
      COMMON /FUNIB/ NFBCS                                               SUTRA_MAIN...16700
      COMMON /FUNITA/ IUNIT                                              SUTRA_MAIN...16800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 SUTRA_MAIN...16900
     1   K10,K11,K12,K13                                                 SUTRA_MAIN...17000
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      SUTRA_MAIN...17100
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            SUTRA_MAIN...17200
      COMMON /ITSOLR/ TOLP,TOLU                                          SUTRA_MAIN...17300
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        SUTRA_MAIN...17400
      COMMON /KPRBCS/ KINACT                                             SUTRA_MAIN...17500
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                SUTRA_MAIN...17600
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            SUTRA_MAIN...17700
      COMMON /MODSOR/ ADSMOD                                             SUTRA_MAIN...17800
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      SUTRA_MAIN...17900
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SUTRA_MAIN...18000
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SUTRA_MAIN...18100
      COMMON /PLT1/ ONCEK5, ONCEK6, ONCEK7, ONCEK8                       SUTRA_MAIN...18200
      COMMON /PLT2/ ONCEK10, ONCEK11, ONCEK12, ONCEK13                   SUTRA_MAIN...18300
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    SUTRA_MAIN...18400
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      SUTRA_MAIN...18500
      COMMON /SOLVN/ NSLVRS                                              SUTRA_MAIN...18600
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       SUTRA_MAIN...18700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SUTRA_MAIN...18800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      SUTRA_MAIN...18900
      COMMON /VER/ VERNUM, VERNIN                                        SUTRA_MAIN...19000
c rbw begin
	INTEGER IERROR
	INTEGER IBOUSZ,NFEAT
	INTEGER IBNODE(IBOUSZ)
	REAL (KIND = 4) ElementValues(IElementValueCount)
	REAL (KIND = 4) NodeValues(INodeValueCount)
	INTEGER  :: Incidence(*)
	INTEGER IncidenceIndex
      CHARACTER(*) INPFILE
	INTEGER MeshInfo(3)
c rbw end
C....."NSLVRS" AND THE ARRAYS "SOLWRD" AND "SOLNAM" ARE INITIALIZED      SUTRA_MAIN...19100
C        IN THE BLOCK-DATA SUBPROGRAM "BDINIT"                           SUTRA_MAIN...19200
C                                                                        SUTRA_MAIN...19300
C                                                                        SUTRA_MAIN...19400
C.....COPY PARAMETER VERN (SUTRA VERSION NUMBER) TO VARIABLE VERNUM,     SUTRA_MAIN...19500
C        WHICH IS PASSED THROUGH COMMON BLOCK VER.                       SUTRA_MAIN...19600
      VERNUM = VERN                                                      SUTRA_MAIN...19700
	IPOS = 0
C                                                                        SUTRA_MAIN...19800
C.....SET THE ALLOCATION FLAGS TO FALSE                                  SUTRA_MAIN...19900
c rbw begin
      IERROR = 0
c rbw emd
      ALLO1 = .FALSE.                                                    SUTRA_MAIN...20000
      ALLO2 = .FALSE.                                                    SUTRA_MAIN...20100
      ALLO3 = .FALSE.                                                    SUTRA_MAIN...20200
C                                                                        SUTRA_MAIN...20300
C.....INITIALIZE FLAG TO INDICATE THAT BCSTEP HAS NOT YET BEEN CALLED    SUTRA_MAIN...20400
C        IN THE MAIN PROGRAM                                             SUTRA_MAIN...20500
      ONCEBCS = .FALSE.                                                  SUTRA_MAIN...20600
C                                                                        SUTRA_MAIN...20700
C_______________________________________________________________________ SUTRA_MAIN...20800
C|                                                                     | SUTRA_MAIN...20900
C|  *****************************************************************  | SUTRA_MAIN...21000
C|  *                                                               *  | SUTRA_MAIN...21100
C|  *   **********  M E M O R Y   A L L O C A T I O N  **********   *  | SUTRA_MAIN...21200
C|  *                                                               *  | SUTRA_MAIN...21300
C|  *   The main arrays used by SUTRA are dimensioned dynamically   *  | SUTRA_MAIN...21400
C|  *   in the main program, SUTRA_MAIN.  The amount of storage     *  | SUTRA_MAIN...21500
C|  *   required by these arrays depends on the dimensionality of   *  | SUTRA_MAIN...21600
C|  *   the problem (2D or 3D) and the particular solver(s) used.   *  | SUTRA_MAIN...21700
C|  *                                                               *  | SUTRA_MAIN...21800
C|  *               |---------------------|---------------------|   *  | SUTRA_MAIN...21900
C|  *               |     sum of real     |    sum of integer   |   *  | SUTRA_MAIN...22000
C|  *               |   array dimensions  |   array dimensions  |   *  | SUTRA_MAIN...22100
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...22200
C|  *   | 2D,       | (2*NBI+27)*NN+19*NE |   NN+5*NE+2*NSOP    |   *  | SUTRA_MAIN...22300
C|  *   | direct    |    +3*NBCN+6*NOBS   | +2*NSOU+4*NBCN+NOBS |   *  | SUTRA_MAIN...22400
C|  *   | solver    |   +2*NSCH+22+NRBCS  |  +3*NSCH+4+NI1BCS   |   *  | SUTRA_MAIN...22500
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...22600
C|  *   | 2D,       | 2*NELT+28*NN+19*NE  |   NELT+2*NN+5*NE    |   *  | SUTRA_MAIN...22700
C|  *   | iterative |   +3*NBCN+6*NOBS    |   +2*NSOP+2*NSOU    |   *  | SUTRA_MAIN...22800
C|  *   | solver(s) |   +2*NSCH+NWF+220   | +4*NBCN+NOBS+3*NSCH |   *  | SUTRA_MAIN...22900
C|  *   |           |       +NRBCS        |   +NWI+2+NI1BCS     |   *  | SUTRA_MAIN...23000
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...23100
C|  *   | 3D,       | (2*NBI+27)*NN+45*NE |   NN+9*NE+2*NSOP    |   *  | SUTRA_MAIN...23200
C|  *   | direct    |    +3*NBCN+6*NOBS   | +2*NSOU+4*NBCN+NOBS |   *  | SUTRA_MAIN...23300
C|  *   | solver    |   +2*NSCH+8+NRBCS   |  +3*NSCH+4+NI1BCS   |   *  | SUTRA_MAIN...23400
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...23500
C|  *   | 3D,       | 2*NELT+28*NN+45*NE  |   NELT+2*NN+9*NE    |   *  | SUTRA_MAIN...23600
C|  *   | iterative |   +3*NBCN+6*NOBS    |   +2*NSOP+2*NSOU    |   *  | SUTRA_MAIN...23700
C|  *   | solver(s) |   +2*NSCH+NWF+6     | +4*NBCN+NOBS+3*NSCH |   *  | SUTRA_MAIN...23800
C|  *   |           |       +NRBCS        |   +NWI+2+NI1BCS     |   *  | SUTRA_MAIN...23900
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...24000
C|  *                                                               *  | SUTRA_MAIN...24100
C|  *               |---------------------|---------------------|   *  | SUTRA_MAIN...24200
C|  *               |  sum of character   |  sum of dimensions  |   *  | SUTRA_MAIN...24300
C|  *               |   array effective   |     of arrays of    |   *  | SUTRA_MAIN...24400
C|  *               |     dimensions      |       pointers      |   *  | SUTRA_MAIN...24500
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...24600
C|  *   | all cases |   73*NOBS+89*NSCH   |        2*NSCH       |   *  | SUTRA_MAIN...24700
C|  *   |           |       +NCIDB        |                     |   *  | SUTRA_MAIN...24800
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...24900
C|  *                                                               *  | SUTRA_MAIN...25000
C|  *               |---------------------|                         *  | SUTRA_MAIN...25100
C|  *               |    sum of logical   |                         *  | SUTRA_MAIN...25200
C|  *               |   array dimensions  |                         *  | SUTRA_MAIN...25300
C|  *   |-----------|---------------------|                         *  | SUTRA_MAIN...25400
C|  *   | all cases |        ITMAX        |                         *  | SUTRA_MAIN...25500
C|  *   |-----------|---------------------|                         *  | SUTRA_MAIN...25600
C|  *                                                               *  | SUTRA_MAIN...25700
C|  *   Quantities in the tables above are defined in Section 7.3   *  | SUTRA_MAIN...25800
C|  *   of the published documentation (Voss & Provost, 2002,       *  | SUTRA_MAIN...25900
C|  *   USGS Water-Resources Investigations Report 02-4231,         *  | SUTRA_MAIN...26000
C|  *   Version of Oct 22, 2009).                                   *  | SUTRA_MAIN...26100
C|  *                                                               *  | SUTRA_MAIN...26200
C|  *   During each run, SUTRA writes memory usage information to   *  | SUTRA_MAIN...26300
C|  *   the LST output file.                                        *  | SUTRA_MAIN...26400
C|  *                                                               *  | SUTRA_MAIN...26500
C|  *****************************************************************  | SUTRA_MAIN...26600
C|_____________________________________________________________________| SUTRA_MAIN...26700
C                                                                        SUTRA_MAIN...26800
C                                                                        SUTRA_MAIN...26900
C_______________________________________________________________________ SUTRA_MAIN...27000
C|                                                                     | SUTRA_MAIN...27100
C|  *****************************************************************  | SUTRA_MAIN...27200
C|  *                                                               *  | SUTRA_MAIN...27300
C|  *   ***********  F I L E   A S S I G N M E N T S  ***********   *  | SUTRA_MAIN...27400
C|  *                                                               *  | SUTRA_MAIN...27500
C|  *   Unit K0 contains the FORTRAN unit number and filename       *  | SUTRA_MAIN...27600
C|  *   assignments for the various SUTRA input and output files.   *  | SUTRA_MAIN...27700
C|  *   Each line of Unit K0 begins with a file type, followed by   *  | SUTRA_MAIN...27800
C|  *   a unit number and a filename for that type, all in free     *  | SUTRA_MAIN...27900
C|  *   format. Permitted file types are INP, BCS, ICS, LST, RST,   *  | SUTRA_MAIN...28000
C|  *   NOD, ELE, BCOF, BCOP, BCOS, BCOU, OBS, OBC, and SMY.        *  | SUTRA_MAIN...28100
C|  *   Assignments may be listed in any order.                     *  | SUTRA_MAIN...28200
C|  *   Example ("#" indicates a comment):                          *  | SUTRA_MAIN...28300
C|  *   'INP'  50  'project.inp'   # required                       *  | SUTRA_MAIN...28400
C|  *   'BCS'  52  'project.bcs'   # optional                       *  | SUTRA_MAIN...28500
C|  *   'ICS'  55  'project.ics'   # required                       *  | SUTRA_MAIN...28600
C|  *   'LST'  60  'project.lst'   # required                       *  | SUTRA_MAIN...28700
C|  *   'RST'  66  'project.rst'   # optional                       *  | SUTRA_MAIN...28800
C|  *   'NOD'  70  'project.nod'   # optional                       *  | SUTRA_MAIN...28900
C|  *   'ELE'  80  'project.ele'   # optional                       *  | SUTRA_MAIN...29000
C|  *   'OBS'  90  'project.obs'   # optional                       *  | SUTRA_MAIN...29100
C|  *   'OBC'  90  'project.obc'   # optional                       *  | SUTRA_MAIN...29200
C|  *   'BCOF' 95  'project.bcof'  # optional                       *  | SUTRA_MAIN...29300
C|  *   'BCOP' 96  'project.bcop'  # optional                       *  | SUTRA_MAIN...29400
C|  *   'BCOS' 97  'project.bcos'  # optional                       *  | SUTRA_MAIN...29500
C|  *   'BCOU' 98  'project.bcou'  # optional                       *  | SUTRA_MAIN...29600
C|  *   'SMY'  40  'project.smy'   # optional; defaults to          *  | SUTRA_MAIN...29700
C|  *                              #           filename="SUTRA.SMY" *  | SUTRA_MAIN...29800
C|  *                                                               *  | SUTRA_MAIN...29900
C|  *   Note that the filenames for types OBS and OBC are actually  *  | SUTRA_MAIN...30000
C|  *   root names from which SUTRA will automatically generate     *  | SUTRA_MAIN...30100
C|  *   observation output filenames based on the combinations of   *  | SUTRA_MAIN...30200
C|  *   schedules and output formats that appear in the observation *  | SUTRA_MAIN...30300
C|  *   specifications.  If a unit number of zero is specified for  *  | SUTRA_MAIN...30400
C|  *   a file, SUTRA will automatically assign a valid unit number *  | SUTRA_MAIN...30500
C|  *   to that file.                                               *  | SUTRA_MAIN...30600
C|  *                                                               *  | SUTRA_MAIN...30700
C|  *****************************************************************  | SUTRA_MAIN...30800
C|_____________________________________________________________________| SUTRA_MAIN...30900
C                                                                        SUTRA_MAIN...31000
C.....SET FILENAME AND FORTRAN UNIT NUMBER FOR UNIT K0                   SUTRA_MAIN...31100
!RBW begin change
!      UNAME = 'SUTRA.FIL'                                                SUTRA_MAIN...31200
      UNAME = INPFILE                                                     SUTRA_MAIN...31200
!RBW end change
      K0 = 10                                                            SUTRA_MAIN...31300
C.....INITIALIZE NFLOMX TO ZERO NOW IN CASE TERMINATION SEQUENCE IS      SUTRA_MAIN...31400
C        CALLED BEFORE NFLOMX GETS SET.                                  SUTRA_MAIN...31500
      NFLOMX = 0                                                         SUTRA_MAIN...31600
C.....ASSIGN UNIT NUMBERS AND OPEN FILE UNITS FOR THIS SIMULATION,       SUTRA_MAIN...31700
C        EXCEPT OBSERVATION OUTPUT FILES.                                SUTRA_MAIN...31800
      ONCEFO = .FALSE.                                                   SUTRA_MAIN...31900
      CALL FOPEN(IERROR)                                                       SUTRA_MAIN...32000
	IF (IERROR.NE.0) GOTO 9000
C.....STORE INP, BCS, AND ICS FILENAMES FOR LATER REFERENCE, SINCE THE   SUTRA_MAIN...32100
C        CORRESPONDING ENTRIES IN FNAME MAY BE OVERWRITTEN BY FILE       SUTRA_MAIN...32200
C        INSERTION.                                                      SUTRA_MAIN...32300
      FNINP = FNAME(1)                                                   SUTRA_MAIN...32400
      FNICS = FNAME(2)                                                   SUTRA_MAIN...32500
      ALLOCATE (FNBCS(NFBCS), IUBCS(NFBCS))                              SUTRA_MAIN...32600
      DO 30 NFB=1,NFBCS                                                  SUTRA_MAIN...32700
         FNBCS(NFB) = FNAMB(NFB)                                         SUTRA_MAIN...32800
         IUBCS(NFB) = IUNIB(NFB)                                         SUTRA_MAIN...32900
   30 CONTINUE                                                           SUTRA_MAIN...33000
C                                                                        SUTRA_MAIN...33100
C                                                                        SUTRA_MAIN...33200
C.....OUTPUT BANNER                                                      SUTRA_MAIN...33300
!      WRITE(K3,110) TRIM(VERNUM)                                         SUTRA_MAIN...33400
!  110 FORMAT('1',131('*')////3(132('*')////)////                         SUTRA_MAIN...33500
!     1   47X,' SSSS   UU  UU  TTTTTT  RRRRR     AA  '/                   SUTRA_MAIN...33600
!     2   47X,'SS   S  UU  UU  T TT T  RR  RR   AAAA '/                   SUTRA_MAIN...33700
!     3   47X,'SSSS    UU  UU    TT    RRRRR   AA  AA'/                   SUTRA_MAIN...33800
!     4   47X,'    SS  UU  UU    TT    RR R    AAAAAA'/                   SUTRA_MAIN...33900
!     5   47X,'SS  SS  UU  UU    TT    RR RR   AA  AA'/                   SUTRA_MAIN...34000
!     6   47X,' SSSS    UUUU     TT    RR  RR  AA  AA'/                   SUTRA_MAIN...34100
!     7   7(/),37X,'U N I T E D    S T A T E S   ',                       SUTRA_MAIN...34200
!     8   'G E O L O G I C A L   S U R V E Y'////                         SUTRA_MAIN...34300
!     9   45X,'SUBSURFACE FLOW AND TRANSPORT SIMULATION MODEL'/           SUTRA_MAIN...34400
!     *   //58X,'-SUTRA VERSION ',A,'-'///                                SUTRA_MAIN...34500
!     A   36X,'*  SATURATED-UNSATURATED FLOW AND SOLUTE OR ENERGY',       SUTRA_MAIN...34600
!     B   ' TRANSPORT  *'////4(////132('*')))                             SUTRA_MAIN...34700
C                                                                        SUTRA_MAIN...34800
C_______________________________________________________________________ SUTRA_MAIN...34900
C|                                                                     | SUTRA_MAIN...35000
C|  *****************************************************************  | SUTRA_MAIN...35100
C|  *                                                               *  | SUTRA_MAIN...35200
C|  *   *********  R E A D I N G   I N P U T   D A T A  *********   *  | SUTRA_MAIN...35300
C|  *   *********  A N D   E R R O R   H A N D L I N G  *********   *  | SUTRA_MAIN...35400
C|  *                                                               *  | SUTRA_MAIN...35500
C|  *   SUTRA typically reads input data line by line as follows.   *  | SUTRA_MAIN...35600
C|  *   Subroutine READIF is called to skip over any comment        *  | SUTRA_MAIN...35700
C|  *   lines and read a single line of input data (up to 1000      *  | SUTRA_MAIN...35800
C|  *   characters) into internal file INTFIL. The input data       *  | SUTRA_MAIN...35900
C|  *   are then read from INTFIL. In case of an error, subroutine  *  | SUTRA_MAIN...36000
C|  *   SUTERR is called to report it, and control passes to the    *  | SUTRA_MAIN...36100
C|  *   termination sequence in subroutine TERSEQ.  The variable    *  | SUTRA_MAIN...36200
C|  *   ERRCOD is used to identify the nature of the error and is   *  | SUTRA_MAIN...36300
C|  *   set prior to calling READIF. The variables CHERR, INERR,    *  | SUTRA_MAIN...36400
C|  *   and RLERR can be used to send character, integer, or real   *  | SUTRA_MAIN...36500
C|  *   error information to subroutine SUTERR.                     *  | SUTRA_MAIN...36600
C|  *   Example from the main program:                              *  | SUTRA_MAIN...36700
C|  *                                                               *  | SUTRA_MAIN...36800
C|  *   ERRCOD = 'REA-INP-3'                                        *  | SUTRA_MAIN...36900
C|  *   CALL READIF(K1, 0, INTFIL, ERRCOD)                          *  | SUTRA_MAIN...37000
C|  *   READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,             *  | SUTRA_MAIN...37100
C|  *  1   NSOP,NSOU,NOBS                                           *  | SUTRA_MAIN...37200
C|  *   IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR) *  | SUTRA_MAIN...37300
C|  *                                                               *  | SUTRA_MAIN...37400
C|  *****************************************************************  | SUTRA_MAIN...37500
C|_____________________________________________________________________| SUTRA_MAIN...37600
C                                                                        SUTRA_MAIN...37700
C.....INPUT DATASET 1:  OUTPUT HEADING                                   SUTRA_MAIN...37800
      ERRCOD = 'REA-INP-1'                                               SUTRA_MAIN...37900
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 SUTRA_MAIN...38000
c rbw begin change
	IF (IERROR.NE.0) GOTO 9000
c end change      
      READ(INTFIL,117,IOSTAT=INERR(1)) TITLE1                            SUTRA_MAIN...38100
c rbw begin change
      IF (INERR(1).NE.0) THEN 
	    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     SUTRA_MAIN...34400
	    GOTO 9000
	ENDIF
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 SUTRA_MAIN...38300
	IF (IERROR.NE.0) GOTO 9000
c end change      
      READ(INTFIL,117,IOSTAT=INERR(1)) TITLE2                            SUTRA_MAIN...38400
c rbw begin change
      IF (INERR(1).NE.0) THEN 
	   CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...34700
	   GOTO 9000
	ENDIF
c end change      
  117 FORMAT(80A1)                                                       SUTRA_MAIN...38600
C                                                                        SUTRA_MAIN...38700
C.....INPUT DATASET 2A:  SIMULATION TYPE (TYPE OF TRANSPORT)             SUTRA_MAIN...38800
C        (SET ME=-1 FOR SOLUTE TRANSPORT, ME=+1 FOR ENERGY TRANSPORT)    SUTRA_MAIN...38900
      IALSAT = 0                                                        
      ERRCOD = 'REA-INP-2A'                                              SUTRA_MAIN...39000
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 SUTRA_MAIN...39100
c rbw begin change
	IF (IERROR.NE.0) GOTO 9000
c end change      
      READ(INTFIL,*,IOSTAT=INERR(1)) SIMSTR                              SUTRA_MAIN...39200
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                SUTRA_MAIN...35500
	   GOTO 9000
	ENDIF
      CALL PRSWDS(SIMSTR, ' ', 5, SIMULA, NWORDS)                        SUTRA_MAIN...39400
      IF(SIMULA(1).NE.'SUTRA     ') THEN                                 SUTRA_MAIN...39500
         ERRCOD = 'INP-2A-1'                                             SUTRA_MAIN...39600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...39700
c rbw begin change
	   GOTO 9000
c end change      
      END IF                                                             SUTRA_MAIN...39800
      IF (SIMULA(2).EQ.'VERSION   ') THEN                                SUTRA_MAIN...39900
         VERNIN = SIMULA(3)                                              SUTRA_MAIN...40000
         IF (VERNIN.EQ.'2D3D.1 ') THEN                                   SUTRA_MAIN...40100
            VERNIN = '2.0'                                               SUTRA_MAIN...40200
         ELSE IF ((VERNIN.NE.'2.0 ').AND.(VERNIN.NE.'2.1 ').AND.         SUTRA_MAIN...40300
     1            (VERNIN.NE.'2.2 ').AND.(VERNIN.NE.'3.0 ')) THEN        AIN...40400
            ERRCOD = 'INP-2A-4'                                          SUTRA_MAIN...40500
            CHERR(1) = VERNIN                                            SUTRA_MAIN...40600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...40700
c rbw begin change
  	      GOTO 9000
c end change      
         END IF                                                          SUTRA_MAIN...40800
         IOFF = 2                                                        SUTRA_MAIN...40900
      ELSE                                                               SUTRA_MAIN...41000
         VERNIN = '2.0'                                                  SUTRA_MAIN...41100
         IOFF = 0                                                        SUTRA_MAIN...41200
      END IF                                                             SUTRA_MAIN...41300
      IF (SIMULA(2+IOFF).EQ.'SOLUTE    ') THEN                          
         IFREEZ = 0
         GOTO 120
      ELSE IF (SIMULA(2+IOFF).EQ.'ENERGY    ') THEN
         IFREEZ = 0
         GOTO 140
      ELSE IF (SIMULA(2+IOFF).EQ.'FREEZING  ') THEN
         IFREEZ = 1
         IALSAT = 1                                                     
         GOTO 140                       
      END IF                                                            
!      IF(SIMULA(2+IOFF).EQ.'SOLUTE    ') GOTO 120                        SUTRA_MAIN...41400
!      IF(SIMULA(2+IOFF).EQ.'ENERGY    ') GOTO 140                        SUTRA_MAIN...41500
      IF (IOFF.EQ.0) THEN                                                SUTRA_MAIN...41600
         ERRCOD = 'INP-2A-2'                                             SUTRA_MAIN...41700
      ELSE                                                               SUTRA_MAIN...41800
         ERRCOD = 'INP-2A-3'                                             SUTRA_MAIN...41900
      END IF                                                             SUTRA_MAIN...42000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           SUTRA_MAIN...42100
c rbw begin change
  	GOTO 9000
c end change      
  120 ME=-1                                                              SUTRA_MAIN...42200
!      WRITE(K3,130)                                                      SUTRA_MAIN...42300
!  130 FORMAT('1'//132('*')///20X,'* * * * *   S U T R A   S O L U ',     SUTRA_MAIN...42400
!     1   'T E   T R A N S P O R T   S I M U L A T I O N   * * * * *'//   SUTRA_MAIN...42500
!     2   /132('*')/)                                                     SUTRA_MAIN...42600
      GOTO 160                                                           SUTRA_MAIN...42700
  140 ME=+1                                                              SUTRA_MAIN...42800
!      WRITE(K3,150)                                                      SUTRA_MAIN...42900
!  150 FORMAT('1'//132('*')///20X,'* * * * *   S U T R A   E N E R ',     SUTRA_MAIN...43000
!     1   'G Y   T R A N S P O R T   S I M U L A T I O N   * * * * *'//   SUTRA_MAIN...43100
!     2   /132('*')/)                                                     SUTRA_MAIN...43200
  160 CONTINUE                                                           SUTRA_MAIN...43300
C                                                                        SUTRA_MAIN...43400
C.....INPUT DATASET 2B:  MESH STRUCTURE                                  SUTRA_MAIN...43500
      ERRCOD = 'REA-INP-2B'                                              SUTRA_MAIN...43600
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 SUTRA_MAIN...43700
c rbw begin change
	IF (IERROR.NE.0) GOTO 9000
c end change      
      READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR                              SUTRA_MAIN...43800
c rbw begin change
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                SUTRA_MAIN...40000
	   GOTO 9000
	ENDIF
c end change      
      CALL PRSWDS(MSHSTR, ' ', 2, MSHTYP, NWORDS)                        SUTRA_MAIN...44000
C.....KTYPE SET ACCORDING TO THE TYPE OF FINITE-ELEMENT MESH:            SUTRA_MAIN...44100
C        2D MESH          ==>   KTYPE(1) = 2                             SUTRA_MAIN...44200
C        3D MESH          ==>   KTYPE(1) = 3                             SUTRA_MAIN...44300
C        IRREGULAR MESH   ==>   KTYPE(2) = 0                             SUTRA_MAIN...44400
C        LAYERED MESH     ==>   KTYPE(2) = 1                             SUTRA_MAIN...44500
C        REGULAR MESH     ==>   KTYPE(2) = 2                             SUTRA_MAIN...44600
C        BLOCKWISE MESH   ==>   KTYPE(2) = 3                             SUTRA_MAIN...44700
      IF (MSHTYP(1).EQ.'2D        ') THEN                                SUTRA_MAIN...44800
         KTYPE(1) = 2                                                    SUTRA_MAIN...44900
      ELSE IF (MSHTYP(1).EQ.'3D        ') THEN                           SUTRA_MAIN...45000
         KTYPE(1) = 3                                                    SUTRA_MAIN...45100
      ELSE                                                               SUTRA_MAIN...45200
         ERRCOD = 'INP-2B-1'                                             SUTRA_MAIN...45300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...45400
c rbw begin change
  	   GOTO 9000
c end change      
      END IF                                                             SUTRA_MAIN...45500
      IF ((MSHTYP(2).EQ.'REGULAR   ').OR.                                SUTRA_MAIN...45600
     1    (MSHTYP(2).EQ.'BLOCKWISE ')) THEN                              SUTRA_MAIN...45700
         ERRCOD = 'REA-INP-2B'                                           SUTRA_MAIN...45800
         IF (KTYPE(1).EQ.2) THEN                                         SUTRA_MAIN...45900
            READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR, NN1, NN2              SUTRA_MAIN...46000
            NN3 = 1                                                      SUTRA_MAIN...46100
         ELSE                                                            SUTRA_MAIN...46200
            READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR, NN1, NN2, NN3         SUTRA_MAIN...46300
         END IF                                                          SUTRA_MAIN...46400
c rbw begin change
         IF (INERR(1).NE.0) THEN
	      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                                                      SUTRA_MAIN...42600
	      GOTO 9000
	   ENDIF
c end change      
         IF ((NN1.LT.2).OR.(NN2.LT.2).OR.                                SUTRA_MAIN...46600
     1      ((KTYPE(1).EQ.3).AND.(NN3.LT.2))) THEN                       SUTRA_MAIN...46700
            ERRCOD = 'INP-2B-3'                                          SUTRA_MAIN...46800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...46900
c rbw begin change
  	      GOTO 9000
c end change      
         END IF                                                          SUTRA_MAIN...47000
         IF (MSHTYP(2).EQ.'BLOCKWISE ') THEN                             SUTRA_MAIN...47100
            KTYPE(2) = 3                                                 SUTRA_MAIN...47200
            ERRCOD = 'REA-INP-2B'                                        SUTRA_MAIN...47300
            DO 177 I1=1,KTYPE(1)                                         SUTRA_MAIN...47400
               CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                        SUTRA_MAIN...47500
c rbw begin change
	         IF (IERROR.NE.0) GOTO 9000
c end change      
               READ(INTFIL,*,IOSTAT=INERR(1)) IDUM1, (IDUM2, I2=1,IDUM1) SUTRA_MAIN...47600
c rbw begin change
               IF (INERR(1).NE.0) THEN
                   CALL SUTERR(ERRCOD,CHERR,INERR,RLERR,IERROR)                   SUTRA_MAIN...43800
	             GOTO 9000
	         ENDIF
c end change      
  177       CONTINUE                                                     SUTRA_MAIN...47800
         ELSE                                                            SUTRA_MAIN...47900
            KTYPE(2) = 2                                                 SUTRA_MAIN...48000
         END IF                                                          SUTRA_MAIN...48100
      ELSE IF (MSHTYP(2).EQ.'LAYERED   ') THEN                           SUTRA_MAIN...48200
         IF (KTYPE(1).EQ.2) THEN                                         SUTRA_MAIN...48300
            ERRCOD = 'INP-2B-5'                                          SUTRA_MAIN...48400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...48500
c rbw begin change
  	      GOTO 9000
c end change      
         END IF                                                          SUTRA_MAIN...48600
         KTYPE(2) = 1                                                    SUTRA_MAIN...48700
         ERRCOD = 'REA-INP-2B'                                           SUTRA_MAIN...48800
         READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR,NLAYS,NNLAY,NELAY,LAYSTR  SUTRA_MAIN...48900
c rbw begin change
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD,CHERR,INERR,RLERR, IERROR)                SUTRA_MAIN...45100
	      GOTO 9000
	   ENDIF
c end change      
         CALL PRSWDS(LAYSTR, ' ', 1, LAYNOR, NWORDS)                     SUTRA_MAIN...49100
         IF ((LAYNOR(1).NE.'ACROSS').AND.(LAYNOR(1).NE.'WITHIN')) THEN   SUTRA_MAIN...49200
            ERRCOD = 'INP-2B-6'                                          SUTRA_MAIN...49300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...49400
c rbw begin change
  	      GOTO 9000
c end change      
         END IF                                                          SUTRA_MAIN...49500
         IF ((NLAYS.LT.2).OR.(NNLAY.LT.4).OR.(NELAY.LT.1)) THEN          SUTRA_MAIN...49600
            ERRCOD = 'INP-2B-7'                                          SUTRA_MAIN...49700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...49800
c rbw begin change
  	      GOTO 9000
c end change      
         END IF                                                          SUTRA_MAIN...49900
      ELSE IF (MSHTYP(2).EQ.'IRREGULAR ') THEN                           SUTRA_MAIN...50000
         KTYPE(2) = 0                                                    SUTRA_MAIN...50100
      ELSE                                                               SUTRA_MAIN...50200
         ERRCOD = 'INP-2B-4'                                             SUTRA_MAIN...50300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...50400
c rbw begin change
  	      GOTO 9000
c end change      
      END IF    
C RBW
	MeshInfo(1) = KTYPE(1) 
	MeshInfo(2) = KTYPE(2) 
	IF (MSHTYP(2).EQ.'LAYERED   ') THEN 
	  MeshInfo(3) = NLAYS 
	  IF (LAYNOR(1).EQ.'WITHIN') THEN
	    MeshInfo(2) = 4 
	  ENDIF
	ELSE
	  MeshInfo(3) = 0
	ENDIF
C RBW
c	                                                                   SUTRA_MAIN...50500
C                                                                        SUTRA_MAIN...50600
C.....OUTPUT DATASET 1                                                   SUTRA_MAIN...50700
!      WRITE(K3,180) TITLE1,TITLE2                                        SUTRA_MAIN...50800
!  180 FORMAT(////1X,131('-')//26X,80A1//26X,80A1//1X,131('-'))           SUTRA_MAIN...50900
C                                                                        SUTRA_MAIN...51000
C.....OUTPUT FILE UNIT ASSIGNMENTS                                       SUTRA_MAIN...51100
!      WRITE(K3,200) IUNIT(1),FNINP,IUNIT(2),FNICS                        SUTRA_MAIN...51200
!  200 FORMAT(/////11X,'F I L E   U N I T   A S S I G N M E N T S'//      SUTRA_MAIN...51300
!     1   13X,'INPUT UNITS:'/                                             SUTRA_MAIN...51400
!     2   13X,' INP FILE (MAIN INPUT)          ',I7,4X,                   SUTRA_MAIN...51500
!     3      'ASSIGNED TO ',A80/                                          SUTRA_MAIN...51600
!     4   13X,' ICS FILE (INITIAL CONDITIONS)  ',I7,4X,                   SUTRA_MAIN...51700
!     5      'ASSIGNED TO ',A80)                                          SUTRA_MAIN...51800
!      IF(IUNIT(9).NE.-1) THEN                                            SUTRA_MAIN...51900
!         DO 202 NFB=1,NFBCS                                              SUTRA_MAIN...52000
!            WRITE(K3,201) IUBCS(NFB),FNBCS(NFB)                          SUTRA_MAIN...52100
!  201       FORMAT(13X,' BCS FILE (TIME-VAR. BND. COND.)',I7,4X,         SUTRA_MAIN...52200
!     1      'ASSIGNED TO ',A80)                                          SUTRA_MAIN...52300
!  202    CONTINUE                                                        SUTRA_MAIN...52400
!      END IF                                                             SUTRA_MAIN...52500
!      WRITE(K3,203) IUNIT(0),FNAME(0),IUNIT(3),FNAME(3)                  SUTRA_MAIN...52600
!  203 FORMAT(/                                                           SUTRA_MAIN...52700
!     6   13X,'OUTPUT UNITS:'/                                            SUTRA_MAIN...52800
!     7   13X,' SMY FILE (RUN SUMMARY)         ',I7,4X,                   SUTRA_MAIN...52900
!     8      'ASSIGNED TO ',A80/                                          SUTRA_MAIN...53000
!     9   13X,' LST FILE (GENERAL OUTPUT)      ',I7,4X,                   SUTRA_MAIN...53100
!     T      'ASSIGNED TO ',A80)                                          SUTRA_MAIN...53200
!      IF(IUNIT(4).NE.-1) WRITE(K3,204) IUNIT(4),FNAME(4)                 SUTRA_MAIN...53300
!  204 FORMAT(13X,' RST FILE (RESTART DATA)        ',I7,4X,               SUTRA_MAIN...53400
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...53500
!      IF(IUNIT(5).NE.-1) WRITE(K3,205) IUNIT(5),FNAME(5)                 SUTRA_MAIN...53600
!  205 FORMAT(13X,' NOD FILE (NODEWISE OUTPUT)     ',I7,4X,               SUTRA_MAIN...53700
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...53800
!      IF(IUNIT(6).NE.-1) WRITE(K3,206) IUNIT(6),FNAME(6)                 SUTRA_MAIN...53900
!  206 FORMAT(13X,' ELE FILE (VELOCITY OUTPUT)     ',I7,4X,               SUTRA_MAIN...54000
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...54100
!      IF(IUNIT(7).NE.-1) WRITE(K3,207) IUNIT(7),                         SUTRA_MAIN...54200
!     1   TRIM(FNAME(7)) // " (BASE FILENAME)"                            SUTRA_MAIN...54300
!  207 FORMAT(13X,' OBS FILE (OBSERVATION OUTPUT) (',I7,')',3X,           SUTRA_MAIN...54400
!     1   'ASSIGNED TO ',A)                                               SUTRA_MAIN...54500
!      IF(IUNIT(8).NE.-1) WRITE(K3,208) IUNIT(8),                         SUTRA_MAIN...54600
!     1   TRIM(FNAME(8)) // " (BASE FILENAME)"                            SUTRA_MAIN...54700
!  208 FORMAT(13X,' OBC FILE (OBSERVATION OUTPUT) (',I7,')',3X,           SUTRA_MAIN...54800
!     1   'ASSIGNED TO ',A)                                               SUTRA_MAIN...54900
!      IF(IUNIT(10).NE.-1) WRITE(K3,209) IUNIT(10),FNAME(10)              SUTRA_MAIN...55000
!  209 FORMAT(13X,' BCOF FILE (BND. COND. OUTPUT)  ',I7,4X,               SUTRA_MAIN...55100
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...55200
!      IF(IUNIT(11).NE.-1) WRITE(K3,210) IUNIT(11),FNAME(11)              SUTRA_MAIN...55300
!  210 FORMAT(13X,' BCOS FILE (BND. COND. OUTPUT)  ',I7,4X,               SUTRA_MAIN...55400
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...55500
!      IF(IUNIT(12).NE.-1) WRITE(K3,211) IUNIT(12),FNAME(12)              SUTRA_MAIN...55600
!  211 FORMAT(13X,' BCOP FILE (BND. COND. OUTPUT)  ',I7,4X,               SUTRA_MAIN...55700
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...55800
!      IF(IUNIT(13).NE.-1) WRITE(K3,212) IUNIT(13),FNAME(13)              SUTRA_MAIN...55900
!  212 FORMAT(13X,' BCOU FILE (BND. COND. OUTPUT)  ',I7,4X,               SUTRA_MAIN...56000
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...56100
!      IF ((IUNIT(7).NE.-1).OR.(IUNIT(8).NE.-1)) WRITE(K3,213)            SUTRA_MAIN...56200
!  213 FORMAT(/14X,'NAMES FOR OBS AND OBC FILES WILL BE GENERATED',       SUTRA_MAIN...56300
!     1   ' AUTOMATICALLY FROM THE BASE NAMES LISTED ABOVE AND SCHEDULE', SUTRA_MAIN...56400
!     2   ' NAMES'/14X,'LISTED LATER IN THIS FILE.  UNIT NUMBERS',        SUTRA_MAIN...56500
!     3   ' ASSIGNED TO THESE FILES WILL BE THE FIRST AVAILABLE',         SUTRA_MAIN...56600
!     4   ' NUMBERS GREATER THAN'/14X,'OR EQUAL TO THE VALUES LISTED',    SUTRA_MAIN...56700
!     5   ' ABOVE IN PARENTHESES.')                                       SUTRA_MAIN...56800
C                                                                        SUTRA_MAIN...56900
C.....INPUT DATASET 3:  SIMULATION CONTROL NUMBERS                       SUTRA_MAIN...57000
      ERRCOD = 'REA-INP-3'                                               SUTRA_MAIN...57100
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 SUTRA_MAIN...57200
c rbw begin change
	IF (IERROR.NE.0) GOTO 9000
      IF ((VERNIN.EQ."2.0").OR.(VERNIN.EQ."2.1").OR.(VERNIN.EQ."2.2"))  
     1   THEN
         READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS
      ELSE
         READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,NSOP,NSOU,
     1      NPBG,NUBG,NOBS
      END IF                                                            
!      READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS      SUTRA_MAIN...57300
c rbw end change
! comment out the previous line and uncomment the following 2 lines for SUTRA-ICE
!      READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,NSOP,NSOU,
!	1   NPBG,NUBG,NOBS
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                SUTRA_MAIN...51500
c rbw begin change
	   GOTO 9000
c rbw end change
	ENDIF
      IF (KTYPE(2).GT.1) THEN                                            SUTRA_MAIN...57500
         NN123 = NN1*NN2*NN3                                             SUTRA_MAIN...57600
         IF(NN123.NE.NN) THEN                                            SUTRA_MAIN...57700
           ERRCOD = 'INP-2B,3-1'                                         SUTRA_MAIN...57800
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      SUTRA_MAIN...57900
c rbw begin change
	     GOTO 9000
c rbw end change
         END IF                                                          SUTRA_MAIN...58000
         IF (KTYPE(1).EQ.3) THEN                                         SUTRA_MAIN...58100
            NE123 = (NN1 - 1)*(NN2 - 1)*(NN3 - 1)                        SUTRA_MAIN...58200
         ELSE                                                            SUTRA_MAIN...58300
            NE123 = (NN1 - 1)*(NN2 - 1)                                  SUTRA_MAIN...58400
         END IF                                                          SUTRA_MAIN...58500
         IF(NE123.NE.NE) THEN                                            SUTRA_MAIN...58600
           ERRCOD = 'INP-2B,3-2'                                         SUTRA_MAIN...58700
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      SUTRA_MAIN...58800
c rbw begin change
	     GOTO 9000
c rbw end change
         END IF                                                          SUTRA_MAIN...58900
      ELSE IF (MSHTYP(2).EQ.'LAYERED   ') THEN                           SUTRA_MAIN...59000
         NNTOT = NLAYS*NNLAY                                             SUTRA_MAIN...59100
         IF(NNTOT.NE.NN) THEN                                            SUTRA_MAIN...59200
           ERRCOD = 'INP-2B,3-3'                                         SUTRA_MAIN...59300
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      SUTRA_MAIN...59400
c rbw begin change
	     GOTO 9000
c rbw end change
         END IF                                                          SUTRA_MAIN...59500
         NETOT = (NLAYS - 1)*NELAY                                       SUTRA_MAIN...59600
         IF(NETOT.NE.NE) THEN                                            SUTRA_MAIN...59700
           ERRCOD = 'INP-2B,3-4'                                         SUTRA_MAIN...59800
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      SUTRA_MAIN...59900
c rbw begin change
	     GOTO 9000
c rbw end change
         END IF                                                          SUTRA_MAIN...60000
      ENDIF                                                              SUTRA_MAIN...60100
C                                                                        SUTRA_MAIN...60200
C RBW begin change                                                                       SUTRA_MAIN...54300
      IBOUSZ = NPBC + NUBC + NSOP + NSOU + 4
	NFEAT = 4
C RBW end change
C                                                                        SUTRA_MAIN...54300
C.....INPUT AND OUTPUT DATASET 4:  SIMULATION MODE OPTIONS               SUTRA_MAIN...60300
      ERRCOD = 'REA-INP-4'                                               SUTRA_MAIN...60400
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 SUTRA_MAIN...60500
c rbw begin change
	IF (IERROR.NE.0) GOTO 9000
c rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) UNSSTR,SSFSTR,SSTSTR,RDSTR,ISTORE   SUTRA_MAIN...60600
c rbw begin change
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                SUTRA_MAIN...54800
	   GOTO 9000
	ENDIF
c rbw end change
      CALL PRSWDS(UNSSTR, ' ', 1, CUNSAT, NWORDS)                        SUTRA_MAIN...60800
      CALL PRSWDS(SSFSTR, ' ', 1, CSSFLO, NWORDS)                        SUTRA_MAIN...60900
      CALL PRSWDS(SSTSTR, ' ', 1, CSSTRA, NWORDS)                        SUTRA_MAIN...61000
      CALL PRSWDS(RDSTR,  ' ', 1, CREAD,  NWORDS)                        SUTRA_MAIN...61100
      ISMERR = 0                                                         SUTRA_MAIN...61200
      IF (CUNSAT.EQ.'UNSATURATED') THEN                                  SUTRA_MAIN...61300
         IUNSAT = +1                                                     SUTRA_MAIN...61400
         IALSAT = +1                                                    
      ELSE IF (CUNSAT.EQ.'SATURATED') THEN                               SUTRA_MAIN...61500
         IUNSAT = 0                                                      SUTRA_MAIN...61600
      ELSE                                                               SUTRA_MAIN...61700
         ERRCOD = 'INP-4-1'                                              SUTRA_MAIN...61800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...61900
c rbw begin change
	   GOTO 9000
c rbw end change
      END IF                                                             SUTRA_MAIN...62000
      IF (CSSFLO.EQ.'TRANSIENT') THEN                                    SUTRA_MAIN...62100
         ISSFLO = 0                                                      SUTRA_MAIN...62200
      ELSE IF (CSSFLO.EQ.'STEADY') THEN                                  SUTRA_MAIN...62300
         ISSFLO = +1                                                     SUTRA_MAIN...62400
      ELSE                                                               SUTRA_MAIN...62500
         ERRCOD = 'INP-4-2'                                              SUTRA_MAIN...62600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...62700
c rbw begin change
	   GOTO 9000
c rbw end change
      END IF                                                             SUTRA_MAIN...62800
      IF (CSSTRA.EQ.'TRANSIENT') THEN                                    SUTRA_MAIN...62900
         ISSTRA = 0                                                      SUTRA_MAIN...63000
      ELSE IF (CSSTRA.EQ.'STEADY') THEN                                  SUTRA_MAIN...63100
         ISSTRA = +1                                                     SUTRA_MAIN...63200
      ELSE                                                               SUTRA_MAIN...63300
         ERRCOD = 'INP-4-3'                                              SUTRA_MAIN...63400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...63500
c rbw begin change
	   GOTO 9000
c rbw end change
      END IF                                                             SUTRA_MAIN...63600
      IF (CREAD.EQ.'COLD') THEN                                          SUTRA_MAIN...63700
         IREAD = +1                                                      SUTRA_MAIN...63800
      ELSE IF (CREAD.EQ.'WARM') THEN                                     SUTRA_MAIN...63900
         IREAD = -1                                                      SUTRA_MAIN...64000
      ELSE                                                               SUTRA_MAIN...64100
         ERRCOD = 'INP-4-4'                                              SUTRA_MAIN...64200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...64300
c rbw begin change
	   GOTO 9000
c rbw end change
      END IF                                                             SUTRA_MAIN...64400
!      WRITE(K3,214)                                                      SUTRA_MAIN...64500
!  214 FORMAT(////11X,'S I M U L A T I O N   M O D E   ',                 SUTRA_MAIN...64600
!     1   'O P T I O N S'/)                                               SUTRA_MAIN...64700
      IF(ISSTRA.EQ.1.AND.ISSFLO.NE.1) THEN                               SUTRA_MAIN...64800
         ERRCOD = 'INP-4-5'                                              SUTRA_MAIN...64900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...65000
c rbw begin change
	   GOTO 9000
c rbw end change
      ENDIF                                                              SUTRA_MAIN...65100
C RBW begin change
	ISTEADYFLOW = ISSFLO
	ISTEADYTRANSPORT = ISSTRA
      IF (ISTART.LE.0) then
	  GOTO 9000
	endif
C RBW end change
!      IF(IUNSAT.EQ.+1) WRITE(K3,215)                                     SUTRA_MAIN...65200
!      IF(IUNSAT.EQ.0) WRITE(K3,216)                                      SUTRA_MAIN...65300
!  215 FORMAT(11X,'- ALLOW UNSATURATED AND SATURATED FLOW:  UNSATURATED', SUTRA_MAIN...65400
!     1   ' PROPERTIES ARE USER-PROGRAMMED IN SUBROUTINE   U N S A T')    SUTRA_MAIN...65500
!  216 FORMAT(11X,'- ASSUME SATURATED FLOW ONLY')                         SUTRA_MAIN...65600
!      IF(ISSFLO.EQ.+1.AND.ME.EQ.-1) WRITE(K3,219)                        SUTRA_MAIN...65700
!      IF(ISSFLO.EQ.+1.AND.ME.EQ.+1) WRITE(K3,220)                        SUTRA_MAIN...65800
!      IF(ISSFLO.EQ.0) WRITE(K3,221)                                      SUTRA_MAIN...65900
!  219 FORMAT(11X,'- ASSUME STEADY-STATE FLOW FIELD CONSISTENT WITH ',    SUTRA_MAIN...66000
!     1   'INITIAL CONCENTRATION CONDITIONS')                             SUTRA_MAIN...66100
!  220 FORMAT(11X,'- ASSUME STEADY-STATE FLOW FIELD CONSISTENT WITH ',    SUTRA_MAIN...66200
!     1   'INITIAL TEMPERATURE CONDITIONS')                               SUTRA_MAIN...66300
!  221 FORMAT(11X,'- ALLOW TIME-DEPENDENT FLOW FIELD')                    SUTRA_MAIN...66400
!      IF(ISSTRA.EQ.+1) WRITE(K3,225)                                     SUTRA_MAIN...66500
!      IF(ISSTRA.EQ.0) WRITE(K3,226)                                      SUTRA_MAIN...66600
!  225 FORMAT(11X,'- ASSUME STEADY-STATE TRANSPORT')                      SUTRA_MAIN...66700
!  226 FORMAT(11X,'- ALLOW TIME-DEPENDENT TRANSPORT')                     SUTRA_MAIN...66800
!      IF(IREAD.EQ.-1) WRITE(K3,230)                                      SUTRA_MAIN...66900
!      IF(IREAD.EQ.+1) WRITE(K3,231)                                      SUTRA_MAIN...67000
!  230 FORMAT(11X,'- WARM START - SIMULATION IS TO BE ',                  SUTRA_MAIN...67100
!     1   'CONTINUED FROM PREVIOUSLY-STORED DATA')                        SUTRA_MAIN...67200
!  231 FORMAT(11X,'- COLD START - BEGIN NEW SIMULATION')                  SUTRA_MAIN...67300
!      IF(ISTORE.GT.0) WRITE(K3,240) ISTORE                               SUTRA_MAIN...67400
!      IF(ISTORE.EQ.0) WRITE(K3,241)                                      SUTRA_MAIN...67500
!  240 FORMAT(11X,'- STORE RESULTS AFTER EVERY',I9,' TIME STEPS IN',      SUTRA_MAIN...67600
!     1   ' RESTART FILE AS BACKUP AND FOR USE IN A SIMULATION RESTART')  SUTRA_MAIN...67700
!  241 FORMAT(11X,'- DO NOT STORE RESULTS FOR USE IN A',                  SUTRA_MAIN...67800
!     1   ' RESTART OF SIMULATION')                                       SUTRA_MAIN...67900
C.....OUTPUT DATASET 3                                                   SUTRA_MAIN...68000
!      IF(ME.EQ.-1)                                                       SUTRA_MAIN...68100
!     1   WRITE(K3,245) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS                    SUTRA_MAIN...68200
!  245 FORMAT(////11X,'S I M U L A T I O N   C O N T R O L   ',           SUTRA_MAIN...68300
!     1   'N U M B E R S'// 8X,I9,5X,'NUMBER OF NODES IN FINITE-',        SUTRA_MAIN...68400
!     2   'ELEMENT MESH'/ 8X,I9,5X,'NUMBER OF ELEMENTS IN MESH'//         SUTRA_MAIN...68500
!     3    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...68600
!     4   'PRESSURE IS A SPECIFIED CONSTANT OR FUNCTION OF TIME'/         SUTRA_MAIN...68700
!     5    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...68800
!     6   'SOLUTE CONCENTRATION IS A SPECIFIED CONSTANT OR ',             SUTRA_MAIN...68900
!     7   'FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES AT',       SUTRA_MAIN...69000
!     8   ' WHICH FLUID INFLOW OR OUTFLOW IS A SPECIFIED CONSTANT',       SUTRA_MAIN...69100
!     9   ' OR FUNCTION OF TIME'/ 8X,I9,5X,'EXACT NUMBER OF NODES AT',    SUTRA_MAIN...69200
!     A   ' WHICH A SOURCE OR SINK OF SOLUTE MASS IS A SPECIFIED ',       SUTRA_MAIN...69300
!     B   'CONSTANT OR FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF ',   SUTRA_MAIN...69400
!     C   'NODES AT WHICH PRESSURE AND CONCENTRATION WILL BE OBSERVED')   SUTRA_MAIN...69500
C                                                                        SUTRA_MAIN...69600
!      IF(ME.EQ.+1)                                                       SUTRA_MAIN...69700
!     1    WRITE(K3,247) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS                   SUTRA_MAIN...69800
!  247 FORMAT(////11X,'S I M U L A T I O N   C O N T R O L   ',           SUTRA_MAIN...69900
!     1   'N U M B E R S'// 8X,I9,5X,'NUMBER OF NODES IN FINITE-',        SUTRA_MAIN...70000
!     2   'ELEMENT MESH'/ 8X,I9,5X,'NUMBER OF ELEMENTS IN MESH'//         SUTRA_MAIN...70100
!     3    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...70200
!     4   'PRESSURE IS A SPECIFIED CONSTANT OR FUNCTION OF TIME'/         SUTRA_MAIN...70300
!     5    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...70400
!     6   'TEMPERATURE IS A SPECIFIED CONSTANT OR ',                      SUTRA_MAIN...70500
!     7   'FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES AT',       SUTRA_MAIN...70600
!     8   ' WHICH FLUID INFLOW OR OUTFLOW IS A SPECIFIED CONSTANT',       SUTRA_MAIN...70700
!     9   ' OR FUNCTION OF TIME'/ 8X,I9,5X,'EXACT NUMBER OF NODES AT',    SUTRA_MAIN...70800
!     A   ' WHICH A SOURCE OR SINK OF ENERGY IS A SPECIFIED CONSTANT',    SUTRA_MAIN...70900
!     B   ' OR FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES ',     SUTRA_MAIN...71000
!     C   'AT WHICH PRESSURE AND TEMPERATURE WILL BE OBSERVED')           SUTRA_MAIN...71100
C                                                                        SUTRA_MAIN...71200
C.....INPUT DATASETS 5 - 7 (NUMERICAL, TEMPORAL, AND ITERATION CONTROLS) SUTRA_MAIN...71300
      CALL INDAT0(IERROR)                                                      SUTRA_MAIN...71400
c rbw begin change
	IF (IERROR.NE.0) GOTO 9000
c rbw end change
C.....KSOLVP AND KSOLVU HAVE BEEN SET ACCORDING TO THE SOLVERS SELECTED: SUTRA_MAIN...71500
C        BANDED GAUSSIAN ELIMINATION (DIRECT)   ==>   0                  SUTRA_MAIN...71600
C        IC-PRECONDITIONED CG                   ==>   1                  SUTRA_MAIN...71700
C        ILU-PRECONDITIONED GMRES               ==>   2                  SUTRA_MAIN...71800
C        ILU-PRECONDITIONED ORTHOMIN            ==>   3                  SUTRA_MAIN...71900
C                                                                        SUTRA_MAIN...72000
C.....OUTPUT DATASETS 7B & 7C                                            SUTRA_MAIN...72100
!      WRITE(K3,261)                                                      SUTRA_MAIN...72200
!  261 FORMAT(////11X,'S O L V E R - R E L A T E D   ',                   SUTRA_MAIN...72300
!     1   'P A R A M E T E R S')                                          SUTRA_MAIN...72400
C.....OUTPUT DATASETS 3B & 3C                                            SUTRA_MAIN...72500
!  266 IF (KSOLVP.NE.0) THEN                                              SUTRA_MAIN...72600
!         WRITE(K3,268)                                                   SUTRA_MAIN...72700
!     1      SOLNAM(KSOLVP), ITRMXP, TOLP,                                SUTRA_MAIN...72800
!     2      SOLNAM(KSOLVU), ITRMXU, TOLU                                 SUTRA_MAIN...72900
!  268    FORMAT(                                                         SUTRA_MAIN...73000
!     1      /13X,'SOLVER FOR P: ',A40                                    SUTRA_MAIN...73100
!     2      //20X,I6,5X,'MAXIMUM NUMBER OF MATRIX SOLVER ITERATIONS',    SUTRA_MAIN...73200
!     3           ' DURING P SOLUTION'                                    SUTRA_MAIN...73300
!     4      /11X,1PE15.4,5X,'CONVERGENCE TOLERANCE FOR MATRIX',          SUTRA_MAIN...73400
!     5           ' SOLVER ITERATIONS DURING P SOLUTION'                  SUTRA_MAIN...73500
!     6      //13X,'SOLVER FOR U: ',A40                                   SUTRA_MAIN...73600
!     7      //20X,I6,5X,'MAXIMUM NUMBER OF MATRIX SOLVER ITERATIONS',    SUTRA_MAIN...73700
!     8           ' DURING U SOLUTION'                                    SUTRA_MAIN...73800
!     9      /11X,1PE15.4,5X,'CONVERGENCE TOLERANCE FOR MATRIX',          SUTRA_MAIN...73900
!     A           ' SOLVER ITERATIONS DURING U SOLUTION' )                SUTRA_MAIN...74000
!      ELSE                                                               SUTRA_MAIN...74100
!         WRITE(K3,269) SOLNAM(KSOLVP)                                    SUTRA_MAIN...74200
!  269    FORMAT(/13X,'SOLVER FOR P AND U: ',A40)                         SUTRA_MAIN...74300
!      END IF                                                             SUTRA_MAIN...74400
C                                                                        SUTRA_MAIN...74500
C.....CALCULATE ARRAY DIMENSIONS, EXCEPT THOSE THAT DEPEND ON            SUTRA_MAIN...74600
C        BANDWIDTH OR NELT                                               SUTRA_MAIN...74700
C                                                                        SUTRA_MAIN...74800
      IF (KSOLVP.EQ.0) THEN                                              SUTRA_MAIN...74900
C........SET DIMENSIONS FOR DIRECT SOLVER                                SUTRA_MAIN...75000
         NNNX = 1                                                        SUTRA_MAIN...75100
         NDIMJA = 1                                                      SUTRA_MAIN...75200
         NNVEC = NN                                                      SUTRA_MAIN...75300
      ELSE                                                               SUTRA_MAIN...75400
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)                          SUTRA_MAIN...75500
         NNNX = NN                                                       SUTRA_MAIN...75600
         NDIMJA = NN + 1                                                 SUTRA_MAIN...75700
         NNVEC = NN                                                      SUTRA_MAIN...75800
      END IF                                                             SUTRA_MAIN...75900
      NBCN=NPBC+NUBC+1                                                   SUTRA_MAIN...76000
      NSOP=NSOP+1                                                        SUTRA_MAIN...76100
      NSOU=NSOU+1                                                        SUTRA_MAIN...76200
      NOBSN=NOBS+1                                                       SUTRA_MAIN...76300
      IF (KTYPE(1).EQ.3) THEN                                            SUTRA_MAIN...76400
         N48 = 8                                                         SUTRA_MAIN...76500
         NEX = NE                                                        SUTRA_MAIN...76600
      ELSE                                                               SUTRA_MAIN...76700
         N48 = 4                                                         SUTRA_MAIN...76800
         NEX = 1                                                         SUTRA_MAIN...76900
      END IF                                                             SUTRA_MAIN...77000
      NIN=NE*N48                                                         SUTRA_MAIN...77100
C                                                                        SUTRA_MAIN...77200
C.....ALLOCATE REAL ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH        SUTRA_MAIN...77300
      ALLOCATE(PITER(NN),UITER(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN),  SUTRA_MAIN...77400
     1   PVEL(NN),SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),       SUTRA_MAIN...77500
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),       SUTRA_MAIN...77600
     3   QIN(NN),UIN(NN),QUIN(NN),QINITR(NN),RCIT(NN),RCITM1(NN))        SUTRA_MAIN...77700
      ALLOCATE(PVEC(NNVEC),UVEC(NNVEC))                                  SUTRA_MAIN...77800
      ALLOCATE(ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),         SUTRA_MAIN...77900
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),          SUTRA_MAIN...78000
     2   PANGL1(NE))                                                     SUTRA_MAIN...78100
      ALLOCATE(ALMID(NEX),ATMID(NEX),                                    SUTRA_MAIN...78200
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),                 SUTRA_MAIN...78300
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX))                SUTRA_MAIN...78400
      ALLOCATE(PBC(NBCN),UBC(NBCN),QPLITR(NBCN),GNUP1(NBCN),GNUU1(NBCN)) SUTRA_MAIN...78500
      ALLOCATE(GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48))                  SUTRA_MAIN...78600
      ALLOCATE(B(NNNX))                                                  SUTRA_MAIN...78700
C.....ALLOCATE INTEGER ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH     SUTRA_MAIN...78800
C        OR NELT                                                         SUTRA_MAIN...78900
      ALLOCATE(IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),    SUTRA_MAIN...79000
     1   NREG(NN),LREG(NE),JA(NDIMJA))                                   SUTRA_MAIN...79100
      ALLOCATE(IIDPBC(NBCN),IIDUBC(NBCN),IIDSOP(NSOP),IIDSOU(NSOU))      SUTRA_MAIN...79200
C.....ALLOCATE INTEGER(1) ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH  SUTRA_MAIN...79300
C        OR NELT                                                         SUTRA_MAIN...79400
      ALLOCATE(IBCPBC(NBCN),IBCUBC(NBCN),IBCSOP(NSOP),IBCSOU(NSOU))      SUTRA_MAIN...79500
C.....ALLOCATE ARRAYS OF DERIVED TYPE, EXCEPT THOSE THAT DEPEND ON       SUTRA_MAIN...79600
C        BANDWIDTH OR NELT                                               SUTRA_MAIN...79700
      ALLOCATE(BCSFL(0:ITMAX),BCSTR(0:ITMAX))                            SUTRA_MAIN...79800
C.....ALLOCATE ARRAYS OF DERIVED TYPE, EXCEPT THOSE THAT DEPEND ON       SUTRA_MAIN...79900
C        BANDWIDTH OR NELT                                               SUTRA_MAIN...80000
      ALLOCATE(OBSPTS(NOBSN))                                            SUTRA_MAIN...80100
      ALLO1 = .TRUE.                                                     SUTRA_MAIN...80200
C                                                                        SUTRA_MAIN...80300
C.....INPUT DATASETS 8 - 15 (OUTPUT CONTROLS; FLUID AND SOLID MATRIX     SUTRA_MAIN...80400
C        PROPERTIES; ADSORPTION PARAMETERS; PRODUCTION OF ENERGY OR      SUTRA_MAIN...80500
C        SOLUTE MASS; GRAVITY; AND NODEWISE AND ELEMENTWISE DATA)        SUTRA_MAIN...80600
      CALL INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,         SUTRA_MAIN...80700
     1   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,                      SUTRA_MAIN...80800
     2   PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG,        SUTRA_MAIN...80900
     3   OBSPTS,
     4   ElementValues, IElementValueCount, NodeValues, 
     5   INodeValueCount, IERROR)                                                         SUTRA_MAIN...81000
c rbw begin change
	IF (IERROR.NE.0) GOTO 9000
c rbw end change
C                                                                        SUTRA_MAIN...81100
C.....KEEP TRACK IF OUTPUT ROUTINES HAVE BEEN EXECUTED, TO PRINT         SUTRA_MAIN...81200
C        HEADERS ONLY ONCE.                                              SUTRA_MAIN...81300
      ONCEK5 = .FALSE.                                                   SUTRA_MAIN...81400
      ONCEK6 = .FALSE.                                                   SUTRA_MAIN...81500
      ONCEK7 = .FALSE.                                                   SUTRA_MAIN...81600
      ONCEK8 = .FALSE.                                                   SUTRA_MAIN...81700
      ALLOCATE(ONCK78(NFLOMX))                                           SUTRA_MAIN...81800
      DO 400 J=1,NFLOMX                                                  SUTRA_MAIN...81900
         ONCK78(J) = .FALSE.                                             SUTRA_MAIN...82000
  400 CONTINUE                                                           SUTRA_MAIN...82100
      ONCEK10 = .FALSE.                                                  SUTRA_MAIN...82200
      ONCEK11 = .FALSE.                                                  SUTRA_MAIN...82300
      ONCEK12 = .FALSE.                                                  SUTRA_MAIN...82400
      ONCEK13 = .FALSE.                                                  SUTRA_MAIN...82500
C                                                                        SUTRA_MAIN...82600
C.....INPUT DATASETS 17 & 18 (SOURCES OF FLUID MASS AND ENERGY OR        SUTRA_MAIN...82700
C        SOLUTE MASS)                                                    SUTRA_MAIN...82800
      CALL ZERO(QIN,NN,0.0D0)                                            SUTRA_MAIN...82900
      CALL ZERO(UIN,NN,0.0D0)                                            SUTRA_MAIN...83000
      CALL ZERO(QUIN,NN,0.0D0)                                           SUTRA_MAIN...83100
c rbw begin change
!      IF(NSOP-1.GT.0.OR.NSOU-1.GT.0)                                     SUTRA_MAIN...83200
!     1   CALL SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT,             SUTRA_MAIN...83300
!     2      IBCSOP,IBCSOU, IERROR, IBOUSZ, IBNODE, IPOS)                                               SUTRA_MAIN...83400
      IF(NSOP-1.GT.0.OR.NSOU-1.GT.0) then                                    SUTRA_MAIN...83200
         CALL SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT,             SUTRA_MAIN...83300
     2      IBCSOP,IBCSOU, IERROR, IBOUSZ, IBNODE, IPOS)                                               SUTRA_MAIN...83400
	ELSE
	  IBNODE(IPOS+1) = 0
	  IBNODE(IPOS+2) = 0
	  IPOS = IPOS + 2
	ENDIF
	IF (IERROR.NE.0) GOTO 9000
c rbw end change
C                                                                        SUTRA_MAIN...83500
C.....INPUT DATASETS 19 & 20 (SPECIFIED P AND U BOUNDARY CONDITIONS)     SUTRA_MAIN...83600
      IF(NBCN-1.GT.0) then
         CALL BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT,                       SUTRA_MAIN...83700
     1   IBCPBC,IBCUBC,GNUP1,GNUU1,                                       SUTRA_MAIN...83800
     2     IERROR, IBOUSZ, IBNODE, IPOS)                                 SUTRA_MAIN...76600
C rbw begin change
	  IF (IERROR.NE.0) GOTO 9000
      ELSE
	  IBNODE(IPOS+1) = 0
	  IBNODE(IPOS+2) = 0
      ENDIF                                                             B650....
c rbw end change



C                                                                           ! boung ...
C.....INPUT DATASETS 21A & 21B (GENERALIZED FLOW AND TRANSPORT              ! cupbgo
C        BOUNDARY CONDITIONS)                                               ! cupbgo
      IF((VERNIN.NE."2.0").AND.(VERNIN.NE."2.1").AND.(VERNIN.NE."2.2")
     1   .AND.(NPBG+NUBG.GT.0)) THEN
        CALL BOUNG(IERROR,IPBG,PBG1,QPBG1,PBG2,QPBG2,
!     *  CPQL1,CPQL2,UPBGI,           ! cupbgo 7,8,9
!     3   CUPBGO,UPBGO,IUBG,UBG1,     ! 10, 11, 12, 13
     *   QUBG1,                      ! 14
!     *   UBG2,                       ! 15  
     *   QUBG2,                      ! 16
!     *   IPBGT,IUBGT,IBCPBG,        ! cupbgo 17, 18, 19
!     4   IBCUBG,                     ! 20
!     *   QPBGIC,GNUPG,QUBGIC,        ! 21, 22, 23
     *   GNUUG)                                    ! ... boung, cupbgo 24
	  IF (IERROR.NE.0) GOTO 9000
	ENDIF 



C                                                                        SUTRA_MAIN...83900
C.....INPUT DATASET 22 (ELEMENT INCIDENCE [MESH CONNECTION] DATA)        SUTRA_MAIN...84000
      CALL CONNEC(IN,IERROR)                                                    SUTRA_MAIN...84100
C rbw begin change
	IF (IERROR.NE.0) GOTO 9000
C RBW
	do IncidenceIndex = 1, NIN
	  Incidence(IncidenceIndex) = IN(IncidenceIndex) -1
	enddo
      goto 9000
C RBW
c rbw end change
C                                                                        SUTRA_MAIN...84200
C.....IF USING OLD (VERSION 2D3D.1) OBSERVATION INPUT FORMAT, LOOK UP    SUTRA_MAIN...84300
C        COORDINATES FOR OBSERVATION POINTS (NODES).                     SUTRA_MAIN...84400
      IF (NOBCYC.NE.-1) THEN                                             SUTRA_MAIN...84500
         DO 710 K=1,NOBS                                                 SUTRA_MAIN...84600
            I = OBSPTS(K)%L                                              SUTRA_MAIN...84700
            OBSPTS(K)%X = X(I)                                           SUTRA_MAIN...84800
            OBSPTS(K)%Y = Y(I)                                           SUTRA_MAIN...84900
            IF (N48.EQ.8) OBSPTS(K)%Z = Z(I)                             SUTRA_MAIN...85000
  710    CONTINUE                                                        SUTRA_MAIN...85100
      END IF                                                             SUTRA_MAIN...85200
C                                                                        SUTRA_MAIN...85300
C.....FIND THE ELEMENT EACH OBSERVATION POINT IS IN.  IN COMPONENTS OF   SUTRA_MAIN...85400
C        OBSPTS, OVERWRITE NODE NUMBERS AND GLOBAL COORDINATES WITH      SUTRA_MAIN...85500
C        ELEMENT NUMBERS AND LOCAL COORDINATES.                          SUTRA_MAIN...85600
      DO 900 K=1,NOBS                                                    SUTRA_MAIN...85700
         XK = OBSPTS(K)%X                                                SUTRA_MAIN...85800
         YK = OBSPTS(K)%Y                                                SUTRA_MAIN...85900
         IF (N48.EQ.8) ZK = OBSPTS(K)%Z                                  SUTRA_MAIN...86000
         DO 800 LL=1,NE                                                  SUTRA_MAIN...86100
            IF (N48.EQ.8) THEN                                           SUTRA_MAIN...86200
               CALL FINDL3(X,Y,Z,IN,LL,XK,YK,ZK,XSI,ETA,ZET,INOUT)       SUTRA_MAIN...86300
            ELSE                                                         SUTRA_MAIN...86400
               CALL FINDL2(X,Y,IN,LL,XK,YK,XSI,ETA,INOUT)                SUTRA_MAIN...86500
            END IF                                                       SUTRA_MAIN...86600
            IF (INOUT.EQ.1) THEN                                         SUTRA_MAIN...86700
               L = LL                                                    SUTRA_MAIN...86800
               GOTO 820                                                  SUTRA_MAIN...86900
            END IF                                                       SUTRA_MAIN...87000
  800    CONTINUE                                                        SUTRA_MAIN...87100
         ERRCOD = 'INP-8D-3'                                             SUTRA_MAIN...87200
         CHERR(1) = OBSPTS(K)%NAME                                       SUTRA_MAIN...87300
!         WRITE(UNIT=CHERR(2), FMT=805)                                   SUTRA_MAIN...87400
!     1      OBSPTS(K)%X, OBSPTS(K)%Y, OBSPTS(K)%Z                        SUTRA_MAIN...87500
!  805    FORMAT('(',2(1PE14.7,','),1PE14.7,')')                          SUTRA_MAIN...87600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...87700
C rbw begin change
	   GOTO 9000
c rbw end change
  820    OBSPTS(K)%L = L                                                 SUTRA_MAIN...87800
         OBSPTS(K)%XSI = XSI                                             SUTRA_MAIN...87900
         OBSPTS(K)%ETA = ETA                                             SUTRA_MAIN...88000
         IF (N48.EQ.8) OBSPTS(K)%ZET = ZET                               SUTRA_MAIN...88100
  900 CONTINUE                                                           SUTRA_MAIN...88200
C                                                                        SUTRA_MAIN...88300
C.....IF ITERATIVE SOLVER IS USED, SET UP POINTER ARRAYS IA AND JA THAT  SUTRA_MAIN...88400
C        SPECIFY MATRIX STRUCTURE IN "SLAP COLUMN" FORMAT.  DIMENSION    SUTRA_MAIN...88500
C        NELT GETS SET HERE.                                             SUTRA_MAIN...88600
      IF (KSOLVP.NE.0) THEN                                              SUTRA_MAIN...88700
         CALL PTRSET()                                                   SUTRA_MAIN...88800
      ELSE                                                               SUTRA_MAIN...88900
         NELT = NN                                                       SUTRA_MAIN...89000
         NDIMIA = 1                                                      SUTRA_MAIN...89100
         ALLOCATE(IA(NDIMIA))                                            SUTRA_MAIN...89200
      END IF                                                             SUTRA_MAIN...89300
      ALLO3 = .TRUE.                                                     SUTRA_MAIN...89400
C                                                                        SUTRA_MAIN...89500
C.....CALCULATE BANDWIDTH                                                SUTRA_MAIN...89600
      CALL BANWID(IN)                                                    SUTRA_MAIN...89700
C                                                                        SUTRA_MAIN...89800
C.....CALCULATE ARRAY DIMENSIONS THAT DEPEND ON BANDWIDTH OR NELT        SUTRA_MAIN...89900
      IF (KSOLVP.EQ.0) THEN                                              SUTRA_MAIN...90000
C........SET DIMENSIONS FOR DIRECT SOLVER                                SUTRA_MAIN...90100
         NCBI = NBI                                                      SUTRA_MAIN...90200
         NELTA = NELT                                                    SUTRA_MAIN...90300
         NWI = 1                                                         SUTRA_MAIN...90400
         NWF = 1                                                         SUTRA_MAIN...90500
      ELSE                                                               SUTRA_MAIN...90600
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)                          SUTRA_MAIN...90700
         NCBI = 1                                                        SUTRA_MAIN...90800
         NELTA = NELT                                                    SUTRA_MAIN...90900
         KSOLVR = KSOLVP                                                 SUTRA_MAIN...91000
         NSAVE = NSAVEP                                                  SUTRA_MAIN...91100
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIP, NWFP)               SUTRA_MAIN...91200
         KSOLVR = KSOLVU                                                 SUTRA_MAIN...91300
         NSAVE = NSAVEU                                                  SUTRA_MAIN...91400
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIU, NWFU)               SUTRA_MAIN...91500
         NWI = MAX(NWIP, NWIU)                                           SUTRA_MAIN...91600
         NWF = MAX(NWFP, NWFU)                                           SUTRA_MAIN...91700
      END IF                                                             SUTRA_MAIN...91800
      MATDIM=NELT*NCBI                                                   SUTRA_MAIN...91900
C                                                                        SUTRA_MAIN...92000
C.....ALLOCATE REAL AND INTEGER ARRAYS THAT DEPEND ON BANDWIDTH OR NELT  SUTRA_MAIN...92100
!      ALLOCATE(PMAT(NELT,NCBI),UMAT(NELT,NCBI),FWK(NWF))                 SUTRA_MAIN...92200
!      ALLOCATE(IWK(NWI))                                                 SUTRA_MAIN...92300
      ALLO2 = .TRUE.                                                     SUTRA_MAIN...92400
C                                                                        SUTRA_MAIN...92500
C.....READ BCS SCHEDULES AND CHECK BCS BOUNDARY CONDITIONS FOR           SUTRA_MAIN...92600
C        INPUT ERRORS.  DETERMINE SIZE OF BCS IDENTIFIER ARRAY.          SUTRA_MAIN...92700
      NCIDB = 1                                                          SUTRA_MAIN...92800
      ALLOCATE(CIDBCS(NCIDB))                                            SUTRA_MAIN...92900
      IF (K9.NE.-1) THEN                                                 SUTRA_MAIN...93000
C........SET UP ARRAY OF SCHEDULE NUMBERS, BCP                           SUTRA_MAIN...93100
         ALLOCATE (BFP(NFBCS))                                           SUTRA_MAIN...93200
         DO 2100 NFB=1,NFBCS                                             SUTRA_MAIN...93300
            K9 = IUNIB(NFB)                                              SUTRA_MAIN...93400
C...........SET FNAME(9) EQUAL TO FNAMB(NFB) FOR CONVENIENCE IN          SUTRA_MAIN...93500
C              ERROR HANDLING                                            SUTRA_MAIN...93600
            FNAME(9) = FNAMB(NFB)                                        SUTRA_MAIN...93700
C...........READ SCHEDULE NAME FOR CURRENT BCS FILE.                     SUTRA_MAIN...93800
            ERRCOD = 'REA-BCS-1'                                         SUTRA_MAIN...93900
            CHERR(1) = 'n/a'                                             SUTRA_MAIN...94000
            CHERR(2) = 'n/a'                                             SUTRA_MAIN...94100
            CALL READIF_22(IERROR,K9, NFB, INTFIL, ERRCOD)                  SUTRA_MAIN...94200
C rbw begin change
            IF (IERROR.NE.0) GOTO 9000
C rbw end change
            READ(INTFIL,*,IOSTAT=INERR(1)) CDUM80                        SUTRA_MAIN...94300
            IF (LEN_TRIM(CDUM80).GT.10) THEN                             SUTRA_MAIN...94400
               ERRCOD = 'BCS-1-4'                                        SUTRA_MAIN...94500
               CHERR(1) = CDUM80                                         SUTRA_MAIN...94600
               CHERR(2) = FNAME(9)                                       SUTRA_MAIN...94700
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  SUTRA_MAIN...94800
C rbw begin change
               GOTO 9000
C rbw end change
            END IF                                                       SUTRA_MAIN...94900
            READ(INTFIL,*,IOSTAT=INERR(1)) BCSSCH                        SUTRA_MAIN...95000
            IF (INERR(1).NE.0) THEN                                          SUTRA_MAIN...95100
                 CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               SUTRA_MAIN...95200
c rbw begin change
                 GOTO 9000
c rbw end change
	      ENDIF
C...........FIND SCHEDULE NUMBER IN LIST.  IF NOT FOUND, ERROR.          SUTRA_MAIN...95300
            DO 2050 NS=1,NSCH                                            SUTRA_MAIN...95400
               IF (BCSSCH.EQ.SCHDLS(NS)%NAME) THEN                       SUTRA_MAIN...95500
                  BFP(NFB)%ISCHED = NS                                   SUTRA_MAIN...95600
                  DENB => SCHDLS(BFP(NFB)%ISCHED)%SLIST                  SUTRA_MAIN...95700
                  LENSCH = SCHDLS(BFP(NFB)%ISCHED)%LLEN                  SUTRA_MAIN...95800
                  DO 2000 LC=1,LENSCH                                    SUTRA_MAIN...95900
                     DITBCS = DENB%DVALU2                                SUTRA_MAIN...96000
                     ITBCS = INT(DITBCS)                                 SUTRA_MAIN...96100
                     IF (DBLE(ITBCS).NE.DITBCS) THEN                     SUTRA_MAIN...96200
                        ERRCOD = 'BCS-1-2'                               SUTRA_MAIN...96300
                        CHERR(1) = BCSSCH                                SUTRA_MAIN...96400
                        CHERR(2) = FNAME(9)                              SUTRA_MAIN...96500
                        RLERR(1) = DITBCS                                SUTRA_MAIN...96600
                        CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)         SUTRA_MAIN...96700
                        GOTO 9000
                     END IF                                              SUTRA_MAIN...96800
                     IF (LC.LT.LENSCH) DENB => DENB%NENT                 SUTRA_MAIN...96900
 2000             CONTINUE                                               SUTRA_MAIN...97000
                  GOTO 2100                                              SUTRA_MAIN...97100
               END IF                                                    SUTRA_MAIN...97200
 2050       CONTINUE                                                     SUTRA_MAIN...97300
            IF (ISSTRA.NE.1) THEN                                        SUTRA_MAIN...97400
               ERRCOD = 'BCS-1-1'                                        SUTRA_MAIN...97500
            ELSE                                                         SUTRA_MAIN...97600
               ERRCOD = 'BCS-1-3'                                        SUTRA_MAIN...97700
            END IF                                                       SUTRA_MAIN...97800
            CHERR(1) = BCSSCH                                            SUTRA_MAIN...97900
            CHERR(2) = FNAME(9)                                          SUTRA_MAIN...98000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...98100
c rbw begin change
            GOTO 9000
c rbw end change
 2100    CONTINUE                                                        SUTRA_MAIN...98200
C........READ THROUGH FILES TO SEARCH FOR INPUT ERRORS, BUT DO NOT       SUTRA_MAIN...98300
C           ACTUALLY SET THE BOUNDARY CONDITIONS (SETBCS = .FALSE.)      SUTRA_MAIN...98400
         SETBCS = .FALSE.                                                SUTRA_MAIN...98500
         DENB => SCHDLS(ISCHTS)%SLIST                                    SUTRA_MAIN...98600
         LENSCH = SCHDLS(ISCHTS)%LLEN                                    SUTRA_MAIN...98700
         DO 2200 LC=1,LENSCH                                             SUTRA_MAIN...98800
            DITBCS = DENB%DVALU2                                         SUTRA_MAIN...98900
            ITBCS = INT(DITBCS)                                          SUTRA_MAIN...99000
            CALL BCSTEP(SETBCS,IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,     SUTRA_MAIN...99100
     1         IQSOU,IPBCT1,IUBCT1,IQSOPT1,IQSOUT1,GNUP1,GNUU1,          SUTRA_MAIN...99200
     2         IBCPBC,IBCUBC,IBCSOP,IBCSOU,IIDPBC,IIDUBC,IIDSOP,         SUTRA_MAIN...99300
     3         IIDSOU,NCID,BCSFL,BCSTR, IERROR)                                  SUTRA_MAIN...99400
C rbw begin change
               IF (IERROR.NE.0) GOTO 9000
C rbw end change
            NCIDB = MAX(NCIDB, NCID)                                     SUTRA_MAIN...99500
            IF (LC.LT.LENSCH) DENB => DENB%NENT                          SUTRA_MAIN...99600
 2200    CONTINUE                                                        SUTRA_MAIN...99700
C........TO CLOSE ALL INSERTED FILES, KEEP READING UNTIL END OF ZERO-    SUTRA_MAIN...99800
C           LEVEL FILE IS ENCOUNTERED.  THEN REWIND ZERO-LEVEL FILE      SUTRA_MAIN...99900
C           AND RE-READ THE SCHEDULE NAME.                               SUTRA_MAIN..100000
         DO 2300 NFB=1,NFBCS                                             SUTRA_MAIN..100100
            K9 = IUNIB(NFB)                                              SUTRA_MAIN..100200
            ERRCOD = 'NO_EOF_ERR'                                        SUTRA_MAIN..100300
            CHERR(1) = 'n/a'                                             SUTRA_MAIN..100400
            CHERR(2) = 'n/a'                                             SUTRA_MAIN..100500
            DO WHILE (ERRCOD.NE.'EOF')                                   SUTRA_MAIN..100600
               CALL READIF_22(IERROR,K9, NFB, INTFIL, ERRCOD)               SUTRA_MAIN..100700
c rbw begin change
               IF (IERROR.NE.0) GOTO 9000                                          SUTRA_MAIN...95100
c rbw end change
            END DO                                                       SUTRA_MAIN..100800
            REWIND (K9)                                                  SUTRA_MAIN..100900
            CALL READIF_22(IERROR,K9, NFB, INTFIL, ERRCOD)                  SUTRA_MAIN..101000
c rbw begin change
            IF (IERROR.NE.0) GOTO 9000                                          SUTRA_MAIN...95100
c rbw end change
            READ(INTFIL,*,IOSTAT=INERR(1)) BCSSCH                        SUTRA_MAIN..101100
c rbw begin change
            IF (INERR(1).NE.0) THEN                                          SUTRA_MAIN...95100
                 CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               SUTRA_MAIN...95200
                 GOTO 9000
	      ENDIF
c rbw end change
 2300    CONTINUE                                                        SUTRA_MAIN..101200
      ELSE                                                               SUTRA_MAIN..101300
C........IF NO BCS FILES, ZERO OUT BCS ID ARRAYS FOR NEATNESS.           SUTRA_MAIN..101400
         IIDSOP = 0                                                      SUTRA_MAIN..101500
         IIDSOU = 0                                                      SUTRA_MAIN..101600
         IIDPBC = 0                                                      SUTRA_MAIN..101700
         IIDUBC = 0                                                      SUTRA_MAIN..101800
      END IF                                                             SUTRA_MAIN..101900
C.....ALLOCATION OF BCS IDENTIFIER ARRAY IS DONE IN SUBROUTINE INDAT2.   SUTRA_MAIN..102000
C                                                                        SUTRA_MAIN..102100
C.....INPUT INITIAL OR RESTART CONDITIONS FROM THE ICS FILE AND          SUTRA_MAIN..102200
C        INITIALIZE PARAMETERS                                           SUTRA_MAIN..102300
!      CALL INDAT2(PVEC,UVEC,PM1,UM1,UM2,CS1,CS2,CS3,SL,SR,RCIT,SW,DSWDP, SUTRA_MAIN..102400
!     1   PBC,IPBC,IPBCT,NREG,QIN,DPDTITR,GNUP1,GNUU1,UIN,UBC,QUIN,       SUTRA_MAIN..102500
!     2   IBCPBC,IBCUBC,IBCSOP,IBCSOU,IIDPBC,IIDUBC,IIDSOP,IIDSOU,IERROR)        SUTRA_MAIN..102600
C                                                                        SUTRA_MAIN..102700
C.....COMPUTE AND OUTPUT DIMENSIONS OF SIMULATION                        SUTRA_MAIN..102800
      IF (K9.NE.-1) THEN                                                 SUTRA_MAIN..102900
         NRBCS = 3*NN + 4*NBCN                                           SUTRA_MAIN..103000
         NI1BCS = 2*NBCN + NSOP + NSOU                                   SUTRA_MAIN..103100
      ELSE                                                               SUTRA_MAIN..103200
         NRBCS = 7                                                       SUTRA_MAIN..103300
         NI1BCS = 4                                                      SUTRA_MAIN..103400
      END IF                                                             SUTRA_MAIN..103500
      RMVDIM = 27*NN + 11*NE + 10*NEX + 3*NBCN + N48*(2*NE + NEX)        SUTRA_MAIN..103600
     1   + NNNX + 2*NELT*NCBI + NWF + 6*NOBSN + 3*NSCH + NRBCS           SUTRA_MAIN..103700
      IMVDIM = NIN + 2*NSOP + 2*NSOU + 4*NBCN + NN + NE                  SUTRA_MAIN..103800
     1   + NDIMJA + NDIMIA + NWI + NOBSN + 3*NSCH                        SUTRA_MAIN..103900
      I1VDIM = NI1BCS                                                    SUTRA_MAIN..104000
      CMVDIM = 73*NOBS + 89*NSCH + NCIDB                                 SUTRA_MAIN..104100
      PMVDIM = 2*NSCH                                                    SUTRA_MAIN..104200
      LMVDIM = ITMAX                                                     SUTRA_MAIN..104300
      TOTMB = (DBLE(RMVDIM)*8D0 + DBLE(IMVDIM)*4D0 + DBLE(I1VDIM)        SUTRA_MAIN..104400
     1   + DBLE(CMVDIM) + DBLE(LMVDIM)*4D0)/1D6                          SUTRA_MAIN..104500
!      WRITE(K3,3000) RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM,TOTMB     SUTRA_MAIN..104600
! 3000 FORMAT(////11X,'S I M U L A T I O N   D I M E N S I O N S'//       SUTRA_MAIN..104700
!     1   13X,'REAL        ARRAYS WERE ALLOCATED ',I12/                   SUTRA_MAIN..104800
!     2   13X,'INTEGER     ARRAYS WERE ALLOCATED ',I12/                   SUTRA_MAIN..104900
!     3   13X,'INTEGER(1)  ARRAYS WERE ALLOCATED ',I12/                   SUTRA_MAIN..105000
!     4   13X,'CHARACTER   ARRAYS WERE ALLOCATED ',I12,                   SUTRA_MAIN..105100
!     5       ' (SUM OF ARRAY_DIMENSION*CHARACTER_LENGTH)'/               SUTRA_MAIN..105200
!     6   13X,'LOGICAL     ARRAYS WERE ALLOCATED ',I12/                   SUTRA_MAIN..105300
!     7   13X,'ARRAYS OF POINTERS WERE ALLOCATED ',I12//                  SUTRA_MAIN..105400
!     8   13X,F10.3,' Mbytes MEMORY USED FOR MAIN ARRAYS'/                SUTRA_MAIN..105500
!     9   13X,'- assuming 1 byte/character'/                              SUTRA_MAIN..105600
!     1   13X,'- assuming 4-byte logical variables'/                      SUTRA_MAIN..105700
!     2   13X,'- pointer storage not included')                           SUTRA_MAIN..105800
C                                                                        SUTRA_MAIN..105900
!      WRITE(K3,4000)                                                     SUTRA_MAIN..106000
! 4000 FORMAT(////////8(132("-")/))                                       SUTRA_MAIN..106100
C                                                                        SUTRA_MAIN..106200
C.....CALL MAIN CONTROL ROUTINE, SUTRA                                   SUTRA_MAIN..106300
!      CALL SUTRA(TITLE1,TITLE2,PMAT,UMAT,PITER,UITER,PM1,DPDTITR,        SUTRA_MAIN..106400
      CALL SUTRA(TITLE1,TITLE2,PITER,UITER,PM1,DPDTITR,                  SUTRA_MAIN..106400
     1   UM1,UM2,PVEL,SL,SR,X,Y,Z,VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,  SUTRA_MAIN..106500
     2   QIN,UIN,QUIN,QINITR,RCIT,RCITM1,PVEC,UVEC,                      SUTRA_MAIN..106600
     3   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,VMAG,VANG1,VANG2,           SUTRA_MAIN..106700
     4   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA_MAIN..106800
     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,B,           SUTRA_MAIN..106900
     6   GNUP1,GNUU1,IN,IQSOP,IQSOU,IPBC,IUBC,OBSPTS,NREG,LREG,          SUTRA_MAIN..107000
     7   IA,JA,IBCPBC,IBCUBC,IBCSOP,IBCSOU,IIDPBC,IIDUBC,IIDSOP,IIDSOU,  SUTRA_MAIN..107100
     8   IQSOPT,IQSOUT,IPBCT,IUBCT,BCSFL,BCSTR,IERROR)                          SUTRA_MAIN..107200
!     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,FWK,B,       SUTRA_MAIN..106900
!     6   GNUP1,GNUU1,IN,IQSOP,IQSOU,IPBC,IUBC,OBSPTS,NREG,LREG,IWK,      SUTRA_MAIN..107000
c rbw begin change
	IF (IERROR.NE.0) GOTO 9000
c rbw end change.
C                                                                        SUTRA_MAIN..107300
C.....TERMINATION SEQUENCE: DEALLOCATE ARRAYS, CLOSE FILES, AND END      SUTRA_MAIN..107400
9000  CONTINUE                                                           SUTRA_MAIN..107500
      CALL TERSEQ()                                                      SUTRA_MAIN..107600
c RBW
      RETURN
C RBW
      END                                                                SUTRA_MAIN..107700
C                                                                        SUTRA_MAIN..107800
C     SUBROUTINE        A  D  S  O  R  B           SUTRA VERSION 2.2     ADSORB.........100
C                                                                        ADSORB.........200
C *** PURPOSE :                                                          ADSORB.........300
C ***  TO CALCULATE VALUES OF EQUILIBRIUM SORPTION PARAMETERS FOR        ADSORB.........400
C ***  LINEAR, FREUNDLICH, AND LANGMUIR MODELS.                          ADSORB.........500
C                                                                        ADSORB........7500
C     SUBROUTINE        B  A  N  W  I  D           SUTRA VERSION 2.2     BANWID.........100
C                                                                        BANWID.........200
C *** PURPOSE :                                                          BANWID.........300
C ***  TO CALCULATE THE BANDWIDTH OF THE FINITE ELEMENT MESH.            BANWID.........400
C                                                                        BANWID.........500
      SUBROUTINE BANWID(IN)                                              BANWID.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BANWID.........700
      CHARACTER*80 UNAME,FNAME(0:13)                                     BANWID.........800
      DIMENSION IN(NIN)                                                  BANWID.........900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BANWID........1000
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            BANWID........1100
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        BANWID........1200
      COMMON /FNAMES/ UNAME,FNAME                                        BANWID........1300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 BANWID........1400
     1   K10,K11,K12,K13                                                 BANWID........1500
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       BANWID........1600
C                                                                        BANWID........1700
      NDIF=0                                                             BANWID........1800
      II=0                                                               BANWID........1900
!      WRITE(K3,100)                                                      BANWID........2000
!  100 FORMAT(////11X,'**** MESH ANALYSIS ****'//)                        BANWID........2100
C                                                                        BANWID........2200
C.....FIND ELEMENT WITH MAXIMUM DIFFERENCE IN NODE NUMBERS               BANWID........2300
      DO 2000 L=1,NE                                                     BANWID........2400
      II=II+1                                                            BANWID........2500
      IELO=IN(II)                                                        BANWID........2600
      IEHI=IN(II)                                                        BANWID........2700
      DO 1000 I=2,N48                                                    BANWID........2800
      II=II+1                                                            BANWID........2900
      IF(IN(II).LT.IELO) IELO=IN(II)                                     BANWID........3000
 1000 IF(IN(II).GT.IEHI) IEHI=IN(II)                                     BANWID........3100
      NDIFF=IEHI-IELO                                                    BANWID........3200
      IF(NDIFF.GT.NDIF) THEN                                             BANWID........3300
       NDIF=NDIFF                                                        BANWID........3400
       LEM=L                                                             BANWID........3500
      ENDIF                                                              BANWID........3600
 2000 CONTINUE                                                           BANWID........3700
C                                                                        BANWID........3800
C.....CALCULATE FULL BANDWIDTH, NB.                                      BANWID........3900
      NB=2*NDIF+1                                                        BANWID........4000
      NBHALF=NDIF+1                                                      BANWID........4100
C.....NBI IS USED TO DIMENSION ARRAYS WHOSE SIZE DEPENDS ON THE          BANWID........4200
C        BANDWIDTH.  IT IS THE SAME AS THE ACTUAL BANDWIDTH, NB.         BANWID........4300
      NBI = NB                                                           BANWID........4400
!      WRITE(K3,2500) NB,LEM                                              BANWID........4500
! 2500 FORMAT(//13X,'MAXIMUM FULL BANDWIDTH, ',I9,                        BANWID........4600
!     1   ', WAS CALCULATED IN ELEMENT ',I9)                              BANWID........4700
C                                                                        BANWID........4800
      RETURN                                                             BANWID........4900
      END                                                                BANWID........5000
C                                                                        BANWID........5100
C     SUBROUTINE        B  A  S  I  S  2           SUTRA VERSION 2.2     BASIS2.........100
C                                                                        BASIS2.........200
C *** PURPOSE :                                                          BASIS2.........300
C ***  TO CALCULATE VALUES OF BASIS AND WEIGHTING FUNCTIONS AND THEIR    BASIS2.........400
C ***  DERIVATIVES, TRANSFORMATION MATRICES BETWEEN LOCAL AND GLOBAL     BASIS2.........500
C ***  COORDINATES AND PARAMETER VALUES AT A SPECIFIED POINT IN A        BASIS2.........600
C ***  QUADRILATERAL FINITE ELEMENT.  THIS SUBROUTINE HANDLES 2D         BASIS2.........700
C ***  CALCULATIONS ONLY; 3D CALCULATIONS ARE PERFORMED IN SUBROUTINE    BASIS2.........800
C ***  BASIS3.                                                           BASIS2.........900
C                                                                        BASIS2........1000
C                                                                        BASIS2.......23400
C     SUBROUTINE        B  A  S  I  S  3           SUTRA VERSION 2.2     BASIS3.........100
C                                                                        BASIS3.........200
C *** PURPOSE :                                                          BASIS3.........300
C ***  TO CALCULATE VALUES OF BASIS AND WEIGHTING FUNCTIONS AND THEIR    BASIS3.........400
C ***  DERIVATIVES, TRANSFORMATION MATRICES BETWEEN LOCAL AND GLOBAL     BASIS3.........500
C ***  COORDINATES AND PARAMETER VALUES AT A SPECIFIED POINT IN A        BASIS3.........600
C ***  QUADRILATERAL FINITE ELEMENT.  THIS SUBROUTINE HANDLES 3D         BASIS3.........700
C ***  CALCULATIONS ONLY; 2D CALCULATIONS ARE PERFORMED IN SUBROUTINE    BASIS3.........800
C ***  BASIS2.                                                           BASIS3.........900
C                                                                        BASIS3........1000
C                                                                        BASIS3.......30100
C     SUBROUTINE        B  C                       SUTRA VERSION 2.2     BC.............100
C                                                                        BC.............200
C *** PURPOSE :                                                          BC.............300
C ***  TO IMPLEMENT SPECIFIED PRESSURE AND SPECIFIED TEMPERATURE OR      BC.............400
C ***  CONCENTRATION CONDITIONS BY MODIFYING THE GLOBAL FLOW AND         BC.............500
C ***  TRANSPORT MATRIX EQUATIONS.                                       BC.............600
C                                                                        BC.............700
C     SUBROUTINE        B  C  S  T  E  P           SUTRA VERSION 2.2     BCSTEP.........100
C                                                                        BCSTEP.........200
C *** PURPOSE :                                                          BCSTEP.........300
C ***  TO READ TIME-DEPENDENT BOUNDARY CONDITIONS FROM THE BCS FILES     BCSTEP.........400
c ***  AND UPDATE THE ARRAYS IN WHICH THEY ARE STORED.                   BCSTEP.........500
C                                                                        BCSTEP.........600
      SUBROUTINE BCSTEP(SETBCS,IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,     BCSTEP.........700
     1   IQSOU,IPBCT1,IUBCT1,IQSOPT1,IQSOUT1,GNUP1,GNUU1,                BCSTEP.........800
     2   IBCPBC,IBCUBC,IBCSOP,IBCSOU,IIDPBC,IIDUBC,IIDSOP,IIDSOU,        BCSTEP.........900
     3   NCID,BCSFL,BCSTR, IERROR)                                               BCSTEP........1000
      USE ALLARR, ONLY : CIDBCS                                          BCSTEP........1100
      USE BCSDEF                                                         BCSTEP........1200
      USE EXPINT                                                         BCSTEP........1300
      USE LLDEF                                                          BCSTEP........1400
      USE SCHDEF                                                         BCSTEP........1500
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BCSTEP........1600
      CHARACTER INTFIL*1000                                              BCSTEP........1700
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:13)                    BCSTEP........1800
      CHARACTER*40 BCSID                                                 BCSTEP........1900
      LOGICAL ONCEBCS,SETBCS,SETFL,SETTR,BCSFL(0:ITMAX),BCSTR(0:ITMAX)   BCSTEP........2000
      LOGICAL USEFL,ANYFL,ANYTR                                          BCSTEP........2100
      INTEGER(1) IBCPBC(NBCN),IBCUBC(NBCN),IBCSOP(NSOP),IBCSOU(NSOU)     BCSTEP........2200
      INTEGER IIDPBC(NBCN),IIDUBC(NBCN),IIDSOP(NSOP),IIDSOU(NSOU)        BCSTEP........2300
      DIMENSION INERR(10),RLERR(10)                                      BCSTEP........2400
      DIMENSION IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN),               BCSTEP........2500
     1   GNUP1(NBCN),GNUU1(NBCN),                                        BCSTEP........2600
     2   QIN(NN),UIN(NN),QUIN(NN),IQSOP(NSOP),IQSOU(NSOU)                BCSTEP........2700
      DIMENSION KTYPE(2)                                                 BCSTEP........2800
      ALLOCATABLE :: IPBC1(:),PBC1(:),IUBC1(:),UBC1(:),                  BCSTEP........2900
     1   QIN1(:),UIN1(:),QUIN1(:),IQSOP1(:),IQSOU1(:)                    BCSTEP........3000
      COMMON /BCSL/ ONCEBCS                                              BCSTEP........3100
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BCSTEP........3200
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,           BCSTEP........3300
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE                                       BCSTEP........3400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BCSTEP........3500
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            BCSTEP........3600
      COMMON /FUNIB/ NFBCS                                               BCSTEP........3700
      COMMON /FNAMES/ UNAME,FNAME                                        BCSTEP........3800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 BCSTEP........3900
     1   K10,K11,K12,K13                                                 BCSTEP........4000
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    BCSTEP........4100
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       BCSTEP........4200
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      BCSTEP........4300
C                                                                        BCSTEP........4400
C.....IF THIS IS THE FIRST IN A SERIES OF CALLS, INITIALIZE BCS          BCSTEP........4500
C        SCHEDULES                                                       BCSTEP........4600
      IF (.NOT.ONCEBCS) THEN                                             BCSTEP........4700
         IF (.NOT.ALLOCATED(LCNT)) ALLOCATE (LCNT(NFBCS),DENBCS(NFBCS))  BCSTEP........4800
         DO 20 NFB=1,NFBCS                                               BCSTEP........4900
            DENBCS(NFB)%NENT => SCHDLS(BFP(NFB)%ISCHED)%SLIST            BCSTEP........5000
            LCNT(NFB) = 1                                                BCSTEP........5100
   20    CONTINUE                                                        BCSTEP........5200
         ONCEBCS = .TRUE.                                                BCSTEP........5300
      END IF                                                             BCSTEP........5400
C                                                                        BCSTEP........5500
C.....INITIALIZE FLAGS THAT INDICATE WHETHER BOUNDARY CONDITIONS ARE     BCSTEP........5600
C        ACTUALLY SET ON THIS TIME STEP.  THESE FLAGS ARE USED IN        BCSTEP........5700
C        DETERMINING SOLUTION CYCLING.  INITIALIZE COUNTER FOR BCS       BCSTEP........5800
C        IDENTIFIERS.                                                    BCSTEP........5900
      IF (ITBCS.NE.0) THEN                                               BCSTEP........6000
         BCSFL(ITBCS) = .FALSE.                                          BCSTEP........6100
         BCSTR(ITBCS) = .FALSE.                                          BCSTEP........6200
      END IF                                                             BCSTEP........6300
      IF (.NOT.((ISSTRA.NE.0).AND.(ITBCS.EQ.1))) NCID = 0                BCSTEP........6400
C                                                                        BCSTEP........6500
C.....LOOP OVER ALL BCS FILES                                            BCSTEP........6600
      DO 1000 NFB=1,NFBCS                                                BCSTEP........6700
         K9 = IUNIB(NFB)                                                 BCSTEP........6800
C........SET FNAME(9) EQUAL TO FNAMB(NFB) FOR CONVENIENCE IN             BCSTEP........6900
C           ERROR HANDLING                                               BCSTEP........7000
         FNAME(9) = FNAMB(NFB)                                           BCSTEP........7100
         LENSCH = SCHDLS(BFP(NFB)%ISCHED)%LLEN                           BCSTEP........7200
C                                                                        BCSTEP........7300
C...,,FIND BOUNDARY CONDITIONS FOR THE CURRENT TIME STEP, IF ANY.        BCSTEP........7400
C        (IF THIS BCS SCHEDULE IS EXHAUSTED, SKIP TO NEXT FILE.)         BCSTEP........7500
  100 IF (LCNT(NFB).GT.LENSCH) GOTO 1000                                 BCSTEP........7600
      ITNBCS = INT(DENBCS(NFB)%NENT%DVALU2)                              BCSTEP........7700
      IF (ITBCS.LT.ITNBCS) THEN                                          BCSTEP........7800
C........THE CURRENT TIME STEP PRECEDES THIS BCS SCHEDULE ENTRY.         BCSTEP........7900
C          SKIP TO NEXT FILE.                                            BCSTEP........8000
         GOTO 1000                                                       BCSTEP........8100
      ELSE                                                               BCSTEP........8200
         WRITE(CHERR(1),*) ITNBCS                                        BCSTEP........8300
         ERRCOD = 'REA-BCS-2'                                            BCSTEP........8400
         CHERR(2) = 'unknown'                                            BCSTEP........8500
         CALL READIF_22(IERROR,K9, NFB, INTFIL, ERRCOD)                     BCSTEP........8600
c rbw begin change
         if (IERROR.NE.0) return
c rbw end change
         READ(INTFIL,*,IOSTAT=INERR(1)) BCSID,NSOP1,NSOU1,NPBC1,NUBC1    BCSTEP........8700
         IF (INERR(1).NE.0) then
	     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      
	     return
	   ENDIF
         IF ((ITNBCS.EQ.0).AND.(NSOU1+NUBC1.GT.0)) THEN                  BCSTEP........8900
            ERRCOD = 'BCS-2-1'                                           BCSTEP........9000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     BCSTEP........9100
	      return
         END IF                                                          BCSTEP........9200
         IF (ITBCS.GT.ITNBCS) THEN                                       BCSTEP........9300
C...........THE CURRENT TIME STEP IS PAST THIS BCS SCHEDULE ENTRY,       BCSTEP........9400
C             READ PAST THIS SPECIFICATION AND ADVANCE TO THE NEXT       BCSTEP........9500
C             SCHEDULE ENTRY.                                            BCSTEP........9600
            CHERR(2) = BCSID                                             BCSTEP........9700
            ERRCOD = 'REA-BCS-3'                                         BCSTEP........9800
            IF (NSOP1.GT.0) THEN                                         BCSTEP........9900
               DO 120 N=1,NSOP1+1                                        BCSTEP.......10000
                  CALL READIF_22(IERROR,K9, NFB, INTFIL, ERRCOD)            BCSTEP.......10100
                  if (IERROR.NE.0) return
  120          CONTINUE                                                  BCSTEP.......10200
            END IF                                                       BCSTEP.......10300
            ERRCOD = 'REA-BCS-4'                                         BCSTEP.......10400
            IF (NSOU1.GT.0) THEN                                         BCSTEP.......10500
               DO 122 N=1,NSOU1+1                                        BCSTEP.......10600
                  CALL READIF_22(IERROR,K9, NFB, INTFIL, ERRCOD)            BCSTEP.......10700
                  if (IERROR.NE.0) return
  122          CONTINUE                                                  BCSTEP.......10800
            END IF                                                       BCSTEP.......10900
            ERRCOD = 'REA-BCS-5'                                         BCSTEP.......11000
            IF (NPBC1.GT.0) THEN                                         BCSTEP.......11100
               DO 124 N=1,NPBC1+1                                        BCSTEP.......11200
                  CALL READIF_22(IERROR,K9, NFB, INTFIL, ERRCOD)            BCSTEP.......11300
                  if (IERROR.NE.0) return
  124          CONTINUE                                                  BCSTEP.......11400
            END IF                                                       BCSTEP.......11500
            ERRCOD = 'REA-BCS-6'                                         BCSTEP.......11600
            IF (NUBC1.GT.0) THEN                                         BCSTEP.......11700
               DO 126 N=1,NUBC1+1                                        BCSTEP.......11800
                  CALL READIF_22(IERROR,K9, NFB, INTFIL, ERRCOD)            BCSTEP.......11900
                  if (IERROR.NE.0) return
126          CONTINUE                                                    BCSTEP.......12000
            END IF                                                       BCSTEP.......12100
            LCNT(NFB) = LCNT(NFB) + 1                                    BCSTEP.......12200
            IF (LCNT(NFB).LE.LENSCH)                                     BCSTEP.......12300
     1         DENBCS(NFB)%NENT => DENBCS(NFB)%NENT%NENT                 BCSTEP.......12400
            GOTO 100                                                     BCSTEP.......12500
         END IF                                                          BCSTEP.......12600
      END IF                                                             BCSTEP.......12700
C                                                                        BCSTEP.......12800
C.....SET NBCN1, AND INCREMENT NSOP1 AND NSOU1 TO ACCOMMODATE FINAL      BCSTEP.......12900
C        ZERO WHEN READING.  COUNTS OF TIME-DEPENDENT NODES REFER TO     BCSTEP.......13000
C        THE BCS FILE CURRENTLY BEING READ.                              BCSTEP.......13100
      NBCN1 = NPBC1 + NUBC1 + 1                                          BCSTEP.......13200
      NSOP1 = NSOP1 + 1                                                  BCSTEP.......13300
      NSOU1 = NSOU1 + 1                                                  BCSTEP.......13400
C.....NSOPI IS ACTUAL NUMBER OF POSSIBLE FLUID SOURCE NODES              BCSTEP.......13500
      NSOPI = NSOP - 1                                                   BCSTEP.......13600
C.....NSOUI IS ACTUAL NUMBER OF POSSIBLE ENERGY OR SOLUTE MASS           BCSTEP.......13700
C        SOURCE NODES                                                    BCSTEP.......13800
      NSOUI = NSOU - 1                                                   BCSTEP.......13900
C.....NSOPI1 IS ACTUAL NUMBER OF TIME-STEP-DEPENDENT FLUID SOURCE NODES  BCSTEP.......14000
C        ON THIS TIME STEP                                               BCSTEP.......14100
      NSOPI1 = NSOP1 - 1                                                 BCSTEP.......14200
C.....NSOUI1 IS ACTUAL NUMBER OF TIME-STEP-DEPENDENT ENERGY OR SOLUTE    BCSTEP.......14300
C        MASS SOURCE NODES ON THIS TIME STEP                             BCSTEP.......14400
      NSOUI1 = NSOU1 - 1                                                 BCSTEP.......14500
C                                                                        BCSTEP.......14600
C.....SET FLAGS THAT DETERMINE WHETHER TO SET FLOW AND/OR TRANSPORT      BCSTEP.......14700
C        BOUNDARY CONDITIONS (IF ANY) AND THAT INDICATE WHETHER BOUNDARY BCSTEP.......14800
C        CONDITIONS ARE ACTUALLY SET ON THIS TIME STEP                   BCSTEP.......14900
      USEFL = ((ISSFLO.NE.0).AND.(ITBCS.EQ.0)).OR.                       BCSTEP.......15000
     1   ((ISSFLO.EQ.0).AND.(ITBCS.NE.0))                                BCSTEP.......15100
      ANYFL = NSOPI1+NPBC1.GT.0                                          BCSTEP.......15200
      ANYTR = NSOUI1+NUBC1.GT.0                                          BCSTEP.......15300
      BCSFL(ITBCS) = USEFL.AND.ANYFL                                     BCSTEP.......15400
      BCSTR(ITBCS) = ANYTR                                               BCSTEP.......15500
      SETFL = SETBCS.AND.BCSFL(ITBCS)                                    BCSTEP.......15600
      SETTR = SETBCS.AND.BCSTR(ITBCS)                                    BCSTEP.......15700
      IF (BCSFL(ITBCS).OR.BCSTR(ITBCS)) THEN                             BCSTEP.......15800
         NCID = NCID + 1                                                 BCSTEP.......15900
         IF (SETBCS) CIDBCS(NCID) = BCSID                                BCSTEP.......16000
      END IF                                                             BCSTEP.......16100
C                                                                        BCSTEP.......16200
C.....IF NO TIME-DEPENDENT SOURCE/SINK CONDITIONS, SKIP THIS SECTION     BCSTEP.......16300
      IF ((NSOPI1+NSOUI1).EQ.0) GOTO 500                                 BCSTEP.......16400
C                                                                        BCSTEP.......16500
C.....ALLOCATE ARRAYS FOR SOURCE/SINK CONDITIONS                         BCSTEP.......16600
      ALLOCATE(QIN1(NN),UIN1(NN),QUIN1(NN),IQSOP1(NSOP1),IQSOU1(NSOU1))  BCSTEP.......16700
C                                                                        BCSTEP.......16800
C.....INPUT BCS DATASETS 3 & 4 (SOURCES OF FLUID MASS AND ENERGY OR      BCSTEP.......16900
C        SOLUTE MASS) FOR CURRENT TIME STEP                              BCSTEP.......17000
!      CALL SOURCE1(QIN1,UIN1,IQSOP1,QUIN1,IQSOU1,IQSOPT1,IQSOUT1,        BCSTEP.......17100
!     1   NSOP1,NSOU1,NFB,BCSID)                                          BCSTEP.......17200
C                                                                        BCSTEP.......17300
C.....SET TIME-STEP-DEPENDENT FLUID SOURCES/SINKS,                       BCSTEP.......17400
C      OR CONCENTRATIONS (TEMPERATURES) OF SOURCE FLUID                  BCSTEP.......17500
C                                                                        BCSTEP.......17600
      IF (NSOPI1.GT.0) THEN                                              BCSTEP.......17700
      DO 200 IQP1=1,NSOPI1                                               BCSTEP.......17800
         I=IQSOP1(IQP1)                                                  BCSTEP.......17900
         DO 150 IQP0=1,NSOPI                                             BCSTEP.......18000
            I0 = IQSOP(IQP0)                                             BCSTEP.......18100
            IF (IABS(I0).EQ.IABS(I)) THEN                                BCSTEP.......18200
               IQP = IQP0                                                BCSTEP.......18300
               GOTO 180                                                  BCSTEP.......18400
            END IF                                                       BCSTEP.......18500
  150    CONTINUE                                                        BCSTEP.......18600
         ERRCOD = 'BCS-3-2'                                              BCSTEP.......18700
         INERR(1) = IABS(I)                                              BCSTEP.......18800
         INERR(2) = ITBCS                                                BCSTEP.......18900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        BCSTEP.......19000
	   return
  180    CONTINUE                                                        BCSTEP.......19100
         IF (SETFL) THEN                                                 BCSTEP.......19200
            IF (I.GT.0) THEN                                             BCSTEP.......19300
               QIN(I) = QIN1(I)                                          BCSTEP.......19400
               IF (QIN(I).GT.0D0) UIN(I) = UIN1(I)                       BCSTEP.......19500
               IBCSOP(IQP) = 1                                           BCSTEP.......19600
            ELSE                                                         BCSTEP.......19700
               QIN(-I) = 0D0                                             BCSTEP.......19800
               IBCSOP(IQP) = 2                                           BCSTEP.......19900
            END IF                                                       BCSTEP.......20000
            IIDSOP(IQP) = NCID                                           BCSTEP.......20100
         END IF                                                          BCSTEP.......20200
  200 CONTINUE                                                           BCSTEP.......20300
      END IF                                                             BCSTEP.......20400
C                                                                        BCSTEP.......20500
C.....SET TIME-STEP-DEPENDENT SOURCES/SINKS                              BCSTEP.......20600
C     OF SOLUTE MASS OR ENERGY                                           BCSTEP.......20700
C                                                                        BCSTEP.......20800
      IF (NSOUI1.GT.0) THEN                                              BCSTEP.......20900
      DO 400 IQU1=1,NSOUI1                                               BCSTEP.......21000
         I=IQSOU1(IQU1)                                                  BCSTEP.......21100
         DO 350 IQU0=1,NSOUI                                             BCSTEP.......21200
            I0 = IQSOU(IQU0)                                             BCSTEP.......21300
            IF (IABS(I0).EQ.IABS(I)) THEN                                BCSTEP.......21400
               IQU = IQU0                                                BCSTEP.......21500
               GOTO 380                                                  BCSTEP.......21600
            END IF                                                       BCSTEP.......21700
  350    CONTINUE                                                        BCSTEP.......21800
         ERRCOD = 'BCS-4-2'                                              BCSTEP.......21900
         INERR(1) = IABS(I)                                              BCSTEP.......22000
         INERR(2) = ITBCS                                                BCSTEP.......22100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        BCSTEP.......22200
c rbw begin
         return
c rbw end
  380    CONTINUE                                                        BCSTEP.......22300
         IF (SETTR) THEN                                                 BCSTEP.......22400
            IF (I.GT.0) THEN                                             BCSTEP.......22500
               QUIN(I) = QUIN1(I)                                        BCSTEP.......22600
               IBCSOU(IQU) = 1                                           BCSTEP.......22700
            ELSE                                                         BCSTEP.......22800
               QUIN(-I) = 0D0                                            BCSTEP.......22900
               IBCSOU(IQU) = 2                                           BCSTEP.......23000
            END IF                                                       BCSTEP.......23100
            IIDSOU(IQU) = NCID                                           BCSTEP.......23200
         END IF                                                          BCSTEP.......23300
  400 CONTINUE                                                           BCSTEP.......23400
      END IF                                                             BCSTEP.......23500
C                                                                        BCSTEP.......23600
C.....DEALLOCATE ARRAYS FOR SOURCE/SINK CONDITIONS                       BCSTEP.......23700
      DEALLOCATE(QIN1,UIN1,QUIN1,IQSOP1,IQSOU1)                          BCSTEP.......23800
C                                                                        BCSTEP.......23900
C.....IF NO TIME-DEPENDENT SPECIFIED P OR U BOUNDARY CONDITIONS, SKIP    BCSTEP.......24000
C        THIS SECTION                                                    BCSTEP.......24100
  500 IF (NBCN1-1.EQ.0) GOTO 900                                         BCSTEP.......24200
C                                                                        BCSTEP.......24300
C.....ALLOCATE ARRAYS FOR SPECIFIED P AND U BOUNDARY CONDITIONS          BCSTEP.......24400
      ALLOCATE(IPBC1(NBCN1),PBC1(NBCN1),IUBC1(NBCN1),UBC1(NBCN1))        BCSTEP.......24500
C                                                                        BCSTEP.......24600
C.....INPUT BCS DATASETS 4 & 5 (SPECIFIED P AND U BOUNDARY CONDITIONS)   BCSTEP.......24700
C        FOR CURRENT TIME STEP                                           BCSTEP.......24800
!      CALL BOUND1(IPBC1,PBC1,IUBC1,UBC1,IPBCT1,IUBCT1,                   BCSTEP.......24900
!     1   NPBC1,NUBC1,NBCN1,NFB,BCSID, IERROR)                                    BCSTEP.......25000

c rbw begin change
      if (IERROR.ne.0) return
c rbw end change
C                                                                        BCSTEP.......25100
C.....SET TIME-STEP-DEPENDENT SPECIFIED PRESSURES OR                     BCSTEP.......25200
C     CONCENTRATIONS (TEMPERATURES) OF INFLOWS AT SPECIFIED              BCSTEP.......25300
C     PRESSURE NODES                                                     BCSTEP.......25400
C                                                                        BCSTEP.......25500
      IF (NPBC1.GT.0) THEN                                               BCSTEP.......25600
      DO 600 IP1=1,NPBC1                                                 BCSTEP.......25700
         I = IPBC1(IP1)                                                  BCSTEP.......25800
         DO 550 IP0=1,NPBC                                               BCSTEP.......25900
            I0 = IPBC(IP0)                                               BCSTEP.......26000
            IF (IABS(I0).EQ.IABS(I)) THEN                                BCSTEP.......26100
               IP = IP0                                                  BCSTEP.......26200
               GOTO 580                                                  BCSTEP.......26300
            END IF                                                       BCSTEP.......26400
  550    CONTINUE                                                        BCSTEP.......26500
         ERRCOD = 'BCS-5-2'                                              BCSTEP.......26600
         INERR(1) = IABS(I)                                              BCSTEP.......26700
         INERR(2) = ITBCS                                                BCSTEP.......26800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        BCSTEP.......26900
c rbw begin change
         return
c rbw end change
  580    CONTINUE                                                        BCSTEP.......27000
         IF (SETFL) THEN                                                 BCSTEP.......27100
            IF (I.GT.0) THEN                                             BCSTEP.......27200
               PBC(IP) = PBC1(IP1)                                       BCSTEP.......27300
               UBC(IP) = UBC1(IP1)                                       BCSTEP.......27400
               GNUP1(IP) = GNUP                                          BCSTEP.......27500
               IBCPBC(IP) = 1                                            BCSTEP.......27600
            ELSE                                                         BCSTEP.......27700
               GNUP1(IP) = 0D0                                           BCSTEP.......27800
               IBCPBC(IP) = 2                                            BCSTEP.......27900
            END IF                                                       BCSTEP.......28000
            IIDPBC(IP) = NCID                                            BCSTEP.......28100
         END IF                                                          BCSTEP.......28200
  600 CONTINUE                                                           BCSTEP.......28300
      END IF                                                             BCSTEP.......28400
C                                                                        BCSTEP.......28500
C.....SET TIME-STEP-DEPENDENT SPECIFIED                                  BCSTEP.......28600
C     CONCENTRATIONS (TEMPERATURES)                                      BCSTEP.......28700
C                                                                        BCSTEP.......28800
      IF (NUBC1.GT.0) THEN                                               BCSTEP.......28900
      DO 800 IU1=1,NUBC1                                                 BCSTEP.......29000
         IUP1 = IU1 + NPBC1                                              BCSTEP.......29100
         I=IUBC1(IUP1)                                                   BCSTEP.......29200
         DO 700 IU0=1,NUBC                                               BCSTEP.......29300
            IUP0 = IU0 + NPBC                                            BCSTEP.......29400
            I0 = IUBC(IUP0)                                              BCSTEP.......29500
            IF (IABS(I0).EQ.IABS(I)) THEN                                BCSTEP.......29600
               IUP = IUP0                                                BCSTEP.......29700
               GOTO 750                                                  BCSTEP.......29800
            END IF                                                       BCSTEP.......29900
  700    CONTINUE                                                        BCSTEP.......30000
         ERRCOD = 'BCS-6-2'                                              BCSTEP.......30100
         INERR(1) = IABS(I)                                              BCSTEP.......30200
         INERR(2) = ITBCS                                                BCSTEP.......30300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        BCSTEP.......30400
c rbw begin change
         return
c rbw end change
  750    CONTINUE                                                        BCSTEP.......30500
         IF (SETTR) THEN                                                 BCSTEP.......30600
            IF (I.GT.0) THEN                                             BCSTEP.......30700
               UBC(IUP) = UBC1(IUP1)                                     BCSTEP.......30800
               GNUU1(IUP) = GNUU                                         BCSTEP.......30900
               IBCUBC(IUP) = 1                                           BCSTEP.......31000
            ELSE                                                         BCSTEP.......31100
               GNUU1(IUP) = 0D0                                          BCSTEP.......31200
               IBCUBC(IUP) = 2                                           BCSTEP.......31300
            END IF                                                       BCSTEP.......31400
            IIDUBC(IUP) = NCID                                           BCSTEP.......31500
         END IF                                                          BCSTEP.......31600
  800 CONTINUE                                                           BCSTEP.......31700
      END IF                                                             BCSTEP.......31800
C                                                                        BCSTEP.......31900
C.....DEALLOCATE ARRAYS FOR SPECIFIED P AND U BOUNDARY CONDITIONS        BCSTEP.......32000
      DEALLOCATE(IPBC1,PBC1,IUBC1,UBC1)                                  BCSTEP.......32100
C                                                                        BCSTEP.......32200
C.....ADVANCE TO NEXT SCHEDULE ENTRY FOR THIS FILE (IF THERE IS ONE).    BCSTEP.......32300
  900 LCNT(NFB) = LCNT(NFB) + 1                                          BCSTEP.......32400
      IF (LCNT(NFB).LE.LENSCH)                                           BCSTEP.......32500
     1   DENBCS(NFB)%NENT => DENBCS(NFB)%NENT%NENT                       BCSTEP.......32600
C                                                                        BCSTEP.......32700
 1000 CONTINUE                                                           BCSTEP.......32800
      RETURN                                                             BCSTEP.......32900
      END                                                                BCSTEP.......33000
C                                                                        BCSTEP.......33100
C     SUBPROGRAM        B  D  I  N  I  T           SUTRA VERSION 2.2     BDINIT.........100
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
C                                                                        BDINIT........2900
C     SUBROUTINE        B  O  U  N  D              SUTRA VERSION 2.2     BOUND..........100
C                                                                        BOUND..........200
C *** PURPOSE :                                                          BOUND..........300
C ***  TO READ AND ORGANIZE DEFAULT VALUES FOR SPECIFIED PRESSURE DATA   BOUND..........400
C ***  AND SPECIFIED TEMPERATURE OR CONCENTRATION DATA.                  BOUND..........500
C                                                                        BOUND..........600
      SUBROUTINE BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT,IBCPBC,IBCUBC,      BOUND..........700
     1   GNUP1,GNUU1,                                                    BOUND..........800
     2   IERROR, IBOUSZ, IBNODE, IPOS)                                    BOUND..........700
      USE EXPINT                                                         BOUND..........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BOUND.........1000
      CHARACTER INTFIL*1000                                              BOUND.........1100
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:13)                    BOUND.........1200
      DIMENSION IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN)                BOUND.........1300
      DIMENSION GNUP1(NBCN),GNUU1(NBCN)                                  BOUND.........1400
      DIMENSION INERR(10),RLERR(10)                                      BOUND.........1500
      INTEGER(1) IBCPBC(NBCN),IBCUBC(NBCN)                               BOUND.........1600
      DIMENSION KTYPE(2)                                                 BOUND.........1700
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BOUND.........1800
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,           BOUND.........1900
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE                                       BOUND.........2000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BOUND.........2100
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            BOUND.........2200
      COMMON /FNAMES/ UNAME,FNAME                                        BOUND.........2300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 BOUND.........2400
     1   K10,K11,K12,K13                                                 BOUND.........2500
C RBW
      INTEGER IERROR
	INTEGER IBOUSZ, IPOS
	INTEGER IBNODE(IBOUSZ)
C RBW
C                                                                        BOUND.........2600
C                                                                        BOUND.........2700
      IPBCT=1                                                            BOUND.........2800
      IUBCT=1                                                            BOUND.........2900
      IP=0                                                               BOUND.........3000
      IPU=0                                                              BOUND.........3100
C RBW
      IPOS = IPOS + 1
	IBNODE(IPOS) = NPBC
C RBW
      IF(NPBC.EQ.0) GOTO 400                                             BOUND.........3200
!      WRITE(K3,100)                                                      BOUND.........3300
!  100 FORMAT('1'////11X,'S P E C I F I E D   P R E S S U R E   D A T A'  BOUND.........3400
!     1   ////11X,'**** NODES AT WHICH PRESSURES ARE SPECIFIED ****'/)    BOUND.........3500
!      IF(ME) 107,107,114                                                 BOUND.........3600
!  107 WRITE(K3,108)                                                      BOUND.........3700
!  108 FORMAT(16X,'(AS WELL AS SOLUTE CONCENTRATION OF ANY'               BOUND.........3800
!     1   /16X,' FLUID INFLOW WHICH MAY OCCUR AT THE POINT'               BOUND.........3900
!     2   /16X,' OF SPECIFIED PRESSURE)'                                  BOUND.........4000
!     3  //12X,'NODE',10X,'DEFAULT PRESSURE',                             BOUND.........4100
!     4     5X,'DEFAULT CONCENTRATION'//)                                 BOUND.........4200
!      GOTO 125                                                           BOUND.........4300
!  114 WRITE(K3,115)                                                      BOUND.........4400
!  115 FORMAT(16X,'(AS WELL AS TEMPERATURE {DEGREES CELSIUS} OF ANY'      BOUND.........4500
!     1   /16X,' FLUID INFLOW WHICH MAY OCCUR AT THE POINT'               BOUND.........4600
!     2   /16X,' OF SPECIFIED PRESSURE)'                                  BOUND.........4700
!     3  //12X,'NODE',10X,'DEFAULT PRESSURE',                             BOUND.........4800
!     4     5X,'  DEFAULT TEMPERATURE'//)                                 BOUND.........4900
C                                                                        BOUND.........5000
C.....INPUT DATASET 19:  DATA FOR SPECIFIED PRESSURE NODES               BOUND.........5100
  125 IPU=IPU+1                                                          BOUND.........5200
      ERRCOD = 'REA-INP-19'                                              BOUND.........5300
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 BOUND.........5400
c rbw begin change
	IF (IERROR.NE.0) RETURN
c rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                                BOUND.........5500
      IF (INERR(1).NE.0) then
	  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                         BOUND.........5600
c rbw begin change
	  RETURN
c rbw end change
	ENDIF
      IDUMA = IABS(IDUM)                                                 BOUND.........5700
      IF (IDUM.EQ.0) THEN                                                BOUND.........5800
         GOTO 180                                                        BOUND.........5900
      ELSE IF (IDUMA.GT.NN) THEN                                         BOUND.........6000
         ERRCOD = 'INP-19-1'                                             BOUND.........6100
         INERR(1) = IDUMA                                                BOUND.........6200
         INERR(2) = NN                                                   BOUND.........6300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        BOUND.........6400
c rbw begin change
	  RETURN
c rbw end change
      ELSE IF (IPU.GT.NPBC) THEN                                         BOUND.........6500
         GOTO 125                                                        BOUND.........6600
      END IF                                                             BOUND.........6700
      IPBC(IPU) = IDUM                                                   BOUND.........6800
C RBW
	IF (IPBC(IPU).GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IPBC(IPU) - 1                                                
	ELSE IF (IPBC(IPU).lT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = -IPBC(IPU) - 1                                                
	ENDIF
C RBW
      IF (IPBC(IPU).GT.0) THEN                                           BOUND.........6900
         ERRCOD = 'REA-INP-19'                                           BOUND.........7000
         READ(INTFIL,*,IOSTAT=INERR(1)) IPBC(IPU),PBC(IPU),UBC(IPU)      BOUND.........7100
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)             BOUND.........6600
C rbw begin change
	      RETURN
c rbw end change
	   ENDIF
!         WRITE(K3,160) IPBC(IPU),PBC(IPU),UBC(IPU)                       BOUND.........7300
      ELSE IF (IPBC(IPU).LT.0) THEN                                      BOUND.........7400
         IPBCT = -1                                                      BOUND.........7500
 !        WRITE(K3,160) IPBC(IPU)                                         BOUND.........7600
      ELSE                                                               BOUND.........7700
         PBC(NBCN) = 0D0                                                 BOUND.........7750
         GOTO 180                                                        BOUND.........7800
      END IF                                                             BOUND.........7900
!  160 FORMAT(7X,I9,6X,1PE20.13,6X,1PE20.13)                              BOUND.........8000
      GOTO 125                                                           BOUND.........8100
  180 IPU=IPU-1                                                          BOUND.........8200
      IP=IPU                                                             BOUND.........8300
      IF(IP.EQ.NPBC) GOTO 200                                            BOUND.........8400
      ERRCOD = 'INP-3,19-1'                                              BOUND.........8500
      INERR(1) = IP                                                      BOUND.........8600
      INERR(2) = NPBC                                                    BOUND.........8700
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           BOUND.........8800
C rbw begin change
  200 CONTINUE
!  200 IF(IPBCT.NE.-1) GOTO 250                                           BOUND.........8900
C rbw end change
!      IF(ME) 205,205,215                                                 BOUND.........9000
!  205 WRITE(K3,206)                                                      BOUND.........9100
!  206 FORMAT(//12X,'TIME-DEPENDENT SPECIFIED PRESSURE OR INFLOW ',       BOUND.........9200
!     1   'CONCENTRATION'/12X,'SET IN SUBROUTINE BCTIME IS INDICATED ',   BOUND.........9300
!     2   'BY NEGATIVE NODE NUMBER')                                      BOUND.........9400
!      GOTO 250                                                           BOUND.........9500
!  215 WRITE(K3,216)                                                      BOUND.........9600
!  216 FORMAT(//12X,'TIME-DEPENDENT SPECIFIED PRESSURE OR INFLOW ',       BOUND.........9700
!     1   'TEMPERATURE'/12X,'SET IN SUBROUTINE BCTIME IS INDICATED ',     BOUND.........9800
!     2   'BY NEGATIVE NODE NUMBER')                                      BOUND.........9900
!  250 WRITE(K3,252)                                                      BOUND........10000
!  252 FORMAT(/11X,'SPECIFICATIONS MADE IN (OPTIONAL) ',                  BOUND........10100
!     1   'BCS INPUT FILES TAKE PRECEDENCE OVER THE'/11X,                 BOUND........10200
!     2   'DEFAULT VALUES LISTED ABOVE AND ANY VALUES ',                  BOUND........10300
!     3   'SET IN SUBROUTINE BCTIME.')                                    BOUND........10400
C.....INITIALIZE GNUP1 ARRAY                                             BOUND........10500
      GNUP1 = GNUP                                                       BOUND........10600
C.....INITIALIZE ARRAY THAT INDICATES WHERE SPECIFIED-PRESSURE           BOUND........10700
C        CONDITIONS WERE SET (0 = INP FILE)                              BOUND........10800
      IBCPBC = 0                                                         BOUND........10900
C                                                                        BOUND........11000
C RBW
  400 IF(NUBC.EQ.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = 0                                                
      ENDIF                                                             
C RBW
      IF(NUBC.EQ.0) GOTO 9000                                            BOUND........11100
!      IF(ME) 500,500,550                                                 BOUND........11200
!  500 WRITE(K3,1000)                                                     BOUND........11300
! 1000 FORMAT('1'////11X,'S P E C I F I E D   C O N C E N T R A T I O N', BOUND........11400
!     1   '   D A T A'                                                    BOUND........11500
!     2   ////11X,'**** NODES AT WHICH SOLUTE CONCENTRATIONS ARE ',       BOUND........11600
!     3   'SPECIFIED TO BE INDEPENDENT OF LOCAL FLOWS AND FLUID SOURCES', BOUND........11700
!     4   ' ****'//12X,'NODE',5X,'DEFAULT CONCENTRATION'//)               BOUND........11800
!      GOTO 1125                                                          BOUND........11900
!  550 WRITE(K3,1001)                                                     BOUND........12000
! 1001 FORMAT('1'////11X,'S P E C I F I E D   T E M P E R A T U R E',     BOUND........12100
!     1   '   D A T A'                                                    BOUND........12200
!     2   ////11X,'**** NODES AT WHICH TEMPERATURES ARE ',                BOUND........12300
!     3   'SPECIFIED TO BE INDEPENDENT OF LOCAL FLOWS AND FLUID SOURCES', BOUND........12400
!     4   ' ****'//12X,'NODE',5X,'  DEFAULT TEMPERATURE'//)               BOUND........12500
C                                                                        BOUND........12600
C.....INPUT DATASET 20:  DATA FOR SPECIFIED CONCENTRATION OR             BOUND........12700
C        TEMPERATURE NODES                                               BOUND........12800
C RBW
      IPOS = IPOS + 1
	IBNODE(IPOS) = NUBC
C RBW
 1125 IPU=IPU+1                                                          BOUND........12900
      ERRCOD = 'REA-INP-20'                                              BOUND........13000
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 BOUND........13100
c rbw begin change
	IF (IERROR.NE.0) RETURN
c rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                                BOUND........13200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                BOUND........11100
c rbw begin change
	   RETURN
c rbw end change
	ENDIF
      IDUMA = IABS(IDUM)                                                 BOUND........13400
      IF (IDUM.EQ.0) THEN                                                BOUND........13500
         GOTO 1180                                                       BOUND........13600
      ELSE IF (IDUMA.GT.NN) THEN                                         BOUND........13700
         ERRCOD = 'INP-20-1'                                             BOUND........13800
         INERR(1) = IDUMA                                                BOUND........13900
         INERR(2) = NN                                                   BOUND........14000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        BOUND........14100
c rbw begin change
	   RETURN
c rbw end change
      ELSE IF (IPU.GT.NPBC+NUBC) THEN                                    BOUND........14200
         GOTO 1125                                                       BOUND........14300
      END IF                                                             BOUND........14400
      IUBC(IPU) = IDUM                                                   BOUND........14500
C RBW
	IF (IUBC(IPU).GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IUBC(IPU) - 1                                                
	ELSE IF (IUBC(IPU).lT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = -IUBC(IPU) - 1                                                
	ENDIF
C RBW
      IF (IUBC(IPU).GT.0) THEN                                           BOUND........14600
         ERRCOD = 'REA-INP-20'                                           BOUND........14700
         READ(INTFIL,*,IOSTAT=INERR(1)) IUBC(IPU),UBC(IPU)               BOUND........14800
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)              BOUND........12700
c rbw begin change
	      RETURN
c rbw end change
	   ENDIF
!         WRITE(K3,1150) IUBC(IPU),UBC(IPU)                               BOUND........15000
      ELSE IF (IUBC(IPU).LT.0) THEN                                      BOUND........15100
         IUBCT = -1                                                      BOUND........15200
!         WRITE(K3,1150) IUBC(IPU)                                        BOUND........15300
      ELSE                                                               BOUND........15400
         GOTO 1180                                                       BOUND........15500
      END IF                                                             BOUND........15600
! 1150 FORMAT(7X,I9,6X,1PE20.13)                                          BOUND........15700
      GOTO 1125                                                          BOUND........15800
 1180 IPU=IPU-1                                                          BOUND........15900
      IU=IPU-IP                                                          BOUND........16000
      IF(IU.EQ.NUBC) GOTO 1200                                           BOUND........16100
      IF (ME.EQ.1) THEN                                                  BOUND........16200
         ERRCOD = 'INP-3,20-2'                                           BOUND........16300
      ELSE                                                               BOUND........16400
         ERRCOD = 'INP-3,20-1'                                           BOUND........16500
      END IF                                                             BOUND........16600
      INERR(1) = IU                                                      BOUND........16700
      INERR(2) = NUBC                                                    BOUND........16800
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           BOUND........16900
c rbw begin change
	RETURN
 1200 continue 
c rbw end change
! 1200 IF(IUBCT.NE.-1) GOTO 6000                                          BOUND........17000
!      IF(ME) 1205,1205,1215                                              BOUND........17100
! 1205 WRITE(K3,1206)                                                     BOUND........17200
! 1206 FORMAT(//11X,'TIME-DEPENDENT SPECIFIED CONCENTRATIONS USER-',      BOUND........17300
!     1   'PROGRAMMED IN'/12X,'SUBROUTINE BCTIME ARE INDICATED BY ',      BOUND........17400
!     2   'A NEGATIVE NODE NUMBER.')                                      BOUND........17500
!      GOTO 6000                                                          BOUND........17600
! 1215 WRITE(K3,1216)                                                     BOUND........17700
! 1216 FORMAT(//11X,'TIME-DEPENDENT SPECIFIED TEMPERATURES USER-',        BOUND........17800
!     1   'PROGRAMMED IN'/12X,'SUBROUTINE BCTIME ARE INDICATED BY ',      BOUND........17900
!     2   'A NEGATIVE NODE NUMBER.')                                      BOUND........18000
! 6000 WRITE(K3,252)                                                      BOUND........18100
c rbw begin change
 6000 CONTINUE
c rbw end change
C.....INITIALIZE GNUU1 ARRAY                                             BOUND........18200
      GNUU1 = GNUU                                                       BOUND........18300
C.....INITIALIZE ARRAY THAT INDICATES WHERE SPECIFIED-CONC OR TEMP       BOUND........18400
C        CONDITIONS WERE SET (0 = INP FILE)                              BOUND........18500
      IBCUBC = 0                                                         BOUND........18600
C                                                                        BOUND........18700
C                                                                        BOUND........18800
 9000 RETURN                                                             BOUND........18900
      END                                                                BOUND........19000
C                                                                        BOUND........19100
C     SUBROUTINE        B  O  U  N  D  1           SUTRA VERSION 2.2     BOUND1.........100
C                                                                        BOUND1.........200
C *** PURPOSE :                                                          BOUND1.........300
C ***  TO READ AND ORGANIZE TIME-DEPENDENT SPECIFIED PRESSURE DATA AND   BOUND1.........400
C ***  SPECIFIED TEMPERATURE OR CONCENTRATION DATA SPECIFIED IN THE      BOUND1.........500
C ***  OPTIONAL BCS INPUT FILE.                                          BOUND1.........600
C                                                                        BOUND1.........700

C     SUBROUTINE        B  O  U  N  G              SUTRA VERSION 3.0       ! frz_vern    ! boung, entirely new subroutine ...
C                                                                       
C *** PURPOSE :                                                          
C ***  TO READ AND ORGANIZE DEFAULT VALUES FOR GENERALIZED-FLOW            ! cupbgo
C ***  AND GENERALIZED-TRANSPORT BOUNDARY CONDITION DATA.                  ! cupbgo
C                                                                        
      SUBROUTINE BOUNG(IERROR,IPBG,PBG1,QPBG1,PBG2,QPBG2, 
!     *	CPQL1,CPQL2, ! 7, 8
!     *  UPBGI,       ! cupbgo 9
!     1   CUPBGO,UPBGO,IUBG,UBG1, ! 10, 11, 12, 13
     *   QUBG1,                  ! 14
!     *   UBG2,                   ! 15
     *   QUBG2,                  ! 16
!     *   IPBGT,IUBGT,              ! cupbgo 17, 18
!     2   IBCPBG,                  ! 19
!     *   IBCUBG,                  ! 20
!     *   QPBGIC,                  ! 21
!     *   GNUPG,QUBGIC,            ! 22, 23
     *   GNUUG)                            ! cupbgo 24
      USE EXPINT                                                        
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)     
	integer IERROR                          
      CHARACTER INTFIL*1000                                            
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:13)                   
      CHARACTER*1 CPQL1(NPBG),CPQL2(NPBG)                               
      CHARACTER*3 CUPBGO(NPBG)                                            ! cupbgo
      DIMENSION PBG1(NPBG),QPBG1(NPBG),PBG2(NPBG),
     1   QPBG2(NPBG),UPBGI(NPBG),UBG1(NUBG),QUBG1(NUBG),                   ! cupbgo
     2   UBG2(NUBG),QUBG2(NUBG),UPBGO(NPBG)                                ! cupbgo
      DIMENSION IPBG(NPBG),IUBG(NUBG)
      DIMENSION QPBGIC(NPBG),GNUPG(NPBG),QUBGIC(NUBG),GNUUG(NUBG)          ! cupbgo
      DIMENSION INERR(10),RLERR(10)                                     
      INTEGER(1) IBCPBG(NBCN),IBCUBG(NBCN)                              
      DIMENSION KTYPE(2)                                                
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC, 
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,         
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE   ! frz_allsat                                 ! frz_input fixed                                      
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,             
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB 
      COMMON /FNAMES/ UNAME,FNAME                                      
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,               
     1   K10,K11,K12,K13                                               
C                                                                       
C                                                                      
ccc      IPBGT=1                                                             ! ibctf ccc  
ccc      IUBGT=1                                                             ! ibctf ccc  
      IPG=0                                                             
      IPUG=0                                                            
      IF(NPBG.EQ.0) GOTO 400                                           
!      WRITE(K3,100)                                                     
!  100 FORMAT('1'////11X,
!     1   'G E N E R A L I Z E D   F L O W   D A T A'                        ! cupbgo
!     2   ////11X,'**** NODES AT WHICH GENERALIZED-FLOW DATA ARE ',
!     3   'SPECIFIED ****'/)
!      IF(ME) 107,107,114                                               
!  107 WRITE(K3,108)                                                    
!  108 FORMAT(16X,'(AS WELL AS SOLUTE CONCENTRATION OF ANY'             
!     1   /16X,' FLUID INFLOW OR OUTFLOW WHICH MAY OCCUR'                    ! cupbgo
!     2   /16X,'  AT THE GENERALIZED-FLOW NODE)'                             ! cupbgo
!     3  //12X,'NODE',8X,'DEFAULT PRESSURE 1',6X,'DEFAULT IN/OUTFLOW 1',
!     4     8X,'DEFAULT PRESSURE 2',6X,'DEFAULT IN/OUTFLOW 2',
!     5     6X,'LIMIT 1',6X,'LIMIT 2',                                       ! cupbgo
!     6     7X,'DEFAULT INFLOW CONC',6X,'DEFAULT OUTFLOW CONC'//)            ! cupbgo
!      GOTO 125                                                         
!  114 WRITE(K3,115)                                                     
!  115 FORMAT(16X,'(AS WELL AS TEMPERATURE {DEGREES CELSIUS} OF ANY'     
!     1   /16X,' FLUID INFLOW OR OUTFLOW WHICH MAY OCCUR'                    ! cupbgo
!     2   /16X,'  AT THE GENERALIZED-FLOW NODE)'                             ! cupbgo
!     3  //12X,'NODE',8X,'DEFAULT PRESSURE 1',6X,'DEFAULT IN/OUTFLOW 1',
!     4     8X,'DEFAULT PRESSURE 2',6X,'DEFAULT IN/OUTFLOW 2',
!     5     6X,'LIMIT 1',6X,'LIMIT 2',                                       ! cupbgo
!     6     7X,'DEFAULT INFLOW TEMP',6X,'DEFAULT OUTFLOW TEMP'//)            ! cupbgo
C                                                                        
C.....INPUT DATASET 21A:  DATA FOR GENERALIZED-FLOW NODES                   ! cupbgo               
  125 IPUG=IPUG+1                                                       
      ERRCOD = 'REA-INP-21A'               ! kluge -- need to add error messages
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD) 
	if (IERROR.ne.0) then
	  return
	endif                                
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                               
      IF (INERR(1).NE.0) then
	  IERROR = 1
	  return
!	  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)    
	endif   
      IDUMA = IABS(IDUM)                                                
      IF (IDUM.EQ.0) THEN                                              
         GOTO 180                                                       
      ELSE IF (IDUMA.GT.NN) THEN                                        
	  IERROR = 1
	  return
!         ERRCOD = 'INP-21A-1'                                          
!         INERR(1) = IDUMA                                               
!         INERR(2) = NN                                                  
!         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                       
      ELSE IF (IPUG.GT.NPBG) THEN                                       
         GOTO 125                                                       
      END IF                                                            
      IPBG(IPUG) = IDUM                                          
      IF (IPBG(IPUG).GT.0) THEN                               
         ERRCOD = 'REA-INP-21A'                                         
         READ(INTFIL,*,IOSTAT=INERR(1)) IPBG(IPUG),
     1      PBG1(IPUG),QPBG1(IPUG),PBG2(IPUG),QPBG2(IPUG),
     2      CPQL1(IPUG),CPQL2(IPUG),UPBGI(IPUG),CUPBGO(IPUG),UPBGO(IPUG)    !cupbgo
         IF (INERR(1).NE.0) then
  	     IERROR = 1
	     return
!	     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)    
         endif
         RNUM = QPBG2(IPUG) - QPBG1(IPUG)
         RDEN = PBG2(IPUG) - PBG1(IPUG)
         IF (RDEN.NE.0D0) THEN                                      ! rdenz ...
            GNUPG(IPUG) = -RNUM/RDEN
         ELSE
            GNUPG(IPUG) = 0D0             ! kluge note: is this ok? -- think through; error if RNUM<>0 ???
         END IF                                                     ! ... rdenz
         QPBGIC(IPUG) = QPBG1(IPUG) + GNUPG(IPUG)*PBG1(IPUG)                ! cupbgo, budgen (bug fix)
!         WRITE(K3,160) IPBG(IPUG),
!     1      PBG1(IPUG),QPBG1(IPUG),PBG2(IPUG),QPBG2(IPUG),
!     2      CPQL1(IPUG),CPQL2(IPUG),UPBGI(IPUG),UPBGO(IPUG),CUPBGO(IPUG)    ! cupbgo
      ELSE IF (IPBG(IPUG).LT.0) THEN                               
         IPBGT = -1                                              
!         WRITE(K3,160) IPBG(IPUG)                                  
      ELSE                                                              
         GOTO 180                                                       
      END IF                                                            
  !160 FORMAT(7X,I9,4(6X,1PE20.13),2(A13),2(6X,1PE20.13),2X,A3)            ! cupbgo
      GOTO 125                                                          
  180 IPUG=IPUG-1                                                       
      IPG=IPUG                                                          
      IF(IPG.EQ.NPBG) GOTO 200                                          
	IERROR = 1
	return
!      ERRCOD = 'INP-3,21A-1'        ! kluge -- need to update dataset 3, too 
!      INERR(1) = IPG                                                    
!      INERR(2) = NPBG                                                   
!      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                          
  200 IF(IPBGT.NE.-1) GOTO 400                                          
!       IF(ME) 205,205,215                                               
!  205 WRITE(K3,206)                                                     
!  206 FORMAT(//12X,'TIME-DEPENDENT GENERALIZED-FLOW CONDITION ',           ! cupbgo
!     1   /12X,'SET IN SUBROUTINE BCTIME IS INDICATED ',                    ! cupbgo
!     2   'BY NEGATIVE NODE NUMBER')                                     
!      GOTO 250                                                          
!  215 WRITE(K3,216)                                                     
!  216 FORMAT(//12X,'TIME-DEPENDENT GENERALIZED-FLOW CONDITION ',           ! cupbgo
!     1   /12X,'SET IN SUBROUTINE BCTIME IS INDICATED ',                    ! cupbgo
!     2   'BY NEGATIVE NODE NUMBER')                                     
!  250 WRITE(K3,252)                                                     
!  252 FORMAT(/11X,'SPECIFICATIONS MADE IN (OPTIONAL) ',                 
!     1   'BCS INPUT FILES TAKE PRECEDENCE OVER THE'/11X,                
!     2   'DEFAULT VALUES LISTED ABOVE AND ANY VALUES ',                 
!     3   'SET IN SUBROUTINE BCTIME.')                                  
C.....INITIALIZE ARRAY THAT INDICATES WHERE GENERALIZED-FLOW         
C        CONDITIONS WERE SET (0 = INP FILE)                            
      IBCPBG = 0                                                       
C                                                                       
  400 IF(NUBG.EQ.0) GOTO 9000                                          
      IPUG = 0
!      IF(ME) 500,500,550                                                
!  500 WRITE(K3,1000)                                                    
! 1000 FORMAT('1'////11X,
!     1   'G E N E R A L I Z E D   T R A N S P O R T   D A T A'             ! cupbgo
!     2   ////11X,'**** NODES AT WHICH GENERALIZED-TRANSPORT ',             ! cupbgo
!     3   'DATA ARE SPECIFIED ****'                                         ! cupbgo
!     3  //12X,'NODE',
!     4     3X,'DEFAULT CONCENTRATION 1',6X,'DEFAULT IN/OUTFLOW 1',
!     5     3X,'DEFAULT CONCENTRATION 2',6X,'DEFAULT IN/OUTFLOW 2'//)
!      GOTO 1125                                                         
!  550 WRITE(K3,1001)                                                   
! 1001 FORMAT('1'////11X,
!     1   'G E N E R A L I Z E D   T R A N S P O R T   D A T A'             ! cupbgo
!     2   ////11X,'**** NODES AT WHICH GENERALIZED-TRANSPORT ',             ! cupbgo
!     3   'DATA ARE SPECIFIED ****'                                         ! cupbgo
!     3  //12X,'NODE',
!     4     5X,'DEFAULT TEMPERATURE 1',6X,'DEFAULT IN/OUTFLOW 1',
!     5     5X,'DEFAULT TEMPERATURE 2',6X,'DEFAULT IN/OUTFLOW 2'//)
C                                                                      
C.....INPUT DATASET 21B:  DATA FOR GENERALIZED-TRANSPORT NODES             ! cupbgo                                               
 1125 IPUG = IPUG + 1                                                
      ERRCOD = 'REA-INP-21B'                                           
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD) 
	if (IERROR.ne.0) then
	  return
	endif                                
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                               
      IF (INERR(1).NE.0) THEN
	  IERROR = 1
	  return
!	  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)      
	endif
      IDUMA = IABS(IDUM)                                                
      IF (IDUM.EQ.0) THEN                                              
         GOTO 1180                                                      
      ELSE IF (IDUMA.GT.NN) THEN                                        
	  IERROR = 1
	  return
!         ERRCOD = 'INP-21B-1'                                          
!         INERR(1) = IDUMA                                               
!         INERR(2) = NN                                                  
!         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                      
      ELSE IF (IPUG.GT.NUBG) THEN                                  
         GOTO 1125                                                      
      END IF                                                           
      IUBG(IPUG) = IDUM                                           
      IF (IUBG(IPUG).GT.0) THEN                                  
         ERRCOD = 'REA-INP-21B'                                         
         READ(INTFIL,*,IOSTAT=INERR(1)) IUBG(IPUG),
     1      UBG1(IPUG),QUBG1(IPUG),UBG2(IPUG),QUBG2(IPUG)
         IF (INERR(1).NE.0) then
  	     IERROR = 1
	     return
!	     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)    
	   endif
         RNUM = QUBG2(IPUG) - QUBG1(IPUG)
         RDEN = UBG2(IPUG) - UBG1(IPUG)
         GNUUG(IPUG) = -RNUM/RDEN
         QUBGIC(IPUG) = QUBG1(IPUG) + GNUUG(IPUG)*UBG1(IPUG)               ! cupbgo, budgen (bug fix)
!         WRITE(K3,1150) IUBG(IPUG),
!     1      UBG1(IPUG),QUBG1(IPUG),UBG2(IPUG),QUBG2(IPUG)
      ELSE IF (IUBG(IPUG).LT.0) THEN                                
         IUBGT = -1                                                     
!         WRITE(K3,1150) IUBG(IPUG)                                  
      ELSE                                                              
         GOTO 1180                                                      
      END IF                                                            
! 1150 FORMAT(11X,I9,5(6X,1PE20.13))                                     
      GOTO 1125                                                         
 1180 IPUG=IPUG-1                                                       
      IUG=IPUG                                                          
      IF(IUG.EQ.NUBG) GOTO 1200   
	IERROR = 1
	return                                      
!      IF (ME.EQ.1) THEN                                                 
!         ERRCOD = 'INP-3,21B-2'                                         
!      ELSE                                                              
!         ERRCOD = 'INP-3,21B-1'                                         
!      END IF                                                            
!      INERR(1) = IUG                                                    
!      INERR(2) = NUBG                                                   
!      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)  
 1200  CONTINUE                        
! 1200 IF(IUBGT.NE.-1) GOTO 6000                                         
!      IF(ME) 1205,1205,1215                                             
! 1205 WRITE(K3,1206)                                                   
! 1206 FORMAT(//11X,'TIME-DEPENDENT GENERALIZED-TRANSPORT CONDITIONS',     ! cupbgo
!     1   ' USER-PROGRAMMED IN'/12X,'SUBROUTINE BCTIME ARE INDICATED',     ! cupbgo
!     2   ' BY A NEGATIVE NODE NUMBER.')                                   ! cupbgo
!      GOTO 6000                                                        
! 1215 WRITE(K3,1216)                                                   
! 1216 FORMAT(//11X,'TIME-DEPENDENT GENERALIZED-TRANSPORT CONDITIONS',     ! cupbgo
!     1   ' USER-PROGRAMMED IN'/12X,'SUBROUTINE BCTIME ARE INDICATED',     ! cupbgo
!     2   ' BY A NEGATIVE NODE NUMBER.')                                   ! cupbgo
! 6000 WRITE(K3,252)                                                    
C.....INITIALIZE ARRAY THAT INDICATES WHERE GENERALIZED-TRANSPORT         ! cupbgo      
C        CONDITIONS WERE SET (0 = INP FILE)                             
      IBCUBG = 0                                                       
C                                                                       
C                                                                       
 9000 RETURN                                                           
      END                                                              

C     SUBROUTINE        B  U  D  G  E  T           SUTRA VERSION 2.2     BUDGET.........100
C                                                                        BUDGET.........200
C *** PURPOSE :                                                          BUDGET.........300
C ***  TO CALCULATE AND OUTPUT FLUID MASS AND SOLUTE MASS OR             BUDGET.........400
C ***  ENERGY BUDGETS.                                                   BUDGET.........500
C                                                                        BUDGET.........600
C     SUBROUTINE        C  O  N  N  E  C           SUTRA VERSION 2.2     CONNEC.........100
C                                                                        CONNEC.........200
C *** PURPOSE :                                                          CONNEC.........300
C ***  TO READ, ORGANIZE, AND CHECK DATA ON NODE INCIDENCES.             CONNEC.........400
C                                                                        CONNEC.........500
      SUBROUTINE CONNEC(IN, IERROR)                                              CONNEC.........600
      USE EXPINT                                                         CONNEC.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                CONNEC.........800
      CHARACTER INTFIL*1000                                              CONNEC.........900
      CHARACTER CDUM10*10                                                CONNEC........1000
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:13)                    CONNEC........1100
      DIMENSION IN(NIN)                                                  CONNEC........1200
      DIMENSION IIN(8)                                                   CONNEC........1300
      DIMENSION INERR(10),RLERR(10)                                      CONNEC........1400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              CONNEC........1500
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            CONNEC........1600
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        CONNEC........1700
      COMMON /FNAMES/ UNAME,FNAME                                        CONNEC........1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 CONNEC........1900
     1   K10,K11,K12,K13                                                 CONNEC........2000
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                CONNEC........2100
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            CONNEC........2200
C                                                                        CONNEC........2300
      IPIN=0                                                             CONNEC........2400
!      IF(KINCID.EQ.0) WRITE(K3,1)                                        CONNEC........2500
!    1 FORMAT('1'////11X,'M E S H   C O N N E C T I O N   D A T A'//      CONNEC........2600
!     1   16X,'PRINTOUT OF NODAL INCIDENCES CANCELLED.')                  CONNEC........2700
!      IF(KINCID.EQ.+1) WRITE(K3,2)                                       CONNEC........2800
!    2 FORMAT('1'////11X,'M E S H   C O N N E C T I O N   D A T A',       CONNEC........2900
!     1   ///11X,'**** NODAL INCIDENCES ****'///)                         CONNEC........3000
C                                                                        CONNEC........3100
C.....INPUT DATASET 22 AND CHECK FOR ERRORS                              CONNEC........3200
      ERRCOD = 'REA-INP-22'                                              CONNEC........3300
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 CONNEC........3400
C rbw begin change
	IF (IERROR.NE.0) RETURN
c rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                              CONNEC........3500
C rbw begin change
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                CONNEC........3400
	   RETURN
	ENDIF
c rbw end change
      IF (CDUM10.NE.'INCIDENCE ') THEN                                   CONNEC........3700
         ERRCOD = 'INP-22-1'                                             CONNEC........3800
C rbw begin change
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        CONNEC........3700
	   RETURN
c rbw end change
      END IF                                                             CONNEC........4000
      DO 1000 L=1,NE                                                     CONNEC........4100
      ERRCOD = 'REA-INP-22'                                              CONNEC........4200
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 CONNEC........4300
C rbw begin change
	IF (IERROR.NE.0) RETURN
c rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) LL,(IIN(II),II=1,N48)               CONNEC........4400
C rbw begin change
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                CONNEC........4300
	   RETURN
	ENDIF
c rbw end change
C.....PREPARE NODE INCIDENCE LIST FOR MESH, IN.                          CONNEC........4600
      DO 5 II=1,N48                                                      CONNEC........4700
      III=II+(L-1)*N48                                                   CONNEC........4800
    5 IN(III)=IIN(II)                                                    CONNEC........4900
      IF(IABS(LL).EQ.L) GOTO 500                                         CONNEC........5000
      ERRCOD = 'INP-22-2'                                                CONNEC........5100
      INERR(1) = LL                                                      CONNEC........5200
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           CONNEC........5300
C rbw begin change
	RETURN
c rbw end change
C                                                                        CONNEC........5400
C                                                                        CONNEC........5500
  500 M1=(L-1)*N48+1                                                     CONNEC........5600
      M8=M1+N48-1                                                        CONNEC........5700
!      IF(KINCID.EQ.0) GOTO 1000                                          CONNEC........5800
!      WRITE(K3,650) L,(IN(M),M=M1,M8)                                    CONNEC........5900
!  650 FORMAT(11X,'ELEMENT',I9,5X,' NODES AT : ',6X,'CORNERS ',           CONNEC........6000
!     1   5('*'),8I9,1X,5('*'))                                           CONNEC........6100
C                                                                        CONNEC........6200
 1000 CONTINUE                                                           CONNEC........6300
C                                                                        CONNEC........6400
C                                                                        CONNEC........6500
 5000 RETURN                                                             CONNEC........6600
      END                                                                CONNEC........6700
C                                                                        CONNEC........6800
C     FUNCTION          C  U  T  S  M  L           SUTRA VERSION 2.2     CUTSML.........100
C                                                                        CUTSML.........200
C *** PURPOSE :                                                          CUTSML.........300
C ***  TO RETURN ARGUMENT DPNUM IF ITS MAGNITUDE IS GREATER THAN OR      CUTSML.........400
C ***  EQUAL TO 1.D-99, AND ZERO OTHERWISE.                              CUTSML.........500
C                                                                        CUTSML.........600
      DOUBLE PRECISION FUNCTION CUTSML(DPNUM)                            CUTSML.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               CUTSML.........800
C                                                                        CUTSML.........900
C.....RETURN DPNUM IF ITS ABSOLUTE VALUE IS >= 1.D-99, OTHERWISE         CUTSML........1000
C        RETURN ZERO                                                     CUTSML........1100
      IF (DABS(DPNUM).LT.1.D-99) THEN                                    CUTSML........1200
         CUTSML = 0D0                                                    CUTSML........1300
      ELSE                                                               CUTSML........1400
         CUTSML = DPNUM                                                  CUTSML........1500
      END IF                                                             CUTSML........1600
C                                                                        CUTSML........1700
      RETURN                                                             CUTSML........1800
      END                                                                CUTSML........1900
C                                                                        CUTSML........2000
C     SUBROUTINE        D  I  M  W  R  K           SUTRA VERSION 2.2     DIMWRK.........100
C                                                                        DIMWRK.........200
C *** PURPOSE :                                                          DIMWRK.........300
C ***  TO RETURN DIMENSIONS FOR THE SOLVER WORK ARRAYS, WHICH DEPEND ON  DIMWRK.........400
C ***  THE PARTICULAR SOLVER CHOSEN.                                     DIMWRK.........500
C                                                                        DIMWRK.........600
      SUBROUTINE DIMWRK(KSOLVR, NSAVE, NN, NELT, NWI, NWF)               DIMWRK.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                DIMWRK.........800
C                                                                        DIMWRK.........900
C.....COMPUTE SOLVER WORK ARRAY DIMENSIONS                               DIMWRK........1000
      IF (KSOLVR.EQ.1) THEN                                              DIMWRK........1100
         NL = (NELT + NN)/2                                              DIMWRK........1200
         NWI = 11 + 2*NL                                                 DIMWRK........1300
         NWF = NL + 5*NN + 1                                             DIMWRK........1400
      ELSE IF (KSOLVR.EQ.2) THEN                                         DIMWRK........1500
         NWI = 31 + 2*NELT                                               DIMWRK........1600
         NWF = 2 + NN*(NSAVE + 7) + NSAVE*(NSAVE + 3) + (NELT - NN)      DIMWRK........1700
      ELSE IF (KSOLVR.EQ.3) THEN                                         DIMWRK........1800
         NWI = 11 + 2*NELT                                               DIMWRK........1900
         NWF = 1 + 3*NN*(NSAVE + 1) + 7*NN + NSAVE + (NELT - NN)         DIMWRK........2000
      END IF                                                             DIMWRK........2100
C                                                                        DIMWRK........2200
      RETURN                                                             DIMWRK........2300
      END                                                                DIMWRK........2400
C                                                                        DIMWRK........2500
C     SUBROUTINE        D  I  S  P  R  3           SUTRA VERSION 2.2     DISPR3.........100
C                                                                        DISPR3.........200
C *** PURPOSE :                                                          DISPR3.........300
C ***  TO COMPUTE THE COMPONENTS OF THE 3D DISPERSION TENSOR IN          DISPR3.........400
C ***  X,Y,Z-COORDINATES USING AN AD HOC, 3D ANISOTROPIC DISPERSION      DISPR3.........500
C ***  MODEL.                                                            DISPR3.........600
C                                                                        DISPR3.........700
C     FUNCTION          D  P  3  S  T  R           SUTRA VERSION 2.2     DP3STR.........100
C                                                                        DP3STR.........200
C *** PURPOSE :                                                          DP3STR.........300
C ***  TO RETURN THREE DOUBLE-PRECISION NUMBERS IN THE FORM OF A         DP3STR.........400
C ***  STRING.  THE THREE NUMBERS ARE PASSED IN THROUGH ARRAY DPA        DP3STR.........500
C ***  AND ARE ROUNDED USING FUNCTION CUTSML IN PREPARATION FOR OUTPUT.  DP3STR.........600
C                                                                        DP3STR.........700
C     SUBROUTINE        E  L  E  M  N  2           SUTRA VERSION 2.2     ELEMN2.........100
C                                                                        ELEMN2.........200
C *** PURPOSE :                                                          ELEMN2.........300
C ***  TO CONTROL AND CARRY OUT ALL CALCULATIONS FOR EACH ELEMENT BY     ELEMN2.........400
C ***  OBTAINING ELEMENT INFORMATION FROM THE BASIS FUNCTION ROUTINE,    ELEMN2.........500
C ***  CARRYING OUT GAUSSIAN INTEGRATION OF FINITE ELEMENT INTEGRALS,    ELEMN2.........600
C ***  AND ASSEMBLING RESULTS OF ELEMENTWISE INTEGRATIONS INTO           ELEMN2.........700
C ***  A GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW AND TRANSPORT     ELEMN2.........800
C ***  EQUATIONS. ALSO CALCULATES VELOCITY AT EACH ELEMENT CENTROID FOR  ELEMN2.........900
C ***  PRINTED OUTPUT. THIS SUBROUTINE HANDLES 2D CALCULATIONS ONLY;     ELEMN2........1000
C ***  3D CALCULATIONS ARE PERFORMED IN SUBROUTINE ELEMN3.               ELEMN2........1100
C                                                                        ELEMN2........1200
C                                                                        ELEMN2........1300
C     SUBROUTINE        E  L  E  M  N  3           SUTRA VERSION 2.2     ELEMN3.........100
C                                                                        ELEMN3.........200
C *** PURPOSE :                                                          ELEMN3.........300
C ***  TO CONTROL AND CARRY OUT ALL CALCULATIONS FOR EACH ELEMENT BY     ELEMN3.........400
C ***  OBTAINING ELEMENT INFORMATION FROM THE BASIS FUNCTION ROUTINE,    ELEMN3.........500
C ***  CARRYING OUT GAUSSIAN INTEGRATION OF FINITE ELEMENT INTEGRALS,    ELEMN3.........600
C ***  AND ASSEMBLING RESULTS OF ELEMENTWISE INTEGRATIONS INTO           ELEMN3.........700
C ***  A GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW AND TRANSPORT     ELEMN3.........800
C ***  EQUATIONS. ALSO CALCULATES VELOCITY AT EACH ELEMENT CENTROID FOR  ELEMN3.........900
C ***  PRINTED OUTPUT. THIS SUBROUTINE HANDLES 3D CALCULATIONS ONLY.     ELEMN3........1000
C ***  2D CALCULATIONS ARE PERFORMED IN SUBROUTINE ELEMN2.               ELEMN3........1100
C                                                                        ELEMN3........1200
C                                                                        ELEMN3........1300
C     SUBROUTINE        F  I  N  D  L  2           SUTRA VERSION 2.2     FINDL2.........100
C                                                                        FINDL2.........200
C *** PURPOSE :                                                          FINDL2.........300
C ***  TO DETERMINE WHETHER POINT (XK, YK) IN 2D GLOBAL COORDINATES      FINDL2.........400
C ***  IS CONTAINED WITHIN ELEMENT LL.  IF THE POINT IS INSIDE THE       FINDL2.........500
C ***  ELEMENT, SET INOUT = 1; IF OUTSIDE, SET INOUT = 0.  CONDITION     FINDL2.........600
C ***  INOUT = 99 SIGNALS CONVERGENCE FAILURE.  ADAPTED FROM SUTRAPLOT   FINDL2.........700
C ***  SUBROUTINE ITER2D.                                                FINDL2.........800
C                                                                        FINDL2.........900
      SUBROUTINE FINDL2(X,Y,IN,LL,XK,YK,XSI,ETA,INOUT)                   FINDL2........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FINDL2........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              FINDL2........1200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            FINDL2........1300
      DIMENSION IN(NE*4)                                                 FINDL2........1400
      DIMENSION X(NN), Y(NN)                                             FINDL2........1500
      DATA TOL /0.001/, ITRMAX /25/, EPSILON /0.001/                     FINDL2........1600
C                                                                        FINDL2........1700
C.....DEFINE OPE = (1. + EPSILON) FOR CONVENIENCE.                       FINDL2........1800
      OPE = 1D0 + EPSILON                                                FINDL2........1900
C                                                                        FINDL2........2000
C.....SET CORNER COORDINATES.                                            FINDL2........2100
      M0 = (LL - 1)*4                                                    FINDL2........2200
      X1 = X(IN(M0+1))                                                   FINDL2........2300
      X2 = X(IN(M0+2))                                                   FINDL2........2400
      X3 = X(IN(M0+3))                                                   FINDL2........2500
      X4 = X(IN(M0+4))                                                   FINDL2........2600
      Y1 = Y(IN(M0+1))                                                   FINDL2........2700
      Y2 = Y(IN(M0+2))                                                   FINDL2........2800
      Y3 = Y(IN(M0+3))                                                   FINDL2........2900
      Y4 = Y(IN(M0+4))                                                   FINDL2........3000
C                                                                        FINDL2........3100
C.....CALCULATE COEFFICIENTS.                                            FINDL2........3200
      AX = +X1+X2+X3+X4                                                  FINDL2........3300
      BX = -X1+X2+X3-X4                                                  FINDL2........3400
      CX = -X1-X2+X3+X4                                                  FINDL2........3500
      DX = +X1-X2+X3-X4                                                  FINDL2........3600
      AY = +Y1+Y2+Y3+Y4                                                  FINDL2........3700
      BY = -Y1+Y2+Y3-Y4                                                  FINDL2........3800
      CY = -Y1-Y2+Y3+Y4                                                  FINDL2........3900
      DY = +Y1-Y2+Y3-Y4                                                  FINDL2........4000
                                                                         FINDL2........4100
C                                                                        FINDL2........4200
C.....INITIAL GUESS OF ZERO FOR XSI AND ETA.                             FINDL2........4300
      XSI=0.0                                                            FINDL2........4400
      ETA=0.0                                                            FINDL2........4500
C                                                                        FINDL2........4600
C.....ITERATION LOOP TO SOLVE FOR LOCAL COORDINATES.                     FINDL2........4700
C                                                                        FINDL2........4800
      DO 800 I=1,ITRMAX                                                  FINDL2........4900
C                                                                        FINDL2........5000
         F10 = AX - 4.*XK + BX*XSI + CX*ETA + DX*XSI*ETA                 FINDL2........5100
         F20 = AY - 4.*YK + BY*XSI + CY*ETA + DY*XSI*ETA                 FINDL2........5200
         FP11 = BX + DX*ETA                                              FINDL2........5300
         FP12 = CX + DX*XSI                                              FINDL2........5400
         FP21 = BY + DY*ETA                                              FINDL2........5500
         FP22 = CY + DY*XSI                                              FINDL2........5600
C                                                                        FINDL2........5700
         DETXSI = -F10*FP22 + F20*FP12                                   FINDL2........5800
         DETETA = -F20*FP11 + F10*FP21                                   FINDL2........5900
         DETERM = FP11*FP22 - FP12*FP21                                  FINDL2........6000
         DELXSI = DETXSI/DETERM                                          FINDL2........6100
         DELETA = DETETA/DETERM                                          FINDL2........6200
C                                                                        FINDL2........6300
         XSI = XSI + DELXSI                                              FINDL2........6400
         ETA = ETA + DELETA                                              FINDL2........6500
C                                                                        FINDL2........6600
C........STOP ITERATING IF CHANGE IN XSI AND ETA < TOL.                  FINDL2........6700
         IF ((ABS(DELXSI).LT.TOL).AND.(ABS(DELETA).LT.TOL)) GOTO 900     FINDL2........6800
C                                                                        FINDL2........6900
  800 CONTINUE                                                           FINDL2........7000
C                                                                        FINDL2........7100
C.....ITERATONS FAILED TO CONVERGE.  SET INOUT = 99 AND RETURN.          FINDL2........7200
      INOUT = 99                                                         FINDL2........7300
      GOTO 1000                                                          FINDL2........7400
C                                                                        FINDL2........7500
C.....ITERATIONS CONVERGED.  IF POINT IS INSIDE THE ELEMENT,             FINDL2........7600
C        SET INOUT = 1.  IF OUTSIDE, SET INOUT = 0.                      FINDL2........7700
  900 INOUT = 1                                                          FINDL2........7800
      IF ((ABS(XSI).GT.OPE).OR.(ABS(ETA).GT.OPE)) INOUT = 0              FINDL2........7900
C                                                                        FINDL2........8000
 1000 RETURN                                                             FINDL2........8100
      END                                                                FINDL2........8200
C                                                                        FINDL2........8300
C     SUBROUTINE        F  I  N  D  L  3           SUTRA VERSION 2.2     FINDL3.........100
C                                                                        FINDL3.........200
C *** PURPOSE :                                                          FINDL3.........300
C ***  TO DETERMINE WHETHER POINT (XK, YK, ZK) IN 3D GLOBAL COORDINATES  FINDL3.........400
C ***  IS CONTAINED WITHIN ELEMENT LL.  IF THE POINT IS INSIDE THE       FINDL3.........500
C ***  ELEMENT, SET INOUT = 1; IF OUTSIDE, SET INOUT = 0.  CONDITION     FINDL3.........600
C ***  INOUT = 99 SIGNALS CONVERGENCE FAILURE.  ADAPTED FROM SUTRAPLOT   FINDL3.........700
C ***  SUBROUTINE ITER3D.                                                FINDL3.........800
C                                                                        FINDL3.........900
      SUBROUTINE FINDL3(X,Y,Z,IN,LL,XK,YK,ZK,XSI,ETA,ZET,INOUT)          FINDL3........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FINDL3........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              FINDL3........1200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            FINDL3........1300
      DIMENSION IN(NE*8)                                                 FINDL3........1400
      DIMENSION X(NN), Y(NN), Z(NN)                                      FINDL3........1500
      DATA TOL /0.001/, ITRMAX /25/, EPSILON /0.001/                     FINDL3........1600
C                                                                        FINDL3........1700
C.....DEFINE OPE = (1. + EPSILON) FOR CONVENIENCE.                       FINDL3........1800
      OPE = 1D0 + EPSILON                                                FINDL3........1900
C                                                                        FINDL3........2000
C.....SET CORNER COORDINATES.                                            FINDL3........2100
      M0 = (LL - 1)*8                                                    FINDL3........2200
      X1 = X(IN(M0+1))                                                   FINDL3........2300
      X2 = X(IN(M0+2))                                                   FINDL3........2400
      X3 = X(IN(M0+3))                                                   FINDL3........2500
      X4 = X(IN(M0+4))                                                   FINDL3........2600
      X5 = X(IN(M0+5))                                                   FINDL3........2700
      X6 = X(IN(M0+6))                                                   FINDL3........2800
      X7 = X(IN(M0+7))                                                   FINDL3........2900
      X8 = X(IN(M0+8))                                                   FINDL3........3000
      Y1 = Y(IN(M0+1))                                                   FINDL3........3100
      Y2 = Y(IN(M0+2))                                                   FINDL3........3200
      Y3 = Y(IN(M0+3))                                                   FINDL3........3300
      Y4 = Y(IN(M0+4))                                                   FINDL3........3400
      Y5 = Y(IN(M0+5))                                                   FINDL3........3500
      Y6 = Y(IN(M0+6))                                                   FINDL3........3600
      Y7 = Y(IN(M0+7))                                                   FINDL3........3700
      Y8 = Y(IN(M0+8))                                                   FINDL3........3800
      Z1 = Z(IN(M0+1))                                                   FINDL3........3900
      Z2 = Z(IN(M0+2))                                                   FINDL3........4000
      Z3 = Z(IN(M0+3))                                                   FINDL3........4100
      Z4 = Z(IN(M0+4))                                                   FINDL3........4200
      Z5 = Z(IN(M0+5))                                                   FINDL3........4300
      Z6 = Z(IN(M0+6))                                                   FINDL3........4400
      Z7 = Z(IN(M0+7))                                                   FINDL3........4500
      Z8 = Z(IN(M0+8))                                                   FINDL3........4600
C                                                                        FINDL3........4700
C.....CALCULATE COEFFICIENTS.                                            FINDL3........4800
      AX = +X1+X2+X3+X4+X5+X6+X7+X8                                      FINDL3........4900
      BX = -X1+X2+X3-X4-X5+X6+X7-X8                                      FINDL3........5000
      CX = -X1-X2+X3+X4-X5-X6+X7+X8                                      FINDL3........5100
      DX = -X1-X2-X3-X4+X5+X6+X7+X8                                      FINDL3........5200
      EX = +X1-X2+X3-X4+X5-X6+X7-X8                                      FINDL3........5300
      FX = +X1-X2-X3+X4-X5+X6+X7-X8                                      FINDL3........5400
      GX = +X1+X2-X3-X4-X5-X6+X7+X8                                      FINDL3........5500
      HX = -X1+X2-X3+X4+X5-X6+X7-X8                                      FINDL3........5600
      AY = +Y1+Y2+Y3+Y4+Y5+Y6+Y7+Y8                                      FINDL3........5700
      BY = -Y1+Y2+Y3-Y4-Y5+Y6+Y7-Y8                                      FINDL3........5800
      CY = -Y1-Y2+Y3+Y4-Y5-Y6+Y7+Y8                                      FINDL3........5900
      DY = -Y1-Y2-Y3-Y4+Y5+Y6+Y7+Y8                                      FINDL3........6000
      EY = +Y1-Y2+Y3-Y4+Y5-Y6+Y7-Y8                                      FINDL3........6100
      FY = +Y1-Y2-Y3+Y4-Y5+Y6+Y7-Y8                                      FINDL3........6200
      GY = +Y1+Y2-Y3-Y4-Y5-Y6+Y7+Y8                                      FINDL3........6300
      HY = -Y1+Y2-Y3+Y4+Y5-Y6+Y7-Y8                                      FINDL3........6400
      AZ = +Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8                                      FINDL3........6500
      BZ = -Z1+Z2+Z3-Z4-Z5+Z6+Z7-Z8                                      FINDL3........6600
      CZ = -Z1-Z2+Z3+Z4-Z5-Z6+Z7+Z8                                      FINDL3........6700
      DZ = -Z1-Z2-Z3-Z4+Z5+Z6+Z7+Z8                                      FINDL3........6800
      EZ = +Z1-Z2+Z3-Z4+Z5-Z6+Z7-Z8                                      FINDL3........6900
      FZ = +Z1-Z2-Z3+Z4-Z5+Z6+Z7-Z8                                      FINDL3........7000
      GZ = +Z1+Z2-Z3-Z4-Z5-Z6+Z7+Z8                                      FINDL3........7100
      HZ = -Z1+Z2-Z3+Z4+Z5-Z6+Z7-Z8                                      FINDL3........7200
C                                                                        FINDL3........7300
C.....INITIAL GUESS OF ZERO FOR XSI, ETA, AND ZETA.                      FINDL3........7400
      XSI=0.0                                                            FINDL3........7500
      ETA=0.0                                                            FINDL3........7600
      ZET=0.0                                                            FINDL3........7700
C                                                                        FINDL3........7800
C.....ITERATION LOOP TO SOLVE FOR LOCAL COORDINATES.                     FINDL3........7900
C                                                                        FINDL3........8000
      DO 800 I=1,ITRMAX                                                  FINDL3........8100
C                                                                        FINDL3........8200
         F10 = AX - 8.*XK + BX*XSI + CX*ETA + DX*ZET + EX*XSI*ETA        FINDL3........8300
     1        + FX*XSI*ZET + GX*ETA*ZET + HX*XSI*ETA*ZET                 FINDL3........8400
         F20 = AY - 8.*YK + BY*XSI + CY*ETA + DY*ZET + EY*XSI*ETA        FINDL3........8500
     1        + FY*XSI*ZET + GY*ETA*ZET + HY*XSI*ETA*ZET                 FINDL3........8600
         F30 = AZ - 8.*ZK + BZ*XSI + CZ*ETA + DZ*ZET + EZ*XSI*ETA        FINDL3........8700
     1        + FZ*XSI*ZET + GZ*ETA*ZET + HZ*XSI*ETA*ZET                 FINDL3........8800
         FP11 = BX + EX*ETA + FX*ZET + HX*ETA*ZET                        FINDL3........8900
         FP12 = CX + EX*XSI + GX*ZET + HX*XSI*ZET                        FINDL3........9000
         FP13 = DX + FX*XSI + GX*ETA + HX*XSI*ETA                        FINDL3........9100
         FP21 = BY + EY*ETA + FY*ZET + HY*ETA*ZET                        FINDL3........9200
         FP22 = CY + EY*XSI + GY*ZET + HY*XSI*ZET                        FINDL3........9300
         FP23 = DY + FY*XSI + GY*ETA + HY*XSI*ETA                        FINDL3........9400
         FP31 = BZ + EZ*ETA + FZ*ZET + HZ*ETA*ZET                        FINDL3........9500
         FP32 = CZ + EZ*XSI + GZ*ZET + HZ*XSI*ZET                        FINDL3........9600
         FP33 = DZ + FZ*XSI + GZ*ETA + HZ*XSI*ETA                        FINDL3........9700
C                                                                        FINDL3........9800
         S11 = FP22*FP33 - FP32*FP23                                     FINDL3........9900
         S12 = FP21*FP33 - FP31*FP23                                     FINDL3.......10000
         S13 = FP21*FP32 - FP31*FP22                                     FINDL3.......10100
         CF12 = -F20*FP33 + F30*FP23                                     FINDL3.......10200
         CF34 = -F20*FP32 + F30*FP22                                     FINDL3.......10300
         CF43 = -CF34                                                    FINDL3.......10400
         CF56 = -F30*FP21 + F20*FP31                                     FINDL3.......10500
C                                                                        FINDL3.......10600
         DETXSI = -F10*S11 - FP12*CF12 + FP13*CF34                       FINDL3.......10700
         DETETA = FP11*CF12 + F10*S12 + FP13*CF56                        FINDL3.......10800
         DETZET = FP11*CF43 - FP12*CF56 - F10*S13                        FINDL3.......10900
         DETERM = FP11*S11 - FP12*S12 + FP13*S13                         FINDL3.......11000
         DELXSI = DETXSI/DETERM                                          FINDL3.......11100
         DELETA = DETETA/DETERM                                          FINDL3.......11200
         DELZET = DETZET/DETERM                                          FINDL3.......11300
C                                                                        FINDL3.......11400
         XSI = XSI + DELXSI                                              FINDL3.......11500
         ETA = ETA + DELETA                                              FINDL3.......11600
         ZET = ZET + DELZET                                              FINDL3.......11700
C                                                                        FINDL3.......11800
C........STOP ITERATING IF CHANGE IN XSI, ETA, AND ZETA < TOL.           FINDL3.......11900
         IF ((ABS(DELXSI).LT.TOL).AND.(ABS(DELETA).LT.TOL).AND.          FINDL3.......12000
     1       (ABS(DELZET).LT.TOL)) GOTO 900                              FINDL3.......12100
C                                                                        FINDL3.......12200
  800 CONTINUE                                                           FINDL3.......12300
C                                                                        FINDL3.......12400
C.....ITERATONS FAILED TO CONVERGE.  SET INOUT = 99 AND RETURN.          FINDL3.......12500
      INOUT = 99                                                         FINDL3.......12600
      GOTO 1000                                                          FINDL3.......12700
C                                                                        FINDL3.......12800
C.....ITERATIONS CONVERGED.  IF POINT IS INSIDE THE ELEMENT,             FINDL3.......12900
C        SET INOUT = 1.  IF OUTSIDE, SET INOUT = 0.                      FINDL3.......13000
  900 INOUT = 1                                                          FINDL3.......13100
      IF ((ABS(XSI).GT.OPE).OR.(ABS(ETA).GT.OPE).OR.(ABS(ZET).GT.OPE))   FINDL3.......13200
     1   INOUT = 0                                                       FINDL3.......13300
C                                                                        FINDL3.......13400
 1000 RETURN                                                             FINDL3.......13500
      END                                                                FINDL3.......13600
C                                                                        FINDL3.......13700
C     SUBROUTINE        F  O  P  E  N              SUTRA VERSION 2.2     FOPEN..........100
C                                                                        FOPEN..........200
C *** PURPOSE :                                                          FOPEN..........300
C ***  OPENS FILES FOR SUTRA SIMULATION.  READS AND PROCESSES FILE       FOPEN..........400
C ***  SPECIFICATIONS FROM "SUTRA.FIL" AND OPENS INPUT AND OUTPUT FILES. FOPEN..........500
C                                                                        FOPEN..........600
      SUBROUTINE FOPEN(IERROR)                                                 FOPEN..........700
      USE EXPINT                                                         FOPEN..........800
      USE SCHDEF                                                         FOPEN..........900
      USE BCSDEF                                                         FOPEN.........1000
      USE FINDEF                                                         FOPEN.........1100
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FOPEN.........1200
      PARAMETER (IUNMIN=11)                                              FOPEN.........1300
      CHARACTER*80 FT,FN,UNAME,FNAME,ENAME,ENDEF,FTSTR                   FOPEN.........1400
      CHARACTER*80 FNROOT,FNEXTN                                         FOPEN.........1500
      CHARACTER*80 ERRCOD,CHERR(10)                                      FOPEN.........1600
      CHARACTER*8 VERNUM, VERNIN                                         FOPEN.........1700
      CHARACTER INTFIL*1000                                              FOPEN.........1800
      LOGICAL IS                                                         FOPEN.........1900
      LOGICAL ONCEFO                                                     FOPEN.........2000
      LOGICAL ALCBCS,ALCFIN,ALCOBS                                       FOPEN.........2100
      DIMENSION FNAME(0:13),IUNIT(0:13)                                  FOPEN.........2200
      DIMENSION FTSTR(0:13)                                              FOPEN.........2300
      DIMENSION INERR(10),RLERR(10)                                      FOPEN.........2400
      DIMENSION KTYPE(2)                                                 FOPEN.........2500
      COMMON /ALC/ ALCBCS,ALCFIN,ALCOBS                                  FOPEN.........2600
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  FOPEN.........2700
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,           FOPEN.........2800
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE                                       FOPEN.........2900
      COMMON /FNAMES/ UNAME,FNAME                                        FOPEN.........3000
      COMMON /FO/ONCEFO                                                  FOPEN.........3100
      COMMON /FUNIB/ NFBCS                                               FOPEN.........3200
      COMMON /FUNITA/ IUNIT                                              FOPEN.........3300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 FOPEN.........3400
     1   K10,K11,K12,K13                                                 FOPEN.........3500
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      FOPEN.........3600
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    FOPEN.........3700
      COMMON /VER/ VERNUM, VERNIN                                        FOPEN.........3800
      DATA (FTSTR(NFT),NFT=0,13)/'SMY','INP','ICS','LST','RST',          FOPEN.........3900
     1   'NOD','ELE','OBS','OBC','BCS','BCOF','BCOS','BCOP','BCOU'/      FOPEN.........4000
C                                                                        FOPEN.........4100
C.....IF THIS IS THE FIRST CALL, READ AND PROCESS FILE SPECIFICATIONS    FOPEN.........4200
C        FROM "SUTRA.FIL" AND OPEN ALL OUTPUT FILES EXCEPT OBSERVATION   FOPEN.........4300
C        OUTPUT.  OBSERVATION OUTPUT FILES ARE CREATED ON THE SECOND     FOPEN.........4400
C        CALL, AFTER DATASET 8D HAS BEEN READ.                           FOPEN.........4500
      IF (.NOT.ONCEFO) THEN                                              FOPEN.........4600
C                                                                        FOPEN.........4700
C........INITIALIZE FLAGS THAT INDICATE WHETHER CERTAIN ARRAYS           FOPEN.........4800
C           HAVE BEEN ALLOCATED.  THEY ARE USED BY SUBROUTINE NAFU.      FOPEN.........4900
         ALCBCS = .FALSE.                                                FOPEN.........5000
         ALCFIN = .FALSE.                                                FOPEN.........5100
         ALCOBS = .FALSE.                                                FOPEN.........5200
C                                                                        FOPEN.........5300
C........INITIALIZE UNIT NUMBERS AND FILENAMES                           FOPEN.........5400
         K1 = -1                                                         FOPEN.........5500
         K2 = -1                                                         FOPEN.........5600
         K3 = -1                                                         FOPEN.........5700
         K4 = -1                                                         FOPEN.........5800
         K5 = -1                                                         FOPEN.........5900
         K6 = -1                                                         FOPEN.........6000
         K7 = -1                                                         FOPEN.........6100
         K8 = -1                                                         FOPEN.........6200
         K9 = -1                                                         FOPEN.........6300
         K10 = -1                                                        FOPEN.........6400
         K11 = -1                                                        FOPEN.........6500
         K12 = -1                                                        FOPEN.........6600
         K13 = -1                                                        FOPEN.........6700
         DO 20 NF=0,13                                                   FOPEN.........6800
            IUNIT(NF) = -1                                               FOPEN.........6900
            FNAME(NF) = ""                                               FOPEN.........7000
   20    CONTINUE                                                        FOPEN.........7100
C                                                                        FOPEN.........7200
C........SET DEFAULT VALUES FOR THE SMY FILE.  THE DEFAULT FILE WILL     FOPEN.........7300
C           NOT ACTUALLY BE CREATED UNLESS IT IS NEEDED.                 FOPEN.........7400
         K00 = K0 + 1                                                    FOPEN.........7500
         ENDEF = 'SUTRA.SMY'                                             FOPEN.........7600
C                                                                        FOPEN.........7700
C........OPEN FILE UNIT CONTAINING UNIT NUMBERS AND FILE ASSIGNMENTS     FOPEN.........7800
         IU=K0                                                           FOPEN.........7900
         FN=UNAME                                                        FOPEN.........8000
         INQUIRE(FILE=UNAME,EXIST=IS)                                    FOPEN.........8100
         IF (IS) THEN                                                    FOPEN.........8200
            OPEN(UNIT=IU,FILE=UNAME,STATUS='OLD',FORM='FORMATTED',       FOPEN.........8300
     1         IOSTAT=KERR)                                              FOPEN.........8400
            IF(KERR.GT.0) GOTO 9000                                      FOPEN.........8500
         ELSE                                                            FOPEN.........8600
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN.........8700
	      IF (IERROR.NE.0) RETURN
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN.........8800
            GOTO 8000                                                    FOPEN.........8900
         ENDIF                                                           FOPEN.........9000
C RBW
	   k1 = K0
	   ! BCS files are read from sutra.fil which Model Viewer doesn't read.
	   goto 32
C RBW
C                                                                        FOPEN.........9100
C........COUNT HOW MANY BCS FILES LISTED, THEN REWIND.  ALLOCATE AND     FOPEN.........9200
C           INITIALIZE BCS-RELATED ARRAYS IUNIB AND FNAMB, AS WELL AS    FOPEN.........9300
C           NKS, KLIST, AND FNAIN.                                       FOPEN.........9400
         NFBCS = 0                                                       FOPEN.........9500
   30    READ(K0,'(A)',IOSTAT=INERR(1),END=32) INTFIL                    FOPEN.........9600
         IF (INERR(1).NE.0) THEN                                         FOPEN.........9700
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN.........9800
C rbw begin change
	      IF (IERROR.NE.0) RETURN
C rbw end change
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN.........9900
            ERRCOD = 'REA-FIL'                                           FOPEN........10000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     FOPEN........10100
C rbw begin change
	      RETURN
C rbw end change
         END IF                                                          FOPEN........10200
         FT = ''                                                         FOPEN........10300
         READ(INTFIL,*,IOSTAT=INERR(1)) FT                               FOPEN........10400
C rbw begin change
         IF (INERR(1).NE.0) THEN
	      IERROR = 1
	      RETURN
	   ENDIF
C rbw end change
         IF (FT.EQ.'BCS') NFBCS = NFBCS + 1                              FOPEN........10500
         GOTO 30                                                         FOPEN........10600
C rbw begin change
!   32    REWIND(K0)                                                      FOPEN........10700
   32    CONTINUE                                                        FOPEN........10700
         if ( .NOT. Allocated(IUNIB))
C rbw end change
     *     ALLOCATE (IUNIB(NFBCS),FNAMB(NFBCS))                            FOPEN........10800
         ALCBCS = .TRUE.                                                 FOPEN........10900
C rbw begin change
         if ( .NOT. Allocated(NKS))
C rbw end change
     *    ALLOCATE (NKS(2+NFBCS),KLIST(2+NFBCS,20),FNAIN(2+NFBCS,20))     FOPEN........11000
         ALCFIN = .TRUE.                                                 FOPEN........11100
         DO 33 N=1,2+NFBCS                                               FOPEN........11200
            NKS(N) = 0                                                   FOPEN........11300
   33    CONTINUE                                                        FOPEN........11400
         DO 35 NFB=1,NFBCS                                               FOPEN........11500
            IUNIB(NFB) = -1                                              FOPEN........11600
            FNAMB(NFB) = ""                                              FOPEN........11700
   35    CONTINUE                                                        FOPEN........11800
C rbw begin change
	   Goto 301
C rbw end change
C........COMPUTE TOTAL NUMBER OF FILE SPECIFICATIONS.                    FOPEN........11900
         NFSPEC = 12 + NFBCS                                             FOPEN........12000
C                                                                        FOPEN........12100
C........READ IN UNIT NUMBERS AND FILE ASSIGNMENTS.  ASSIGN COMPATIBLE   FOPEN........12200
C           UNIT NUMBERS.  CLOSE UNIT K0.                                FOPEN........12300
         NFB = 0                                                         FOPEN........12400
         DO 90 NF=0,NFSPEC                                               FOPEN........12500
C...........READ A FILE SPECIFICATION                                    FOPEN........12600
            READ(K0,'(A)',IOSTAT=INERR(1),END=99) INTFIL                 FOPEN........12700
            IF (INERR(1).NE.0) THEN                                      FOPEN........12800
               CALL NAFU(K00,0,ENDEF,IERROR)                                    FOPEN........12900
C rbw begin change
	         IF (IERROR.NE.0) RETURN
C rbw end change
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN........13000
               ERRCOD = 'REA-FIL'                                        FOPEN........13100
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  FOPEN........13200
C rbw begin change
	         RETURN
C rbw end change
            END IF                                                       FOPEN........13300
            IF (VERIFY(INTFIL,' ').EQ.0) GOTO 99                         FOPEN........13400
            READ(INTFIL,*,IOSTAT=INERR(1)) FT, IU, FN                    FOPEN........13500
            IF (INERR(1).NE.0) THEN                                      FOPEN........13600
               CALL NAFU(K00,0,ENDEF,IERROR)                                    FOPEN........13700
C rbw begin change
	         IF (IERROR.NE.0) RETURN
C rbw end change
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN........13800
               ERRCOD = 'REA-FIL'                                        FOPEN........13900
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  FOPEN........14000
C rbw begin change
	         RETURN
C rbw end change
            END IF                                                       FOPEN........14100
C...........CHECK FOR ILLEGAL SPECIFICATIONS                             FOPEN........14200
            IF (FN.EQ.UNAME) THEN                                        FOPEN........14300
               CALL NAFU(K00,0,ENDEF,IERROR)                                    FOPEN........14400
C rbw begin change
	         IF (IERROR.NE.0) RETURN
C rbw end change
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN........14500
               ERRCOD = 'FIL-9'                                          FOPEN........14600
               CHERR(1) = UNAME                                          FOPEN........14700
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  FOPEN........14800
C rbw begin change
	         RETURN
C rbw end change
            END IF                                                       FOPEN........14900
C...........IF THE SPECIFIED UNIT NUMBER IS LESS THAN IUNMIN,            FOPEN........15000
C              SET IT TO IUNMIN                                          FOPEN........15100
            IU = MAX(IU, IUNMIN)                                         FOPEN........15200
C...........STORE THE FILE INFORMATION, CHECKING FOR INVALID AND         FOPEN........15300
C              REPEATED FILE TYPE SPECIFICATIONS AND ASSIGNING UNIT      FOPEN........15400
C              NUMBERS TO NON-OBSERVATION FILES ALONG THE WAY            FOPEN........15500
            IF (FT.EQ.'BCS') THEN                                        FOPEN........15600
               CALL NAFU(IU,0,FN,IERROR)                                        FOPEN........15700
C rbw begin change
	         IF (IERROR.NE.0) RETURN
C rbw end change
               IUNIT(9) = IU                                             FOPEN........15800
               FNAME(9) = FN                                             FOPEN........15900
               NFB = NFB + 1                                             FOPEN........16000
               IUNIB(NFB) = IU                                           FOPEN........16100
               FNAMB(NFB) = FN                                           FOPEN........16200
               GOTO 60                                                   FOPEN........16300
            END IF                                                       FOPEN........16400
            DO 50 NFT=0,13                                               FOPEN........16500
               IF (FT.EQ.FTSTR(NFT)) THEN                                FOPEN........16600
                  IF (IUNIT(NFT).EQ.-1) THEN                             FOPEN........16700
                     IF ((NFT.LE.6).OR.(NFT.GE.9)) CALL 
     1               NAFU(IU,0,FN,IERROR)                                FOPEN........16800
C rbw begin change
	               IF (IERROR.NE.0) RETURN
C rbw end change
                     IUNIT(NFT) = IU                                     FOPEN........16900
                     FNAME(NFT) = FN                                     FOPEN........17000
                     GOTO 60                                             FOPEN........17100
                  ELSE                                                   FOPEN........17200
                     CALL NAFU(K00,0,ENDEF,IERROR)                              FOPEN........17300
C rbw begin change
	               IF (IERROR.NE.0) RETURN
C rbw end change
                     OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')          FOPEN........17400
                     ERRCOD = 'FIL-6'                                    FOPEN........17500
                     CHERR(1) = UNAME                                    FOPEN........17600
                     CHERR(2) = FT                                       FOPEN........17700
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            FOPEN........17800
C rbw begin change
	               RETURN
C rbw end change
                  END IF                                                 FOPEN........17900
               END IF                                                    FOPEN........18000
   50       CONTINUE                                                     FOPEN........18100
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN........18200
C rbw begin change
	      IF (IERROR.NE.0) RETURN
C rbw end change
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN........18300
            ERRCOD = 'FIL-5'                                             FOPEN........18400
            CHERR(1) = UNAME                                             FOPEN........18500
            CHERR(2) = FT                                                FOPEN........18600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     FOPEN........18700
C rbw begin change
	      RETURN
C rbw end change
   60       CONTINUE                                                     FOPEN........18800
   90    CONTINUE                                                        FOPEN........18900
   99    CLOSE(K0)                                                       FOPEN........19000
C                                                                        FOPEN........19100
C........OPEN THE SMY FILE.                                              FOPEN........19200
C                                                                        FOPEN........19300
C........IF NO SMY SPECIFICATION, USE THE DEFAULT.                       FOPEN........19400
         IF (IUNIT(0).EQ.-1) THEN                                        FOPEN........19500
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN........19600
C rbw begin change
	      IF (IERROR.NE.0) RETURN
C rbw end change
            IUNIT(0) = K00                                               FOPEN........19700
            FNAME(0) = ENDEF                                             FOPEN........19800
         END IF                                                          FOPEN........19900
         IU = IUNIT(0)                                                   FOPEN........20000
         FN = FNAME(0)                                                   FOPEN........20100
         OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',IOSTAT=KERR)              FOPEN........20200
C........IN CASE OF ERROR WHILE OPENING SMY FILE, WRITE ERROR            FOPEN........20300
C           MESSAGE TO DEFAULT FILE                                      FOPEN........20400
         IF (KERR.GT.0) THEN                                             FOPEN........20500
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN........20600
C rbw begin change
	      IF (IERROR.NE.0) RETURN
C rbw end change
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN........20700
            GOTO 9000                                                    FOPEN........20800
         END IF                                                          FOPEN........20900
C........SET K00 AND ENAME                                               FOPEN........21000
         K00 = IU                                                        FOPEN........21100
         ENAME = FN                                                      FOPEN........21200
C                                                                        FOPEN........21300
C........CHECK FOR REPEATED FILENAMES (EXCEPT OBS AND OBC FILES)         FOPEN........21400
C           AND MISSING SPECIFICATIONS FOR REQUIRED FILE TYPES           FOPEN........21500
         DO 260 NF=0,13                                                  FOPEN........21600
            IF ((NF.GE.7).OR.(NF.LE.9)) CYCLE                            FOPEN........21700
            IF (IUNIT(NF).EQ.-1) THEN                                    FOPEN........21800
               IF ((NF.GE.1).AND.(NF.LE.3)) THEN                         FOPEN........21900
                  ERRCOD = 'FIL-7'                                       FOPEN........22000
                  CHERR(1) = UNAME                                       FOPEN........22100
                  CHERR(2) = FTSTR(NF)                                   FOPEN........22200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               FOPEN........22300
C rbw begin change
	            RETURN
C rbw end change
               ELSE                                                      FOPEN........22400
                  CYCLE                                                  FOPEN........22500
               END IF                                                    FOPEN........22600
            END IF                                                       FOPEN........22700
            DO 250 NF2=NF+1,13                                           FOPEN........22800
               IF ((NF2.GE.7).OR.(NF2.LE.9)) CYCLE                       FOPEN........22900
               IF (FNAME(NF2).EQ.FNAME(NF)) THEN                         FOPEN........23000
                  ERRCOD = 'FIL-4'                                       FOPEN........23100
                  CHERR(1) = UNAME                                       FOPEN........23200
                  CHERR(2) = FNAME(NF)                                   FOPEN........23300
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               FOPEN........23400
C rbw begin change
	            RETURN
C rbw end change
               END IF                                                    FOPEN........23500
  250       CONTINUE                                                     FOPEN........23600
            DO 255 NFB=1,NFBCS                                           FOPEN........23700
               IF (FNAME(NFB).EQ.FNAME(NF)) THEN                         FOPEN........23800
                  ERRCOD = 'FIL-4'                                       FOPEN........23900
                  CHERR(1) = UNAME                                       FOPEN........24000
                  CHERR(2) = FNAME(NF)                                   FOPEN........24100
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               FOPEN........24200
C rbw begin change
	            RETURN
C rbw end change
               END IF                                                    FOPEN........24300
  255       CONTINUE                                                     FOPEN........24400
  260    CONTINUE                                                        FOPEN........24500
         DO 280 NFB=1,NFBCS                                              FOPEN........24600
            DO 270 NFB2=NFB+1,NFBCS                                      FOPEN........24700
               IF (FNAMB(NFB2).EQ.FNAMB(NFB)) THEN                       FOPEN........24800
                  ERRCOD = 'FIL-4'                                       FOPEN........24900
                  CHERR(1) = UNAME                                       FOPEN........25000
                  CHERR(2) = FNAMB(NF)                                   FOPEN........25100
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               FOPEN........25200
C rbw begin change
	            RETURN
C rbw end change
               END IF                                                    FOPEN........25300
  270       CONTINUE                                                     FOPEN........25400
  280    CONTINUE                                                        FOPEN........25500
C                                                                        FOPEN........25600
C........SET UNIT NUMBERS K1 - K13.  (K00 HAS BEEN SET PREVIOUSLY.)      FOPEN........25700
         K1=IUNIT(1)                                                     FOPEN........25800
         K2=IUNIT(2)                                                     FOPEN........25900
         K3=IUNIT(3)                                                     FOPEN........26000
         K4=IUNIT(4)                                                     FOPEN........26100
         K5=IUNIT(5)                                                     FOPEN........26200
         K6=IUNIT(6)                                                     FOPEN........26300
         K7=IUNIT(7)                                                     FOPEN........26400
         K8=IUNIT(8)                                                     FOPEN........26500
         K9=IUNIT(9)                                                     FOPEN........26600
         K10=IUNIT(10)                                                   FOPEN........26700
         K11=IUNIT(11)                                                   FOPEN........26800
         K12=IUNIT(12)                                                   FOPEN........26900
         K13=IUNIT(13)                                                   FOPEN........27000
C                                                                        FOPEN........27100
C........CHECK FOR EXISTENCE OF INPUT FILES AND OPEN INPUT AND OUTPUT    FOPEN........27200
C           FILES (EXCEPT SMY, OBS, AND OBC)                             FOPEN........27300
         DO 300 NF=1,13                                                  FOPEN........27400
            IF ((NF.EQ.7).OR.(NF.EQ.8)) CYCLE                            FOPEN........27500
            IU=IUNIT(NF)                                                 FOPEN........27600
            FN=FNAME(NF)                                                 FOPEN........27700
            IF (IU.EQ.-1) GOTO 300                                       FOPEN........27800
            IF (NF.LE.2) THEN                                            FOPEN........27900
               INQUIRE(FILE=FN,EXIST=IS)                                 FOPEN........28000
               IF(IS) THEN                                               FOPEN........28100
                  OPEN(UNIT=IU,FILE=FN,STATUS='OLD',FORM='FORMATTED',    FOPEN........28200
     1               IOSTAT=KERR)                                        FOPEN........28300
               ELSE                                                      FOPEN........28400
                  GOTO 8000                                              FOPEN........28500
               ENDIF                                                     FOPEN........28600
            ELSE IF (NF.EQ.9) THEN                                       FOPEN........28700
               DO 290 NFB=1,NFBCS                                        FOPEN........28800
                  IU = IUNIB(NFB)                                        FOPEN........28900
                  FN = FNAMB(NFB)                                        FOPEN........29000
                  INQUIRE(FILE=FN,EXIST=IS)                              FOPEN........29100
                  IF(IS) THEN                                            FOPEN........29200
                     OPEN(UNIT=IU,FILE=FN,STATUS='OLD',FORM='FORMATTED', FOPEN........29300
     1                  IOSTAT=KERR)                                     FOPEN........29400
                  ELSE                                                   FOPEN........29500
                     GOTO 8000                                           FOPEN........29600
                  ENDIF                                                  FOPEN........29700
  290          CONTINUE                                                  FOPEN........29800
            ELSE                                                         FOPEN........29900
               OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',FORM='FORMATTED',   FOPEN........30000
     1            IOSTAT=KERR)                                           FOPEN........30100
            ENDIF                                                        FOPEN........30200
            IF(KERR.GT.0) GOTO 9000                                      FOPEN........30300
  300    CONTINUE                                                        FOPEN........30400
! rbw begin change
  301    continue
! rbw end change
C                                                                        FOPEN........30500
C........SET FLAG TO INDICATE THAT FIRST CALL IS COMPLETED, THEN RETURN  FOPEN........30600
         ONCEFO = .TRUE.                                                 FOPEN........30700
         RETURN                                                          FOPEN........30800
C                                                                        FOPEN........30900
      ELSE                                                               FOPEN........31000
C                                                                        FOPEN........31100
C........ALLOCATE AND INITIALIZE OBSERVATION-RELATED UNIT NUMBERS        FOPEN........31200
C           AND FILENAMES                                                FOPEN........31300
! rbw begin change
         if (.not. Allocated(IUNIO))
! rbw end change
     *    ALLOCATE(IUNIO(NFLOMX),FNAMO(NFLOMX))                           FOPEN........31400
         ALCOBS = .TRUE.                                                 FOPEN........31500
         DO 330 NFO=1,NFLOMX                                             FOPEN........31600
            IUNIO(NFO) = -1                                              FOPEN........31700
            FNAMO(NFO) = ""                                              FOPEN........31800
  330    CONTINUE                                                        FOPEN........31900
C RBW
         RETURN
C RBW
C                                                                        FOPEN........32000
C........OPEN OBS AND OBC FILES, AUTOMATICALLY GENERATING UNIT NUMBERS   FOPEN........32100
C           AND FILENAMES                                                FOPEN........32200
C                                                                        FOPEN........32300
C........LOOP OVER THE TWO FILE TYPES                                    FOPEN........32400
         DO 400 NF=7,8                                                   FOPEN........32500
C...........IF NO FILE SPECIFICATION OF THIS TYPE, MOVE ON               FOPEN........32600
            IF (IUNIT(NF).EQ.-1) CYCLE                                   FOPEN........32700
C...........DETERMINE LENGTH OF THE SPECIFIED FILENAME AND ITS ROOT      FOPEN........32800
            LNAME = LEN_TRIM(FNAME(NF))                                  FOPEN........32900
            LROOT = SCAN(FNAME(NF),'.',BACK=.TRUE.) - 1                  FOPEN........33000
C...........SET THE ROOT NAME AND EXTENSION THAT WILL BE USED FOR FILES  FOPEN........33100
C              OF THIS TYPE                                              FOPEN........33200
            IF (LROOT.NE.-1) THEN                                        FOPEN........33300
               IF (LROOT.NE.0) THEN                                      FOPEN........33400
                  FNROOT = FNAME(NF)(1:LROOT)                            FOPEN........33500
               ELSE                                                      FOPEN........33600
                  FNROOT = "SUTRA"                                       FOPEN........33700
               END IF                                                    FOPEN........33800
               IF (LROOT.NE.LNAME-1) THEN                                FOPEN........33900
                  FNEXTN = FNAME(NF)(LROOT+1:LNAME)                      FOPEN........34000
               ELSE                                                      FOPEN........34100
                  FNEXTN = "." // FTSTR(NF)                              FOPEN........34200
               END IF                                                    FOPEN........34300
            ELSE                                                         FOPEN........34400
               IF (LNAME.NE.0) THEN                                      FOPEN........34500
                  FNROOT = FNAME(NF)                                     FOPEN........34600
               ELSE                                                      FOPEN........34700
                  FNROOT = "SUTRA"                                       FOPEN........34800
               END IF                                                    FOPEN........34900
               FNEXTN = "." // FTSTR(NF)                                 FOPEN........35000
            END IF                                                       FOPEN........35100
C...........INITIALIZE UNIT NUMBER                                       FOPEN........35200
            IUNEXT = IUNIT(NF)                                           FOPEN........35300
C...........LOOP OVER OBSERVATION OUTPUT FILES                           FOPEN........35400
            DO 380 J=1,NFLOMX                                            FOPEN........35500
               JM1 = J - 1                                               FOPEN........35600
C..............IF FILE IS NOT OF THE TYPE CURRENTLY BEING PROCESSED,     FOPEN........35700
C                 SKIP FILE                                              FOPEN........35800
               IF (OFP(J)%FRMT.NE.FTSTR(NF)) CYCLE                       FOPEN........35900
C..............CONSTRUCT FILENAME FROM ROOT NAME, SCHEDULE NAME,         FOPEN........36000
C                 AND EXTENSION                                          FOPEN........36100
               IF ((ISSTRA.NE.1).AND.                                    FOPEN........36200
     1             (SCHDLS(OFP(J)%ISCHED)%NAME.NE."-")) THEN             FOPEN........36300
                  FN = TRIM(FNROOT) // "_"                               FOPEN........36400
     1               // TRIM(SCHDLS(OFP(J)%ISCHED)%NAME) // FNEXTN       FOPEN........36500
               ELSE                                                      FOPEN........36600
                  FN = TRIM(FNROOT) // FNEXTN                            FOPEN........36700
               END IF                                                    FOPEN........36800
C..............CHECK FOR DUPLICATE FILENAME AMONG NON-OBSERVATION        FOPEN........36900
C                 FILES                                                  FOPEN........37000
               DO 350 NFF=0,13                                           FOPEN........37100
                  IF ((NFF.GE.7).OR.(NFF.LE.9)) CYCLE                    FOPEN........37200
                  IF (FN.EQ.FNAME(NFF)) THEN                             FOPEN........37300
                     ERRCOD = 'FIL-4'                                    FOPEN........37400
                     CHERR(1) = UNAME                                    FOPEN........37500
                     CHERR(2) = FN                                       FOPEN........37600
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            FOPEN........37700
C rbw begin change
	               RETURN
C rbw end change
                  END IF                                                 FOPEN........37800
  350          CONTINUE                                                  FOPEN........37900
               DO 352 NFB=1,NFBCS                                        FOPEN........38000
                  IF (FN.EQ.FNAMB(NFB)) THEN                             FOPEN........38100
                     ERRCOD = 'FIL-4'                                    FOPEN........38200
                     CHERR(1) = UNAME                                    FOPEN........38300
                     CHERR(2) = FN                                       FOPEN........38400
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            FOPEN........38500
C rbw begin change
	               RETURN
C rbw end change
                  END IF                                                 FOPEN........38600
  352          CONTINUE                                                  FOPEN........38700
C..............CHECK FOR DUPLICATE FILENAME AMONG PREVIOUSLY DEFINED     FOPEN........38800
C                 OBSERVATION OUTPUT FILES                               FOPEN........38900
               DO 355 NJ=1,J-1                                           FOPEN........39000
                  IF (FN.EQ.FNAMO(NJ)) THEN                              FOPEN........39100
                     ERRCOD = 'FIL-4'                                    FOPEN........39200
                     CHERR(1) = UNAME                                    FOPEN........39300
                     CHERR(2) = FN                                       FOPEN........39400
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            FOPEN........39500
C rbw begin change
	               RETURN
C rbw end change
                  END IF                                                 FOPEN........39600
  355          CONTINUE                                                  FOPEN........39700
C..............ASSIGN NEXT AVAILABLE UNIT NUMBER, RECORD FILE            FOPEN........39800
C                 INFORMATION, AND OPEN THE FILE                         FOPEN........39900
               CALL NAFU(IUNEXT,JM1,FN,IERROR)                                  FOPEN........40000
C rbw begin change
	         IF (IERROR.NE.0) RETURN
C rbw end change
               IU = IUNEXT                                               FOPEN........40100
               IUNIO(J) = IU                                             FOPEN........40200
               FNAMO(J) = FN                                             FOPEN........40300
               INQUIRE(UNIT=IU, OPENED=IS)                               FOPEN........40400
               OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',FORM='FORMATTED',   FOPEN........40500
     1            IOSTAT=KERR)                                           FOPEN........40600
               IF(KERR.GT.0) GOTO 9000                                   FOPEN........40700
  380       CONTINUE                                                     FOPEN........40800
  400    CONTINUE                                                        FOPEN........40900
C                                                                        FOPEN........41000
C........SECOND CALL IS COMPLETED, SO RETURN                             FOPEN........41100
         RETURN                                                          FOPEN........41200
C                                                                        FOPEN........41300
      END IF                                                             FOPEN........41400
C                                                                        FOPEN........41500
 8000 CONTINUE                                                           FOPEN........41600
C.....GENERATE ERROR                                                     FOPEN........41700
      ERRCOD = 'FIL-1'                                                   FOPEN........41800
      CHERR(1) = UNAME                                                   FOPEN........41900
      CHERR(2) = FN                                                      FOPEN........42000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           FOPEN........42100
C rbw begin change
	RETURN
C rbw end change
C                                                                        FOPEN........42200
 9000 CONTINUE                                                           FOPEN........42300
C.....GENERATE ERROR                                                     FOPEN........42400
      ERRCOD = 'FIL-2'                                                   FOPEN........42500
      CHERR(1) = UNAME                                                   FOPEN........42600
      CHERR(2) = FN                                                      FOPEN........42700
      INERR(1) = IU                                                      FOPEN........42800
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           FOPEN........42900
C rbw begin change
	RETURN
C rbw end change
C                                                                        FOPEN........43000
      END                                                                FOPEN........43100
C                                                                        FOPEN........43200
C     FUNCTION          F  R  C  S  T  P           SUTRA VERSION 2.2     FRCSTP.........100
C                                                                        FRCSTP.........200
C *** PURPOSE :                                                          FRCSTP.........300
C ***  TO RETURN THE FRACTIONAL TIME STEP FOR A GIVEN TIME.  IF THE      FRCSTP.........400
C ***  SPECIFIED TIME IS GREATER THAN THE MAXIMUM, A VALUE OF            FRCSTP.........500
C ***  +HUGE(1D0) (THE LARGEST NUMBER THAT CAN BE REPRESENTED IN DOUBLE  FRCSTP.........600
C ***  PRECISION) IS RETURNED.  IF THE SPECIFIED TIME IS LESS THAN       FRCSTP.........700
C ***  TSTART, A VALUE OF -HUGE(1D0) IS RETURNED.  IF THE TIME STEP      FRCSTP.........800
C ***  SCHEDULE HAS NOT YET BEEN DEFINED, A VALUE OF ZERO IS RETURNED.   FRCSTP.........900
C                                                                        FRCSTP........1000
      DOUBLE PRECISION FUNCTION FRCSTP(TIME)                             FRCSTP........1100
      USE LLDEF                                                          FRCSTP........1200
      USE SCHDEF                                                         FRCSTP........1300
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FRCSTP........1400
      TYPE (LLD), POINTER :: DEN                                         FRCSTP........1500
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    FRCSTP........1600
C                                                                        FRCSTP........1700
      IF (ISCHTS.EQ.0) THEN                                              FRCSTP........1800
         FRCSTP = DNINT(DBLE(0))                                         FRCSTP........1900
         RETURN                                                          FRCSTP........2000
      END IF                                                             FRCSTP........2100
C                                                                        FRCSTP........2200
      NSMAX = SCHDLS(ISCHTS)%LLEN - 1                                    FRCSTP........2300
C                                                                        FRCSTP........2400
      DEN => SCHDLS(ISCHTS)%SLIST                                        FRCSTP........2500
      T1 = DEN%DVALU1                                                    FRCSTP........2600
      IF (TIME.EQ.T1) THEN                                               FRCSTP........2700
         FRCSTP = DNINT(DBLE(0))                                         FRCSTP........2800
         RETURN                                                          FRCSTP........2900
      ELSE IF (TIME.LT.T1) THEN                                          FRCSTP........3000
         FRCSTP = -HUGE(1D0)                                             FRCSTP........3100
         RETURN                                                          FRCSTP........3200
      END IF                                                             FRCSTP........3300
      DO 100 NS=1,NSMAX                                                  FRCSTP........3400
         DEN => DEN%NENT                                                 FRCSTP........3500
         T2 = DEN%DVALU1                                                 FRCSTP........3600
         IF (TIME.EQ.T2) THEN                                            FRCSTP........3700
            FRCSTP = DNINT(DBLE(NS))                                     FRCSTP........3800
            RETURN                                                       FRCSTP........3900
         ELSE IF (TIME.LT.T2) THEN                                       FRCSTP........4000
            WT = (TIME - T1)/(T2 - T1)                                   FRCSTP........4100
            S1 = DBLE(NS - 1)                                            FRCSTP........4200
            S2 = DBLE(NS)                                                FRCSTP........4300
            FRCSTP = (1D0 - WT)*S1 + WT*S2                               FRCSTP........4400
            RETURN                                                       FRCSTP........4500
         END IF                                                          FRCSTP........4600
  100 CONTINUE                                                           FRCSTP........4700
      FRCSTP = +HUGE(1D0)                                                FRCSTP........4800
C                                                                        FRCSTP........4900
      RETURN                                                             FRCSTP........5000
      END                                                                FRCSTP........5100
C                                                                        FRCSTP........5200
C     SUBROUTINE        G  L  O  B  A  N           SUTRA VERSION 2.2     GLOBAN.........100
C                                                                        GLOBAN.........200
C *** PURPOSE :                                                          GLOBAN.........300
C ***  TO ASSEMBLE RESULTS OF ELEMENTWISE INTEGRATIONS INTO              GLOBAN.........400
C ***  A GLOBAL BANDED MATRIX AND GLOBAL VECTOR FOR BOTH                 GLOBAN.........500
C ***  FLOW AND TRANSPORT EQUATIONS.                                     GLOBAN.........600
C                                                                        GLOBAN.........700
C     SUBROUTINE        G  L  O  C  O  L           SUTRA VERSION 2.2     GLOCOL.........100
C                                                                        GLOCOL.........200
C *** PURPOSE :                                                          GLOCOL.........300
C ***  TO ASSEMBLE RESULTS OF ELEMENTWISE INTEGRATIONS INTO              GLOCOL.........400
C ***  A GLOBAL "SLAP COLUMN"-FORMAT MATRIX AND GLOBAL VECTOR            GLOCOL.........500
C ***  FOR BOTH FLOW AND TRANSPORT EQUATIONS.                            GLOCOL.........600
C                                                                        GLOCOL.........700
C     SUBROUTINE        I  N  D  A  T  0           SUTRA VERSION 2.2     INDAT0.........100
C                                                                        INDAT0.........200
C *** PURPOSE :                                                          INDAT0.........300
C ***  TO INPUT, OUTPUT, AND ORGANIZE A PORTION OF THE INP FILE          INDAT0.........400
C ***  INPUT DATA (DATASETS 5 THROUGH 7)                                 INDAT0.........500
C                                                                        INDAT0.........600
      SUBROUTINE INDAT0(IERROR)                                                INDAT0.........700
      USE EXPINT                                                         INDAT0.........800
      USE LLDEF                                                          INDAT0.........900
      USE SCHDEF                                                         INDAT0........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT0........1100
      CHARACTER INTFIL*1000                                              INDAT0........1200
      CHARACTER*10 ADSMOD                                                INDAT0........1300
      CHARACTER SOLWRD(0:10)*10,SOLNAM(0:10)*40                          INDAT0........1400
      CHARACTER*10 CSOLVP,CSOLVU                                         INDAT0........1500
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:13)                    INDAT0........1600
      CHARACTER SCHTYP*12, CDUM10*10, CDUM80*80                          INDAT0........1700
      CHARACTER*10 SCHNAM                                                INDAT0........1800
      CHARACTER CTICS*20, CREFT*8                                        INDAT0........1900
      DIMENSION INERR(10),RLERR(10)                                      INDAT0........2000
      DIMENSION KTYPE(2)                                                 INDAT0........2100
      ALLOCATABLE :: ISLIST(:), TLIST(:), DTMP1(:), DTMP2(:)             INDAT0........2200
      CHARACTER*8 VERNUM, VERNIN                                         INDAT0........2300
      LOGICAL, ALLOCATABLE :: SBASED(:), ELAPSD(:)                       INDAT0........2400
      LOGICAL TSYES                                                      INDAT0........2500
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT0........2600
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,           INDAT0........2700
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE                                       INDAT0........2800
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT0........2900
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            INDAT0........3000
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        INDAT0........3100
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           INDAT0........3200
      COMMON /FNAMES/ UNAME,FNAME                                        INDAT0........3300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 INDAT0........3400
     1   K10,K11,K12,K13                                                 INDAT0........3500
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INDAT0........3600
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      INDAT0........3700
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            INDAT0........3800
      COMMON /ITSOLR/ TOLP,TOLU                                          INDAT0........3900
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                INDAT0........4000
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            INDAT0........4100
      COMMON /MODSOR/ ADSMOD                                             INDAT0........4200
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    INDAT0........4300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT0........4400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT0........4500
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       INDAT0........4600
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           INDAT0........4700
      COMMON /SOLVN/ NSLVRS                                              INDAT0........4800
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT0........4900
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      INDAT0........5000
      COMMON /VER/ VERNUM, VERNIN                                        INDAT0........5100
C                                                                        INDAT0........5200
      INSTOP=0                                                           INDAT0........5300
C                                                                        INDAT0........5400
C.....INPUT DATASET 5: NUMERICAL CONTROL PARAMETERS                      INDAT0........5500
      ERRCOD = 'REA-INP-5'                                               INDAT0........5600
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT0........5700
C rbw begin change
	IF (IERROR.NE.0) RETURN
C rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) UP,GNUP,GNUU                        INDAT0........5800
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0........5800
	   RETURN
	ENDIF
!      IF(ME.EQ.-1) WRITE(K3,70) UP,GNUP,GNUU                             INDAT0........6000
!   70 FORMAT(////11X,'N U M E R I C A L   C O N T R O L   D A T A'//     INDAT0........6100
!     1   11X,F15.5,5X,'"UPSTREAM WEIGHTING" FACTOR'/                     INDAT0........6200
!     2   11X,1PE15.4,5X,'SPECIFIED PRESSURE BOUNDARY CONDITION FACTOR'/  INDAT0........6300
!     3   11X,1PE15.4,5X,'SPECIFIED CONCENTRATION BOUNDARY CONDITION ',   INDAT0........6400
!     4   'FACTOR')                                                       INDAT0........6500
!      IF(ME.EQ.+1) WRITE(K3,80) UP,GNUP,GNUU                             INDAT0........6600
!   80 FORMAT(////11X,'N U M E R I C A L   C O N T R O L   D A T A'//     INDAT0........6700
!     1   11X,F15.5,5X,'"UPSTREAM WEIGHTING" FACTOR'/                     INDAT0........6800
!     2   11X,1PE15.4,5X,'SPECIFIED PRESSURE BOUNDARY CONDITION FACTOR'/  INDAT0........6900
!     3   11X,1PE15.4,5X,'SPECIFIED TEMPERATURE BOUNDARY CONDITION ',     INDAT0........7000
!     4   'FACTOR')                                                       INDAT0........7100
C                                                                        INDAT0........7200
C.....INPUT DATASET 6: TEMPORAL CONTROL AND SOLUTION CYCLING DATA        INDAT0........7300
C RBW 
      TICS = 0
	GOTO 100
!     DON'T READ THE ICS FILE.	
C RBW
      ERRCOD = 'REA-ICS-1'                                               INDAT0........7400
      CALL READIF_22(IERROR,K2, 0, INTFIL, ERRCOD)                                 INDAT0........7500
C rbw begin change
	IF (IERROR.NE.0) RETURN
C rbw end change
      IF (IREAD.EQ.+1) THEN                                              INDAT0........7600
         READ(INTFIL,*,IOSTAT=INERR(1)) TICS                             INDAT0........7700
      ELSE                                                               INDAT0........7800
         READ(INTFIL,*,IOSTAT=INERR(1)) TICS,DUM,DUM,IDUM,TICS0          INDAT0........7900
      END IF                                                             INDAT0........8000
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0........7600
	   RETURN
	ENDIF
      REWIND(K2)                                                         INDAT0........8200
C RBW
  100 CONTINUE
C RBW
!      WRITE(CTICS,'(E20.10)') TICS                                       INDAT0........8300
!      WRITE(K3,120)                                                      INDAT0........8400
!  120 FORMAT('1'////11X,'T E M P O R A L   C O N T R O L   A N D   ',    INDAT0........8500
!     1   'S O L U T I O N   C Y C L I N G   D A T A')                    INDAT0........8600
      ERRCOD = 'REA-INP-6'                                               INDAT0........8700
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT0........8800
C rbw begin change
	IF (IERROR.NE.0) RETURN
C rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) NSCH                                INDAT0........8900
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0........8500
	   RETURN
	ENDIF
C.....SET NUMBER OF USUAL AUTOMATIC SCHEDULES THAT WILL BE CREATED.      INDAT0........9100
C        CURRENTLY, THESE ARE "STEPS_1&UP", "STEP_0", AND "STEP_1".      INDAT0........9200
      NSCHAU = 3                                                         INDAT0........9300
C.....IF VERSION 2.0 INPUT, RE-READ DATASET IN OLD FORMAT.  ELSE IF      INDAT0........9400
C        NSCH>0, RE-READ FIRST LINE OF DATASET IN NEW FORMAT.  ELSE      INDAT0........9500
C        IF NSCH<0, OR NSCH=0 AND TRANSPORT IS NOT STEADY-STATE,         INDAT0........9600
C        GENERATE ERROR.                                                 INDAT0........9700
      IF (VERNIN.EQ."2.0") THEN                                          INDAT0........9800
C........READ TEMPORAL AND SOLUTION CYCLING CONTROLS.                    INDAT0........9900
         READ(INTFIL,*,IOSTAT=INERR(1)) ITMAX,DELT,TMAX,ITCYC,DTMULT,    INDAT0.......10000
     1      DTMAX,NPCYC,NUCYC                                            INDAT0.......10100
C rbw begin change
         IF (INERR(1).NE.0) THEN 
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0........9400
	     RETURN
	   ENDIF
C rbw end change
C........ERROR CHECKING SPECIFIC TO OLD FORMAT.                          INDAT0.......10300
         IF (DELT.GT.DTMAX) THEN                                         INDAT0.......10400
            ERRCOD = 'INP-6-3'                                           INDAT0.......10500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     INDAT0.......10600
C rbw begin change
	      RETURN
C rbw end change
         END IF                                                          INDAT0.......10700
      ELSE IF (NSCH.GT.0) THEN                                           INDAT0.......10800
C........READ FIRST LINE OF DATASET.                                     INDAT0.......10900
         READ(INTFIL,*,IOSTAT=INERR(1)) NSCH, NPCYC, NUCYC               INDAT0.......11000
C rbw begin change
         IF (INERR(1).NE.0) THEN 
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0........9400
	     RETURN
	   ENDIF
C rbw end change
      ELSE IF (NSCH.LT.0) THEN                                           INDAT0.......11200
            ERRCOD = 'INP-6-8'                                           INDAT0.......11300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......11400
C rbw begin change
	      RETURN
C rbw end change
      ELSE                                                               INDAT0.......11500
         IF (ISSTRA.EQ.0) THEN                                           INDAT0.......11600
            ERRCOD = 'INP-6-13'                                          INDAT0.......11700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......11800
C rbw begin change
	      RETURN
C rbw end change
         END IF                                                          INDAT0.......11900
         NPCYC = 1                                                       INDAT0.......12000
         NUCYC = 1                                                       INDAT0.......12100
      END IF                                                             INDAT0.......12200
C.....ERROR CHECKING COMMON TO BOTH FORMATS.                             INDAT0.......12300
      IF (NPCYC.LT.1.OR.NUCYC.LT.1) THEN                                 INDAT0.......12400
         ERRCOD = 'INP-6-1'                                              INDAT0.......12500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......12600
C rbw begin change
	   RETURN
C rbw end change
      ELSE IF (NPCYC.NE.1.AND.NUCYC.NE.1) THEN                           INDAT0.......12700
         ERRCOD = 'INP-6-2'                                              INDAT0.......12800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......12900
C rbw begin change
	   RETURN
C rbw end change
      END IF                                                             INDAT0.......13000
C.....IF TRANSPORT IS STEADY-STATE, SKIP THROUGH THE REST OF THE         INDAT0.......13100
C        DATASET AND CREATE A TRIVIAL "TIME_STEPS" SCHEDULE.             INDAT0.......13200
C        (NOTE THAT IF TRANSPORT IS STEADY-STATE, SO IS FLOW.)           INDAT0.......13300
C        EVENTUALLY, IN ADDITION, THE USUAL AUTOMATIC SCHEDULES WILL     INDAT0.......13400
C        BE CREATED.                                                     INDAT0.......13500
      IF (ISSTRA.EQ.1) THEN                                              INDAT0.......13600
         TSTART = TICS                                                   INDAT0.......13700
         IF (VERNIN.NE."2.0") THEN                                       INDAT0.......13800
            ERRCOD = 'REA-INP-6'                                         INDAT0.......13900
            CDUM10 = ''                                                  INDAT0.......14000
            DO WHILE (CDUM10.NE.'-')                                     INDAT0.......14100
               CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                        INDAT0.......14200
C rbw begin change
	         IF (IERROR.NE.0) RETURN
C rbw end change
               IF (INERR(1).NE.0) THEN
                  CALL SUTERR(ERRCOD, CHERR, INERR,                      INDAT0.......13500
     1              RLERR,IERROR)                                                 INDAT0.......13600
	            RETURN
	         ENDIF
               READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                     INDAT0.......14500
            END DO                                                       INDAT0.......14600
            DELT = MAX(1D-1*DABS(TSTART), 1D0)                           INDAT0.......14700
         END IF                                                          INDAT0.......14800
         NSCH = 1 + NSCHAU                                               INDAT0.......14900
         ALLOCATE(SCHDLS(NSCH))                                          INDAT0.......15000
         DO 135 NS=1,NSCH                                                INDAT0.......15100
            ALLOCATE(SCHDLS(NS)%SLIST, SCHDLS(NS)%SLAST)                 INDAT0.......15200
            SCHDLS(NS)%LLEN = 0                                          INDAT0.......15300
  135    CONTINUE                                                        INDAT0.......15400
         ISCHTS = 1                                                      INDAT0.......15500
         SCHDLS(ISCHTS)%NAME = 'TIME_STEPS'                              INDAT0.......15600
         NS1UP = 2                                                       INDAT0.......15700
         SCHDLS(NS1UP)%NAME = 'STEPS_1&UP'                               INDAT0.......15800
         NS0 = 3                                                         INDAT0.......15900
         SCHDLS(NS0)%NAME = 'STEP_0'                                     INDAT0.......16000
         NS1 = 4                                                         INDAT0.......16100
         SCHDLS(NS1)%NAME = 'STEP_1'                                     INDAT0.......16200
         TIME = TSTART                                                   INDAT0.......16300
         STEP = 0D0                                                      INDAT0.......16400
         CALL LLDINS(SCHDLS(ISCHTS)%LLEN, SCHDLS(ISCHTS)%SLIST, TIME,    INDAT0.......16500
     1      STEP, SCHDLS(ISCHTS)%SLAST)                                  INDAT0.......16600
         CALL LLDINS(SCHDLS(NS0)%LLEN, SCHDLS(NS0)%SLIST, TIME, STEP,    INDAT0.......16700
     1      SCHDLS(NS0)%SLAST)                                           INDAT0.......16800
         TIME = TIME + DELT                                              INDAT0.......16900
         STEP = 1D0                                                      INDAT0.......17000
         CALL LLDINS(SCHDLS(ISCHTS)%LLEN, SCHDLS(ISCHTS)%SLIST, TIME,    INDAT0.......17100
     1      STEP, SCHDLS(ISCHTS)%SLAST)                                  INDAT0.......17200
         CALL LLDINS(SCHDLS(NS1)%LLEN, SCHDLS(NS1)%SLIST, TIME, STEP,    INDAT0.......17300
     1      SCHDLS(NS1)%SLAST)                                           INDAT0.......17400
         CALL LLDINS(SCHDLS(NS1UP)%LLEN, SCHDLS(NS1UP)%SLIST, TIME,      INDAT0.......17500
     1       STEP, SCHDLS(NS1UP)%SLAST)                                  INDAT0.......17600
         ITMAX = 1                                                       INDAT0.......17700
C........WRITE STEADY-STATE OUTPUT INFORMATION AND BEGIN WRITING         INDAT0.......17800
C           DESCRIPTIONS OF SCHEDULES DEFINED BY SUTRA.                  INDAT0.......17900
!         WRITE(K3,138) NSCH                                              INDAT0.......18000
!  138    FORMAT (/13X,'NOTE: BECAUSE FLOW AND TRANSPORT ARE STEADY-',    INDAT0.......18100
!     1      'STATE, USER-DEFINED SCHEDULES ARE NOT IN EFFECT.  '         INDAT0.......18200
!     2      /13X,'STEADY-STATE RESULTS WILL BE WRITTEN TO THE ',         INDAT0.......18300
!     3      'APPROPRIATE OUTPUT FILES.'                                  INDAT0.......18400
!     4      //13X,'THE FOLLOWING ',I1,' SCHEDULES CAN BE USED ',         INDAT0.......18500
!     5      'TO CONTROL SPECIFICATION OF STEADY-STATE BOUNDARY'          INDAT0.......18600
!     6      /13X,'CONDITIONS IN (OPTIONAL) .BCS FILES:')                 INDAT0.......18700
!         WRITE(K3,139) "TIME_STEPS"                                      INDAT0.......18800
!  139    FORMAT(/16X,'SCHEDULE ',A10, 3X,'CONSISTS OF TIME STEPS 0 ',    INDAT0.......18900
!     1      '(STEADY FLOW) AND 1 (STEADY TRANSPORT);',                   INDAT0.......19000
!     2      /41X,'THIS SCHEDULE IS DEFINED AUTOMATICALLY BY SUTRA')      INDAT0.......19100
C........SKIP OVER PROCESSING AND WRITING OF TEMPORAL DATA.              INDAT0.......19200
         GOTO 846                                                        INDAT0.......19300
      END IF                                                             INDAT0.......19400
C.....IF DATASET IN OLD FORMAT, WRITE SPECIFICATIONS AND                 INDAT0.......19500
C        CREATE A CORRESPONDING SCHEDULE CALLED "TIME_STEPS".            INDAT0.......19600
C        IF NSCH=0, GENERATE AN ERROR.  IF IN NEW FORMAT, READ           INDAT0.......19700
C        AND PROCESS USER-DEFINED SCHEDULES.                             INDAT0.......19800
      IF (VERNIN.EQ."2.0") THEN                                          INDAT0.......19900
         TSTART = TICS                                                   INDAT0.......20000
!         WRITE(K3,150) ITMAX,DELT,TMAX,ITCYC,DTMULT,DTMAX                INDAT0.......20100
!  150    FORMAT (/13X,'NOTE: BECAUSE TEMPORAL CONTROL AND SOLUTION ',    INDAT0.......20200
!     1      'CYCLING DATA WERE ENTERED USING THE OLD (VERSION 2D3D.1) ', INDAT0.......20300
!     2      'INPUT FORMAT,'/13X,'A CORRESPONDING SCHEDULE, ',            INDAT0.......20400
!     3      '"TIME_STEPS", WAS CREATED AUTOMATICALLY FROM THE ',         INDAT0.......20500
!     4      'FOLLOWING PARAMETERS:'                                      INDAT0.......20600
!     5      //11X,I15,5X,'MAXIMUM ALLOWED NUMBER OF TIME STEPS'          INDAT0.......20700
!     6      /11X,1PE15.4,5X,'INITIAL TIME STEP (IN SECONDS)'             INDAT0.......20800
!     7      /11X,1PE15.4,5X,'MAXIMUM ALLOWED SIMULATION TIME ',          INDAT0.......20900
!     8      '(IN SECONDS)'                                               INDAT0.......21000
!     9      //11X,I15,5X,'TIME STEP MULTIPLIER CYCLE (IN TIME STEPS)'    INDAT0.......21100
!     1      /11X,0PF15.5,5X,'MULTIPLICATION FACTOR FOR TIME STEP CHANGE' INDAT0.......21200
!     2      /11X,1PE15.4,5X,'MAXIMUM ALLOWED TIME STEP (IN SECONDS)')    INDAT0.......21300
C........FIVE DEFAULT SCHEDULES WILL EVENTUALLY BE DEFINED:              INDAT0.......21400
C           "TIME_STEPS", WHICH CONTROLS TIME STEPPING; "STEPS_1&UP",    INDAT0.......21500
C           WHICH IS IDENTICAL TO "TIME_STEPS" EXCEPT THAT IT OMITS      INDAT0.......21600
C           TIME STEP 0; "STEP_0", WHICH CONSISTS ONLY OF TIME STEP 0;   INDAT0.......21700
C           "STEP_1", WHICH CONSISTS ONLY OF TIME STEP 1; AND "OBS",     INDAT0.......21800
C           WHICH CONTROLS TIMING OF OBSERVATION OUTPUT.  SET THE        INDAT0.......21900
C           NUMBER OF SCHEDULES ACCORDINGLY AND ALLOCATE THE SCHEDULE    INDAT0.......22000
C           ARRAY AND ITS LINKED LISTS.                                  INDAT0.......22100
         NSCH = 5                                                        INDAT0.......22200
         ALLOCATE(SCHDLS(NSCH))                                          INDAT0.......22300
         DO 185 NS=1,NSCH                                                INDAT0.......22400
            ALLOCATE(SCHDLS(NS)%SLIST, SCHDLS(NS)%SLAST)                 INDAT0.......22500
            SCHDLS(NS)%LLEN = 0                                          INDAT0.......22600
  185    CONTINUE                                                        INDAT0.......22700
C........DEFINE THE DEFAULT "TIME_STEPS" SCHEDULE BASED ON THE           INDAT0.......22800
C           TEMPORAL CONTROLS.  NOTE THAT, FOR BACKWARD COMPATIBILITY    INDAT0.......22900
C           WITH OLD DATASETS, THE ORIGINAL METHOD OF HANDLING CHANGES   INDAT0.......23000
C           IN TIME STEP SIZE [BASED ON MOD(JT,ITCYC).EQ.0, NOT          INDAT0.......23100
C           MOD(JT-1,ITCYC).EQ.0] HAS BEEN RETAINED.                     INDAT0.......23200
C           AT THE SAME TIME, DEFINE SCHEDULE "STEPS_1&UP".              INDAT0.......23300
         SCHDLS(1)%NAME = "TIME_STEPS"                                   INDAT0.......23400
         SCHDLS(3)%NAME = "STEPS_1&UP"                                   INDAT0.......23500
         TIME = TSTART                                                   INDAT0.......23600
         STEP = 0D0                                                      INDAT0.......23700
         CALL LLDINS(SCHDLS(1)%LLEN, SCHDLS(1)%SLIST, TIME, STEP,        INDAT0.......23800
     1      SCHDLS(1)%SLAST)                                             INDAT0.......23900
         DTIME = DELT                                                    INDAT0.......24000
         DO 580 JT=1,ITMAX                                               INDAT0.......24100
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DTIME=DTIME*DTMULT     INDAT0.......24200
            IF (DTIME.GT.DTMAX) DTIME = DTMAX                            INDAT0.......24300
            TIME = TIME + DTIME                                          INDAT0.......24400
            STEP = DBLE(JT)                                              INDAT0.......24500
            CALL LLDINS(SCHDLS(1)%LLEN, SCHDLS(1)%SLIST, TIME, STEP,     INDAT0.......24600
     1         SCHDLS(1)%SLAST)                                          INDAT0.......24700
            CALL LLDINS(SCHDLS(3)%LLEN, SCHDLS(3)%SLIST, TIME, STEP,     INDAT0.......24800
     1         SCHDLS(3)%SLAST)                                          INDAT0.......24900
            IF (TIME.GE.TMAX) EXIT                                       INDAT0.......25000
  580    CONTINUE                                                        INDAT0.......25100
         ITMAX = SCHDLS(1)%LLEN - 1                                      INDAT0.......25200
         ISCHTS = 1                                                      INDAT0.......25300
C........SKIP OVER THE CODE THAT READS SCHEDULE SPECIFICATIONS.          INDAT0.......25400
         GOTO 850                                                        INDAT0.......25500
      END IF                                                             INDAT0.......25600
C.....INCREMENT NSCH TO ACCOUNT FOR THREE AUTOMATICALLY DEFINED          INDAT0.......25700
C        SCHEDULES: "STEPS_1&UP", WHICH IS IDENTICAL TO "TIME_STEPS"     INDAT0.......25800
C        EXCEPT THAT IT OMITS TIME STEP 0; "STEP_0", WHICH CONSISTS      INDAT0.......25900
C        ONLY OF TIME STEP 0; AND "STEP_1", WHICH CONSISTS ONLY OF       INDAT0.......26000
C        TIME STEP 1.                                                    INDAT0.......26100
      NSCH = NSCH + NSCHAU                                               INDAT0.......26200
C.....WRITE SCHEDULE PARAMETERS.                                         INDAT0.......26300
!      WRITE(K3,700) NSCH                                                 INDAT0.......26400
!  700 FORMAT(/13X,'THE ',I5,' SCHEDULES ARE LISTED BELOW.'               INDAT0.......26500
!     1   '  SCHEDULE "TIME_STEPS" CONTROLS TIME STEPPING.')              INDAT0.......26600
C.....ALLOCATE SCHEDULE-RELATED ARRAYS AND INITIALIZE SCHEDULE NUMBER    INDAT0.......26700
C        FOR "TIME_STEPS".                                               INDAT0.......26800
      ALLOCATE(SCHDLS(NSCH), SBASED(NSCH), ELAPSD(NSCH))                 INDAT0.......26900
      ISCHTS = 0                                                         INDAT0.......27000
C.....LOOP THROUGH THE LIST OF SCHEDULE SPECIFICATIONS, CONSTRUCTING     INDAT0.......27100
C        SCHEDULES.                                                      INDAT0.......27200
      DO 800 I=1,NSCH-NSCHAU                                             INDAT0.......27300
C........ALLOCATE HEAD OF LINKED LIST FOR THE CURRENT SCHEDULE AND SET   INDAT0.......27400
C           LIST LENGTH TO ZERO.                                         INDAT0.......27500
         ALLOCATE(SCHDLS(I)%SLIST, SCHDLS(I)%SLAST)                      INDAT0.......27600
         SCHDLS(I)%LLEN = 0                                              INDAT0.......27700
C........READ SCHEDULE NAME AND DO SOME ERROR CHECKING.                  INDAT0.......27800
         ERRCOD = 'REA-INP-6'                                            INDAT0.......27900
         CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                              INDAT0.......28000
C rbw begin change
	   IF (IERROR.NE.0) RETURN
C rbw end change
         READ(INTFIL,*,IOSTAT=INERR(1)) CDUM80                           INDAT0.......28100
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......23900
	      RETURN
	   ENDIF
c RBW begin change
         CDUM80 = TRIM(CDUM80)
         ICDUM80Len = LEN_TRIM(CDUM80)
!         IF (LEN_TRIM(CDUM80).GT.10) THEN                                INDAT0.......28300
         IF (ICDUM80Len.GT.10) THEN                                         INDAT0.......28300
c RBW end change	   
            ERRCOD = 'INP-6-15'                                          INDAT0.......28400
            CHERR(1) = CDUM80                                            INDAT0.......28500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......28600
C rbw begin change
	      RETURN
C rbw end change
         ELSE IF (CDUM80.EQ."-") THEN                                    INDAT0.......28700
            ERRCOD = 'INP-6-4'                                           INDAT0.......28800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......28900
C rbw begin change
	      RETURN
C rbw end change
         ELSE IF ((CDUM80.EQ."STEPS_1&UP").OR.                           INDAT0.......29000
     1            (CDUM80.EQ."STEP_0").OR.(CDUM80.EQ."STEP_1")) THEN     INDAT0.......29100
            ERRCOD = 'INP-6-11'                                          INDAT0.......29200
            CHERR(1) = CDUM80                                            INDAT0.......29300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......29400
C rbw begin change
	      RETURN
C rbw end change
         ELSE                                                            INDAT0.......29500
            DO 710 II=1,I-1                                              INDAT0.......29600
               IF (CDUM80.EQ.SCHDLS(II)%NAME) THEN                       INDAT0.......29700
                  ERRCOD = 'INP-6-5'                                     INDAT0.......29800
                  CHERR(1) = CDUM80                                      INDAT0.......29900
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0.......30000
C rbw begin change
	            RETURN
C rbw end change
               END IF                                                    INDAT0.......30100
  710       CONTINUE                                                     INDAT0.......30200
         END IF                                                          INDAT0.......30300
C........(RE)READ SCHEDULE NAME AND TYPE.                                INDAT0.......30400
         READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP                   INDAT0.......30500
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......25400
	      RETURN
	   ENDIF
C........BASED ON THE SCHEDULE TYPE, READ IN THE SPECIFICATIONS AND      INDAT0.......30700
C           CONSTRUCT THE SCHEDULE.                                      INDAT0.......30800
         IF (SCHTYP.EQ."STEP CYCLE") THEN                                INDAT0.......30900
            SBASED(I) = .TRUE.                                           INDAT0.......31000
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......31100
            READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP,               INDAT0.......31200
     1         NSMAX, ISTEPI, ISTEPL, ISTEPC                             INDAT0.......31300
            IF (INERR(1).NE.0) THEN 
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           INDAT0.......26200
	         RETURN
	      ENDIF
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......31500
            ELAPSD(I) = .FALSE.                                          INDAT0.......31600
C...........CONSTRUCT THE SCHEDULE BY STEPPING THROUGH THE STEP CYCLE    INDAT0.......31700
C              AND STORING THE RESULTS IN THE LINKED LIST.  SET TIME     INDAT0.......31800
C              EQUAL TO STEP FOR NOW SO THAT THE LIST IS CONSTRUCTED     INDAT0.......31900
C              IN THE PROPER ORDER.                                      INDAT0.......32000
            NSTEP = ISTEPI                                               INDAT0.......32100
            NDSTEP = ISTEPC                                              INDAT0.......32200
            STEP = DNINT(DBLE(NSTEP))                                    INDAT0.......32300
            TIME = STEP                                                  INDAT0.......32400
            CALL LLDINS(SCHDLS(I)%LLEN, SCHDLS(I)%SLIST, TIME, STEP,     INDAT0.......32500
     1         SCHDLS(I)%SLAST)                                          INDAT0.......32600
            DO 720 NS=1,NSMAX                                            INDAT0.......32700
               NSTEP = NSTEP + NDSTEP                                    INDAT0.......32800
               STEP = DNINT(DBLE(NSTEP))                                 INDAT0.......32900
               TIME = STEP                                               INDAT0.......33000
               CALL LLDINS(SCHDLS(I)%LLEN, SCHDLS(I)%SLIST, TIME, STEP,  INDAT0.......33100
     1            SCHDLS(I)%SLAST)                                       INDAT0.......33200
  720       CONTINUE                                                     INDAT0.......33300
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......33400
!            WRITE(K3,722) SCHDLS(I)%NAME, NSMAX, ISTEPI, ISTEPL, ISTEPC  INDAT0.......33500
!  722       FORMAT(/16X,'SCHEDULE ',A, 3X,'STEP CYCLE WITH THE ',        INDAT0.......33600
!     1         'FOLLOWING SPECIFICATIONS:'                               INDAT0.......33700
!     2         /40X, I8, 5X, 'MAXIMUM NUMBER OF TIME STEPS AFTER ',      INDAT0.......33800
!     3            'INITIAL TIME STEP NUMBER'                             INDAT0.......33900
!     4         /40X, I8, 5X, 'INITIAL TIME STEP NUMBER'                  INDAT0.......34000
!     5         /40X, I8, 5X, 'LIMITING TIME STEP NUMBER'                 INDAT0.......34100
!     6         /40X, I8, 5X, 'TIME STEP INCREMENT')                      INDAT0.......34200
         ELSE IF (SCHTYP.EQ."TIME CYCLE") THEN                           INDAT0.......34300
            SBASED(I) = .FALSE.                                          INDAT0.......34400
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......34500
            READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,        INDAT0.......34600
     1         SCALT, NTMAX, TIMEI, TIMEL, TIMEC, NTCYC,                 INDAT0.......34700
     2         TCMULT, TCMIN, TCMAX                                      INDAT0.......34800
C rbw begin change
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           INDAT0.......29800
	         RETURN
	      ENDIF
C rbw end change
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......35000
            IF (CREFT.EQ.'ELAPSED ') THEN                                INDAT0.......35100
               IF ((SCHNAM.EQ.'TIME_STEPS').AND.(TIMEI.NE.0D0)) THEN     INDAT0.......35200
                  ERRCOD = 'INP-6-7'                                     INDAT0.......35300
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0.......35400
C rbw begin change
	            RETURN
C rbw end change
               END IF                                                    INDAT0.......35500
               ELAPSD(I) = .TRUE.                                        INDAT0.......35600
            ELSE IF (CREFT.EQ.'ABSOLUTE') THEN                           INDAT0.......35700
               ELAPSD(I) = .FALSE.                                       INDAT0.......35800
            ELSE                                                         INDAT0.......35900
               ERRCOD = 'INP-6-6'                                        INDAT0.......36000
               CHERR(1) = CREFT                                          INDAT0.......36100
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  INDAT0.......36200
C rbw begin change
	         RETURN
C rbw end change
            END IF                                                       INDAT0.......36300
C...........SCALE ALL TIME SPECIFICATIONS                                INDAT0.......36400
            TIMEI = TIMEI*SCALT                                          INDAT0.......36500
            TIMEL = TIMEL*SCALT                                          INDAT0.......36600
            TIMEC = TIMEC*SCALT                                          INDAT0.......36700
            TCMIN = TCMIN*SCALT                                          INDAT0.......36800
            TCMAX = TCMAX*SCALT                                          INDAT0.......36900
C...........CONSTRUCT THE SCHEDULE BY STEPPING THROUGH THE TIME CYCLE    INDAT0.......37000
C              AND STORING THE RESULTS IN THE LINKED LIST.               INDAT0.......37100
            TIME = TIMEI                                                 INDAT0.......37200
            STEP = FRCSTP(TIME)                                          INDAT0.......37300
            DTIME = TIMEC                                                INDAT0.......37400
            CALL LLDINS(SCHDLS(I)%LLEN, SCHDLS(I)%SLIST, TIME, STEP,     INDAT0.......37500
     1         SCHDLS(I)%SLAST)                                          INDAT0.......37600
            DO 730 NT=1,NTMAX                                            INDAT0.......37700
               IF (MOD(NT-1,NTCYC).EQ.0 .AND. NT.GT.1)                   INDAT0.......37800
     1            DTIME=DTIME*TCMULT                                     INDAT0.......37900
               IF (DTIME.GT.TCMAX) DTIME = TCMAX                         INDAT0.......38000
               IF (DTIME.LT.TCMIN) DTIME = TCMIN                         INDAT0.......38100
               TIME = TIME + DTIME                                       INDAT0.......38200
               STEP = FRCSTP(TIME)                                       INDAT0.......38300
               CALL LLDINS(SCHDLS(I)%LLEN, SCHDLS(I)%SLIST, TIME, STEP,  INDAT0.......38400
     1            SCHDLS(I)%SLAST)                                       INDAT0.......38500
               IF (TIME.GE.TIMEL) EXIT                                   INDAT0.......38600
  730       CONTINUE                                                     INDAT0.......38700
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......38800
!            WRITE(K3,732) SCHDLS(I)%NAME, TRIM(CREFT), NTMAX, TIMEI,     INDAT0.......38900
!     1          TIMEL, TIMEC, NTCYC, TCMULT, TCMIN, TCMAX                INDAT0.......39000
!  732       FORMAT(/16X,'SCHEDULE ',A, 3X,'TIME CYCLE WITH THE ',        INDAT0.......39100
!     1         'FOLLOWING SPECIFICATIONS IN TERMS OF ', A, ' TIMES:'     INDAT0.......39200
!     2         /46X, I8, 5X, 'MAXIMUM NUMBER OF TIMES AFTER ',           INDAT0.......39300
!     3            'INITIAL TIME'                                         INDAT0.......39400
!     4         /39X, 1PE15.7, 5X, 'INITIAL TIME'                         INDAT0.......39500
!     5         /39X, 1PE15.7, 5X, 'LIMITING TIME'                        INDAT0.......39600
!     6         /39X, 1PE15.7, 5X, 'INITIAL TIME INCREMENT'               INDAT0.......39700
!     7         /46X, I8, 5X, 'TIME INCREMENT CHANGE CYCLE '              INDAT0.......39800
!     8         /39X, 1PE15.7, 5X, 'TIME INCREMENT MULTIPLIER'            INDAT0.......39900
!     9         /39X, 1PE15.7, 5X, 'MINIMUM TIME INCREMENT'               INDAT0.......40000
!     1         /39X, 1PE15.7, 5X, 'MAXIMUM TIME INCREMENT')              INDAT0.......40100
         ELSE IF (SCHTYP.EQ."STEP LIST") THEN                            INDAT0.......40200
            SBASED(I) = .TRUE.                                           INDAT0.......40300
C...........READ THE SCHEDULE NAME, TYPE, AND LENGTH.                    INDAT0.......40400
            BACKSPACE(K1)                                                INDAT0.......40500
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, NSLIST            INDAT0.......40600
C rbw begin change
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            INDAT0.......35600
	         RETURN
	      ENDIF
C rbw end change
C...........ALLOCATE A TEMPORARY ARRAY TO HOLD THE STEP LIST.            INDAT0.......40800
            ALLOCATE (ISLIST(NSLIST))                                    INDAT0.......40900
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......41000
            BACKSPACE(K1)                                                INDAT0.......41100
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP,                   INDAT0.......41200
     1         NSLIST, (ISLIST(NS),NS=1,NSLIST)                          INDAT0.......41300
C rbw begin change
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           INDAT0.......39600
	         RETURN
	      ENDIF
C rbw end change
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......41500
            ELAPSD(I) = .FALSE.                                          INDAT0.......41600
C...........CONSTRUCT THE SCHEDULE BY TRANSFERRING THE LIST FROM ARRAY   INDAT0.......41700
C              ISLIST TO THE LINKED LIST.  SET TIME EQUAL TO STEP FOR    INDAT0.......41800
C              NOW SO THAT THE LIST IS CONSTRUCTED IN THE PROPER ORDER.  INDAT0.......41900
            DO 740 NS=1,NSLIST                                           INDAT0.......42000
               NSTEP = ISLIST(NS)                                        INDAT0.......42100
               STEP = DNINT(DBLE(NSTEP))                                 INDAT0.......42200
               TIME = STEP                                               INDAT0.......42300
               CALL LLDINS(SCHDLS(I)%LLEN, SCHDLS(I)%SLIST, TIME, STEP,  INDAT0.......42400
     1            SCHDLS(I)%SLAST)                                       INDAT0.......42500
  740       CONTINUE                                                     INDAT0.......42600
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......42700
!            WRITE(K3,742) SCHDLS(I)%NAME, (ISLIST(NS),NS=1,NSLIST)       INDAT0.......42800
!  742       FORMAT(/16X,'SCHEDULE ',A, 3X,'STEP LIST THAT INCLUDES ',    INDAT0.......42900
!     1         'THE FOLLOWING TIME STEPS:'/:(38X,8(2X,I8)))              INDAT0.......43000
C...........DEALLOCATE THE TEMPORARY ARRAY.                              INDAT0.......43100
            DEALLOCATE (ISLIST)                                          INDAT0.......43200
         ELSE IF (SCHTYP.EQ."TIME LIST") THEN                            INDAT0.......43300
            SBASED(I) = .FALSE.                                          INDAT0.......43400
C...........READ THE SCHEDULE NAME, TYPE, SCALE FACTOR, AND LENGTH.      INDAT0.......43500
            BACKSPACE(K1)                                                INDAT0.......43600
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,            INDAT0.......43700
     1         SCALT, NTLIST                                             INDAT0.......43800
            IF (INERR(1).NE.0) THEN
              CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)            INDAT0.......43900
		    RETURN
	      ENDIF
C...........ALLOCATE A TEMPORARY ARRAY TO HOLD THE TIME LIST.            INDAT0.......44000
            ALLOCATE (TLIST(NTLIST))                                     INDAT0.......44100
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......44200
            BACKSPACE(K1)                                                INDAT0.......44300
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,            INDAT0.......44400
     1         SCALT, NTLIST, (TLIST(NT),NT=1,NTLIST)                    INDAT0.......44500
            IF (INERR(1).NE.0) THEN
		     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                   INDAT0.......44600
		    RETURN
	      ENDIF
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......44700
            IF (CREFT.EQ.'ELAPSED ') THEN                                INDAT0.......44800
               IF ((SCHNAM.EQ.'TIME_STEPS').AND.(TLIST(1).NE.0D0)) THEN  INDAT0.......44900
                  ERRCOD = 'INP-6-7'                                     INDAT0.......45000
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               INDAT0.......45100
  		        RETURN
               END IF                                                    INDAT0.......45200
               ELAPSD(I) = .TRUE.                                        INDAT0.......45300
            ELSE IF (CREFT.EQ.'ABSOLUTE') THEN                           INDAT0.......45400
               ELAPSD(I) = .FALSE.                                       INDAT0.......45500
            ELSE                                                         INDAT0.......45600
               ERRCOD = 'INP-6-6'                                        INDAT0.......45700
               CHERR(1) = CREFT                                          INDAT0.......45800
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  INDAT0.......45900
  		     RETURN
            END IF                                                       INDAT0.......46000
C...........SCALE ALL TIME SPECIFICATIONS                                INDAT0.......46100
            DO 745 NT=1,NTLIST                                           INDAT0.......46200
               TLIST(NT) = TLIST(NT)*SCALT                               INDAT0.......46300
  745       CONTINUE                                                     INDAT0.......46400
C...........CONSTRUCT THE SCHEDULE BY TRANSFERRING THE LIST FROM ARRAY   INDAT0.......46500
C              TLIST TO THE LINKED LIST.                                 INDAT0.......46600
            DO 750 NT=1,NTLIST                                           INDAT0.......46700
               TIME = TLIST(NT)                                          INDAT0.......46800
               STEP = FRCSTP(TIME)                                       INDAT0.......46900
               CALL LLDINS(SCHDLS(I)%LLEN, SCHDLS(I)%SLIST, TIME, STEP,  INDAT0.......47000
     1            SCHDLS(I)%SLAST)                                       INDAT0.......47100
  750       CONTINUE                                                     INDAT0.......47200
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......47300
!            WRITE(K3,752) SCHDLS(I)%NAME, TRIM(CREFT),                   INDAT0.......47400
!     1         (TLIST(NT),NT=1,NTLIST)                                   INDAT0.......47500
!  752       FORMAT(/16X,'SCHEDULE ',A, 3X,'TIME LIST THAT INCLUDES ',    INDAT0.......47600
!     1         'THE FOLLOWING ', A, ' TIMES (SEC):'                      INDAT0.......47700
!     2         /:(38X,4(1X,1PE15.7)))                                    INDAT0.......47800
            DEALLOCATE (TLIST)                                           INDAT0.......47900
         ELSE                                                            INDAT0.......48000
C...........THE SPECIFIED SCHEDULE TYPE IS INVALID, SO GENERATE AN       INDAT0.......48100
C              ERROR.                                                    INDAT0.......48200
            ERRCOD = 'INP-6-9'                                           INDAT0.......48300
            CHERR(1) = SCHTYP                                            INDAT0.......48400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     INDAT0.......48500
  		  RETURN
         END IF                                                          INDAT0.......48600
  800 CONTINUE                                                           INDAT0.......48700
C.....READ ONE MORE LINE TO CHECK FOR THE END-OF-LIST MARKER ('-').      INDAT0.......48800
C        IF NOT FOUND, GENERATE AN ERROR.                                INDAT0.......48900
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT0.......49000
C rbw begin change
	IF (IERROR.NE.0) RETURN
C rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                              INDAT0.......49100
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0.......44300
	   RETURN
	ENDIF
      IF (CDUM10.NE.'-') THEN                                            INDAT0.......49300
         ERRCOD = 'INP-6-4'                                              INDAT0.......49400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        INDAT0.......49500
C rbw begin change
	   RETURN
C rbw end change
      END IF                                                             INDAT0.......49600
C.....FIND SCHEDULE "TIME_STEPS".                                        INDAT0.......49700
      DO 810 I=1,NSCH-NSCHAU                                             INDAT0.......49800
         IF (SCHDLS(I)%NAME.EQ."TIME_STEPS") THEN                        INDAT0.......49900
            ISCHTS=I                                                     INDAT0.......50000
            TSYES = .TRUE.                                               INDAT0.......50100
            EXIT                                                         INDAT0.......50200
         END IF                                                          INDAT0.......50300
  810 CONTINUE                                                           INDAT0.......50400
C.....IF TRANSPORT IS TRANSIENT AND SCHEDULE "TIME_STEPS" HAS NOT        INDAT0.......50500
C        BEEN DEFINED, GENERATE ERROR                                    INDAT0.......50600
      IF ((ISSTRA.EQ.0).AND.(.NOT.TSYES)) THEN                           INDAT0.......50700
         ERRCOD = 'INP-6-14'                                             INDAT0.......50800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        INDAT0.......50900
	   RETURN
      END IF                                                             INDAT0.......51000
C.....IF "TIME_STEPS" LENGTH IS <=1 (SCHEDULE CONTAINS, AT MOST,         INDAT0.......51100
C        ONLY THE INITIAL TIME, AND NO SUBSEQUENT TIME STEPS),           INDAT0.......51200
C        GENERATE AN ERROR.                                              INDAT0.......51300
      IF (SCHDLS(ISCHTS)%LLEN.LE.1) THEN                                 INDAT0.......51400
         ERRCOD = 'INP-6-10'                                             INDAT0.......51500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        INDAT0.......51600
	   RETURN
      END IF                                                             INDAT0.......51700
C.....IN SCHEDULE TIME_STEPS, FILL IN TIME STEP NUMBERS, ADDING          INDAT0.......51800
C        TICS TO ELAPSED TIMES IF NECESSARY.  GENERATE ERROR IF          INDAT0.......51900
C        ANY TIMES ARE REPEATED.                                         INDAT0.......52000
C        AT THE SAME TIME, DEFINE SCHEDULE "STEPS_1&UP".                 INDAT0.......52100
      NSMAX = SCHDLS(ISCHTS)%LLEN                                        INDAT0.......52200
      ALLOCATE(DTMP1(NSMAX),DTMP2(NSMAX))                                INDAT0.......52300
      CALL LLD2AR(NSMAX, SCHDLS(ISCHTS)%SLIST, DTMP1, DTMP2)             INDAT0.......52400
      NS1UP = NSCH - NSCHAU + 1                                          INDAT0.......52500
      ALLOCATE (SCHDLS(NS1UP)%SLIST, SCHDLS(NS1UP)%SLAST)                INDAT0.......52600
      SCHDLS(ISCHTS)%LLEN = 0                                            INDAT0.......52700
      SCHDLS(NS1UP)%NAME = "STEPS_1&UP"                                  INDAT0.......52800
      SCHDLS(NS1UP)%LLEN = 0                                             INDAT0.......52900
      IF (ELAPSD(ISCHTS)) THEN                                           INDAT0.......53000
         IF (IREAD.EQ.+1) THEN                                           INDAT0.......53100
            TREF = TICS                                                  INDAT0.......53200
         ELSE                                                            INDAT0.......53300
            TREF = TICS0                                                 INDAT0.......53400
         END IF                                                          INDAT0.......53500
      ELSE                                                               INDAT0.......53600
         TREF = 0D0                                                      INDAT0.......53700
      END IF                                                             INDAT0.......53800
      ITMAX = NSMAX - 1                                                  INDAT0.......53900
      TSTART = TREF + DTMP1(1)                                           INDAT0.......54000
      TFINSH = TREF + DTMP1(NSMAX)                                       INDAT0.......54100
      DELT = DTMP1(2) - DTMP1(1)                                         INDAT0.......54200
      DO 820 NS=1,NSMAX                                                  INDAT0.......54300
         IF (NS.GT.1) THEN                                               INDAT0.......54400
         IF (DTMP1(NS).EQ.DTMP1(NS-1)) THEN                              INDAT0.......54500
            ERRCOD = 'INP-6-12'                                          INDAT0.......54600
            IF (ELAPSD(ISCHTS)) THEN                                     INDAT0.......54700
               CHERR(1) = "elapsed time"                                 INDAT0.......54800
            ELSE                                                         INDAT0.......54900
               CHERR(1) = "absolute time"                                INDAT0.......55000
            END IF                                                       INDAT0.......55100
            CHERR(2) = "TIME_STEPS"                                      INDAT0.......55200
            RLERR(1) = DTMP1(NS)                                         INDAT0.......55300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     INDAT0.......55400
C rbw begin change
	      RETURN
C rbw end change
         END IF                                                          INDAT0.......55500
         END IF                                                          INDAT0.......55600
         NSTEP = NS - 1                                                  INDAT0.......55700
         TIME = TREF + DTMP1(NS)                                         INDAT0.......55800
         STEP = DNINT(DBLE(NSTEP))                                       INDAT0.......55900
         CALL LLDINS(SCHDLS(ISCHTS)%LLEN, SCHDLS(ISCHTS)%SLIST, TIME,    INDAT0.......56000
     1      STEP, SCHDLS(ISCHTS)%SLAST)                                  INDAT0.......56100
         IF (NS.GT.1) CALL LLDINS(SCHDLS(NS1UP)%LLEN,                    INDAT0.......56200
     1       SCHDLS(NS1UP)%SLIST, TIME, STEP, SCHDLS(NS1UP)%SLAST)       INDAT0.......56300
  820 CONTINUE                                                           INDAT0.......56400
      DEALLOCATE(DTMP1,DTMP2)                                            INDAT0.......56500
C.....DEFINE SCHEDULES STEP_0 AND STEP_1.                                INDAT0.......56600
      NS0 = NSCH - NSCHAU + 2                                            INDAT0.......56700
      ALLOCATE (SCHDLS(NS0)%SLIST, SCHDLS(NS0)%SLAST)                    INDAT0.......56800
      SCHDLS(NS0)%NAME = "STEP_0"                                        INDAT0.......56900
      SCHDLS(NS0)%LLEN = 0                                               INDAT0.......57000
      STEP = 0                                                           INDAT0.......57100
      TIME = TIMETS(INT(STEP))                                           INDAT0.......57200
      CALL LLDINS(SCHDLS(NS0)%LLEN, SCHDLS(NS0)%SLIST, TIME,             INDAT0.......57300
     1      STEP, SCHDLS(NS0)%SLAST)                                     INDAT0.......57400
      NS1 = NS0 + 1                                                      INDAT0.......57500
      ALLOCATE (SCHDLS(NS1)%SLIST, SCHDLS(NS1)%SLAST)                    INDAT0.......57600
      SCHDLS(NS1)%NAME = "STEP_1"                                        INDAT0.......57700
      SCHDLS(NS1)%LLEN = 0                                               INDAT0.......57800
      STEP = 1                                                           INDAT0.......57900
      TIME = TIMETS(INT(STEP))                                           INDAT0.......58000
      CALL LLDINS(SCHDLS(NS1)%LLEN, SCHDLS(NS1)%SLIST, TIME,             INDAT0.......58100
     1      STEP, SCHDLS(NS1)%SLAST)                                     INDAT0.......58200
C.....FILL IN TIMES OR STEPS FOR REMAINING SCHEDULES, SHIFTING           INDAT0.......58300
C        ELAPSED TIMES TO ABSOLUTE TIMES IF NECESSARY.  PRUNE ENTRIES    INDAT0.......58400
C        THAT ARE OUTSIDE THE RANGE OF SCHEDULE "TIME_STEPS".            INDAT0.......58500
C        GENERATE ERROR IF ANY TIME STEPS OR TIMES ARE REPEATED.         INDAT0.......58600
      DO 845 I=1,NSCH-NSCHAU                                             INDAT0.......58700
         IF (I.EQ.ISCHTS) CYCLE                                          INDAT0.......58800
         NSMAX = SCHDLS(I)%LLEN                                          INDAT0.......58900
         ALLOCATE(DTMP1(NSMAX),DTMP2(NSMAX))                             INDAT0.......59000
         CALL LLD2AR(NSMAX, SCHDLS(I)%SLIST, DTMP1, DTMP2)               INDAT0.......59100
         SCHDLS(I)%LLEN = 0                                              INDAT0.......59200
         IF (ELAPSD(I)) THEN                                             INDAT0.......59300
            TREF = TSTART                                                INDAT0.......59400
         ELSE                                                            INDAT0.......59500
            TREF = 0D0                                                   INDAT0.......59600
         END IF                                                          INDAT0.......59700
         IF (SBASED(I)) THEN                                             INDAT0.......59800
            DO 840 NS=1,NSMAX                                            INDAT0.......59900
               IF (NS.GT.1) THEN                                         INDAT0.......60000
               IF (DTMP2(NS).EQ.DTMP2(NS-1)) THEN                        INDAT0.......60100
                  ERRCOD = 'INP-6-12'                                    INDAT0.......60200
                  CHERR(1) = "time step"                                 INDAT0.......60300
                  CHERR(2) = SCHDLS(I)%NAME                              INDAT0.......60400
                  RLERR(1) = DTMP2(NS)                                   INDAT0.......60500
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               INDAT0.......60600
C rbw begin change
	            RETURN
C rbw end change
               END IF                                                    INDAT0.......60700
               END IF                                                    INDAT0.......60800
               STEP = DTMP2(NS)                                          INDAT0.......60900
               NSTEP = NINT(STEP)                                        INDAT0.......61000
               IF ((NSTEP.LT.0).OR.(NSTEP.GT.ITMAX)) CYCLE               INDAT0.......61100
               TIME = TIMETS(NSTEP)                                      INDAT0.......61200
               CALL LLDINS(SCHDLS(I)%LLEN, SCHDLS(I)%SLIST, TIME,        INDAT0.......61300
     1            STEP, SCHDLS(I)%SLAST)                                 INDAT0.......61400
  840       CONTINUE                                                     INDAT0.......61500
         ELSE                                                            INDAT0.......61600
            DO 842 NS=1,NSMAX                                            INDAT0.......61700
               IF (NS.GT.1) THEN                                         INDAT0.......61800
               IF (DTMP1(NS).EQ.DTMP1(NS-1)) THEN                        INDAT0.......61900
                  ERRCOD = 'INP-6-12'                                    INDAT0.......62000
                  IF (ELAPSD(I)) THEN                                    INDAT0.......62100
                     CHERR(1) = "elapsed time"                           INDAT0.......62200
                  ELSE                                                   INDAT0.......62300
                     CHERR(1) = "absolute time"                          INDAT0.......62400
                  END IF                                                 INDAT0.......62500
                  CHERR(2) = SCHDLS(I)%NAME                              INDAT0.......62600
                  RLERR(1) = DTMP1(NS)                                   INDAT0.......62700
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               INDAT0.......62800
C rbw begin change
	            RETURN
C rbw end change
               END IF                                                    INDAT0.......62900
               END IF                                                    INDAT0.......63000
               TIME = TREF + DTMP1(NS)                                   INDAT0.......63100
               IF ((TIME.LT.TSTART).OR.(TIME.GT.TFINSH)) CYCLE           INDAT0.......63200
               STEP = FRCSTP(TIME)                                       INDAT0.......63300
               CALL LLDINS(SCHDLS(I)%LLEN, SCHDLS(I)%SLIST, TIME,        INDAT0.......63400
     1            STEP, SCHDLS(I)%SLAST)                                 INDAT0.......63500
  842       CONTINUE                                                     INDAT0.......63600
         END IF                                                          INDAT0.......63700
         DEALLOCATE(DTMP1,DTMP2)                                         INDAT0.......63800
  845 CONTINUE                                                           INDAT0.......63900
C.....DEALLOCATE ARRAY THAT INDICATES METHODS OF SCHEDULE SPECIFICATION  INDAT0.......64000
      DEALLOCATE(SBASED)                                                 INDAT0.......64100
C.....WRITE SPECIFICATIONS OF SCHEDULES "STEPS_1&UP", "STEP_0", AND      INDAT0.......64200
C        "STEP_1".                                                       INDAT0.......64300
  846 CONTINUE
!  846 WRITE(K3,847) "STEPS_1&UP"                                         INDAT0.......64400
!  847 FORMAT(/16X,'SCHEDULE ',A10, 3X,'IDENTICAL TO SCHEDULE ',          INDAT0.......64500
!     1   '"TIME_STEPS", EXCEPT THAT IT OMITS TIME STEP 0;',              INDAT0.......64600
!     2   /41X,'THIS SCHEDULE IS DEFINED AUTOMATICALLY BY SUTRA')         INDAT0.......64700
!      WRITE(K3,848) "STEP_0", 0                                          INDAT0.......64800
!      WRITE(K3,848) "STEP_1", 1                                          INDAT0.......64900
!  848 FORMAT(/16X,'SCHEDULE ',A6, 4X,                                    INDAT0.......65000
!     1   3X,'CONSISTS ONLY OF TIME STEP ', I1, ';',                      INDAT0.......65100
!     2   /41X,'THIS SCHEDULE IS DEFINED AUTOMATICALLY BY SUTRA')         INDAT0.......65200
C.....WRITE THE SOLUTION CYCLING CONTROLS.                               INDAT0.......65300
  850 CONTINUE
!  850 WRITE(K3,874) NPCYC,NUCYC                                          INDAT0.......65400
!  874 FORMAT (/13X,'SOLUTION CYCLING DATA:'                              INDAT0.......65500
!     1      //11X,I15,5X,'FLOW SOLUTION CYCLE (IN TIME STEPS)'           INDAT0.......65600
!     2      /11X,I15,5X,'TRANSPORT SOLUTION CYCLE (IN TIME STEPS)'       INDAT0.......65700
!     3      //16X,'FLOW AND TRANSPORT SOLUTIONS ARE ALSO COMPUTED '      INDAT0.......65800
!     4      'AUTOMATICALLY ON TIME STEPS ON WHICH FLOW-RELATED '         INDAT0.......65900
!     5      /16X,'AND TRANSPORT-RELATED BOUNDARY CONDITIONS, '           INDAT0.......66000
!     5      'RESPECTIVELY, ARE SET IN (OPTIONAL) BCS FILES.')            INDAT0.......66100
C.....SET SOLUTION CYCLING FOR STEADY-STATE FLOW                         INDAT0.......66200
      IF(ISSFLO.EQ.1) THEN                                               INDAT0.......66300
         NPCYC=ITMAX+1                                                   INDAT0.......66400
         NUCYC=1                                                         INDAT0.......66500
      END IF                                                             INDAT0.......66600
C                                                                        INDAT0.......66700
C.....INPUT DATASET 7A:  ITERATION CONTROLS FOR RESOLVING NONLINEARITIES INDAT0.......66800
      ERRCOD = 'REA-INP-7A'                                              INDAT0.......66900
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT0.......67000
C rbw begin change
	IF (IERROR.NE.0) RETURN
C rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) ITRMAX                              INDAT0.......67100
C rbw begin change
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0.......57600
	   RETURN
	ENDIF
C rbw end change
      IF (ITRMAX.GT.1) THEN                                              INDAT0.......67300
         ERRCOD = 'REA-INP-7A'                                           INDAT0.......67400
         READ(INTFIL,*,IOSTAT=INERR(1)) ITRMAX,RPMAX,RUMAX               INDAT0.......67500
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......58000
	      RETURN
	   ENDIF
      END IF                                                             INDAT0.......67700
!      IF(ITRMAX-1) 1192,1192,1194                                        INDAT0.......67800
! 1192 WRITE(K3,1193)                                                     INDAT0.......67900
! 1193 FORMAT(////11X,'I T E R A T I O N   C O N T R O L   D A T A',      INDAT0.......68000
!     1   //11X,'  NON-ITERATIVE SOLUTION')                               INDAT0.......68100
!      GOTO 1196                                                          INDAT0.......68200
! 1194 WRITE(K3,1195) ITRMAX,RPMAX,RUMAX                                  INDAT0.......68300
! 1195 FORMAT(////11X,'I T E R A T I O N   C O N T R O L   D A T A',      INDAT0.......68400
!     1   //11X,I15,5X,'MAXIMUM NUMBER OF ITERATIONS PER TIME STEP',      INDAT0.......68500
!     2   /11X,1PE15.4,5X,'ABSOLUTE CONVERGENCE CRITERION FOR FLOW',      INDAT0.......68600
!     3   ' SOLUTION'/11X,1PE15.4,5X,'ABSOLUTE CONVERGENCE CRITERION',    INDAT0.......68700
!     4   ' FOR TRANSPORT SOLUTION')                                      INDAT0.......68800
 1196 CONTINUE                                                           INDAT0.......68900
C                                                                        INDAT0.......69000
C.....INPUT DATASETS 7B & 7C:  MATRIX EQUATION SOLVER CONTROLS FOR       INDAT0.......69100
C        PRESSURE AND TRANSPORT SOLUTIONS                                INDAT0.......69200
      ERRCOD = 'REA-INP-7B'                                              INDAT0.......69300
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT0.......69400
C rbw begin change
	IF (IERROR.NE.0) RETURN
C rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVP                              INDAT0.......69500
C rbw begin change
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0.......60000
	   RETURN
	ENDIF
C rbw end change
      IF ((CSOLVP.NE.SOLWRD(0))) THEN                                    INDAT0.......69700
         ERRCOD = 'REA-INP-7B'                                           INDAT0.......69800
         READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVP,ITRMXP,TOLP               INDAT0.......69900
         IF (INERR(1).NE.0) then
	     call SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                                                            INDAT0.......70000
	     return
	   endif

      END IF                                                             INDAT0.......70100
      ERRCOD = 'REA-INP-7C'                                              INDAT0.......70200
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT0.......70300
C rbw begin change
	IF (IERROR.NE.0) RETURN
C rbw end change
      READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVU                              INDAT0.......70400
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0.......60900
	   RETURN
	ENDIF
      IF ((CSOLVU.NE.SOLWRD(0))) THEN                                    INDAT0.......70600
         ERRCOD = 'REA-INP-7C'                                           INDAT0.......70700
         READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVU,ITRMXU,TOLU               INDAT0.......70800
         IF (INERR(1).NE.0) THEN 
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......61300
C rbw begin change
	      RETURN
C rbw end change
	   ENDIF
      END IF                                                             INDAT0.......71000
      KSOLVP = -1                                                        INDAT0.......71100
      KSOLVU = -1                                                        INDAT0.......71200
      DO 1250 M=0,NSLVRS-1                                               INDAT0.......71300
         IF (CSOLVP.EQ.SOLWRD(M)) KSOLVP = M                             INDAT0.......71400
         IF (CSOLVU.EQ.SOLWRD(M)) KSOLVU = M                             INDAT0.......71500
 1250 CONTINUE                                                           INDAT0.......71600
      IF ((KSOLVP.LT.0).OR.(KSOLVU.LT.0)) THEN                           INDAT0.......71700
         ERRCOD = 'INP-7B&C-1'                                           INDAT0.......71800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......71900
C rbw begin change
         RETURN
C rbw end change
      ELSE IF ((KSOLVP*KSOLVU.EQ.0).AND.(KSOLVP+KSOLVU.NE.0)) THEN       INDAT0.......72000
         ERRCOD = 'INP-7B&C-2'                                           INDAT0.......72100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......72200
C rbw begin change
         RETURN
C rbw end change
      ELSE IF ((KSOLVU.EQ.1).OR.((KSOLVP.EQ.1).AND.(UP.NE.0D0))) THEN    INDAT0.......72300
         ERRCOD = 'INP-7B&C-3'                                           INDAT0.......72400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......72500
C rbw begin change
         RETURN
C rbw end change
      END IF                                                             INDAT0.......72600
      IF (KSOLVP.EQ.2) THEN                                              INDAT0.......72700
         ITOLP = 0                                                       INDAT0.......72800
      ELSE                                                               INDAT0.......72900
         ITOLP = 1                                                       INDAT0.......73000
      END IF                                                             INDAT0.......73100
      IF (KSOLVU.EQ.2) THEN                                              INDAT0.......73200
         ITOLU = 0                                                       INDAT0.......73300
      ELSE                                                               INDAT0.......73400
         ITOLU = 1                                                       INDAT0.......73500
      END IF                                                             INDAT0.......73600
      NSAVEP = 10                                                        INDAT0.......73700
      NSAVEU = 10                                                        INDAT0.......73800
C                                                                        INDAT0.......73900
C                                                                        INDAT0.......74000
      RETURN                                                             INDAT0.......74100
      END                                                                INDAT0.......74200
C                                                                        INDAT0.......74300
C     SUBROUTINE        I  N  D  A  T  1           SUTRA VERSION 2.2     INDAT1.........100
C                                                                        INDAT1.........200
C *** PURPOSE :                                                          INDAT1.........300
C ***  TO INPUT, OUTPUT, AND ORGANIZE A MAJOR PORTION OF INP FILE        INDAT1.........400
C ***  INPUT DATA (DATASETS 8 THROUGH 15)                                INDAT1.........500
C                                                                        INDAT1.........600
      SUBROUTINE INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,         INDAT1.........700
     1   ATMIN,PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,                       INDAT1.........800
     2   PERMYZ,PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG, INDAT1.........900
     3   OBSPTS,                                                         INDAT1........1000
     4   ElementValues, IElementValueCount, NodeValues, 
     5   INodeValueCount, IERROR)                                        
      USE ALLARR, ONLY : OBSDAT                                          INDAT1........1100
      USE LLDEF                                                          INDAT1........1200
      USE EXPINT                                                         INDAT1........1300
      USE SCHDEF                                                         INDAT1........1400
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT1........1500
      PARAMETER (NCOLMX=9)                                               INDAT1........1600
!      CHARACTER*10 ADSMOD,CDUM10                                         INDAT1........1700
      CHARACTER*10 ADSMOD,SWMOD(10),SWPNM(10),CDUM10,ADSMOD1(10)           ! frz_input2, frz_input2a, svara
      CHARACTER*10 SIMOD(10),SIPNM(10),RKMOD(10),RKPNM(10)                         ! frz_input2, frz_input2a
      CHARACTER*6 STYPE(2)                                               INDAT1........1800
      CHARACTER K5SYM(8)*1, NCOL(NCOLMX)*1, VARNK5(7)*25                 INDAT1........1900
      CHARACTER K6SYM(7)*2, LCOL(NCOLMX)*2, VARNK6(7)*25                 INDAT1........2000
      CHARACTER*1 CNODAL,CELMNT,CINCID,CPANDS,CVEL,CCORT,CBUDG,          INDAT1........2100
     1   CSCRN,CPAUSE,CINACT                                             INDAT1........2200
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:13)                    INDAT1........2300
      CHARACTER INTFIL*1000,DOTS45*45                                    INDAT1........2400
      CHARACTER OBSNAM*40, OBSSCH*10, OBSFMT*3, CDUM80*80                INDAT1........2500
      CHARACTER*8 VERNUM, VERNIN                                         INDAT1........2600
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         INDAT1........2700
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             INDAT1........2800
      DIMENSION X(NN),Y(NN),Z(NN),POR(NN),SOP(NN),NREG(NN)               INDAT1........2900
      DIMENSION PERMXX(NE),PERMXY(NE),PERMXZ(NEX),PERMYX(NE),PERMYY(NE), INDAT1........3000
     1   PERMYZ(NEX),PERMZX(NEX),PERMZY(NEX),PERMZZ(NEX),PANGL1(NE),     INDAT1........3100
     2   PANGL2(NEX),PANGL3(NEX),ALMAX(NE),ALMID(NEX),ALMIN(NE),         INDAT1........3200
     3   ATMAX(NE),ATMID(NEX),ATMIN(NE),LREG(NE)                         INDAT1........3300
      DIMENSION SWPAR(10,10),RKPAR(10,10),SIPAR(10,10)                        ! frz_input2a
      DIMENSION NSWPAR(10), NSIPAR(10), NRKPAR(10)                         ! frz_input2
      DIMENSION CHI11(10), CHI21(10)                                       ! svara
      DIMENSION INERR(10),RLERR(10)                                      INDAT1........3400
      DIMENSION KTYPE(2)                                                 INDAT1........3500
      DIMENSION IUNIT(0:13)                                              INDAT1........3600
      ALLOCATABLE :: INOB(:)                                             INDAT1........3700
      TYPE (LLD), POINTER :: DENTS                                       INDAT1........3800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT1........3900
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,           INDAT1........4000
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE                                       INDAT1........4100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT1........4200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            INDAT1........4300
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        INDAT1........4400
      COMMON /FNAMES/ UNAME,FNAME                                        INDAT1........4500
      COMMON /FUNITA/ IUNIT                                              INDAT1........4600
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 INDAT1........4700
     1   K10,K11,K12,K13                                                 INDAT1........4800
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INDAT1........4900
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      INDAT1........5000
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        INDAT1........5100
      COMMON /KPRBCS/ KINACT                                             INDAT1........5200
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                INDAT1........5300
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            INDAT1........5400
      COMMON /MODSOR/ ADSMOD                                             INDAT1........5500
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      INDAT1........5600
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT1........5700
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT1........5800
      COMMON /PARUI/ SWPAR, RKPAR, SIPAR                                   ! frz_input2
      COMMON /PARSOR/ CHI11,CHI21                                          ! svara
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    INDAT1........5900
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT1........6000
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      INDAT1........6100
      COMMON /VER/ VERNUM, VERNIN                                        INDAT1........6200
      DATA STYPE(1)/'ENERGY'/,STYPE(2)/'SOLUTE'/                         INDAT1........6300
      DATA (K5SYM(MM), MM=1,8) /'N', 'X', 'Y', 'Z', 'P', 'U', 'S', 'I'/    ! siio   INDAT1........6400
      DATA (VARNK5(MM), MM=1,7) /'NODE NUMBER',                          INDAT1........6500
     1   'X-COORDINATE', 'Y-COORDINATE', 'Z-COORDINATE',                 INDAT1........6600
     2   'PRESSURE', 'CONCENTRATION/TEMPERATURE', 'SATURATION'/          INDAT1........6700
      DATA (K6SYM(MM), MM=1,7) /'E', 'X', 'Y', 'Z', 'VX', 'VY', 'VZ'/    INDAT1........6800
      DATA (VARNK6(MM), MM=1,7) /'ELEMENT NUMBER',                       INDAT1........6900
     1   'X-COORDINATE OF CENTROID', 'Y-COORDINATE OF CENTROID',         INDAT1........7000
     2   'Z-COORDINATE OF CENTROID', 'X-VELOCITY', 'Y-VELOCITY',         INDAT1........7100
     3   'Z-VELOCITY'/                                                   INDAT1........7200
      DATA DOTS45 /'.............................................'/      INDAT1........7300
      SAVE STYPE,K5SYM,VARNK5,K6SYM,VARNK6                               INDAT1........7400
C RBW
      INTEGER IERROR
      INTEGER IElementValueCount
      INTEGER INodeValueCount
	REAL (KIND = 4) ElementValues(IElementValueCount)
	REAL (KIND = 4) NodeValues(INodeValueCount)
C RBW
C                                                                        INDAT1........7500
      INSTOP=0                                                           INDAT1........7600
!	goto 9999                                                      
C                                                                        INDAT1........7700
C.....INPUT DATASET 8A:  OUTPUT CONTROLS AND OPTIONS FOR LST FILE        INDAT1........7800
C        AND SCREEN                                                      INDAT1........7900
      ERRCOD = 'REA-INP-8A'                                              INDAT1........8000
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1........8100
	IF (IERROR.NE.0) RETURN
      IF ((VERNIN.EQ.'2.0').OR.(VERNIN.EQ.'2.1')) THEN                   INDAT1........8200
         READ(INTFIL,*,IOSTAT=INERR(1)) NPRINT,CNODAL,CELMNT,CINCID,     INDAT1........8300
     1      CVEL,CBUDG,CSCRN,CPAUSE                                      INDAT1........8400
         CPANDS = 'Y'                                                    INDAT1........8500
         CCORT = 'Y'                                                     INDAT1........8600
      ELSE                                                               INDAT1........8700
         READ(INTFIL,*,IOSTAT=INERR(1)) NPRINT,CNODAL,CELMNT,CINCID,     INDAT1........8800
     1      CPANDS,CVEL,CCORT,CBUDG,CSCRN,CPAUSE                         INDAT1........8900
      END IF                                                             INDAT1........9000
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1........8000
	   RETURN
	ENDIF
      IF (CNODAL.EQ.'Y') THEN                                            INDAT1........9200
         KNODAL = +1                                                     INDAT1........9300
      ELSE IF (CNODAL.EQ.'N') THEN                                       INDAT1........9400
         KNODAL = 0                                                      INDAT1........9500
      ELSE                                                               INDAT1........9600
         ERRCOD = 'INP-8A-1'                                             INDAT1........9700
         CHERR(1) = 'CNODAL '                                            INDAT1........9800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1........9900
	   RETURN
      END IF                                                             INDAT1.......10000
      IF (CELMNT.EQ.'Y') THEN                                            INDAT1.......10100
         KELMNT = +1                                                     INDAT1.......10200
      ELSE IF (CELMNT.EQ.'N') THEN                                       INDAT1.......10300
         KELMNT = 0                                                      INDAT1.......10400
      ELSE                                                               INDAT1.......10500
         ERRCOD = 'INP-8A-2'                                             INDAT1.......10600
         CHERR(1) = 'CELMNT'                                             INDAT1.......10700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......10800
	   RETURN
      END IF                                                             INDAT1.......10900
      IF (CINCID.EQ.'Y') THEN                                            INDAT1.......11000
         KINCID = +1                                                     INDAT1.......11100
      ELSE IF (CINCID.EQ.'N') THEN                                       INDAT1.......11200
         KINCID = 0                                                      INDAT1.......11300
      ELSE                                                               INDAT1.......11400
         ERRCOD = 'INP-8A-3'                                             INDAT1.......11500
         CHERR(1) = 'CINCID'                                             INDAT1.......11600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......11700
	   RETURN
      END IF                                                             INDAT1.......11800
      IF (CPANDS.EQ.'Y') THEN                                            INDAT1.......11900
         KPANDS = +1                                                     INDAT1.......12000
      ELSE IF (CPANDS.EQ.'N') THEN                                       INDAT1.......12100
         KPANDS = 0                                                      INDAT1.......12200
      ELSE                                                               INDAT1.......12300
         ERRCOD = 'INP-8A-8'                                             INDAT1.......12400
         CHERR(1) = 'CPANDS'                                             INDAT1.......12500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......12600
	   RETURN
      END IF                                                             INDAT1.......12700
      IF (CVEL.EQ.'Y') THEN                                              INDAT1.......12800
         KVEL = +1                                                       INDAT1.......12900
      ELSE IF (CVEL.EQ.'N') THEN                                         INDAT1.......13000
         KVEL = 0                                                        INDAT1.......13100
      ELSE                                                               INDAT1.......13200
         ERRCOD = 'INP-8A-4'                                             INDAT1.......13300
         CHERR(1) = 'CVEL  '                                             INDAT1.......13400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......13500
	   RETURN
      END IF                                                             INDAT1.......13600
      IF (CCORT.EQ.'Y') THEN                                             INDAT1.......13700
         KCORT = +1                                                      INDAT1.......13800
      ELSE IF (CCORT.EQ.'N') THEN                                        INDAT1.......13900
         KCORT = 0                                                       INDAT1.......14000
      ELSE                                                               INDAT1.......14100
         ERRCOD = 'INP-8A-9'                                             INDAT1.......14200
         CHERR(1) = 'CCORT '                                             INDAT1.......14300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......14400
	   RETURN
      END IF                                                             INDAT1.......14500
      IF (CBUDG.EQ.'Y') THEN                                             INDAT1.......14600
         KBUDG = +1                                                      INDAT1.......14700
      ELSE IF (CBUDG.EQ.'N') THEN                                        INDAT1.......14800
         KBUDG = 0                                                       INDAT1.......14900
      ELSE                                                               INDAT1.......15000
         ERRCOD = 'INP-8A-5'                                             INDAT1.......15100
         CHERR(1) = 'CBUDG '                                             INDAT1.......15200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......15300
	   RETURN
      END IF                                                             INDAT1.......15400
      IF (CSCRN.EQ.'Y') THEN                                             INDAT1.......15500
         KSCRN = +1                                                      INDAT1.......15600
      ELSE IF (CSCRN.EQ.'N') THEN                                        INDAT1.......15700
         KSCRN = 0                                                       INDAT1.......15800
      ELSE                                                               INDAT1.......15900
         ERRCOD = 'INP-8A-6'                                             INDAT1.......16000
         CHERR(1) = 'CSCRN '                                             INDAT1.......16100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......16200
	   RETURN
      END IF                                                             INDAT1.......16300
      IF (CPAUSE.EQ.'Y') THEN                                            INDAT1.......16400
         KPAUSE = +1                                                     INDAT1.......16500
      ELSE IF (CPAUSE.EQ.'N') THEN                                       INDAT1.......16600
         KPAUSE = 0                                                      INDAT1.......16700
      ELSE                                                               INDAT1.......16800
         ERRCOD = 'INP-8A-7'                                             INDAT1.......16900
         CHERR(1) = 'CPAUSE'                                             INDAT1.......17000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......17100
	   RETURN
      END IF                                                             INDAT1.......17200
C                                                                        INDAT1.......17300
!      WRITE(K3,72) NPRINT                                                INDAT1.......17400
!   72 FORMAT(////11X,'O U T P U T   C O N T R O L S   A N D   ',         INDAT1.......17500
!     1   'O P T I O N S'//13X,'.LST FILE'/13X,'---------'                INDAT1.......17600
!     2   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)')             INDAT1.......17700
!      IF(KNODAL.EQ.+1) WRITE(K3,74)                                      INDAT1.......17800
!      IF(KNODAL.EQ.0) WRITE(K3,75)                                       INDAT1.......17900
!   74 FORMAT(/13X,'- PRINT NODE COORDINATES, THICKNESSES AND',           INDAT1.......18000
!     1   ' POROSITIES')                                                  INDAT1.......18100
!   75 FORMAT(/13X,'- CANCEL PRINT OF NODE COORDINATES, THICKNESSES AND', INDAT1.......18200
!     1   ' POROSITIES')                                                  INDAT1.......18300
!      IF(KELMNT.EQ.+1) WRITE(K3,76)                                      INDAT1.......18400
!      IF(KELMNT.EQ.0) WRITE(K3,77)                                       INDAT1.......18500
!   76 FORMAT(13X,'- PRINT ELEMENT PERMEABILITIES AND DISPERSIVITIES')    INDAT1.......18600
!   77 FORMAT(13X,'- CANCEL PRINT OF ELEMENT PERMEABILITIES AND ',        INDAT1.......18700
!     1   'DISPERSIVITIES')                                               INDAT1.......18800
!      IF(KINCID.EQ.+1) WRITE(K3,78)                                      INDAT1.......18900
!      IF(KINCID.EQ.0) WRITE(K3,79)                                       INDAT1.......19000
!   78 FORMAT(13X,'- PRINT NODE INCIDENCES IN EACH ELEMENT')              INDAT1.......19100
!   79 FORMAT(13X,'- CANCEL PRINT OF NODE INCIDENCES IN EACH ELEMENT')    INDAT1.......19200
      IME=2                                                              INDAT1.......19300
      IF(ME.EQ.+1) IME=1                                                 INDAT1.......19400
!      IF(KPANDS.EQ.+1) WRITE(K3,81)                                      INDAT1.......19500
!      IF(KPANDS.EQ.0) WRITE(K3,82)                                       INDAT1.......19600
!   81 FORMAT(/13X,'- PRINT PRESSURES AND SATURATIONS AT NODES ',         INDAT1.......19700
!     1   'ON EACH TIME STEP WITH OUTPUT')                                INDAT1.......19800
!   82 FORMAT(/13X,'- CANCEL PRINT OF PRESSURES AND SATURATIONS')         INDAT1.......19900
!      IF(KVEL.EQ.+1) WRITE(K3,84)                                        INDAT1.......20000
!      IF(KVEL.EQ.0) WRITE(K3,85)                                         INDAT1.......20100
!   84 FORMAT(13X,'- CALCULATE AND PRINT VELOCITIES AT ELEMENT ',         INDAT1.......20200
!     1   'CENTROIDS ON EACH TIME STEP WITH OUTPUT')                      INDAT1.......20300
!   85 FORMAT(13X,'- CANCEL PRINT OF VELOCITIES')                         INDAT1.......20400
!      IF(KCORT.EQ.+1) THEN                                               INDAT1.......20500
!         IF (ME.EQ.-1) THEN                                              INDAT1.......20600
!            WRITE(K3,86)                                                 INDAT1.......20700
!         ELSE                                                            INDAT1.......20800
!            WRITE(K3,87)                                                 INDAT1.......20900
!         END IF                                                          INDAT1.......21000
!      ELSE                                                               INDAT1.......21100
!         IF (ME.EQ.-1) THEN                                              INDAT1.......21200
!            WRITE(K3,88)                                                 INDAT1.......21300
!         ELSE                                                            INDAT1.......21400
!            WRITE(K3,89)                                                 INDAT1.......21500
!         END IF                                                          INDAT1.......21600
!      END IF                                                             INDAT1.......21700
!   86 FORMAT(13X,'- PRINT CONCENTRATIONS AT NODES ',                     INDAT1.......21800
!     1   'ON EACH TIME STEP WITH OUTPUT')                                INDAT1.......21900
!   87 FORMAT(13X,'- PRINT TEMPERATURES AT NODES ',                       INDAT1.......22000
!     1   'ON EACH TIME STEP WITH OUTPUT')                                INDAT1.......22100
!   88 FORMAT(13X,'- CANCEL PRINT OF CONCENTRATIONS')                     INDAT1.......22200
!   89 FORMAT(13X,'- CANCEL PRINT OF TEMPERATURES')                       INDAT1.......22300
!      IF(KBUDG.EQ.+1) WRITE(K3,90) STYPE(IME)                            INDAT1.......22400
!      IF(KBUDG.EQ.0) WRITE(K3,91)                                        INDAT1.......22500
!   90 FORMAT(/13X,'- CALCULATE AND PRINT FLUID AND ',A6,' BUDGETS ',     INDAT1.......22600
!     1   'ON EACH TIME STEP WITH OUTPUT')                                INDAT1.......22700
!   91 FORMAT(/13X,'- CANCEL PRINT OF BUDGETS')                           INDAT1.......22800
C                                                                        INDAT1.......22900
C.....INPUT DATASET 8B:  OUTPUT CONTROLS AND OPTIONS FOR NOD FILE        INDAT1.......23000
      ERRCOD = 'REA-INP-8B'                                              INDAT1.......23100
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......23200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) NCOLPR                              INDAT1.......23300
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......18100
	   RETURN
	ENDIF
      DO 140 M=1,NCOLMX                                                  INDAT1.......23500
         READ(INTFIL,*,IOSTAT=INERR(1)) NCOLPR, (NCOL(MM), MM=1,M)       INDAT1.......23600
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT1.......18400
	      RETURN
	   ENDIF
         IF (NCOL(M).EQ.'-') THEN                                        INDAT1.......23800
            NCOLS5 = M - 1                                               INDAT1.......23900
            GOTO 142                                                     INDAT1.......24000
         END IF                                                          INDAT1.......24100
  140 CONTINUE                                                           INDAT1.......24200
      NCOLS5 = NCOLMX                                                    INDAT1.......24300
  142 CONTINUE                                                           INDAT1.......24400
!      WRITE(K3,144) NCOLPR                                               INDAT1.......24500
!  144 FORMAT (//13X,'.NOD FILE'/13X,'---------'                          INDAT1.......24600
!     1   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......24700
      DO 148 M=1,NCOLS5                                                  INDAT1.......24800
         DO 146 MM=1,8                                                   INDAT1.......24900
            IF (NCOL(M).EQ.K5SYM(MM)) THEN                               INDAT1.......25000
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN                          INDAT1.......25100
                  ERRCOD = 'INP-8B-1'                                    INDAT1.......25200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......25300
	            RETURN
               END IF                                                    INDAT1.......25400
               IF ((MM.EQ.4).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......25500
                  ERRCOD = 'INP-8B-2'                                    INDAT1.......25600
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......25700
	            RETURN
               END IF                                                    INDAT1.......25800
               J5COL(M) = MM                                             INDAT1.......25900
               GOTO 148                                                  INDAT1.......26000
            END IF                                                       INDAT1.......26100
  146    CONTINUE                                                        INDAT1.......26200
         ERRCOD = 'INP-8B-3'                                             INDAT1.......26300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......26400
	   RETURN
  148 CONTINUE                                                           INDAT1.......26500
!      WRITE(K3,150) (M,VARNK5(J5COL(M)),M=1,NCOLS5)                      INDAT1.......26600
!  150 FORMAT (13X,'COLUMN ',I1,':',2X,A)                                 INDAT1.......26700
C                                                                        INDAT1.......26800
C.....INPUT DATASET 8C:  OUTPUT CONTROLS AND OPTIONS FOR ELE FILE        INDAT1.......26900
      ERRCOD = 'REA-INP-8C'                                              INDAT1.......27000
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......27100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) LCOLPR                              INDAT1.......27200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......22000
	   RETURN
	ENDIF
      DO 160 M=1,NCOLMX                                                  INDAT1.......27400
         READ(INTFIL,*,IOSTAT=INERR(1)) LCOLPR, (LCOL(MM), MM=1,M)       INDAT1.......27500
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT1.......22300
	      RETURN
	   ENDIF
         IF (LCOL(M).EQ.'-') THEN                                        INDAT1.......27700
            NCOLS6 = M - 1                                               INDAT1.......27800
            GOTO 162                                                     INDAT1.......27900
         END IF                                                          INDAT1.......28000
  160 CONTINUE                                                           INDAT1.......28100
      NCOLS6 = NCOLMX                                                    INDAT1.......28200
  162 CONTINUE                                                           INDAT1.......28300
!      WRITE(K3,164) LCOLPR                                               INDAT1.......28400
!  164 FORMAT (//13X,'.ELE FILE'/13X,'---------'                          INDAT1.......28500
!     1   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......28600
      DO 168 M=1,NCOLS6                                                  INDAT1.......28700
         DO 166 MM=1,7                                                   INDAT1.......28800
            IF (LCOL(M).EQ.K6SYM(MM)) THEN                               INDAT1.......28900
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN                          INDAT1.......29000
                  ERRCOD = 'INP-8C-1'                                    INDAT1.......29100
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......29200
	            RETURN
               END IF                                                    INDAT1.......29300
               IF ((MM.EQ.4).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......29400
                  ERRCOD = 'INP-8C-2'                                    INDAT1.......29500
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......29600
	            RETURN
               END IF                                                    INDAT1.......29700
               IF ((MM.EQ.7).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......29800
                  ERRCOD = 'INP-8C-4'                                    INDAT1.......29900
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......30000
	            RETURN
               END IF                                                    INDAT1.......30100
               J6COL(M) = MM                                             INDAT1.......30200
               GOTO 168                                                  INDAT1.......30300
            END IF                                                       INDAT1.......30400
  166    CONTINUE                                                        INDAT1.......30500
         ERRCOD = 'INP-8C-3'                                             INDAT1.......30600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......30700
	   RETURN
  168 CONTINUE                                                           INDAT1.......30800
!      WRITE(K3,170) (M,VARNK6(J6COL(M)),M=1,NCOLS6)                      INDAT1.......30900
!  170 FORMAT (13X,'COLUMN ',I1,':',2X,A)                                 INDAT1.......31000
C                                                                        INDAT1.......31100
C.....INPUT DATASET 8D:  OUTPUT CONTROLS AND OPTIONS FOR OBSERVATIONS    INDAT1.......31200
      NOBCYC = ITMAX + 1                                                 INDAT1.......31300
      IF (NOBSN-1.EQ.0) GOTO 999                                         INDAT1.......31400
C.....NOBS IS ACTUAL NUMBER OF OBSERVATION POINTS                        INDAT1.......31500
C.....NTOBS IS MAXIMUM NUMBER OF TIME STEPS WITH OBSERVATIONS            INDAT1.......31600
      NOBS=NOBSN-1                                                       INDAT1.......31700
C.....READ IN OBSERVATION POINTS                                         INDAT1.......31800
      ERRCOD = 'REA-INP-8D'                                              INDAT1.......31900
C.....DO THIS READ NOW TO SKIP ANY COMMENTS AND BLANK LINES.             INDAT1.......32000
C        (BACKSPACE LATER IF IT MUST BE REREAD IN OLD FORMAT.)           INDAT1.......32100
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......32200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) NOBLIN                              INDAT1.......32300
      IF (INERR(1).NE.0) THEN
        CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                   INDAT1.......32400
	  RETURN
	ENDIF
C.....IF OLD (VERSION 2.0) INPUT FORMAT IS BEING USED, CONSTRUCT A       INDAT1.......32500
C        CORRESPONDING OBSERVATION OUTPUT SCHEDULE IF TRANSPORT IS       INDAT1.......32600
C        TRANSIENT.                                                      INDAT1.......32700
      IF (VERNIN.EQ."2.0") THEN                                          INDAT1.......32800
C........SET THE MAX NUMBER OF OBSERVATIONS PER LINE TO THE TOTAL        INDAT1.......32900
C           NUMBER OF OBSERVATIONS.                                      INDAT1.......33000
         NOBLIN = NOBS                                                   INDAT1.......33100
C........SET UP A TEMPORARY ARRAY TO HOLD OBSERVATION NODES.             INDAT1.......33200
         ALLOCATE(INOB(NOBSN))                                           INDAT1.......33300
C........BACKSPACE AND REREAD DATASET IN OLD FORMAT                      INDAT1.......33400
         BACKSPACE(K1)                                                   INDAT1.......33500
         READ(K1,*,IOSTAT=INERR(1)) NOBCYC, (INOB(JJ), JJ=1,NOBSN)       INDAT1.......33600
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               INDAT1.......33700
	      RETURN
	   ENDIF
C........IF THE LAST NODE NUMBER IS NOT ZERO, GENERATE AN ERROR.         INDAT1.......33800
         IF (INOB(NOBSN).NE.0) THEN                                      INDAT1.......33900
            ERRCOD = 'INP-8D-1'                                          INDAT1.......34000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     INDAT1.......34100
	      RETURN
         END IF                                                          INDAT1.......34200
C........IF A NODE NUMBER IS INVALID, GENERATE AN ERROR.                 INDAT1.......34300
         DO 510 JJ=1,NOBS                                                INDAT1.......34400
            IF ((INOB(JJ).LT.1).OR.(INOB(JJ).GT.NN)) THEN                INDAT1.......34500
               ERRCOD = 'INP-8D-2'                                       INDAT1.......34600
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  INDAT1.......34700
  	         RETURN
            END IF                                                       INDAT1.......34800
  510    CONTINUE                                                        INDAT1.......34900
C........IF TRANSPORT IS TRANSIENT, CONSTRUCT A SCHEDULE THAT            INDAT1.......35000
C           CORRESPONDS TO THE CYCLE SPECIFIED BY NOBCYC.                INDAT1.......35100
         IF (ISSTRA.EQ.0) THEN                                           INDAT1.......35200
            SCHDLS(2)%NAME = "-"                                         INDAT1.......35300
            ISTEPI = 0                                                   INDAT1.......35400
            ISTEPL = ITMAX                                               INDAT1.......35500
            ITERM = ISTEPL - ISTEPI                                      INDAT1.......35600
C...........IF NOBCYC=0, SET THE CYCLE TO THE TOTAL NUMBER OF TIME       INDAT1.......35700
C              STEPS, SO THAT THE SCHEDULE CONSISTS OF TIME STEPS        INDAT1.......35800
C              0, 1, AND ITMAX.  (VERSION 2.0 SIMPLY BOMBS IF            INDAT1.......35900
C              NOBCYC=0.)                                                INDAT1.......36000
            IF (NOBCYC.EQ.0) THEN                                        INDAT1.......36100
               ISTEPC = ITERM                                            INDAT1.......36200
            ELSE                                                         INDAT1.......36300
               ISTEPC = IABS(NOBCYC)                                     INDAT1.......36400
            END IF                                                       INDAT1.......36500
            NTORS = INT(ITERM/ISTEPC) + 1                                INDAT1.......36600
            IF (MOD(ITERM,ISTEPC).NE.0) NTORS = NTORS + 1                INDAT1.......36700
            NSTEP = ISTEPI                                               INDAT1.......36800
            DENTS => SCHDLS(ISCHTS)%SLIST                                INDAT1.......36900
            JT = 0                                                       INDAT1.......37000
            DO 580 NT=1,NTORS                                            INDAT1.......37100
               NSTEP = MIN(ISTEPL, ISTEPI + (NT - 1)*ISTEPC)             INDAT1.......37200
               DO WHILE (NSTEP.GT.JT)                                    INDAT1.......37300
                  DENTS => DENTS%NENT                                    INDAT1.......37400
                  JT = JT + 1                                            INDAT1.......37500
               END DO                                                    INDAT1.......37600
               STEP = DENTS%DVALU2                                       INDAT1.......37700
               TIME = DENTS%DVALU1                                       INDAT1.......37800
               CALL LLDINS(SCHDLS(2)%LLEN, SCHDLS(2)%SLIST, TIME, STEP,  INDAT1.......37900
     1            SCHDLS(2)%SLAST)                                       INDAT1.......38000
  580       CONTINUE                                                     INDAT1.......38100
C...........IF NOBCYC>=0, INCLUDE TIME STEP 1 IF NOT ALREADY INCLUDED.   INDAT1.......38200
            IF ((NOBCYC.GE.0).AND.(NOBCYC.NE.1)) THEN                    INDAT1.......38300
               DENTS => SCHDLS(ISCHTS)%SLIST                             INDAT1.......38400
               STEP = DNINT(DBLE(1))                                     INDAT1.......38500
               TIME = DENTS%NENT%DVALU1                                  INDAT1.......38600
               CALL LLDINS(SCHDLS(2)%LLEN, SCHDLS(2)%SLIST, TIME, STEP,  INDAT1.......38700
     1            SCHDLS(2)%SLAST)                                       INDAT1.......38800
            END IF                                                       INDAT1.......38900
         END IF                                                          INDAT1.......39000
C........CONVERT NODES TO GENERALIZED OBSERVATION POINTS.  THE POINTS    INDAT1.......39100
C           ARE NAMED "NODE_#", WHERE # IS THE NODE NUMBER.              INDAT1.......39200
         DO 540 I=1,NOBS                                                 INDAT1.......39300
!            WRITE(OBSPTS(I)%NAME,*) INOB(I)                              INDAT1.......39400
            OBSPTS(I)%NAME = "NODE_" // ADJUSTL(OBSPTS(I)%NAME)          INDAT1.......39500
            OBSPTS(I)%SCHED = "-"                                        INDAT1.......39600
            OBSPTS(I)%FRMT = "OBS"                                       INDAT1.......39700
            OBSPTS(I)%L = INOB(I)                                        INDAT1.......39800
  540    CONTINUE                                                        INDAT1.......39900
C........DEALLOCATE TEMPORARY ARRAY.                                     INDAT1.......40000
         DEALLOCATE(INOB)                                                INDAT1.......40100
C........SKIP PAST THE CODE THAT READS A LIST OF GENERALIZED             INDAT1.......40200
C           OBSERVATION POINTS.                                          INDAT1.......40300
         GOTO 820                                                        INDAT1.......40400
      END IF                                                             INDAT1.......40500
C.....READ THE LIST OF GENERALIZED OBSERVATION POINTS.                   INDAT1.......40600
      NOBCYC = -1                                                        INDAT1.......40700
      DO 690 I=1,NOBS                                                    INDAT1.......40800
C........READ THE OBSERVATION NAME.                                      INDAT1.......40900
         CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                              INDAT1.......41000
	   IF (IERROR.NE.0) RETURN
         READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM                           INDAT1.......41100
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               INDAT1.......41200
	      RETURN
	   ENDIF
C........IF END-OF-LIST MARKER ENCOUNTERED TOO SOON, GENERATE ERROR.     INDAT1.......41300
         IF (OBSNAM.EQ.'-') THEN                                         INDAT1.......41400
            ERRCOD = 'INP-8D-4'                                          INDAT1.......41500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     INDAT1.......41600
	      RETURN
         END IF                                                          INDAT1.......41700
C........READ IN (X,Y,Z) OR (X,Y) COORDINATES, DEPENDING ON PROBLEM      INDAT1.......41800
C           DIMENSIONALITY, AS WELL AS OUTPUT SCHEDULE AND FORMAT.       INDAT1.......41900
         IF (KTYPE(1).EQ.3) THEN                                         INDAT1.......42000
            READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM, XOBS, YOBS, ZOBS,     INDAT1.......42100
     1         CDUM80, OBSFMT                                            INDAT1.......42200
         ELSE                                                            INDAT1.......42300
            READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM, XOBS, YOBS,           INDAT1.......42400
     1         CDUM80, OBSFMT                                            INDAT1.......42500
            ZOBS = 0D0                                                   INDAT1.......42600
         END IF                                                          INDAT1.......42700
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               INDAT1.......42800
	      RETURN
	   ENDIF
         IF (LEN_TRIM(CDUM80).GT.10) THEN                                INDAT1.......42900
            ERRCOD = 'INP-8D-6'                                          INDAT1.......43000
            CHERR(1) = CDUM80                                            INDAT1.......43100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     INDAT1.......43200
	      RETURN
         END IF                                                          INDAT1.......43300
         OBSSCH = CDUM80                                                 INDAT1.......43400
         OBSPTS(I)%NAME = OBSNAM                                         INDAT1.......43500
         OBSPTS(I)%X = XOBS                                              INDAT1.......43600
         OBSPTS(I)%Y = YOBS                                              INDAT1.......43700
         OBSPTS(I)%Z = ZOBS                                              INDAT1.......43800
         IF (ISSTRA.EQ.1) THEN                                           INDAT1.......43900
            OBSPTS(I)%SCHED = "TIME_STEPS"                               INDAT1.......44000
         ELSE                                                            INDAT1.......44100
            OBSPTS(I)%SCHED = OBSSCH                                     INDAT1.......44200
         END IF                                                          INDAT1.......44300
         OBSPTS(I)%FRMT = OBSFMT                                         INDAT1.......44400
  690 CONTINUE                                                           INDAT1.......44500
C.....READ ONE MORE LINE TO CHECK FOR THE END-OF-LIST MARKER ('-').      INDAT1.......44600
C        IF NOT FOUND, GENERATE ERROR.                                   INDAT1.......44700
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......44800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM                              INDAT1.......44900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  INDAT1.......45000
	   RETURN
	ENDIF
      IF (OBSNAM.NE.'-') THEN                                            INDAT1.......45100
         ERRCOD = 'INP-8D-4'                                             INDAT1.......45200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        INDAT1.......45300
	   RETURN
      END IF                                                             INDAT1.......45400
C                                                                        INDAT1.......45500
C.....CONDENSE SCHEDULE AND FILE TYPE INFORMATION FROM OBSDAT INTO       INDAT1.......45600
C        ARRAY OFP, CHECKING FOR UNDEFINED SCHEDULES                     INDAT1.......45700
  820 ALLOCATE (OFP(NSCH*2))                                             INDAT1.......45800
      IF (ISSTRA.EQ.1) THEN                                              INDAT1.......45900
         NFLOMX=0                                                        INDAT1.......46000
         DO 840 I=1,NOBS                                                 INDAT1.......46100
            DO 835 J=1,NFLOMX                                            INDAT1.......46200
               IF (OBSPTS(I)%FRMT.EQ.OFP(J)%FRMT) GOTO 840               INDAT1.......46300
  835       CONTINUE                                                     INDAT1.......46400
            NFLOMX = NFLOMX + 1                                          INDAT1.......46500
            OFP(NFLOMX)%ISCHED = 2                                       INDAT1.......46600
            OFP(NFLOMX)%FRMT = OBSPTS(I)%FRMT                            INDAT1.......46700
  840    CONTINUE                                                        INDAT1.......46800
      ELSE                                                               INDAT1.......46900
         NFLOMX = 0                                                      INDAT1.......47000
         DO 860 I=1,NOBS                                                 INDAT1.......47100
            DO 850 NS=1,NSCH                                             INDAT1.......47200
               IF (OBSPTS(I)%SCHED.EQ.SCHDLS(NS)%NAME) THEN              INDAT1.......47300
                  INS = NS                                               INDAT1.......47400
                  GOTO 852                                               INDAT1.......47500
               END IF                                                    INDAT1.......47600
  850       CONTINUE                                                     INDAT1.......47700
            ERRCOD = 'INP-8D-5'                                          INDAT1.......47800
            CHERR(1) = OBSPTS(I)%SCHED                                   INDAT1.......47900
            CHERR(2) = OBSPTS(I)%NAME                                    INDAT1.......48000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     INDAT1.......48100
	      RETURN
  852       DO 855 J=1,NFLOMX                                            INDAT1.......48200
               IF ((OBSPTS(I)%SCHED.EQ.SCHDLS(OFP(J)%ISCHED)%NAME).AND.  INDAT1.......48300
     1             (OBSPTS(I)%FRMT.EQ.OFP(J)%FRMT)) GOTO 860             INDAT1.......48400
  855       CONTINUE                                                     INDAT1.......48500
            NFLOMX = NFLOMX + 1                                          INDAT1.......48600
            OFP(NFLOMX)%ISCHED = INS                                     INDAT1.......48700
            OFP(NFLOMX)%FRMT = OBSPTS(I)%FRMT                            INDAT1.......48800
  860    CONTINUE                                                        INDAT1.......48900
      END IF                                                             INDAT1.......49000
C                                                                        INDAT1.......49100
C.....ASSIGN UNIT NUMBERS AND OPEN FILE UNITS FOR OBSERVATION OUTPUT     INDAT1.......49200
C        FILES.                                                          INDAT1.......49300
      CALL FOPEN(IERROR)                                                       INDAT1.......49400
c RBW begin change
	IF (IERROR.NE.0) RETURN
c RBW end change
C                                                                        INDAT1.......49500
C.....OUTPUT OBSERVATION FILE INFORMATION                                INDAT1.......49600
!      IF (ISSTRA.EQ.1) THEN                                              INDAT1.......49700
!         WRITE(K3,868) IUNIO(1),FNAMO(1)                                 INDAT1.......49800
!  868    FORMAT (//13X,'.OBS AND .OBC FILES'/13X,'-------------------'// INDAT1.......49900
!     1      (13X,'UNIT ',I7,4X,'ASSIGNED TO ',A80))                      INDAT1.......50000
!         WRITE(K3,869)                                                   INDAT1.......50100
!  869    FORMAT (/13X,'NOTE: BECAUSE FLOW AND TRANSPORT ARE STEADY-',    INDAT1.......50200
!     1      'STATE, USER-DEFINED SCHEDULES ARE NOT IN EFFECT.  '         INDAT1.......50300
!     2      /13X,'STEADY-STATE OBSERVATIONS WILL BE WRITTEN TO THE ',    INDAT1.......50400
!     3      'APPROPRIATE OUTPUT FILES.')                                 INDAT1.......50500
!      ELSE IF (VERNIN.NE."2.0") THEN                                     INDAT1.......50600
!         WRITE(K3,870) (SCHDLS(OFP(J)%ISCHED)%NAME,OFP(J)%FRMT,          INDAT1.......50700
!     1      IUNIO(J),FNAMO(J),J=1,NFLOMX)                                INDAT1.......50800
!  870    FORMAT (//13X,'.OBS AND .OBC FILES'/13X,'-------------------'// INDAT1.......50900
!     1      (13X,'SCHEDULE ',A,', FORMAT ',A,', UNIT ',I7,4X,            INDAT1.......51000
!     2      'ASSIGNED TO ',A80))                                         INDAT1.......51100
!      ELSE                                                               INDAT1.......51200
!         WRITE(K3,868) IUNIO(1),FNAMO(1)                                 INDAT1.......51300
!        WRITE(K3,872) NOBCYC, SCHDLS(2)%LLEN                            INDAT1.......51400
!  872    FORMAT (/13X,'NOTE: OBSERVATION OUTPUT CYCLING ',               INDAT1.......51500
!     1      'INFORMATION WAS ENTERED USING THE OLD (VERSION 2D3D.1) '    INDAT1.......51600
!     2      'INPUT FORMAT.'/13X,'OBSERVATIONS WILL BE MADE EVERY ',I8,   INDAT1.......51700
!     3      ' TIME STEPS, AS WELL AS ON THE FIRST AND LAST TIME STEP,'   INDAT1.......51800
!    4      /13X,'FOR A TOTAL OF ',I8,' TIME STEPS.')                    INDAT1.......51900
!      END IF                                                             INDAT1.......52000
C                                                                        INDAT1.......52100
C.....OUTPUT GENERALIZED OBSERVATION POINT INFORMATION.                  INDAT1.......52200
!      WRITE(K3,982)                                                      INDAT1.......52300
!  982 FORMAT(////11X,'O B S E R V A T I O N   P O I N T S')              INDAT1.......52400
C.....3D PROBLEM.                                                        INDAT1.......52500
!      IF (KTYPE(1).EQ.3) THEN                                            INDAT1.......52600
C........WRITE HEADER.                                                   INDAT1.......52700
!         WRITE(K3,987)                                                   INDAT1.......52800
!  987    FORMAT(                                                         INDAT1.......52900
!     1        //13X,'NAME',42X,'COORDINATES',37X,'SCHEDULE',4X,'FORMAT'  INDAT1.......53000
!     2         /13X,'----',42X,'-----------',37X,'--------',4X,'------') INDAT1.......53100
C........PRINT INFORMATION FOR EACH POINT.  IF POINTS WERE CONVERTED     INDAT1.......53200
C           FROM NODES, COORDINATES HAVE YET TO BE READ IN, SO PUT IN    INDAT1.......53300
C           A PLACEHOLDER.                                               INDAT1.......53400
!         IF (NOBCYC.NE.-1) THEN                                          INDAT1.......53500
!            DO 989 JJ=1,NOBS                                             INDAT1.......53600
!               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......53700
!               IF (ISSTRA.EQ.1) THEN                                     INDAT1.......53800
!                  OBSSCH = '-'                                           INDAT1.......53900
!               ELSE                                                      INDAT1.......54000
!                  OBSSCH = OBSPTS(JJ)%SCHED                              INDAT1.......54100
!               END IF                                                    INDAT1.......54200
!               WRITE(K3,988) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),    INDAT1.......54300
!     1            OBSSCH,OBSPTS(JJ)%FRMT                                 INDAT1.......54400
!  988          FORMAT(13X,A,1X,A,1X,                                     INDAT1.......54500
!     1            '( _______ TO BE READ FROM DATASET 14 _______ )',      INDAT1.......54600
!     2            3X,A,2X,A)                                             INDAT1.......54700
!  989       CONTINUE                                                     INDAT1.......54800
!         ELSE                                                            INDAT1.......54900
!            DO 991 JJ=1,NOBS                                             INDAT1.......55000
!               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......55100
!               IF (ISSTRA.EQ.1) THEN                                     INDAT1.......55200
!                  OBSSCH = '-'                                           INDAT1.......55300
!               ELSE                                                      INDAT1.......55400
!                  OBSSCH = OBSPTS(JJ)%SCHED                              INDAT1.......55500
!               END IF                                                    INDAT1.......55600
!               WRITE(K3,990) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),    INDAT1.......55700
!     1            OBSPTS(JJ)%X,OBSPTS(JJ)%Y,OBSPTS(JJ)%Z,                INDAT1.......55800
!     2            OBSSCH,OBSPTS(JJ)%FRMT                                 INDAT1.......55900
!  990          FORMAT(13X,A,1X,A,1X,'(',2(1PE14.7,','),1PE14.7,')',      INDAT1.......56000
!     1            3X,A,2X,A)                                             INDAT1.......56100
!  991       CONTINUE                                                     INDAT1.......56200
!         END IF                                                          INDAT1.......56300
C.....2D PROBLEM.                                                        INDAT1.......56400
!      ELSE                                                               INDAT1.......56500
C........WRITE HEADER.                                                   INDAT1.......56600
!         WRITE(K3,993)                                                   INDAT1.......56700
!  993    FORMAT(                                                         INDAT1.......56800
!     1        //13X,'NAME',42X,'COORDINATES',22X,'SCHEDULE',4X,'FORMAT'  INDAT1.......56900
!     2         /13X,'----',42X,'-----------',22X,'--------',4X,'------') INDAT1.......57000
C........PRINT INFORMATION FOR EACH POINT.  IF POINTS WERE CONVERTED     INDAT1.......57100
C           FROM NODES, COORDINATES HAVE YET TO BE READ IN, SO PUT IN    INDAT1.......57200
C           A PLACEHOLDER.                                               INDAT1.......57300
!         IF (NOBCYC.NE.-1) THEN                                          INDAT1.......57400
!            DO 995 JJ=1,NOBS                                             INDAT1.......57500
!               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......57600
!               IF (ISSTRA.EQ.1) THEN                                     INDAT1.......57700
!                  OBSSCH = '-'                                           INDAT1.......57800
!               ELSE                                                      INDAT1.......57900
!                  OBSSCH = OBSPTS(JJ)%SCHED                              INDAT1.......58000
!               END IF                                                    INDAT1.......58100
!               WRITE(K3,994) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),    INDAT1.......58200
!     1            OBSSCH,OBSPTS(JJ)%FRMT                                 INDAT1.......58300
!  994          FORMAT(13X,A,1X,A,1X,'( TO BE READ FROM DATASET 14  )',   INDAT1.......58400
!     1            3X,A,2X,A)                                             INDAT1.......58500
!  995       CONTINUE                                                     INDAT1.......58600
!         ELSE                                                            INDAT1.......58700
!            DO 997 JJ=1,NOBS                                             INDAT1.......58800
!               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......58900
!               IF (ISSTRA.EQ.1) THEN                                     INDAT1.......59000
!                  OBSSCH = '-'                                           INDAT1.......59100
!               ELSE                                                      INDAT1.......59200
!                  OBSSCH = OBSPTS(JJ)%SCHED                              INDAT1.......59300
!               END IF                                                    INDAT1.......59400
!               WRITE(K3,996) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),    INDAT1.......59500
!     1            OBSPTS(JJ)%X,OBSPTS(JJ)%Y,                             INDAT1.......59600
!     2            OBSSCH,OBSPTS(JJ)%FRMT                                 INDAT1.......59700
!  996          FORMAT(13X,A,1X,A,1X,'(',1PE14.7,',',1PE14.7,')',         INDAT1.......59800
!     1            3X,A,2X,A)                                             INDAT1.......59900
!  997       CONTINUE                                                     INDAT1.......60000
!         END IF                                                          INDAT1.......60100
!      END IF                                                             INDAT1.......60200
  999 CONTINUE                                                           INDAT1.......60300
C                                                                        INDAT1.......60400
C.....INPUT DATASET 8E:  OUTPUT CONTROLS AND OPTIONS FOR BCOF, BCOP,     INDAT1.......60500
C        BCOS, AND BCOU FILES                                            INDAT1.......60600
      IF ((VERNIN.EQ.'2.0').OR.(VERNIN.EQ.'2.1')) THEN                   INDAT1.......60700
         NBCFPR = HUGE(1)                                                INDAT1.......60800
         NBCSPR = HUGE(1)                                                INDAT1.......60900
         NBCPPR = HUGE(1)                                                INDAT1.......61000
         NBCUPR = HUGE(1)                                                INDAT1.......61100
C........CINACT IS IRRELEVANT IN THIS CASE                               INDAT1.......61200
         GOTO 1100                                                       INDAT1.......61300
      END IF                                                             INDAT1.......61400
      ERRCOD = 'REA-INP-8E'                                              INDAT1.......61500
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......61600
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) NBCFPR, NBCSPR, NBCPPR, NBCUPR,     INDAT1.......61700
     1   CINACT                                                          INDAT1.......61800
      IF (INERR(1).NE.0) THEN
        CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  INDAT1.......61900
	  RETURN
	ENDIF
      IF (CINACT.EQ.'Y') THEN                                            INDAT1.......62000
         KINACT = +1                                                     INDAT1.......62100
      ELSE IF (CINACT.EQ.'N') THEN                                       INDAT1.......62200
         KINACT = 0                                                      INDAT1.......62300
      ELSE                                                               INDAT1.......62400
         ERRCOD = 'INP-8E-1'                                             INDAT1.......62500
         CHERR(1) = 'CINACT'                                             INDAT1.......62600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        INDAT1.......62700
	   RETURN
      END IF                                                             INDAT1.......62800
!      IF (ME.EQ.-1) THEN                                                 INDAT1.......62900
!         WRITE(K3,1050) NBCFPR, NBCSPR, NBCPPR, NBCUPR                   INDAT1.......63000
! 1050    FORMAT(//13X,'.BCOF, .BCOS, .BCOP, AND .BCOU FILES'             INDAT1.......63100
!     1           /13X,'------------------------------------'             INDAT1.......63200
!     2      //13X,I8,3X,'PRINTED OUTPUT CYCLE FOR FLUID SOURCES/SINK ',  INDAT1.......63300
!     3        'NODES TO .BCOF FILE (IN TIME STEPS)'                      INDAT1.......63400
!     4       /13X,I8,3X,'PRINTED OUTPUT CYCLE FOR SOLUTE ',              INDAT1.......63500
!     5        'SOURCES/SINK NODES TO .BCOS FILE (IN TIME STEPS)'         INDAT1.......63600
!     6       /13X,I8,3X,'PRINTED OUTPUT CYCLE FOR SPECIFIED PRESSURE ',  INDAT1.......63700
!     7        'NODES TO .BCOP FILE (IN TIME STEPS)'                      INDAT1.......63800
!     8       /13X,I8,3X,'PRINTED OUTPUT CYCLE FOR SPECIFIED ',           INDAT1.......63900
!     9        'CONCENTRATION NODES TO .BCOU FILE (IN TIME STEPS)')       INDAT1.......64000
!      ELSE                                                               INDAT1.......64100
!         WRITE(K3,1051) NBCFPR, NBCSPR, NBCPPR, NBCUPR                   INDAT1.......64200
! 1051    FORMAT(//13X,'.BCOF, .BCOS, .BCOP, AND .BCOU FILES'             INDAT1.......64300
!     1           /13X,'------------------------------------'             INDAT1.......64400
!     2      //13X,I8,3X,'PRINTED OUTPUT CYCLE FOR FLUID SOURCES/SINK ',  INDAT1.......64500
!     3        'NODES TO .BCOF FILE (IN TIME STEPS)'                      INDAT1.......64600
!     4       /13X,I8,3X,'PRINTED OUTPUT CYCLE FOR ENERGY ',              INDAT1.......64700
!     5        'SOURCES/SINK NODES TO .BCOS FILE (IN TIME STEPS)'         INDAT1.......64800
!     6       /13X,I8,3X,'PRINTED OUTPUT CYCLE FOR SPECIFIED PRESSURE ',  INDAT1.......64900
!     7        'NODES TO .BCOP FILE (IN TIME STEPS)'                      INDAT1.......65000
!     8       /13X,I8,3X,'PRINTED OUTPUT CYCLE FOR SPECIFIED ',           INDAT1.......65100
!     9        'TEMPERATURE NODES TO .BCOU FILE (IN TIME STEPS)')         INDAT1.......65200
!      END IF                                                             INDAT1.......65300
!      IF(KINACT.EQ.+1) WRITE(K3,1052)                                    INDAT1.......65400
!      IF(KINACT.EQ.0) WRITE(K3,1053)                                     INDAT1.......65500
! 1052 FORMAT(/13X,'- PRINT INACTIVE BOUNDARY CONDITIONS')                INDAT1.......65600
! 1053 FORMAT(/13X,'- CANCEL PRINT OF INACTIVE BOUNDARY CONDITIONS')      INDAT1.......65700
C                                                                        INDAT1.......65800
C.....INPUT DATASET 9:  FLUID PROPERTIES                                 INDAT1.......65900
 1100 ERRCOD = 'REA-INP-9'                                               INDAT1.......66000
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......66100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) COMPFL,CW,SIGMAW,RHOW0,URHOW0,      INDAT1.......66200
     1   DRWDU,VISC0                                                     INDAT1.......66300
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......54300
	   RETURN
	ENDIF
C.....INPUT DATASET 10:  SOLID MATRIX PROPERTIES                         INDAT1.......66500
      ERRCOD = 'REA-INP-10'                                              INDAT1.......66600
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......66700
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) COMPMA,CS,SIGMAS,RHOS               INDAT1.......66800
      IF (INERR(1).NE.0) THEN
        CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                    INDAT1.......66900
	  RETURN
	ENDIF
!      IF(ME.EQ.+1)                                                       INDAT1.......67000
!     1  WRITE(K3,1210) COMPFL,COMPMA,CW,CS,VISC0,RHOS,RHOW0,DRWDU,       INDAT1.......67100
!     2     URHOW0,SIGMAW,SIGMAS                                          INDAT1.......67200
! 1210 FORMAT('1'////11X,'C O N S T A N T   P R O P E R T I E S   O F',   INDAT1.......67300
!     1   '   F L U I D   A N D   S O L I D   M A T R I X'                INDAT1.......67400
!     2   //11X,1PE15.4,5X,'COMPRESSIBILITY OF FLUID'/11X,1PE15.4,5X,     INDAT1.......67500
!     3   'COMPRESSIBILITY OF POROUS MATRIX'//11X,1PE15.4,5X,             INDAT1.......67600
!     4   'SPECIFIC HEAT CAPACITY OF FLUID',/11X,1PE15.4,5X,              INDAT1.......67700
!     5   'SPECIFIC HEAT CAPACITY OF SOLID GRAIN'//13X,'FLUID VISCOSITY', INDAT1.......67800
!     6   ' IS CALCULATED BY SUTRA AS A FUNCTION OF TEMPERATURE IN ',     INDAT1.......67900
!     7   'UNITS OF {kg/(m*s)}'//11X,1PE15.4,5X,'VISC0, CONVERSION ',     INDAT1.......68000
!     8   'FACTOR FOR VISCOSITY UNITS,  {desired units} = VISC0*',        INDAT1.......68100
!     9   '{kg/(m*s)}'//11X,1PE15.4,5X,'DENSITY OF A SOLID GRAIN'         INDAT1.......68200
!     *   //13X,'FLUID DENSITY, RHOW'/13X,'CALCULATED BY ',               INDAT1.......68300
!     1   'SUTRA IN TERMS OF TEMPERATURE, U, AS:'/13X,'RHOW = RHOW0 + ',  INDAT1.......68400
!     2   'DRWDU*(U-URHOW0)'//11X,1PE15.4,5X,'FLUID BASE DENSITY, RHOW0'  INDAT1.......68500
!     3   /11X,1PE15.4,5X,'COEFFICIENT OF DENSITY CHANGE WITH ',          INDAT1.......68600
!     4   'TEMPERATURE, DRWDU'/11X,1PE15.4,5X,'TEMPERATURE, URHOW0, ',    INDAT1.......68700
!     5   'AT WHICH FLUID DENSITY IS AT BASE VALUE, RHOW0'                INDAT1.......68800
!     6   //11X,1PE15.4,5X,'THERMAL CONDUCTIVITY OF FLUID'                INDAT1.......68900
!     7   /11X,1PE15.4,5X,'THERMAL CONDUCTIVITY OF SOLID GRAIN')          INDAT1.......69000
!      IF(ME.EQ.-1)                                                       INDAT1.......69100
!     1  WRITE(K3,1220) COMPFL,COMPMA,VISC0,RHOS,RHOW0,DRWDU,             INDAT1.......69200
!     2     URHOW0,SIGMAW                                                 INDAT1.......69300
! 1220 FORMAT('1'////11X,'C O N S T A N T   P R O P E R T I E S   O F',   INDAT1.......69400
!     1   '   F L U I D   A N D   S O L I D   M A T R I X'                INDAT1.......69500
!     2   //11X,1PE15.4,5X,'COMPRESSIBILITY OF FLUID'/11X,1PE15.4,5X,     INDAT1.......69600
!     3   'COMPRESSIBILITY OF POROUS MATRIX'                              INDAT1.......69700
!     4   //11X,1PE15.4,5X,'FLUID VISCOSITY'                              INDAT1.......69800
!     4   //11X,1PE15.4,5X,'DENSITY OF A SOLID GRAIN'                     INDAT1.......69900
!     5   //13X,'FLUID DENSITY, RHOW'/13X,'CALCULATED BY ',               INDAT1.......70000
!     6   'SUTRA IN TERMS OF SOLUTE CONCENTRATION, U, AS:',               INDAT1.......70100
!     7   /13X,'RHOW = RHOW0 + DRWDU*(U-URHOW0)'                          INDAT1.......70200
!     8   //11X,1PE15.4,5X,'FLUID BASE DENSITY, RHOW0'                    INDAT1.......70300
!     9   /11X,1PE15.4,5X,'COEFFICIENT OF DENSITY CHANGE WITH ',          INDAT1.......70400
!     *   'SOLUTE CONCENTRATION, DRWDU'                                   INDAT1.......70500
!     1   /11X,1PE15.4,5X,'SOLUTE CONCENTRATION, URHOW0, ',               INDAT1.......70600
!     4   'AT WHICH FLUID DENSITY IS AT BASE VALUE, RHOW0'                INDAT1.......70700
!     5   //11X,1PE15.4,5X,'MOLECULAR DIFFUSIVITY OF SOLUTE IN FLUID')    INDAT1.......70800
C                                                                        INDAT1.......70900
C.....INPUT DATASET 11:  ADSORPTION PARAMETERS                           INDAT1.......71000
      ERRCOD = 'REA-INP-11'                                              INDAT1.......71100
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......71200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) ADSMOD                              INDAT1.......71300
      IF (INERR(1).NE.0) THEN
        CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1.......71400
	  RETURN
	ENDIF
      IF (ADSMOD.NE.'NONE      ') THEN                                   INDAT1.......71500
         READ(INTFIL,*,IOSTAT=INERR(1)) ADSMOD,CHI1,CHI2                 INDAT1.......71600
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......71700
	      RETURN
  	   ENDIF
      END IF                                                             INDAT1.......71800
      IF(ME.EQ.+1) GOTO 1250                                             INDAT1.......71900
!      IF(ADSMOD.EQ.'NONE      ') GOTO 1234                               INDAT1.......72000
!      WRITE(K3,1232) ADSMOD                                              INDAT1.......72100
! 1232 FORMAT(////11X,'A D S O R P T I O N   P A R A M E T E R S'         INDAT1.......72200
!     1   //16X,A10,5X,'EQUILIBRIUM SORPTION ISOTHERM')                   INDAT1.......72300
!      GOTO 1236                                                          INDAT1.......72400
! 1234 WRITE(K3,1235)                                                     INDAT1.......72500
! 1235 FORMAT(////11X,'A D S O R P T I O N   P A R A M E T E R S'         INDAT1.......72600
!     1   //16X,'NON-SORBING SOLUTE')                                     INDAT1.......72700
 1236 IF((ADSMOD.EQ.'NONE      ').OR.(ADSMOD.EQ.'LINEAR    ').OR.             INDAT1.......72800
     1   (ADSMOD.EQ.'FREUNDLICH').OR.(ADSMOD.EQ.'LANGMUIR  ')) GOTO 1238 INDAT1.......72900
      ERRCOD = 'INP-11-1'                                                INDAT1.......73000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                           INDAT1.......73100
	RETURN
 1238 continue
! 1238 IF(ADSMOD.EQ.'LINEAR    ') WRITE(K3,1242) CHI1                     INDAT1.......73200
! 1242 FORMAT(11X,1PE15.4,5X,'LINEAR DISTRIBUTION COEFFICIENT')           INDAT1.......73300
!      IF(ADSMOD.EQ.'FREUNDLICH') WRITE(K3,1244) CHI1,CHI2                INDAT1.......73400
! 1244 FORMAT(11X,1PE15.4,5X,'FREUNDLICH DISTRIBUTION COEFFICIENT'        INDAT1.......73500
!     1   /11X,1PE15.4,5X,'SECOND FREUNDLICH COEFFICIENT')                INDAT1.......73600
      IF(ADSMOD.EQ.'FREUNDLICH'.AND.CHI2.LE.0.D0) THEN                   INDAT1.......73700
         ERRCOD = 'INP-11-2'                                             INDAT1.......73800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......73900
	   RETURN
      ENDIF                                                              INDAT1.......74000
!      IF(ADSMOD.EQ.'LANGMUIR  ') WRITE(K3,1246) CHI1,CHI2                INDAT1.......74100
! 1246 FORMAT(11X,1PE15.4,5X,'LANGMUIR DISTRIBUTION COEFFICIENT'          INDAT1.......74200
!     1   /11X,1PE15.4,5X,'SECOND LANGMUIR COEFFICIENT')                  INDAT1.......74300
! uncomment the following two lines for SUTRA-ICE
!      read (INTFIL,*)
!      read (INTFIL,*)
C                                                                        INDAT1.......74400
C.....INPUT DATASET 11B:  UNSATURATED FLOW AND FREEZING FUNCTION           ! frz_input2 ...
C        PARAMETERS
! rbw begin change
ccc 1250 ERRCOD = 'REA-INP-11B'                                            ! 11b3only ccc
 1250 IF ((VERNIN.EQ."2.0").OR.(VERNIN.EQ."2.1").OR.(VERNIN.EQ."2.2"))     ! 11b3only ...
     1   THEN
         NUIREG = 2
         GOTO 1270
      END IF
      ERRCOD = 'REA-INP-11B'                                               ! ... 11b3only
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)              ! kluge, need to set default van G params for versions prior to 3.0                  
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) NUIREG           ! kluge note: only for V3.0 and later                    
      IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif         
      IF (NUIREG.GT.10) THEN
         ERRCOD = 'INP-11B-1'                                              ! svara
         INERR(1) = NUIREG                                                 ! svara
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
		  return                         ! svara
      END IF
      SWMOD = "NULL"                                                                                     
      RKMOD = "NULL"                                           
      SIMOD = "NULL"        
ccc      IF (NUIREG.GT.10) THEN                                           ! svara ccc ...    
ccc         print *, "ERROR -- NUIREG must be no more than 10."        ! kluge
ccc         stop                                                       ! kluge
ccc      ELSE IF (NUIREG.EQ.0) THEN                                       ! ... svara ccc
      IF (NUIREG.EQ.0) THEN                                               ! svara
         IF (IUNSAT.NE.0) THEN
            ERRCOD = 'INP-11B-2'                                           ! svara
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
		  return                       ! svara
         ELSE IF (IFREEZ.NE.0) THEN
            ERRCOD = 'INP-11B-3'                                           ! svara
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
		  return                       ! svara
         END IF
         GOTO 1270
      END IF                                                            
      DO 1260 NR=1,NUIREG                                               
         CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)
	IF (IERROR.NE.0) RETURN
         READ(INTFIL,*,IOSTAT=INERR(1)) SWMOD(NR)                      
         IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif   
         IF (SWMOD(NR).EQ.'NONE') THEN                                 
            NSWPAR(NR) = 0
         ELSE IF (SWMOD(NR).EQ.'VGEN') THEN                            
            NSWPAR(NR) = 3
            SWPNM(1) = "SWRES"
            SWPNM(2) = "AA"
            SWPNM(3) = "VN"
         ELSE IF (SWMOD(NR).EQ.'BCOR') THEN
            NSWPAR(NR) = 3
            SWPNM(1) = "SWRES"
            SWPNM(2) = "PENT"
            SWPNM(3) = "RLAMB"
         ELSE IF (SWMOD(NR).EQ.'PLIN') THEN
            NSWPAR(NR) = 3
            SWPNM(1) = "SWRES"
            SWPNM(2) = "PENT"
            SWPNM(3) = "PSWRES"
         ELSE IF (SWMOD(NR).EQ.'UDEF') THEN
            READ(INTFIL,*,IOSTAT=INERR(1)) SWMOD(NR), NSWPAR(NR)
            IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif   
            DO 1252 NP=1,NSWPAR(NR)
               WRITE(CDUM10,'(I3)') NP
               SWPNM(NP) = "PARAM " // CDUM10(1:3)
 1252       CONTINUE
         ELSE
ccc            STOP                                                        ! svara ccc
            ERRCOD = 'INP-11B-4'                                           ! svara
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return                       ! svara
         END IF                                                         
         IF (SWMOD(NR).EQ.'UDEF') THEN
            READ(INTFIL,*,IOSTAT=INERR(1))                              
     1         SWMOD(NR),NSWPAR(NR),(SWPAR(NR,NP),NP=1,NSWPAR(NR))      
         ELSE IF (NSWPAR(NR).GT.0) THEN
            READ(INTFIL,*,IOSTAT=INERR(1))                              
     1         SWMOD(NR),(SWPAR(NR,NP),NP=1,NSWPAR(NR))              
         END IF                                                         
         IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif     
         CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)
	IF (IERROR.NE.0) RETURN
         READ(INTFIL,*,IOSTAT=INERR(1)) RKMOD(NR)                      
         IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif  
         IF (RKMOD(NR).EQ.'NONE') THEN
            NRKPAR(NR) = 0
         ELSE IF (RKMOD(NR).EQ.'VGEN') THEN
            NRKPAR(NR) = 2
            RKPNM(1) = "SWRES"
            RKPNM(2) = "VN"
         ELSE IF (RKMOD(NR).EQ.'BCOR') THEN
            NRKPAR(NR) = 2
            RKPNM(1) = "SWRES"
            RKPNM(2) = "RLAMB"
         ELSE IF (RKMOD(NR).EQ.'PLIN') THEN
            NRKPAR(NR) = 2
            RKPNM(1) = "SWRES"
            RKPNM(2) = "RKRES"
         ELSE IF (RKMOD(NR).EQ.'IMPE') THEN
            NRKPAR(NR) = 2
            RKPNM(1) = "OMPOR"
            RKPNM(2) = "RKRES"
         ELSE IF (RKMOD(NR).EQ.'UDEF') THEN
            READ(INTFIL,*,IOSTAT=INERR(1)) RKMOD(NR), NRKPAR(NR)
            IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif  
            DO 1254 NP=1,NRKPAR(NR)
               WRITE(CDUM10,'(I3)') NP
               RKPNM(NP) = "PARAM " // CDUM10(1:3)
 1254       CONTINUE
         ELSE
ccc            STOP                                                        ! svara ccc
            ERRCOD = 'INP-11B-5'                                           ! svara
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return                       ! svara
         END IF
         IF (RKMOD(NR).EQ.'UDEF') THEN
            READ(INTFIL,*,IOSTAT=INERR(1))                              
     1         RKMOD(NR),NRKPAR(NR),(RKPAR(NR,NP),NP=1,NRKPAR(NR))      
         ELSE IF (NRKPAR(NR).GT.0) THEN
            READ(INTFIL,*,IOSTAT=INERR(1))                              
     1         RKMOD(NR),(RKPAR(NR,NP),NP=1,NRKPAR(NR))              
         END IF                                                         
         IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif  
         CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)
	IF (IERROR.NE.0) RETURN
         READ(INTFIL,*,IOSTAT=INERR(1)) SIMOD(NR)                      
         IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif  
         IF (SIMOD(NR).EQ.'NONE') THEN
            NSIPAR(NR) = 0
         ELSE IF (SIMOD(NR).EQ.'EXPO') THEN                           
            NSIPAR(NR) = 2
            SIPNM(1) = "SWRESI"
            SIPNM(2) = "W"
         ELSE IF (SIMOD(NR).EQ.'PLIN') THEN
            NSIPAR(NR) = 2
            SIPNM(1) = "SWRESI"
            SIPNM(2) = "TSWRESI"
         ELSE IF (SIMOD(NR).EQ.'UDEF') THEN
            READ(INTFIL,*,IOSTAT=INERR(1)) SIMOD(NR), NSIPAR(NR)
            IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif  
            DO 1256 NP=1,NSIPAR(NR)
               WRITE(CDUM10,'(I3)') NP
               SIPNM(NP) = "PARAM " // CDUM10(1:3)
 1256       CONTINUE
         ELSE
ccc            STOP                                                        ! svara ccc
            ERRCOD = 'INP-11B-6'                                           ! svara
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return                       ! svara
         END IF
         IF (SIMOD(NR).EQ.'UDEF') THEN
            READ(INTFIL,*,IOSTAT=INERR(1))                              
     1         SIMOD(NR),NSIPAR(NR),(SIPAR(NR,NP),NP=1,NSIPAR(NR))      
         ELSE IF (NSIPAR(NR).GT.0) THEN
            READ(INTFIL,*,IOSTAT=INERR(1))                              
     1         SIMOD(NR),(SIPAR(NR,NP),NP=1,NSIPAR(NR))              
         END IF                                                         
         IF (INERR(1).NE.0) then
		    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)
			return
	      endif    
 1260 CONTINUE
 1270 continue
! 1270 WRITE(K3,1272)                                                    
! 1272 FORMAT(////11X,'U N S A T U R A T E D   F L O W   A N D   ',        ! bug fix during svara
!     1   'F R E E Z I N G   F U N C T I O N   P A R A M E T E R S')
      IF (IUNSAT.EQ.0) THEN
!         WRITE(K3,1274)
! 1274    FORMAT(/16X,'SATURATED FLOW IS BEING SIMULATED')
      END IF
      IF (IFREEZ.EQ.0) THEN
!         WRITE(K3,1276)
! 1276    FORMAT(/16X,'SATURATED FLOW IS BEING SIMULATED')
      END IF
      IF ((IUNSAT.EQ.0).AND.(IFREEZ.EQ.0)) GOTO 1368
      DO 1298 NR=1,NUIREG
!         WRITE(K3,1278) NR
! 1278    FORMAT(/16X,"REGION",I4/16X,"---------")                       
         IF (IUNSAT.NE.0) THEN
!            WRITE(K3,1280) SWMOD(NR), "WATER SATURATION MODEL"   
! 1280       FORMAT(/11X,11X,A4,5X,A)                                   
            IF (NSWPAR(NR).GT.0) THEN                                  
               DO 1284 NP=1,NSWPAR(NR)                                 
!                  WRITE(K3,1282) SWPAR(NR,NP),SWPNM(NP)                
! 1282             FORMAT(11X,1PE15.4,5X,A10)                           
 1284          CONTINUE                                                 
            END IF                                                     
         END IF
         IF ((IUNSAT.NE.0).AND.(IFREEZ.NE.0)) THEN
 !           WRITE(K3,1286) SIMOD(NR), "RELATIVE PERMEABILITY MODEL" 
 !1286       FORMAT(/11X,11X,A4,5X,A)                                   
            IF (NRKPAR(NR).GT.0) THEN                                  
               DO 1290 NP=1,NRKPAR(NR)                                 
!                  WRITE(K3,1288) RKPAR(NR,NP),RKPNM(NP)                
! 1288             FORMAT(11X,1PE15.4,5X,A10)                           
 1290          CONTINUE                                                 
            END IF                                                     
         END IF
         IF (IFREEZ.NE.0) THEN
!            WRITE(K3,1292) SIMOD(NR), "ICE SATURATION MODEL"   
! 1292       FORMAT(/11X,11X,A4,5X,A)                                   
            IF (NSIPAR(NR).GT.0) THEN                                  
               DO 1296 NP=1,NSIPAR(NR)                                 
!                  WRITE(K3,1294) SIPAR(NR,NP),SIPNM(NP)                
! 1294             FORMAT(11X,1PE15.4,5X,A10)                           
 1296          CONTINUE                                                 
            END IF                                                     
         END IF
 1298 CONTINUE
C                                                                             ! ... frz_input2
cccC.....INPUT DATASET 11B:  UNSATURATED FLOW FUNCTION PARAMETERS             ! funsat ..., frz_input2 ccc ...
ccc 1250 ERRCOD = 'REA-INP-11B'                          ! kluge, need to update error handling
ccc      CALL READIF(K1, 0, INTFIL, ERRCOD)              ! kluge, need to set default van G params for versions prior to 3.0                  
cccccc      READ(INTFIL,*,IOSTAT=INERR(1)) UNSMOD, NUREG                     ! frz_input ccc           
ccc      READ(INTFIL,*,IOSTAT=INERR(1)) NUREG                                ! frz_input
ccc      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)       
ccc      UNSMOD = "NULL"                                                     ! frz_input
ccc      IF (NUREG.GT.10) THEN                                               ! frz_input ...
ccc         print *, "ERROR -- NUREG must be no more than 10."         ! kluge
ccc         stop                                                       ! kluge
ccc      ELSE IF (NUREG.EQ.0) THEN
ccc         IF (IUNSAT.NE.0) THEN
ccc            print *,"ERROR -- Unsaturated parameters must be ",         ! kluge
ccc     1         "specified (NUREG>0) when flow is UNSATURATED."          ! kluge
ccc            stop                                                        ! kluge
ccc         END IF
ccc         GOTO 1252
ccc      END IF                                                              ! ... frz_input
cccccc      IF (UNSMOD.EQ.'VGEN') THEN                                       ! frz_input ccc ...
cccccc         NUPAR = 3
cccccc      ELSE IF (UNSMOD.EQ.'BCOR') THEN
cccccc         NUPAR = 4
cccccc      ELSE IF (UNSMOD.EQ.'PLIN') THEN
cccccc         NUPAR = 4
cccccc      ELSE IF (UNSMOD.EQ.'UDEF') THEN
cccccc         READ(INTFIL,*,IOSTAT=INERR(1)) UNSMOD, NUREG, NUPAR
cccccc         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
cccccc      ELSE
cccccc         STOP                ! kluge, need error handling
cccccc      END IF                                                           ! ... frz_input ccc
ccc      DO 1251 NR=1,NUREG                                               
ccc         CALL READIF(K1, 0, INTFIL, ERRCOD)
cccccc         READ(INTFIL,*,IOSTAT=INERR(1))(FUNPAR(NR,NP),NP=1,NUPAR)      ! frz_input ccc
ccc         READ(INTFIL,*,IOSTAT=INERR(1)) UNSMOD(NR)                        ! frz_input
ccc         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
ccc         IF (UNSMOD(NR).EQ.'NONE') THEN                                   ! frz_input ...
ccc            NUPAR(NR) = 0
ccc         ELSE IF (UNSMOD(NR).EQ.'VGEN') THEN                                 
ccc            NUPAR(NR) = 3
ccc         ELSE IF (UNSMOD(NR).EQ.'BCOR') THEN
ccc            NUPAR(NR) = 4
ccc         ELSE IF (UNSMOD(NR).EQ.'PLIN') THEN
ccc            NUPAR(NR) = 4
ccc         ELSE IF (UNSMOD(NR).EQ.'UDEF') THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1)) UNSMOD(NR), NUPAR(NR)
ccc            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
ccc         ELSE
ccc            STOP                ! kluge, need error handling
ccc         END IF                                                         
ccc         IF (UNSMOD(NR).EQ.'UDEF') THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1))                              
ccc     1         UNSMOD(NR),NUPAR(NR),(FUNPAR(NR,NP),NP=1,NUPAR(NR))              
ccc         ELSE IF (NUPAR(NR).GT.0) THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1))                              
ccc     1         UNSMOD(NR),(FUNPAR(NR,NP),NP=1,NUPAR(NR))              
ccc         END IF                                                         
ccc         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)      ! ... frz_input
ccc 1251 CONTINUE
cccccc      WRITE(K3,1252)                                                   ! frz_input ccc
cccccc 1252 FORMAT(////11X,'U N S A T U R A T E D   F L O W   ',             ! frz_input ccc
ccc 1252 WRITE(K3,1253)                                                      ! frz_input
ccc 1253 FORMAT(////11X,'U N S A T U R A T E D   F L O W   ',                ! frz_input
ccc     1   'F U N C T I O N   P A R A M E T E R S')
ccc      IF(IUNSAT.EQ.0) THEN
cccccc         WRITE(K3,1253)                                                ! frz_input ccc
cccccc 1253    FORMAT(/16X,'SATURATED FLOW IS BEING SIMULATED')              ! frz_input ccc
ccc         WRITE(K3,1254)
ccc 1254    FORMAT(/16X,'SATURATED FLOW IS BEING SIMULATED')
cccccc         GOTO 1268                                                    ! frz_input ccc
ccc         GOTO 1368                                                       ! frz_input
ccc      END IF                                             
cccccc      WRITE(K3,1254) UNSMOD                                           ! frz_input ccc ...
cccccc 1254 FORMAT(/16X,A10,5X,'UNSATURATED FLOW FUNCTION MODEL')           
cccccc      IF (UNSMOD.EQ.'VGEN') THEN                                  
cccccc         FUNPNM(1) = "SWRES"
cccccc         FUNPNM(2) = "AA"
cccccc         FUNPNM(3) = "VN"
cccccc      ELSE IF (UNSMOD.EQ.'BCOR') THEN
cccccc         FUNPNM(1) = "SWRES"
cccccc         FUNPNM(2) = "PENT"
cccccc         FUNPNM(3) = "LAMBDA"
cccccc      ELSE IF (UNSMOD.EQ.'PLIN') THEN
cccccc         FUNPNM(1) = "SWRES"
cccccc         FUNPNM(2) = "PENT"
cccccc         FUNPNM(3) = "PSWRES"
cccccc         FUNPNM(4) = "RKRES"
cccccc      ELSE IF (UNSMOD.EQ.'UDEF') THEN
cccccc         DO 1255 NP=1,NUPAR
cccccc            WRITE(CDUM10,'(I3)') NP
cccccc            FUNPNM(NP) = "PARAM " // CDUM10(1:3)
cccccc 1255    CONTINUE
cccccc      END IF                                                          ! ... frz_input ccc
ccc      DO 1262 NR=1,NUREG
ccc         IF (UNSMOD(NR).EQ.'VGEN') THEN                                  
ccc            FUNPNM(1) = "SWRES"
ccc            FUNPNM(2) = "AA"
ccc            FUNPNM(3) = "VN"
ccc         ELSE IF (UNSMOD(NR).EQ.'BCOR') THEN
ccc            FUNPNM(1) = "SWRES"
ccc            FUNPNM(2) = "PENT"
ccc            FUNPNM(3) = "LAMBDA"
ccc         ELSE IF (UNSMOD(NR).EQ.'PLIN') THEN
ccc            FUNPNM(1) = "SWRES"
ccc            FUNPNM(2) = "PENT"
ccc            FUNPNM(3) = "PSWRES"
ccc            FUNPNM(4) = "RKRES"
ccc         ELSE IF (UNSMOD(NR).EQ.'UDEF') THEN
ccc            DO 1255 NP=1,NUPAR(NR)
ccc               WRITE(CDUM10,'(I3)') NP
ccc               FUNPNM(NP) = "PARAM " // CDUM10(1:3)
ccc 1255       CONTINUE
ccc         END IF
ccc         WRITE(K3,1256) NR
cccccc 1256    FORMAT(/16X,"ZONE",I4/16X,"--------")                         ! frz_input ccc
ccc 1256    FORMAT(/16X,"REGION",I4/16X,"---------")                         ! frz_input
ccc         WRITE(K3,1257) UNSMOD(NR), "UNSATURATED FLOW FUNCTION MODEL"     ! frz_input
ccc 1257    FORMAT(/11X,11X,A4,5X,A)                                         ! frz_input
ccc         IF (NUPAR(NR).GT.0) THEN                                         ! frz_input
ccc            DO 1260 NP=1,NUPAR(NR)                                        ! frz_input indented
ccc               WRITE(K3,1258) FUNPAR(NR,NP),FUNPNM(NP)                    ! frz_input indented
ccc 1258          FORMAT(11X,1PE15.4,5X,A10)                                 ! frz_input indented
ccc 1260       CONTINUE                                                      ! frz_input indented
ccc         END IF                                                           ! frz_input
ccc 1262 CONTINUE
cccC                                                                          ! ... funsat
cccC.....INPUT DATASET 11C:  FREEZING FUNCTION PARAMETERS                     ! frz_input ...
ccc 1270 ERRCOD = 'REA-INP-11C'                          ! kluge, need to update error handling
ccc      CALL READIF(K1, 0, INTFIL, ERRCOD)                                
ccc      READ(INTFIL,*,IOSTAT=INERR(1)) NIREG
ccc      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)       
ccc      SISMOD = "NULL"
ccc      SIKMOD = "NULL"
ccc      IF (NIREG.GT.10) THEN                                             
ccc         print *, "ERROR -- NIREG must be no more than 10."         ! kluge
ccc         stop                                                       ! kluge
ccc      ELSE IF (NIREG.EQ.0) THEN
ccc         IF (IFREEZ.NE.0) THEN
ccc            print *,"ERROR -- Freezing parameters must be specified ",  ! kluge
ccc     1         "(NIREG>0) when FREEZING is turned on."                  ! kluge
ccc            stop                                                        ! kluge
ccc         END IF
ccc         GOTO 1272
ccc      END IF                                                            
ccc      DO 1271 NR=1,NIREG
ccc         CALL READIF(K1, 0, INTFIL, ERRCOD)
ccc         READ(INTFIL,*,IOSTAT=INERR(1)) SISMOD(NR)                      
ccc         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
ccc         IF (SISMOD(NR).EQ.'NONE') THEN
ccc            NISPAR(NR) = 0
ccc         ELSE IF (SISMOD(NR).EQ.'EXPO') THEN                           
ccc            NISPAR(NR) = 2
ccc         ELSE IF (SISMOD(NR).EQ.'PLIN') THEN
ccc            NISPAR(NR) = 2
ccc         ELSE IF (SISMOD(NR).EQ.'UDEF') THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1)) SISMOD(NR), NISPAR(NR)
ccc            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
ccc         ELSE
ccc            STOP                ! kluge, need error handling
ccc         END IF
ccc         IF (SISMOD(NR).EQ.'UDEF') THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1))                              
ccc     1         SISMOD(NR),NISPAR(NR),(SISPAR(NR,NP),NP=1,NISPAR(NR))   
ccc         ELSE IF (NISPAR(NR).GT.0) THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1))                              
ccc     1         SISMOD(NR),(SISPAR(NR,NP),NP=1,NISPAR(NR))              
ccc         END IF                                                         
ccc         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
ccc         CALL READIF(K1, 0, INTFIL, ERRCOD)
ccc         READ(INTFIL,*,IOSTAT=INERR(1)) SIKMOD(NR)                      
ccc         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
ccc         IF (SIKMOD(NR).EQ.'NONE') THEN
ccc            NIKPAR(NR) = 0
ccc         ELSE IF (SIKMOD(NR).EQ.'IMPE') THEN
ccc            NIKPAR(NR) = 2
ccc         ELSE IF (SIKMOD(NR).EQ.'LINT') THEN
ccc            NIKPAR(NR) = 2
ccc         ELSE IF (SIKMOD(NR).EQ.'LINS') THEN
ccc            NIKPAR(NR) = 2
ccc         ELSE IF (SIKMOD(NR).EQ.'UDEF') THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1)) SIKMOD(NR), NIKPAR(NR)
ccc            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
ccc         ELSE
ccc            STOP                ! kluge, need error handling
ccc         END IF
ccc         IF ((NISPAR(NR)*NIKPAR(NR).EQ.0).AND.
ccc     1       (NISPAR(NR)+NIKPAR(NR).NE.0)) THEN
ccc            print *, "ERROR -- For a given region, SISMOD and SIKMOD ",  ! kluge
ccc     1         "must either be both 'NONE' or both not 'NONE'."          ! kluge
ccc            stop                                                         ! kluge
ccc         END IF
ccc         IF (SIKMOD(NR).EQ.'UDEF') THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1))                              
ccc     1         SIKMOD(NR),NIKPAR(NR),(SIKPAR(NR,NP),NP=1,NIKPAR(NR))              
ccc         ELSE IF (NIKPAR(NR).GT.0) THEN
ccc            READ(INTFIL,*,IOSTAT=INERR(1))                              
ccc     1         SIKMOD(NR),(SIKPAR(NR,NP),NP=1,NIKPAR(NR))              
ccc         END IF                                                         
ccc         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)
ccc 1271 CONTINUE
ccc 1272 WRITE(K3,1273)
ccc 1273 FORMAT(////11X,'F R E E Z I N G   ',
ccc     1   'F U N C T I O N   P A R A M E T E R S')
ccc      IF(IFREEZ.EQ.0) THEN                                        
ccc         WRITE(K3,1274)
ccc 1274    FORMAT(/16X,'NONFREEZING FLOW IS BEING SIMULATED')
cccccc         GOTO 1268                                                    ! frz_input ccc
ccc         GOTO 1368                                                       ! frz_input
ccc      END IF                                             
ccc      DO 1292 NR=1,NIREG
ccc         IF (SISMOD(NR).EQ.'EXPO') THEN                                  
ccc            SISPNM(1) = "SWRESI"
ccc            SISPNM(2) = "W"
ccc         ELSE IF (SISMOD(NR).EQ.'PLIN') THEN
ccc            SISPNM(1) = "SWRESI"
ccc            SISPNM(2) = "SLOPE"
ccc         ELSE IF (SISMOD(NR).EQ.'UDEF') THEN
ccc            DO 1275 NP=1,NISPAR(NR)
ccc               WRITE(CDUM10,'(I3)') NP
ccc               SISPNM(NP) = "PARAM " // CDUM10(1:3)
ccc 1275       CONTINUE
ccc         END IF
ccc         WRITE(K3,1276) NR
ccc 1276    FORMAT(/16X,"REGION",I4/16X,"---------")                       
ccc         WRITE(K3,1277) SISMOD(NR), "ICE SATURATION MODEL"   
ccc 1277    FORMAT(/11X,11X,A4,5X,A)                                        
ccc         IF (NISPAR(NR).GT.0) THEN
ccc            DO 1280 NP=1,NISPAR(NR)
ccc               WRITE(K3,1278) SISPAR(NR,NP),SISPNM(NP)
ccc 1278          FORMAT(11X,1PE15.4,5X,A10)      
ccc 1280       CONTINUE
ccc         END IF
ccc         IF (SIKMOD(NR).EQ.'IMPE') THEN                                  
ccc            SIKPNM(1) = "OMPOR"
ccc            SIKPNM(2) = "RKRES"
ccc         ELSE IF (SIKMOD(NR).EQ.'LINT') THEN
ccc            SIKPNM(1) = "BREAKT"
ccc            SIKPNM(2) = "RKRES"
cccccc         ELSE IF (SIKMOD(NR).EQ.'LINS') THEN
ccc            SIKPNM(1) = "SWRESI"
ccc            SIKPNM(2) = "RKRES"
ccc         ELSE IF (SIKMOD(NR).EQ.'UDEF') THEN
ccc            DO 1285 NP=1,NIKPAR(NR)
ccc               WRITE(CDUM10,'(I3)') NP
ccc               SIKPNM(NP) = "PARAM " // CDUM10(1:3)
ccc 1285       CONTINUE
ccc         END IF
ccc         WRITE(K3,1287) SIKMOD(NR), "ICE RELATIVE PERMEABILITY MODEL"   
ccc 1287    FORMAT(/11X,11X,A4,5X,A)                                        
ccc         IF (NIKPAR(NR).GT.0) THEN
ccc            DO 1290 NP=1,NIKPAR(NR)
ccc               WRITE(K3,1288) SIKPAR(NR,NP),SIKPNM(NP)
ccc 1288          FORMAT(11X,1PE15.4,5X,A10)      
ccc 1290       CONTINUE
ccc         END IF
ccc 1292 CONTINUE
cccC                                                                          ! ... frz_input, ... frz_input2 ccc

C.....INPUT DATASET 12:  PRODUCTION OF ENERGY OR SOLUTE MASS             INDAT1.......74500
 1368 ERRCOD = 'REA-INP-12'                                              INDAT1.......74600
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......74700
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) PRODF0,PRODS0,PRODF1,PRODS1         INDAT1.......74800
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1.......74900
	   RETURN
      ENDIF                                                              INDAT1.......74000
!      IF(ME.EQ.-1) WRITE(K3,1250) PRODF0,PRODS0,PRODF1,PRODS1            INDAT1.......75000
! 1250 FORMAT(////11X,'P R O D U C T I O N   A N D   D E C A Y   O F   ', INDAT1.......75100
!     1   'S P E C I E S   M A S S'//13X,'PRODUCTION RATE (+)'/13X,       INDAT1.......75200
!     2   'DECAY RATE (-)'//11X,1PE15.4,5X,'ZERO-ORDER RATE OF SOLUTE ',  INDAT1.......75300
!     3   'MASS PRODUCTION/DECAY IN FLUID'/11X,1PE15.4,5X,                INDAT1.......75400
!     4   'ZERO-ORDER RATE OF ADSORBATE MASS PRODUCTION/DECAY IN ',       INDAT1.......75500
!     5   'IMMOBILE PHASE'/11X,1PE15.4,5X,'FIRST-ORDER RATE OF SOLUTE ',  INDAT1.......75600
!     3   'MASS PRODUCTION/DECAY IN FLUID'/11X,1PE15.4,5X,                INDAT1.......75700
!     4   'FIRST-ORDER RATE OF ADSORBATE MASS PRODUCTION/DECAY IN ',      INDAT1.......75800
!     5   'IMMOBILE PHASE')                                               INDAT1.......75900
!      IF(ME.EQ.+1) WRITE(K3,1260) PRODF0,PRODS0                          INDAT1.......76000
! 1260 FORMAT(////11X,'P R O D U C T I O N   A N D   L O S S   O F   ',   INDAT1.......76100
!     1   'E N E R G Y'//13X,'PRODUCTION RATE (+)'/13X,                   INDAT1.......76200
!     2   'LOSS RATE (-)'//11X,1PE15.4,5X,'ZERO-ORDER RATE OF ENERGY ',   INDAT1.......76300
!     3   'PRODUCTION/LOSS IN FLUID'/11X,1PE15.4,5X,                      INDAT1.......76400
!     4   'ZERO-ORDER RATE OF ENERGY PRODUCTION/LOSS IN ',                INDAT1.......76500
!     5   'SOLID GRAINS')                                                 INDAT1.......76600
C.....SET PARAMETER SWITCHES FOR EITHER ENERGY OR SOLUTE TRANSPORT       INDAT1.......76700
      IF(ME) 1272,1272,1274                                              INDAT1.......76800
C     FOR SOLUTE TRANSPORT:                                              INDAT1.......76900
 1272 CS=0.0D0                                                           INDAT1.......77000
      CW=1.D00                                                           INDAT1.......77100
      SIGMAS=0.0D0                                                       INDAT1.......77200
      GOTO 1278                                                          INDAT1.......77300
C     FOR ENERGY TRANSPORT:                                              INDAT1.......77400
 1274 ADSMOD='NONE      '                                                INDAT1.......77500
      CHI1=0.0D0                                                         INDAT1.......77600
      CHI2=0.0D0                                                         INDAT1.......77700
      PRODF1=0.0D0                                                       INDAT1.......77800
      PRODS1=0.0D0                                                       INDAT1.......77900
 1278 CONTINUE                                                           INDAT1.......78000
C                                                                        INDAT1.......78100
      IF (KTYPE(1).EQ.3) THEN                                            INDAT1.......78200
C.....READ 3D INPUT FROM DATASETS 13 - 15.                               INDAT1.......78300
C                                                                        INDAT1.......78400
C.....INPUT DATASET 13:  ORIENTATION OF COORDINATES TO GRAVITY           INDAT1.......78500
      ERRCOD = 'REA-INP-13'                                              INDAT1.......78600
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......78700
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) GRAVX,GRAVY,GRAVZ                   INDAT1.......78800
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1.......78900
	   RETURN
	ENDIF
!      WRITE(K3,1320) GRAVX,GRAVY,GRAVZ                                   INDAT1.......79000
! 1320 FORMAT(////11X,'C O O R D I N A T E   O R I E N T A T I O N   ',   INDAT1.......79100
!     1   'T O   G R A V I T Y'//13X,'COMPONENT OF GRAVITY VECTOR',       INDAT1.......79200
!     2   /13X,'IN +X DIRECTION, GRAVX'/11X,1PE15.4,5X,                   INDAT1.......79300
!     3   'GRAVX = -GRAV * D(ELEVATION)/DX'//13X,'COMPONENT OF GRAVITY',  INDAT1.......79400
!     4   ' VECTOR'/13X,'IN +Y DIRECTION, GRAVY'/11X,1PE15.4,5X,          INDAT1.......79500
!     5   'GRAVY = -GRAV * D(ELEVATION)/DY'//13X,'COMPONENT OF GRAVITY',  INDAT1.......79600
!     6   ' VECTOR'/13X,'IN +Z DIRECTION, GRAVZ'/11X,1PE15.4,5X,          INDAT1.......79700
!     7   'GRAVZ = -GRAV * D(ELEVATION)/DZ')                              INDAT1.......79800
C                                                                        INDAT1.......79900
C.....INPUT DATASETS 14A & 14B:  NODEWISE DATA                           INDAT1.......80000
      ERRCOD = 'REA-INP-14A'                                             INDAT1.......80100
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......80200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,SCALX,SCALY,SCALZ,PORFAC     INDAT1.......80300
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1.......80400
	   RETURN
	ENDIF
      IF (CDUM10.NE.'NODE      ') THEN                                   INDAT1.......80500
         ERRCOD = 'INP-14A-1'                                            INDAT1.......80600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......80700
	   RETURN
      END IF                                                             INDAT1.......80800
      NRTEST=1                                                           INDAT1.......80900
      DO 1450 I=1,NN                                                     INDAT1.......81000
      ERRCOD = 'REA-INP-14B'                                             INDAT1.......81100
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......81200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) II,NREG(II),X(II),Y(II),Z(II),      INDAT1.......81300
     1   POR(II)                                                         INDAT1.......81400
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1.......81500
	   RETURN
	ENDIF
      X(II)=X(II)*SCALX                                                  INDAT1.......81600
      Y(II)=Y(II)*SCALY                                                  INDAT1.......81700
      Z(II)=Z(II)*SCALZ                                                  INDAT1.......81800
      POR(II)=POR(II)*PORFAC                                             INDAT1.......81900
C RBW
	IF (INodeValueCount.GT.0) THEN
	  if (II.GT.INodeValueCount) then
	    IERROR = 1
	    return
	  endif
	  NodeValues(II) = POR(II)
	ENDIF
C RBW
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                   INDAT1.......82000
      NROLD=NREG(II)                                                     INDAT1.......82100
C.....SET SPECIFIC PRESSURE STORATIVITY, SOP.                            INDAT1.......82200
 1450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                       INDAT1.......82300
! 1460 IF(KNODAL.EQ.0) WRITE(K3,1461) SCALX,SCALY,SCALZ,PORFAC            INDAT1.......82400
! 1461 FORMAT('1'////11X,'N O D E   I N F O R M A T I O N'//16X,          INDAT1.......82500
!     1   'PRINTOUT OF NODE COORDINATES AND POROSITIES ',                 INDAT1.......82600
!     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'X-SCALE'/   INDAT1.......82700
!     3   33X,1PE15.4,5X,'Y-SCALE'/33X,1PE15.4,5X,'Z-SCALE'/              INDAT1.......82800
!     4   33X,1PE15.4,5X,'POROSITY FACTOR')                               INDAT1.......82900
!      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.NE.1) WRITE(K3,1463)     INDAT1.......83000
!      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.EQ.1) WRITE(K3,1465)     INDAT1.......83100
! 1463 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......83200
!     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......83300
! 1465 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......83400
!     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......83500
!      IF(KNODAL.EQ.+1.AND.IUNSAT.NE.1)                                   INDAT1.......83600
!     1   WRITE(K3,1470)(I,X(I),Y(I),Z(I),POR(I),I=1,NN)                  INDAT1.......83700
! 1470 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,            INDAT1.......83800
!     1   'NODE',7X,'X',16X,'Y',16X,'Z',15X,'POROSITY'//                  INDAT1.......83900
!     2   (9X,I9,3(3X,1PE14.5),6X,0PF8.5))                                INDAT1.......84000
!      IF(KNODAL.EQ.+1.AND.IUNSAT.EQ.1)                                   INDAT1.......84100
!     1   WRITE(K3,1480)(I,NREG(I),X(I),Y(I),Z(I),POR(I),I=1,NN)          INDAT1.......84200
! 1480 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,'NODE',3X,  INDAT1.......84300
!     1   'REGION',7X,'X',16X,'Y',16X,'Z',15X,'POROSITY'//                INDAT1.......84400
!     2   (9X,I9,3X,I6,3(3X,1PE14.5),6X,0PF8.5))                          INDAT1.......84500
C                                                                        INDAT1.......84600
C.....INPUT DATASETS 15A & 15B:  ELEMENTWISE DATA                        INDAT1.......84700
      ERRCOD = 'REA-INP-15A'                                             INDAT1.......84800
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......84900
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,PMAXFA,PMIDFA,PMINFA,        INDAT1.......85000
     1   ANG1FA,ANG2FA,ANG3FA,ALMAXF,ALMIDF,ALMINF,                      INDAT1.......85100
     1   ATMXF,ATMDF,ATMNF                                               INDAT1.......85200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1.......85300
	   RETURN
	ENDIF
      IF (CDUM10.NE.'ELEMENT   ') THEN                                   INDAT1.......85400
         ERRCOD = 'INP-15A-1'                                            INDAT1.......85500
         CHERR(1) = '3D'                                                 INDAT1.......85600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......85700
	   RETURN
      END IF                                                             INDAT1.......85800
!      IF(KELMNT.EQ.+1) THEN                                              INDAT1.......85900
!         IF (IUNSAT.EQ.1) THEN                                           INDAT1.......86000
!            WRITE(K3,1500)                                               INDAT1.......86100
! 1500       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......86200
!     1         11X,'ELEMENT',3X,'REGION',4X,                             INDAT1.......86300
!     2         'MAXIMUM',9X,'MIDDLE',10X,'MINIMUM',18X,                  INDAT1.......86400
!     2         'ANGLE1',9X,'ANGLE2',9X,'ANGLE3',4X,                      INDAT1.......86500
!     2         'LONGITUDINAL',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,    INDAT1.......86600
!     2         'TRANSVERSE',5X,'TRANSVERSE',5X,'TRANSVERSE'/             INDAT1.......86700
!     3         31X,'PERMEABILITY',4X,'PERMEABILITY',4X,'PERMEABILITY',   INDAT1.......86800
!     4         8X,'(IN DEGREES)',3X,'(IN DEGREES)',3X,'(IN DEGREES)',3X, INDAT1.......86900
!     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY',3X,    INDAT1.......87000
!     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY'/       INDAT1.......87100
!     4         2(64X),' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM' INDAT1.......87200
!     4         3X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM'/    INDAT1.......87300
!     1         2(64X),'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION' INDAT1.......87400
!     2         3X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)   INDAT1.......87500
!         ELSE                                                            INDAT1.......87600
!            WRITE(K3,1501)                                               INDAT1.......87700
! 1501       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......87800
!     1         11X,'ELEMENT',4X,                                         INDAT1.......87900
!     2         'MAXIMUM',9X,'MIDDLE',10X,'MINIMUM',18X,                  INDAT1.......88000
!     2         'ANGLE1',9X,'ANGLE2',9X,'ANGLE3',4X,                      INDAT1.......88100
!     2         'LONGITUDINAL',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,    INDAT1.......88200
!     2         'TRANSVERSE',5X,'TRANSVERSE',5X,'TRANSVERSE'/             INDAT1.......88300
!     3         22X,'PERMEABILITY',4X,'PERMEABILITY',4X,'PERMEABILITY',   INDAT1.......88400
!     4         8X,'(IN DEGREES)',3X,'(IN DEGREES)',3X,'(IN DEGREES)',3X, INDAT1.......88500
!     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY',3X,    INDAT1.......88600
!     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY'/       INDAT1.......88700
!     4         119X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM',  INDAT1.......88800
!     4         3X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM'/    INDAT1.......88900
!     1         119X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION',  INDAT1.......89000
!    2         3X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)   INDAT1.......89100
!        END IF                                                          INDAT1.......89200
!      END IF                                                             INDAT1.......89300
      LRTEST=1                                                           INDAT1.......89400
      DO 1550 LL=1,NE                                                    INDAT1.......89500
      ERRCOD = 'REA-INP-15B'                                             INDAT1.......89600
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......89700
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) L,LREG(L),PMAX,PMID,PMIN,           INDAT1.......89800
     1   ANGLE1,ANGLE2,ANGLE3,ALMAX(L),ALMID(L),ALMIN(L),                INDAT1.......89900
     1   ATMAX(L),ATMID(L),ATMIN(L)                                      INDAT1.......90000
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  INDAT1.......90100
	   RETURN
	ENDIF
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                   INDAT1.......90200
      LROLD=LREG(L)                                                      INDAT1.......90300
      PMAX=PMAX*PMAXFA                                                   INDAT1.......90400
      PMID=PMID*PMIDFA                                                   INDAT1.......90500
      PMIN=PMIN*PMINFA                                                   INDAT1.......90600
      ANGLE1=ANGLE1*ANG1FA                                               INDAT1.......90700
      ANGLE2=ANGLE2*ANG2FA                                               INDAT1.......90800
      ANGLE3=ANGLE3*ANG3FA                                               INDAT1.......90900
      ALMAX(L)=ALMAX(L)*ALMAXF                                           INDAT1.......91000
      ALMID(L)=ALMID(L)*ALMIDF                                           INDAT1.......91100
      ALMIN(L)=ALMIN(L)*ALMINF                                           INDAT1.......91200
      ATMAX(L)=ATMAX(L)*ATMXF                                            INDAT1.......91300
      ATMID(L)=ATMID(L)*ATMDF                                            INDAT1.......91400
      ATMIN(L)=ATMIN(L)*ATMNF                                            INDAT1.......91500
C RBW
! If ElementValues has been allocated, store data in it for display by Model Viewer
	IF (IElementValueCount.GT.0) THEN
	  if ((L+11*NE).GT.IElementValueCount) then
	    IERROR = 1
	    return
	  endif
	  ElementValues(L) = PMAX
	  ElementValues(L+NE)    = PMID
	  ElementValues(L+2*NE)  = PMIN
	  ElementValues(L+3*NE)  = ALMAX(L)
	  ElementValues(L+4*NE)  = ALMID(L)
	  ElementValues(L+5*NE)  = ALMIN(L)
	  ElementValues(L+6*NE)  = ATMAX(L)
	  ElementValues(L+7*NE)  = ATMID(L)
	  ElementValues(L+8*NE)  = ATMIN(L)
	  ElementValues(L+9*NE)  = ANGLE1
	  ElementValues(L+10*NE) = ANGLE2
	  ElementValues(L+11*NE) = ANGLE3
	ENDIF
C RBW
!      IF(KELMNT.EQ.+1.AND.IUNSAT.NE.1) WRITE(K3,1520) L,                 INDAT1.......91600
!     1   PMAX,PMID,PMIN,ANGLE1,ANGLE2,ANGLE3,                            INDAT1.......91700
!     2   ALMAX(L),ALMID(L),ALMIN(L),ATMAX(L),ATMID(L),ATMIN(L)           INDAT1.......91800
! 1520 FORMAT(9X,I9,2X,3(1PE14.5,2X),7X,9(G11.4,4X))                      INDAT1.......91900
!      IF(KELMNT.EQ.+1.AND.IUNSAT.EQ.1) WRITE(K3,1530) L,LREG(L),         INDAT1.......92000
!     1   PMAX,PMID,PMIN,ANGLE1,ANGLE2,ANGLE3,                            INDAT1.......92100
!     2   ALMAX(L),ALMID(L),ALMIN(L),ATMAX(L),ATMID(L),ATMIN(L)           INDAT1.......92200
! 1530 FORMAT(9X,I9,4X,I5,2X,3(1PE14.5,2X),7X,9(G11.4,4X))                INDAT1.......92300
C                                                                        INDAT1.......92400
C.....ROTATE PERMEABILITY FROM MAX/MID/MIN TO X/Y/Z DIRECTIONS.          INDAT1.......92500
C        BASED ON CODE WRITTEN BY DAVID POLLOCK (USGS).                  INDAT1.......92600
      D2R=1.745329252D-2                                                 INDAT1.......92700
      PANGL1(L)=D2R*ANGLE1                                               INDAT1.......92800
      PANGL2(L)=D2R*ANGLE2                                               INDAT1.......92900
      PANGL3(L)=D2R*ANGLE3                                               INDAT1.......93000
      ZERO = 0D0                                                         INDAT1.......93100
!      CALL ROTMAT(PANGL1(L),PANGL2(L),PANGL3(L),Q11,Q12,Q13,             INDAT1.......93200
!     1   Q21,Q22,Q23,Q31,Q32,Q33)                                        INDAT1.......93300
!      CALL TENSYM(PMAX,PMID,PMIN,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q33,    INDAT1.......93400
!     1   PERMXX(L),PERMXY(L),PERMXZ(L),PERMYX(L),PERMYY(L),PERMYZ(L),    INDAT1.......93500
!     2   PERMZX(L),PERMZY(L),PERMZZ(L))                                  INDAT1.......93600
 1550 CONTINUE                                                           INDAT1.......93700
!      IF(KELMNT.EQ.0)                                                    INDAT1.......93800
!     1   WRITE(K3,1569) PMAXFA,PMIDFA,PMINFA,ANG1FA,ANG2FA,ANG3FA,       INDAT1.......93900
!     2      ALMAXF,ALMIDF,ALMINF,ATMXF,ATMDF,ATMNF                       INDAT1.......94000
! 1569 FORMAT(////11X,'E L E M E N T   I N F O R M A T I O N'//           INDAT1.......94100
!     1   16X,'PRINTOUT OF ELEMENT PERMEABILITIES AND DISPERSIVITIES ',   INDAT1.......94200
!     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'MAXIMUM ',  INDAT1.......94300
!     3   'PERMEABILITY FACTOR'/33X,1PE15.4,5X,'MIDDLE PERMEABILITY ',    INDAT1.......94400
!     4   'FACTOR '/33X,1PE15.4,5X,'MINIMUM PERMEABILITY FACTOR'/         INDAT1.......94500
!     5   33X,1PE15.4,5X,'ANGLE1 FACTOR'/33X,1PE15.4,5X,'ANGLE2 FACTOR'/  INDAT1.......94600
!     6   33X,1PE15.4,5X,'ANGLE3 FACTOR'/                                 INDAT1.......94700
!     7   33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL DISPERSIVITY IN ',      INDAT1.......94800
!     8   'MAX-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL ', INDAT1.......94900
!     9   'DISPERSIVITY IN MID-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR ',  INDAT1.......95000
!     T   'FOR LONGITUDINAL DISPERSIVITY IN MIN-PERM DIRECTION'/          INDAT1.......95100
!     1   33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE DISPERSIVITY IN ',        INDAT1.......95200
!     2   'MAX-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE ',   INDAT1.......95300
!     3   'DISPERSIVITY IN MID-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR',   INDAT1.......95400
!     4   ' FOR TRANSVERSE DISPERSIVITY IN MIN-PERM DIRECTION')           INDAT1.......95500
!      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.NE.1) WRITE(K3,1573)     INDAT1.......95600
!      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.EQ.1) WRITE(K3,1575)     INDAT1.......95700
! 1573 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......95800
!     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......95900
! 1575 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......96000
!     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......96100
C                                                                        INDAT1.......96200
      ELSE                                                               INDAT1.......96300
C.....READ 2D INPUT FROM DATASETS 13 - 15.                               INDAT1.......96400
C.....NOTE THAT Z = THICKNESS AND PANGL1 = PANGLE.                       INDAT1.......96500
C                                                                        INDAT1.......96600
C.....INPUT DATASET 13:  ORIENTATION OF COORDINATES TO GRAVITY           INDAT1.......96700
      ERRCOD = 'REA-INP-13'                                              INDAT1.......96800
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......96900
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) GRAVX,GRAVY                         INDAT1.......97000
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        INDAT1.......97100
	   RETURN
	ENDIF
      GRAVZ = 0D0                                                        INDAT1.......97200
!      WRITE(K3,2320) GRAVX,GRAVY                                         INDAT1.......97300
! 2320 FORMAT(////11X,'C O O R D I N A T E   O R I E N T A T I O N   ',   INDAT1.......97400
!     1   'T O   G R A V I T Y'//13X,'COMPONENT OF GRAVITY VECTOR',       INDAT1.......97500
!     2   /13X,'IN +X DIRECTION, GRAVX'/11X,1PE15.4,5X,                   INDAT1.......97600
!     3   'GRAVX = -GRAV * D(ELEVATION)/DX'//13X,'COMPONENT OF GRAVITY',  INDAT1.......97700
!     4   ' VECTOR'/13X,'IN +Y DIRECTION, GRAVY'/11X,1PE15.4,5X,          INDAT1.......97800
!     5   'GRAVY = -GRAV * D(ELEVATION)/DY')                              INDAT1.......97900
C                                                                        INDAT1.......98000
C.....INPUT DATASETS 14A & 14B:  NODEWISE DATA                           INDAT1.......98100
      ERRCOD = 'REA-INP-14A'                                             INDAT1.......98200
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......98300
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,SCALX,SCALY,SCALTH,PORFAC    INDAT1.......98400
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1.......98500
	   RETURN
	ENDIF
      IF (CDUM10.NE.'NODE      ') THEN                                   INDAT1.......98600
         ERRCOD = 'INP-14A-1'                                            INDAT1.......98700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......98800
	   RETURN
      END IF                                                             INDAT1.......98900
      NRTEST=1                                                           INDAT1.......99000
      DO 2450 I=1,NN                                                     INDAT1.......99100
      ERRCOD = 'REA-INP-14B'                                             INDAT1.......99200
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1.......99300
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) II,NREG(II),X(II),Y(II),Z(II),      INDAT1.......99400
     1   POR(II)                                                         INDAT1.......99500
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1.......99600
	   RETURN
      END IF                                                             INDAT1.......98900
      X(II)=X(II)*SCALX                                                  INDAT1.......99700
      Y(II)=Y(II)*SCALY                                                  INDAT1.......99800
      Z(II)=Z(II)*SCALTH                                                 INDAT1.......99900
      POR(II)=POR(II)*PORFAC                                             INDAT1......100000
C RBW
	IF (INodeValueCount.GT.0) THEN
	  if (II.GT.INodeValueCount) then
	    IERROR = 1
	    return
	  endif
	  NodeValues(II) = POR(II)
	ENDIF
C RBW
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                   INDAT1......100100
      NROLD=NREG(II)                                                     INDAT1......100200
C.....SET SPECIFIC PRESSURE STORATIVITY, SOP.                            INDAT1......100300
 2450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                       INDAT1......100400
! 2460 IF(KNODAL.EQ.0) WRITE(K3,2461) SCALX,SCALY,SCALTH,PORFAC           INDAT1......100500
! 2461 FORMAT('1'////11X,'N O D E   I N F O R M A T I O N'//16X,          INDAT1......100600
!     1   'PRINTOUT OF NODE COORDINATES, THICKNESSES AND POROSITIES ',    INDAT1......100700
!     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'X-SCALE'/   INDAT1......100800
!     1   33X,1PE15.4,5X,'Y-SCALE'/33X,1PE15.4,5X,'THICKNESS FACTOR'/     INDAT1......100900
!     2   33X,1PE15.4,5X,'POROSITY FACTOR')                               INDAT1......101000
!      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.NE.1) WRITE(K3,2463)     INDAT1......101100
!      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.EQ.1) WRITE(K3,2465)     INDAT1......101200
! 2463 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1......101300
!     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1......101400
! 2465 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1......101500
!     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1......101600
!      IF(KNODAL.EQ.+1.AND.IUNSAT.NE.1)                                   INDAT1......101700
!     1   WRITE(K3,2470)(I,X(I),Y(I),Z(I),POR(I),I=1,NN)                  INDAT1......101800
! 2470 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,            INDAT1......101900
!     1   'NODE',7X,'X',16X,'Y',17X,'THICKNESS',6X,'POROSITY'//           INDAT1......102000
!     2   (9X,I9,3(3X,1PE14.5),6X,0PF8.5))                                INDAT1......102100
 !     IF(KNODAL.EQ.+1.AND.IUNSAT.EQ.1)                                   INDAT1......102200
 !    1   WRITE(K3,2480)(I,NREG(I),X(I),Y(I),Z(I),POR(I),I=1,NN)          INDAT1......102300
 !2480 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,'NODE',3X,  INDAT1......102400
 !    1   'REGION',7X,'X',16X,'Y',17X,'THICKNESS',6X,'POROSITY'//         INDAT1......102500
 !    2   (9X,I9,3X,I6,3(3X,1PE14.5),6X,0PF8.5))                          INDAT1......102600
C                                                                        INDAT1......102700
C.....INPUT DATASETS 15A & 15B:  ELEMENTWISE DATA                        INDAT1......102800
      ERRCOD = 'REA-INP-15A'                                             INDAT1......102900
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1......103000
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,PMAXFA,PMINFA,ANGFAC,        INDAT1......103100
     1   ALMAXF,ALMINF,ATMAXF,ATMINF                                     INDAT1......103200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                   INDAT1......103300
	   RETURN
	ENDIF
      IF (CDUM10.NE.'ELEMENT   ') THEN                                   INDAT1......103400
         ERRCOD = 'INP-15A-1'                                            INDAT1......103500
         CHERR(1) = '2D'                                                 INDAT1......103600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1......103700
	   RETURN
      END IF                                                             INDAT1......103800
!      IF (KELMNT.EQ.+1) THEN                                             INDAT1......103900
!         IF (IUNSAT.EQ.1) THEN                                           INDAT1......104000
!            WRITE(K3,2500)                                               INDAT1......104100
! 2500       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1......104200
!     1         11X,'ELEMENT',3X,'REGION',4X,'MAXIMUM',9X,'MINIMUM',12X,  INDAT1......104300
!     2         'ANGLE BETWEEN',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,   INDAT1......104400
!     3         'TRANSVERSE',5X,'TRANSVERSE'/                             INDAT1......104500
!     4         31X,'PERMEABILITY',4X,'PERMEABILITY',4X,                  INDAT1......104600
!     5         '+X-DIRECTION AND',3X,'DISPERSIVITY',3X,'DISPERSIVITY',   INDAT1......104700
!     6         3X,'DISPERSIVITY',3X,'DISPERSIVITY'/                      INDAT1......104800
!     7         59X,'MAXIMUM PERMEABILITY',3X,' IN MAX-PERM',             INDAT1......104900
!     8         3X,' IN MIN-PERM',3X,' IN MAX-PERM',3X,' IN MIN-PERM'/    INDAT1......105000
!     9         67X,'(IN DEGREES)',3X,'   DIRECTION',3X,                  INDAT1......105100
!     1         '   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)      INDAT1......105200
!         ELSE                                                            INDAT1......105300
!            WRITE(K3,2501)                                               INDAT1......105400
! 2501       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1......105500
!     1         11X,'ELEMENT',4X,'MAXIMUM',9X,'MINIMUM',12X,              INDAT1......105600
!     2         'ANGLE BETWEEN',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,   INDAT1......105700
!     3         'TRANSVERSE',5X,'TRANSVERSE'/                             INDAT1......105800
!     4         22X,'PERMEABILITY',4X,'PERMEABILITY',4X,                  INDAT1......105900
!     5         '+X-DIRECTION AND',3X,'DISPERSIVITY',3X,'DISPERSIVITY',   INDAT1......106000
!     6         3X,'DISPERSIVITY',3X,'DISPERSIVITY'/                      INDAT1......106100
!     7         50X,'MAXIMUM PERMEABILITY',3X,' IN MAX-PERM',             INDAT1......106200
!     8         3X,' IN MIN-PERM',3X,' IN MAX-PERM',3X,' IN MIN-PERM'/    INDAT1......106300
!     9         58X,'(IN DEGREES)',3X,'   DIRECTION',3X,                  INDAT1......106400
!     1         '   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)      INDAT1......106500
!         END IF                                                          INDAT1......106600
!      END IF                                                             INDAT1......106700
      LRTEST=1                                                           INDAT1......106800
      DO 2550 LL=1,NE                                                    INDAT1......106900
      ERRCOD = 'REA-INP-15B'                                             INDAT1......107000
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 INDAT1......107100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) L,LREG(L),PMAX,PMIN,ANGLEX,         INDAT1......107200
     1   ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)                             INDAT1......107300
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                          INDAT1......107400
	   RETURN
	ENDIF
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                   INDAT1......107500
      LROLD=LREG(L)                                                      INDAT1......107600
      PMAX=PMAX*PMAXFA                                                   INDAT1......107700
      PMIN=PMIN*PMINFA                                                   INDAT1......107800
      ANGLEX=ANGLEX*ANGFAC                                               INDAT1......107900
      ALMAX(L)=ALMAX(L)*ALMAXF                                           INDAT1......108000
      ALMIN(L)=ALMIN(L)*ALMINF                                           INDAT1......108100
      ATMAX(L)=ATMAX(L)*ATMAXF                                           INDAT1......108200
      ATMIN(L)=ATMIN(L)*ATMINF                                           INDAT1......108300
C RBW
! If ElementValues has been allocated, store data in it for display by Model Viewer
	IF (IElementValueCount.GT.0) THEN
	  if ((L+6*NE).GT.IElementValueCount) then
	    IERROR = 1
	    return
	  endif
	  ElementValues(L)      = PMAX
	  ElementValues(L+NE)   = PMIN
	  ElementValues(L+2*NE) = ALMAX(L)
	  ElementValues(L+3*NE) = ALMIN(L)
	  ElementValues(L+4*NE) = ATMAX(L)
	  ElementValues(L+5*NE) = ATMIN(L)
	  ElementValues(L+6*NE) = ANGLEX
	ENDIF
C RBW
!      IF(KELMNT.EQ.+1.AND.IUNSAT.NE.1) WRITE(K3,2520) L,                 INDAT1......108400
!     1   PMAX,PMIN,ANGLEX,ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)            INDAT1......108500
! 2520 FORMAT(9X,I9,2X,2(1PE14.5,2X),7X,5(G11.4,4X))                      INDAT1......108600
!      IF(KELMNT.EQ.+1.AND.IUNSAT.EQ.1) WRITE(K3,2530) L,LREG(L),         INDAT1......108700
!     1   PMAX,PMIN,ANGLEX,ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)            INDAT1......108800
! 2530 FORMAT(9X,I9,4X,I5,2X,2(1PE14.5,2X),7X,5(G11.4,4X))                INDAT1......108900
C                                                                        INDAT1......109000
C.....ROTATE PERMEABILITY FROM MAXIMUM/MINIMUM TO X/Y DIRECTIONS         INDAT1......109100
      RADIAX=1.745329D-2*ANGLEX                                          INDAT1......109200
      SINA=DSIN(RADIAX)                                                  INDAT1......109300
      COSA=DCOS(RADIAX)                                                  INDAT1......109400
      SINA2=SINA*SINA                                                    INDAT1......109500
      COSA2=COSA*COSA                                                    INDAT1......109600
      PERMXX(L)=PMAX*COSA2+PMIN*SINA2                                    INDAT1......109700
      PERMYY(L)=PMAX*SINA2+PMIN*COSA2                                    INDAT1......109800
      PERMXY(L)=(PMAX-PMIN)*SINA*COSA                                    INDAT1......109900
      PERMYX(L)=PERMXY(L)                                                INDAT1......110000
      PANGL1(L)=RADIAX                                                   INDAT1......110100
 2550 CONTINUE                                                           INDAT1......110200
!      IF(KELMNT.EQ.0)                                                    INDAT1......110300
!     1   WRITE(K3,2569) PMAXFA,PMINFA,ANGFAC,ALMAXF,ALMINF,ATMAXF,ATMINF INDAT1......110400
! 2569 FORMAT(////11X,'E L E M E N T   I N F O R M A T I O N'//           INDAT1......110500
!     1   16X,'PRINTOUT OF ELEMENT PERMEABILITIES AND DISPERSIVITIES ',   INDAT1......110600
!     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'MAXIMUM ',  INDAT1......110700
!     3   'PERMEABILITY FACTOR'/33X,1PE15.4,5X,'MINIMUM PERMEABILITY ',   INDAT1......110800
!     4   'FACTOR'/33X,1PE15.4,5X,'ANGLE FROM +X TO MAXIMUM DIRECTION',   INDAT1......110900
!     5   ' FACTOR'/33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL DISPERSIVITY' INDAT1......111000
!     6  ,' IN MAX-PERM DIRECTION'/33X,1PE15.4,5X,                        INDAT1......111100
!     7   'FACTOR FOR LONGITUDINAL DISPERSIVITY IN MIN-PERM DIRECTION',   INDAT1......111200
!     8   /33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE DISPERSIVITY',           INDAT1......111300
!     9   ' IN MAX-PERM DIRECTION'/33X,1PE15.4,5X,                        INDAT1......111400
!     *   'FACTOR FOR TRANSVERSE DISPERSIVITY IN MIN-PERM DIRECTION')     INDAT1......111500
!      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.NE.1) WRITE(K3,2573)     INDAT1......111600
!      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.EQ.1) WRITE(K3,2575)     INDAT1......111700
! 2573 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1......111800
!     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1......111900
! 2575 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1......112000
!     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1......112100
C                                                                        INDAT1......112200
      END IF                                                             INDAT1......112300
C                                                                        INDAT1......112400
      RETURN                                                             INDAT1......112500
      END                                                                INDAT1......112600
C                                                                        INDAT1......112700
C     SUBROUTINE        I  N  D  A  T  2           SUTRA VERSION 2.2     INDAT2.........100
C                                                                        INDAT2.........200
C *** PURPOSE :                                                          INDAT2.........300
C ***  TO READ INITIAL CONDITIONS FROM ICS FILE, AND TO                  INDAT2.........400
C ***  INITIALIZE DATA FOR EITHER WARM OR COLD START OF                  INDAT2.........500
C ***  THE SIMULATION.                                                   INDAT2.........600
C                                                                        INDAT2.........700
C     SUBROUTINE        L  L  D  2  A  R           SUTRA VERSION 2.2     LLD2AR.........100
C                                                                        LLD2AR.........200
C *** PURPOSE :                                                          LLD2AR.........300
C ***  TO LOAD A LINKED LIST OF DOUBLE-PRECISION PAIRS INTO TWO ARRAYS.  LLD2AR.........400
C                                                                        LLD2AR.........500
      SUBROUTINE LLD2AR(LSTLEN, DLIST, DARR1, DARR2)                     LLD2AR.........600
      USE LLDEF                                                          LLD2AR.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LLD2AR.........800
      TYPE (LLD), POINTER :: DEN, DLIST                                  LLD2AR.........900
      DIMENSION DARR1(*), DARR2(*)                                       LLD2AR........1000
C                                                                        LLD2AR........1100
      DEN => DLIST                                                       LLD2AR........1200
      DO 100 K=1,LSTLEN                                                  LLD2AR........1300
         DARR1(K) = DEN%DVALU1                                           LLD2AR........1400
         DARR2(K) = DEN%DVALU2                                           LLD2AR........1500
         DEN => DEN%NENT                                                 LLD2AR........1600
  100 CONTINUE                                                           LLD2AR........1700
C                                                                        LLD2AR........1800
      RETURN                                                             LLD2AR........1900
      END                                                                LLD2AR........2000
C                                                                        LLD2AR........2100
C                                                                        LLD2AR........2200
C     SUBROUTINE        L  L  D  I  N  S           SUTRA VERSION 2.2     LLDINS.........100
C                                                                        LLDINS.........200
C *** PURPOSE :                                                          LLDINS.........300
C ***  TO INSERT A PAIR OF DOUBLE-PRECISION VALUES INTO A LINKED         LLDINS.........400
C ***  LIST, IN ASCENDING ORDER BASED ON THE FIRST VALUE IN THE PAIR.    LLDINS.........500
C                                                                        LLDINS.........600
      SUBROUTINE LLDINS(LSTLEN, DLIST, DNUM1, DNUM2, DLAST)              LLDINS.........700
      USE LLDEF                                                          LLDINS.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LLDINS.........900
      TYPE (LLD), POINTER :: DEN, DENPV, DENNW, DLIST, DLAST             LLDINS........1000
C                                                                        LLDINS........1100
C.....IF LIST IS EMPTY, PLACE PAIR AT HEAD OF LIST, ELSE INSERT          LLDINS........1200
C        INTO LIST IN ASCENDING ORDER BASED ON FIRST VALUE               LLDINS........1300
      IF (LSTLEN.EQ.0) THEN                                              LLDINS........1400
C........PLACE AT HEAD                                                   LLDINS........1500
         DLIST%DVALU1 = DNUM1                                            LLDINS........1600
         DLIST%DVALU2 = DNUM2                                            LLDINS........1700
         DLAST => DLIST                                                  LLDINS........1800
      ELSE IF (DNUM1.GE.DLAST%DVALU1) THEN                               LLDINS........1900
C........APPEND TO TAIL                                                  LLDINS........2000
         ALLOCATE(DENNW)                                                 LLDINS........2100
         DENNW%DVALU1 = DNUM1                                            LLDINS........2200
         DENNW%DVALU2 = DNUM2                                            LLDINS........2300
         DLAST%NENT => DENNW                                             LLDINS........2400
         DLAST => DENNW                                                  LLDINS........2500
      ELSE                                                               LLDINS........2600
C........INSERT INTO LISTS                                               LLDINS........2700
         DEN => DLIST                                                    LLDINS........2800
         DO 770 K=1,LSTLEN                                               LLDINS........2900
            IF (DNUM1.LT.DEN%DVALU1) THEN                                LLDINS........3000
               ALLOCATE(DENNW)                                           LLDINS........3100
               DENNW%DVALU1 = DNUM1                                      LLDINS........3200
               DENNW%DVALU2 = DNUM2                                      LLDINS........3300
               DENNW%NENT => DEN                                         LLDINS........3400
               IF (K.EQ.1) THEN                                          LLDINS........3500
                  DLIST => DENNW                                         LLDINS........3600
               ELSE                                                      LLDINS........3700
                  DENPV%NENT => DENNW                                    LLDINS........3800
               END IF                                                    LLDINS........3900
               GOTO 780                                                  LLDINS........4000
            END IF                                                       LLDINS........4100
            DENPV => DEN                                                 LLDINS........4200
            DEN => DEN%NENT                                              LLDINS........4300
  770    CONTINUE                                                        LLDINS........4400
      END IF                                                             LLDINS........4500
C                                                                        LLDINS........4600
  780 LSTLEN = LSTLEN + 1                                                LLDINS........4700
      RETURN                                                             LLDINS........4800
      END                                                                LLDINS........4900
C                                                                        LLDINS........5000
C     SUBROUTINE        L  O  D  O  B  S           SUTRA VERSION 2.2     LODOBS.........100
C                                                                        LODOBS.........200
C *** PURPOSE :                                                          LODOBS.........300
C ***  TO LOAD OBSERVATION POINT INDICES NOBLIN AT A TIME, STARTING      LODOBS.........400
C ***  WITH INDEX JNEXT, INTO ARRAY JSET.  ONLY OBSERVATION POINTS       LODOBS.........500
C ***  WHOSE SCHEDULE AND OUTPUT FORMAT MATCH THOSE THAT CORRESPOND      LODOBS.........600
C ***  TO FILE INDEX NFLO ARE LOADED.  THE NUMBER OF OBSERVATIONS        LODOBS.........700
C ***  LOADED, JLOAD, CAN BE LESS THAN NOBLIN IF THE LIST OF             LODOBS.........800
C ***  OBSERVATION POINT INDICES IS EXHAUSTED.                           LODOBS.........900
C                                                                        LODOBS........1000
      SUBROUTINE LODOBS(NFLO,JNEXT,OBSPTS,JSET,JLOAD)                    LODOBS........1100
      USE ALLARR, ONLY : OBSDAT                                          LODOBS........1200
      USE SCHDEF                                                         LODOBS........1300
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LODOBS........1400
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         LODOBS........1500
      DIMENSION JSET(*)                                                  LODOBS........1600
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      LODOBS........1700
C                                                                        LODOBS........1800
      NOBS = NOBSN - 1                                                   LODOBS........1900
C                                                                        LODOBS........2000
      JLOAD = 0                                                          LODOBS........2100
      DO 300 J=JNEXT,NOBS                                                LODOBS........2200
         IF ((OBSPTS(J)%FRMT.EQ."OBS").AND.                              LODOBS........2300
     1      (OBSPTS(J)%SCHED.EQ.SCHDLS(OFP(NFLO)%ISCHED)%NAME))          LODOBS........2400
     2      THEN                                                         LODOBS........2500
            JLOAD = JLOAD + 1                                            LODOBS........2600
            JSET(JLOAD) = J                                              LODOBS........2700
            IF ((JLOAD.EQ.NOBLIN).OR.(J.EQ.NOBS)) THEN                   LODOBS........2800
               JNEXT = J + 1                                             LODOBS........2900
               RETURN                                                    LODOBS........3000
            END IF                                                       LODOBS........3100
         END IF                                                          LODOBS........3200
  300 CONTINUE                                                           LODOBS........3300
C                                                                        LODOBS........3400
      JNEXT = NOBS + 1                                                   LODOBS........3500
      RETURN                                                             LODOBS........3600
      END                                                                LODOBS........3700
C                                                                        LODOBS........3800
C     SUBROUTINE        N  A  F  U                 SUTRA VERSION 2.2     NAFU...........100
C                                                                        NAFU...........200
C *** PURPOSE :                                                          NAFU...........300
C ***  TO FIND THE NEXT AVAILABLE FORTRAN UNIT.  ON INPUT, IUNEXT IS     NAFU...........400
C ***  THE UNIT NUMBER FROM WHICH THE SEARCH IS TO BEGIN.  ON OUTPUT,    NAFU...........500
C ***  IUNEXT IS THE NEXT AVAILABLE UNIT NUMBER.                         NAFU...........600
C                                                                        NAFU...........700
      SUBROUTINE NAFU(IUNEXT,NJMAX,FN, IERROR)                                   NAFU...........800
      USE SCHDEF, ONLY : IUNIO                                           NAFU...........900
      USE BCSDEF                                                         NAFU..........1000
      USE FINDEF                                                         NAFU..........1100
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                NAFU..........1200
      CHARACTER*80 FN,UNAME,FNAME(0:13)                                  NAFU..........1300
      CHARACTER*80 ERRCOD,CHERR(10)                                      NAFU..........1400
      LOGICAL EXST                                                       NAFU..........1500
      LOGICAL ALCBCS,ALCFIN,ALCOBS                                       NAFU..........1600
      DIMENSION INERR(10),RLERR(10)                                      NAFU..........1700
      DIMENSION IUNIT(0:13)                                              NAFU..........1800
      COMMON /ALC/ ALCBCS,ALCFIN,ALCOBS                                  NAFU..........1900
      COMMON /FNAMES/ UNAME,FNAME                                        NAFU..........2000
      COMMON /FUNIB/ NFBCS                                               NAFU..........2100
      COMMON /FUNITA/ IUNIT                                              NAFU..........2200
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 NAFU..........2300
     1   K10,K11,K12,K13                                                 NAFU..........2400
C                                                                        NAFU..........2500
C.....CHECK "SUTRA.FIL" (UNIT K0)                                        NAFU..........2600
  100 IF (IUNEXT.EQ.K0) IUNEXT = IUNEXT + 1                              NAFU..........2700
C.....CHECK NON-INSERTED, NON-OBSERVATION FILES                          NAFU..........2800
  200 DO 300 NFF=0,13                                                    NAFU..........2900
         IF ((NFF.EQ.7).OR.(NFF.EQ.8)) CYCLE                             NAFU..........3000
         IF (IUNEXT.EQ.IUNIT(NFF)) THEN                                  NAFU..........3100
            IUNEXT = IUNEXT + 1                                          NAFU..........3200
            GOTO 100                                                     NAFU..........3300
         END IF                                                          NAFU..........3400
  300 CONTINUE                                                           NAFU..........3500
      IF (ALCBCS) THEN                                                   NAFU..........3600
         DO 350 NFB=1,NFBCS                                              NAFU..........3700
            IF (IUNEXT.EQ.IUNIB(NFB)) THEN                               NAFU..........3800
               IUNEXT = IUNEXT + 1                                       NAFU..........3900
               GOTO 100                                                  NAFU..........4000
            END IF                                                       NAFU..........4100
  350    CONTINUE                                                        NAFU..........4200
      END IF                                                             NAFU..........4300
C.....CHECK OBSERVATION FILES                                            NAFU..........4400
      IF (ALCOBS) THEN                                                   NAFU..........4500
  400    DO 500 NJ=1,NJMAX                                               NAFU..........4600
            IF (IUNEXT.EQ.IUNIO(NJ)) THEN                                NAFU..........4700
               IUNEXT = IUNEXT + 1                                       NAFU..........4800
               GOTO 100                                                  NAFU..........4900
            END IF                                                       NAFU..........5000
  500    CONTINUE                                                        NAFU..........5100
      END IF                                                             NAFU..........5200
C.....CHECK INSERTED FILES                                               NAFU..........5300
      IF ((IUNEXT.EQ.K1).OR.(IUNEXT.EQ.K2).OR.(IUNEXT.EQ.K9)) THEN       NAFU..........5400
         IUNEXT = IUNEXT + 1                                             NAFU..........5500
         GOTO 100                                                        NAFU..........5600
      END IF                                                             NAFU..........5700
      IF (ALCFIN) THEN                                                   NAFU..........5800
         DO 600 I=1,2+NFBCS                                              NAFU..........5900
            DO 600 K=1,NKS(I)                                            NAFU..........6000
               IF (IUNEXT.EQ.KLIST(I,K)) THEN                            NAFU..........6100
                  IUNEXT = IUNEXT + 1                                    NAFU..........6200
                  GOTO 100                                               NAFU..........6300
               END IF                                                    NAFU..........6400
  600    CONTINUE                                                        NAFU..........6500
      END IF                                                             NAFU..........6600
C.....IF THE UNIT NUMBER SELECTED IS NOT VALID, GENERATE ERROR           NAFU..........6700
      INQUIRE(UNIT=IUNEXT, EXIST=EXST)                                   NAFU..........6800
      IF (.NOT.EXST) THEN                                                NAFU..........6900
         ERRCOD = 'FIL-10'                                               NAFU..........7000
         INERR(1) = IUNEXT                                               NAFU..........7100
         CHERR(1) = UNAME                                                NAFU..........7200
         CHERR(2) = FN                                                   NAFU..........7300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        NAFU..........7400
	   RETURN
      END IF                                                             NAFU..........7500
C                                                                        NAFU..........7600
      RETURN                                                             NAFU..........7700
      END                                                                NAFU..........7800
C                                                                        NAFU..........7900
C     SUBROUTINE        N  O  D  A  L              SUTRA VERSION 2.2     NODAL..........100
C                                                                        NODAL..........200
C *** PURPOSE :                                                          NODAL..........300
C ***  (1) TO CARRY OUT ALL CELLWISE CALCULATIONS AND TO ADD CELLWISE    NODAL..........400
C ***      TERMS TO THE GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW    NODAL..........500
C ***      AND TRANSPORT EQUATIONS.                                      NODAL..........600
C ***  (2) TO ADD FLUID SOURCE AND SOLUTE MASS OR ENERGY SOURCE TERMS    NODAL..........700
C ***      TO THE MATRIX EQUATIONS.                                      NODAL..........800
C                                                                        NODAL..........900
C     SUBROUTINE        O  U  T  B  C  O  F        SUTRA VERSION 2.2     OUTBCOF........100
C                                                                        OUTBCOF........200
C *** PURPOSE :                                                          OUTBCOF........300
C ***  TO PRINT BOUNDARY CONDITION INFORMATION AT FLUID SOURCE/SINK      OUTBCOF........400
C ***  NODES IN A FLEXIBLE, COLUMNWISE FORMAT.  OUTPUT IS TO THE         OUTBCOF........500
C ***  BCOF FILE.                                                        OUTBCOF........600
C                                                                        OUTBCOF........700
C     SUBROUTINE        O  U  T  B  C  O  P        SUTRA VERSION 2.2     OUTBCOP........100
C                                                                        OUTBCOP........200
C *** PURPOSE :                                                          OUTBCOP........300
C ***  TO PRINT BOUNDARY CONDITION INFORMATION AT SPECIFIED PRESSURE     OUTBCOP........400
C ***  NODES IN A FLEXIBLE, COLUMNWISE FORMAT.  OUTPUT IS TO THE         OUTBCOP........500
C ***  BCOP FILE.                                                        OUTBCOP........600
C                                                                        OUTBCOP........700
C     SUBROUTINE        O  U  T  B  C  O  S        SUTRA VERSION 2.2     OUTBCOS........100
C                                                                        OUTBCOS........200
C *** PURPOSE :                                                          OUTBCOS........300
C ***  TO PRINT BOUNDARY CONDITION INFORMATION AT SOLUTE/ENERGY          OUTBCOS........400
C ***  SOURCE/SINK NODES IN A FLEXIBLE, COLUMNWISE FORMAT.  OUTPUT IS    OUTBCOS........500
C ***  TO THE BCOS FILE.                                                 OUTBCOS........600
C                                                                        OUTBCOS........700
C                                                                        OUTBCOS......23400
C     SUBROUTINE        O  U  T  B  C  O  U        SUTRA VERSION 2.2     OUTBCOU........100
C                                                                        OUTBCOU........200
C *** PURPOSE :                                                          OUTBCOU........300
C ***  TO PRINT BOUNDARY CONDITION INFORMATION AT SPECIFIED CONC/TEMP    OUTBCOU........400
C ***  NODES IN A FLEXIBLE, COLUMNWISE FORMAT.  OUTPUT IS TO THE         OUTBCOU........500
C ***  BCOU FILE.                                                        OUTBCOU........600
C                                                                        OUTBCOU........700
C     SUBROUTINE        O  U  T  E  L  E           SUTRA VERSION 2.2     OUTELE.........100
C                                                                        OUTELE.........200
C *** PURPOSE :                                                          OUTELE.........300
C ***  TO PRINT ELEMENT CENTROID COORDINATES AND VELOCITY COMPONENTS     OUTELE.........400
C ***  IN A FLEXIBLE, COLUMNWISE FORMAT.  OUTPUT IS TO THE ELE FILE.     OUTELE.........500
C                                                                        OUTELE.........600
C     SUBROUTINE        O  U  T  L  S  T  2        SUTRA VERSION 2.2     OUTLST2........100
C                                                                        OUTLST2........200
C *** PURPOSE :                                                          OUTLST2........300
C ***  TO PRINT PRESSURE AND TEMPERATURE OR CONCENTRATION                OUTLST2........400
C ***  SOLUTIONS AND TO OUTPUT INFORMATION ON TIME STEP, ITERATIONS,     OUTLST2........500
C ***  SATURATIONS, AND FLUID VELOCITIES FOR 2D PROBLEMS.                OUTLST2........600
C ***  OUTPUT IS TO THE LST FILE.                                        OUTLST2........700
C                                                                        OUTLST2........800
C     SUBROUTINE        O  U  T  L  S  T  3        SUTRA VERSION 2.2     OUTLST3........100
C                                                                        OUTLST3........200
C *** PURPOSE :                                                          OUTLST3........300
C ***  TO PRINT PRESSURE AND TEMPERATURE OR CONCENTRATION                OUTLST3........400
C ***  SOLUTIONS AND TO OUTPUT INFORMATION ON TIME STEP, ITERATIONS,     OUTLST3........500
C ***  SATURATIONS, AND FLUID VELOCITIES FOR 3D PROBLEMS.                OUTLST3........600
C ***  OUTPUT IS TO THE LST FILE.                                        OUTLST3........700
C                                                                        OUTLST3........800
C     SUBROUTINE        O  U  T  N  O  D           SUTRA VERSION 2.2     OUTNOD.........100
C                                                                        OUTNOD.........200
C *** PURPOSE :                                                          OUTNOD.........300
C ***  TO PRINT NODE COORDINATES, PRESSURES, CONCENTRATIONS OR           OUTNOD.........400
C ***  TEMPERATURES, AND SATURATIONS IN A FLEXIBLE, COLUMNWISE FORMAT.   OUTNOD.........500
C ***  OUTPUT IS TO THE NOD FILE.                                        OUTNOD.........600
C                                                                        OUTNOD.........700
C     SUBROUTINE        O  U  T  O  B  C           SUTRA VERSION 2.2     OUTOBC.........100
C                                                                        OUTOBC.........200
C *** PURPOSE :                                                          OUTOBC.........300
C ***  TO PRINT THE SOLUTION AT OBSERVATION POINTS.  SPECIFICALLY,       OUTOBC.........400
C ***  TO PRINT PRESSURES, CONCENTRATIONS OR TEMPERATURES, AND           OUTOBC.........500
C ***  SATURATIONS IN A COLUMNWISE FORMAT SIMILAR TO THAT USED IN THE    OUTOBC.........600
C ***  NODEWISE AND ELEMENTWISE OUTPUT FILES.                            OUTOBC.........700
C                                                                        OUTOBC.........800
C     SUBROUTINE        O  U  T  O  B  S           SUTRA VERSION 2.2     OUTOBS.........100
C                                                                        OUTOBS.........200
C *** PURPOSE :                                                          OUTOBS.........300
C ***  TO PRINT THE SOLUTION AT OBSERVATION POINTS.  SPECIFICALLY,       OUTOBS.........400
C ***  TO PRINT PRESSURES, CONCENTRATIONS OR TEMPERATURES, AND           OUTOBS.........500
C ***  SATURATIONS IN A COLUMNWISE FORMAT.                               OUTOBS.........600
C                                                                        OUTOBS.........700
C     SUBROUTINE        O  U  T  R  S  T           SUTRA VERSION 2.2     OUTRST.........100
C                                                                        OUTRST.........200
C *** PURPOSE :                                                          OUTRST.........300
C ***  TO STORE RESULTS THAT MAY LATER BE USED TO RESTART                OUTRST.........400
C ***  THE SIMULATION.                                                   OUTRST.........500
C                                                                        OUTRST.........600
C     SUBROUTINE        P  R  S  W  D  S           SUTRA VERSION 2.2     PRSWDS.........100
C                                                                        PRSWDS.........200
C *** PURPOSE :                                                          PRSWDS.........300
C ***  PARSE A CHARACTER STRING INTO WORDS.  WORDS ARE CONSIDERED TO BE  PRSWDS.........400
C ***  SEPARATED BY ONE OR MORE OF THE SINGLE-CHARACTER DELIMITER DELIM  PRSWDS.........500
C ***  AND/OR BLANKS.  PARSING CONTINUES UNTIL THE ENTIRE STRING HAS     PRSWDS.........600
C ***  BEEN PROCESSED OR THE NUMBER OF WORDS PARSED EQUALS NWMAX.  IF    PRSWDS.........700
C ***  NWMAX IS SET TO ZERO, PRSWDS SIMPLY COMPUTES THE NUMBER OF WORDS. PRSWDS.........800
C                                                                        PRSWDS.........900
      SUBROUTINE PRSWDS(STRING, DELIM, NWMAX, WORD, NWORDS)              PRSWDS........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                PRSWDS........1100
      CHARACTER*(*) STRING, WORD(NWMAX)                                  PRSWDS........1200
      CHARACTER DELIM*1, DELIM2*2                                        PRSWDS........1300
C                                                                        PRSWDS........1400
C.....DEFINE SET OF DELIMITERS (SPACE PLUS USER-SPECIFIED CHARACTER)     PRSWDS........1500
      DELIM2 = " " // DELIM                                              PRSWDS........1600
C                                                                        PRSWDS........1700
C.....COMPUTE LENGTH OF STRING WITHOUT TRAILING BLANKS                   PRSWDS........1800
      LSTRNG = LEN_TRIM(STRING)                                          PRSWDS........1900
C                                                                        PRSWDS........2000
C.....INITIALIZE WORD LIST AND COUNTERS                                  PRSWDS........2100
      DO 50 I=1,NWMAX                                                    PRSWDS........2200
         WORD(I) = ""                                                    PRSWDS........2300
   50 CONTINUE                                                           PRSWDS........2400
      NWORDS = 0                                                         PRSWDS........2500
      M2 = 0                                                             PRSWDS........2600
C                                                                        PRSWDS........2700
  300 CONTINUE                                                           PRSWDS........2800
C.....FIND THE NEXT CHARACTER THAT IS NOT A DELIMITER                    PRSWDS........2900
      M1L = VERIFY(STRING(M2+1:LSTRNG),DELIM2)                           PRSWDS........3000
      IF (M1L.EQ.0) RETURN                                               PRSWDS........3100
      M1 = M2 + M1L                                                      PRSWDS........3200
C                                                                        PRSWDS........3300
  400 CONTINUE                                                           PRSWDS........3400
C.....FIND THE NEXT CHARACTER THAT IS A DELIMITER                        PRSWDS........3500
      M2L = SCAN(STRING(M1+1:LSTRNG),DELIM2)                             PRSWDS........3600
      IF (M2L.EQ.0) THEN                                                 PRSWDS........3700
         M2 = LSTRNG + 1                                                 PRSWDS........3800
      ELSE                                                               PRSWDS........3900
         M2 = M1 + M2L                                                   PRSWDS........4000
      END IF                                                             PRSWDS........4100
C                                                                        PRSWDS........4200
  500 CONTINUE                                                           PRSWDS........4300
C.....STORE THE LATEST WORD FOUND                                        PRSWDS........4400
      NWORDS = NWORDS + 1                                                PRSWDS........4500
      IF (NWMAX.GT.0) WORD(NWORDS) = STRING(M1:M2-1)                     PRSWDS........4600
C                                                                        PRSWDS........4700
C.....IF END OF STRING NOT REACHED AND NUMBER OF WORDS IS LESS THAN      PRSWDS........4800
C        THE MAXIMUM ALLOWED, CONTINUE PARSING                           PRSWDS........4900
      IF ((M2.LT.LSTRNG).AND.((NWORDS.LT.NWMAX).OR.(NWMAX.EQ.0)))        PRSWDS........5000
     1   GOTO 300                                                        PRSWDS........5100
C                                                                        PRSWDS........5200
      RETURN                                                             PRSWDS........5300
      END                                                                PRSWDS........5400
C                                                                        PRSWDS........5500
C     SUBROUTINE        P  T  R  S  E  T           SUTRA VERSION 2.2     PTRSET.........100
C                                                                        PTRSET.........200
C *** PURPOSE :                                                          PTRSET.........300
C ***  TO SET UP POINTER ARRAYS NEEDED TO SPECIFY THE MATRIX STRUCTURE.  PTRSET.........400
C                                                                        PTRSET.........500
      SUBROUTINE PTRSET()                                                PTRSET.........600
      USE ALLARR                                                         PTRSET.........700
      USE PTRDEF                                                         PTRSET.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                PTRSET.........900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              PTRSET........1000
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            PTRSET........1100
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        PTRSET........1200
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        PTRSET........1300
C                                                                        PTRSET........1400
C.....SET UP POINTER ARRAYS IA AND JA THAT SPECIFY MATRIX STRUCTURE IN   PTRSET........1500
C        "SLAP COLUMN" FORMAT.  FOR EACH NODE, CONSTRUCT A LINKED LIST   PTRSET........1600
C        OF NEIGHBORING NODES.  HLIST(K) POINTS TO THE HEAD OF THE LIST  PTRSET........1700
C        FOR NODE K.  THEN, TRANSFER THE LISTS TO ARRAYS IA AND JA.      PTRSET........1800
C                                                                        PTRSET........1900
C.....ALLOCATE HLIST AND LLIST, AND INITIALIZE LIST LENGTHS TO ZERO.     PTRSET........2000
      ALLOCATE(LLIST(NN), HLIST(NN))                                     PTRSET........2100
      DO 490 I=1,NN                                                      PTRSET........2200
         ALLOCATE(HLIST(I)%PL)                                           PTRSET........2300
         LLIST(I) = 0                                                    PTRSET........2400
  490 CONTINUE                                                           PTRSET........2500
C.....LOOP THROUGH INCIDENCE LIST.                                       PTRSET........2600
      DO 500 L=1,NE                                                      PTRSET........2700
      DO 500 IL=1,N48                                                    PTRSET........2800
         IC = IN((L-1)*N48+IL)                                           PTRSET........2900
      DO 500 JL=1,N48                                                    PTRSET........3000
         JC = IN((L-1)*N48+JL)                                           PTRSET........3100
C........INSERT NEIGHBOR JC IN LIST FOR NODE IC IN ASCENDING ORDER.      PTRSET........3200
C           (IF DUPLICATE OR SELF-NEIGHBOR, SKIP IT.)                    PTRSET........3300
         IF (JC.EQ.IC) THEN                                              PTRSET........3400
C...........SKIP SELF-NEIGHBOR.                                          PTRSET........3500
            GOTO 500                                                     PTRSET........3600
         ELSE IF (LLIST(IC).EQ.0) THEN                                   PTRSET........3700
C...........PLACE FIRST LIST ENTRY AT HEAD.                              PTRSET........3800
            HLIST(IC)%PL%NODNUM = JC                                     PTRSET........3900
            GOTO 498                                                     PTRSET........4000
         ELSE                                                            PTRSET........4100
C...........INSERT INTO LIST, OR SKIP IF DUPLICATE.                      PTRSET........4200
            ALLOCATE(DENTPV)                                             PTRSET........4300
            DENTPI => DENTPV                                             PTRSET........4400
            DENTPV%NENT => HLIST(IC)%PL                                  PTRSET........4500
            DO 495 K=1,LLIST(IC)                                         PTRSET........4600
               DENT => DENTPV%NENT                                       PTRSET........4700
               IF (JC.EQ.DENT%NODNUM) THEN                               PTRSET........4800
                  GOTO 500                                               PTRSET........4900
               ELSE IF (JC.LT.DENT%NODNUM) THEN                          PTRSET........5000
                  ALLOCATE(DENTNW)                                       PTRSET........5100
                  DENTNW%NODNUM = JC                                     PTRSET........5200
                  DENTNW%NENT => DENT                                    PTRSET........5300
                  IF (K.EQ.1) THEN                                       PTRSET........5400
                     HLIST(IC)%PL => DENTNW                              PTRSET........5500
                  ELSE                                                   PTRSET........5600
                     DENTPV%NENT => DENTNW                               PTRSET........5700
                  END IF                                                 PTRSET........5800
                  DEALLOCATE(DENTPI)                                     PTRSET........5900
                  GOTO 498                                               PTRSET........6000
               END IF                                                    PTRSET........6100
               DENTPV => DENT                                            PTRSET........6200
  495       CONTINUE                                                     PTRSET........6300
C...........APPEND TO TAIL.                                              PTRSET........6400
            ALLOCATE(DENTNW)                                             PTRSET........6500
            DENTNW%NODNUM = JC                                           PTRSET........6600
            DENT%NENT => DENTNW                                          PTRSET........6700
            DEALLOCATE(DENTPI)                                           PTRSET........6800
         END IF                                                          PTRSET........6900
  498    LLIST(IC) = LLIST(IC) + 1                                       PTRSET........7000
  500 CONTINUE                                                           PTRSET........7100
C.....COMPUTE THE ARRAY DIMENSION NELT AND ALLOCATE ARRAY IA.            PTRSET........7200
      NELT = 0                                                           PTRSET........7300
      DO 600 I=1,NN                                                      PTRSET........7400
  600    NELT = NELT + LLIST(I) + 1                                      PTRSET........7500
      NDIMIA = NELT                                                      PTRSET........7600
      ALLOCATE(IA(NDIMIA))                                               PTRSET........7700
C.....TRANSFER THE LINKED LISTS TO ARRAYS IA AND JA IN SLAP COLUMN       PTRSET........7800
C        FORMAT.  DEALLOCATE POINTERS AS THEY ARE TRANSFERRED.           PTRSET........7900
      JASTRT = 1                                                         PTRSET........8000
      DO 660 I=1,NN                                                      PTRSET........8100
         JA(I) = JASTRT                                                  PTRSET........8200
         IA(JASTRT) = I                                                  PTRSET........8300
         DENT => HLIST(I)%PL                                             PTRSET........8400
         DO 650 K=1,LLIST(I)                                             PTRSET........8500
            IA(JASTRT + K) = DENT%NODNUM                                 PTRSET........8600
            DENTPV => DENT                                               PTRSET........8700
            DENT => DENT%NENT                                            PTRSET........8800
            DEALLOCATE(DENTPV)                                           PTRSET........8900
  650    CONTINUE                                                        PTRSET........9000
         JASTRT = JASTRT + LLIST(I) + 1                                  PTRSET........9100
  660 CONTINUE                                                           PTRSET........9200
      JA(NN + 1) = NELT + 1                                              PTRSET........9300
      DEALLOCATE(HLIST, LLIST)                                           PTRSET........9400
C                                                                        PTRSET........9500
      RETURN                                                             PTRSET........9600
      END                                                                PTRSET........9700
C                                                                        PTRSET........9800
C                                                                        PTRSET........9900
C     SUBROUTINE        P  U                       SUTRA VERSION 2.2     PU.............100
C                                                                        PU.............200
C *** PURPOSE :                                                          PU.............300
C ***  TO EVALUATE P AND U AT SPECIFIED LOCAL COORDINATES WITHIN A       PU.............400
C ***  2D OR 3D ELEMENT.  ADAPTED FROM SUBROUTINES BASIS2 AND BASIS3.    PU.............500
C                                                                        PU.............600
C     FUNCTION          P  U  S  W  F              SUTRA VERSION 2.2     PUSWF..........100
C                                                                        PUSWF..........200
C *** PURPOSE :                                                          PUSWF..........300
C ***  TO INTERPOLATE P, U, AND SW AT A FRACTIONAL TIME STEP (BETWEEN    PUSWF..........400
C ***  THE CURRENT AND PREVIOUS TIME STEPS) AND RETURN THE VALUES IN     PUSWF..........500
C ***  AN ARRAY.                                                         PUSWF..........600
C                                                                        PUSWF..........700
C     SUBROUTINE        R  E  A  D  I  F           SUTRA VERSION 2.2     READIF.........100
C                                                                        READIF.........200
C *** PURPOSE :                                                          READIF.........300
C ***  TO READ A LINE FROM AN INPUT FILE INTO THE CHARACTER VARIABLE     READIF.........400
C ***  INTFIL.  HANDLE OPENING AND CLOSING OF INSERTED FILES AS          READIF.........500
C ***  NECESSARY.                                                        READIF.........600
C                                                                        READIF.........700
!      SUBROUTINE READIF_22(IERROR, KUU, NFB, INTFIL, ERRCIO, CHERIN)                READIF.........800
      SUBROUTINE READIF_22(IERROR, KUU, NFB, INTFIL, ERRCIO)                READIF.........800
      USE SCHDEF, ONLY : IUNIO, FNAMO                                    READIF.........900
      USE BCSDEF                                                         READIF........1000
      USE FINDEF                                                         READIF........1100
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                READIF........1200
      PARAMETER (KINMIN=10)                                              READIF........1300
      CHARACTER INTFIL*1000                                              READIF........1400
      CHARACTER*80 ERRCOD,ERRCIO,CHERR(10)                               READIF........1500
!      CHARACTER*80, DIMENSION(10), OPTIONAL :: CHERIN                    READIF........1600
      CHARACTER*80 UNAME,FNAME                                           READIF........1700
      CHARACTER ERRF*3, FINS*80                                          READIF........1800
      LOGICAL IS                                                         READIF........1900
      DIMENSION INERR(10),RLERR(10)                                      READIF........2000
      DIMENSION IUNIT(0:13)                                              READIF........2100
      DIMENSION FNAME(0:13)                                              READIF........2200
      COMMON /FNAMES/ UNAME,FNAME                                        READIF........2300
      COMMON /FUNIB/ NFBCS                                               READIF........2400
      COMMON /FUNITA/ IUNIT                                              READIF........2500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 READIF........2600
     1   K10,K11,K12,K13                                                 READIF........2700
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      READIF........2800
C                                                                        READIF........2900
C.....COPY ERRCIO INTO ERRCOD AND ARRAY CHERIN (IF PRESENT AS AN         READIF........3000
C        ARGUMENT) INTO CHERR.                                           READIF........3100
      ERRCOD = ERRCIO                                                    READIF........3200
! RBW begin change
! This causes an error for some reason
!      IF (PRESENT(CHERIN)) CHERR = CHERIN                                READIF........3300
! RBW end change
C                                                                        READIF........3400
C.....COPY KUU INTO KU. SHOULD AVOID CHANGING KUU, SINCE IT IS ALREADY   READIF........3500
C        LINKED TO K1, K2, OR K9 THROUGH THE ARGUMENT LIST, AND THE      READIF........3600
C        LATTER ARE ALSO PASSED IN THROUGH COMMON BLOCK FUNITS.          READIF........3700
      KU = KUU                                                           READIF........3800
C                                                                        READIF........3900
C.....READ A LINE OF INPUT (UP TO 1000 CHARACTERS) FROM UNIT KU          READIF........4000
C        INTO INTFIL                                                     READIF........4100
100   READ(KU,'(A)',IOSTAT=INERR(1)) INTFIL                              READIF........4200
C.....IF THE END OF AN INSERTED FILE IS REACHED, CLOSE THAT FILE AND     READIF........4300
C        CONTINUE READING FROM THE NEXT-LEVEL-UP FILE                    READIF........4400
      IF (INERR(1).LT.0) THEN                                            READIF........4500
C........SET FLAG IK TO INDICATE WHETHER THE READ WAS ATTEMPTED FROM     READIF........4600
C           AN INP DATASET (IK=1), AN ICS DATASET (IK=2), OR A BCS       READIF........4700
C           DATASET (IK>2)                                               READIF........4800
         IF (KU.EQ.K1) THEN                                              READIF........4900
            IK = 1                                                       READIF........5000
            IIK = 1                                                      READIF........5100
         ELSE IF (KU.EQ.K2) THEN                                         READIF........5200
            IK = 2                                                       READIF........5300
            IIK = 2                                                      READIF........5400
         ELSE                                                            READIF........5500
            IK = 2 + NFB                                                 READIF........5600
            IIK = 9                                                      READIF........5700
         END IF                                                          READIF........5800
C........IF READING FROM AN INSERTED FILE, CLOSE THAT FILE, UPDATE       READIF........5900
C           UNIT NUMBERS, FILENAME, AND COUNTER TO INDICATE THE          READIF........6000
C           NEXT-LEVEL-UP FILE, AND CONTINUE READING                     READIF........6100
         IF (NKS(IK).GT.0) THEN                                          READIF........6200
            CLOSE(KU)                                                    READIF........6300
            IF (KU.EQ.K1) THEN                                           READIF........6400
               K1 = KLIST(IK, NKS(IK))                                   READIF........6500
               FNAME(IIK) = FNAIN(IK, NKS(IK))                           READIF........6600
            ELSE IF (KU.EQ.K2) THEN                                      READIF........6700
               K2 = KLIST(IK, NKS(IK))                                   READIF........6800
               FNAME(IIK) = FNAIN(IK, NKS(IK))                           READIF........6900
            ELSE                                                         READIF........7000
               K9 = KLIST(IK, NKS(IK))                                   READIF........7100
               FNAMB(NFB) = FNAIN(IK, NKS(IK))                           READIF........7200
               IUNIB(NFB) = K9                                           READIF........7300
               FNAME(IIK) = FNAIN(IK, NKS(IK))                           READIF........7400
            END IF                                                       READIF........7500
            KU = KLIST(IK, NKS(IK))                                      READIF........7600
            NKS(IK) = NKS(IK) - 1                                        READIF........7700
            GOTO 100                                                     READIF........7800
         ELSE                                                            READIF........7900
C...........REACHED END OF ZERO-LEVEL FILE. IF ERRCOD="NO_EOF_ERR"       READIF........8000
C                ON INPUT, RETURN; ELSE GENERATE ERROR.                  READIF........8100
            IF (ERRCIO.EQ."NO_EOF_ERR") THEN                             READIF........8200
               ERRCIO = "EOF"                                            READIF........8300
               GOTO 999                                                  READIF........8400
            ELSE                                                         READIF........8500
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF........8600
	         RETURN
            END IF                                                       READIF........8700
         END IF                                                          READIF........8800
C.....ELSE IF THE READ RESULTS IN A DIFFERENT KIND OF ERROR, GENERATE    READIF........8900
C        ERROR MESSAGE                                                   READIF........9000
      ELSE IF (INERR(1).GT.0) THEN                                       READIF........9100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        READIF........9200
	   RETURN
      END IF                                                             READIF........9300
C                                                                        READIF........9400
C.....IF BLANK OR COMMENT LINE, SKIP IT.                                 READIF........9500
      IF ((INTFIL(1:1).EQ.'#').OR.(INTFIL.EQ.'')) GOTO 100               READIF........9600
C                                                                        READIF........9700
C.....IF INSERT STATEMENT, OPEN THE FILE AND CONTINUE READING            READIF........9800
      IF (INTFIL(1:7).EQ.'@INSERT') THEN                                 READIF........9900
C........SET FLAG IK TO INDICATE WHETHER THE READ WAS DONE FROM          READIF.......10000
C           AN INP DATASET (IK=1), AN ICS DATASET (IK=2), OR A BCS       READIF.......10100
C           DATASET (IK>2).  SET ERRF TO THE FILE TYPE ('INP', 'ICS',    READIF.......10200
C           OR 'BCS').                                                   READIF.......10300
         IF (KU.EQ.K1) THEN                                              READIF.......10400
            IK = 1                                                       READIF.......10500
            IIK = 1                                                      READIF.......10600
            ERRF = 'INP'                                                 READIF.......10700
         ELSE IF (KU.EQ.K2) THEN                                         READIF.......10800
            IK = 2                                                       READIF.......10900
            IIK = 2                                                      READIF.......11000
            ERRF = 'ICS'                                                 READIF.......11100
         ELSE                                                            READIF.......11200
            IK = 2 + NFB                                                 READIF.......11300
            IIK = 9                                                      READIF.......11400
            ERRF = 'BCS'                                                 READIF.......11500
         END IF                                                          READIF.......11600
C........READ THE FILE SPECIFICATION FOR THE INSERTED FILE               READIF.......11700
         READ(INTFIL(8:),*,IOSTAT=INERR(1)) KINS, FINS                   READIF.......11800
         IF (INERR(1).NE.0) THEN                                         READIF.......11900
            CHERR(1) = ERRCOD                                            READIF.......12000
            ERRCOD = 'REA-' // ERRF // '-INS'                            READIF.......12100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     READIF.......12200
	      RETURN
         END IF                                                          READIF.......12300
C........CHECK FOR DUPLICATE FILENAME AMONG INSERTED FILES               READIF.......12400
         DO 550 I=1,2+NFB                                                READIF.......12500
         DO 550 K=1,NKS(I)                                               READIF.......12600
            IF (FINS.EQ.FNAIN(I, K)) THEN                                READIF.......12700
               ERRCOD = 'FIL-4'                                          READIF.......12800
               INERR(1) = KINS                                           READIF.......12900
               CHERR(1) = FNAME(IIK)                                     READIF.......13000
               CHERR(2) = FINS                                           READIF.......13100
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF.......13200
 	         RETURN
            END IF                                                       READIF.......13300
  550    CONTINUE                                                        READIF.......13400
C........CHECK FOR DUPLICATE FILENAME AMONG NON-INSERTED,                READIF.......13500
C           NON-OBSERVATION FILES                                        READIF.......13600
         DO 560 NFF=0,13                                                 READIF.......13700
            IF ((NFF.GE.7).OR.(NFF.LE.9)) CYCLE                          READIF.......13800
            IF (FINS.EQ.FNAME(NFF)) THEN                                 READIF.......13900
               ERRCOD = 'FIL-4'                                          READIF.......14000
               INERR(1) = KINS                                           READIF.......14100
               CHERR(1) = FNAME(IIK)                                     READIF.......14200
               CHERR(2) = FINS                                           READIF.......14300
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF.......14400
 	         RETURN
            END IF                                                       READIF.......14500
  560    CONTINUE                                                        READIF.......14600
         DO 565 NFFB=1,NFBCS                                             READIF.......14700
            IF (FINS.EQ.FNAMB(NFFB)) THEN                                READIF.......14800
               ERRCOD = 'FIL-4'                                          READIF.......14900
               INERR(1) = KINS                                           READIF.......15000
               CHERR(1) = FNAME(IIK)                                     READIF.......15100
               CHERR(2) = FINS                                           READIF.......15200
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF.......15300
 	         RETURN
            END IF                                                       READIF.......15400
  565    CONTINUE                                                        READIF.......15500
C........CHECK FOR DUPLICATE FILENAME AMONG OBSERVATION FILES            READIF.......15600
         DO 570 NJ=1,NFLOMX                                              READIF.......15700
            IF (FINS.EQ.FNAMO(NJ)) THEN                                  READIF.......15800
               ERRCOD = 'FIL-4'                                          READIF.......15900
               INERR(1) = KINS                                           READIF.......16000
               CHERR(1) = FNAME(IIK)                                     READIF.......16100
               CHERR(2) = FINS                                           READIF.......16200
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF.......16300
 	         RETURN
            END IF                                                       READIF.......16400
  570    CONTINUE                                                        READIF.......16500
C........IF THE SPECIFIED UNIT NUMBER IS LESS THAN KINMIN,               READIF.......16600
C           SET IT TO KINMIN                                             READIF.......16700
         KINS = MAX(KINS, KINMIN)                                        READIF.......16800
C........IF THE FILE TO BE INSERTED EXISTS, ASSIGN IT A UNIT NUMBER      READIF.......16900
C           AND OPEN IT                                                  READIF.......17000
         INQUIRE(FILE=FINS,EXIST=IS)                                     READIF.......17100
         IF (IS) THEN                                                    READIF.......17200
            CALL NAFU(KINS,NFLOMX,FINS, IERROR)                                  READIF.......17300
	      if (IERROR.NE.0) return
            OPEN(UNIT=KINS,FILE=FINS,STATUS='OLD',FORM='FORMATTED',      READIF.......17400
     1         IOSTAT=KERR)                                              READIF.......17500
            IF (KERR.GT.0) THEN                                          READIF.......17600
               CHERR(1) = FNAME(IIK)                                     READIF.......17700
               CHERR(2) = FINS                                           READIF.......17800
               INERR(1) = KINS                                           READIF.......17900
               ERRCOD = 'FIL-2'                                          READIF.......18000
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF.......18100
 	         RETURN
            END IF                                                       READIF.......18200
         ELSE                                                            READIF.......18300
            CHERR(1) = FNAME(IIK)                                        READIF.......18400
            CHERR(2) = FINS                                              READIF.......18500
            ERRCOD = 'FIL-1'                                             READIF.......18600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     READIF.......18700
 	      RETURN
         END IF                                                          READIF.......18800
C........UPDATE THE INSERTION COUNTER.  IF THE COUNT EXCEEDS 20,         READIF.......18900
C           GENERATE AN ERROR                                            READIF.......19000
         NKS(IK) = NKS(IK) + 1                                           READIF.......19100
         IF (NKS(IK).GT.20) THEN                                         READIF.......19200
            CHERR(1) = FNAME(IIK)                                        READIF.......19300
            CHERR(2) = FINS                                              READIF.......19400
            ERRCOD = 'FIL-8'                                             READIF.......19500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     READIF.......19600
 	      RETURN
         END IF                                                          READIF.......19700
C........UPDATE UNIT NUMBERS AND FILENAMES TO INDICATE THE NEWLY         READIF.......19800
C           INSERTED FILE, AND CONTINUE READING                          READIF.......19900
         IF (KU.EQ.K1) THEN                                              READIF.......20000
            K1 = KINS                                                    READIF.......20100
            FNAIN(IK, NKS(IK)) = FNAME(IIK)                              READIF.......20200
            FNAME(IIK) = FINS                                            READIF.......20300
         ELSE IF (KU.EQ.K2) THEN                                         READIF.......20400
            K2 = KINS                                                    READIF.......20500
            FNAIN(IK, NKS(IK)) = FNAME(IIK)                              READIF.......20600
            FNAME(IIK) = FINS                                            READIF.......20700
         ELSE                                                            READIF.......20800
            K9 = KINS                                                    READIF.......20900
            FNAIN(IK, NKS(IK)) = FNAMB(NFB)                              READIF.......21000
            FNAMB(NFB) = FINS                                            READIF.......21100
            IUNIB(NFB) = KINS                                            READIF.......21200
C...........SET FNAME(9) EQUAL TO FNAMB(NFB) FOR CONVENIENCE IN          READIF.......21300
C              ERROR HANDLING                                            READIF.......21400
            FNAME(9) = FNAMB(NFB)                                        READIF.......21500
         END IF                                                          READIF.......21600
         KLIST(IK, NKS(IK)) = KU                                         READIF.......21700
         KU = KINS                                                       READIF.......21800
         GOTO 100                                                        READIF.......21900
      END IF                                                             READIF.......22000
C                                                                        READIF.......22100
  999 RETURN                                                             READIF.......22200
      END                                                                READIF.......22300
C                                                                        READIF.......22400
C     SUBROUTINE        R  O  T  A  T  E           SUTRA VERSION 2.2     ROTATE.........100
C                                                                        ROTATE.........200
C *** PURPOSE :                                                          ROTATE.........300
C ***  TO TRANSFORM THE COORDINATES OF A VECTOR, {x}, BY APPLYING THE    ROTATE.........400
C ***  ROTATION MATRIX, [G]:  {xp}=[G]{x}.                               ROTATE.........500
C                                                                        ROTATE.........600
C                                                                        ROTATE........1800
C     SUBROUTINE        R  O  T  M  A  T           SUTRA VERSION 2.2     ROTMAT.........100
C                                                                        ROTMAT.........200
C *** PURPOSE :                                                          ROTMAT.........300
C ***  TO COMPUTE A TRANSFORMATION MATRIX, [G], THAT CONVERTS            ROTMAT.........400
C ***  COORDINATES OF A VECTOR, {v}, FROM A COORDINATE SYSTEM (X, Y, Z)  ROTMAT.........500
C ***  TO A NEW COORDINATE SYSTEM (X', Y', Z'):  {v'} = [G]{v}.          ROTMAT.........600
C ***  THE OVERALL TRANSFORMATION IS THE RESULT OF THREE ROTATIONS       ROTMAT.........700
C ***  APPLIED CONSECUTIVELY:                                            ROTMAT.........800
C ***  A1 = ROTATION IN THE XY-PLANE, COUNTER-CLOCKWISE FROM THE         ROTMAT.........900
C ***     +X-AXIS (LOOKING DOWN THE +Z-AXIS TOWARD THE ORIGIN),          ROTMAT........1000
C ***  A2 = ROTATION IN THE NEW XZ-PLANE, COUNTER-CLOCKWISE FROM THE     ROTMAT........1100
C ***     NEW +X-AXIS (LOOKING DOWN THE NEW +Y-AXIS TOWARD THE ORIGIN),  ROTMAT........1200
C ***  A3 = ROTATION IN THE NEW YZ-PLANE, COUNTER-CLOCKWISE FROM THE     ROTMAT........1300
C ***     NEW +Y-AXIS (LOOKING DOWN THE NEW +X-AXIS TOWARD THE ORIGIN).  ROTMAT........1400
C                                                                        ROTMAT........1500
C                                                                        ROTMAT........3900
C     SUBROUTINE        S  O  L  V  E  B           SUTRA VERSION 2.2     SOLVEB.........100
C                                                                        SOLVEB.........200
C *** PURPOSE :                                                          SOLVEB.........300
C ***  TO SOLVE THE MATRIX EQUATION BY:                                  SOLVEB.........400
C ***   (1) DECOMPOSING THE MATRIX                                       SOLVEB.........500
C ***   (2) MODIFYING THE RIGHT-HAND SIDE                                SOLVEB.........600
C ***   (3) BACK-SUBSTITUTING FOR THE SOLUTION                           SOLVEB.........700
C                                                                        SOLVEB.........800
C     SUBROUTINE        S  O  L  V  E  R           SUTRA VERSION 2.2     SOLVER.........100
C                                                                        SOLVER.........200
C *** PURPOSE :                                                          SOLVER.........300
C ***  TO CALL THE APPROPRIATE MATRIX EQUATION SOLVER.                   SOLVER.........400
C                                                                        SOLVER.........500
C     SUBROUTINE        S  O  L  W  R  P           SUTRA VERSION 2.2     SOLWRP.........100
C                                                                        SOLWRP.........200
C *** PURPOSE :                                                          SOLWRP.........300
C ***  TO SERVE AS A WRAPPER FOR THE ITERATIVE SOLVERS, PERFORMING       SOLWRP.........400
C ***  SOME PRELIMINARIES ON VECTORS BEFORE CALLING A SOLVER.            SOLWRP.........500
C                                                                        SOLWRP.........600
C     SUBROUTINE        S  O  U  R  C  E           SUTRA VERSION 2.2     SOURCE.........100
C                                                                        SOURCE.........200
C *** PURPOSE :                                                          SOURCE.........300
C ***  TO READ AND ORGANIZE DEFAULT VALUES FOR FLUID MASS SOURCE DATA    SOURCE.........400
C ***  AND ENERGY OR SOLUTE MASS SOURCE DATA.                            SOURCE.........500
C                                                                        SOURCE.........600
      SUBROUTINE SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT,          SOURCE.........700
     1   IBCSOP,IBCSOU, IERROR, IBOUSZ, IBNODE, IPOS)                                                  SOURCE.........800
      USE EXPINT                                                         SOURCE.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOURCE........1000
      CHARACTER INTFIL*1000                                              SOURCE........1100
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:13)                    SOURCE........1200
      INTEGER(1) IBCSOP(NSOP),IBCSOU(NSOU)                               SOURCE........1300
      DIMENSION KTYPE(2)                                                 SOURCE........1400
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SOURCE........1500
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,           SOURCE........1600
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE                                       SOURCE........1700
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SOURCE........1800
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            SOURCE........1900
      COMMON /FNAMES/ UNAME,FNAME                                        SOURCE........2000
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 SOURCE........2100
     1   K10,K11,K12,K13                                                 SOURCE........2200
      DIMENSION QIN(NN),UIN(NN),IQSOP(NSOP),QUIN(NN),IQSOU(NSOU)         SOURCE........2300
      DIMENSION INERR(10),RLERR(10)                                      SOURCE........2400
C RBW
      INTEGER IERROR
	INTEGER IBOUSZ, IPOS
	INTEGER IBNODE(IBOUSZ)
C RBW
C                                                                        SOURCE........2500
C.....NSOPI IS ACTUAL NUMBER OF FLUID SOURCE NODES.                      SOURCE........2600
C.....NSOUI IS ACTUAL NUMBER OF SOLUTE MASS OR ENERGY SOURCE NODES.      SOURCE........2700
      NSOPI=NSOP-1                                                       SOURCE........2800
      NSOUI=NSOU-1                                                       SOURCE........2900
      IQSOPT=1                                                           SOURCE........3000
      IQSOUT=1                                                           SOURCE........3100
      NIQP=0                                                             SOURCE........3200
      NIQU=0                                                             SOURCE........3300
C RBW
      IF(NSOPI.EQ.0) then
	  ipos = 1
	  ibnode(ipos) = 0
        GOTO 1000          
      end if                                               
C RBW
      IF(NSOPI.EQ.0) GOTO 1000                                           SOURCE........3400
!      IF(ME) 50,50,150                                                   SOURCE........3500
!   50 WRITE(K3,100)                                                      SOURCE........3600
!  100 FORMAT('1'////11X,'F L U I D   S O U R C E   D A T A'              SOURCE........3700
!     1   ////11X,'**** NODES AT WHICH FLUID INFLOWS OR OUTFLOWS ARE ',   SOURCE........3800
!     2   'SPECIFIED ****'                                                SOURCE........3900
!     3   //16X,13X,'DEFAULT FLUID',5X,'DEFAULT CONCENTRATION'            SOURCE........4000
!     4    /16X,6X,'INFLOW(+)/OUTFLOW(-)',8X,'OF INFLOWING FLUID'         SOURCE........4100
!     5    /12X,'NODE',7X,'(FLUID MASS/SECOND)',2X,                       SOURCE........4200
!     6   '(MASS SOLUTE/MASS WATER)'//)                                   SOURCE........4300
!      GOTO 300                                                           SOURCE........4400
!  150 WRITE(K3,200)                                                      SOURCE........4500
!  200 FORMAT('1'////11X,'F L U I D   S O U R C E   D A T A'              SOURCE........4600
!     1   ////11X,'**** NODES AT WHICH FLUID INFLOWS OR OUTFLOWS ARE ',   SOURCE........4700
!     2   'SPECIFIED ****'                                                SOURCE........4800
!     3   //16X,13X,'DEFAULT FLUID',5X,'  DEFAULT TEMPERATURE'            SOURCE........4900
!     4    /16X,6X,'INFLOW(+)/OUTFLOW(-)',8X,'OF INFLOWING FLUID'         SOURCE........5000
!     5    /12X,'NODE',7X,'(FLUID MASS/SECOND)',2X,                       SOURCE........5100
!     6   '       (DEGREES CELSIUS)'//)                                   SOURCE........5200
C                                                                        SOURCE........5300
C.....INPUT DATASET 17:  DATA FOR FLUID SOURCES AND SINKS                SOURCE........5400
  300 CONTINUE                                                           SOURCE........5500
C RBW
      IPOS = 1 
      IBNODE(IPOS) = NSOPI
C RBW
  305 NIQP=NIQP+1                                                        SOURCE........5600
      ERRCOD = 'REA-INP-17'                                              SOURCE........5700
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 SOURCE........5800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) IQCP                                SOURCE........5900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                          SOURCE........6000
	   RETURN
	ENDIF
      IQCPA = IABS(IQCP)                                                 SOURCE........6100
C RBW
	IF (IQCPA.GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IQCPA - 1  
	ENDIF
C RBW
      IF (IQCP.EQ.0) THEN                                                SOURCE........6200
         GOTO 700                                                        SOURCE........6300
      ELSE IF (IQCPA.GT.NN) THEN                                         SOURCE........6400
         ERRCOD = 'INP-17-1'                                             SOURCE........6500
         INERR(1) = IQCPA                                                SOURCE........6600
         INERR(2) = NN                                                   SOURCE........6700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SOURCE........6800
	   RETURN
      ELSE IF (NIQP.GT.NSOPI) THEN                                       SOURCE........6900
         GOTO 305                                                        SOURCE........7000
      END IF                                                             SOURCE........7100
      ERRCOD = 'REA-INP-17'                                              SOURCE........7200
      IF (IQCP.GT.0) THEN                                                SOURCE........7300
         READ(INTFIL,*,IOSTAT=INERR(1)) IQCP,QINC                        SOURCE........7400
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)               SOURCE........7500
	      RETURN
	   ENDIF
         IF (QINC.GT.0D0) THEN                                           SOURCE........7600
            READ(INTFIL,*,IOSTAT=INERR(1)) IQCP,QINC,UINC                SOURCE........7700
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)            SOURCE........7800
	         RETURN
	      ENDIF
         ELSE                                                            SOURCE........7900
            UINC = 0D0                                                   SOURCE........8000
         END IF                                                          SOURCE........8100
      ELSE                                                               SOURCE........8200
         QINC = 0D0                                                      SOURCE........8300
         UINC = 0D0                                                      SOURCE........8400
      END IF                                                             SOURCE........8500
      IQSOP(NIQP)=IQCP                                                   SOURCE........8600
      IF(IQCP.LT.0) IQSOPT=-1                                            SOURCE........8700
      IQP=IABS(IQCP)                                                     SOURCE........8800
      QIN(IQP)=QINC                                                      SOURCE........8900
      UIN(IQP)=UINC                                                      SOURCE........9000
      IF(IQCP.GT.0) GOTO 450                                             SOURCE........9100
!      WRITE(K3,500) IQCP                                                 SOURCE........9200
      GOTO 600                                                           SOURCE........9300
  450 IF(QINC.GT.0) GOTO 460                                             SOURCE........9400
!      WRITE(K3,500) IQCP,QINC                                            SOURCE........9500
      GOTO 600                                                           SOURCE........9600
  460 CONTINUE
!  460 WRITE(K3,500) IQCP,QINC,UINC                                       SOURCE........9700
  !500 FORMAT(7X,I9,6X,1PE20.13,6X,1PE20.13)                              SOURCE........9800
  600 GOTO 305                                                           SOURCE........9900
  700 NIQP = NIQP - 1                                                    SOURCE.......10000
      IF(NIQP.EQ.NSOPI) GOTO 800                                         SOURCE.......10100
         ERRCOD = 'INP-3,17-1'                                           SOURCE.......10200
         INERR(1) = NIQP                                                 SOURCE.......10300
         INERR(2) = NSOPI                                                SOURCE.......10400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SOURCE.......10500
	   RETURN
  800 CONTINUE
C RBW
      IF(NSOUI.EQ.0) THEN
	   IPOS = IPOS + 1 
         IBNODE(IPOS) = 0
	END IF
C RBW
!      IF(IQSOPT.NE.-1) GOTO 950                                          SOURCE.......10600
!      IF(ME) 805,805,815                                                 SOURCE.......10700
!  805 WRITE(K3,806)                                                      SOURCE.......10800
!  806 FORMAT(//12X,'TIME-DEPENDENT FLUID SOURCE/SINK OR INFLOW ',        SOURCE.......10900
!     1   'CONCENTRATION'/12X,'SET IN SUBROUTINE BCTIME IS INDICATED ',   SOURCE.......11000
!     2   'BY NEGATIVE NODE NUMBER')                                      SOURCE.......11100
!      GOTO 950                                                           SOURCE.......11200
!  815 WRITE(K3,816)                                                      SOURCE.......11300
!  816 FORMAT(//12X,'TIME-DEPENDENT FLUID SOURCE/SINK OR INFLOW ',        SOURCE.......11400
!     1   'TEMPERATURE'/12X,'SET IN SUBROUTINE BCTIME IS INDICATED ',     SOURCE.......11500
!     2   'BY NEGATIVE NODE NUMBER')                                      SOURCE.......11600
!  950 WRITE(K3,952)                                                      SOURCE.......11700
!  952 FORMAT(/11X,'SPECIFICATIONS MADE IN (OPTIONAL) ',                  SOURCE.......11800
!     1   'BCS INPUT FILES TAKE PRECEDENCE OVER THE'/11X,                 SOURCE.......11900
!     2   'DEFAULT VALUES LISTED ABOVE AND ANY VALUES ',                  SOURCE.......12000
!     3   'SET IN SUBROUTINE BCTIME.')                                    SOURCE.......12100
C.....INITIALIZE ARRAY THAT INDICATES WHERE FLUID SOURCE                 SOURCE.......12200
C        CONDITIONS WERE SET (0 = INP FILE)                              SOURCE.......12300
      IBCSOP = 0                                                         SOURCE.......12400
C                                                                        SOURCE.......12500
C                                                                        SOURCE.......12600
C                                                                        SOURCE.......12700
 1000 IF(NSOUI.EQ.0) GOTO 9000                                           SOURCE.......12800
!      IF(ME) 1050,1050,1150                                              SOURCE.......12900
! 1050 WRITE(K3,1100)                                                     SOURCE.......13000
! 1100 FORMAT(////////11X,'S O L U T E   S O U R C E   D A T A'           SOURCE.......13100
!     1   ////11X,'**** NODES AT WHICH SOURCES OR SINKS OF SOLUTE ',      SOURCE.......13200
!     2   'MASS ARE SPECIFIED ****'                                       SOURCE.......13300
!     3   //16X,12X,'DEFAULT SOLUTE'/16X,9X,'SOURCE(+)/SINK(-)'           SOURCE.......13400
!     4    /12X,'NODE',6X,'(SOLUTE MASS/SECOND)'//)                       SOURCE.......13500
!      GOTO 1305                                                          SOURCE.......13600
! 1150 WRITE(K3,1200)                                                     SOURCE.......13700
! 1200 FORMAT(////////11X,'E N E R G Y   S O U R C E   D A T A'           SOURCE.......13800
!     1   ////11X,'**** NODES AT WHICH SOURCES OR SINKS OF ',             SOURCE.......13900
!     2   'ENERGY ARE SPECIFIED ****'                                     SOURCE.......14000
!     3   //16X,12X,'DEFAULT ENERGY'/16X,9X,'SOURCE(+)/SINK(-)'           SOURCE.......14100
!     4    /12X,'NODE',11X,'(ENERGY/SECOND)'//)                           SOURCE.......14200
C                                                                        SOURCE.......14300
C.....INPUT DATASET 18:  DATA FOR ENERGY OR SOLUTE MASS SOURCES OR SINKS SOURCE.......14400
C RBW
      IPOS = IPOS + 1 
      IBNODE(IPOS) = NSOUI
C RBW
 1305 NIQU=NIQU+1                                                        SOURCE.......14500
      ERRCOD = 'REA-INP-18'                                              SOURCE.......14600
      CALL READIF_22(IERROR,K1, 0, INTFIL, ERRCOD)                                 SOURCE.......14700
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) IQCU                                SOURCE.......14800
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  SOURCE.......14900
	   RETURN
      END IF                                                             SOURCE.......13600
      IQCUA = IABS(IQCU)                                                 SOURCE.......15000
C RBW
	IF (IQCUA.GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IQCUA - 1   
	ENDIF
C RBW
      IF (IQCU.EQ.0) THEN                                                SOURCE.......15100
         GOTO 1700                                                       SOURCE.......15200
      ELSE IF (IQCUA.GT.NN) THEN                                         SOURCE.......15300
         ERRCOD = 'INP-18-1'                                             SOURCE.......15400
         INERR(1) = IQCUA                                                SOURCE.......15500
         INERR(2) = NN                                                   SOURCE.......15600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SOURCE.......15700
	   RETURN
      ELSE IF (NIQU.GT.NSOUI) THEN                                       SOURCE.......15800
         GOTO 1305                                                       SOURCE.......15900
      END IF                                                             SOURCE.......16000
      IF (IQCU.GT.0) THEN                                                SOURCE.......16100
         ERRCOD = 'REA-INP-18'                                           SOURCE.......16200
         READ(INTFIL,*,IOSTAT=INERR(1)) IQCU,QUINC                       SOURCE.......16300
         IF (INERR(1).NE.0) THEN
	      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                       SOURCE.......16400
	      RETURN
	   ENDIF
      ELSE                                                               SOURCE.......16500
         QUINC = 0D0                                                     SOURCE.......16600
      END IF                                                             SOURCE.......16700
      IQSOU(NIQU)=IQCU                                                   SOURCE.......16800
      IF(IQCU.LT.0) IQSOUT=-1                                            SOURCE.......16900
      IQU=IABS(IQCU)                                                     SOURCE.......17000
      QUIN(IQU)=QUINC                                                    SOURCE.......17100
      IF(IQCU.GT.0) GOTO 1450                                            SOURCE.......17200
!      WRITE(K3,1500) IQCU                                                SOURCE.......17300
      GOTO 1600                                                          SOURCE.......17400
 1450 CONTINUE
! 1450 WRITE(K3,1500) IQCU,QUINC                                          SOURCE.......17500
! 1500 FORMAT(7X,I9,6X,1PE20.13)                                          SOURCE.......17600
 1600 GOTO 1305                                                          SOURCE.......17700
 1700 NIQU = NIQU - 1                                                    SOURCE.......17800
      IF(NIQU.EQ.NSOUI) GOTO 1800                                        SOURCE.......17900
         ERRCOD = 'INP-3,18-1'                                           SOURCE.......18000
         IF (ME.EQ.1) THEN                                               SOURCE.......18100
            CHERR(1) = 'energy'                                          SOURCE.......18200
         ELSE                                                            SOURCE.......18300
            CHERR(1) = 'solute'                                          SOURCE.......18400
         END IF                                                          SOURCE.......18500
         INERR(1) = NIQU                                                 SOURCE.......18600
         INERR(2) = NSOUI                                                SOURCE.......18700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SOURCE.......18800
	   RETURN
 1800 CONTINUE
 !1800 IF(IQSOPT.NE.-1) GOTO 6000                                         SOURCE.......18900
!      IF(ME) 1805,1805,1815                                              SOURCE.......19000
! 1805 WRITE(K3,1806)                                                     SOURCE.......19100
! 1806 FORMAT(//12X,'TIME-DEPENDENT SOLUTE SOURCE/SINK SET IN ',          SOURCE.......19200
!     1   /12X,'SUBROUTINE BCTIME IS INDICATED ',                         SOURCE.......19300
!     2   'BY NEGATIVE NODE NUMBER')                                      SOURCE.......19400
!      GOTO 6000                                                          SOURCE.......19500
! 1815 WRITE(K3,1816)                                                     SOURCE.......19600
! 1816 FORMAT(//12X,'TIME-DEPENDENT ENERGY SOURCE/SINK SET IN ',          SOURCE.......19700
!     1   /12X,'SUBROUTINE BCTIME IS INDICATED ',                         SOURCE.......19800
!     2   'BY NEGATIVE NODE NUMBER')                                      SOURCE.......19900
! 6000 WRITE(K3,952)                                                      SOURCE.......20000
C.....INITIALIZE ARRAY THAT INDICATES WHERE ENERGY OR SOLUTE SOURCE      SOURCE.......20100
C        CONDITIONS WERE SET (0 = INP FILE)                              SOURCE.......20200
      IBCSOU = 0                                                         SOURCE.......20300
C                                                                        SOURCE.......20400
C                                                                        SOURCE.......20500
 9000 RETURN                                                             SOURCE.......20600
C                                                                        SOURCE.......20700
      END                                                                SOURCE.......20800
C                                                                        SOURCE.......20900
C     SUBROUTINE        S  O  U  R  C  E  1        SUTRA VERSION 2.2     SOURCE1........100
C                                                                        SOURCE1........200
C *** PURPOSE :                                                          SOURCE1........300
C ***  TO READ AND ORGANIZE TIME-DEPENDENT FLUID MASS SOURCE DATA AND    SOURCE1........400
C ***  ENERGY OR SOLUTE MASS SOURCE DATA SPECIFIED IN THE OPTIONAL       SOURCE1........500
C ***  BCS INPUT FILE.                                                   SOURCE1........600
C                                                                        SOURCE1........700
C     SUBROUTINE        S  U  T  E  R  R           SUTRA VERSION 2.2     SUTERR.........100
C                                                                        SUTERR.........200
C *** PURPOSE :                                                          SUTERR.........300
C ***  TO HANDLE SUTRA AND FORTRAN ERRORS.                               SUTERR.........400
C                                                                        SUTERR.........500
      SUBROUTINE SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTERR.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTERR.........700
      CHARACTER*80 ERRCOD,CHERR(10),CODE(3),CODUM(3),UNAME,FNAME(0:13)   SUTERR.........800
      CHARACTER*70 DS(50),EX(50)                                         SUTERR.........900
      CHARACTER CDUM80*80                                                SUTERR........1000
      CHARACTER CINERR(10)*9,CRLERR(10)*15                               SUTERR........1100
      CHARACTER SOLNAM(0:10)*40,SOLWRD(0:10)*10                          SUTERR........1200
      CHARACTER*8 VERNUM, VERNIN                                         SUTERR........1300
      DIMENSION INERR(10), RLERR(10)                                     SUTERR........1400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTERR........1500
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            SUTERR........1600
      COMMON /FNAMES/ UNAME,FNAME                                        SUTERR........1700
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 SUTERR........1800
     1   K10,K11,K12,K13                                                 SUTERR........1900
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                SUTERR........2000
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            SUTERR........2100
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       SUTERR........2200
      COMMON /SOLVN/ NSLVRS                                              SUTERR........2300
      COMMON /VER/ VERNUM, VERNIN                                        SUTERR........2400
C                                                                        SUTERR........2500
C RBW
	IERROR = 1
	RETURN
C RBW
C.....PARSE THE ERROR CODE                                               SUTERR........2600
      CALL PRSWDS(ERRCOD, '-', 3, CODE, NWORDS)                          SUTERR........2700
C                                                                        SUTERR........2800
C.....IF AN ERROR OTHER THAN A MATRIX SOLVER OR NONLINEAR CONVERGENCE    SUTERR........2900
C        ERROR HAS OCCURRED, OVERRIDE THE SCREEN OUTPUT CONTROLS SO      SUTERR........3000
C        THAT THE ERROR IS PRINTED TO THE SCREEN AND SUTRA PAUSES FOR    SUTERR........3100
C        A USER RESPONSE.                                                SUTERR........3200
      IF ((CODE(1).NE.'SOL').AND.(CODE(1).NE.'CON')) THEN                SUTERR........3300
         KSCRN = +1                                                      SUTERR........3400
         KPAUSE = +1                                                     SUTERR........3500
      END IF                                                             SUTERR........3600
C                                                                        SUTERR........3700
C.....COPY INTEGER AND REAL ERROR PARAMETERS INTO CHARACTER STRINGS      SUTERR........3800
      DO 150 I=1,10                                                      SUTERR........3900
         WRITE(UNIT=CINERR(I), FMT='(I9)') INERR(I)                      SUTERR........4000
         WRITE(UNIT=CRLERR(I), FMT='(1PE15.7)') RLERR(I)                 SUTERR........4100
  150 CONTINUE                                                           SUTERR........4200
C                                                                        SUTERR........4300
C.....INITIALIZE THE ERROR OUTPUT STRINGS                                SUTERR........4400
      DO 200 I=1,50                                                      SUTERR........4500
         DS(I) = "null_line"                                             SUTERR........4600
         EX(I) = "null_line"                                             SUTERR........4700
  200 CONTINUE                                                           SUTERR........4800
C                                                                        SUTERR........4900
C.....SET THE ERROR OUTPUT STRINGS ACCORDING TO THE TYPE OF ERROR        SUTERR........5000
      IF (ERRCOD.EQ.'INP-2A-1') THEN                                     SUTERR........5100
        DS(1)="The first word of SIMULA is not 'SUTRA'."                 SUTERR........5200
        EX(1)="In dataset 2A of the main input file, the first word"     SUTERR........5300
        EX(2)="of the variable SIMULA must be 'SUTRA'."                  SUTERR........5400
        EX(3)=" "                                                        SUTERR........5500
        EX(4)="Example of a valid dataset 2A:"                           SUTERR........5600
        EX(5)="'SUTRA SOLUTE TRANSPORT'"                                 SUTERR........5700
      ELSE IF (ERRCOD.EQ.'INP-2A-2') THEN                                SUTERR........5800
        DS(1)="The second word of SIMULA is not 'SOLUTE' or 'ENERGY'."   SUTERR........5900
        EX(1)="In dataset 2A of the main input file, when the second"    SUTERR........6000
        EX(2)="word is not 'VERSION', the version 2.0 input format is"   SUTERR........6100
        EX(3)="assumed, and the second word must be 'SOLUTE' or"         SUTERR........6200
        EX(4)="'ENERGY'."                                                SUTERR........6300
        EX(5)=" "                                                        SUTERR........6400
        EX(6)="Example of a valid (version 2.0) dataset 2A:"             SUTERR........6500
        EX(7)="'SUTRA SOLUTE TRANSPORT'"                                 SUTERR........6600
      ELSE IF (ERRCOD.EQ.'INP-2A-3') THEN                                SUTERR........6700
        DS(1)="The fourth word of SIMULA is not 'SOLUTE' or 'ENERGY'."   SUTERR........6800
        EX(1)="In dataset 2A of the main input file, the fourth word"    SUTERR........6900
        EX(2)="must be 'SOLUTE' or 'ENERGY' (unless the version 2.0"     SUTERR........7000
        EX(3)="input format is being used)."                             SUTERR........7100
        EX(4)=" "                                                        SUTERR........7200
        EX(5)="Example of a valid (version 2.0) dataset 2A:"             SUTERR........7300
        EX(6)="'SUTRA VERSION " // TRIM(VERNUM) // " SOLUTE TRANSPORT'"  SUTERR........7400
      ELSE IF (ERRCOD.EQ.'INP-2A-4') THEN                                SUTERR........7500
        DS(1)="Unsupported SUTRA version: " // CHERR(1)                  SUTERR........7600
        EX(1)="Input files from SUTRA version " // TRIM(CHERR(1))        SUTERR........7700
        EX(2)="are not supported by version " // TRIM(VERNUM) // "."     SUTERR........7800
      ELSE IF (ERRCOD.EQ.'INP-2B-1') THEN                                SUTERR........7900
        DS(1)="The first word of MSHSTR is not '2D' or '3D'."            SUTERR........8000
        EX(1)="In dataset 2B of the main input file, the first word"     SUTERR........8100
        EX(2)="of the variable MSHSTR must be '2D' or '3D'."             SUTERR........8200
        EX(3)=" "                                                        SUTERR........8300
        EX(4)="Example of a valid dataset 2B:"                           SUTERR........8400
        EX(5)="'3D REGULAR MESH'  10  20  30"                            SUTERR........8500
C.....ERROR CODE 'INP-2B-2' IS NO LONGER USED.                           SUTERR........8600
      ELSE IF (ERRCOD.EQ.'INP-2B-3') THEN                                SUTERR........8700
        DS(1)="At least one of the rectangular dimensions NN1, NN2,"     SUTERR........8800
        DS(2)="and NN3 is set improperly."                               SUTERR........8900
        EX(1)="In dataset 2B of the main input file, the rectangular"    SUTERR........9000
        EX(2)="dimensions NN1, NN2, and (for 3D problems) NN3 must"      SUTERR........9100
        EX(3)="each be greater than 1."                                  SUTERR........9200
        EX(4)=" "                                                        SUTERR........9300
        EX(5)="Example of a valid dataset 2B:"                           SUTERR........9400
        EX(6)="'3D BLOCKWISE MESH'  10  20  30"                          SUTERR........9500
      ELSE IF (ERRCOD.EQ.'INP-2B-4') THEN                                SUTERR........9600
        DS(1)="The second word of MSHSTR is not 'IRREGULAR', 'REGULAR'," SUTERR........9700
        DS(2)="'BLOCKWISE', or 'LAYERED'."                               SUTERR........9800
        EX(1)="In dataset 2B of the main input file, the second word"    SUTERR........9900
        EX(2)="of the variable MSHSTR must be 'IRREGULAR', 'REGULAR',"   SUTERR.......10000
        EX(3)="'BLOCKWISE' or 'LAYERED'.  By definition, only 3D meshes" SUTERR.......10100
        EX(4)="can be LAYERED."                                          SUTERR.......10200
        EX(5)=" "                                                        SUTERR.......10300
        EX(6)="Example of a valid dataset 2B:"                           SUTERR.......10400
        EX(7)="'3D REGULAR MESH'  10  20  30"                            SUTERR.......10500
      ELSE IF (ERRCOD.EQ.'INP-2B-5') THEN                                SUTERR.......10600
        DS(1)="A 2D LAYERED mesh has been specified."                    SUTERR.......10700
        EX(1)="By definition, only 3D meshes can be LAYERED."            SUTERR.......10800
        EX(2)=" "                                                        SUTERR.......10900
        EX(3)="Example of a valid dataset 2B:"                           SUTERR.......11000
        EX(4)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......11100
      ELSE IF (ERRCOD.EQ.'INP-2B-6') THEN                                SUTERR.......11200
        DS(1)="The first word of LAYSTR is not 'ACROSS' or 'WITHIN'."    SUTERR.......11300
        EX(1)="In dataset 2B of the main input file, the first word"     SUTERR.......11400
        EX(2)="of the variable LAYSTR must be 'ACROSS' or 'WITHIN'."     SUTERR.......11500
        EX(3)=" "                                                        SUTERR.......11600
        EX(4)="Example of a valid dataset 2B:"                           SUTERR.......11700
        EX(5)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......11800
      ELSE IF (ERRCOD.EQ.'INP-2B-7') THEN                                SUTERR.......11900
        DS(1)="At least one of the layer dimensions NLAYS, NNLAY,"       SUTERR.......12000
        DS(2)="and NELAY is set improperly."                             SUTERR.......12100
        EX(1)="In dataset 2B of the main input file, the layer"          SUTERR.......12200
        EX(2)="dimensions are subject to the following constraints:"     SUTERR.......12300
        EX(3)="NLAYS>1, NNLAY>3, and NELAY>0."                           SUTERR.......12400
        EX(4)=" "                                                        SUTERR.......12500
        EX(5)="Example of a valid dataset 2B:"                           SUTERR.......12600
        EX(6)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......12700
      ELSE IF (ERRCOD.EQ.'INP-2B,3-1') THEN                              SUTERR.......12800
        DS(1)="The number of nodes, NN, does not match the rectangular"  SUTERR.......12900
        DS(2)="dimensions, NN1*NN2(*NN3)."                               SUTERR.......13000
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......13100
        EX(2)="number of nodes, NN, must equal the product of the"       SUTERR.......13200
        EX(3)="rectangular dimensions, NN1*NN2 in 2D, or NN1*NN2*NN3"    SUTERR.......13300
        EX(4)="in 3D."                                                   SUTERR.......13400
        EX(5)=" "                                                        SUTERR.......13500
        EX(6)="Example:"                                                 SUTERR.......13600
        EX(7)="If NN1=10, NN2=20, and NN3=30 (dataset 2B), then"         SUTERR.......13700
        EX(8)="NN=10*20*30=6000 (dataset 3)."                            SUTERR.......13800
      ELSE IF (ERRCOD.EQ.'INP-2B,3-2') THEN                              SUTERR.......13900
        DS(1)="The number of elements, NE, does not match the"           SUTERR.......14000
        DS(2)="rectangular dimensions, (NN1-1)*(NN2-1)[*(NN3-1)]."       SUTERR.......14100
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......14200
        EX(2)="number of elements, NE, must equal the product of the"    SUTERR.......14300
        EX(3)="rectangular dimensions, (NN1-1)*(NN2-1) in 2D, or"        SUTERR.......14400
        EX(4)="(NN1-1)*(NN2-1)*(NN3-1) in 3D."                           SUTERR.......14500
        EX(5)=" "                                                        SUTERR.......14600
        EX(6)="Example:"                                                 SUTERR.......14700
        EX(7)="If NN1=10, NN2=20, and NN3=30 (dataset 2B), then"         SUTERR.......14800
        EX(8)="NE=9*19*29=4959 (dataset 3)."                             SUTERR.......14900
      ELSE IF (ERRCOD.EQ.'INP-2B,3-3') THEN                              SUTERR.......15000
        DS(1)="The number of nodes, NN, does not match the layered"      SUTERR.......15100
        DS(2)="dimensions, NLAYS*NNLAY."                                 SUTERR.......15200
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......15300
        EX(2)="number of nodes, NN, must equal the product of the"       SUTERR.......15400
        EX(3)="layered dimensions, NLAYS*NNLAY."                         SUTERR.......15500
        EX(4)=" "                                                        SUTERR.......15600
        EX(5)="Example:"                                                 SUTERR.......15700
        EX(6)="If NLAYS=10 and NNLAY=2560 (dataset 2B), then"            SUTERR.......15800
        EX(7)="NN=10*2560=25600 (dataset 3)."                            SUTERR.......15900
      ELSE IF (ERRCOD.EQ.'INP-2B,3-4') THEN                              SUTERR.......16000
        DS(1)="The number of nodes, NE, does not match the layered"      SUTERR.......16100
        DS(2)="dimensions, (NLAYS-1)*NELAY."                             SUTERR.......16200
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......16300
        EX(2)="number of nodes, NE, must equal the product of the"       SUTERR.......16400
        EX(3)="layered dimensions, (NLAYS-1)*NELAY."                     SUTERR.......16500
        EX(4)=" "                                                        SUTERR.......16600
        EX(5)="Example:"                                                 SUTERR.......16700
        EX(6)="If NLAYS=10 and NELAY=2210 (dataset 2B), then"            SUTERR.......16800
        EX(7)="NN=9*2210=19890 (dataset 3)."                             SUTERR.......16900
      ELSE IF (ERRCOD.EQ.'INP-4-1') THEN                                 SUTERR.......17000
        DS(1)="The first word of CUNSAT is not 'SATURATED' or"           SUTERR.......17100
        DS(2)="'UNSATURATED'."                                           SUTERR.......17200
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......17300
        EX(2)="of the variable CUNSAT must be 'SATURATED' or"            SUTERR.......17400
        EX(3)="'UNSATURATED'."                                           SUTERR.......17500
        EX(4)=" "                                                        SUTERR.......17600
        EX(5)="Example of a valid dataset 4:"                            SUTERR.......17700
        EX(6)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......17800
     1        " 'COLD' 10"                                               SUTERR.......17900
      ELSE IF (ERRCOD.EQ.'INP-4-2') THEN                                 SUTERR.......18000
        DS(1)="The first word of CSSFLO is not 'STEADY' or 'TRANSIENT'." SUTERR.......18100
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......18200
        EX(2)="of the variable CSSFLO must be 'STEADY' or 'TRANSIENT'."  SUTERR.......18300
        EX(3)=" "                                                        SUTERR.......18400
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......18500
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......18600
     1        " 'COLD' 10"                                               SUTERR.......18700
      ELSE IF (ERRCOD.EQ.'INP-4-3') THEN                                 SUTERR.......18800
        DS(1)="The first word of CSSTRA is not 'STEADY' or 'TRANSIENT'." SUTERR.......18900
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......19000
        EX(2)="of the variable CSSTRA must be 'STEADY' or 'TRANSIENT'."  SUTERR.......19100
        EX(3)=" "                                                        SUTERR.......19200
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......19300
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......19400
     1        " 'COLD' 10"                                               SUTERR.......19500
      ELSE IF (ERRCOD.EQ.'INP-4-4') THEN                                 SUTERR.......19600
        DS(1)="The first word of CREAD is not 'COLD' or 'WARM'."         SUTERR.......19700
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......19800
        EX(2)="of the variable CREAD must be 'COLD' or 'WARM'."          SUTERR.......19900
        EX(3)=" "                                                        SUTERR.......20000
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......20100
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......20200
     1        " 'COLD' 10"                                               SUTERR.......20300
      ELSE IF (ERRCOD.EQ.'INP-4-5') THEN                                 SUTERR.......20400
        DS(1)="Specified TRANSIENT flow with STEADY transport."          SUTERR.......20500
        EX(1)="In dataset 4 of the main input file, TRANSIENT flow"      SUTERR.......20600
        EX(2)="requires TRANSIENT transport.  Likewise, STEADY"          SUTERR.......20700
        EX(3)="transport requires STEADY flow.  The following are"       SUTERR.......20800
        EX(4)="valid combinations:"                                      SUTERR.......20900
        EX(5)=" "                                                        SUTERR.......21000
        EX(6)="     CSSFLO      CSSTRA"                                  SUTERR.......21100
        EX(7)="   ----------- -----------"                               SUTERR.......21200
        EX(8)="    'STEADY'    'STEADY'"                                 SUTERR.......21300
        EX(9)="    'STEADY'   'TRANSIENT'"                               SUTERR.......21400
        EX(10)="   'TRANSIENT' 'TRANSIENT'"                              SUTERR.......21500
        EX(11)=" "                                                       SUTERR.......21600
        EX(12)="Example of a valid dataset 4:"                           SUTERR.......21700
        EX(13)="'SATURATED FLOW' 'STEADY FLOW' 'STEADY TRANSPORT'" //    SUTERR.......21800
     1        " 'COLD' 10"                                               SUTERR.......21900
      ELSE IF (ERRCOD.EQ.'INP-7B&C-1') THEN                              SUTERR.......22000
        DS(1)="Unrecognized solver name."                                SUTERR.......22100
        EX(1)="In datasets 7B&C, valid solver selections are:"           SUTERR.......22200
        EX(2)=" "                                                        SUTERR.......22300
        DO 400 M=0,NSLVRS-1                                              SUTERR.......22400
           EX(M+3)=SOLWRD(M) // " --> " // SOLNAM(M)                     SUTERR.......22500
  400   CONTINUE                                                         SUTERR.......22600
        EX(NSLVRS+3)=" "                                                 SUTERR.......22700
        EX(NSLVRS+4)="Note that solver selections for P and U must be"   SUTERR.......22800
        EX(NSLVRS+5)="both DIRECT or both iterative."                    SUTERR.......22900
      ELSE IF (ERRCOD.EQ.'INP-7B&C-2') THEN                              SUTERR.......23000
        DS(1)="Solver selections for P and U are not both DIRECT or"     SUTERR.......23100
        DS(2)="both iterative."                                          SUTERR.......23200
        EX(1)="The solver selections for P and U must be both"           SUTERR.......23300
        EX(2)="DIRECT or both iterative."                                SUTERR.......23400
      ELSE IF (ERRCOD.EQ.'INP-7B&C-3') THEN                              SUTERR.......23500
        DS(1)="Invalid selection of the CG solver."                      SUTERR.......23600
        EX(1)="The CG solver may be used only for the flow (P) equation" SUTERR.......23700
        EX(2)="with no upstream weighting (UP=0.0).  It may not be used" SUTERR.......23800
        EX(3)="for the transport (U) equation."                          SUTERR.......23900
C.....ERROR CODE 'INP-7B&C-4' IS NO LONGER USED.                         SUTERR.......24000
      ELSE IF (ERRCOD.EQ.'INP-3,19-1') THEN                              SUTERR.......24100
        DS(1)="The actual number of specified pressure nodes, "          SUTERR.......24200
     1        // CINERR(1) // ","                                        SUTERR.......24300
        DS(2)="does not equal the input value,                "          SUTERR.......24400
     1        // CINERR(2) // "."                                        SUTERR.......24500
        EX(1)="In dataset 3 of the main input file, the variable NPBC"   SUTERR.......24600
        EX(2)="must specify the exact number of specified pressure"      SUTERR.......24700
        EX(3)="nodes listed in dataset 19."                              SUTERR.......24800
      ELSE IF (ERRCOD.EQ.'INP-3,20-1') THEN                              SUTERR.......24900
        DS(1)="The actual number of specified conc. nodes, "             SUTERR.......25000
     1        // CINERR(1) // ","                                        SUTERR.......25100
        DS(2)="does not equal the input value,             "             SUTERR.......25200
     1        // CINERR(2) // "."                                        SUTERR.......25300
        EX(1)="In dataset 3 of the main input file, the variable NUBC"   SUTERR.......25400
        EX(2)="must specify the exact number of specified concentration" SUTERR.......25500
        EX(3)="nodes listed in dataset 20."                              SUTERR.......25600
      ELSE IF (ERRCOD.EQ.'INP-3,20-2') THEN                              SUTERR.......25700
        DS(1)="The actual number of specified temp. nodes, "             SUTERR.......25800
     1        // CINERR(1) // ","                                        SUTERR.......25900
        DS(2)="does not equal the input value,             "             SUTERR.......26000
     1        // CINERR(2) // "."                                        SUTERR.......26100
        EX(1)="In dataset 3 of the main input file, the variable NUBC"   SUTERR.......26200
        EX(2)="must specify the exact number of specified temperature"   SUTERR.......26300
        EX(3)="nodes listed in dataset 20."                              SUTERR.......26400
      ELSE IF (ERRCOD.EQ.'INP-22-1') THEN                                SUTERR.......26500
        DS(1)="Line 1 of the element incidence data does not begin with" SUTERR.......26600
        DS(2)="the word 'INCIDENCE'."                                    SUTERR.......26700
        EX(1)="In dataset 22 of the main input file, the first line"     SUTERR.......26800
        EX(2)="must begin with the word 'INCIDENCE'."                    SUTERR.......26900
      ELSE IF (ERRCOD.EQ.'INP-22-2') THEN                                SUTERR.......27000
        DS(1)="The incidence data for element " // CINERR(1)             SUTERR.......27100
        DS(2)="are not in numerical order in the dataset."               SUTERR.......27200
        EX(1)="In dataset 22 of the main input file, incidence data"     SUTERR.......27300
        EX(2)="must be listed in order of increasing element number."    SUTERR.......27400
        EX(3)="Note that the numbering of elements must begin at 1"      SUTERR.......27500
        EX(4)="and be continuous; element numbers may not be skipped."   SUTERR.......27600
      ELSE IF (ERRCOD.EQ.'INP-14B,22-1') THEN                            SUTERR.......27700
        DS(1)="At least one element has incorrect geometry."             SUTERR.......27800
        EX(1)="Incorrect element geometry can result from improper"      SUTERR.......27900
        EX(2)="specification of node coordinates in dataset 14B of the"  SUTERR.......28000
        EX(3)="main input file, or from improper ordering of nodes in"   SUTERR.......28100
        EX(4)="a node incidence list in dataset 22 of the same file."    SUTERR.......28200
      ELSE IF (ERRCOD.EQ.'FIL-1') THEN                                   SUTERR.......28300
        DS(1)="The file " // CHERR(2)                                    SUTERR.......28400
        DS(2)="does not exist."                                          SUTERR.......28500
        EX(1)="One of the files required by SUTRA does not exist."       SUTERR.......28600
        EX(2)="Check the filename and the directory path."               SUTERR.......28700
      ELSE IF (ERRCOD.EQ.'FIL-2') THEN                                   SUTERR.......28800
        DS(1)="The file " // CHERR(2)                                    SUTERR.......28900
        DS(2)="could not be opened on FORTRAN unit " // CINERR(1) // "." SUTERR.......29000
        EX(1)="One of the files required by SUTRA could not be opened."  SUTERR.......29100
        EX(2)="Check to make sure the file is not protected or in use"   SUTERR.......29200
        EX(3)="by another application, and that the FORTRAN unit number" SUTERR.......29300
        EX(4)="is valid."                                                SUTERR.......29400
C.....ERROR CODE 'FIL-3' IS NO LONGER USED.                              SUTERR.......29500
      ELSE IF (ERRCOD.EQ.'FIL-4') THEN                                   SUTERR.......29600
        DS(1)="An attempt was made to use the file"                      SUTERR.......29700
        DS(2)=CHERR(2)                                                   SUTERR.......29800
        DS(3)="for more than one purpose simultaneously."                SUTERR.......29900
        EX(1)='Each filename listed in "SUTRA.FIL" must be unique'       SUTERR.......30000
        EX(2)='and may not be reused in an "@INSERT" statement.'         SUTERR.......30100
        EX(3)='Also, if you have nested "@INSERT" statements'            SUTERR.......30200
        EX(4)='(i.e., a file inserted into a file, which is itself'      SUTERR.......30300
        EX(5)='inserted into a file, etc.), a given file may be'         SUTERR.......30400
        EX(6)='used only once in the nested sequence.'                   SUTERR.......30500
      ELSE IF (ERRCOD.EQ.'FIL-5') THEN                                   SUTERR.......30600
        DS(1)="Invalid file type: " // CHERR(2)                          SUTERR.......30700
        EX(1)="Valid file types are:"                                    SUTERR.......30800
        EX(2)='   INP (".inp" input file)'                               SUTERR.......30900
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......31000
        EX(4)='   BCS (".bcs" input file)'                               SUTERR.......31100
        EX(5)='   SMY (".smy" output file)'                              SUTERR.......31200
        EX(6)='   LST (".lst" output file)'                              SUTERR.......31300
        EX(7)='   RST (".rst" output file)'                              SUTERR.......31400
        EX(8)='   NOD (".nod" output file)'                              SUTERR.......31500
        EX(9)='   ELE (".ele" output file)'                              SUTERR.......31600
        EX(10)='   OBS (".obs" output file)'                             SUTERR.......31700
        EX(11)='   OBC (".obc" output file)'                             SUTERR.......31800
        EX(12)='   BCOF (".bcof" output file)'                           SUTERR.......31900
        EX(13)='   BCOP (".bcop" output file)'                           SUTERR.......32000
        EX(14)='   BCOS (".bcos" output file)'                           SUTERR.......32100
        EX(15)='   BCOU (".bcou" output file)'                           SUTERR.......32200
      ELSE IF (ERRCOD.EQ.'FIL-6') THEN                                   SUTERR.......32300
        DS(1)="File type " // CHERR(2)                                   SUTERR.......32400
        DS(2)="has been assigned more than once."                        SUTERR.......32500
        EX(1)="The following file types must be assigned:"               SUTERR.......32600
        EX(2)='   INP (".inp" input file)'                               SUTERR.......32700
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......32800
        EX(4)='   LST (".lst" output file)'                              SUTERR.......32900
        EX(5)="The following file types are optional:"                   SUTERR.......33000
        EX(6)='   BCS (".bcs" input file)'                               SUTERR.......33100
        EX(7)='   SMY (".smy" output file; defaults to "SUTRA.SMY")'     SUTERR.......33200
        EX(8)='   RST (".rst" output file)'                              SUTERR.......33300
        EX(9)='   NOD (".nod" output file)'                              SUTERR.......33400
        EX(10)='   ELE (".ele" output file)'                             SUTERR.......33500
        EX(11)='   OBS (".obs" output file)'                             SUTERR.......33600
        EX(12)='   OBC (".obc" output file)'                             SUTERR.......33700
        EX(13)='   BCOF (".bcof" output file)'                           SUTERR.......33800
        EX(14)='   BCOP (".bcop" output file)'                           SUTERR.......33900
        EX(15)='   BCOS (".bcos" output file)'                           SUTERR.......34000
        EX(16)='   BCOU (".bcou" output file)'                           SUTERR.......34100
        EX(17)="No file type except BCS may be assigned more than once." SUTERR.......34200
      ELSE IF (ERRCOD.EQ.'FIL-7') THEN                                   SUTERR.......34300
        DS(1)="Required file type " // CHERR(2)                          SUTERR.......34400
        DS(2)="has not been assigned."                                   SUTERR.......34500
        EX(1)="The following file types must be assigned:"               SUTERR.......34600
        EX(2)='   INP (".inp" input file)'                               SUTERR.......34700
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......34800
        EX(4)='   LST (".lst" output file)'                              SUTERR.......34900
        EX(5)="The following file types are optional:"                   SUTERR.......35000
        EX(6)='   BCS (".bcs" input file)'                               SUTERR.......35100
        EX(7)='   SMY (".smy" output file; defaults to "SUTRA.SMY")'     SUTERR.......35200
        EX(8)='   RST (".rst" output file)'                              SUTERR.......35300
        EX(9)='   NOD (".nod" output file)'                              SUTERR.......35400
        EX(10)='   ELE (".ele" output file)'                             SUTERR.......35500
        EX(11)='   OBS (".obs" output file)'                             SUTERR.......35600
        EX(12)='   OBC (".obc" output file)'                             SUTERR.......35700
        EX(13)='   BCOF (".bcof" output file)'                           SUTERR.......35800
        EX(14)='   BCOP (".bcop" output file)'                           SUTERR.......35900
        EX(15)='   BCOS (".bcos" output file)'                           SUTERR.......36000
        EX(16)='   BCOU (".bcou" output file)'                           SUTERR.......36100
        EX(17)="No file type except BCS may be assigned more than once." SUTERR.......36200
      ELSE IF (ERRCOD.EQ.'FIL-8') THEN                                   SUTERR.......36300
        DS(1)="The file " // CHERR(2)                                    SUTERR.......36400
        DS(2)="could not be inserted."                                   SUTERR.......36500
        EX(1)="Inserts cannot be nested more than 20 levels deep."       SUTERR.......36600
      ELSE IF (ERRCOD.EQ.'FIL-9') THEN                                   SUTERR.......36700
        DS(1)="A file listed in 'SUTRA.FIL' is named 'SUTRA.FIL'."       SUTERR.......36800
        EX(1)="The filename 'SUTRA.FIL' is reserved by SUTRA."           SUTERR.......36900
        EX(2)="Files listed in 'SUTRA.FIL' may not be named"             SUTERR.......37000
        EX(3)="'SUTRA.FIL'."                                             SUTERR.......37100
      ELSE IF (ERRCOD.EQ.'FIL-10') THEN                                  SUTERR.......37200
        DS(1)="SUTRA was unable to automatically"                        SUTERR.......37300
        DS(2)="assign unit number " // CINERR(1)                         SUTERR.......37400
        DS(3)="to file " // CHERR(2)                                     SUTERR.......37500
        EX(1)="SUTRA attempted to automatically assign to one of the "   SUTERR.......37600
        EX(2)="files listed in 'SUTRA.FIL' a unit number that is not"    SUTERR.......37700
        EX(3)="allowed on this computer.  Please check the unit"         SUTERR.......37800
        EX(4)="number assignments in 'SUTRA.FIL'.  It may be possible"   SUTERR.......37900
        EX(5)="to avoid this problem by explicitly assigning a"          SUTERR.......38000
        EX(6)="different unit number to the file in question or by"      SUTERR.......38100
        EX(7)="reducing the number of optional files listed in"          SUTERR.......38200
        EX(8)="'SUTRA.FIL'."                                             SUTERR.......38300
      ELSE IF (ERRCOD.EQ.'INP-6-1') THEN                                 SUTERR.......38400
        DS(1)="NPCYC<1 and/or NUCYC<1."                                  SUTERR.......38500
        EX(1)="In dataset 6 of the main input file, both NPCYC and"      SUTERR.......38600
        EX(2)="NUCYC must be set greater than or equal to 1."            SUTERR.......38700
      ELSE IF (ERRCOD.EQ.'INP-6-2') THEN                                 SUTERR.......38800
        DS(1)="Neither NPCYC nor NUCYC is set to 1."                     SUTERR.......38900
        EX(1)="In dataset 6 of the main input file, either NPCYC or"     SUTERR.......39000
        EX(2)="NUCYC (or both) must be set to 1."                        SUTERR.......39100
      ELSE IF (ERRCOD.EQ.'INP-6-3') THEN                                 SUTERR.......39200
        DS(1)="DELT is greater than DTMAX."                              SUTERR.......39300
        EX(1)="In dataset 6 of the main input file, DELT must be set"    SUTERR.......39400
        EX(2)="less than or equal to DTMAX."                             SUTERR.......39500
      ELSE IF (ERRCOD.EQ.'INP-6-4') THEN                                 SUTERR.......39600
        DS(1)="The actual number of schedules listed does not equal"     SUTERR.......39700
        DS(2)="the input value, or the schedule list does not end"       SUTERR.......39800
        DS(3)="with '-'."                                                SUTERR.......39900
        EX(1)="In dataset 6 of the main input file, the number of"       SUTERR.......40000
        EX(2)="schedules listed must equal the number, NSCH, specified"  SUTERR.......40100
        EX(3)="in dataset 6 of the same file, and the final entry in"    SUTERR.......40200
        EX(4)="the list must be '-'."                                    SUTERR.......40300
        EX(5)=" "                                                        SUTERR.......40400
        EX(6)="Example of a valid dataset 6 with two schedules:"         SUTERR.......40500
        EX(7)="2   1   1"                                                SUTERR.......40600
        EX(8)="'TIME_STEPS' 'TIME CYCLE' 'ELAPSED'  1. 100   0. " //     SUTERR.......40700
     1     "3.e+9 3.e+7 999 1. 0. 1.e+99"                                SUTERR.......40800
        EX(9)="'SCHED_A'    'STEP LIST'        4   20  40  60  80"       SUTERR.......40900
        EX(10)="'-'"                                                     SUTERR.......41000
      ELSE IF (ERRCOD.EQ.'INP-6-5') THEN                                 SUTERR.......41100
        DS(1)="Multiple definitions of schedule " // CHERR(1)            SUTERR.......41200
        EX(1)="A given schedule name may not be defined more than once"  SUTERR.......41300
        EX(2)="in dataset 6 of the main input file."                     SUTERR.......41400
      ELSE IF (ERRCOD.EQ.'INP-6-6') THEN                                 SUTERR.......41500
        DS(1)="Invalid time descriptor " // CHERR(1)                     SUTERR.......41600
        EX(1)="Time-based schedules must be defined in terms of either"  SUTERR.......41700
        EX(2)="ABSOLUTE or ELAPSED times."                               SUTERR.......41800
      ELSE IF (ERRCOD.EQ.'INP-6-7') THEN                                 SUTERR.......41900
        DS(1)="ELAPSED times in TIME_STEPS schedule,"                    SUTERR.......42000
        DS(2)="but initial elapsed time is not zero."                    SUTERR.......42100
        EX(1)="When the TIME_STEPS schedule is defined in terms of"      SUTERR.......42200
        EX(2)="ELAPSED times, the first (initial) elapsed time in the"   SUTERR.......42300
        EX(3)="schedule must be set to zero."                            SUTERR.......42400
      ELSE IF (ERRCOD.EQ.'INP-6-8') THEN                                 SUTERR.......42500
        DS(1)="Invalid number of schedules (NSCH<0)."                    SUTERR.......42600
        EX(1)="The number of schedules, NSCH, must be non-negative."     SUTERR.......42700
        EX(2)="NSCH=0 is allowed only if flow and transport are both"    SUTERR.......42800
        EX(3)="steady-state."                                            SUTERR.......42900
      ELSE IF (ERRCOD.EQ.'INP-6-9') THEN                                 SUTERR.......43000
        DS(1)="Invalid schedule type " // CHERR(1)                       SUTERR.......43100
        EX(1)="An invalid schedule type has been specified."             SUTERR.......43200
        EX(2)="Valid schedule types are:"                                SUTERR.......43300
        EX(3)="   'TIME CYCLE'"                                          SUTERR.......43400
        EX(4)="   'TIME LIST'"                                           SUTERR.......43500
        EX(5)="   'STEP CYCLE'"                                          SUTERR.......43600
        EX(6)="   'STEP LIST'"                                           SUTERR.......43700
      ELSE IF (ERRCOD.EQ.'INP-6-10') THEN                                SUTERR.......43800
        DS(1)="Incomplete TIME_STEPS schedule."                          SUTERR.......43900
        EX(1)="The TIME_STEPS schedule must contain at least two"        SUTERR.......44000
        EX(2)="distinct times, including the starting time."             SUTERR.......44100
      ELSE IF (ERRCOD.EQ.'INP-6-11') THEN                                SUTERR.......44200
        DS(1)="Invalid user-defined schedule name, " // TRIM(CHERR(1))   SUTERR.......44300
        EX(1)="Schedule names 'STEP_0', 'STEP_1', and 'STEPS_1&UP'"      SUTERR.......44400
        EX(2)="are reserved by SUTRA and may not be used to name a"      SUTERR.......44500
        EX(3)="user-defined schedule."                                   SUTERR.......44600
      ELSE IF (ERRCOD.EQ.'INP-6-12') THEN                                SUTERR.......44700
        DS(1)="Repeated " // TRIM(CHERR(1)) // " " // CRLERR(1)          SUTERR.......44800
        DS(2)="in schedule " // CHERR(2)                                 SUTERR.......44900
        EX(1)="A time or time step value may not appear more than once"  SUTERR.......45000
        EX(2)="in a given schedule."                                     SUTERR.......45100
      ELSE IF (ERRCOD.EQ.'INP-6-13') THEN                                SUTERR.......45200
        DS(1)="Invalid number of schedules (NSCH=0)."                    SUTERR.......45300
        EX(1)="NSCH=0 is allowed only if flow and transport are both"    SUTERR.......45400
        EX(2)="steady-state."                                            SUTERR.......45500
      ELSE IF (ERRCOD.EQ.'INP-6-14') THEN                                SUTERR.......45600
        DS(1)="Missing TIME_STEPS schedule."                             SUTERR.......45700
        EX(1)="When transport is transient, a TIME_STEPS schedule must"  SUTERR.......45800
        EX(2)="be defined by the user in dataset 6."                     SUTERR.......45900
      ELSE IF (ERRCOD.EQ.'INP-6-15') THEN                                SUTERR.......46000
        DS(1)="Schedule name " // CHERR(1)                               SUTERR.......46100
        DS(2)="is too long"                                              SUTERR.......46200
        EX(1)="Schedule names are limited to 10 characters."             SUTERR.......46300
      ELSE IF ((ERRCOD.EQ.'INP-8A-1').OR.(ERRCOD.EQ.'INP-8A-2')          SUTERR.......46400
     1     .OR.(ERRCOD.EQ.'INP-8A-3').OR.(ERRCOD.EQ.'INP-8A-4')          SUTERR.......46500
     2     .OR.(ERRCOD.EQ.'INP-8A-5').OR.(ERRCOD.EQ.'INP-8A-6')          SUTERR.......46600
     3     .OR.(ERRCOD.EQ.'INP-8A-7').OR.(ERRCOD.EQ.'INP-8A-8')          SUTERR.......46700
     4     .OR.(ERRCOD.EQ.'INP-8A-9')) THEN                              SUTERR.......46800
        DS(1)=CHERR(1)(1:6) // " is not 'Y' or 'N'."                     SUTERR.......46900
        EX(1)="In dataset 8A of the main input file, " // CHERR(1)(1:6)  SUTERR.......47000
        EX(2)="must be set to either 'Y' or 'N'."                        SUTERR.......47100
        EX(3)=" "                                                        SUTERR.......47200
        EX(4)="Example of a valid dataset 8A:"                           SUTERR.......47300
        EX(5)="10  'N' 'N' 'N' 'Y' 'Y' 'Y' 'Y' 'Y' 'Y' 'Y' 'Y'"          SUTERR.......47400
      ELSE IF (ERRCOD.EQ.'INP-8B-1') THEN                                SUTERR.......47500
        DS(1)="Node number listed in column other than column 1."        SUTERR.......47600
        EX(1)="In dataset 8B of the main input file, if the node number" SUTERR.......47700
        EX(2)="is to appear, it must appear only in column 1, i.e.,"     SUTERR.......47800
        EX(3)="only NCOL(1) can be set to 'N'."                          SUTERR.......47900
      ELSE IF (ERRCOD.EQ.'INP-8B-2') THEN                                SUTERR.......48000
        DS(1)="Specified that 'Z' be output for a 2D problem."           SUTERR.......48100
        EX(1)="In dataset 8B of the main input file, 'Z' can be listed"  SUTERR.......48200
        EX(2)="only if the problem is 3D."                               SUTERR.......48300
      ELSE IF (ERRCOD.EQ.'INP-8B-3') THEN                                SUTERR.......48400
        DS(1)="Unrecognized value for NCOL."                             SUTERR.......48500
        EX(1)="In dataset 8B of the main input file, the following"      SUTERR.......48600
        EX(2)="variables may be listed:"                                 SUTERR.......48700
        EX(3)=" "                                                        SUTERR.......48800
        EX(4)="'N'  =  node number (if used, it must appear first)"      SUTERR.......48900
        EX(5)="'X'  =  X-coordinate of node"                             SUTERR.......49000
        EX(6)="'Y'  =  Y-coordinate of node"                             SUTERR.......49100
        EX(7)="'Z'  =  Z-coordinate of node (3D only)"                   SUTERR.......49200
        EX(8)="'P'  =  pressure"                                         SUTERR.......49300
        EX(9)="'U'  =  concentration or temperature"                     SUTERR.......49400
        EX(10)="'S'  =  saturation"                                      SUTERR.......49500
        EX(11)=" "                                                       SUTERR.......49600
        EX(12)="The symbol '-' (a single dash) is used to end the list." SUTERR.......49700
        EX(13)="Any symbols following '-' are ignored."                  SUTERR.......49800
        EX(14)=" "                                                       SUTERR.......49900
        EX(15)="Example of a valid dataset 8B for a 3D problem:"         SUTERR.......50000
        EX(16)="10  'N'  'X'  'Y'  'Z'  'S'  'U'  '-'"                   SUTERR.......50100
      ELSE IF (ERRCOD.EQ.'INP-8C-1') THEN                                SUTERR.......50200
        DS(1)="Element number listed in column other than column 1."     SUTERR.......50300
        EX(1)="In dataset 8C of the main input file, if the element"     SUTERR.......50400
        EX(2)="number is to appear, it must appear only in column 1,"    SUTERR.......50500
        EX(3)="i.e., only LCOL(1) can be set to 'E'."                    SUTERR.......50600
      ELSE IF (ERRCOD.EQ.'INP-8C-2') THEN                                SUTERR.......50700
        DS(1)="Specified that 'Z' be output for a 2D problem."           SUTERR.......50800
        EX(1)="In dataset 8C of the main input file, 'Z' can be listed"  SUTERR.......50900
        EX(2)="only if the problem is 3D."                               SUTERR.......51000
      ELSE IF (ERRCOD.EQ.'INP-8C-3') THEN                                SUTERR.......51100
        DS(1)="Unrecognized value for LCOL."                             SUTERR.......51200
        EX(1)="In dataset 8C of the main input file, the following"      SUTERR.......51300
        EX(2)="variables may be listed:"                                 SUTERR.......51400
        EX(3)=" "                                                        SUTERR.......51500
        EX(4)="'E'  =  element number (if used, it must appear first)"   SUTERR.......51600
        EX(5)="'X'  =  X-coordinate of element centroid"                 SUTERR.......51700
        EX(6)="'Y'  =  Y-coordinate of element centroid"                 SUTERR.......51800
        EX(7)="'Z'  =  Z-coordinate of element centroid (3D only)"       SUTERR.......51900
        EX(8)="'VX'  =  X-component of fluid velocity"                   SUTERR.......52000
        EX(9)="'VY'  =  Y-component of fluid velocity"                   SUTERR.......52100
        EX(10)="'VZ'  =  Z-component of fluid velocity (3D only)"        SUTERR.......52200
        EX(11)=" "                                                       SUTERR.......52300
        EX(12)="The symbol '-' (a single dash) is used to end the list." SUTERR.......52400
        EX(13)="Any symbols following '-' are ignored."                  SUTERR.......52500
        EX(14)=" "                                                       SUTERR.......52600
        EX(15)="Example of a valid dataset 8B for a 3D problem:"         SUTERR.......52700
        EX(16)="10  'E'  'X'  'Y'  'Z'  'VX'  'VY'  'VZ'  '-'"           SUTERR.......52800
      ELSE IF (ERRCOD.EQ.'INP-8C-4') THEN                                SUTERR.......52900
        DS(1)="Specified that 'VZ' be output for a 2D problem."          SUTERR.......53000
        EX(1)="In dataset 8C of the main input file, 'VZ' can be listed" SUTERR.......53100
        EX(2)="only if the problem is 3D."                               SUTERR.......53200
      ELSE IF (ERRCOD.EQ.'INP-8D-1') THEN                                SUTERR.......53300
        DS(1)="The actual number of observation points listed does not"  SUTERR.......53400
        DS(2)="equal the input value, or the observation point list"     SUTERR.......53500
        DS(3)="does not end with a zero."                                SUTERR.......53600
        EX(1)="In dataset 8D of the main input file, the number of"      SUTERR.......53700
        EX(2)="points listed must equal the number, NOBS, specified in"  SUTERR.......53800
        EX(3)="dataset 3 of the same file, and a zero must appear after" SUTERR.......53900
        EX(4)="the last point in the list when the old format is used."  SUTERR.......54000
        EX(5)="Any information appearing after the zero is ignored."     SUTERR.......54100
        EX(6)=" "                                                        SUTERR.......54200
        EX(7)="Example of a valid old-format dataset 8D with three"      SUTERR.......54300
        EX(8)="observation points (nodes 45, 46, and 7347),"             SUTERR.......54400
        EX(9)="assuming NN>=7347:"                                       SUTERR.......54500
        EX(10)="10   45   46   7347   0"                                 SUTERR.......54600
      ELSE IF (ERRCOD.EQ.'INP-8D-2') THEN                                SUTERR.......54700
        DS(1)="The observation node list contains an invalid node"       SUTERR.......54800
        DS(2)="number."                                                  SUTERR.......54900
        EX(1)="In dataset 8D of the main input file, all node numbers"   SUTERR.......55000
        EX(2)="must be greater than or equal to 1, and less than or"     SUTERR.......55100
        EX(3)="equal to NN, the total number of nodes.  The last entry"  SUTERR.......55200
        EX(4)="must be a zero, which signals the end of the list."       SUTERR.......55300
        EX(5)=" "                                                        SUTERR.......55400
        EX(6)="Example of a valid old-format dataset 8D with three"      SUTERR.......55500
        EX(7)="observation nodes (45, 46, and 7347),"                    SUTERR.......55600
        EX(8)="assuming NN>=7347:"                                       SUTERR.......55700
        EX(9)="10   45   46   7347   0"                                  SUTERR.......55800
      ELSE IF (ERRCOD.EQ.'INP-8D-3') THEN                                SUTERR.......55900
        DS(1)="Element not found for the following observation point:"   SUTERR.......56000
        DS(2)="   " // CHERR(1)                                          SUTERR.......56100
        DS(3)="   " // CHERR(2)                                          SUTERR.......56200
        EX(1)="SUTRA was unable to find an element that contains"        SUTERR.......56300
        EX(2)="the observation point named above.  Please check"         SUTERR.......56400
        EX(3)="to make sure the coordinates specified for that"          SUTERR.......56500
        EX(4)="observation point are within the model domain."           SUTERR.......56600
      ELSE IF (ERRCOD.EQ.'INP-8D-4') THEN                                SUTERR.......56700
        DS(1)="The actual number of observation points listed does not"  SUTERR.......56800
        DS(2)="equal the input value, or the observation point list"     SUTERR.......56900
        DS(3)="does not end with '-'."                                   SUTERR.......57000
        EX(1)="In dataset 8D of the main input file, the number of"      SUTERR.......57100
        EX(2)="points listed must equal the number, NOBS, specified in"  SUTERR.......57200
        EX(3)="dataset 3 of the same file, and the final entry in the"   SUTERR.......57300
        EX(4)="list must be '-'."                                        SUTERR.......57400
        EX(5)=" "                                                        SUTERR.......57500
        EX(6)="Example of a valid dataset 8D with two 3D observation"    SUTERR.......57600
        EX(7)="points, assuming schedules A and B have been defined:"    SUTERR.......57700
        EX(8)="100"                                                      SUTERR.......57800
        EX(9)="'POINT_1'     0.   100.   500.   'A'   'OBS'"             SUTERR.......57900
        EX(10)="'POINT_2'   100.   200.   800.   'B'   'OBC'"            SUTERR.......58000
        EX(11)="'-'"                                                     SUTERR.......58100
      ELSE IF (ERRCOD.EQ.'INP-8D-5') THEN                                SUTERR.......58200
        DS(1)="Undefined schedule " // CHERR(1)                          SUTERR.......58300
        DS(2)="specified for observation " // CHERR(2)                   SUTERR.......58400
        EX(1)="The output schedule specified for one of the"             SUTERR.......58500
        EX(2)="observation points has not been defined in dataset 6"     SUTERR.......58600
        EX(3)="of the main input file."                                  SUTERR.......58700
      ELSE IF (ERRCOD.EQ.'INP-8D-6') THEN                                SUTERR.......58800
        DS(1)="Schedule name " // CHERR(1)                               SUTERR.......58900
        DS(2)="is too long"                                              SUTERR.......59000
        EX(1)="Schedule names are limited to 10 characters."             SUTERR.......59100
      ELSE IF (ERRCOD.EQ.'INP-8E-1') THEN                                SUTERR.......59200
        DS(1)=CHERR(1)(1:6) // " is not 'Y' or 'N'."                     SUTERR.......59300
        EX(1)="In dataset 8E of the main input file, " // CHERR(1)(1:6)  SUTERR.......59400
        EX(2)="must be set to either 'Y' or 'N'."                        SUTERR.......59500
        EX(3)=" "                                                        SUTERR.......59600
        EX(4)="Example of a valid dataset 8E:"                           SUTERR.......59700
        EX(5)="1   1    1    1   'Y'"                                    SUTERR.......59800
      ELSE IF (ERRCOD.EQ.'INP-11-1') THEN                                SUTERR.......59900
        DS(1)="Unrecognized sorption model."                             SUTERR.......60000
        EX(1)="In dataset 11 of the main input file, the sorption model" SUTERR.......60100
        EX(2)="may be chosen from the following:"                        SUTERR.......60200
        EX(3)=" "                                                        SUTERR.......60300
        EX(4)="'NONE'       =  No sorption"                              SUTERR.......60400
        EX(5)="'LINEAR'     =  Linear sorption model"                    SUTERR.......60500
        EX(6)="'FREUNDLICH' =  Freundlich sorption model"                SUTERR.......60600
        EX(7)="'LANGMUIR'   =  Langmuir sorption model"                  SUTERR.......60700
      ELSE IF (ERRCOD.EQ.'INP-11-2') THEN                                SUTERR.......60800
        DS(1)="The second Freundlich sorption coefficient is less than"  SUTERR.......60900
        DS(2)="or equal to zero."                                        SUTERR.......61000
        EX(1)="In dataset 11 of the main input file, the second"         SUTERR.......61100
        EX(2)="coefficient, CHI2, must be positive if Freundlich"        SUTERR.......61200
        EX(3)="sorption is chosen."                                      SUTERR.......61300
      ELSE IF (ERRCOD.EQ.'INP-14A-1') THEN                               SUTERR.......61400
        DS(1)="Dataset 14A does not begin with the word 'NODE'."         SUTERR.......61500
        EX(1)="Dataset 14A of the main input file must begin with the"   SUTERR.......61600
        EX(2)="word 'NODE'."                                             SUTERR.......61700
        EX(3)=" "                                                        SUTERR.......61800
        EX(4)="Example of a valid dataset 14A:"                          SUTERR.......61900
        EX(5)="'NODE'  1000.  1000.  1.  0.1"                            SUTERR.......62000
      ELSE IF (ERRCOD.EQ.'INP-15A-1') THEN                               SUTERR.......62100
        DS(1)="Dataset 15A does not begin with the word 'ELEMENT'."      SUTERR.......62200
        EX(1)="Dataset 15A of the main input file must begin with the"   SUTERR.......62300
        EX(2)="word 'ELEMENT'."                                          SUTERR.......62400
        EX(3)=" "                                                        SUTERR.......62500
        EX(4)="Example of a valid dataset 15A for a " // CHERR(1)(1:2)   SUTERR.......62600
     1         // " problem:"                                            SUTERR.......62700
        IF (CHERR(1).EQ."3D") THEN                                       SUTERR.......62800
          EX(5)="'ELEMENT' 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1." SUTERR.......62900
        ELSE                                                             SUTERR.......63000
          EX(5)="'ELEMENT' 1. 1. 1. 1. 1. 1. 1."                         SUTERR.......63100
        END IF                                                           SUTERR.......63200
      ELSE IF (ERRCOD.EQ.'ICS-1-1') THEN                                 SUTERR.......63300
        DS(1)="Unable to restart simulation."                            SUTERR.......63400
        EX(1)="The time step number from which the simulation is trying" SUTERR.......63500
        EX(2)="to restart (" // CINERR(1) // ") equals or exceeds the"   SUTERR.......63600
        EX(3)="number of the final time step (" // CINERR(2) // ")."     SUTERR.......63700
      ELSE IF (ERRCOD.EQ.'ICS-2-1') THEN                                 SUTERR.......63800
        DS(1)="Unrecognized initialization type."                        SUTERR.......63900
        EX(1)="In dataset 2 of the initial conditions input file,"       SUTERR.......64000
        EX(2)="the valid types of initializations for P are UNIFORM"     SUTERR.......64100
        EX(3)="and NONUNIFORM."                                          SUTERR.......64200
      ELSE IF (ERRCOD.EQ.'ICS-2-2') THEN                                 SUTERR.......64300
        DS(1)="Did not specify NONUNIFORM initial values during a WARM"  SUTERR.......64400
        DS(2)="start."                                                   SUTERR.......64500
        EX(1)="In dataset 2 of the initial conditions input file,"       SUTERR.......64600
        EX(2)="initial values for P must be specified as NONUNIFORM"     SUTERR.......64700
        EX(3)="during a WARM start (i.e., if CREAD='WARM' in dataset 4"  SUTERR.......64800
        EX(4)="of the main input file)."                                 SUTERR.......64900
      ELSE IF (ERRCOD.EQ.'ICS-3-1') THEN                                 SUTERR.......65000
        DS(1)="Unrecognized initialization type."                        SUTERR.......65100
        EX(1)="In dataset 3 of the initial conditions input file,"       SUTERR.......65200
        EX(2)="the valid types of initializations for U are UNIFORM"     SUTERR.......65300
        EX(3)="and NONUNIFORM."                                          SUTERR.......65400
      ELSE IF (ERRCOD.EQ.'ICS-3-2') THEN                                 SUTERR.......65500
        DS(1)="Did not specify NONUNIFORM initial values during a WARM"  SUTERR.......65600
        DS(2)="start."                                                   SUTERR.......65700
        EX(1)="In dataset 3 of the initial conditions input file,"       SUTERR.......65800
        EX(2)="initial values for U must be specified as NONUNIFORM"     SUTERR.......65900
        EX(3)="during a WARM start (i.e., if CREAD='WARM' in dataset 4"  SUTERR.......66000
        EX(4)="of the main input file)."                                 SUTERR.......66100
      ELSE IF (ERRCOD.EQ.'SOL-1') THEN                                   SUTERR.......66200
        DS(1)="Error returned by the " // CHERR(2)(1:10)                 SUTERR.......66300
        DS(2)="solver while solving for " // CHERR(1)(1:1) // "."        SUTERR.......66400
        EX(1)="The iterative solver has stopped because of an error."    SUTERR.......66500
        EX(2)="Error flag values are interpreted as follows:"            SUTERR.......66600
        EX(3)="  "                                                       SUTERR.......66700
        EX(4)="IERR = 2  =>  Method stalled or failed to converge in"    SUTERR.......66800
        EX(5)="              the maximum number of iterations allowed."  SUTERR.......66900
        EX(6)="IERR = 4  =>  Convergence tolerance set too tight for"    SUTERR.......67000
        EX(7)="              machine precision."                         SUTERR.......67100
        EX(8)="IERR = 5  =>  Method broke down because preconditioning"  SUTERR.......67200
        EX(9)="              matrix is non-positive-definite."           SUTERR.......67300
        EX(10)="IERR = 6  =>  Method broke down because matrix is non-"  SUTERR.......67400
        EX(11)="              positive-definite or nearly so."           SUTERR.......67500
        EX(12)=" "                                                       SUTERR.......67600
        EX(13)="If the P-solution resulted in a solver error, an"        SUTERR.......67700
        EX(14)="attempt was still made to obtain a U-solution."          SUTERR.......67800
        EX(15)="The last P and U solutions were written to the"          SUTERR.......67900
        EX(16)="appropriate output files (except the restart file)"      SUTERR.......68000
        EX(17)="whether or not they resulted in solver errors."          SUTERR.......68100
      ELSE IF (ERRCOD.EQ.'INP-3,17-1') THEN                              SUTERR.......68200
        DS(1)="The actual number of"                                     SUTERR.......68300
        DS(2)="specified fluid source nodes,   " // CINERR(1) // ","     SUTERR.......68400
        DS(3)="does not equal the input value, " // CINERR(2) // "."     SUTERR.......68500
        EX(1)="In dataset 3 of the main input file, the variable NSOP"   SUTERR.......68600
        EX(2)="must specify the exact number of specified fluid source"  SUTERR.......68700
        EX(3)="nodes listed in dataset 17."                              SUTERR.......68800
      ELSE IF (ERRCOD.EQ.'INP-3,18-1') THEN                              SUTERR.......68900
        DS(1)="The actual number of"                                     SUTERR.......69000
        DS(2)="specified " // CHERR(1)(1:6) // " source nodes,  "        SUTERR.......69100
     1         // CINERR(1) // ","                                       SUTERR.......69200
        DS(3)="does not equal the input value, " // CINERR(2) // "."     SUTERR.......69300
        EX(1)="In dataset 3 of the main input file, the variable NSOU"   SUTERR.......69400
        EX(2)="must specify the exact number of specified "              SUTERR.......69500
     1         // CHERR(1)(1:6) // " source"                             SUTERR.......69600
        EX(3)="nodes listed in dataset 18."                              SUTERR.......69700
      ELSE IF (ERRCOD.EQ.'INP-17-1') THEN                                SUTERR.......69800
        DS(1)="Invalid node number referenced in dataset 17: "           SUTERR.......69900
     1         // CINERR(1)                                              SUTERR.......70000
        EX(1)="Dataset 17 of the main input file contains a reference"   SUTERR.......70100
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......70200
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......70300
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......70400
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......70500
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......70600
      ELSE IF (ERRCOD.EQ.'INP-18-1') THEN                                SUTERR.......70700
        DS(1)="Invalid node number referenced in dataset 18: "           SUTERR.......70800
     1         // CINERR(1)                                              SUTERR.......70900
        EX(1)="Dataset 18 of the main input file contains a reference"   SUTERR.......71000
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......71100
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......71200
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......71300
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......71400
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......71500
      ELSE IF (ERRCOD.EQ.'INP-19-1') THEN                                SUTERR.......71600
        DS(1)="Invalid node number referenced in dataset 19: "           SUTERR.......71700
     1         // CINERR(1)                                              SUTERR.......71800
        EX(1)="Dataset 19 of the main input file contains a reference"   SUTERR.......71900
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......72000
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......72100
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......72200
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......72300
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......72400
      ELSE IF (ERRCOD.EQ.'INP-20-1') THEN                                SUTERR.......72500
        DS(1)="Invalid node number referenced in dataset 20: "           SUTERR.......72600
     1         // CINERR(1)                                              SUTERR.......72700
        EX(1)="Dataset 20 of the main input file contains a reference"   SUTERR.......72800
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......72900
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......73000
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......73100
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......73200
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......73300
      ELSE IF (ERRCOD.EQ.'BCS-1-1') THEN                                 SUTERR.......73400
        DS(1)="Undefined schedule " // CHERR(1)                          SUTERR.......73500
        DS(2)="specified in BCS file " // CHERR(2)                       SUTERR.......73600
        EX(1)="The schedule specified in the BCS file mentioned"         SUTERR.......73700
        EX(2)="above has not been defined in dataset 6 of the"           SUTERR.......73800
        EX(3)="main input file."                                         SUTERR.......73900
      ELSE IF (ERRCOD.EQ.'BCS-1-2') THEN                                 SUTERR.......74000
        DS(1)="Schedule " // CHERR(1)                                    SUTERR.......74100
        DS(2)="used in BCS file " // CHERR(2)                            SUTERR.......74200
        DS(3)="includes a fractional time step number, " // CRLERR(1)    SUTERR.......74300
        EX(1)="Schedules that are used in BCS files must not include"    SUTERR.......74400
        EX(2)="fractional time step numbers.  Only whole-numbered"       SUTERR.......74500
        EX(3)="time steps (0, 1, 2, 3, etc.) are allowed."               SUTERR.......74600
      ELSE IF (ERRCOD.EQ.'BCS-1-3') THEN                                 SUTERR.......74700
        DS(1)="Undefined schedule " // CHERR(1)                          SUTERR.......74800
        DS(2)="specified in BCS file " // CHERR(2)                       SUTERR.......74900
        EX(1)="When transport is steady-state, user-defined"             SUTERR.......75000
        EX(2)="schedules are not in effect, and steady-state boundary"   SUTERR.......75100
        EX(3)="conditions in BCS files must be controlled using the"     SUTERR.......75200
        EX(4)="following schedules automatically defined by SUTRA:"      SUTERR.......75300
        EX(5)=" "                                                        SUTERR.......75400
        EX(6)="TIME_STEPS (time steps 0 and 1)"                          SUTERR.......75500
        EX(7)="STEP_0     (time step 0 only)"                            SUTERR.......75600
        EX(8)="STEP_1     (time step 1 only)"                            SUTERR.......75700
        EX(9)="STEPS_1&UP (equivalent to 'STEP_1' in this case)"         SUTERR.......75800
      ELSE IF (ERRCOD.EQ.'BCS-1-4') THEN                                 SUTERR.......75900
        DS(1)="Schedule name " // CHERR(1)                               SUTERR.......76000
        DS(2)="specified in BCS file " // CHERR(2)                       SUTERR.......76100
        DS(3)="is too long"                                              SUTERR.......76200
        EX(1)="Schedule names are limited to 10 characters."             SUTERR.......76300
      ELSE IF (ERRCOD.EQ.'BCS-2-1') THEN                                 SUTERR.......76400
        DS(1)="Transport boundary conditions"                            SUTERR.......76500
        DS(2)="are specified for time step 0"                            SUTERR.......76600
        DS(3)="in the BCS file mentioned above."                         SUTERR.......76700
        EX(1)="Boundary conditions for transport (BCS datasets 4 and 6)" SUTERR.......76800
        EX(2)="cannot be specified for time step 0.  If transport is"    SUTERR.......76900
        EX(3)="steady-state, use time step 1 (not 0) to specify"         SUTERR.......77000
        EX(4)="boundary conditions for transport in BCS files."          SUTERR.......77100
      ELSE IF (ERRCOD.EQ.'BCS-2,3-1') THEN                               SUTERR.......77200
        DS(1)="The actual number of time-"                               SUTERR.......77300
        DS(2)="varying fluid source nodes,     " // CINERR(1) // ","     SUTERR.......77400
        DS(3)="does not equal the input value, " // CINERR(2) // ","     SUTERR.......77500
        DS(4)="for BCS time step " // CINERR(3) // "."                   SUTERR.......77600
        EX(1)="In dataset 2 of the BCS file, the variable NSOP1"         SUTERR.......77700
        EX(2)="must specify the exact number of time-dependent fluid"    SUTERR.......77800
        EX(3)="source nodes listed in dataset 3."                        SUTERR.......77900
      ELSE IF (ERRCOD.EQ.'BCS-2,4-1') THEN                               SUTERR.......78000
        DS(1)="The actual number of time-"                               SUTERR.......78100
        DS(2)="varying " // CHERR(1)(1:6) // " source nodes,    "        SUTERR.......78200
     1         // CINERR(1) // ","                                       SUTERR.......78300
        DS(3)="does not equal the input value, " // CINERR(2) // ","     SUTERR.......78400
        DS(4)="for BCS time step " // CINERR(3) // "."                   SUTERR.......78500
        EX(1)="In dataset 2 of the BCS file, the variable NSOU1"         SUTERR.......78600
        EX(2)="must specify the exact number of time-dependent "         SUTERR.......78700
     1         // CHERR(1)(1:6) // " source"                             SUTERR.......78800
        EX(3)="nodes listed in dataset 4."                               SUTERR.......78900
      ELSE IF (ERRCOD.EQ.'BCS-2,5-1') THEN                               SUTERR.......79000
        DS(1)="The actual number of"                                     SUTERR.......79100
        DS(2)="TIME-DEPENDENT pressure nodes,    " // CINERR(1) // ","   SUTERR.......79200
        DS(3)="does not equal the input value, " // CINERR(2) // ","     SUTERR.......79300
        DS(4)="for BCS time step " // CINERR(3) // "."                   SUTERR.......79400
        EX(1)="In dataset 2 of the BCS file, the variable NPBC1"         SUTERR.......79500
        EX(2)="must specify the exact number of time-dependent"          SUTERR.......79600
        EX(3)="pressure nodes listed in dataset 5."                      SUTERR.......79700
      ELSE IF (ERRCOD.EQ.'BCS-2,6-1') THEN                               SUTERR.......79800
        DS(1)="The actual number of"                                     SUTERR.......79900
        DS(2)="TIME-DEPENDENT " // CHERR(1)(1:13) // " nodes,  "         SUTERR.......80000
     1         // CINERR(1) // ","                                       SUTERR.......80100
        DS(3)="does not equal the input value,   " // CINERR(2) // ","   SUTERR.......80200
        DS(4)="for BCS time step " // CINERR(3) // "."                   SUTERR.......80300
        EX(1)="In dataset 2 of the BCS file, the variable NUBC1"         SUTERR.......80400
        EX(2)="must specify the exact number of time-dependent "         SUTERR.......80500
     1         // CHERR(1)(1:13)                                         SUTERR.......80600
        EX(3)="nodes listed in dataset 6."                               SUTERR.......80700
      ELSE IF (ERRCOD.EQ.'BCS-3-1') THEN                                 SUTERR.......80800
        DS(1)="Invalid node number referenced in dataset 3: "            SUTERR.......80900
     1         // CINERR(1)                                              SUTERR.......81000
        DS(2)="on BCS time step " // CINERR(3) // "."                    SUTERR.......81100
        EX(1)="Dataset 3 of the BCS file contains a reference"           SUTERR.......81200
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......81300
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......81400
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......81500
        EX(5)="(excluding the negative sign that precedes node"          SUTERR.......81600
        EX(6)="numbers for inactive boundary conditions)."               SUTERR.......81700
      ELSE IF (ERRCOD.EQ.'BCS-3-2') THEN                                 SUTERR.......81800
        DS(1)="Invalid node number referenced in dataset 3: "            SUTERR.......81900
     1         // CINERR(1)                                              SUTERR.......82000
        DS(2)="on BCS time step " // CINERR(2) // "."                    SUTERR.......82100
        EX(1)="Dataset 3 of the BCS file contains a reference"           SUTERR.......82200
        EX(2)="to an unrecognized boundary-condition node number."       SUTERR.......82300
        EX(3)="Any node number used in dataset 3 of the BCS file must"   SUTERR.......82400
        EX(4)="also appear in dataset 17 of the main input file"         SUTERR.......82500
        EX(5)="(excluding the negative signs that precede node"          SUTERR.......82600
        EX(6)="numbers for inactive and BCTIME boundary conditions)."    SUTERR.......82700
      ELSE IF (ERRCOD.EQ.'BCS-4-1') THEN                                 SUTERR.......82800
        DS(1)="Invalid node number referenced in dataset 4: "            SUTERR.......82900
     1         // CINERR(1)                                              SUTERR.......83000
        DS(2)="on BCS time step " // CINERR(3) // "."                    SUTERR.......83100
        EX(1)="Dataset 4 of the BCS file contains a reference"           SUTERR.......83200
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......83300
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......83400
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......83500
        EX(5)="(excluding the negative sign that precedes node"          SUTERR.......83600
        EX(6)="numbers for inactive boundary conditions)."               SUTERR.......83700
      ELSE IF (ERRCOD.EQ.'BCS-4-2') THEN                                 SUTERR.......83800
        DS(1)="Invalid node number referenced in dataset 4: "            SUTERR.......83900
     1         // CINERR(1)                                              SUTERR.......84000
        DS(2)="on BCS time step " // CINERR(2) // "."                    SUTERR.......84100
        EX(1)="Dataset 4 of the BCS file contains a reference"           SUTERR.......84200
        EX(2)="to an unrecognized boundary-condition node number."       SUTERR.......84300
        EX(3)="Any node number used in dataset 4 of the BCS file must"   SUTERR.......84400
        EX(4)="also appear in dataset 18 of the main input file"         SUTERR.......84500
        EX(5)="(excluding the negative signs that precede node"          SUTERR.......84600
        EX(6)="numbers for inactive and BCTIME boundary conditions)."    SUTERR.......84700
      ELSE IF (ERRCOD.EQ.'BCS-5-1') THEN                                 SUTERR.......84800
        DS(1)="Invalid node number referenced in dataset 5: "            SUTERR.......84900
     1         // CINERR(1)                                              SUTERR.......85000
        DS(2)="on BCS time step " // CINERR(3) // "."                    SUTERR.......85100
        EX(1)="Dataset 5 of the BCS file contains a reference"           SUTERR.......85200
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......85300
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......85400
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......85500
        EX(5)="(excluding the negative sign that precedes node"          SUTERR.......85600
        EX(6)="numbers for inactive boundary conditions)."               SUTERR.......85700
      ELSE IF (ERRCOD.EQ.'BCS-5-2') THEN                                 SUTERR.......85800
        DS(1)="Invalid node number referenced in dataset 5: "            SUTERR.......85900
     1         // CINERR(1)                                              SUTERR.......86000
        DS(2)="on BCS time step " // CINERR(2) // "."                    SUTERR.......86100
        EX(1)="Dataset 5 of the BCS file contains a reference"           SUTERR.......86200
        EX(2)="to an unrecognized boundary-condition node number."       SUTERR.......86300
        EX(3)="Any node number used in dataset 5 of the BCS file must"   SUTERR.......86400
        EX(4)="also appear in dataset 19 of the main input file"         SUTERR.......86500
        EX(5)="(excluding the negative signs that precede node"          SUTERR.......86600
        EX(6)="numbers for inactive and BCTIME boundary conditions)."    SUTERR.......86700
      ELSE IF (ERRCOD.EQ.'BCS-6-1') THEN                                 SUTERR.......86800
        DS(1)="Invalid node number referenced in dataset 6: "            SUTERR.......86900
     1         // CINERR(1)                                              SUTERR.......87000
        DS(2)="on BCS time step " // CINERR(3) // "."                    SUTERR.......87100
        EX(1)="Dataset 6 of the BCS file contains a reference"           SUTERR.......87200
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......87300
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......87400
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......87500
        EX(5)="(excluding the negative sign that precedes node"          SUTERR.......87600
        EX(6)="numbers for inactive boundary conditions)."               SUTERR.......87700
      ELSE IF (ERRCOD.EQ.'BCS-6-2') THEN                                 SUTERR.......87800
        DS(1)="Invalid node number referenced in dataset 6: "            SUTERR.......87900
     1         // CINERR(1)                                              SUTERR.......88000
        DS(2)="on BCS time step " // CINERR(2) // "."                    SUTERR.......88100
        EX(1)="Dataset 6 of the BCS file contains a reference"           SUTERR.......88200
        EX(2)="to an unrecognized boundary-condition node number."       SUTERR.......88300
        EX(3)="Any node number used in dataset 6 of the BCS file must"   SUTERR.......88400
        EX(4)="also appear in dataset 20 of the main input file"         SUTERR.......88500
        EX(5)="(excluding the negative signs that precede node"          SUTERR.......88600
        EX(6)="numbers for inactive and BCTIME boundary conditions)."    SUTERR.......88700
      ELSE IF (ERRCOD.EQ.'CON-1') THEN                                   SUTERR.......88800
        CDUM80 = 's'                                                     SUTERR.......88900
        IF (INERR(4).GT.13) THEN                                         SUTERR.......89000
           LDUM = 1                                                      SUTERR.......89100
        ELSE                                                             SUTERR.......89200
           LDUM = 0                                                      SUTERR.......89300
        END IF                                                           SUTERR.......89400
        DS(1)="Simulation terminated due to unconverged non-linearity"   SUTERR.......89500
        DS(2)="iterations.  Tolerance" // CDUM80(1:LDUM)                 SUTERR.......89600
     1         // " for " // CHERR(1)(1:INERR(4))                        SUTERR.......89700
        DS(3)="not reached."                                             SUTERR.......89800
        EX(1)="The " // CHERR(1)(1:INERR(4)) // " solution"              SUTERR.......89900
     1         // CDUM80(1:LDUM) // " failed"                            SUTERR.......90000
        EX(2)="to converge to the specified tolerance"                   SUTERR.......90100
     1         // CDUM80(1:LDUM) // " within"                            SUTERR.......90200
        EX(3)="the maximum number of iterations allowed to resolve"      SUTERR.......90300
        EX(4)="non-linearities.  The parameters that control these"      SUTERR.......90400
        EX(5)="iterations are set in dataset 7A of the main input file." SUTERR.......90500
      ELSE IF ((CODE(1).EQ.'REA').AND.                                   SUTERR.......90600
     1         ((CODE(2).EQ.'INP').OR.(CODE(2).EQ.'ICS').OR.             SUTERR.......90700
     2          (CODE(2).EQ.'BCS'))) THEN                                SUTERR.......90800
        IF (CODE(2).EQ.'INP') THEN                                       SUTERR.......90900
           CDUM80 = 'main input'                                         SUTERR.......91000
           LDUM = 10                                                     SUTERR.......91100
        ELSE IF (CODE(2).EQ.'ICS') THEN                                  SUTERR.......91200
           CDUM80 = 'initial conditions'                                 SUTERR.......91300
           LDUM = 18                                                     SUTERR.......91400
        ELSE IF (CODE(2).EQ.'BCS') THEN                                  SUTERR.......91500
           CDUM80 = 'boundary conditions'                                SUTERR.......91600
           LDUM = 19                                                     SUTERR.......91700
        END IF                                                           SUTERR.......91800
        IF ((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')) THEN                SUTERR.......91900
          DS(1)="FORTRAN returned an error while reading the restart"    SUTERR.......92000
          DS(2)="information following dataset 3 of the initial"         SUTERR.......92100
          DS(3)="conditions."                                            SUTERR.......92200
        ELSE IF (CODE(3).EQ.'INS') THEN                                  SUTERR.......92300
          CALL PRSWDS(CHERR(1), '-', 3, CODUM, NWORDS)                   SUTERR.......92400
          DS(1)="FORTRAN returned an error while reading an '@INSERT'"   SUTERR.......92500
          DS(2)="statement in the vicinity of dataset " // CODUM(3)(1:3) SUTERR.......92600
          DS(3)="of the " // CDUM80(1:LDUM) // "."                       SUTERR.......92700
        ELSE                                                             SUTERR.......92800
          DS(1)="FORTRAN returned an error while reading"                SUTERR.......92900
          DS(2)="dataset " // CODE(3)(1:3)                               SUTERR.......93000
     1           // " of the " // CDUM80(1:LDUM) // "."                  SUTERR.......93100
        END IF                                                           SUTERR.......93200
        EX(1)="A FORTRAN error has occurred while reading input data."   SUTERR.......93300
        EX(2)="Error status flag values are interpreted as follows:"     SUTERR.......93400
        EX(3)=" "                                                        SUTERR.......93500
        EX(4)="IOSTAT < 0  =>  The end of a line was reached before"     SUTERR.......93600
        EX(5)="                all the required data were read from"     SUTERR.......93700
        EX(6)="                that line.  Check the specified dataset"  SUTERR.......93800
        EX(7)="                for missing data or lines of data that"   SUTERR.......93900
        EX(8)="                exceed 1000 characters."                  SUTERR.......94000
        EX(9)="IOSTAT > 0  =>  An error occurred while the specified"    SUTERR.......94100
        EX(10)="                dataset was being read.  Usually, this"  SUTERR.......94200
        EX(11)="                indicates that the READ statement"       SUTERR.......94300
        EX(12)="                encountered data of a type that is"      SUTERR.......94400
        EX(13)="                incompatible with the type it expected." SUTERR.......94500
        EX(14)="                Check the dataset for typographical"     SUTERR.......94600
        EX(15)="                errors and missing or extraneous data."  SUTERR.......94700
      ELSE IF ((CODE(1).EQ.'REA').AND.(CODE(2).EQ.'FIL')) THEN           SUTERR.......94800
        DS(1)='FORTRAN returned an error while reading "SUTRA.FIL".'     SUTERR.......94900
        EX(1)='A FORTRAN error has occurred while reading "SUTRA.FIL".'  SUTERR.......95000
        EX(2)="Error status flag values are interpreted as follows:"     SUTERR.......95100
        EX(3)=" "                                                        SUTERR.......95200
        EX(4)="IOSTAT < 0  =>  The end of a line was reached before"     SUTERR.......95300
        EX(5)="                all the required data were read from"     SUTERR.......95400
        EX(6)='                that line.  Check "SUTRA.FIL" for'        SUTERR.......95500
        EX(7)="                missing data."                            SUTERR.......95600
        EX(8)="IOSTAT > 0  =>  An error occurred while the input"        SUTERR.......95700
        EX(9)="                file was being read.  Usually, this"      SUTERR.......95800
        EX(10)="                indicates that the READ statement"       SUTERR.......95900
        EX(11)="                encountered data of a type that is"      SUTERR.......96000
        EX(12)="                incompatible with the type it expected." SUTERR.......96100
        EX(13)='                Check "SUTRA.FIL" for typographical'     SUTERR.......96200
        EX(14)="                errors and missing or extraneous data."  SUTERR.......96300
      END IF                                                             SUTERR.......96400
C                                                                        SUTERR.......96500
C.....WRITE ERROR MESSAGE.  FORMAT DEPENDS ON THE TYPE OF ERROR.         SUTERR.......96600
      IF ((CODE(1).EQ.'INP').OR.(CODE(1).EQ.'ICS').OR.                   SUTERR.......96700
     1    (CODE(1).EQ.'BCS')) THEN                                       SUTERR.......96800
C........ERROR TYPES 'INP', 'ICS', AND 'BCS' (INPUT DATA ERROR)          SUTERR.......96900
         IF (KSCRN.EQ.1)                                                 SUTERR.......97000
     1      WRITE (*,1888) '           INPUT DATA ERROR           '      SUTERR.......97100
         WRITE (K00,1888) '           INPUT DATA ERROR           '       SUTERR.......97200
         IF (KSCRN.EQ.1) WRITE (*,1011)                                  SUTERR.......97300
         WRITE (K00,1011)                                                SUTERR.......97400
 1011    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......97500
         IF (CODE(1).EQ.'INP') THEN                                      SUTERR.......97600
            CDUM80 = FNAME(1)                                            SUTERR.......97700
         ELSE IF (CODE(1).EQ.'ICS') THEN                                 SUTERR.......97800
            CDUM80 = FNAME(2)                                            SUTERR.......97900
         ELSE                                                            SUTERR.......98000
            CDUM80 = FNAME(9)                                            SUTERR.......98100
         END IF                                                          SUTERR.......98200
         IF (KSCRN.EQ.1) WRITE (*,1013) ERRCOD, CDUM80, CODE(2)          SUTERR.......98300
         WRITE (K00,1013) ERRCOD, CDUM80, CODE(2)                        SUTERR.......98400
 1013    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......98500
     1           /4X,'File:      ',2X,A40                                SUTERR.......98600
     1           /4X,'Dataset(s):',2X,A40/)                              SUTERR.......98700
         DO 1015 I=1,50                                                  SUTERR.......98800
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......98900
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......99000
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......99100
 1015    CONTINUE                                                        SUTERR.......99200
         IF (KSCRN.EQ.1) WRITE (*,1021)                                  SUTERR.......99300
         WRITE (K00,1021)                                                SUTERR.......99400
 1021    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......99500
         DO 1025 I=1,50                                                  SUTERR.......99600
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......99700
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......99800
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......99900
 1025    CONTINUE                                                        SUTERR......100000
         IF (KSCRN.EQ.1) WRITE (*,1081)                                  SUTERR......100100
         WRITE (K00,1081)                                                SUTERR......100200
 1081    FORMAT (/1X,'GENERAL NOTE'/                                     SUTERR......100300
     1     /4X,'If the dataset for which SUTRA has reported an error'    SUTERR......100400
     1     /4X,'appears to be correct, check the preceding lines'        SUTERR......100500
     1     /4X,'for missing data or extraneous characters.')             SUTERR......100600
      ELSE IF (CODE(1).EQ.'FIL') THEN                                    SUTERR......100700
C........ERROR TYPE 'FIL' (FILE ERROR)                                   SUTERR......100800
         IF (KSCRN.EQ.1)                                                 SUTERR......100900
     1      WRITE (*,1888)'              FILE ERROR              '       SUTERR......101000
         WRITE (K00,1888) '              FILE ERROR              '       SUTERR......101100
         IF (KSCRN.EQ.1) WRITE (*,1211)                                  SUTERR......101200
         WRITE (K00,1211)                                                SUTERR......101300
 1211    FORMAT (/1X,'DESCRIPTION')                                      SUTERR......101400
         IF (KSCRN.EQ.1) WRITE (*,1213) ERRCOD, CHERR(1)                 SUTERR......101500
         WRITE (K00,1213) ERRCOD, CHERR(1)                               SUTERR......101600
 1213    FORMAT (/4X,'Error code:',2X,A40                                SUTERR......101700
     1           /4X,'File:      ',2X,A40/)                              SUTERR......101800
         DO 1215 I=1,50                                                  SUTERR......101900
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR......102000
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR......102100
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR......102200
 1215    CONTINUE                                                        SUTERR......102300
         IF (KSCRN.EQ.1) WRITE (*,1221)                                  SUTERR......102400
         WRITE (K00,1221)                                                SUTERR......102500
 1221    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR......102600
         DO 1225 I=1,50                                                  SUTERR......102700
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR......102800
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR......102900
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR......103000
 1225    CONTINUE                                                        SUTERR......103100
      ELSE IF (CODE(1).EQ.'SOL') THEN                                    SUTERR......103200
C........ERROR TYPE 'SOL' (MATRIX SOLVER ERROR)                          SUTERR......103300
         IF (KSCRN.EQ.1)                                                 SUTERR......103400
     1      WRITE (*,1888) '         MATRIX SOLVER ERROR          '      SUTERR......103500
         WRITE (K00,1888) '         MATRIX SOLVER ERROR          '       SUTERR......103600
         IF (KSCRN.EQ.1) WRITE (*,1311)                                  SUTERR......103700
         WRITE (K00,1311)                                                SUTERR......103800
 1311    FORMAT (/1X,'DESCRIPTION')                                      SUTERR......103900
         IF (KSCRN.EQ.1) WRITE (*,1313) ERRCOD, CHERR(2),                SUTERR......104000
     1      INERR(1), INERR(2), RLERR(1), RLERR(2)                       SUTERR......104100
         WRITE (K00,1313) ERRCOD, CHERR(2), INERR(1), INERR(2),          SUTERR......104200
     1      RLERR(1), RLERR(2)                                           SUTERR......104300
 1313    FORMAT (/4X,'Error code:',2X,A40                                SUTERR......104400
     1           /4X,'Solver:    ',2X,A40                                SUTERR......104500
     1          //4X,'Error flag..........IERR = ',I3                    SUTERR......104600
     1           /4X,'# of solver iters...ITRS = ',I5                    SUTERR......104700
     1           /4X,'Error estimate.......ERR = ',1PE8.1                SUTERR......104800
     1           /4X,'Error tolerance......TOL = ',1PE8.1/)              SUTERR......104900
         DO 1315 I=1,50                                                  SUTERR......105000
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR......105100
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR......105200
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR......105300
 1315    CONTINUE                                                        SUTERR......105400
         IF (KSCRN.EQ.1) WRITE (*,1321)                                  SUTERR......105500
         WRITE (K00,1321)                                                SUTERR......105600
 1321    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR......105700
         DO 1325 I=1,50                                                  SUTERR......105800
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR......105900
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR......106000
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR......106100
 1325    CONTINUE                                                        SUTERR......106200
      ELSE IF (CODE(1).EQ.'CON') THEN                                    SUTERR......106300
C........ERROR TYPE 'CON' (CONVERGENCE ERROR)                            SUTERR......106400
         IF (KSCRN.EQ.1)                                                 SUTERR......106500
     1      WRITE (*,1888) '          CONVERGENCE ERROR           '      SUTERR......106600
         WRITE (K00,1888) '         CONVERGENCE ERROR          '         SUTERR......106700
         IF (KSCRN.EQ.1) WRITE (*,1411)                                  SUTERR......106800
         WRITE (K00,1411)                                                SUTERR......106900
 1411    FORMAT (/1X,'DESCRIPTION')                                      SUTERR......107000
         IF (KSCRN.EQ.1) WRITE (*,1413) ERRCOD, CHERR(1), INERR(3),      SUTERR......107100
     1       RLERR(1), INERR(1), RLERR(2), RLERR(3), INERR(2), RLERR(4)  SUTERR......107200
         WRITE (K00,1413) ERRCOD, CHERR(1), INERR(3),                    SUTERR......107300
     1       RLERR(1), INERR(1), RLERR(2), RLERR(3), INERR(2), RLERR(4)  SUTERR......107400
 1413    FORMAT (/4X,'Error code: ',2X,A40                               SUTERR......107500
     1           /4X,'Unconverged:',2X,A40                               SUTERR......107600
     1      //4X,'# of iterations.....ITER = ',I5                        SUTERR......107700
     1       /4X,'Maximum P change.....RPM = ',1PE14.5,' (node ',I9,')'  SUTERR......107800
     1       /4X,'Tolerance for P....RPMAX = ',1PE14.5                   SUTERR......107900
     1       /4X,'Maximum U change.....RUM = ',1PE14.5,' (node ',I9,')'  SUTERR......108000
     1       /4X,'Tolerance for U....RUMAX = ',1PE14.5/)                 SUTERR......108100
         DO 1415 I=1,50                                                  SUTERR......108200
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR......108300
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR......108400
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR......108500
 1415    CONTINUE                                                        SUTERR......108600
         IF (KSCRN.EQ.1) WRITE (*,1421)                                  SUTERR......108700
         WRITE (K00,1421)                                                SUTERR......108800
 1421    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR......108900
         DO 1425 I=1,50                                                  SUTERR......109000
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR......109100
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR......109200
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR......109300
 1425    CONTINUE                                                        SUTERR......109400
      ELSE IF ((CODE(1).EQ.'REA').AND.                                   SUTERR......109500
     1         ((CODE(2).EQ.'INP').OR.(CODE(2).EQ.'ICS').OR.             SUTERR......109600
     2          (CODE(2).EQ.'BCS'))) THEN                                SUTERR......109700
C........ERROR TYPE 'REA-INP', 'REA-ICS', OR 'REA-BCS'                   SUTERR......109800
C           (FORTRAN READ ERROR)                                         SUTERR......109900
         IF (KSCRN.EQ.1)                                                 SUTERR......110000
     1      WRITE (*,1888) '          FORTRAN READ ERROR          '      SUTERR......110100
         WRITE (K00,1888) '          FORTRAN READ ERROR          '       SUTERR......110200
         IF (KSCRN.EQ.1) WRITE (*,1511)                                  SUTERR......110300
         WRITE (K00,1511)                                                SUTERR......110400
 1511    FORMAT (/1X,'DESCRIPTION')                                      SUTERR......110500
         IF (CODE(2).EQ.'INP') THEN                                      SUTERR......110600
            CDUM80 = FNAME(1)                                            SUTERR......110700
         ELSE IF (CODE(2).EQ.'ICS') THEN                                 SUTERR......110800
            CDUM80 = FNAME(2)                                            SUTERR......110900
         ELSE                                                            SUTERR......111000
            CDUM80 = FNAME(9)                                            SUTERR......111100
         END IF                                                          SUTERR......111200
         IF (((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')).OR.               SUTERR......111300
     1       (CODE(3).EQ.'INS')) THEN                                    SUTERR......111400
           IF (KSCRN.EQ.1) WRITE (*,1512) ERRCOD, CDUM80, INERR(1)       SUTERR......111500
           WRITE (K00,1512) ERRCOD, CDUM80, INERR(1)                     SUTERR......111600
 1512      FORMAT (/4X,'Error code:',2X,A40                              SUTERR......111700
     1             /4X,'File:      ',2X,A40                              SUTERR......111800
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR......111900
         ELSE IF (CODE(2).EQ.'BCS') THEN                                 SUTERR......112000
           IF (KSCRN.EQ.1) WRITE (*,1513) ERRCOD, CDUM80,                SUTERR......112100
     1        ADJUSTL(TRIM(CHERR(1))), ADJUSTL(TRIM(CHERR(2))),          SUTERR......112200
     2        CODE(3)(1:3), INERR(1)                                     SUTERR......112300
           WRITE (K00,1513) ERRCOD, CDUM80, ADJUSTL(TRIM(CHERR(1))),     SUTERR......112400
     1        ADJUSTL(TRIM(CHERR(2))), CODE(3)(1:3), INERR(1)            SUTERR......112500
 1513      FORMAT (/4X,'Error code:',2X,A40                              SUTERR......112600
     1             /4X,'File:      ',2X,A40                              SUTERR......112700
     1             /4X,'Time step: ',2X,A                                SUTERR......112800
     1             /4X,'Identifier:',2X,A                                SUTERR......112900
     1             /4X,'Dataset:   ',2X,A3                               SUTERR......113000
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR......113100
         ELSE                                                            SUTERR......113200
           IF (KSCRN.EQ.1) WRITE (*,1514) ERRCOD, CDUM80, CODE(3)(1:3),  SUTERR......113300
     1        INERR(1)                                                   SUTERR......113400
           WRITE (K00,1514) ERRCOD, CDUM80, CODE(3)(1:3), INERR(1)       SUTERR......113500
 1514      FORMAT (/4X,'Error code:',2X,A40                              SUTERR......113600
     1             /4X,'File:      ',2X,A40                              SUTERR......113700
     1             /4X,'Dataset:   ',2X,A3                               SUTERR......113800
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR......113900
         END IF                                                          SUTERR......114000
         DO 1515 I=1,50                                                  SUTERR......114100
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR......114200
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR......114300
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR......114400
 1515    CONTINUE                                                        SUTERR......114500
         IF (KSCRN.EQ.1) WRITE (*,1521)                                  SUTERR......114600
         WRITE (K00,1521)                                                SUTERR......114700
 1521    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR......114800
         DO 1525 I=1,50                                                  SUTERR......114900
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR......115000
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR......115100
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR......115200
 1525    CONTINUE                                                        SUTERR......115300
         IF (KSCRN.EQ.1) WRITE (*,1581)                                  SUTERR......115400
         WRITE (K00,1581)                                                SUTERR......115500
 1581    FORMAT (/1X,'GENERAL NOTE'/                                     SUTERR......115600
     1     /4X,'If the dataset for which SUTRA has reported an error'    SUTERR......115700
     1     /4X,'appears to be correct, check the preceding lines'        SUTERR......115800
     1     /4X,'for missing data or extraneous characters.')             SUTERR......115900
      ELSE IF ((CODE(1).EQ.'REA').AND.(CODE(2).EQ.'FIL')) THEN           SUTERR......116000
C........ERROR TYPE 'REA-FIL' (FORTRAN READ ERROR)                       SUTERR......116100
         IF (KSCRN.EQ.1)                                                 SUTERR......116200
     1      WRITE (*,1888) '          FORTRAN READ ERROR          '      SUTERR......116300
         WRITE (K00,1888) '          FORTRAN READ ERROR          '       SUTERR......116400
         IF (KSCRN.EQ.1) WRITE (*,1611)                                  SUTERR......116500
         WRITE (K00,1611)                                                SUTERR......116600
 1611    FORMAT (/1X,'DESCRIPTION')                                      SUTERR......116700
         IF (KSCRN.EQ.1) WRITE (*,1613) ERRCOD, INERR(1)                 SUTERR......116800
         WRITE (K00,1613) ERRCOD, INERR(1)                               SUTERR......116900
 1613    FORMAT (/4X,'Error code:',2X,A40                                SUTERR......117000
     1           /4X,'File:      ',2X,'SUTRA.FIL'                        SUTERR......117100
     1          //4X,'Error status flag.....IOSTAT = ',I5/)              SUTERR......117200
         DO 1615 I=1,50                                                  SUTERR......117300
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR......117400
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR......117500
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR......117600
 1615    CONTINUE                                                        SUTERR......117700
         IF (KSCRN.EQ.1) WRITE (*,1621)                                  SUTERR......117800
         WRITE (K00,1621)                                                SUTERR......117900
 1621    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR......118000
         DO 1625 I=1,50                                                  SUTERR......118100
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR......118200
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR......118300
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR......118400
 1625    CONTINUE                                                        SUTERR......118500
      END IF                                                             SUTERR......118600
 1888 FORMAT (                                                           SUTERR......118700
     1   /1X,'+--------+',38('-'),'+--------+'                           SUTERR......118800
     1   /1X,'| \\  // |',38('-'),'| \\  // |'                           SUTERR......118900
     1   /1X,'|  \\//  |',38(' '),'|  \\//  |'                           SUTERR......119000
     1   /1X,'|   //   |',A38,    '|   //   |'                           SUTERR......119100
     1   /1X,'|  //\\  |',38(' '),'|  //\\  |'                           SUTERR......119200
     1   /1X,'| //  \\ |',38('-'),'| //  \\ |'                           SUTERR......119300
     1   /1X,'+--------+',38('-'),'+--------+')                          SUTERR......119400
C                                                                        SUTERR......119500
C.....WRITE RUN TERMINATION MESSAGES AND CALL TERMINATION SEQUENCE       SUTERR......119600
      IF (KSCRN.EQ.1) WRITE (*,8888)                                     SUTERR......119700
      WRITE (K00,8888)                                                   SUTERR......119800
      IF (K3.NE.-1) WRITE (K3,8889)                                      SUTERR......119900
      IF (K5.NE.-1) WRITE (K5,8889)                                      SUTERR......120000
      IF (K6.NE.-1) WRITE (K6,8889)                                      SUTERR......120100
 8888 FORMAT (/1X,'+',56('-'),'+'/1X,'| ',54X,' |'/1X,'|',3X,            SUTERR......120200
     1   8('*'),3X,'RUN TERMINATED DUE TO ERROR',3X,9('*'),              SUTERR......120300
     1   3X,'|'/1X,'| ',54X,' |'/1X,'+',56('-'),'+')                     SUTERR......120400
 8889 FORMAT (//13X,'+',56('-'),'+'/13X,'| ',54X,' |'/13X,'|',3X,        SUTERR......120500
     1   8('*'),3X,'RUN TERMINATED DUE TO ERROR',3X,9('*'),              SUTERR......120600
     1   3X,'|'/13X,'| ',54X,' |'/13X,'+',56('-'),'+')                   SUTERR......120700
      IF (KSCRN.EQ.1) WRITE (*,8890)                                     SUTERR......120800
 8890 FORMAT (/' The above error message also appears in the SMY file,'  SUTERR......120900
     1        /' which may contain additional error information.')       SUTERR......121000
      CALL TERSEQ()                                                      SUTERR......121100
C                                                                        SUTERR......121200
      RETURN                                                             SUTERR......121300
      END                                                                SUTERR......121400
C                                                                        SUTERR......121500
C     SUBROUTINE        S  U  T  R  A              SUTRA VERSION 2.2     SUTRA..........100
C                                                                        SUTRA..........200
C *** PURPOSE :                                                          SUTRA..........300
C ***  MAIN CONTROL ROUTINE FOR SUTRA SIMULATION.  ORGANIZES             SUTRA..........400
C ***  INITIALIZATION, CALCULATIONS FOR EACH TIME STEP AND ITERATION,    SUTRA..........500
C ***  AND VARIOUS OUTPUTS.                                              SUTRA..........600
C                                                                        SUTRA..........700
!      SUBROUTINE SUTRA(TITLE1,TITLE2,PMAT,UMAT,PITER,UITER,PM1,DPDTITR,  SUTRA..........800
      SUBROUTINE SUTRA(TITLE1,TITLE2,PITER,UITER,PM1,DPDTITR,            SUTRA..........800
     1   UM1,UM2,PVEL,SL,SR,X,Y,Z,VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,  SUTRA..........900
     2   QIN,UIN,QUIN,QINITR,RCIT,RCITM1,PVEC,UVEC,                      SUTRA.........1000
     3   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,VMAG,VANG1,VANG2,           SUTRA.........1100
     4   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA.........1200
     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,B,           SUTRA.........1300
     6   GNUP1,GNUU1,IN,IQSOP,IQSOU,IPBC,IUBC,OBSPTS,NREG,LREG,          SUTRA.........1400
     7   IA,JA,IBCPBC,IBCUBC,IBCSOP,IBCSOU,IIDPBC,IIDUBC,IIDSOP,IIDSOU,  SUTRA.........1500
     8   IQSOPT,IQSOUT,IPBCT,IUBCT,BCSFL,BCSTR,IERROR)                          SUTRA.........1600
!     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,FWK,B,       SUTRA.........1300
!     6   GNUP1,GNUU1,IN,IQSOP,IQSOU,IPBC,IUBC,OBSPTS,NREG,LREG,IWK,      SUTRA.........1400
      USE ALLARR, ONLY : OBSDAT,CIDBCS                                   SUTRA.........1700
      USE LLDEF                                                          SUTRA.........1800
      USE EXPINT                                                         SUTRA.........1900
      USE SCHDEF                                                         SUTRA.........2000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTRA.........2100
      PARAMETER (NCOLMX=9)                                               SUTRA.........2200
      CHARACTER*8 VERNUM, VERNIN                                         SUTRA.........2300
      CHARACTER*1 TITLE1(80),TITLE2(80)                                  SUTRA.........2400
      CHARACTER*10 ADSMOD                                                SUTRA.........2500
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:13),CDUM80             SUTRA.........2600
      CHARACTER*40 SOLNAM(0:10)                                          SUTRA.........2700
      CHARACTER*10 SOLWRD(0:10)                                          SUTRA.........2800
      LOGICAL PRNALL,PRN0,PRNDEF,PRNK3,PRNK5,PRNK6                       SUTRA.........2900
      LOGICAL PRNBCF,PRNBCS,PRNBCP,PRNBCU                                SUTRA.........3000
      LOGICAL TSPRTD                                                     SUTRA.........3100
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                SUTRA.........3200
      LOGICAL ONCEK10,ONCEK11,ONCEK12,ONCEK13                            SUTRA.........3300
      LOGICAL ONCEP                                                      SUTRA.........3400
      LOGICAL ONCEBCS, SETBCS, BCSFL(0:ITMAX), BCSTR(0:ITMAX)            SUTRA.........3500
      INTEGER(1) IBCPBC(NBCN),IBCUBC(NBCN),IBCSOP(NSOP),IBCSOU(NSOU)     SUTRA.........3600
      INTEGER IIDPBC(NBCN),IIDUBC(NBCN),IIDSOP(NSOP),IIDSOU(NSOU)        SUTRA.........3700
      DIMENSION INERR(10),RLERR(10)                                      SUTRA.........3800
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             SUTRA.........3900
!      DIMENSION PMAT(NELT,NCBI),UMAT(NELT,NCBI)                          SUTRA.........4000
      DIMENSION PITER(NN),UITER(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN), SUTRA.........4100
     1   PVEL(NN),SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),       SUTRA.........4200
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),       SUTRA.........4300
     3   QIN(NN),QINITR(NN),UIN(NN),QUIN(NN),RCIT(NN),RCITM1(NN)         SUTRA.........4400
      DIMENSION PVEC(NNVEC),UVEC(NNVEC)                                  SUTRA.........4500
      DIMENSION ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),        SUTRA.........4600
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),          SUTRA.........4700
     2   PANGL1(NE)                                                      SUTRA.........4800
      DIMENSION ALMID(NEX),ATMID(NEX),                                   SUTRA.........4900
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),                 SUTRA.........5000
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX)                 SUTRA.........5100
      DIMENSION PBC(NBCN),UBC(NBCN),QPLITR(NBCN),GNUP1(NBCN),GNUU1(NBCN) SUTRA.........5200
      DIMENSION GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48)                  SUTRA.........5300
      DIMENSION B(NNNX)                                                  SUTRA.........5400
!      DIMENSION FWK(NWF),B(NNNX)                                         SUTRA.........5400
      DIMENSION IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),   SUTRA.........5500
     1   NREG(NN),LREG(NE),IA(NDIMIA),JA(NDIMJA)                         SUTRA.........5600
!     1   NREG(NN),LREG(NE),IWK(NWI),IA(NDIMIA),JA(NDIMJA)                SUTRA.........5600
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         SUTRA.........5700
      DIMENSION KTYPE(2)                                                 SUTRA.........5800
      TYPE (LLD), POINTER :: DENTS                                       SUTRA.........5900
      TYPE (LLD), ALLOCATABLE :: DENOB(:)                                SUTRA.........6000
      DIMENSION LCNT(NFLOMX)                                             SUTRA.........6100
      COMMON /BCSL/ ONCEBCS                                              SUTRA.........6200
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SUTRA.........6300
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,IREAD,           SUTRA.........6400
     2   ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE                                       SUTRA.........6500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTRA.........6600
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                            SUTRA.........6700
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        SUTRA.........6800
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        SUTRA.........6900
      COMMON /FNAMES/ UNAME,FNAME                                        SUTRA.........7000
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 SUTRA.........7100
     1   K10,K11,K12,K13                                                 SUTRA.........7200
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      SUTRA.........7300
      COMMON /ITSOLR/ TOLP,TOLU                                          SUTRA.........7400
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             SUTRA.........7500
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                SUTRA.........7600
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            SUTRA.........7700
      COMMON /MODSOR/ ADSMOD                                             SUTRA.........7800
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      SUTRA.........7900
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SUTRA.........8000
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SUTRA.........8100
      COMMON /PLT1/ ONCEK5, ONCEK6, ONCEK7, ONCEK8                       SUTRA.........8200
      COMMON /PLT2/ ONCEK10, ONCEK11, ONCEK12, ONCEK13                   SUTRA.........8300
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    SUTRA.........8400
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       SUTRA.........8500
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           SUTRA.........8600
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SUTRA.........8700
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      SUTRA.........8800
      COMMON /VER/ VERNUM, VERNIN                                        SUTRA.........8900
C                                                                        SUTRA.........9000
C.....INITIALIZE FLAG TO INDICATE THAT BCSTEP HAS NOT YET BEEN CALLED    SUTRA.........9100
C        IN THIS SUBROUTINE                                              SUTRA.........9200
      ONCEBCS = .FALSE.                                                  SUTRA.........9300
C                                                                        SUTRA.........9400
C.....WRITE TITLE TO CONSOLE                                             SUTRA.........9500
      DO 100 I=80,1,-1                                                   SUTRA.........9600
         IF (TITLE1(I).NE.' ') THEN                                      SUTRA.........9700
            LENT1 = I                                                    SUTRA.........9800
            GOTO 101                                                     SUTRA.........9900
         END IF                                                          SUTRA........10000
  100 CONTINUE                                                           SUTRA........10100
      LENT1 = 1                                                          SUTRA........10200
  101 DO 105 I=80,1,-1                                                   SUTRA........10300
         IF (TITLE2(I).NE.' ') THEN                                      SUTRA........10400
            LENT2 = I                                                    SUTRA........10500
            GOTO 106                                                     SUTRA........10600
         END IF                                                          SUTRA........10700
  105 CONTINUE                                                           SUTRA........10800
      LENT2 = 1                                                          SUTRA........10900
  106 CONTINUE                                                           SUTRA........11000
!      IF (KSCRN.EQ.1) WRITE (*,121) VERNUM                               SUTRA........11100
!      WRITE (K00,121) VERNUM                                             SUTRA........11200
!  121 FORMAT (/1X,9X,53("=")//1X,25X,"S    U    T    R    A",//          SUTRA........11300
!     1   31X,"Version ",A8//1X,9X,53("=")/)                              SUTRA........11400
!      IF (KSCRN.EQ.1) WRITE (*,122) (TITLE1(I),I=1,LENT1)                SUTRA........11500
!      WRITE (K00,122) (TITLE1(I),I=1,LENT1)                              SUTRA........11600
!      IF (KSCRN.EQ.1) WRITE (*,122) (TITLE2(I),I=1,LENT2)                SUTRA........11700
!      WRITE (K00,122) (TITLE2(I),I=1,LENT2)                              SUTRA........11800
!  122 FORMAT (1X,80A1)                                                   SUTRA........11900
!      IF (KSCRN.EQ.1) WRITE (*,*)                                        SUTRA........12000
!      WRITE (K00,*)                                                      SUTRA........12100
C                                                                        SUTRA........12200
C.....DETERMINE MAXIMUM SIMULATION TIME AND DURATION FROM TIME           SUTRA........12300
C        STEP SCHEDULE.                                                  SUTRA........12400
      IF (ISSTRA.EQ.0) THEN                                              SUTRA........12500
         DENTS => SCHDLS(ISCHTS)%SLIST                                   SUTRA........12600
         TMAX = DENTS%DVALU1                                             SUTRA........12700
         DO 310 K=1,ITMAX                                                SUTRA........12800
            DENTS => DENTS%NENT                                          SUTRA........12900
            TMAX = DENTS%DVALU1                                          SUTRA........13000
  310    CONTINUE                                                        SUTRA........13100
      ELSE                                                               SUTRA........13200
         TMAX = TSTART                                                   SUTRA........13300
      END IF                                                             SUTRA........13400
      TEMAX = TMAX - TSTART                                              SUTRA........13500
C                                                                        SUTRA........13600
C.....INITIALIZE TIME STEP NUMBER AND SCHEDULE POINTERS.  IF RESTART,    SUTRA........13700
C        SKIP AHEAD THROUGH TIME STEPS AND OBSERVATION SCHEDULES AND     SUTRA........13800
C        UPDATE TSEC.                                                    SUTRA........13900
      IT = ITRST                                                         SUTRA........14000
      ITBCS = IT                                                         SUTRA........14100
      DIT = DNINT(DBLE(IT))                                              SUTRA........14200
      IF (IT.EQ.0) THEN                                                  SUTRA........14300
         DELTLC = DELT                                                   SUTRA........14400
         DENTS => SCHDLS(ISCHTS)%SLIST                                   SUTRA........14500
      ELSE                                                               SUTRA........14600
         DENTS => SCHDLS(ISCHTS)%SLIST                                   SUTRA........14700
         DO 320 K=1,IT-1                                                 SUTRA........14800
            DENTS => DENTS%NENT                                          SUTRA........14900
  320    CONTINUE                                                        SUTRA........15000
         TMITM1 = DENTS%DVALU1                                           SUTRA........15100
         DENTS => DENTS%NENT                                             SUTRA........15200
         TMIT = DENTS%DVALU1                                             SUTRA........15300
         DELT = TMIT - TMITM1                                            SUTRA........15400
         DELTLC = DELT                                                   SUTRA........15500
         TSEC = TMIT                                                     SUTRA........15600
      END IF                                                             SUTRA........15700
      ALLOCATE(DENOB(NFLOMX))                                            SUTRA........15800
      DO 400 NFLO=1,NFLOMX                                               SUTRA........15900
         DENOB(NFLO)%NENT => SCHDLS(OFP(NFLO)%ISCHED)%SLIST              SUTRA........16000
         LCNT(NFLO) = 1                                                  SUTRA........16100
         IF ((IT.NE.0).AND.(IUNIO(NFLO).NE.-1).AND.(ISSTRA.EQ.0)) THEN   SUTRA........16200
            LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                       SUTRA........16300
            STEP = DENOB(NFLO)%NENT%DVALU2                               SUTRA........16400
            DO WHILE ((DIT.GE.STEP).AND.(LCNT(NFLO).LE.LENSCH))          SUTRA........16500
               IF (LCNT(NFLO).LT.LENSCH) THEN                            SUTRA........16600
                  DENOB(NFLO)%NENT => DENOB(NFLO)%NENT%NENT              SUTRA........16700
                  STEP = DENOB(NFLO)%NENT%DVALU2                         SUTRA........16800
               END IF                                                    SUTRA........16900
               LCNT(NFLO) = LCNT(NFLO) + 1                               SUTRA........17000
            END DO                                                       SUTRA........17100
         END IF                                                          SUTRA........17200
  400 CONTINUE                                                           SUTRA........17300
C                                                                        SUTRA........17400
C.....INITIALIZE FLAG THAT INDICATES WHETHER P HAS BEEN SOLVED FOR       SUTRA........17500
C         AT LEAST ONCE.  SET FLAG TO MAKE CALLS TO BCSTEP ACTUALLY      SUTRA........17600
C         SET THE BOUNDARY CONDITIONS (AS OPPOSED TO SIMPLY READING      SUTRA........17700
C         THEM IN SEARCH IF INPUT ERRORS).                               SUTRA........17800
      ONCEP = .FALSE.                                                    SUTRA........17900
      SETBCS = .TRUE.                                                    SUTRA........18000
C                                                                        SUTRA........18100
C.....SET FLAG FOR TIME-DEPENDENT SOURCES OR BOUNDARY CONDITIONS         SUTRA........18200
C        SET IN BCTIME. WHEN IBCT=+4, THERE ARE NO TIME-DEPENDENT        SUTRA........18300
C        SPECIFICATIONS IN BCTIME.                                       SUTRA........18400
      IBCT=IQSOPT+IQSOUT+IPBCT+IUBCT                                     SUTRA........18500
C                                                                        SUTRA........18600
C.....SET STARTING TIME OF SIMULATION CLOCK                              SUTRA........18700
C     TSEC=TSTART                                                        SUTRA........18800
      TSECP0=TSEC                                                        SUTRA........18900
      TSECU0=TSEC                                                        SUTRA........19000
      TMIN=TSEC/60.D0                                                    SUTRA........19100
      THOUR=TMIN/60.D0                                                   SUTRA........19200
      TDAY=THOUR/24.D0                                                   SUTRA........19300
      TWEEK=TDAY/7.D0                                                    SUTRA........19400
      TMONTH=TDAY/30.4375D0                                              SUTRA........19500
      TYEAR=TDAY/365.25D0                                                SUTRA........19600
C                                                                        SUTRA........19700
C.....OUTPUT INITIAL/STARTING CONDITIONS FOR TRANSIENT TRANSPORT         SUTRA........19800
      IF(ISSTRA.NE.1) THEN                                               SUTRA........19900
C........PRINT TO LST OUTPUT FILE                                        SUTRA........20000
!         IF (KTYPE(1).EQ.3) THEN                                         SUTRA........20100
!            CALL OUTLST3(0,0,0,0,0,0D0,0,0,0D0,PVEC,UVEC,VMAG,VANG1,     SUTRA........20200
!     1         VANG2,SW)                                                 SUTRA........20300
!         ELSE                                                            SUTRA........20400
!            CALL OUTLST2(0,0,0,0,0,0D0,0,0,0D0,PVEC,UVEC,VMAG,VANG1,SW)  SUTRA........20500
!         END IF                                                          SUTRA........20600
C........IF TRANSIENT FLOW, PRINT TO NODEWISE AND OBSERVATION OUTPUT     SUTRA........20700
C           FILES NOW.  (OTHERWISE, WAIT UNTIL STEADY-STATE FLOW         SUTRA........20800
C           SOLUTION IS COMPUTED.)                                       SUTRA........20900
!         IF (ISSFLO.EQ.0) THEN                                           SUTRA........21000
!            IF (K5.NE.-1)                                                SUTRA........21100
!     1         CALL OUTNOD(PVEC,UVEC,SW,X,Y,Z,TITLE1,TITLE2,BCSFL,BCSTR) SUTRA........21200
!            DO 650 NFLO=1,NFLOMX                                         SUTRA........21300
!               IF (IUNIO(NFLO).NE.-1) THEN                               SUTRA........21400
!                  IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                      SUTRA........21500
!                     CALL OUTOBS(NFLO,OBSPTS,TSEC,DIT,PM1,UM1,           SUTRA........21600
!     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG,BCSFL,BCSTR)     SUTRA........21700
!                  ELSE                                                   SUTRA........21800
!                     CALL OUTOBC(NFLO,OBSPTS,TSEC,DIT,PM1,UM1,           SUTRA........21900
!     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG,BCSFL,BCSTR)     SUTRA........22000
!                  END IF                                                 SUTRA........22100
!                  STEP = DENOB(NFLO)%NENT%DVALU2                         SUTRA........22200
!                  LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                 SUTRA........22300
!                  IF ((STEP.EQ.0D0).AND.(LCNT(NFLO).LT.LENSCH)) THEN     SUTRA........22400
!                     DENOB(NFLO)%NENT => DENOB(NFLO)%NENT%NENT           SUTRA........22500
!                     LCNT(NFLO) = LCNT(NFLO) + 1                         SUTRA........22600
!                  END IF                                                 SUTRA........22700
!               END IF                                                    SUTRA........22800
!  650       CONTINUE                                                     SUTRA........22900
!         END IF                                                          SUTRA........23000
      END IF                                                             SUTRA........23100
C                                                                        SUTRA........23200
C.....SET SWITCHES AND PARAMETERS FOR SOLUTION WITH STEADY-STATE FLOW    SUTRA........23300
      IF(ISSFLO.NE.1) GOTO 1000                                          SUTRA........23400
      ML=1                                                               SUTRA........23500
      NOUMAT=0                                                           SUTRA........23600
      ISSFLO=2                                                           SUTRA........23700
      ITER=0                                                             SUTRA........23800
      DLTPM1=DELTP                                                       SUTRA........23900
      DLTUM1=DELTU                                                       SUTRA........24000
      BDELP1 = 1D0                                                       SUTRA........24100
      BDELP=0.0D0                                                        SUTRA........24200
      BDELU=0.0D0                                                        SUTRA........24300
      IF (ISSTRA.NE.0) THEN                                              SUTRA........24400
!         IF (KSCRN.EQ.1) WRITE (*,902) IT, ITMAX                         SUTRA........24500
!         WRITE (K00,902) IT, ITMAX                                       SUTRA........24600
      ELSE                                                               SUTRA........24700
         TELAPS = TSEC - TSTART                                          SUTRA........24800
!         IF (KSCRN.EQ.1) WRITE (*,903) IT, ITMAX, TELAPS, TEMAX          SUTRA........24900
!         WRITE (K00,903) IT, ITMAX, TSEC, TEMAX                          SUTRA........25000
      END IF                                                             SUTRA........25100
!  902 FORMAT (1X, 'TIME STEP ', I8, ' OF ', I8)                          SUTRA........25200
!  903 FORMAT (1X, 'TIME STEP ', I8, ' OF ', I8, ';',                     SUTRA........25300
!     1        3X, 'ELAPSED TIME: ', 1PE11.4, ' OF ', 1PE11.4, ' [s]')    SUTRA........25400
      GOTO 1100                                                          SUTRA........25500
C                                                                        SUTRA........25600
C                                                                        SUTRA........25700
C ********************************************************************** SUTRA........25800
C.....BEGIN TIME STEP ************************************************** SUTRA........25900
C ********************************************************************** SUTRA........26000
C.....INCREMENT TIME STEP NUMBER                                         SUTRA........26100
 1000 IT=IT+1                                                            SUTRA........26200
      ITREL = IT - ITRST                                                 SUTRA........26300
      ITBCS = IT                                                         SUTRA........26400
      DIT = DNINT(DBLE(IT))                                              SUTRA........26500
      DENTS => DENTS%NENT                                                SUTRA........26600
      ITER=0                                                             SUTRA........26700
      ML=0                                                               SUTRA........26800
      NOUMAT=0                                                           SUTRA........26900
C.....SET NOUMAT TO OBTAIN U SOLUTION BY SIMPLE BACK SUBSTITUTION        SUTRA........27000
C        BEGINNING ON SECOND TIME STEP AFTER A PRESSURE SOLUTION         SUTRA........27100
C        IF THE SOLUTION IS NON-ITERATIVE (ITRMAX=1)                     SUTRA........27200
      IF (ONCEP.AND.ITREL.GT.2) THEN                                     SUTRA........27300
      IF(MOD(IT-1,NPCYC).NE.0.AND.MOD(IT,NPCYC).NE.0                     SUTRA........27400
     1   .AND.(.NOT.BCSFL(IT-1)).AND.(.NOT.BCSFL(IT))                    SUTRA........27500
     2   .AND.ITRMAX.EQ.1) NOUMAT=1                                      SUTRA........27600
      END IF                                                             SUTRA........27700
C.....CHOOSE SOLUTION VARIABLE ON THIS TIME STEP:                        SUTRA........27800
C        ML=0 FOR P AND U, ML=1 FOR P ONLY, AND ML=2 FOR U ONLY.         SUTRA........27900
      IF(IT.EQ.1.AND.ISSFLO.NE.2) GOTO 1005                              SUTRA........28000
      IF(MOD(IT,NPCYC).NE.0 .AND. (.NOT.BCSFL(IT))) ML=2                 SUTRA........28100
      IF(MOD(IT,NUCYC).NE.0 .AND. (.NOT.BCSTR(IT))) ML=1                 SUTRA........28200
C.....INCREMENT SIMULATION CLOCK, TSEC, TO END OF NEW TIME STEP          SUTRA........28300
 1005 TSECM1 = TSEC                                                      SUTRA........28400
      TSEC = DENTS%DVALU1                                                SUTRA........28500
      TMIN=TSEC/60.D0                                                    SUTRA........28600
      THOUR=TMIN/60.D0                                                   SUTRA........28700
      TDAY=THOUR/24.D0                                                   SUTRA........28800
      TWEEK=TDAY/7.D0                                                    SUTRA........28900
      TMONTH=TDAY/30.4375D0                                              SUTRA........29000
      TYEAR=TDAY/365.25D0                                                SUTRA........29100
C.....UPDATE TIME STEP SIZE                                              SUTRA........29200
      DELTM1 = DELT                                                      SUTRA........29300
      DELT = TSEC - TSECM1                                               SUTRA........29400
C.....NO SIMPLE BACK SUBSTITUTION FOR U IF TIME STEP HAS CHANGED         SUTRA........29500
C        BY MORE THAN A VERY SMALL TOLERANCE                             SUTRA........29600
      RELCHG = DABS((DELT - DELTLC)/DELTLC)                              SUTRA........29700
      IF (RELCHG.GT.1D-14) THEN                                          SUTRA........29800
         DELTLC = DELT                                                   SUTRA........29900
         NOUMAT = 0                                                      SUTRA........30000
      END IF                                                             SUTRA........30100
C                                                                        SUTRA........30200
C.....WRITE TIME STEP NUMBER AND ELAPSED TIME                            SUTRA........30300
      IF (ISSTRA.NE.0) THEN                                              SUTRA........30400
!         IF (KSCRN.EQ.1) WRITE (*,902) IT, ITMAX                         SUTRA........30500
!         WRITE (K00,902) IT, ITMAX                                       SUTRA........30600
      ELSE                                                               SUTRA........30700
         TELAPS = TSEC - TSTART                                          SUTRA........30800
!         IF (KSCRN.EQ.1) WRITE (*,903) IT, ITMAX, TELAPS, TEMAX          SUTRA........30900
!         WRITE (K00,903) IT, ITMAX, TELAPS, TEMAX                        SUTRA........31000
      END IF                                                             SUTRA........31100
C                                                                        SUTRA........31200
C.....SET TIME STEP (DELTP AND/OR DELTU) AND INCREMENT CLOCK             SUTRA........31300
C        FOR WHICHEVER OF P AND/OR U ARE SOLVED FOR ON THIS TIME STEP    SUTRA........31400
      IF(ML-1) 1010,1020,1030                                            SUTRA........31500
 1010 DLTPM1=DELTP                                                       SUTRA........31600
      DLTUM1=DELTU                                                       SUTRA........31700
      DELTP=TSEC-TSECP0                                                  SUTRA........31800
      DELTU=TSEC-TSECU0                                                  SUTRA........31900
      TSECP0=TSEC                                                        SUTRA........32000
      TSECU0=TSEC                                                        SUTRA........32100
      GOTO 1040                                                          SUTRA........32200
 1020 DLTPM1=DELTP                                                       SUTRA........32300
      DELTP=TSEC-TSECP0                                                  SUTRA........32400
      TSECP0=TSEC                                                        SUTRA........32500
      GOTO 1040                                                          SUTRA........32600
 1030 DLTUM1=DELTU                                                       SUTRA........32700
      DELTU=TSEC-TSECU0                                                  SUTRA........32800
      TSECU0=TSEC                                                        SUTRA........32900
 1040 CONTINUE                                                           SUTRA........33000
C.....SET PROJECTION FACTORS USED ON FIRST ITERATION TO EXTRAPOLATE      SUTRA........33100
C        AHEAD ONE-HALF TIME STEP                                        SUTRA........33200
      BDELP=(DELTP/DLTPM1)*0.50D0                                        SUTRA........33300
      BDELU=(DELTU/DLTUM1)*0.50D0                                        SUTRA........33400
      BDELP1=BDELP+1.0D0                                                 SUTRA........33500
      BDELU1=BDELU+1.0D0                                                 SUTRA........33600
C                                                                        SUTRA........33700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........33800
C.....BEGIN ITERATION - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........33900
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........34000
C.....INCREMENT ITERATION NUMBER                                         SUTRA........34100
 1100 ITER=ITER+1                                                        SUTRA........34200
C.....IF ITERATIVE SOLUTION, WRITE ITERATION NUMBER                      SUTRA........34300
!      IF (ITRMAX.NE.1) THEN                                              SUTRA........34400
!         IF (KSCRN.EQ.1) WRITE (*,1104) ITER                             SUTRA........34500
!         WRITE (K00,1104) ITER                                           SUTRA........34600
!      END IF                                                             SUTRA........34700
! 1104 FORMAT (1X, 3X, 'NON-LINEARITY ITERATION ', I5)                    SUTRA........34800
C                                                                        SUTRA........34900
!      IF(ML-1) 2000,2200,2400                                            SUTRA........35000
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH BOTH P AND U SOLUTIONS    SUTRA........35100
! 2000 DO 2025 I=1,NN                                                     SUTRA........35200
C.....SET DPDT-ITERATE TO VALUE FROM PREVIOUS ITERATION FOR PRESSURE     SUTRA........35300
C      COMING FROM THIS TIME STEP                                        SUTRA........35400
C       (THIS IS OVERWRITTEN ON THE FIRST ITERATION JUST BELOW)          SUTRA........35500
C     NOTE: DPDTITR IS USED ONLY IN THE BUDGET                           SUTRA........35600
!      DPDTITR(I)=(PVEC(I)-PM1(I))/DELTP                                  SUTRA........35700
!      PITER(I)=PVEC(I)                                                   SUTRA........35800
!      PVEL(I)=PVEC(I)                                                    SUTRA........35900
!      UITER(I)=UVEC(I)                                                   SUTRA........36000
!      RCITM1(I)=RCIT(I)                                                  SUTRA........36100
! 2025 RCIT(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                              SUTRA........36200
!      DO 2050 IP=1,NPBC                                                  SUTRA........36300
!      I=IABS(IPBC(IP))                                                   SUTRA........36400
!      QPLITR(IP)=GNUP1(IP)*(PBC(IP)-PITER(I))                            SUTRA........36500
! 2050 CONTINUE                                                           SUTRA........36600
C.....QINITR VALUE DIFFERS FROM QIN ONLY IF BCTIME OR BCSTEP CHANGED QIN SUTRA........36700
!      IF (ITER.LE.2) THEN                                                SUTRA........36800
!         DO 2060 I=1,NN                                                  SUTRA........36900
! 2060    QINITR(I)=QIN(I)                                                SUTRA........37000
!      END IF                                                             SUTRA........37100
!      IF(ITER.GT.1) GOTO 2600                                            SUTRA........37200
!      DO 2075 I=1,NN                                                     SUTRA........37300
!      PITER(I)=BDELP1*PVEC(I)-BDELP*PM1(I)                               SUTRA........37400
!      UITER(I)=BDELU1*UVEC(I)-BDELU*UM1(I)                               SUTRA........37500
C.....RESETS DPDT-ITERATE TO VALUE FROM MOST RECENT PRESSURE TIME STEP   SUTRA........37600
C      ON THE FIRST ITERATION FOR THIS TIME STEP                         SUTRA........37700
!      DPDTITR(I)=(PVEC(I)-PM1(I))/DLTPM1                                 SUTRA........37800
!      PM1(I)=PVEC(I)                                                     SUTRA........37900
!      UM2(I)=UM1(I)                                                      SUTRA........38000
! 2075 UM1(I)=UVEC(I)                                                     SUTRA........38100
!      GOTO 2600                                                          SUTRA........38200
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH P SOLUTION ONLY           SUTRA........38300
! 2200 DO 2225 I=1,NN                                                     SUTRA........38400
!      PVEL(I)=PVEC(I)                                                    SUTRA........38500
! 2225 PITER(I)=PVEC(I)                                                   SUTRA........38600
!      IF(ITER.GT.1) GOTO 2600                                            SUTRA........38700
!      DO 2250 I=1,NN                                                     SUTRA........38800
!      PITER(I)=BDELP1*PVEC(I)-BDELP*PM1(I)                               SUTRA........38900
!      UITER(I)=UVEC(I)                                                   SUTRA........39000
!      RCITM1(I)=RCIT(I)                                                  SUTRA........39100
!      RCIT(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                              SUTRA........39200
! 2250 PM1(I)=PVEC(I)                                                     SUTRA........39300
!      GOTO 2600                                                          SUTRA........39400
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH U SOLUTION ONLY           SUTRA........39500
! 2400 IF (ITER.EQ.1) THEN                                                SUTRA........39600
!         DO 2405 I=1,NN                                                  SUTRA........39700
! 2405       UITER(I)=BDELU1*UVEC(I)-BDELU*UM1(I)                         SUTRA........39800
!      ELSE                                                               SUTRA........39900
!         DO 2410 I=1,NN                                                  SUTRA........40000
! 2410       UITER(I)=UVEC(I)                                             SUTRA........40100
!      END IF                                                             SUTRA........40200
!      IF(NOUMAT.EQ.1) GOTO 2480                                          SUTRA........40300
C.....SET PARAMETERS FROM MOST RECENT PRESSURE TIME STEP                 SUTRA........40400
!      IF(ITER.GT.1) GOTO 2600                                            SUTRA........40500
!      DO 2450 I=1,NN                                                     SUTRA........40600
!      DPDTITR(I)=(PVEC(I)-PM1(I))/DELTP                                  SUTRA........40700
!      QINITR(I)=QIN(I)                                                   SUTRA........40800
!      PITER(I)=PVEC(I)                                                   SUTRA........40900
!      PVEL(I)=PVEC(I)                                                    SUTRA........41000
! 2450 RCITM1(I)=RCIT(I)                                                  SUTRA........41100
!      DO 2475 IP=1,NPBC                                                  SUTRA........41200
!      I=IABS(IPBC(IP))                                                   SUTRA........41300
!      QPLITR(IP)=GNUP1(IP)*(PBC(IP)-PITER(I))                            SUTRA........41400
! 2475 CONTINUE                                                           SUTRA........41500
! 2480 DO 2500 I=1,NN                                                     SUTRA........41600
!      UM2(I)=UM1(I)                                                      SUTRA........41700
! 2500 UM1(I)=UVEC(I)                                                     SUTRA........41800
! 2600 CONTINUE                                                           SUTRA........41900
C                                                                        SUTRA........42000
C.....INITIALIZE ARRAYS WITH VALUE OF ZERO                               SUTRA........42100
!      MATDIM=NELT*NCBI                                                   SUTRA........42200
!      IF(ML-1) 3000,3000,3300                                            SUTRA........42300
! 3000 CALL ZERO(PMAT,MATDIM,0.0D0)                                       SUTRA........42400
!      CALL ZERO(PVEC,NNVEC,0.0D0)                                        SUTRA........42500
!      CALL ZERO(VOL,NN,0.0D0)                                            SUTRA........42600
!      IF(ML-1) 3300,3400,3300                                            SUTRA........42700
! 3300 IF(NOUMAT) 3350,3350,3375                                          SUTRA........42800
! 3350 CALL ZERO(UMAT,MATDIM,0.0D0)                                       SUTRA........42900
! 3375 CALL ZERO(UVEC,NNVEC,0.0D0)                                        SUTRA........43000
! 3400 CONTINUE                                                           SUTRA........43100
C                                                                        SUTRA........43200
C.....SET TIME-DEPENDENT BOUNDARY CONDITIONS, SOURCES AND SINKS          SUTRA........43300
C        FOR THIS TIME STEP                                              SUTRA........43400
!      IF (ITER.EQ.1.AND.IBCT.NE.4)                                       SUTRA........43500
!     1   CALL BCTIME(IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,IQSOU,         SUTRA........43600
!     2   IPBCT,IUBCT,IQSOPT,IQSOUT,X,Y,Z,IBCPBC,IBCUBC,IBCSOP,IBCSOU)    SUTRA........43700
!      IF ((ITER.EQ.1).AND.(K9.NE.-1))                                    SUTRA........43800
!     1   CALL BCSTEP(SETBCS,IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,IQSOU,  SUTRA........43900
!     2   IPBCT1,IUBCT1,IQSOPT1,IQSOUT1,GNUP1,GNUU1,                      SUTRA........44000
!     3   IBCPBC,IBCUBC,IBCSOP,IBCSOU,IIDPBC,IIDUBC,IIDSOP,IIDSOU,        SUTRA........44100
!     4   NCID,BCSFL,BCSTR)                                               SUTRA........44200
C                                                                        SUTRA........44300
C.....SET SORPTION PARAMETERS FOR THIS TIME STEP                         SUTRA........44400
!      IF(ML.NE.1.AND.ME.EQ.-1.AND.NOUMAT.EQ.0.AND.                       SUTRA........44500
!     1   ADSMOD.NE.'NONE      ') CALL ADSORB(CS1,CS2,CS3,SL,SR,UITER)    SUTRA........44600
C                                                                        SUTRA........44700
C.....DO ELEMENTWISE CALCULATIONS IN MATRIX EQUATION FOR P AND/OR U      SUTRA........44800
!      IF (NOUMAT.EQ.0) THEN                                              SUTRA........44900
!       IF (KTYPE(1).EQ.3) THEN                                           SUTRA........45000
C..... 3D PROBLEM                                                        SUTRA........45100
!       CALL ELEMN3(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,              SUTRA........45200
!     2   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,                            SUTRA........45300
!     3   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA........45400
!     4   PANGL1,PANGL2,PANGL3,VMAG,VANG1,VANG2,VOL,PMAT,PVEC,            SUTRA........45500
!     5   UMAT,UVEC,GXSI,GETA,GZET,PVEL,LREG,IA,JA)                       SUTRA........45600
!       ELSE                                                              SUTRA........45700
C..... 2D PROBLEM                                                        SUTRA........45800
!       CALL ELEMN2(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,              SUTRA........45900
!     2   ALMAX,ALMIN,ATMAX,ATMIN,PERMXX,PERMXY,PERMYX,PERMYY,PANGL1,     SUTRA........46000
!     3   VMAG,VANG1,VOL,PMAT,PVEC,UMAT,UVEC,GXSI,GETA,PVEL,LREG,IA,JA)   SUTRA........46100
!       END IF                                                            SUTRA........46200
!      END IF                                                             SUTRA........46300
C                                                                        SUTRA........46400
C.....DO NODEWISE CALCULATIONS IN MATRIX EQUATION FOR P AND/OR U         SUTRA........46500
!      CALL NODAL(ML,VOL,PMAT,PVEC,UMAT,UVEC,PITER,UITER,PM1,UM1,UM2,     SUTRA........46600
!     1   POR,QIN,UIN,QUIN,QINITR,CS1,CS2,CS3,SL,SR,SW,DSWDP,RHO,SOP,     SUTRA........46700
!     2   NREG,JA)                                                        SUTRA........46800
C                                                                        SUTRA........46900
C.....SET SPECIFIED P AND U CONDITIONS IN MATRIX EQUATION FOR P AND/OR U SUTRA........47000
!      CALL BC(ML,PMAT,PVEC,UMAT,UVEC,IPBC,PBC,IUBC,UBC,QPLITR,JA,        SUTRA........47100
!     1   GNUP1,GNUU1)                                                    SUTRA........47200
C                                                                        SUTRA........47300
C.....MATRIX EQUATION FOR P AND/OR U COMPLETE.  SOLVE EQUATIONS:         SUTRA........47400
C        WITH DIRECT SOLVER,                                             SUTRA........47500
C           WHEN KMT=0, DECOMPOSE AND BACK-SUBSTITUTE,                   SUTRA........47600
C           WHEN KMT=2, BACK-SUBSTITUTE ONLY.                            SUTRA........47700
C        KPU=1 WHEN SOLVING FOR P,                                       SUTRA........47800
C        KPU=2 WHEN SOLVING FOR U.                                       SUTRA........47900
!      IHALFB=NBHALF-1                                                    SUTRA........48000
!      IERRP = 0                                                          SUTRA........48100
!      IERRU = 0                                                          SUTRA........48200
!      IF(ML-1) 5000,5000,5500                                            SUTRA........48300
C                                                                        SUTRA........48400
C.....SOLVE FOR P                                                        SUTRA........48500
! 5000 KMT=000000                                                         SUTRA........48600
!      KPU=1                                                              SUTRA........48700
!      KSOLVR = KSOLVP                                                    SUTRA........48800
!      CALL SOLVER(KMT,KPU,KSOLVR,PMAT,PVEC,PITER,B,NN,IHALFB,NELT,NCBI,  SUTRA........48900
!     1            IWK,FWK,IA,JA,IERRP,ITRSP,ERRP)                        SUTRA........49000
!      ONCEP = .TRUE.                                                     SUTRA........49100
C.....P SOLUTION NOW IN PVEC                                             SUTRA........49200
C                                                                        SUTRA........49300
C.....IF STEADY FLOW, SET PM1=PVEC SO THAT INTERPOLATION AT FRACTIONAL   SUTRA........49400
C        TIME STEPS YIELDS THE STEADY-STATE PRESSURE.                    SUTRA........49500
!      IF (ISSFLO.NE.0) THEN                                              SUTRA........49600
!         DO 5200 I=1,NN                                                  SUTRA........49700
!            PM1(I) = PVEC(I)                                             SUTRA........49800
! 5200    CONTINUE                                                        SUTRA........49900
!      END IF                                                             SUTRA........50000
C                                                                        SUTRA........50100
!      IF(ML-1) 5500,6000,5500                                            SUTRA........50200
C                                                                        SUTRA........50300
C.....SOLVE FOR U                                                        SUTRA........50400
! 5500 KMT=000000                                                         SUTRA........50500
!      KPU=2                                                              SUTRA........50600
!      IF(NOUMAT) 5700,5700,5600                                          SUTRA........50700
! 5600 KMT=2                                                              SUTRA........50800
! 5700 KSOLVR = KSOLVU                                                    SUTRA........50900
!      CALL SOLVER(KMT,KPU,KSOLVR,UMAT,UVEC,UITER,B,NN,IHALFB,NELT,NCBI,  SUTRA........51000
!     1            IWK,FWK,IA,JA,IERRU,ITRSU,ERRU)                        SUTRA........51100
! 6000 CONTINUE                                                           SUTRA........51200
C.....U SOLUTION NOW IN UVEC                                             SUTRA........51300
C                                                                        SUTRA........51400
!      IERR = IABS(IERRP) + IABS(IERRU)                                   SUTRA........51500
C                                                                        SUTRA........51600
C.....CHECK PROGRESS AND CONVERGENCE OF NON-LINEARITY ITERATIONS         SUTRA........51700
C        AND SET STOP AND GO FLAGS:                                      SUTRA........51800
C           ISTOP = -1   NOT CONVERGED - STOP SIMULATION                 SUTRA........51900
C           ISTOP =  0   ITERATIONS LEFT OR CONVERGED - KEEP SIMULATING  SUTRA........52000
C           ISTOP =  1   LAST TIME STEP REACHED - STOP SIMULATION        SUTRA........52100
C           IGOI = 0   P AND U CONVERGED, OR NO ITERATIONS DONE          SUTRA........52200
C           IGOI = 1   ONLY P HAS NOT YET CONVERGED TO CRITERION         SUTRA........52300
C           IGOI = 2   ONLY U HAS NOT YET CONVERGED TO CRITERION         SUTRA........52400
C           IGOI = 3   BOTH P AND U HAVE NOT YET CONVERGED TO CRITERIA   SUTRA........52500
!      ISTOP=0                                                            SUTRA........52600
!      IGOI=0                                                             SUTRA........52700
!      IF(ITRMAX-1) 7500,7500,7000                                        SUTRA........52800
! 7000 RPM=0.D0                                                           SUTRA........52900
!      RUM=0.D0                                                           SUTRA........53000
!      IPWORS=0                                                           SUTRA........53100
!      IUWORS=0                                                           SUTRA........53200
!      IF(ML-1) 7050,7050,7150                                            SUTRA........53300
! 7050 DO 7100 I=1,NN                                                     SUTRA........53400
!      RP=DABS(PVEC(I)-PITER(I))                                          SUTRA........53500
!      IF(RP-RPM) 7100,7060,7060                                          SUTRA........53600
! 7060 RPM=RP                                                             SUTRA........53700
!      IPWORS=I                                                           SUTRA........53800
! 7100 CONTINUE                                                           SUTRA........53900
!      IF(RPM.GT.RPMAX) IGOI=IGOI+1                                       SUTRA........54000
! 7150 IF(ML-1) 7200,7350,7200                                            SUTRA........54100
! 7200 DO 7300 I=1,NN                                                     SUTRA........54200
!      RU=DABS(UVEC(I)-UITER(I))                                          SUTRA........54300
!      IF(RU-RUM) 7300,7260,7260                                          SUTRA........54400
! 7260 RUM=RU                                                             SUTRA........54500
!      IUWORS=I                                                           SUTRA........54600
! 7300 CONTINUE                                                           SUTRA........54700
!      IF(RUM.GT.RUMAX) IGOI=IGOI+2                                       SUTRA........54800
! 7350 CONTINUE                                                           SUTRA........54900
!      IF (KSCRN.EQ.1) WRITE (*,7377) RPM, RUM                            SUTRA........55000
!     WRITE (K00,7377) RPM, RUM                                          SUTRA........55100
! 7377 FORMAT (1X, 6X, 'Maximum changes in P, U: ',1PE8.1,", ",1PE8.1)    SUTRA........55200
!      IF(IGOI.GT.0.AND.ITER.EQ.ITRMAX) ISTOP=-1                          SUTRA........55300
!      IF(IGOI.GT.0.AND.ISTOP.EQ.0.AND.IERR.EQ.0) GOTO 1100               SUTRA........55400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........55500
C.....END ITERATION - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........55600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........55700
C                                                                        SUTRA........55800
 7500 CONTINUE                                                           SUTRA........55900
      IF(ISTOP.NE.-1.AND.IT.EQ.ITMAX) ISTOP=1                            SUTRA........56000
C                                                                        SUTRA........56100
C.....OUTPUT RESULTS FOR TIME STEP IN ACCORDANCE WITH PRINT CYCLES       SUTRA........56200
C                                                                        SUTRA........56300
C.....COMPUTE SOME LOGICAL CONDITIONS.  PRNALL=.TRUE. INDICATES THAT     SUTRA........56400
C        ALL RESULTS SHOULD BE PRINTED BECAUSE THIS IS THE LAST TIME     SUTRA........56500
C        STEP (EITHER BY DESIGN OR BECAUSE OF AN ERROR).  PRN0=.TRUE.    SUTRA........56600
C        INDICATES THAT INITIAL CONDITIONS ARE TO BE PRINTED FOR A       SUTRA........56700
C        STEADY-FLOW, TRANSIENT-TRANSPORT RUN.  PRNDEF=.TRUE. IF         SUTRA........56800
C        EITHER OF THE TWO PRECEDING CONDITIONS IS TRUE.                 SUTRA........56900
!      PRNALL = ((ISTOP.NE.0).OR.(IERR.NE.0))                             SUTRA........57000
!      PRN0 = ((ITREL.EQ.0).AND.(ISSFLO.NE.0).AND.(ISSTRA.NE.1))          SUTRA........57100
!      PRNDEF = (PRNALL.OR.PRN0)                                          SUTRA........57200
C.....PRINT RESULTS TO THE LST OUTPUT FILE                               SUTRA........57300
!      PRNK3 = (PRNDEF.OR.(MOD(IT,NPRINT).EQ.0)                           SUTRA........57400
!     1         .OR.((ITREL.EQ.1).AND.(NPRINT.GT.0)))                     SUTRA........57500
!      IF (PRNK3) THEN                                                    SUTRA........57600
!      IF (KTYPE(1).EQ.3) THEN                                            SUTRA........57700
!         CALL OUTLST3(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,IERRU,ITRSU,ERRU,   SUTRA........57800
!     1      PVEC,UVEC,VMAG,VANG1,VANG2,SW)                               SUTRA........57900
!      ELSE                                                               SUTRA........58000
!         CALL OUTLST2(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,IERRU,ITRSU,ERRU,   SUTRA........58100
!     1      PVEC,UVEC,VMAG,VANG1,SW)                                     SUTRA........58200
!      END IF                                                             SUTRA........58300
C.....CALCULATE AND PRINT FLUID MASS AND/OR ENERGY OR SOLUTE MASS BUDGET SUTRA........58400
!      IF(KBUDG.EQ.1)                                                     SUTRA........58500
!     1   CALL BUDGET(ML,IBCT,VOL,SW,DSWDP,RHO,SOP,QIN,PVEC,PM1,DPDTITR,  SUTRA........58600
!     2      PBC,QPLITR,IPBC,IQSOP,POR,UVEC,UM1,UM2,UIN,QUIN,QINITR,      SUTRA........58700
!     3      IQSOU,UBC,IUBC,CS1,CS2,CS3,SL,SR,NREG,GNUP1,GNUU1,           SUTRA........58800
!     4      IBCSOP,IBCSOU)                                               SUTRA........58900
!      END IF                                                             SUTRA........59000
C.....PRINT NODEWISE AND ELEMENTWISE RESULTS TO OUTPUT FILES             SUTRA........59100
!      PRNK5 = ((PRNDEF.OR.((IT.NE.0).AND.(MOD(IT,NCOLPR).EQ.0))          SUTRA........59200
!     1         .OR.((ITREL.EQ.1).AND.(NCOLPR.GT.0))).AND.(K5.NE.-1))     SUTRA........59300
!      IF (PRNK5) CALL OUTNOD(PVEC,UVEC,SW,X,Y,Z,TITLE1,TITLE2,           SUTRA........59400
!     1   BCSFL,BCSTR)                                                    SUTRA........59500
!      PRNK6 = ((PRNALL.OR.((IT.NE.0).AND.(MOD(IT,LCOLPR).EQ.0))          SUTRA........59600
!     1         .OR.(ITREL.EQ.1)).AND.(K6.NE.-1))                         SUTRA........59700
!      IF (PRNK6) CALL OUTELE(VMAG,VANG1,VANG2,IN,X,Y,Z,TITLE1,TITLE2,    SUTRA........59800
!     1   BCSFL,BCSTR)                                                    SUTRA........59900
C.....PRINT RESULTS TO BOUNDARY CONDITION OUTPUT FILES.                  SUTRA........60000
!      PRNBCF = ((PRNALL.OR.((IT.NE.0).AND.(MOD(IT,NBCFPR).EQ.0))         SUTRA........60100
!     1          .OR.((ITREL.EQ.1).AND.(NBCFPR.GT.0))).AND.(K10.NE.-1))   SUTRA........60200
!      PRNBCS = ((PRNALL.OR.((IT.NE.0).AND.(MOD(IT,NBCSPR).EQ.0))         SUTRA........60300
!     1          .OR.((ITREL.EQ.1).AND.(NBCSPR.GT.0))).AND.(K11.NE.-1))   SUTRA........60400
!      PRNBCP = ((PRNALL.OR.((IT.NE.0).AND.(MOD(IT,NBCPPR).EQ.0))         SUTRA........60500
!     1          .OR.((ITREL.EQ.1).AND.(NBCPPR.GT.0))).AND.(K12.NE.-1))   SUTRA........60600
!      PRNBCU = ((PRNALL.OR.((IT.NE.0).AND.(MOD(IT,NBCUPR).EQ.0))         SUTRA........60700
!     1          .OR.((ITREL.EQ.1).AND.(NBCUPR.GT.0))).AND.(K13.NE.-1))   SUTRA........60800
!      IF (PRNBCF)                                                        SUTRA........60900
!     1   CALL OUTBCOF(QIN,IQSOP,UVEC,UIN,QINITR,IBCSOP,TITLE1,TITLE2,    SUTRA........61000
!     2      IIDSOP)                                                      SUTRA........61100
!      IF (PRNBCS)                                                        SUTRA........61200
!     1   CALL OUTBCOS(QUIN,IQSOU,IBCSOU,TITLE1,TITLE2,IIDSOU)            SUTRA........61300
!      IF (PRNBCP)                                                        SUTRA........61400
!     1   CALL OUTBCOP(PVEC,UVEC,PBC,UBC,QPLITR,GNUP1,IPBC,IBCPBC,        SUTRA........61500
!     2      TITLE1,TITLE2,IIDPBC)                                        SUTRA........61600
!      IF (PRNBCU)                                                        SUTRA........61700
!     1   CALL OUTBCOU(UVEC,UBC,GNUU1,IUBC,IBCUBC,TITLE1,TITLE2,          SUTRA........61800
!     2      IIDUBC)                                                      SUTRA........61900
C.....PRINT RESULTS TO OBSERVATION OUTPUT FILES.  CHECK FOR OUTPUT       SUTRA........62000
C       SCHEDULED WITHIN THE CURRENT TIME STEP AND PRINT IT.  IF THIS    SUTRA........62100
C       IS THE INITIAL CONDITION (IT=0) OR THE FINAL TIME STEP, PRINT    SUTRA........62200
C       RESULTS IF THEY HAVE NOT ALREADY BEEN PRINTED.                   SUTRA........62300
C.....LOOP OVER OBSERVATION OUTPUT SCHEDULES.                            SUTRA........62400
!      DO 7650 NFLO=1,NFLOMX                                              SUTRA........62500
C........IF NO FILE FOR THIS OUTPUT, SKIP IT                             SUTRA........62600
!         IF (IUNIO(NFLO).EQ.-1) CYCLE                                    SUTRA........62700
C........SET FLAG INDICATING THAT OUTPUT HAS NOT (YET) BEEN PRINTED      SUTRA........62800
C           FOR THE END OF THE CURRENT TIME STEP                         SUTRA........62900
!         TSPRTD = .FALSE.                                                SUTRA........63000
C........IF TRANSPORT IS TRANSIENT AND THIS IS NOT THE INITIAL           SUTRA........63100
C            CONDITION, CHECK FOR SCHEDULED OUTPUT AND PRINT IT.         SUTRA........63200
!         IF ((ISSTRA.EQ.0).AND.(ITREL.NE.0)) THEN                        SUTRA........63300
C...........GET THE LENGTH OF THE SCHEDULE AND THE NEXT TIME/STEP        SUTRA........63400
C              SCHEDULED TO BE OUTPUT.                                   SUTRA........63500
!            LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                       SUTRA........63600
!            TIME = DENOB(NFLO)%NENT%DVALU1                               SUTRA........63700
!            STEP = DENOB(NFLO)%NENT%DVALU2                               SUTRA........63800
C...........LOOP THROUGH THE SCHEDULE, PRINTING ANY OUTPUT SCHEDULED     SUTRA........63900
C              WITHIN THE CURRENT TIME STEP.  (LOOP AS LONG AS THE       SUTRA........64000
C              SCHEDULED OUTPUT IS WITHIN THE CURRENT TIME STEP          SUTRA........64100
C              AND THE SCHEDULE HAS NOT BEEN EXHAUSTED.)                 SUTRA........64200
!            DO WHILE ((DIT.GE.STEP).AND.(LCNT(NFLO).LE.LENSCH))          SUTRA........64300
C..............IF THE SCHEDULED STEP IS NOT ZERO, PRINT RESULTS.         SUTRA........64400
!               IF (STEP.NE.0D0) THEN                                     SUTRA........64500
!                  IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                      SUTRA........64600
!                     CALL OUTOBS(NFLO,OBSPTS,TIME,STEP,PM1,UM1,          SUTRA........64700
!     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG,BCSFL,BCSTR)     SUTRA........64800
!                  ELSE                                                   SUTRA........64900
!                     CALL OUTOBC(NFLO,OBSPTS,TIME,STEP,PM1,UM1,          SUTRA........65000
!     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG,BCSFL,BCSTR)     SUTRA........65100
!                  END IF                                                 SUTRA........65200
C.................IF END OF TIME STEP HAS JUST BEEN PRINTED, SET FLAG.   SUTRA........65300
!                  IF (DIT.EQ.STEP) TSPRTD = .TRUE.                       SUTRA........65400
!               END IF                                                    SUTRA........65500
C..............GO TO THE NEXT ENTRY IN THE SCHEDULE, IF THERE IS ONE.    SUTRA........65600
!               IF (LCNT(NFLO).LT.LENSCH) THEN                            SUTRA........65700
!                  DENOB(NFLO)%NENT => DENOB(NFLO)%NENT%NENT              SUTRA........65800
!                  TIME = DENOB(NFLO)%NENT%DVALU1                         SUTRA........65900
!                  STEP = DENOB(NFLO)%NENT%DVALU2                         SUTRA........66000
!               END IF                                                    SUTRA........66100
C..............INCREMENT THE COUNTER.                                    SUTRA........66200
!               LCNT(NFLO) = LCNT(NFLO) + 1                               SUTRA........66300
!            END DO                                                       SUTRA........66400
!         END IF                                                          SUTRA........66500
C........IF THIS IS THE INITIAL OR FINAL CONDITION, PRINT IT IF IT       SUTRA........66600
C           HAS NOT ALREADY BEEN PRINTED.                                SUTRA........66700
!         IF (PRNDEF.AND.(.NOT.TSPRTD)) THEN                              SUTRA........66800
!            IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                            SUTRA........66900
!               CALL OUTOBS(NFLO,OBSPTS,TSEC,DIT,PM1,UM1,PVEC,UVEC,       SUTRA........67000
!     1            TITLE1,TITLE2,IN,LREG,BCSFL,BCSTR)                     SUTRA........67100
!            ELSE                                                         SUTRA........67200
!               CALL OUTOBC(NFLO,OBSPTS,TSEC,DIT,PM1,UM1,PVEC,UVEC,       SUTRA........67300
!     1            TITLE1,TITLE2,IN,LREG,BCSFL,BCSTR)                     SUTRA........67400
!            END IF                                                       SUTRA........67500
!         END IF                                                          SUTRA........67600
 !7650 CONTINUE                                                           SUTRA........67700
C                                                                        SUTRA........67800
C.....STORE RESULTS FOR POSSIBLE RESTART OF SIMULATION EACH              SUTRA........67900
C        ISTORE TIME STEPS AND AFTER LAST TIME STEP, THEN GO             SUTRA........68000
C        TO NEXT TIME STEP                                               SUTRA........68100
!      IF (IERR.EQ.0) THEN                                                SUTRA........68200
!         IF ((K4.NE.-1).AND.(ISTORE.NE.0).AND.((ISTOP.NE.0).OR.          SUTRA........68300
!     1      (MOD(IT,ISTORE).EQ.0)))                                      SUTRA........68400
!     2      CALL OUTRST(PVEC,UVEC,PM1,UM1,CS1,RCIT,SW,QIN,PBC,           SUTRA........68500
!     3         UIN,UBC,QUIN,IBCPBC,IBCUBC,IBCSOP,IBCSOU,                 SUTRA........68600
!     4         IIDPBC,IIDUBC,IIDSOP,IIDSOU)                              SUTRA........68700
!         IF (ISTOP.EQ.0) GOTO 1000                                       SUTRA........68800
!      END IF                                                             SUTRA........68900
C                                                                        SUTRA........69000
C ********************************************************************** SUTRA........69100
C.....END TIME STEP **************************************************** SUTRA........69200
C ********************************************************************** SUTRA........69300
C                                                                        SUTRA........69400
C                                                                        SUTRA........69500
C.....DEALLOCATE ARRAY DENOB                                             SUTRA........69600
      DEALLOCATE (DENOB)                                                 SUTRA........69700
C                                                                        SUTRA........69800
C.....COMPLETE OUTPUT AND TERMINATE SIMULATION                           SUTRA........69900
!      IF (IERRP.NE.0) THEN                                               SUTRA........70000
!         ERRCOD = 'SOL-1'                                                SUTRA........70100
!         CHERR(1) = 'P'                                                  SUTRA........70200
!         CHERR(2) = SOLWRD(KSOLVP)                                       SUTRA........70300
!         INERR(1) = IERRP                                                SUTRA........70400
!         INERR(2) = ITRSP                                                SUTRA........70500
!         RLERR(1) = ERRP                                                 SUTRA........70600
!         RLERR(2) = TOLP                                                 SUTRA........70700
!         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA........70800
!      ELSE IF (IERRU.NE.0) THEN                                          SUTRA........70900
!         ERRCOD = 'SOL-1'                                                SUTRA........71000
!         CHERR(1) = 'U'                                                  SUTRA........71100
!         CHERR(2) = SOLWRD(KSOLVU)                                       SUTRA........71200
!         INERR(1) = IERRU                                                SUTRA........71300
!         INERR(2) = ITRSU                                                SUTRA........71400
!         RLERR(1) = ERRU                                                 SUTRA........71500
!         RLERR(2) = TOLU                                                 SUTRA........71600
!         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA........71700
!      END IF                                                             SUTRA........71800
C                                                                        SUTRA........71900
!      IF(ISTORE.GT.0) WRITE(K3,8100)                                     SUTRA........72000
! 8100 FORMAT(//////11X,'*** LAST SOLUTION HAS BEEN STORED ',             SUTRA........72100
!     1   'IN THE RESTART DATA FILE ***')                                 SUTRA........72200
C                                                                        SUTRA........72300
C.....OUTPUT END OF SIMULATION MESSAGE AND RETURN TO MAIN FOR STOP       SUTRA........72400
      IF(ISTOP.EQ.-1) THEN                                               SUTRA........72500
!         ERRCOD = 'CON-1'                                                SUTRA........72600
!         IF (ME.EQ.1) THEN                                               SUTRA........72700
!            CDUM80 = 'temperature'                                       SUTRA........72800
!            LENC = 11                                                    SUTRA........72900
!         ELSE                                                            SUTRA........73000
!            CDUM80 = 'concentration'                                     SUTRA........73100
!            LENC = 13                                                    SUTRA........73200
!         END IF                                                          SUTRA........73300
!         IF (IGOI.EQ.1) THEN                                             SUTRA........73400
!            CHERR(1) = 'pressure'                                        SUTRA........73500
!            LENC = 8                                                     SUTRA........73600
!         ELSE IF (IGOI.EQ.2) THEN                                        SUTRA........73700
!            CHERR(1) = CDUM80                                            SUTRA........73800
!         ELSE IF (IGOI.EQ.3) THEN                                        SUTRA........73900
!            CHERR(1) = 'pressure and ' // CDUM80(1:LENC)                 SUTRA........74000
!            LENC = 13 + LENC                                             SUTRA........74100
!         END IF                                                          SUTRA........74200
!         INERR(1) = IPWORS                                               SUTRA........74300
!         INERR(2) = IUWORS                                               SUTRA........74400
!         INERR(3) = ITER                                                 SUTRA........74500
!         INERR(4) = LENC                                                 SUTRA........74600
!         RLERR(1) = RPM                                                  SUTRA........74700
!         RLERR(2) = RPMAX                                                SUTRA........74800
!         RLERR(3) = RUM                                                  SUTRA........74900
!         RLERR(4) = RUMAX                                                SUTRA........75000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        SUTRA........75100
	   RETURN
!      ELSE IF (ISTOP.EQ.2) THEN                                          SUTRA........75200
!         WRITE(K3,8450)                                                  SUTRA........75300
! 8450    FORMAT(////////11X,'SUTRA SIMULATION TERMINATED AT',            SUTRA........75400
!     1      ' COMPLETION OF TIME PERIOD'/                                SUTRA........75500
!     2                  11X,'***** ********** ********** **',            SUTRA........75600
!     3      ' ********** ** **** ******')                                SUTRA........75700
!      ELSE                                                               SUTRA........75800
!         WRITE(K3,8550)                                                  SUTRA........75900
! 8550    FORMAT(////////11X,'SUTRA SIMULATION TERMINATED AT',            SUTRA........76000
!     1      ' COMPLETION OF TIME STEPS'/                                 SUTRA........76100
!     2                  11X,'***** ********** ********** **',            SUTRA........76200
!     3      ' ********** ** **** *****')                                 SUTRA........76300
      END IF                                                             SUTRA........76400
C                                                                        SUTRA........76500
 !     IF (KSCRN.EQ.1) WRITE(*,8590)                                      SUTRA........76600
 !     WRITE(K00,8590)                                                    SUTRA........76700
 !8590 FORMAT(/1X,'S I M U L A T I O N   E N D E D'/)                     SUTRA........76800
      RETURN                                                             SUTRA........76900
C                                                                        SUTRA........77000
      END                                                                SUTRA........77100
C                                                                        SUTRA........77200
C     SUBROUTINE        T  E  N  S  Y  M           SUTRA VERSION 2.2     TENSYM.........100
C                                                                        TENSYM.........200
C *** PURPOSE :                                                          TENSYM.........300
C ***  TO TRANSFORM A DIAGONAL MATRIX TO A NEW COORDINATE SYSTEM.        TENSYM.........400
C ***  [T] IS THE DIAGONAL MATRIX EXPRESSED IN THE FIRST (INPUT)         TENSYM.........500
C ***  COORDINATE SYSTEM; [P] IS THE (SYMMETRIC) MATRIX EXPRESSED        TENSYM.........600
C ***  IN THE SECOND (OUTPUT) COORDINATE SYSTEM; AND [Q] IS THE          TENSYM.........700
C ***  THE TRANSFORMATION MATRIX.                                        TENSYM.........800
C                                                                        TENSYM.........900
C     SUBROUTINE        T  E  R  S  E  Q           SUTRA VERSION 2.2     TERSEQ.........100
C                                                                        TERSEQ.........200
C *** PURPOSE :                                                          TERSEQ.........300
C ***  TO GRACEFULLY TERMINATE A SUTRA RUN BY DEALLOCATING THE MAIN      TERSEQ.........400
C ***  ALLOCATABLE ARRAYS AND CLOSING ALL FILES.                         TERSEQ.........500
C                                                                        TERSEQ.........600
      SUBROUTINE TERSEQ()                                                TERSEQ.........700
      USE ALLARR                                                         TERSEQ.........800
      USE BCSDEF                                                         TERSEQ.........900
      USE FINDEF                                                         TERSEQ........1000
      USE SCHDEF                                                         TERSEQ........1100
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                TERSEQ........1200
      CHARACTER CDUM*1                                                   TERSEQ........1300
      LOGICAL ALCBCS,ALCFIN,ALCOBS                                       TERSEQ........1400
      COMMON /ALC/ ALCBCS,ALCFIN,ALCOBS                                  TERSEQ........1500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 TERSEQ........1600
     1   K10,K11,K12,K13                                                 TERSEQ........1700
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                TERSEQ........1800
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            TERSEQ........1900
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      TERSEQ........2000
C                                                                        TERSEQ........2100
C.....TERMINATION SEQUENCE: DEALLOCATE ARRAYS, CLOSE FILES, AND STOP     TERSEQ........2200
      IF (ALLO1) THEN                                                    TERSEQ........2300
         DEALLOCATE(PITER,UITER,PM1,DPDTITR,UM1,UM2,PVEL,SL,SR,X,Y,Z,    TERSEQ........2400
     1      VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,QIN,UIN,QUIN,QINITR,    TERSEQ........2500
     2      RCIT,RCITM1,GNUP1,GNUU1)                                     TERSEQ........2600
         DEALLOCATE(PVEC,UVEC)                                           TERSEQ........2700
         DEALLOCATE(ALMAX,ALMIN,ATMAX,ATMIN,VMAG,VANG1,PERMXX,PERMXY,    TERSEQ........2800
     1      PERMYX,PERMYY,PANGL1)                                        TERSEQ........2900
         DEALLOCATE(ALMID,ATMID,VANG2,PERMXZ,PERMYZ,PERMZX,PERMZY,       TERSEQ........3000
     1      PERMZZ,PANGL2,PANGL3)                                        TERSEQ........3100
         DEALLOCATE(PBC,UBC,QPLITR)                                      TERSEQ........3200
         DEALLOCATE(GXSI,GETA,GZET)                                      TERSEQ........3300
         DEALLOCATE(B)                                                   TERSEQ........3400
         DEALLOCATE(IN,IQSOP,IQSOU,IPBC,IUBC,NREG,LREG,JA)               TERSEQ........3500
c RBW begin change
         DEALLOCATE(IIDPBC, IIDUBC, IIDSOP, IIDSOU)
c RBW end change
         DEALLOCATE(IBCPBC,IBCUBC,IBCSOP,IBCSOU)                         TERSEQ........3600
c RBW begin change
         DEALLOCATE(BCSFL, BCSTR)
c RBW end change
         DEALLOCATE(OBSPTS)                                              TERSEQ........3700
      END IF                                                             TERSEQ........3800
!      IF (ALLO2) THEN                                                    TERSEQ........3900
!         DEALLOCATE(PMAT,UMAT,FWK)                                       TERSEQ........4000
!         DEALLOCATE(IWK)                                                 TERSEQ........4100
!      END IF                                                             TERSEQ........4200
c RBW begin change
	if (ALLOCATED(CIDBCS)) DEALLOCATE(CIDBCS)
c RBW end change
      IF (ALLO3) THEN                                                    TERSEQ........4300
         DEALLOCATE(IA)                                                  TERSEQ........4400
      END IF                                                             TERSEQ........4500
      IF (ALCFIN) THEN                                                   TERSEQ........4600
         DEALLOCATE(NKS,KLIST,FNAIN)                                     TERSEQ........4700
      END IF                                                             TERSEQ........4800
      IF (ALCBCS) THEN                                                   TERSEQ........4900
         DEALLOCATE(IUNIB,FNAMB)                                         TERSEQ........5000
      END IF                                                             TERSEQ........5100
      IF (ALLOCATED(SCHDLS)) DEALLOCATE(SCHDLS)                          TERSEQ........5200
      IF (ALLOCATED(OFP)) DEALLOCATE(OFP)                                TERSEQ........5300
      IF (ALCOBS) DEALLOCATE(FNAMO)                                      TERSEQ........5400
      IF (ALLOCATED(ONCK78)) DEALLOCATE(ONCK78)                          TERSEQ........5500
C.....ARRAY IUNIO WILL BE DEALLOCATED AFTER THE OBSERVATION OUTPUT       TERSEQ........5600
C        FILES ARE CLOSED                                                TERSEQ........5700
      CLOSE(K00)                                                         TERSEQ........5800
      CLOSE(K0)                                                          TERSEQ........5900
      CLOSE(K1)                                                          TERSEQ........6000
      CLOSE(K2)                                                          TERSEQ........6100
      CLOSE(K3)                                                          TERSEQ........6200
      CLOSE(K4)                                                          TERSEQ........6300
      CLOSE(K5)                                                          TERSEQ........6400
      CLOSE(K6)                                                          TERSEQ........6500
      CLOSE(K7)                                                          TERSEQ........6600
      CLOSE(K8)                                                          TERSEQ........6700
!      DO 8000 NFO=1,NFLOMX                                               TERSEQ........6800
!         CLOSE(IUNIO(NFO))                                               TERSEQ........6900
! 8000 CONTINUE                                                           TERSEQ........7000
      IF (ALCOBS) DEALLOCATE(IUNIO)                                      TERSEQ........7100
! RBW begin change
!      IF ((KSCRN.EQ.1).AND.(KPAUSE.EQ.1)) THEN                           TERSEQ........7200
!         WRITE(*,9990)                                                   TERSEQ........7300
! 9990    FORMAT(/' Press ENTER to exit ...')                             TERSEQ........7400
!         READ(*,'(A1)') CDUM                                             TERSEQ........7500
!      END IF                                                             TERSEQ........7600
!      STOP ' '                                                           TERSEQ........7700
! rbw end change
C                                                                        TERSEQ........7800
      RETURN                                                             TERSEQ........7900
      END                                                                TERSEQ........8000
C                                                                        TERSEQ........8100
C     FUNCTION          T  I  M  E  T  S           SUTRA VERSION 2.2     TIMETS.........100
C                                                                        TIMETS.........200
C *** PURPOSE :                                                          TIMETS.........300
C ***  TO RETURN THE TIME ASSOCIATED WITH A GIVEN TIME STEP.  IF THE     TIMETS.........400
C ***  SPECIFIED TIME STEP IS GREATER THAN THE MAXIMUM, A VALUE OF       TIMETS.........500
C ***  +HUGE(1D0) (THE LARGEST NUMBER THAT CAN BE REPRESENTED IN DOUBLE  TIMETS.........600
C ***  PRECISION) IS RETURNED.  IF THE SPECIFIED TIME STEP IS LESS THAN  TIMETS.........700
C ***  ZERO, A VALUE OF -HUGE(1D0) IS RETURNED.  IF THE TIME STEP        TIMETS.........800
C ***  SCHEDULE HAS NOT YET BEEN DEFINED, A VALUE OF ZERO IS RETURNED.   TIMETS.........900
C                                                                        TIMETS........1000
      DOUBLE PRECISION FUNCTION TIMETS(NSTEP)                            TIMETS........1100
      USE LLDEF                                                          TIMETS........1200
      USE SCHDEF                                                         TIMETS........1300
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                TIMETS........1400
      TYPE (LLD), POINTER :: DEN                                         TIMETS........1500
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    TIMETS........1600
C                                                                        TIMETS........1700
      IF (ISCHTS.EQ.0) THEN                                              TIMETS........1800
         TIMETS = 0D0                                                    TIMETS........1900
         RETURN                                                          TIMETS........2000
      END IF                                                             TIMETS........2100
C                                                                        TIMETS........2200
      NSMAX = SCHDLS(ISCHTS)%LLEN - 1                                    TIMETS........2300
C                                                                        TIMETS........2400
      IF (NSTEP.LT.0) THEN                                               TIMETS........2500
         TIMETS = -HUGE(1D0)                                             TIMETS........2600
         RETURN                                                          TIMETS........2700
      ELSE IF (NSTEP.GT.NSMAX) THEN                                      TIMETS........2800
         TIMETS = +HUGE(1D0)                                             TIMETS........2900
         RETURN                                                          TIMETS........3000
      END IF                                                             TIMETS........3100
C                                                                        TIMETS........3200
      DEN => SCHDLS(ISCHTS)%SLIST                                        TIMETS........3300
      DO 100 NS=1,NSTEP                                                  TIMETS........3400
         DEN => DEN%NENT                                                 TIMETS........3500
  100 CONTINUE                                                           TIMETS........3600
      TIMETS = DEN%DVALU1                                                TIMETS........3700
C                                                                        TIMETS........3800
      RETURN                                                             TIMETS........3900
      END                                                                TIMETS........4000
C                                                                        TIMETS........4100
C     SUBROUTINE        Z  E  R  O                 SUTRA VERSION 2.2     ZERO...........100
C                                                                        ZERO...........200
C *** PURPOSE :                                                          ZERO...........300
C ***  TO FILL AN ARRAY WITH A CONSTANT VALUE (USUALLY ZERO).            ZERO...........400
C                                                                        ZERO...........500
      SUBROUTINE ZERO(A,IADIM,FILL)                                      ZERO...........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ZERO...........700
      DIMENSION A(IADIM)                                                 ZERO...........800
C                                                                        ZERO...........900
C.....FILL ARRAY A WITH VALUE IN VARIABLE 'FILL'                         ZERO..........1000
      DO 10 I=1,IADIM                                                    ZERO..........1100
   10 A(I)=FILL                                                          ZERO..........1200
C                                                                        ZERO..........1300
C                                                                        ZERO..........1400
      RETURN                                                             ZERO..........1500
      END                                                                ZERO..........1600
C RBW
      SUBROUTINE SUTRA_LABELS22(FLABELS)
!DEC$ attributes dllexport :: SUTRA_LABELS22
C FLABELS CONTAINS THE NAMES OF THE BOUNDARY FEATURES. 
	CHARACTER(*) FLABELS
      FLABELS  = 'fluid src/sink                          energy/sol src
     &/sink                     spec. pressure                          
     &spec. conc/temp                         '
      RETURN
      END
C RBW
