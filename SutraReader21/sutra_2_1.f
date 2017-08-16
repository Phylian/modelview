C     MAIN PROGRAM       S U T R A _ M A I N       SUTRA VERSION 2.1     SUTRA_MAIN.....100
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
C|            02-4231, 250p. (Version of February 7, 2008)             | SUTRA_MAIN....6200
C|                                                                     | SUTRA_MAIN....6300
C|                                                                     | SUTRA_MAIN....6400
C|                                                                     | SUTRA_MAIN....6500
C|       Users who wish to be notified of updates of the SUTRA         | SUTRA_MAIN....6600
C|       code and documentation may be added to the mailing list       | SUTRA_MAIN....6700
C|       by sending a request to :                                     | SUTRA_MAIN....6800
C|                                                                     | SUTRA_MAIN....6900
C|                           SUTRA Support                             | SUTRA_MAIN....7000
C|                       U.S. Geological Survey                        | SUTRA_MAIN....7100
C|                        431 National Center                          | SUTRA_MAIN....7200
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
C|    *  Original Release: 1984                                   *    | SUTRA_MAIN....9000
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    | SUTRA_MAIN....9100
C|    *                                                           *    | SUTRA_MAIN....9200
C|    *  First Revision: June 1990, Version V06902D               *    | SUTRA_MAIN....9300
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    | SUTRA_MAIN....9400
C|    *                                                           *    | SUTRA_MAIN....9500
C|    *  Second Revision: September 1997, Version V09972D         *    | SUTRA_MAIN....9600
C|    *  by: C.I. Voss and David Boldt, U.S. Geological Survey    *    | SUTRA_MAIN....9700
C|    *                                                           *    | SUTRA_MAIN....9800
C|    *  Third Revision: September 2003, Version 2D3D.1           *    | SUTRA_MAIN....9900
C|    *  by: A.M. Provost & C.I. Voss, U.S. Geological Survey     *    | SUTRA_MAIN...10000
C|    *                                                           *    | SUTRA_MAIN...10100
C|    *  Fourth Revision: February 2008, Version 2.1              *    | SUTRA_MAIN...10200
C|    *  by: A.M. Provost & C.I. Voss, U.S. Geological Survey     *    | SUTRA_MAIN...10300
C|    *                                                           *    | SUTRA_MAIN...10400
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN...10500
C|                                                                     | SUTRA_MAIN...10600
C|                                                                     | SUTRA_MAIN...10700
C|_____________________________________________________________________| SUTRA_MAIN...10800
C                                                                        SUTRA_MAIN...10900
C                                                                        SUTRA_MAIN...11000
C                                                                        SUTRA_MAIN...11100
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
      SUBROUTINE SUTRA_21(IERROR, ISTART, IBOUSZ, 
     &        IBNODE, NFEAT, ISTEADYFLOW, ISTEADYTRANSPORT,
     &        ElementValues, IElementValueCount, Incidence, NodeValues, 
     &        INodeValueCount, MeshInfo, INPFILE)     
!DEC$ attributes dllexport :: SUTRA_21
!      PROGRAM SUTRA_MAIN                                                 SUTRA_MAIN...11200
      USE ALLARR                                                         SUTRA_MAIN...11300
      USE PTRDEF                                                         SUTRA_MAIN...11400
      USE EXPINT                                                         SUTRA_MAIN...11500
      USE SCHDEF                                                         SUTRA_MAIN...11600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTRA_MAIN...11700
      PARAMETER (NCOLMX=9)                                               SUTRA_MAIN...11800
C                                                                        SUTRA_MAIN...11900
C.....PROGRAMMERS SET SUTRA VERSION NUMBER HERE (8 CHARACTERS MAXIMUM)   SUTRA_MAIN...12000
      CHARACTER*8, PARAMETER :: VERN='2.1'                               SUTRA_MAIN...12100
C                                                                        SUTRA_MAIN...12200
      CHARACTER*8 VERNUM, VERNIN                                         SUTRA_MAIN...12300
      CHARACTER*1 TITLE1(80),TITLE2(80)                                  SUTRA_MAIN...12400
      CHARACTER*80 SIMULA(5),MSHTYP(2),LAYNOR(2),SIMSTR,MSHSTR,LAYSTR    SUTRA_MAIN...12500
      CHARACTER*80 CUNSAT, CSSFLO ,CSSTRA, CREAD                         SUTRA_MAIN...12600
      CHARACTER*80 UNSSTR, SSFSTR ,SSTSTR, RDSTR                         SUTRA_MAIN...12700
      CHARACTER*80 UNAME,FNAME,FNINP,FNICS                               SUTRA_MAIN...12800
      CHARACTER*80 ERRCOD,CHERR(10)                                      SUTRA_MAIN...12900
      CHARACTER*40 SOLNAM(0:10)                                          SUTRA_MAIN...13000
      CHARACTER*10 SOLWRD(0:10)                                          SUTRA_MAIN...13100
      CHARACTER*10 ADSMOD                                                SUTRA_MAIN...13200
      CHARACTER INTFIL*1000                                              SUTRA_MAIN...13300
      INTEGER RMVDIM,IMVDIM,CMVDIM,PMVDIM                                SUTRA_MAIN...13400
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                SUTRA_MAIN...13500
      LOGICAL ONCEFO                                                     SUTRA_MAIN...13600
      DIMENSION FNAME(0:8),IUNIT(0:8)                                    SUTRA_MAIN...13700
      DIMENSION FNAIN(2,20)                                              SUTRA_MAIN...13800
      DIMENSION INERR(10), RLERR(10)                                     SUTRA_MAIN...13900
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             SUTRA_MAIN...14000
      DIMENSION NKS(2), KLIST(2,20)                                      SUTRA_MAIN...14100
      DIMENSION KTYPE(2)                                                 SUTRA_MAIN...14200
      COMMON /CLAY/ LAYSTR                                               SUTRA_MAIN...14300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SUTRA_MAIN...14400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SUTRA_MAIN...14500
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  SUTRA_MAIN...14600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTRA_MAIN...14700
     1   NSOP,NSOU,NBCN                                                  SUTRA_MAIN...14800
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        SUTRA_MAIN...14900
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        SUTRA_MAIN...15000
      COMMON /FNAINS/ FNAIN                                              SUTRA_MAIN...15100
      COMMON /FNAMES/ UNAME,FNAME                                        SUTRA_MAIN...15200
      COMMON /FO/ONCEFO                                                  SUTRA_MAIN...15300
      COMMON /FUNINS/ NKS,KLIST                                          SUTRA_MAIN...15400
      COMMON /FUNITA/ IUNIT                                              SUTRA_MAIN...15500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SUTRA_MAIN...15600
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      SUTRA_MAIN...15700
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            SUTRA_MAIN...15800
      COMMON /ITSOLR/ TOLP,TOLU                                          SUTRA_MAIN...15900
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        SUTRA_MAIN...16000
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTRA_MAIN...16100
     1   KSCRN,KPAUSE                                                    SUTRA_MAIN...16200
      COMMON /MODSOR/ ADSMOD                                             SUTRA_MAIN...16300
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      SUTRA_MAIN...16400
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SUTRA_MAIN...16500
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SUTRA_MAIN...16600
      COMMON /PLT1/ ONCEK5, ONCEK6, ONCEK7, ONCEK8                       SUTRA_MAIN...16700
      COMMON /SCH/ NSCH,ISCHTS                                           SUTRA_MAIN...16800
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      SUTRA_MAIN...16900
      COMMON /SOLVN/ NSLVRS                                              SUTRA_MAIN...17000
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       SUTRA_MAIN...17100
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SUTRA_MAIN...17200
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  SUTRA_MAIN...17300
      COMMON /VER/ VERNUM, VERNIN                                        SUTRA_MAIN...17400
c rbw begin
	INTEGER IBOUSZ,NFEAT
	INTEGER IBNODE(IBOUSZ)
	REAL (KIND = 4) ElementValues(IElementValueCount)
	REAL (KIND = 4) NodeValues(INodeValueCount)
	INTEGER  :: Incidence(*)
	INTEGER IncidenceIndex
      CHARACTER(*) INPFILE
	INTEGER MeshInfo(3)
c rbw end
C....."NSLVRS" AND THE ARRAYS "SOLWRD" AND "SOLNAM" ARE INITIALIZED      SUTRA_MAIN...17500
C        IN THE BLOCK-DATA SUBPROGRAM "BDINIT"                           SUTRA_MAIN...17600
C                                                                        SUTRA_MAIN...17700
C                                                                        SUTRA_MAIN...17800
C.....COPY PARAMETER VERN (SUTRA VERSION NUMBER) TO VARIABLE VERNUM,     SUTRA_MAIN...17900
C        WHICH IS PASSED THROUGH COMMON BLOCK VER.                       SUTRA_MAIN...18000
      VERNUM = VERN                                                      SUTRA_MAIN...18100
C                                                                        SUTRA_MAIN...18200
C.....SET THE ALLOCATION FLAGS TO FALSE                                  SUTRA_MAIN...18300
      IERROR = 0
      ALLO1 = .FALSE.                                                    SUTRA_MAIN...18400
      ALLO2 = .FALSE.                                                    SUTRA_MAIN...18500
      ALLO3 = .FALSE.                                                    SUTRA_MAIN...18600
C                                                                        SUTRA_MAIN...18700
C_______________________________________________________________________ SUTRA_MAIN...18800
C|                                                                     | SUTRA_MAIN...18900
C|  *****************************************************************  | SUTRA_MAIN...19000
C|  *                                                               *  | SUTRA_MAIN...19100
C|  *   **********  M E M O R Y   A L L O C A T I O N  **********   *  | SUTRA_MAIN...19200
C|  *                                                               *  | SUTRA_MAIN...19300
C|  *   The main arrays used by SUTRA are dimensioned dynamically   *  | SUTRA_MAIN...19400
C|  *   in the main program, SUTRA_MAIN.  The amount of storage     *  | SUTRA_MAIN...19500
C|  *   required by these arrays depends on the dimensionality of   *  | SUTRA_MAIN...19600
C|  *   the problem (2D or 3D) and the particular solver(s) used.   *  | SUTRA_MAIN...19700
C|  *                                                               *  | SUTRA_MAIN...19800
C|  *               |---------------------|---------------------|   *  | SUTRA_MAIN...19900
C|  *               |     sum of real     |    sum of integer   |   *  | SUTRA_MAIN...20000
C|  *               |   array dimensions  |   array dimensions  |   *  | SUTRA_MAIN...20100
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...20200
C|  *   | 2D,       | (2*NBI+27)*NN+19*NE |  NN+5*NE+NSOP+NSOU  |   *  | SUTRA_MAIN...20300
C|  *   | direct    |    +3*NBCN+6*NOBS   |    +2*NBCN+NOBS     |   *  | SUTRA_MAIN...20400
C|  *   | solver    |      +2*NSCH+22     |      +3*NSCH+4      |   *  | SUTRA_MAIN...20500
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...20600
C|  *   | 2D,       | 2*NELT+28*NN+19*NE  | NELT+2*NN+5*NE+NSOP |   *  | SUTRA_MAIN...20700
C|  *   | iterative |   +3*NBCN+6*NOBS    |  +NSOU+2*NBCN+NOBS  |   *  | SUTRA_MAIN...20800
C|  *   | solver(s) |   +2*NSCH+NWF+220   |    +3*NSCH+NWI+2    |   *  | SUTRA_MAIN...20900
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...21000
C|  *   | 3D,       | (2*NBI+27)*NN+45*NE |  NN+9*NE+NSOP+NSOU  |   *  | SUTRA_MAIN...21100
C|  *   | direct    |    +3*NBCN+6*NOBS   |    +2*NBCN+NOBS     |   *  | SUTRA_MAIN...21200
C|  *   | solver    |      +2*NSCH+8      |      +3*NSCH+4      |   *  | SUTRA_MAIN...21300
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...21400
C|  *   | 3D,       | 2*NELT+28*NN+45*NE  | NELT+2*NN+9*NE+NSOP |   *  | SUTRA_MAIN...21500
C|  *   | iterative |   +3*NBCN+6*NOBS    |  +NSOU+2*NBCN+NOBS  |   *  | SUTRA_MAIN...21600
C|  *   | solver(s) |   +2*NSCH+NWF+6     |    +3*NSCH+NWI+2    |   *  | SUTRA_MAIN...21700
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...21800
C|  *                                                               *  | SUTRA_MAIN...21900
C|  *               |---------------------|---------------------|   *  | SUTRA_MAIN...22000
C|  *               |  sum of character   |  sum of dimensions  |   *  | SUTRA_MAIN...22100
C|  *               |   array effective   |     of arrays of    |   *  | SUTRA_MAIN...22200
C|  *               |     dimensions      |       pointers      |   *  | SUTRA_MAIN...22300
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...22400
C|  *   | all cases |  73*NOBS + 89*NSCH  |        2*NSCH       |   *  | SUTRA_MAIN...22500
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...22600
C|  *                                                               *  | SUTRA_MAIN...22700
C|  *   Quantities in the tables above are defined in Section 7.3   *  | SUTRA_MAIN...22800
C|  *   of the published documentation (Voss & Provost, 2002,       *  | SUTRA_MAIN...22900
C|  *   USGS Water-Resources Investigations Report 02-4231,         *  | SUTRA_MAIN...23000
C|  *   Version of February 7, 2008).                               *  | SUTRA_MAIN...23100
C|  *                                                               *  | SUTRA_MAIN...23200
C|  *   During each run, SUTRA writes memory usage information to   *  | SUTRA_MAIN...23300
C|  *   the LST output file.                                        *  | SUTRA_MAIN...23400
C|  *                                                               *  | SUTRA_MAIN...23500
C|  *****************************************************************  | SUTRA_MAIN...23600
C|_____________________________________________________________________| SUTRA_MAIN...23700
C                                                                        SUTRA_MAIN...23800
C                                                                        SUTRA_MAIN...23900
C_______________________________________________________________________ SUTRA_MAIN...24000
C|                                                                     | SUTRA_MAIN...24100
C|  *****************************************************************  | SUTRA_MAIN...24200
C|  *                                                               *  | SUTRA_MAIN...24300
C|  *   ***********  F I L E   A S S I G N M E N T S  ***********   *  | SUTRA_MAIN...24400
C|  *                                                               *  | SUTRA_MAIN...24500
C|  *   Unit K0 contains the FORTRAN unit number and filename       *  | SUTRA_MAIN...24600
C|  *   assignments for the various SUTRA input and output files.   *  | SUTRA_MAIN...24700
C|  *   Each line of Unit K0 begins with a file type, followed by   *  | SUTRA_MAIN...24800
C|  *   a unit number and a filename for that type, all in free     *  | SUTRA_MAIN...24900
C|  *   format. Permitted file types are INP, ICS, LST, RST, NOD,   *  | SUTRA_MAIN...25000
C|  *   ELE, OBS, OBC, and SMY. Assignments may be listed in any    *  | SUTRA_MAIN...25100
C|  *   order.  Example ("#" indicates a comment):                  *  | SUTRA_MAIN...25200
C|  *   'INP'  50  'project.inp'   # required                       *  | SUTRA_MAIN...25300
C|  *   'ICS'  55  'project.ics'   # required                       *  | SUTRA_MAIN...25400
C|  *   'LST'  60  'project.lst'   # required                       *  | SUTRA_MAIN...25500
C|  *   'RST'  66  'project.rst'   # required if ISTORE>0           *  | SUTRA_MAIN...25600
C|  *   'NOD'  70  'project.nod'   # optional                       *  | SUTRA_MAIN...25700
C|  *   'ELE'  80  'project.ele'   # optional                       *  | SUTRA_MAIN...25800
C|  *   'OBS'  90  'project.obs'   # optional                       *  | SUTRA_MAIN...25900
C|  *   'OBC'  90  'project.obc'   # optional                       *  | SUTRA_MAIN...26000
C|  *   'SMY'  40  'project.smy'   # optional; defaults to          *  | SUTRA_MAIN...26100
C|  *                              #           filename="SUTRA.SMY" *  | SUTRA_MAIN...26200
C|  *                                                               *  | SUTRA_MAIN...26300
C|  *   Note that the filenames for types OBS and OBC are actually  *  | SUTRA_MAIN...26400
C|  *   root names from which SUTRA will automatically generate     *  | SUTRA_MAIN...26500
C|  *   observation output filenames based on the combinations of   *  | SUTRA_MAIN...26600
C|  *   schedules and output formats that appear in the observation *  | SUTRA_MAIN...26700
C|  *   specifications.  If a unit number of zero is specified for  *  | SUTRA_MAIN...26800
C|  *   a file, SUTRA will automatically assign a valid unit number *  | SUTRA_MAIN...26900
C|  *   to that file.                                               *  | SUTRA_MAIN...27000
C|  *                                                               *  | SUTRA_MAIN...27100
C|  *****************************************************************  | SUTRA_MAIN...27200
C|_____________________________________________________________________| SUTRA_MAIN...27300
C                                                                        SUTRA_MAIN...27400
C.....SET FILENAME AND FORTRAN UNIT NUMBER FOR UNIT K0                   SUTRA_MAIN...27500
      UNAME = INPFILE                                                    SUTRA_MAIN...27600
      K0 = 10                                                            SUTRA_MAIN...27700
C.....INITIALIZE "INSERT" FILE COUNTERS                                  SUTRA_MAIN...27800
      NKS(1) = 0                                                         SUTRA_MAIN...27900
      NKS(2) = 0                                                         SUTRA_MAIN...28000
C.....INITIALIZE NFLOMX TO ZERO NOW IN CASE TERMINATION SEQUENCE IS      SUTRA_MAIN...28100
C        CALLED BEFORE NFLOMX GETS SET.                                  SUTRA_MAIN...28200
      NFLOMX = 0                                                         SUTRA_MAIN...28300
C.....ASSIGN UNIT NUMBERS AND OPEN FILE UNITS FOR THIS SIMULATION,       SUTRA_MAIN...28400
C        EXCEPT OBSERVATION OUTPUT FILES.                                SUTRA_MAIN...28500
      ONCEFO = .FALSE.                                                   SUTRA_MAIN...28600
      CALL FOPEN(IERROR)                                                       SUTRA_MAIN...28700
	IF (IERROR.NE.0) GOTO 9000
C.....STORE INP AND ICS FILENAMES FOR LATER REFERENCE, SINCE THE         SUTRA_MAIN...28800
C        CORRESPONDING ENTRIES IN FNAME MAY BE OVERWRITTEN BY FILE       SUTRA_MAIN...28900
C        INSERTION.                                                      SUTRA_MAIN...29000
      FNINP = FNAME(1)                                                   SUTRA_MAIN...29100
      FNICS = FNAME(2)                                                   SUTRA_MAIN...29200
C                                                                        SUTRA_MAIN...29300
C                                                                        SUTRA_MAIN...29400
C.....OUTPUT BANNER                                                      SUTRA_MAIN...29500
!      WRITE(K3,110) TRIM(VERNUM)                                         SUTRA_MAIN...29600
!  110 FORMAT('1',131('*')////3(132('*')////)////                         SUTRA_MAIN...29700
!     1   47X,' SSSS   UU  UU  TTTTTT  RRRRR     AA  '/                   SUTRA_MAIN...29800
!     2   47X,'SS   S  UU  UU  T TT T  RR  RR   AAAA '/                   SUTRA_MAIN...29900
!     3   47X,'SSSS    UU  UU    TT    RRRRR   AA  AA'/                   SUTRA_MAIN...30000
!     4   47X,'    SS  UU  UU    TT    RR R    AAAAAA'/                   SUTRA_MAIN...30100
!     5   47X,'SS  SS  UU  UU    TT    RR RR   AA  AA'/                   SUTRA_MAIN...30200
!     6   47X,' SSSS    UUUU     TT    RR  RR  AA  AA'/                   SUTRA_MAIN...30300
!     7   7(/),37X,'U N I T E D    S T A T E S   ',                       SUTRA_MAIN...30400
!     8   'G E O L O G I C A L   S U R V E Y'////                         SUTRA_MAIN...30500
!     9   45X,'SUBSURFACE FLOW AND TRANSPORT SIMULATION MODEL'/           SUTRA_MAIN...30600
!     *   //58X,'-SUTRA VERSION ',A,'-'///                                SUTRA_MAIN...30700
!     A   36X,'*  SATURATED-UNSATURATED FLOW AND SOLUTE OR ENERGY',       SUTRA_MAIN...30800
!     B   ' TRANSPORT  *'////4(////132('*')))                             SUTRA_MAIN...30900
C                                                                        SUTRA_MAIN...31000
C_______________________________________________________________________ SUTRA_MAIN...31100
C|                                                                     | SUTRA_MAIN...31200
C|  *****************************************************************  | SUTRA_MAIN...31300
C|  *                                                               *  | SUTRA_MAIN...31400
C|  *   *********  R E A D I N G   I N P U T   D A T A  *********   *  | SUTRA_MAIN...31500
C|  *   *********  A N D   E R R O R   H A N D L I N G  *********   *  | SUTRA_MAIN...31600
C|  *                                                               *  | SUTRA_MAIN...31700
C|  *   SUTRA typically reads input data line by line as follows.   *  | SUTRA_MAIN...31800
C|  *   Subroutine READIF is called to skip over any comment        *  | SUTRA_MAIN...31900
C|  *   lines and read a single line of input data (up to 1000      *  | SUTRA_MAIN...32000
C|  *   characters) into internal file INTFIL. The input data       *  | SUTRA_MAIN...32100
C|  *   are then read from INTFIL. In case of an error, subroutine  *  | SUTRA_MAIN...32200
C|  *   SUTERR is called to report it, and control passes to the    *  | SUTRA_MAIN...32300
C|  *   termination sequence in subroutine TERSEQ.  The variable    *  | SUTRA_MAIN...32400
C|  *   ERRCOD is used to identify the nature of the error and is   *  | SUTRA_MAIN...32500
C|  *   set prior to calling READIF. The variables CHERR, INERR,    *  | SUTRA_MAIN...32600
C|  *   and RLERR can be used to send character, integer, or real   *  | SUTRA_MAIN...32700
C|  *   error information to subroutine SUTERR.                     *  | SUTRA_MAIN...32800
C|  *   Example from the main program:                              *  | SUTRA_MAIN...32900
C|  *                                                               *  | SUTRA_MAIN...33000
C|  *   ERRCOD = 'REA-INP-3'                                        *  | SUTRA_MAIN...33100
C|  *   CALL READIF(K1, INTFIL, ERRCOD)                             *  | SUTRA_MAIN...33200
C|  *   READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,             *  | SUTRA_MAIN...33300
C|  *  1   NSOP,NSOU,NOBS                                           *  | SUTRA_MAIN...33400
C|  *   IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR) *  | SUTRA_MAIN...33500
C|  *                                                               *  | SUTRA_MAIN...33600
C|  *****************************************************************  | SUTRA_MAIN...33700
C|_____________________________________________________________________| SUTRA_MAIN...33800
C                                                                        SUTRA_MAIN...33900
C.....INPUT DATASET 1:  OUTPUT HEADING                                   SUTRA_MAIN...34000
      ERRCOD = 'REA-INP-1'                                               SUTRA_MAIN...34100
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    SUTRA_MAIN...34200
	IF (IERROR.NE.0) GOTO 9000
      READ(INTFIL,117,IOSTAT=INERR(1)) TITLE1                            SUTRA_MAIN...34300
      IF (INERR(1).NE.0) THEN 
	    CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     SUTRA_MAIN...34400
	    GOTO 9000
	ENDIF
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    SUTRA_MAIN...34500
	IF (IERROR.NE.0) GOTO 9000
      READ(INTFIL,117,IOSTAT=INERR(1)) TITLE2                            SUTRA_MAIN...34600
      IF (INERR(1).NE.0) THEN 
	   CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...34700
	   GOTO 9000
	ENDIF
  117 FORMAT(80A1)                                                       SUTRA_MAIN...34800
C                                                                        SUTRA_MAIN...34900
C.....INPUT DATASET 2A:  SIMULATION TYPE (TYPE OF TRANSPORT)             SUTRA_MAIN...35000
C        (SET ME=-1 FOR SOLUTE TRANSPORT, ME=+1 FOR ENERGY TRANSPORT)    SUTRA_MAIN...35100
      ERRCOD = 'REA-INP-2A'                                              SUTRA_MAIN...35200
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    SUTRA_MAIN...35300
	IF (IERROR.NE.0) GOTO 9000
      READ(INTFIL,*,IOSTAT=INERR(1)) SIMSTR                              SUTRA_MAIN...35400
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                SUTRA_MAIN...35500
	   GOTO 9000
	ENDIF
      CALL PRSWDS(SIMSTR, ' ', 5, SIMULA, NWORDS)                        SUTRA_MAIN...35600
      IF(SIMULA(1).NE.'SUTRA     ') THEN                                 SUTRA_MAIN...35700
         ERRCOD = 'INP-2A-1'                                             SUTRA_MAIN...35800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...35900
	   GOTO 9000
      END IF                                                             SUTRA_MAIN...36000
      IF (SIMULA(2).EQ.'VERSION   ') THEN                                SUTRA_MAIN...36100
         VERNIN = SIMULA(3)                                              SUTRA_MAIN...36200
         IF (VERNIN.EQ.'2D3D.1 ') THEN                                   SUTRA_MAIN...36300
            VERNIN = '2.0'                                               SUTRA_MAIN...36400
         ELSE IF ((VERNIN.NE.'2.0 ').AND.(VERNIN.NE.'2.1 ')) THEN        SUTRA_MAIN...36500
            ERRCOD = 'INP-2A-4'                                          SUTRA_MAIN...36600
            CHERR(1) = VERNIN                                            SUTRA_MAIN...36700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...36800
	      GOTO 9000
         END IF                                                          SUTRA_MAIN...36900
         IOFF = 2                                                        SUTRA_MAIN...37000
      ELSE                                                               SUTRA_MAIN...37100
         VERNIN = '2.0'                                                  SUTRA_MAIN...37200
         IOFF = 0                                                        SUTRA_MAIN...37300
      END IF                                                             SUTRA_MAIN...37400
      IF(SIMULA(2+IOFF).EQ.'SOLUTE    ') GOTO 120                        SUTRA_MAIN...37500
      IF(SIMULA(2+IOFF).EQ.'ENERGY    ') GOTO 140                        SUTRA_MAIN...37600
      IF (IOFF.EQ.0) THEN                                                SUTRA_MAIN...37700
         ERRCOD = 'INP-2A-2'                                             SUTRA_MAIN...37800
      ELSE                                                               SUTRA_MAIN...37900
         ERRCOD = 'INP-2A-3'                                             SUTRA_MAIN...38000
      END IF                                                             SUTRA_MAIN...38100
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           SUTRA_MAIN...38200
	GOTO 9000
  120 ME=-1                                                              SUTRA_MAIN...38300
!      WRITE(K3,130)                                                      SUTRA_MAIN...38400
!  130 FORMAT('1'//132('*')///20X,'* * * * *   S U T R A   S O L U ',     SUTRA_MAIN...38500
!     1   'T E   T R A N S P O R T   S I M U L A T I O N   * * * * *'//   SUTRA_MAIN...38600
!     2   /132('*')/)                                                     SUTRA_MAIN...38700
      GOTO 160                                                           SUTRA_MAIN...38800
  140 ME=+1                                                              SUTRA_MAIN...38900
!      WRITE(K3,150)                                                      SUTRA_MAIN...39000
!  150 FORMAT('1'//132('*')///20X,'* * * * *   S U T R A   E N E R ',     SUTRA_MAIN...39100
!     1   'G Y   T R A N S P O R T   S I M U L A T I O N   * * * * *'//   SUTRA_MAIN...39200
!     2   /132('*')/)                                                     SUTRA_MAIN...39300
  160 CONTINUE                                                           SUTRA_MAIN...39400
C                                                                        SUTRA_MAIN...39500
C.....INPUT DATASET 2B:  MESH STRUCTURE                                  SUTRA_MAIN...39600
      ERRCOD = 'REA-INP-2B'                                              SUTRA_MAIN...39700
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    SUTRA_MAIN...39800
	IF (IERROR.NE.0) GOTO 9000
      READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR                              SUTRA_MAIN...39900
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                SUTRA_MAIN...40000
	   GOTO 9000
	ENDIF
      CALL PRSWDS(MSHSTR, ' ', 2, MSHTYP, NWORDS)                        SUTRA_MAIN...40100
C.....KTYPE SET ACCORDING TO THE TYPE OF FINITE-ELEMENT MESH:            SUTRA_MAIN...40200
C        2D MESH          ==>   KTYPE(1) = 2                             SUTRA_MAIN...40300
C        3D MESH          ==>   KTYPE(1) = 3                             SUTRA_MAIN...40400
C        IRREGULAR MESH   ==>   KTYPE(2) = 0                             SUTRA_MAIN...40500
C        LAYERED MESH     ==>   KTYPE(2) = 1                             SUTRA_MAIN...40600
C        REGULAR MESH     ==>   KTYPE(2) = 2                             SUTRA_MAIN...40700
C        BLOCKWISE MESH   ==>   KTYPE(2) = 3                             SUTRA_MAIN...40800
      IF (MSHTYP(1).EQ.'2D        ') THEN                                SUTRA_MAIN...40900
         KTYPE(1) = 2                                                    SUTRA_MAIN...41000
      ELSE IF (MSHTYP(1).EQ.'3D        ') THEN                           SUTRA_MAIN...41100
         KTYPE(1) = 3                                                    SUTRA_MAIN...41200
      ELSE                                                               SUTRA_MAIN...41300
         ERRCOD = 'INP-2B-1'                                             SUTRA_MAIN...41400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...41500
	   GOTO 9000
      END IF                                                             SUTRA_MAIN...41600
      IF ((MSHTYP(2).EQ.'REGULAR   ').OR.                                SUTRA_MAIN...41700
     1    (MSHTYP(2).EQ.'BLOCKWISE ')) THEN                              SUTRA_MAIN...41800
         ERRCOD = 'REA-INP-2B'                                           SUTRA_MAIN...41900
         IF (KTYPE(1).EQ.2) THEN                                         SUTRA_MAIN...42000
            READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR, NN1, NN2              SUTRA_MAIN...42100
            NN3 = 1                                                      SUTRA_MAIN...42200
         ELSE                                                            SUTRA_MAIN...42300
            READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR, NN1, NN2, NN3         SUTRA_MAIN...42400
         END IF                                                          SUTRA_MAIN...42500
         IF (INERR(1).NE.0) THEN
	      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                                                      SUTRA_MAIN...42600
	      GOTO 9000
	   ENDIF
         IF ((NN1.LT.2).OR.(NN2.LT.2).OR.                                SUTRA_MAIN...42700
     1      ((KTYPE(1).EQ.3).AND.(NN3.LT.2))) THEN                       SUTRA_MAIN...42800
            ERRCOD = 'INP-2B-3'                                          SUTRA_MAIN...42900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...43000
	      GOTO 9000
         END IF                                                          SUTRA_MAIN...43100
         IF (MSHTYP(2).EQ.'BLOCKWISE ') THEN                             SUTRA_MAIN...43200
            KTYPE(2) = 3                                                 SUTRA_MAIN...43300
            ERRCOD = 'REA-INP-2B'                                        SUTRA_MAIN...43400
            DO 177 I1=1,KTYPE(1)                                         SUTRA_MAIN...43500
               CALL READIF(K1, INTFIL, ERRCOD,IERROR)                           SUTRA_MAIN...43600
	         IF (IERROR.NE.0) GOTO 9000
               READ(INTFIL,*,IOSTAT=INERR(1)) IDUM1, (IDUM2, I2=1,IDUM1) SUTRA_MAIN...43700
               IF (INERR(1).NE.0) THEN
                   CALL SUTERR(ERRCOD,CHERR,INERR,RLERR,IERROR)                   SUTRA_MAIN...43800
	             GOTO 9000
	         ENDIF
  177       CONTINUE                                                     SUTRA_MAIN...43900
         ELSE                                                            SUTRA_MAIN...44000
            KTYPE(2) = 2                                                 SUTRA_MAIN...44100
         END IF                                                          SUTRA_MAIN...44200
      ELSE IF (MSHTYP(2).EQ.'LAYERED   ') THEN                           SUTRA_MAIN...44300
         IF (KTYPE(1).EQ.2) THEN                                         SUTRA_MAIN...44400
            ERRCOD = 'INP-2B-5'                                          SUTRA_MAIN...44500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...44600
	      GOTO 9000
         END IF                                                          SUTRA_MAIN...44700
         KTYPE(2) = 1                                                    SUTRA_MAIN...44800
         ERRCOD = 'REA-INP-2B'                                           SUTRA_MAIN...44900
         READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR,NLAYS,NNLAY,NELAY,LAYSTR  SUTRA_MAIN...45000
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD,CHERR,INERR,RLERR, IERROR)                SUTRA_MAIN...45100
	      GOTO 9000
	   ENDIF
         CALL PRSWDS(LAYSTR, ' ', 1, LAYNOR, NWORDS)                     SUTRA_MAIN...45200
         IF ((LAYNOR(1).NE.'ACROSS').AND.(LAYNOR(1).NE.'WITHIN')) THEN   SUTRA_MAIN...45300
            ERRCOD = 'INP-2B-6'                                          SUTRA_MAIN...45400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...45500
	      GOTO 9000
         END IF                                                          SUTRA_MAIN...45600
         IF ((NLAYS.LT.2).OR.(NNLAY.LT.4).OR.(NELAY.LT.1)) THEN          SUTRA_MAIN...45700
            ERRCOD = 'INP-2B-7'                                          SUTRA_MAIN...45800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTRA_MAIN...45900
	      GOTO 9000
         END IF                                                          SUTRA_MAIN...46000
      ELSE IF (MSHTYP(2).EQ.'IRREGULAR ') THEN                           SUTRA_MAIN...46100
         KTYPE(2) = 0                                                    SUTRA_MAIN...46200
      ELSE                                                               SUTRA_MAIN...46300
         ERRCOD = 'INP-2B-4'                                             SUTRA_MAIN...46400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...46500
	   GOTO 9000
      END IF                                                             SUTRA_MAIN...46600
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
C                                                                        SUTRA_MAIN...46700
C.....OUTPUT DATASET 1                                                   SUTRA_MAIN...46800
!      WRITE(K3,180) TITLE1,TITLE2                                        SUTRA_MAIN...46900
!  180 FORMAT(////1X,131('-')//26X,80A1//26X,80A1//1X,131('-'))           SUTRA_MAIN...47000
C                                                                        SUTRA_MAIN...47100
C.....OUTPUT FILE UNIT ASSIGNMENTS                                       SUTRA_MAIN...47200
!      WRITE(K3,202) IUNIT(1),FNINP,IUNIT(2),FNICS,IUNIT(0),FNAME(0),     SUTRA_MAIN...47300
!     1   IUNIT(3),FNAME(3)                                               SUTRA_MAIN...47400
!  202 FORMAT(/////11X,'F I L E   U N I T   A S S I G N M E N T S'//      SUTRA_MAIN...47500
!     1   13X,'INPUT UNITS:'/                                             SUTRA_MAIN...47600
!     2   13X,' INP FILE (MAIN INPUT)          ',I7,4X,                   SUTRA_MAIN...47700
!     3      'ASSIGNED TO ',A80/                                          SUTRA_MAIN...47800
!     4   13X,' ICS FILE (INITIAL CONDITIONS)  ',I7,4X,                   SUTRA_MAIN...47900
!     5      'ASSIGNED TO ',A80//                                         SUTRA_MAIN...48000
!     6   13X,'OUTPUT UNITS:'/                                            SUTRA_MAIN...48100
!     7   13X,' SMY FILE (RUN SUMMARY)         ',I7,4X,                   SUTRA_MAIN...48200
!     8      'ASSIGNED TO ',A80/                                          SUTRA_MAIN...48300
!     9   13X,' LST FILE (GENERAL OUTPUT)      ',I7,4X,                   SUTRA_MAIN...48400
!     T      'ASSIGNED TO ',A80)                                          SUTRA_MAIN...48500
!      IF(IUNIT(4).NE.-1) WRITE(K3,203) IUNIT(4),FNAME(4)                 SUTRA_MAIN...48600
!  203 FORMAT(13X,' RST FILE (RESTART DATA)        ',I7,4X,               SUTRA_MAIN...48700
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...48800
!      IF(IUNIT(5).NE.-1) WRITE(K3,204) IUNIT(5),FNAME(5)                 SUTRA_MAIN...48900
!  204 FORMAT(13X,' NOD FILE (NODEWISE OUTPUT)     ',I7,4X,               SUTRA_MAIN...49000
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...49100
!      IF(IUNIT(6).NE.-1) WRITE(K3,206) IUNIT(6),FNAME(6)                 SUTRA_MAIN...49200
!  206 FORMAT(13X,' ELE FILE (VELOCITY OUTPUT)     ',I7,4X,               SUTRA_MAIN...49300
!     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...49400
!      IF(IUNIT(7).NE.-1) WRITE(K3,207) IUNIT(7),                         SUTRA_MAIN...49500
!     1   TRIM(FNAME(7)) // " (BASE FILENAME)"                            SUTRA_MAIN...49600
!  207 FORMAT(13X,' OBS FILE (OBSERVATION OUTPUT) (',I7,')',3X,           SUTRA_MAIN...49700
!     1   'ASSIGNED TO ',A)                                               SUTRA_MAIN...49800
!      IF(IUNIT(8).NE.-1) WRITE(K3,208) IUNIT(8),                         SUTRA_MAIN...49900
!     1   TRIM(FNAME(8)) // " (BASE FILENAME)"                            SUTRA_MAIN...50000
!  208 FORMAT(13X,' OBC FILE (OBSERVATION OUTPUT) (',I7,')',3X,           SUTRA_MAIN...50100
!     1   'ASSIGNED TO ',A)                                               SUTRA_MAIN...50200
!      WRITE(K3,209)                                                      SUTRA_MAIN...50300
!  209 FORMAT(/14X,'NAMES FOR OBS AND OBC FILES WILL BE GENERATED',       SUTRA_MAIN...50400
!     1   ' AUTOMATICALLY FROM THE BASE NAMES LISTED ABOVE AND SCHEDULE', SUTRA_MAIN...50500
!     2   ' NAMES'/14X,'LISTED LATER IN THIS FILE.  UNIT NUMBERS',        SUTRA_MAIN...50600
!     3   ' ASSIGNED TO THESE FILES WILL BE THE FIRST AVAILABLE',         SUTRA_MAIN...50700
!     4   ' NUMBERS GREATER THAN'/14X,'OR EQUAL TO THE VALUES LISTED',    SUTRA_MAIN...50800
!     5   ' ABOVE IN PARENTHESES.')                                       SUTRA_MAIN...50900
C                                                                        SUTRA_MAIN...51000
C.....INPUT DATASET 3:  SIMULATION CONTROL NUMBERS                       SUTRA_MAIN...51100
      ERRCOD = 'REA-INP-3'                                               SUTRA_MAIN...51200
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    SUTRA_MAIN...51300
	IF (IERROR.NE.0) GOTO 9000
      READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS      SUTRA_MAIN...51400
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                SUTRA_MAIN...51500
	   GOTO 9000
	ENDIF
      IF (KTYPE(2).GT.1) THEN                                            SUTRA_MAIN...51600
         NN123 = NN1*NN2*NN3                                             SUTRA_MAIN...51700
         IF(NN123.NE.NN) THEN                                            SUTRA_MAIN...51800
           ERRCOD = 'INP-2B,3-1'                                         SUTRA_MAIN...51900
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      SUTRA_MAIN...52000
	     GOTO 9000
         END IF                                                          SUTRA_MAIN...52100
         IF (KTYPE(1).EQ.3) THEN                                         SUTRA_MAIN...52200
            NE123 = (NN1 - 1)*(NN2 - 1)*(NN3 - 1)                        SUTRA_MAIN...52300
         ELSE                                                            SUTRA_MAIN...52400
            NE123 = (NN1 - 1)*(NN2 - 1)                                  SUTRA_MAIN...52500
         END IF                                                          SUTRA_MAIN...52600
         IF(NE123.NE.NE) THEN                                            SUTRA_MAIN...52700
           ERRCOD = 'INP-2B,3-2'                                         SUTRA_MAIN...52800
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      SUTRA_MAIN...52900
	     GOTO 9000
         END IF                                                          SUTRA_MAIN...53000
      ELSE IF (MSHTYP(2).EQ.'LAYERED   ') THEN                           SUTRA_MAIN...53100
         NNTOT = NLAYS*NNLAY                                             SUTRA_MAIN...53200
         IF(NNTOT.NE.NN) THEN                                            SUTRA_MAIN...53300
           ERRCOD = 'INP-2B,3-3'                                         SUTRA_MAIN...53400
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      SUTRA_MAIN...53500
	     GOTO 9000
         END IF                                                          SUTRA_MAIN...53600
         NETOT = (NLAYS - 1)*NELAY                                       SUTRA_MAIN...53700
         IF(NETOT.NE.NE) THEN                                            SUTRA_MAIN...53800
           ERRCOD = 'INP-2B,3-4'                                         SUTRA_MAIN...53900
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                      SUTRA_MAIN...54000
	     GOTO 9000
         END IF                                                          SUTRA_MAIN...54100
      ENDIF                                                              SUTRA_MAIN...54200
C                                                                        SUTRA_MAIN...54300
C                                                                        SUTRA_MAIN...54300
      IBOUSZ = NPBC + NUBC + NSOP + NSOU + 4
	NFEAT = 4
C                                                                        SUTRA_MAIN...54300
C                                                                        SUTRA_MAIN...54300
C.....INPUT AND OUTPUT DATASET 4:  SIMULATION MODE OPTIONS               SUTRA_MAIN...54400
      ERRCOD = 'REA-INP-4'                                               SUTRA_MAIN...54500
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    SUTRA_MAIN...54600
	IF (IERROR.NE.0) GOTO 9000
      READ(INTFIL,*,IOSTAT=INERR(1)) UNSSTR,SSFSTR,SSTSTR,RDSTR,ISTORE   SUTRA_MAIN...54700
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                SUTRA_MAIN...54800
	   GOTO 9000
	ENDIF
      CALL PRSWDS(UNSSTR, ' ', 1, CUNSAT, NWORDS)                        SUTRA_MAIN...54900
      CALL PRSWDS(SSFSTR, ' ', 1, CSSFLO, NWORDS)                        SUTRA_MAIN...55000
      CALL PRSWDS(SSTSTR, ' ', 1, CSSTRA, NWORDS)                        SUTRA_MAIN...55100
      CALL PRSWDS(RDSTR,  ' ', 1, CREAD, NWORDS)                         SUTRA_MAIN...55200
      ISMERR = 0                                                         SUTRA_MAIN...55300
      IF (CUNSAT.EQ.'UNSATURATED') THEN                                  SUTRA_MAIN...55400
         IUNSAT = +1                                                     SUTRA_MAIN...55500
      ELSE IF (CUNSAT.EQ.'SATURATED') THEN                               SUTRA_MAIN...55600
         IUNSAT = 0                                                      SUTRA_MAIN...55700
      ELSE                                                               SUTRA_MAIN...55800
         ERRCOD = 'INP-4-1'                                              SUTRA_MAIN...55900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...56000
	   GOTO 9000
      END IF                                                             SUTRA_MAIN...56100
      IF (CSSFLO.EQ.'TRANSIENT') THEN                                    SUTRA_MAIN...56200
         ISSFLO = 0                                                      SUTRA_MAIN...56300
      ELSE IF (CSSFLO.EQ.'STEADY') THEN                                  SUTRA_MAIN...56400
         ISSFLO = +1                                                     SUTRA_MAIN...56500
      ELSE                                                               SUTRA_MAIN...56600
         ERRCOD = 'INP-4-2'                                              SUTRA_MAIN...56700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...56800
	   GOTO 9000
      END IF                                                             SUTRA_MAIN...56900
      IF (CSSTRA.EQ.'TRANSIENT') THEN                                    SUTRA_MAIN...57000
         ISSTRA = 0                                                      SUTRA_MAIN...57100
      ELSE IF (CSSTRA.EQ.'STEADY') THEN                                  SUTRA_MAIN...57200
         ISSTRA = +1                                                     SUTRA_MAIN...57300
      ELSE                                                               SUTRA_MAIN...57400
         ERRCOD = 'INP-4-3'                                              SUTRA_MAIN...57500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...57600
	   GOTO 9000
      END IF                                                             SUTRA_MAIN...57700
      IF (CREAD.EQ.'COLD') THEN                                          SUTRA_MAIN...57800
         IREAD = +1                                                      SUTRA_MAIN...57900
      ELSE IF (CREAD.EQ.'WARM') THEN                                     SUTRA_MAIN...58000
         IREAD = -1                                                      SUTRA_MAIN...58100
      ELSE                                                               SUTRA_MAIN...58200
         ERRCOD = 'INP-4-4'                                              SUTRA_MAIN...58300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...58400
	   GOTO 9000
      END IF                                                             SUTRA_MAIN...58500
!      WRITE(K3,210)                                                      SUTRA_MAIN...58600
!  210 FORMAT(////11X,'S I M U L A T I O N   M O D E   ',                 SUTRA_MAIN...58700
!     1   'O P T I O N S'/)                                               SUTRA_MAIN...58800
      IF(ISSTRA.EQ.1.AND.ISSFLO.NE.1) THEN                               SUTRA_MAIN...58900
         ERRCOD = 'INP-4-5'                                              SUTRA_MAIN...59000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...59100
	   GOTO 9000
      ENDIF                                                              SUTRA_MAIN...59200
C RBW
	ISTEADYFLOW = ISSFLO
	ISTEADYTRANSPORT = ISSTRA
      IF (ISTART.LE.0) then
	  GOTO 9000
	endif
C RBW
!      IF(IUNSAT.EQ.+1) WRITE(K3,215)                                     SUTRA_MAIN...59300
!      IF(IUNSAT.EQ.0) WRITE(K3,216)                                      SUTRA_MAIN...59400
!  215 FORMAT(11X,'- ALLOW UNSATURATED AND SATURATED FLOW:  UNSATURATED', SUTRA_MAIN...59500
!     1   ' PROPERTIES ARE USER-PROGRAMMED IN SUBROUTINE   U N S A T')    SUTRA_MAIN...59600
!  216 FORMAT(11X,'- ASSUME SATURATED FLOW ONLY')                         SUTRA_MAIN...59700
!      IF(ISSFLO.EQ.+1.AND.ME.EQ.-1) WRITE(K3,219)                        SUTRA_MAIN...59800
!      IF(ISSFLO.EQ.+1.AND.ME.EQ.+1) WRITE(K3,220)                        SUTRA_MAIN...59900
!      IF(ISSFLO.EQ.0) WRITE(K3,221)                                      SUTRA_MAIN...60000
!  219 FORMAT(11X,'- ASSUME STEADY-STATE FLOW FIELD CONSISTENT WITH ',    SUTRA_MAIN...60100
!     1   'INITIAL CONCENTRATION CONDITIONS')                             SUTRA_MAIN...60200
!  220 FORMAT(11X,'- ASSUME STEADY-STATE FLOW FIELD CONSISTENT WITH ',    SUTRA_MAIN...60300
!     1   'INITIAL TEMPERATURE CONDITIONS')                               SUTRA_MAIN...60400
!  221 FORMAT(11X,'- ALLOW TIME-DEPENDENT FLOW FIELD')                    SUTRA_MAIN...60500
!      IF(ISSTRA.EQ.+1) WRITE(K3,225)                                     SUTRA_MAIN...60600
!      IF(ISSTRA.EQ.0) WRITE(K3,226)                                      SUTRA_MAIN...60700
!  225 FORMAT(11X,'- ASSUME STEADY-STATE TRANSPORT')                      SUTRA_MAIN...60800
!  226 FORMAT(11X,'- ALLOW TIME-DEPENDENT TRANSPORT')                     SUTRA_MAIN...60900
!      IF(IREAD.EQ.-1) WRITE(K3,230)                                      SUTRA_MAIN...61000
!      IF(IREAD.EQ.+1) WRITE(K3,231)                                      SUTRA_MAIN...61100
!  230 FORMAT(11X,'- WARM START - SIMULATION IS TO BE ',                  SUTRA_MAIN...61200
!     1   'CONTINUED FROM PREVIOUSLY-STORED DATA')                        SUTRA_MAIN...61300
!  231 FORMAT(11X,'- COLD START - BEGIN NEW SIMULATION')                  SUTRA_MAIN...61400
!      IF(ISTORE.GT.0) WRITE(K3,240) ISTORE                               SUTRA_MAIN...61500
!      IF(ISTORE.EQ.0) WRITE(K3,241)                                      SUTRA_MAIN...61600
!  240 FORMAT(11X,'- STORE RESULTS AFTER EVERY',I9,' TIME STEPS IN',      SUTRA_MAIN...61700
!     1   ' RESTART FILE AS BACKUP AND FOR USE IN A SIMULATION RESTART')  SUTRA_MAIN...61800
!  241 FORMAT(11X,'- DO NOT STORE RESULTS FOR USE IN A',                  SUTRA_MAIN...61900
!     1   ' RESTART OF SIMULATION')                                       SUTRA_MAIN...62000
C.....OUTPUT DATASET 3                                                   SUTRA_MAIN...62100
!      IF(ME.EQ.-1)                                                       SUTRA_MAIN...62200
!     1   WRITE(K3,245) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS                    SUTRA_MAIN...62300
!  245 FORMAT(////11X,'S I M U L A T I O N   C O N T R O L   ',           SUTRA_MAIN...62400
!     1   'N U M B E R S'// 8X,I9,5X,'NUMBER OF NODES IN FINITE-',        SUTRA_MAIN...62500
!     2   'ELEMENT MESH'/ 8X,I9,5X,'NUMBER OF ELEMENTS IN MESH'//         SUTRA_MAIN...62600
!     3    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...62700
!     4   'PRESSURE IS A SPECIFIED CONSTANT OR FUNCTION OF TIME'/         SUTRA_MAIN...62800
!     5    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...62900
!     6   'SOLUTE CONCENTRATION IS A SPECIFIED CONSTANT OR ',             SUTRA_MAIN...63000
!     7   'FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES AT',       SUTRA_MAIN...63100
!     8   ' WHICH FLUID INFLOW OR OUTFLOW IS A SPECIFIED CONSTANT',       SUTRA_MAIN...63200
!     9   ' OR FUNCTION OF TIME'/ 8X,I9,5X,'EXACT NUMBER OF NODES AT',    SUTRA_MAIN...63300
!     A   ' WHICH A SOURCE OR SINK OF SOLUTE MASS IS A SPECIFIED ',       SUTRA_MAIN...63400
!     B   'CONSTANT OR FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF ',   SUTRA_MAIN...63500
!     C   'NODES AT WHICH PRESSURE AND CONCENTRATION WILL BE OBSERVED')   SUTRA_MAIN...63600
C                                                                        SUTRA_MAIN...63700
!      IF(ME.EQ.+1)                                                       SUTRA_MAIN...63800
!     1    WRITE(K3,247) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS                   SUTRA_MAIN...63900
!  247 FORMAT(////11X,'S I M U L A T I O N   C O N T R O L   ',           SUTRA_MAIN...64000
!     1   'N U M B E R S'// 8X,I9,5X,'NUMBER OF NODES IN FINITE-',        SUTRA_MAIN...64100
!     2   'ELEMENT MESH'/ 8X,I9,5X,'NUMBER OF ELEMENTS IN MESH'//         SUTRA_MAIN...64200
!     3    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...64300
!     4   'PRESSURE IS A SPECIFIED CONSTANT OR FUNCTION OF TIME'/         SUTRA_MAIN...64400
!     5    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...64500
!     6   'TEMPERATURE IS A SPECIFIED CONSTANT OR ',                      SUTRA_MAIN...64600
!     7   'FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES AT',       SUTRA_MAIN...64700
!     8   ' WHICH FLUID INFLOW OR OUTFLOW IS A SPECIFIED CONSTANT',       SUTRA_MAIN...64800
!     9   ' OR FUNCTION OF TIME'/ 8X,I9,5X,'EXACT NUMBER OF NODES AT',    SUTRA_MAIN...64900
!     A   ' WHICH A SOURCE OR SINK OF ENERGY IS A SPECIFIED CONSTANT',    SUTRA_MAIN...65000
!     B   ' OR FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES ',     SUTRA_MAIN...65100
!     C   'AT WHICH PRESSURE AND TEMPERATURE WILL BE OBSERVED')           SUTRA_MAIN...65200
C                                                                        SUTRA_MAIN...65300
C.....INPUT DATASETS 5 - 7 (NUMERICAL, TEMPORAL, AND ITERATION CONTROLS) SUTRA_MAIN...65400
      CALL INDAT0(IERROR)                                                      SUTRA_MAIN...65500
	IF (IERROR.NE.0) GOTO 9000
C.....KSOLVP AND KSOLVU HAVE BEEN SET ACCORDING TO THE SOLVERS SELECTED: SUTRA_MAIN...65600
C        BANDED GAUSSIAN ELIMINATION (DIRECT)   ==>   0                  SUTRA_MAIN...65700
C        IC-PRECONDITIONED CG                   ==>   1                  SUTRA_MAIN...65800
C        ILU-PRECONDITIONED GMRES               ==>   2                  SUTRA_MAIN...65900
C        ILU-PRECONDITIONED ORTHOMIN            ==>   3                  SUTRA_MAIN...66000
C                                                                        SUTRA_MAIN...66100
C.....OUTPUT DATASETS 7B & 7C                                            SUTRA_MAIN...66200
!      WRITE(K3,261)                                                      SUTRA_MAIN...66300
!  261 FORMAT(////11X,'S O L V E R - R E L A T E D   ',                   SUTRA_MAIN...66400
!     1   'P A R A M E T E R S')                                          SUTRA_MAIN...66500
C.....OUTPUT DATASETS 3B & 3C                                            SUTRA_MAIN...66600
!  266 IF (KSOLVP.NE.0) THEN                                              SUTRA_MAIN...66700
!         WRITE(K3,268)                                                   SUTRA_MAIN...66800
!     1      SOLNAM(KSOLVP), ITRMXP, TOLP,                                SUTRA_MAIN...66900
!     2      SOLNAM(KSOLVU), ITRMXU, TOLU                                 SUTRA_MAIN...67000
!  268    FORMAT(                                                         SUTRA_MAIN...67100
!     1      /13X,'SOLVER FOR P: ',A40                                    SUTRA_MAIN...67200
!     2      //20X,I6,5X,'MAXIMUM NUMBER OF MATRIX SOLVER ITERATIONS',    SUTRA_MAIN...67300
!     3           ' DURING P SOLUTION'                                    SUTRA_MAIN...67400
!     4      /11X,1PE15.4,5X,'CONVERGENCE TOLERANCE FOR MATRIX',          SUTRA_MAIN...67500
!     5           ' SOLVER ITERATIONS DURING P SOLUTION'                  SUTRA_MAIN...67600
!     6      //13X,'SOLVER FOR U: ',A40                                   SUTRA_MAIN...67700
!     7      //20X,I6,5X,'MAXIMUM NUMBER OF MATRIX SOLVER ITERATIONS',    SUTRA_MAIN...67800
!     8           ' DURING U SOLUTION'                                    SUTRA_MAIN...67900
!     9      /11X,1PE15.4,5X,'CONVERGENCE TOLERANCE FOR MATRIX',          SUTRA_MAIN...68000
!     A           ' SOLVER ITERATIONS DURING U SOLUTION' )                SUTRA_MAIN...68100
!      ELSE                                                               SUTRA_MAIN...68200
!         WRITE(K3,269) SOLNAM(KSOLVP)                                    SUTRA_MAIN...68300
!  269    FORMAT(/13X,'SOLVER FOR P AND U: ',A40)                         SUTRA_MAIN...68400
!      END IF                                                             SUTRA_MAIN...68500
C                                                                        SUTRA_MAIN...68600
C.....CALCULATE ARRAY DIMENSIONS, EXCEPT THOSE THAT DEPEND ON            SUTRA_MAIN...68700
C        BANDWIDTH OR NELT                                               SUTRA_MAIN...68800
C                                                                        SUTRA_MAIN...68900
      IF (KSOLVP.EQ.0) THEN                                              SUTRA_MAIN...69000
C........SET DIMENSIONS FOR DIRECT SOLVER                                SUTRA_MAIN...69100
         NNNX = 1                                                        SUTRA_MAIN...69200
         NDIMJA = 1                                                      SUTRA_MAIN...69300
         NNVEC = NN                                                      SUTRA_MAIN...69400
      ELSE                                                               SUTRA_MAIN...69500
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)                          SUTRA_MAIN...69600
         NNNX = NN                                                       SUTRA_MAIN...69700
         NDIMJA = NN + 1                                                 SUTRA_MAIN...69800
         NNVEC = NN                                                      SUTRA_MAIN...69900
      END IF                                                             SUTRA_MAIN...70000
      NBCN=NPBC+NUBC+1                                                   SUTRA_MAIN...70100
      NSOP=NSOP+1                                                        SUTRA_MAIN...70200
      NSOU=NSOU+1                                                        SUTRA_MAIN...70300
      NOBSN=NOBS+1                                                       SUTRA_MAIN...70400
      IF (KTYPE(1).EQ.3) THEN                                            SUTRA_MAIN...70500
         N48 = 8                                                         SUTRA_MAIN...70600
         NEX = NE                                                        SUTRA_MAIN...70700
      ELSE                                                               SUTRA_MAIN...70800
         N48 = 4                                                         SUTRA_MAIN...70900
         NEX = 1                                                         SUTRA_MAIN...71000
      END IF                                                             SUTRA_MAIN...71100
      NIN=NE*N48                                                         SUTRA_MAIN...71200
C                                                                        SUTRA_MAIN...71300
C.....ALLOCATE REAL ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH        SUTRA_MAIN...71400
      ALLOCATE(PITER(NN),UITER(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN),  SUTRA_MAIN...71500
     1   PVEL(NN),SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),       SUTRA_MAIN...71600
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),       SUTRA_MAIN...71700
     3   QIN(NN),UIN(NN),QUIN(NN),QINITR(NN),RCIT(NN),RCITM1(NN))        SUTRA_MAIN...71800
      ALLOCATE(PVEC(NNVEC),UVEC(NNVEC))                                  SUTRA_MAIN...71900
      ALLOCATE(ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),         SUTRA_MAIN...72000
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),          SUTRA_MAIN...72100
     2   PANGL1(NE))                                                     SUTRA_MAIN...72200
      ALLOCATE(ALMID(NEX),ATMID(NEX),                                    SUTRA_MAIN...72300
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),                 SUTRA_MAIN...72400
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX))                SUTRA_MAIN...72500
      ALLOCATE(PBC(NBCN),UBC(NBCN),QPLITR(NBCN))                         SUTRA_MAIN...72600
      ALLOCATE(GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48))                  SUTRA_MAIN...72700
      ALLOCATE(B(NNNX))                                                  SUTRA_MAIN...72800
C.....ALLOCATE INTEGER ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH     SUTRA_MAIN...72900
C        OR NELT                                                         SUTRA_MAIN...73000
      ALLOCATE(IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),    SUTRA_MAIN...73100
     1   NREG(NN),LREG(NE),JA(NDIMJA))                                   SUTRA_MAIN...73200
C.....ALLOCATE ARRAYS OF DERIVED TYPE, EXCEPT THOSE THAT DEPEND ON       SUTRA_MAIN...73300
C        BANDWIDTH OR NELT                                               SUTRA_MAIN...73400
      ALLOCATE(OBSPTS(NOBSN))                                            SUTRA_MAIN...73500
      ALLO1 = .TRUE.                                                     SUTRA_MAIN...73600
C                                                                        SUTRA_MAIN...73700
C.....INPUT DATASETS 8 - 15 (OUTPUT CONTROLS; FLUID AND SOLID MATRIX     SUTRA_MAIN...73800
C        PROPERTIES; ADSORPTION PARAMETERS; PRODUCTION OF ENERGY OR      SUTRA_MAIN...73900
C        SOLUTE MASS; GRAVITY; AND NODEWISE AND ELEMENTWISE DATA)        SUTRA_MAIN...74000
      CALL INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,         SUTRA_MAIN...74100
     1   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,                      SUTRA_MAIN...74200
     2   PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG,        SUTRA_MAIN...74300
     3   OBSPTS,
     4   ElementValues, IElementValueCount, NodeValues, 
     5   INodeValueCount, IERROR)                                        SUTRA_MAIN...74400
	IF (IERROR.NE.0) GOTO 9000
C                                                                        SUTRA_MAIN...74500
C.....KEEP TRACK IF OUTPUT ROUTINES HAVE BEEN EXECUTED, TO PRINT         SUTRA_MAIN...74600
C        HEADERS ONLY ONCE.                                              SUTRA_MAIN...74700
      ONCEK5 = .FALSE.                                                   SUTRA_MAIN...74800
      ONCEK6 = .FALSE.                                                   SUTRA_MAIN...74900
      ONCEK7 = .FALSE.                                                   SUTRA_MAIN...75000
      ONCEK8 = .FALSE.                                                   SUTRA_MAIN...75100
      ALLOCATE(ONCK78(NFLOMX))                                           SUTRA_MAIN...75200
      DO 400 J=1,NFLOMX                                                  SUTRA_MAIN...75300
         ONCK78(J) = .FALSE.                                             SUTRA_MAIN...75400
  400 CONTINUE                                                           SUTRA_MAIN...75500
C                                                                        SUTRA_MAIN...75600
C.....INPUT DATASETS 17 & 18 (SOURCES OF FLUID MASS AND ENERGY OR        SUTRA_MAIN...75700
C        SOLUTE MASS)                                                    SUTRA_MAIN...75800
      CALL ZERO(QIN,NN,0.0D0)                                            SUTRA_MAIN...75900
      CALL ZERO(UIN,NN,0.0D0)                                            SUTRA_MAIN...76000
      CALL ZERO(QUIN,NN,0.0D0)                                           SUTRA_MAIN...76100
      IF(NSOP-1.GT.0.OR.NSOU-1.GT.0)                                     SUTRA_MAIN...76200
     1   CALL SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT,
     2     IERROR, IBOUSZ, IBNODE, IPOS)                                 SUTRA_MAIN...76300
C                                                                        SUTRA_MAIN...76400
	IF (IERROR.NE.0) GOTO 9000
C.....INPUT DATASETS 19 & 20 (SPECIFIED P AND U BOUNDARY CONDITIONS)     SUTRA_MAIN...76500
      IF(NBCN-1.GT.0) THEN
	  CALL BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT,
     1     IERROR, IBOUSZ, IBNODE, IPOS)                                 SUTRA_MAIN...76600
	  IF (IERROR.NE.0) GOTO 9000
      ELSE
	  IBNODE(IPOS+1) = 0
	  IBNODE(IPOS+2) = 0
      ENDIF                                                             B650....
C                                                                        SUTRA_MAIN...76700
C.....INPUT DATASET 22 (ELEMENT INCIDENCE [MESH CONNECTION] DATA)        SUTRA_MAIN...76800
      CALL CONNEC(IN,IERROR)                                                    SUTRA_MAIN...76900
	IF (IERROR.NE.0) GOTO 9000
C RBW
	do IncidenceIndex = 1, NIN
	  Incidence(IncidenceIndex) = IN(IncidenceIndex) -1
	enddo
C RBW
C                                                                        SUTRA_MAIN...77000
C.....IF USING OLD (VERSION 2D3D.1) OBSERVATION INPUT FORMAT, LOOK UP    SUTRA_MAIN...77100
C        COORDINATES FOR OBSERVATION POINTS (NODES).                     SUTRA_MAIN...77200
      IF (NOBCYC.NE.-1) THEN                                             SUTRA_MAIN...77300
         DO 710 K=1,NOBS                                                 SUTRA_MAIN...77400
            I = OBSPTS(K)%L                                              SUTRA_MAIN...77500
            OBSPTS(K)%X = X(I)                                           SUTRA_MAIN...77600
            OBSPTS(K)%Y = Y(I)                                           SUTRA_MAIN...77700
            IF (N48.EQ.8) OBSPTS(K)%Z = Z(I)                             SUTRA_MAIN...77800
  710    CONTINUE                                                        SUTRA_MAIN...77900
      END IF                                                             SUTRA_MAIN...78000
C                                                                        SUTRA_MAIN...78100
C.....FIND THE ELEMENT EACH OBSERVATION POINT IS IN.  IN COMPONENTS OF   SUTRA_MAIN...78200
C        OBSPTS, OVERWRITE NODE NUMBERS AND GLOBAL COORDINATES WITH      SUTRA_MAIN...78300
C        ELEMENT NUMBERS AND LOCAL COORDINATES.                          SUTRA_MAIN...78400
      DO 900 K=1,NOBS                                                    SUTRA_MAIN...78500
         XK = OBSPTS(K)%X                                                SUTRA_MAIN...78600
         YK = OBSPTS(K)%Y                                                SUTRA_MAIN...78700
         IF (N48.EQ.8) ZK = OBSPTS(K)%Z                                  SUTRA_MAIN...78800
         DO 800 LL=1,NE                                                  SUTRA_MAIN...78900
            IF (N48.EQ.8) THEN                                           SUTRA_MAIN...79000
               CALL FINDL3(X,Y,Z,IN,LL,XK,YK,ZK,XSI,ETA,ZET,INOUT)       SUTRA_MAIN...79100
            ELSE                                                         SUTRA_MAIN...79200
               CALL FINDL2(X,Y,IN,LL,XK,YK,XSI,ETA,INOUT)                SUTRA_MAIN...79300
            END IF                                                       SUTRA_MAIN...79400
            IF (INOUT.EQ.1) THEN                                         SUTRA_MAIN...79500
               L = LL                                                    SUTRA_MAIN...79600
               GOTO 820                                                  SUTRA_MAIN...79700
            END IF                                                       SUTRA_MAIN...79800
  800    CONTINUE                                                        SUTRA_MAIN...79900
         ERRCOD = 'INP-8D-3'                                             SUTRA_MAIN...80000
         CHERR(1) = OBSPTS(K)%NAME                                       SUTRA_MAIN...80100
!         WRITE(UNIT=CHERR(2), FMT=805)                                   SUTRA_MAIN...80200
!     1      OBSPTS(K)%X, OBSPTS(K)%Y, OBSPTS(K)%Z                        SUTRA_MAIN...80300
!  805    FORMAT('(',2(1PE14.7,','),1PE14.7,')')                          SUTRA_MAIN...80400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        SUTRA_MAIN...80500
	   GOTO 9000
  820    OBSPTS(K)%L = L                                                 SUTRA_MAIN...80600
         OBSPTS(K)%XSI = XSI                                             SUTRA_MAIN...80700
         OBSPTS(K)%ETA = ETA                                             SUTRA_MAIN...80800
         IF (N48.EQ.8) OBSPTS(K)%ZET = ZET                               SUTRA_MAIN...80900
  900 CONTINUE                                                           SUTRA_MAIN...81000
C                                                                        SUTRA_MAIN...81100
C.....IF ITERATIVE SOLVER IS USED, SET UP POINTER ARRAYS IA AND JA THAT  SUTRA_MAIN...81200
C        SPECIFY MATRIX STRUCTURE IN "SLAP COLUMN" FORMAT.  DIMENSION    SUTRA_MAIN...81300
C        NELT GETS SET HERE.                                             SUTRA_MAIN...81400
      IF (KSOLVP.NE.0) THEN                                              SUTRA_MAIN...81500
         CALL PTRSET()                                                   SUTRA_MAIN...81600
      ELSE                                                               SUTRA_MAIN...81700
         NELT = NN                                                       SUTRA_MAIN...81800
         NDIMIA = 1                                                      SUTRA_MAIN...81900
         ALLOCATE(IA(NDIMIA))                                            SUTRA_MAIN...82000
      END IF                                                             SUTRA_MAIN...82100
      ALLO3 = .TRUE.                                                     SUTRA_MAIN...82200
C                                                                        SUTRA_MAIN...82300
C.....CALCULATE BANDWIDTH                                                SUTRA_MAIN...82400
      CALL BANWID(IN)                                                    SUTRA_MAIN...82500
C                                                                        SUTRA_MAIN...82600
C.....CALCULATE ARRAY DIMENSIONS THAT DEPEND ON BANDWIDTH OR NELT        SUTRA_MAIN...82700
      IF (KSOLVP.EQ.0) THEN                                              SUTRA_MAIN...82800
C........SET DIMENSIONS FOR DIRECT SOLVER                                SUTRA_MAIN...82900
         NCBI = NBI                                                      SUTRA_MAIN...83000
         NELTA = NELT                                                    SUTRA_MAIN...83100
         NWI = 1                                                         SUTRA_MAIN...83200
         NWF = 1                                                         SUTRA_MAIN...83300
      ELSE                                                               SUTRA_MAIN...83400
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)                          SUTRA_MAIN...83500
         NCBI = 1                                                        SUTRA_MAIN...83600
         NELTA = NELT                                                    SUTRA_MAIN...83700
         KSOLVR = KSOLVP                                                 SUTRA_MAIN...83800
         NSAVE = NSAVEP                                                  SUTRA_MAIN...83900
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIP, NWFP)               SUTRA_MAIN...84000
         KSOLVR = KSOLVU                                                 SUTRA_MAIN...84100
         NSAVE = NSAVEU                                                  SUTRA_MAIN...84200
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIU, NWFU)               SUTRA_MAIN...84300
         NWI = MAX(NWIP, NWIU)                                           SUTRA_MAIN...84400
         NWF = MAX(NWFP, NWFU)                                           SUTRA_MAIN...84500
      END IF                                                             SUTRA_MAIN...84600
      MATDIM=NELT*NCBI                                                   SUTRA_MAIN...84700
C                                                                        SUTRA_MAIN...84800
C.....ALLOCATE REAL AND INTEGER ARRAYS THAT DEPEND ON BANDWIDTH OR NELT  SUTRA_MAIN...84900
!      ALLOCATE(PMAT(NELT,NCBI),UMAT(NELT,NCBI),FWK(NWF))                 SUTRA_MAIN...85000
      ALLOCATE(IWK(NWI))                                                 SUTRA_MAIN...85100
      ALLO2 = .TRUE.                                                     SUTRA_MAIN...85200
C                                                                        SUTRA_MAIN...85300
C.....INPUT INITIAL OR RESTART CONDITIONS FROM THE ICS FILE AND          SUTRA_MAIN...85400
C        INITIALIZE PARAMETERS                                           SUTRA_MAIN...85500
      CALL INDAT2(PVEC,UVEC,PM1,UM1,UM2,CS1,CS2,CS3,SL,SR,RCIT,SW,DSWDP, SUTRA_MAIN...85600
     1   PBC,IPBC,IPBCT,NREG,QIN,DPDTITR,IERROR)                                SUTRA_MAIN...85700
	IF (IERROR.NE.0) GOTO 9000
C                                                                        SUTRA_MAIN...85800
C.....COMPUTE AND OUTPUT DIMENSIONS OF SIMULATION                        SUTRA_MAIN...85900
      RMVDIM = 27*NN + 11*NE + 10*NEX + 3*NBCN + N48*(2*NE + NEX)        SUTRA_MAIN...86000
     1   + NNNX + 2*NELT*NCBI + NWF + 6*NOBSN + 3*NSCH                   SUTRA_MAIN...86100
      IMVDIM = NIN + NSOP + NSOU + 2*NBCN + NN + NE                      SUTRA_MAIN...86200
     1   + NDIMJA + NDIMIA + NWI + NOBSN + 3*NSCH                        SUTRA_MAIN...86300
      CMVDIM = 73*NOBS + 89*NSCH                                         SUTRA_MAIN...86400
      PMVDIM = 2*NSCH                                                    SUTRA_MAIN...86500
      TOTMB = (DBLE(RMVDIM)*8D0 + DBLE(IMVDIM)*4D0 + DBLE(CMVDIM))/1D6   SUTRA_MAIN...86600
!      WRITE(K3,3000) RMVDIM, IMVDIM, CMVDIM, PMVDIM, TOTMB               SUTRA_MAIN...86700
! 3000 FORMAT(////11X,'S I M U L A T I O N   D I M E N S I O N S'//       SUTRA_MAIN...86800
!     1   13X,'REAL        ARRAYS WERE ALLOCATED ',I12/                   SUTRA_MAIN...86900
!     2   13X,'INTEGER     ARRAYS WERE ALLOCATED ',I12/                   SUTRA_MAIN...87000
!     3   13X,'CHARACTER   ARRAYS WERE ALLOCATED ',I12,                   SUTRA_MAIN...87100
!     4       ' (SUM OF ARRAY_DIMENSION*CHARACTER_LENGTH)'/               SUTRA_MAIN...87200
!     5   13X,'ARRAYS OF POINTERS WERE ALLOCATED ',I12//                  SUTRA_MAIN...87300
!     6   13X,F10.3,' Mbytes MEMORY USED FOR MAIN ARRAYS'/                SUTRA_MAIN...87400
!     7   13X,'- assuming 1 byte/character'/                              SUTRA_MAIN...87500
!     8   13X,'- pointer storage not included')                           SUTRA_MAIN...87600
C                                                                        SUTRA_MAIN...87700
!      WRITE(K3,4000)                                                     SUTRA_MAIN...87800
! 4000 FORMAT(////////8(132("-")/))                                       SUTRA_MAIN...87900
C                                                                        SUTRA_MAIN...88000
C.....CALL MAIN CONTROL ROUTINE, SUTRA                                   SUTRA_MAIN...88100
!      CALL SUTRA(TITLE1,TITLE2,PMAT,UMAT,PITER,UITER,PM1,DPDTITR,        SUTRA_MAIN...88200
      CALL SUTRA(TITLE1,TITLE2,PITER,UITER,PM1,DPDTITR,                  SUTRA_MAIN...88200
     1   UM1,UM2,PVEL,SL,SR,X,Y,Z,VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,  SUTRA_MAIN...88300
     2   QIN,UIN,QUIN,QINITR,RCIT,RCITM1,PVEC,UVEC,                      SUTRA_MAIN...88400
     3   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,VMAG,VANG1,VANG2,           SUTRA_MAIN...88500
     4   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA_MAIN...88600
     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,B,           SUTRA_MAIN...88700
     6   IN,IQSOP,IQSOU,IPBC,IUBC,OBSPTS,NREG,LREG,IWK,IA,JA,            SUTRA_MAIN...88800
     7   IQSOPT,IQSOUT,IPBCT,IUBCT,IERROR)                                      SUTRA_MAIN...88900
!     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,FWK,B,       SUTRA_MAIN...88700
	IF (IERROR.NE.0) GOTO 9000
C                                                                        SUTRA_MAIN...89000
C.....TERMINATION SEQUENCE: DEALLOCATE ARRAYS, CLOSE FILES, AND END      SUTRA_MAIN...89100
9000  CONTINUE                                                           SUTRA_MAIN...89200
      CALL TERSEQ()                                                      SUTRA_MAIN...89300
	return
      END                                                                SUTRA_MAIN...89400
C                                                                        SUTRA_MAIN...89500
C     SUBROUTINE        A  D  S  O  R  B           SUTRA VERSION 2.1     ADSORB.........100
C                                                                        ADSORB.........200
C *** PURPOSE :                                                          ADSORB.........300
C ***  TO CALCULATE VALUES OF EQUILIBRIUM SORPTION PARAMETERS FOR        ADSORB.........400
C ***  LINEAR, FREUNDLICH, AND LANGMUIR MODELS.                          ADSORB.........500
C                                                                        ADSORB.........600
C     SUBROUTINE        B  A  N  W  I  D           SUTRA VERSION 2.1     BANWID.........100
C                                                                        BANWID.........200
C *** PURPOSE :                                                          BANWID.........300
C ***  TO CALCULATE THE BANDWIDTH OF THE FINITE ELEMENT MESH.            BANWID.........400
C                                                                        BANWID.........500
      SUBROUTINE BANWID(IN)                                              BANWID.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BANWID.........700
      CHARACTER*80 UNAME,FNAME(0:8)                                      BANWID.........800
      DIMENSION IN(NIN)                                                  BANWID.........900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BANWID........1000
     1   NSOP,NSOU,NBCN                                                  BANWID........1100
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        BANWID........1200
      COMMON /FNAMES/ UNAME,FNAME                                        BANWID........1300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     BANWID........1400
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       BANWID........1500
C                                                                        BANWID........1600
      NDIF=0                                                             BANWID........1700
      II=0                                                               BANWID........1800
!      WRITE(K3,100)                                                      BANWID........1900
!  100 FORMAT(////11X,'**** MESH ANALYSIS ****'//)                        BANWID........2000
C                                                                        BANWID........2100
C.....FIND ELEMENT WITH MAXIMUM DIFFERENCE IN NODE NUMBERS               BANWID........2200
      DO 2000 L=1,NE                                                     BANWID........2300
      II=II+1                                                            BANWID........2400
      IELO=IN(II)                                                        BANWID........2500
      IEHI=IN(II)                                                        BANWID........2600
      DO 1000 I=2,N48                                                    BANWID........2700
      II=II+1                                                            BANWID........2800
      IF(IN(II).LT.IELO) IELO=IN(II)                                     BANWID........2900
 1000 IF(IN(II).GT.IEHI) IEHI=IN(II)                                     BANWID........3000
      NDIFF=IEHI-IELO                                                    BANWID........3100
      IF(NDIFF.GT.NDIF) THEN                                             BANWID........3200
       NDIF=NDIFF                                                        BANWID........3300
       LEM=L                                                             BANWID........3400
      ENDIF                                                              BANWID........3500
 2000 CONTINUE                                                           BANWID........3600
C                                                                        BANWID........3700
C.....CALCULATE FULL BANDWIDTH, NB.                                      BANWID........3800
      NB=2*NDIF+1                                                        BANWID........3900
      NBHALF=NDIF+1                                                      BANWID........4000
C.....NBI IS USED TO DIMENSION ARRAYS WHOSE SIZE DEPENDS ON THE          BANWID........4100
C        BANDWIDTH.  IT IS THE SAME AS THE ACTUAL BANDWIDTH, NB.         BANWID........4200
      NBI = NB                                                           BANWID........4300
!      WRITE(K3,2500) NB,LEM                                              BANWID........4400
! 2500 FORMAT(//13X,'MAXIMUM FULL BANDWIDTH, ',I9,                        BANWID........4500
!     1   ', WAS CALCULATED IN ELEMENT ',I9)                              BANWID........4600
C                                                                        BANWID........4700
      RETURN                                                             BANWID........4800
      END                                                                BANWID........4900
C                                                                        BANWID........5000
C     SUBROUTINE        B  A  S  I  S  2           SUTRA VERSION 2.1     BASIS2.........100
C                                                                        BASIS2.........200
C *** PURPOSE :                                                          BASIS2.........300
C ***  TO CALCULATE VALUES OF BASIS AND WEIGHTING FUNCTIONS AND THEIR    BASIS2.........400
C ***  DERIVATIVES, TRANSFORMATION MATRICES BETWEEN LOCAL AND GLOBAL     BASIS2.........500
C ***  COORDINATES AND PARAMETER VALUES AT A SPECIFIED POINT IN A        BASIS2.........600
C ***  QUADRILATERAL FINITE ELEMENT.  THIS SUBROUTINE HANDLES 2D         BASIS2.........700
C ***  CALCULATIONS ONLY; 3D CALCULATIONS ARE PERFORMED IN SUBROUTINE    BASIS2.........800
C ***  BASIS3.                                                           BASIS2.........900
C                                                                        BASIS2........1000
C     SUBROUTINE        B  A  S  I  S  3           SUTRA VERSION 2.1     BASIS3.........100
C                                                                        BASIS3.........200
C *** PURPOSE :                                                          BASIS3.........300
C ***  TO CALCULATE VALUES OF BASIS AND WEIGHTING FUNCTIONS AND THEIR    BASIS3.........400
C ***  DERIVATIVES, TRANSFORMATION MATRICES BETWEEN LOCAL AND GLOBAL     BASIS3.........500
C ***  COORDINATES AND PARAMETER VALUES AT A SPECIFIED POINT IN A        BASIS3.........600
C ***  QUADRILATERAL FINITE ELEMENT.  THIS SUBROUTINE HANDLES 3D         BASIS3.........700
C ***  CALCULATIONS ONLY; 2D CALCULATIONS ARE PERFORMED IN SUBROUTINE    BASIS3.........800
C ***  BASIS2.                                                           BASIS3.........900
C     SUBROUTINE        B  C                       SUTRA VERSION 2.1     BC.............100
C                                                                        BC.............200
C *** PURPOSE :                                                          BC.............300
C ***  TO IMPLEMENT SPECIFIED PRESSURE AND SPECIFIED TEMPERATURE OR      BC.............400
C ***  CONCENTRATION CONDITIONS BY MODIFYING THE GLOBAL FLOW AND         BC.............500
C ***  TRANSPORT MATRIX EQUATIONS.                                       BC.............600
C                                                                        BC.............700
!      SUBROUTINE BC(ML,PMAT,PVEC,UMAT,UVEC,IPBC,PBC,IUBC,UBC,QPLITR,JA)  BC.............800
!      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BC.............900
!      DIMENSION PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),UVEC(NNVEC), BC............1000
!     1   IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN),QPLITR(NBCN)          BC............1100
!      DIMENSION JA(NDIMJA)                                               BC............1200
!      DIMENSION KTYPE(2)                                                 BC............1300
!      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BC............1400
!     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BC............1500
!      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BC............1600
!     1   NSOP,NSOU,NBCN                                                  BC............1700
!      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        BC............1800
!      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           BC............1900
!      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BC............2000
!     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BC............2100
!      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           BC............2200
!      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       BC............2300
!     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  BC............2400
C                                                                        BC............2500
C                                                                        BC............2600
C     SUBPROGRAM        B  D  I  N  I  T           SUTRA VERSION 2.1     BDINIT.........100
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
C     SUBROUTINE        B  O  U  N  D              SUTRA VERSION 2.1     BOUND..........100
C                                                                        BOUND..........200
C *** PURPOSE :                                                          BOUND..........300
C ***  TO READ AND ORGANIZE SPECIFIED PRESSURE DATA AND                  BOUND..........400
C ***  SPECIFIED TEMPERATURE OR CONCENTRATION DATA.                      BOUND..........500
C                                                                        BOUND..........600
      SUBROUTINE BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT, 
     1  IERROR, IBOUSZ, IBNODE, IPOS)                                    BOUND..........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BOUND..........800
      CHARACTER INTFIL*1000                                              BOUND..........900
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     BOUND.........1000
      DIMENSION IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN)                BOUND.........1100
      DIMENSION INERR(10),RLERR(10)                                      BOUND.........1200
      DIMENSION KTYPE(2)                                                 BOUND.........1300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BOUND.........1400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BOUND.........1500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BOUND.........1600
     1   NSOP,NSOU,NBCN                                                  BOUND.........1700
      COMMON /FNAMES/ UNAME,FNAME                                        BOUND.........1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     BOUND.........1900
C RBW
      INTEGER IERROR
	INTEGER IBOUSZ, IPOS
	INTEGER IBNODE(IBOUSZ)
C RBW
C                                                                        BOUND.........2000
C                                                                        BOUND.........2100
      IPBCT=1                                                            BOUND.........2200
      IUBCT=1                                                            BOUND.........2300
      IP=0                                                               BOUND.........2400
      IPU=0                                                              BOUND.........2500
C RBW
      IPOS = IPOS + 1
	IBNODE(IPOS) = NPBC
C RBW
!      WRITE(K3,50)                                                       BOUND.........2600
!   50 FORMAT('1'////11X,'B O U N D A R Y   C O N D I T I O N S')         BOUND.........2700
      IF(NPBC.EQ.0) GOTO 400                                             BOUND.........2800
!      WRITE(K3,100)                                                      BOUND.........2900
!  100 FORMAT(//11X,'**** NODES AT WHICH PRESSURES ARE',                  BOUND.........3000
!     1   ' SPECIFIED ****'/)                                             BOUND.........3100
!      IF(ME) 107,107,114                                                 BOUND.........3200
!  107 WRITE(K3,108)                                                      BOUND.........3300
!  108 FORMAT(11X,'     (AS WELL AS SOLUTE CONCENTRATION OF ANY'          BOUND.........3400
!     1   /16X,' FLUID INFLOW WHICH MAY OCCUR AT THE POINT'               BOUND.........3500
!     2   /16X,' OF SPECIFIED PRESSURE)'//12X,'NODE',18X,'PRESSURE',      BOUND.........3600
!     3   13X,'CONCENTRATION'//)                                          BOUND.........3700
!      GOTO 125                                                           BOUND.........3800
!  114 WRITE(K3,115)                                                      BOUND.........3900
!  115 FORMAT(11X,'     (AS WELL AS TEMPERATURE {DEGREES CELSIUS} OF ANY' BOUND.........4000
!     1   /16X,' FLUID INFLOW WHICH MAY OCCUR AT THE POINT'               BOUND.........4100
!     2   /16X,' OF SPECIFIED PRESSURE)'//12X,'NODE',18X,                 BOUND.........4200
!     2   'PRESSURE',13X,'  TEMPERATURE'//)                               BOUND.........4300
C                                                                        BOUND.........4400
C.....INPUT DATASET 19:  DATA FOR SPECIFIED PRESSURE NODES               BOUND.........4500
  125 IPU=IPU+1                                                          BOUND.........4600
      ERRCOD = 'REA-INP-19'                                              BOUND.........4700
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    BOUND.........4800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                                BOUND.........4900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                 BOUND.........5000
	   RETURN
	ENDIF
      IDUMA = IABS(IDUM)                                                 BOUND.........5100
      IF (IDUM.EQ.0) THEN                                                BOUND.........5200
         GOTO 180                                                        BOUND.........5300
      ELSE IF (IDUMA.GT.NN) THEN                                         BOUND.........5400
         ERRCOD = 'INP-19-1'                                             BOUND.........5500
         INERR(1) = IDUMA                                                BOUND.........5600
         INERR(2) = NN                                                   BOUND.........5700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        BOUND.........5800
	      RETURN
      ELSE IF (IPU.GT.NPBC) THEN                                         BOUND.........5900
         GOTO 125                                                        BOUND.........6000
      END IF                                                             BOUND.........6100
      IPBC(IPU) = IDUM                                                   BOUND.........6200
C RBW
	IF (IPBC(IPU).GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IPBC(IPU) - 1                                                
	ELSE IF (IPBC(IPU).lT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = -IPBC(IPU) - 1                                                
	ENDIF
C RBW
      IF (IPBC(IPU).GT.0) THEN                                           BOUND.........6300
         ERRCOD = 'REA-INP-19'                                           BOUND.........6400
         READ(INTFIL,*,IOSTAT=INERR(1)) IPBC(IPU),PBC(IPU),UBC(IPU)      BOUND.........6500
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)             BOUND.........6600
	      RETURN
	   ENDIF
!         WRITE(K3,160) IPBC(IPU),PBC(IPU),UBC(IPU)                       BOUND.........6700
      ELSE IF (IPBC(IPU).LT.0) THEN                                      BOUND.........6800
         IPBCT = -1                                                      BOUND.........6900
!         WRITE(K3,160) IPBC(IPU)                                         BOUND.........7000
      ELSE                                                               BOUND.........7100
         GOTO 180                                                        BOUND.........7200
      END IF                                                             BOUND.........7300
!  160 FORMAT(7X,I9,6X,1PE20.13,6X,1PE20.13)                              BOUND.........7400
      GOTO 125                                                           BOUND.........7500
  180 IPU=IPU-1                                                          BOUND.........7600
      IP=IPU                                                             BOUND.........7700
      IF(IP.EQ.NPBC) GOTO 200                                            BOUND.........7800
      ERRCOD = 'INP-3,19-1'                                              BOUND.........7900
      INERR(1) = IP                                                      BOUND.........8000
      INERR(2) = NPBC                                                    BOUND.........8100
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           BOUND.........8200
	      RETURN
  200 IF(IPBCT.NE.-1) GOTO 400                                           BOUND.........8300
!      IF(ME) 205,205,215                                                 BOUND.........8400
!  205 WRITE(K3,206)                                                      BOUND.........8500
!  206 FORMAT(//12X,'TIME-DEPENDENT SPECIFIED PRESSURE'/12X,'OR INFLOW ', BOUND.........8600
!     1   'CONCENTRATION INDICATED'/12X,'BY NEGATIVE NODE NUMBER')        BOUND.........8700
!      GOTO 400                                                           BOUND.........8800
!  215 WRITE(K3,216)                                                      BOUND.........8900
!  216 FORMAT(//11X,'TIME-DEPENDENT SPECIFIED PRESSURE'/12X,'OR INFLOW ', BOUND.........9000
!     1   'TEMPERATURE INDICATED'/12X,'BY NEGATIVE NODE NUMBER')          BOUND.........9100
C RBW
  400 IF(NUBC.EQ.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = 0                                                
      ENDIF                                                             
C RBW
      IF(NUBC.EQ.0) GOTO 6000                                            BOUND.........9200
C                                                                        BOUND.........9300
!      IF(ME) 500,500,550                                                 BOUND.........9400
!  500 WRITE(K3,1000)                                                     BOUND.........9500
! 1000 FORMAT(////11X,'**** NODES AT WHICH SOLUTE CONCENTRATIONS ARE ',   BOUND.........9600
!     1   'SPECIFIED TO BE INDEPENDENT OF LOCAL FLOWS AND FLUID SOURCES', BOUND.........9700
!     2   ' ****'//12X,'NODE',13X,'CONCENTRATION'//)                      BOUND.........9800
!      GOTO 1125                                                          BOUND.........9900
!  550 WRITE(K3,1001)                                                     BOUND........10000
! 1001 FORMAT(////11X,'**** NODES AT WHICH TEMPERATURES ARE ',            BOUND........10100
!     1   'SPECIFIED TO BE INDEPENDENT OF LOCAL FLOWS AND FLUID SOURCES', BOUND........10200
!     2   ' ****'//12X,'NODE',15X,'TEMPERATURE'//)                        BOUND........10300
C                                                                        BOUND........10400
C.....INPUT DATASET 20:  DATA FOR SPECIFIED CONCENTRATION OR             BOUND........10500
C        TEMPERATURE NODES                                               BOUND........10600
C RBW
      IPOS = IPOS + 1
	IBNODE(IPOS) = NUBC
C RBW
 1125 IPU=IPU+1                                                          BOUND........10700
      ERRCOD = 'REA-INP-20'                                              BOUND........10800
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    BOUND........10900
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                                BOUND........11000
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                BOUND........11100
	   RETURN
	ENDIF
      IDUMA = IABS(IDUM)                                                 BOUND........11200
      IF (IDUM.EQ.0) THEN                                                BOUND........11300
         GOTO 1180                                                       BOUND........11400
      ELSE IF (IDUMA.GT.NN) THEN                                         BOUND........11500
         ERRCOD = 'INP-20-1'                                             BOUND........11600
         INERR(1) = IDUMA                                                BOUND........11700
         INERR(2) = NN                                                   BOUND........11800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        BOUND........11900
	      RETURN
      ELSE IF (IPU.GT.NPBC+NUBC) THEN                                    BOUND........12000
         GOTO 1125                                                       BOUND........12100
      END IF                                                             BOUND........12200
      IUBC(IPU) = IDUM                                                   BOUND........12300
C RBW
	IF (IUBC(IPU).GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IUBC(IPU) - 1                                                
	ELSE IF (IUBC(IPU).lT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = -IUBC(IPU) - 1                                                
	ENDIF
C RBW
      IF (IUBC(IPU).GT.0) THEN                                           BOUND........12400
         ERRCOD = 'REA-INP-20'                                           BOUND........12500
         READ(INTFIL,*,IOSTAT=INERR(1)) IUBC(IPU),UBC(IPU)               BOUND........12600
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)              BOUND........12700
	      RETURN
	   ENDIF
!         WRITE(K3,1150) IUBC(IPU),UBC(IPU)                               BOUND........12800
      ELSE IF (IUBC(IPU).LT.0) THEN                                      BOUND........12900
         IUBCT = -1                                                      BOUND........13000
!         WRITE(K3,1150) IUBC(IPU)                                        BOUND........13100
      ELSE                                                               BOUND........13200
         GOTO 1180                                                       BOUND........13300
      END IF                                                             BOUND........13400
! 1150 FORMAT(11X,I9,6X,1PE20.13)                                         BOUND........13500
      GOTO 1125                                                          BOUND........13600
 1180 IPU=IPU-1                                                          BOUND........13700
      IU=IPU-IP                                                          BOUND........13800
      IF(IU.EQ.NUBC) GOTO 1200                                           BOUND........13900
      IF (ME.EQ.1) THEN                                                  BOUND........14000
         ERRCOD = 'INP-3,20-2'                                           BOUND........14100
      ELSE                                                               BOUND........14200
         ERRCOD = 'INP-3,20-1'                                           BOUND........14300
      END IF                                                             BOUND........14400
      INERR(1) = IU                                                      BOUND........14500
      INERR(2) = NUBC                                                    BOUND........14600
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           BOUND........14700
	      RETURN
 1200 CONTINUE
! 1200 IF(IUBCT.NE.-1) GOTO 6000                                          BOUND........14800
!      IF(ME) 1205,1205,1215                                              BOUND........14900
! 1205 WRITE(K3,1206)                                                     BOUND........15000
! 1206 FORMAT(//12X,'TIME-DEPENDENT SPECIFIED CONCENTRATION'/12X,'IS ',   BOUND........15100
!     1   'INDICATED BY NEGATIVE NODE NUMBER')                            BOUND........15200
!      GOTO 6000                                                          BOUND........15300
! 1215 WRITE(K3,1216)                                                     BOUND........15400
! 1216 FORMAT(//11X,'TIME-DEPENDENT SPECIFIED TEMPERATURE'/12X,'IS ',     BOUND........15500
!     1   'INDICATED BY NEGATIVE NODE NUMBER')                            BOUND........15600
!C                                                                        BOUND........15700
 6000 CONTINUE
! 6000 IF(IPBCT.EQ.-1.OR.IUBCT.EQ.-1) WRITE(K3,7000)                      BOUND........15800
! 7000 FORMAT(////11X,'THE SPECIFIED TIME VARIATIONS ARE ',               BOUND........15900
!     1   'USER-PROGRAMMED IN SUBROUTINE  B C T I M E .')                 BOUND........16000
C                                                                        BOUND........16100
C                                                                        BOUND........16200
      RETURN                                                             BOUND........16300
      END                                                                BOUND........16400
C                                                                        BOUND........16500
C     SUBROUTINE        B  U  D  G  E  T           SUTRA VERSION 2.1     BUDGET.........100
C                                                                        BUDGET.........200
C *** PURPOSE :                                                          BUDGET.........300
C ***  TO CALCULATE AND OUTPUT FLUID MASS AND SOLUTE MASS OR             BUDGET.........400
C ***  ENERGY BUDGETS.                                                   BUDGET.........500
C                                                                        BUDGET.........600
C     SUBROUTINE        C  O  N  N  E  C           SUTRA VERSION 2.1     CONNEC.........100
C                                                                        CONNEC.........200
C *** PURPOSE :                                                          CONNEC.........300
C ***  TO READ, ORGANIZE, AND CHECK DATA ON NODE INCIDENCES.             CONNEC.........400
C                                                                        CONNEC.........500
      SUBROUTINE CONNEC(IN, IERROR)                                              CONNEC.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                CONNEC.........700
      CHARACTER INTFIL*1000                                              CONNEC.........800
      CHARACTER CDUM10*10                                                CONNEC.........900
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     CONNEC........1000
      DIMENSION IN(NIN)                                                  CONNEC........1100
      DIMENSION IIN(8)                                                   CONNEC........1200
      DIMENSION INERR(10),RLERR(10)                                      CONNEC........1300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              CONNEC........1400
     1   NSOP,NSOU,NBCN                                                  CONNEC........1500
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        CONNEC........1600
      COMMON /FNAMES/ UNAME,FNAME                                        CONNEC........1700
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     CONNEC........1800
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     CONNEC........1900
     1   KSCRN,KPAUSE                                                    CONNEC........2000
C                                                                        CONNEC........2100
      IPIN=0                                                             CONNEC........2200
!      IF(KINCID.EQ.0) WRITE(K3,1)                                        CONNEC........2300
!    1 FORMAT('1'////11X,'M E S H   C O N N E C T I O N   D A T A'//      CONNEC........2400
!     1   16X,'PRINTOUT OF NODAL INCIDENCES CANCELLED.')                  CONNEC........2500
!      IF(KINCID.EQ.+1) WRITE(K3,2)                                       CONNEC........2600
!    2 FORMAT('1'////11X,'M E S H   C O N N E C T I O N   D A T A',       CONNEC........2700
!     1   ///11X,'**** NODAL INCIDENCES ****'///)                         CONNEC........2800
C                                                                        CONNEC........2900
C.....INPUT DATASET 22 AND CHECK FOR ERRORS                              CONNEC........3000
      ERRCOD = 'REA-INP-22'                                              CONNEC........3100
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    CONNEC........3200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                              CONNEC........3300
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                CONNEC........3400
	   RETURN
	ENDIF
      IF (CDUM10.NE.'INCIDENCE ') THEN                                   CONNEC........3500
         ERRCOD = 'INP-22-1'                                             CONNEC........3600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                        CONNEC........3700
	      RETURN
      END IF                                                             CONNEC........3800
      DO 1000 L=1,NE                                                     CONNEC........3900
      ERRCOD = 'REA-INP-22'                                              CONNEC........4000
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    CONNEC........4100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) LL,(IIN(II),II=1,N48)               CONNEC........4200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                CONNEC........4300
	   RETURN
	ENDIF
C.....PREPARE NODE INCIDENCE LIST FOR MESH, IN.                          CONNEC........4400
      DO 5 II=1,N48                                                      CONNEC........4500
      III=II+(L-1)*N48                                                   CONNEC........4600
    5 IN(III)=IIN(II)                                                    CONNEC........4700
      IF(IABS(LL).EQ.L) GOTO 500                                         CONNEC........4800
      ERRCOD = 'INP-22-2'                                                CONNEC........4900
      INERR(1) = LL                                                      CONNEC........5000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           CONNEC........5100
	      RETURN
C                                                                        CONNEC........5200
C                                                                        CONNEC........5300
  500 M1=(L-1)*N48+1                                                     CONNEC........5400
      M8=M1+N48-1                                                        CONNEC........5500
!      IF(KINCID.EQ.0) GOTO 1000                                          CONNEC........5600
!      WRITE(K3,650) L,(IN(M),M=M1,M8)                                    CONNEC........5700
!  650 FORMAT(11X,'ELEMENT',I9,5X,' NODES AT : ',6X,'CORNERS ',           CONNEC........5800
!     1   5('*'),8I9,1X,5('*'))                                           CONNEC........5900
C                                                                        CONNEC........6000
 1000 CONTINUE                                                           CONNEC........6100
C                                                                        CONNEC........6200
C                                                                        CONNEC........6300
 5000 RETURN                                                             CONNEC........6400
      END                                                                CONNEC........6500
C                                                                        CONNEC........6600
C     FUNCTION          C  U  T  S  M  L           SUTRA VERSION 2.1     CUTSML.........100
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
C     SUBROUTINE        D  I  M  W  R  K           SUTRA VERSION 2.1     DIMWRK.........100
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
C     SUBROUTINE        D  I  S  P  R  3           SUTRA VERSION 2.1     DISPR3.........100
C                                                                        DISPR3.........200
C *** PURPOSE :                                                          DISPR3.........300
C ***  TO COMPUTE THE COMPONENTS OF THE 3D DISPERSION TENSOR IN          DISPR3.........400
C ***  X,Y,Z-COORDINATES USING AN AD HOC, 3D ANISOTROPIC DISPERSION      DISPR3.........500
C ***  MODEL.                                                            DISPR3.........600
C                                                                        DISPR3.........700
C     FUNCTION          D  P  3  S  T  R           SUTRA VERSION 2.1     DP3STR.........100
C                                                                        DP3STR.........200
C *** PURPOSE :                                                          DP3STR.........300
C ***  TO RETURN THREE DOUBLE-PRECISION NUMBERS IN THE FORM OF A         DP3STR.........400
C ***  STRING.  THE THREE NUMBERS ARE PASSED IN THROUGH ARRAY DPA        DP3STR.........500
C ***  AND ARE ROUNDED USING FUNCTION CUTSML IN PREPARATION FOR OUTPUT.  DP3STR.........600
C                                                                        DP3STR.........700
!      FUNCTION DP3STR(DPA)                                               DP3STR.........800
!      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               DP3STR.........900
!      DIMENSION DPA(3)                                                   DP3STR........1000
!      CHARACTER DP3STR*45                                                DP3STR........1100
C                                                                        DP3STR........1200
C.....WRITE NUMBERS TO STRING                                            DP3STR........1300
!      WRITE(UNIT=DP3STR,FMT="(3(1PE15.7))")                              DP3STR........1400
!     1   CUTSML(DPA(1)), CUTSML(DPA(2)), CUTSML(DPA(3))                  DP3STR........1500
C                                                                        DP3STR........1600
!      RETURN                                                             DP3STR........1700
!      END                                                                DP3STR........1800
C                                                                        DP3STR........1900
C     SUBROUTINE        E  L  E  M  N  2           SUTRA VERSION 2.1     ELEMN2.........100
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......32500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......32600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......32700
C     SUBROUTINE        E  L  E  M  N  3           SUTRA VERSION 2.1     ELEMN3.........100
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
C     SUBROUTINE        F  I  N  D  L  2           SUTRA VERSION 2.1     FINDL2.........100
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
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,
     1   NSOP,NSOU,NBCN
      DIMENSION IN(NE*4)                                                 FINDL2........1300
      DIMENSION X(NN), Y(NN)                                             FINDL2........1400
      DATA TOL /0.001/, ITRMAX /25/, EPSILON /0.001/                     FINDL2........1500
C                                                                        FINDL2........1600
C.....DEFINE OPE = (1. + EPSILON) FOR CONVENIENCE.                       FINDL2........1700
      OPE = 1D0 + EPSILON                                                FINDL2........1800
C                                                                        FINDL2........1900
C.....SET CORNER COORDINATES.                                            FINDL2........2000
      M0 = (LL - 1)*4                                                    FINDL2........2100
      X1 = X(IN(M0+1))                                                   FINDL2........2200
      X2 = X(IN(M0+2))                                                   FINDL2........2300
      X3 = X(IN(M0+3))                                                   FINDL2........2400
      X4 = X(IN(M0+4))                                                   FINDL2........2500
      Y1 = Y(IN(M0+1))                                                   FINDL2........2600
      Y2 = Y(IN(M0+2))                                                   FINDL2........2700
      Y3 = Y(IN(M0+3))                                                   FINDL2........2800
      Y4 = Y(IN(M0+4))                                                   FINDL2........2900
C                                                                        FINDL2........3000
C.....CALCULATE COEFFICIENTS.                                            FINDL2........3100
      AX = +X1+X2+X3+X4                                                  FINDL2........3200
      BX = -X1+X2+X3-X4                                                  FINDL2........3300
      CX = -X1-X2+X3+X4                                                  FINDL2........3400
      DX = +X1-X2+X3-X4                                                  FINDL2........3500
      AY = +Y1+Y2+Y3+Y4                                                  FINDL2........3600
      BY = -Y1+Y2+Y3-Y4                                                  FINDL2........3700
      CY = -Y1-Y2+Y3+Y4                                                  FINDL2........3800
      DY = +Y1-Y2+Y3-Y4                                                  FINDL2........3900
                                                                         FINDL2........4000
C                                                                        FINDL2........4100
C.....INITIAL GUESS OF ZERO FOR XSI AND ETA.                             FINDL2........4200
      XSI=0.0                                                            FINDL2........4300
      ETA=0.0                                                            FINDL2........4400
C                                                                        FINDL2........4500
C.....ITERATION LOOP TO SOLVE FOR LOCAL COORDINATES.                     FINDL2........4600
C                                                                        FINDL2........4700
      DO 800 I=1,ITRMAX                                                  FINDL2........4800
C                                                                        FINDL2........4900
         F10 = AX - 4.*XK + BX*XSI + CX*ETA + DX*XSI*ETA                 FINDL2........5000
         F20 = AY - 4.*YK + BY*XSI + CY*ETA + DY*XSI*ETA                 FINDL2........5100
         FP11 = BX + DX*ETA                                              FINDL2........5200
         FP12 = CX + DX*XSI                                              FINDL2........5300
         FP21 = BY + DY*ETA                                              FINDL2........5400
         FP22 = CY + DY*XSI                                              FINDL2........5500
C                                                                        FINDL2........5600
         DETXSI = -F10*FP22 + F20*FP12                                   FINDL2........5700
         DETETA = -F20*FP11 + F10*FP21                                   FINDL2........5800
         DETERM = FP11*FP22 - FP12*FP21                                  FINDL2........5900
         DELXSI = DETXSI/DETERM                                          FINDL2........6000
         DELETA = DETETA/DETERM                                          FINDL2........6100
C                                                                        FINDL2........6200
         XSI = XSI + DELXSI                                              FINDL2........6300
         ETA = ETA + DELETA                                              FINDL2........6400
C                                                                        FINDL2........6500
C........STOP ITERATING IF CHANGE IN XSI AND ETA < TOL.                  FINDL2........6600
         IF ((ABS(DELXSI).LT.TOL).AND.(ABS(DELETA).LT.TOL)) GOTO 900     FINDL2........6700
C                                                                        FINDL2........6800
  800 CONTINUE                                                           FINDL2........6900
C                                                                        FINDL2........7000
C.....ITERATONS FAILED TO CONVERGE.  SET INOUT = 99 AND RETURN.          FINDL2........7100
      INOUT = 99                                                         FINDL2........7200
      GOTO 1000                                                          FINDL2........7300
C                                                                        FINDL2........7400
C.....ITERATIONS CONVERGED.  IF POINT IS INSIDE THE ELEMENT,             FINDL2........7500
C        SET INOUT = 1.  IF OUTSIDE, SET INOUT = 0.                      FINDL2........7600
  900 INOUT = 1                                                          FINDL2........7700
      IF ((ABS(XSI).GT.OPE).OR.(ABS(ETA).GT.OPE)) INOUT = 0              FINDL2........7800
C                                                                        FINDL2........7900
 1000 RETURN                                                             FINDL2........8000
      END                                                                FINDL2........8100
C                                                                        FINDL2........8200
C     SUBROUTINE        F  I  N  D  L  3           SUTRA VERSION 2.1     FINDL3.........100
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
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,
     1   NSOP,NSOU,NBCN
      DIMENSION IN(NE*8)                                                 FINDL3........1300
      DIMENSION X(NN), Y(NN), Z(NN)                                      FINDL3........1400
      DATA TOL /0.001/, ITRMAX /25/, EPSILON /0.001/                     FINDL3........1500
C                                                                        FINDL3........1600
C.....DEFINE OPE = (1. + EPSILON) FOR CONVENIENCE.                       FINDL3........1700
      OPE = 1D0 + EPSILON                                                FINDL3........1800
C                                                                        FINDL3........1900
C.....SET CORNER COORDINATES.                                            FINDL3........2000
      M0 = (LL - 1)*8                                                    FINDL3........2100
      X1 = X(IN(M0+1))                                                   FINDL3........2200
      X2 = X(IN(M0+2))                                                   FINDL3........2300
      X3 = X(IN(M0+3))                                                   FINDL3........2400
      X4 = X(IN(M0+4))                                                   FINDL3........2500
      X5 = X(IN(M0+5))                                                   FINDL3........2600
      X6 = X(IN(M0+6))                                                   FINDL3........2700
      X7 = X(IN(M0+7))                                                   FINDL3........2800
      X8 = X(IN(M0+8))                                                   FINDL3........2900
      Y1 = Y(IN(M0+1))                                                   FINDL3........3000
      Y2 = Y(IN(M0+2))                                                   FINDL3........3100
      Y3 = Y(IN(M0+3))                                                   FINDL3........3200
      Y4 = Y(IN(M0+4))                                                   FINDL3........3300
      Y5 = Y(IN(M0+5))                                                   FINDL3........3400
      Y6 = Y(IN(M0+6))                                                   FINDL3........3500
      Y7 = Y(IN(M0+7))                                                   FINDL3........3600
      Y8 = Y(IN(M0+8))                                                   FINDL3........3700
      Z1 = Z(IN(M0+1))                                                   FINDL3........3800
      Z2 = Z(IN(M0+2))                                                   FINDL3........3900
      Z3 = Z(IN(M0+3))                                                   FINDL3........4000
      Z4 = Z(IN(M0+4))                                                   FINDL3........4100
      Z5 = Z(IN(M0+5))                                                   FINDL3........4200
      Z6 = Z(IN(M0+6))                                                   FINDL3........4300
      Z7 = Z(IN(M0+7))                                                   FINDL3........4400
      Z8 = Z(IN(M0+8))                                                   FINDL3........4500
C                                                                        FINDL3........4600
C.....CALCULATE COEFFICIENTS.                                            FINDL3........4700
      AX = +X1+X2+X3+X4+X5+X6+X7+X8                                      FINDL3........4800
      BX = -X1+X2+X3-X4-X5+X6+X7-X8                                      FINDL3........4900
      CX = -X1-X2+X3+X4-X5-X6+X7+X8                                      FINDL3........5000
      DX = -X1-X2-X3-X4+X5+X6+X7+X8                                      FINDL3........5100
      EX = +X1-X2+X3-X4+X5-X6+X7-X8                                      FINDL3........5200
      FX = +X1-X2-X3+X4-X5+X6+X7-X8                                      FINDL3........5300
      GX = +X1+X2-X3-X4-X5-X6+X7+X8                                      FINDL3........5400
      HX = -X1+X2-X3+X4+X5-X6+X7-X8                                      FINDL3........5500
      AY = +Y1+Y2+Y3+Y4+Y5+Y6+Y7+Y8                                      FINDL3........5600
      BY = -Y1+Y2+Y3-Y4-Y5+Y6+Y7-Y8                                      FINDL3........5700
      CY = -Y1-Y2+Y3+Y4-Y5-Y6+Y7+Y8                                      FINDL3........5800
      DY = -Y1-Y2-Y3-Y4+Y5+Y6+Y7+Y8                                      FINDL3........5900
      EY = +Y1-Y2+Y3-Y4+Y5-Y6+Y7-Y8                                      FINDL3........6000
      FY = +Y1-Y2-Y3+Y4-Y5+Y6+Y7-Y8                                      FINDL3........6100
      GY = +Y1+Y2-Y3-Y4-Y5-Y6+Y7+Y8                                      FINDL3........6200
      HY = -Y1+Y2-Y3+Y4+Y5-Y6+Y7-Y8                                      FINDL3........6300
      AZ = +Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8                                      FINDL3........6400
      BZ = -Z1+Z2+Z3-Z4-Z5+Z6+Z7-Z8                                      FINDL3........6500
      CZ = -Z1-Z2+Z3+Z4-Z5-Z6+Z7+Z8                                      FINDL3........6600
      DZ = -Z1-Z2-Z3-Z4+Z5+Z6+Z7+Z8                                      FINDL3........6700
      EZ = +Z1-Z2+Z3-Z4+Z5-Z6+Z7-Z8                                      FINDL3........6800
      FZ = +Z1-Z2-Z3+Z4-Z5+Z6+Z7-Z8                                      FINDL3........6900
      GZ = +Z1+Z2-Z3-Z4-Z5-Z6+Z7+Z8                                      FINDL3........7000
      HZ = -Z1+Z2-Z3+Z4+Z5-Z6+Z7-Z8                                      FINDL3........7100
C                                                                        FINDL3........7200
C.....INITIAL GUESS OF ZERO FOR XSI, ETA, AND ZETA.                      FINDL3........7300
      XSI=0.0                                                            FINDL3........7400
      ETA=0.0                                                            FINDL3........7500
      ZET=0.0                                                            FINDL3........7600
C                                                                        FINDL3........7700
C.....ITERATION LOOP TO SOLVE FOR LOCAL COORDINATES.                     FINDL3........7800
C                                                                        FINDL3........7900
      DO 800 I=1,ITRMAX                                                  FINDL3........8000
C                                                                        FINDL3........8100
         F10 = AX - 8.*XK + BX*XSI + CX*ETA + DX*ZET + EX*XSI*ETA        FINDL3........8200
     1        + FX*XSI*ZET + GX*ETA*ZET + HX*XSI*ETA*ZET                 FINDL3........8300
         F20 = AY - 8.*YK + BY*XSI + CY*ETA + DY*ZET + EY*XSI*ETA        FINDL3........8400
     1        + FY*XSI*ZET + GY*ETA*ZET + HY*XSI*ETA*ZET                 FINDL3........8500
         F30 = AZ - 8.*ZK + BZ*XSI + CZ*ETA + DZ*ZET + EZ*XSI*ETA        FINDL3........8600
     1        + FZ*XSI*ZET + GZ*ETA*ZET + HZ*XSI*ETA*ZET                 FINDL3........8700
         FP11 = BX + EX*ETA + FX*ZET + HX*ETA*ZET                        FINDL3........8800
         FP12 = CX + EX*XSI + GX*ZET + HX*XSI*ZET                        FINDL3........8900
         FP13 = DX + FX*XSI + GX*ETA + HX*XSI*ETA                        FINDL3........9000
         FP21 = BY + EY*ETA + FY*ZET + HY*ETA*ZET                        FINDL3........9100
         FP22 = CY + EY*XSI + GY*ZET + HY*XSI*ZET                        FINDL3........9200
         FP23 = DY + FY*XSI + GY*ETA + HY*XSI*ETA                        FINDL3........9300
         FP31 = BZ + EZ*ETA + FZ*ZET + HZ*ETA*ZET                        FINDL3........9400
         FP32 = CZ + EZ*XSI + GZ*ZET + HZ*XSI*ZET                        FINDL3........9500
         FP33 = DZ + FZ*XSI + GZ*ETA + HZ*XSI*ETA                        FINDL3........9600
C                                                                        FINDL3........9700
         S11 = FP22*FP33 - FP32*FP23                                     FINDL3........9800
         S12 = FP21*FP33 - FP31*FP23                                     FINDL3........9900
         S13 = FP21*FP32 - FP31*FP22                                     FINDL3.......10000
         CF12 = -F20*FP33 + F30*FP23                                     FINDL3.......10100
         CF34 = -F20*FP32 + F30*FP22                                     FINDL3.......10200
         CF43 = -CF34                                                    FINDL3.......10300
         CF56 = -F30*FP21 + F20*FP31                                     FINDL3.......10400
C                                                                        FINDL3.......10500
         DETXSI = -F10*S11 - FP12*CF12 + FP13*CF34                       FINDL3.......10600
         DETETA = FP11*CF12 + F10*S12 + FP13*CF56                        FINDL3.......10700
         DETZET = FP11*CF43 - FP12*CF56 - F10*S13                        FINDL3.......10800
         DETERM = FP11*S11 - FP12*S12 + FP13*S13                         FINDL3.......10900
         DELXSI = DETXSI/DETERM                                          FINDL3.......11000
         DELETA = DETETA/DETERM                                          FINDL3.......11100
         DELZET = DETZET/DETERM                                          FINDL3.......11200
C                                                                        FINDL3.......11300
         XSI = XSI + DELXSI                                              FINDL3.......11400
         ETA = ETA + DELETA                                              FINDL3.......11500
         ZET = ZET + DELZET                                              FINDL3.......11600
C                                                                        FINDL3.......11700
C........STOP ITERATING IF CHANGE IN XSI, ETA, AND ZETA < TOL.           FINDL3.......11800
         IF ((ABS(DELXSI).LT.TOL).AND.(ABS(DELETA).LT.TOL).AND.          FINDL3.......11900
     1       (ABS(DELZET).LT.TOL)) GOTO 900                              FINDL3.......12000
C                                                                        FINDL3.......12100
  800 CONTINUE                                                           FINDL3.......12200
C                                                                        FINDL3.......12300
C.....ITERATONS FAILED TO CONVERGE.  SET INOUT = 99 AND RETURN.          FINDL3.......12400
      INOUT = 99                                                         FINDL3.......12500
      GOTO 1000                                                          FINDL3.......12600
C                                                                        FINDL3.......12700
C.....ITERATIONS CONVERGED.  IF POINT IS INSIDE THE ELEMENT,             FINDL3.......12800
C        SET INOUT = 1.  IF OUTSIDE, SET INOUT = 0.                      FINDL3.......12900
  900 INOUT = 1                                                          FINDL3.......13000
      IF ((ABS(XSI).GT.OPE).OR.(ABS(ETA).GT.OPE).OR.(ABS(ZET).GT.OPE))   FINDL3.......13100
     1   INOUT = 0                                                       FINDL3.......13200
C                                                                        FINDL3.......13300
 1000 RETURN                                                             FINDL3.......13400
      END                                                                FINDL3.......13500
C                                                                        FINDL3.......13600
C     SUBROUTINE        F  O  P  E  N              SUTRA VERSION 2.1     FOPEN..........100
C                                                                        FOPEN..........200
C *** PURPOSE :                                                          FOPEN..........300
C ***  OPENS FILES FOR SUTRA SIMULATION.  READS AND PROCESSES FILE       FOPEN..........400
C ***  SPECIFICATIONS FROM "SUTRA.FIL" AND OPENS INPUT AND OUTPUT FILES. FOPEN..........500
C                                                                        FOPEN..........600
      SUBROUTINE FOPEN(IERROR)                                                 FOPEN..........700
      USE EXPINT                                                         FOPEN..........800
      USE SCHDEF                                                         FOPEN..........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FOPEN.........1000
      PARAMETER (IUNMIN=11)                                              FOPEN.........1100
      CHARACTER*80 FT,FN,UNAME,FNAME,ENAME,ENDEF,FTSTR                   FOPEN.........1200
      CHARACTER*80 FNROOT,FNEXTN                                         FOPEN.........1300
      CHARACTER*80 ERRCOD,CHERR(10)                                      FOPEN.........1400
      CHARACTER*8 VERNUM, VERNIN                                         FOPEN.........1500
      CHARACTER INTFIL*1000                                              FOPEN.........1600
      LOGICAL IS                                                         FOPEN.........1700
      LOGICAL EXST                                                       FOPEN.........1800
      LOGICAL ONCEFO                                                     FOPEN.........1900
      DIMENSION FTYPE(0:8),FNAME(0:8),IUNIT(0:8)                         FOPEN.........2000
      DIMENSION FTSTR(0:8)                                               FOPEN.........2100
      DIMENSION INERR(10),RLERR(10)                                      FOPEN.........2200
      COMMON /FNAMES/ UNAME,FNAME                                        FOPEN.........2300
      COMMON /FO/ONCEFO                                                  FOPEN.........2400
      COMMON /FUNITA/ IUNIT                                              FOPEN.........2500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     FOPEN.........2600
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      FOPEN.........2700
      COMMON /SCH/ NSCH,ISCHTS                                           FOPEN.........2800
      COMMON /VER/ VERNUM, VERNIN                                        FOPEN.........2900
      DATA (FTSTR(NFT),NFT=0,8)/'SMY','INP','ICS','LST','RST',           FOPEN.........3000
     1   'NOD','ELE','OBS','OBC'/                                        FOPEN.........3100
C                                                                        FOPEN.........3200
C.....IF THIS IS THE FIRST PASS, READ AND PROCESS FILE SPECIFICATIONS    FOPEN.........3300
C        FROM "SUTRA.FIL" AND OPEN ALL OUTPUT FILES EXCEPT OBSERVATION   FOPEN.........3400
C        OUTPUT.  OBSERVATION OUTPUT FILES ARE CREATED ON THE SECOND     FOPEN.........3500
C        PASS, AFTER DATASET 8D HAS BEEN READ.                           FOPEN.........3600
      IF (.NOT.ONCEFO) THEN                                              FOPEN.........3700
C                                                                        FOPEN.........3800
C........INITIALIZE UNIT NUMBERS AND FILENAMES                           FOPEN.........3900
         K1 = -1                                                         FOPEN.........4000
         K2 = -1                                                         FOPEN.........4100
         K3 = -1                                                         FOPEN.........4200
         K4 = -1                                                         FOPEN.........4300
         K5 = -1                                                         FOPEN.........4400
         K6 = -1                                                         FOPEN.........4500
         K7 = -1                                                         FOPEN.........4600
         K8 = -1                                                         FOPEN.........4700
         DO 20 NF=0,8                                                    FOPEN.........4800
            IUNIT(NF) = -1                                               FOPEN.........4900
            FNAME(NF) = ""                                               FOPEN.........5000
   20    CONTINUE                                                        FOPEN.........5100
C                                                                        FOPEN.........5200
C........SET DEFAULT VALUES FOR THE SMY FILE.  THE DEFAULT FILE WILL     FOPEN.........5300
C           NOT ACTUALLY BE CREATED UNLESS IT IS NEEDED.                 FOPEN.........5400
         K00 = K0 + 1                                                    FOPEN.........5500
         ENDEF = 'SUTRA.SMY'                                             FOPEN.........5600
C                                                                        FOPEN.........5700
C........OPEN FILE UNIT CONTAINING UNIT NUMBERS AND FILE ASSIGNMENTS     FOPEN.........5800
         IU=K0                                                           FOPEN.........5900
         FN=UNAME                                                        FOPEN.........6000
         INQUIRE(FILE=UNAME,EXIST=IS)                                    FOPEN.........6100
         IF (IS) THEN                                                    FOPEN.........6200
            OPEN(UNIT=IU,FILE=UNAME,STATUS='OLD',FORM='FORMATTED',       FOPEN.........6300
     1         IOSTAT=KERR)                                              FOPEN.........6400
            IF(KERR.GT.0) GOTO 9000                                      FOPEN.........6500
         ELSE                                                            FOPEN.........6600
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN.........6700
	      IF (IERROR.NE.0) RETURN
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN.........6800
            GOTO 8000                                                    FOPEN.........6900
         ENDIF                                                           FOPEN.........7000
C RBW
	   k1 = K0
	   return                                                                            FOPEN.........5000
C RBW
C                                                                        FOPEN.........7100
C........READ IN UNIT NUMBERS AND FILE ASSIGNMENTS.  ASSIGN COMPATIBLE   FOPEN.........7200
C           UNIT NUMBERS.  CLOSE UNIT K0.                                FOPEN.........7300
         NFILE = 0                                                       FOPEN.........7400
         DO 90 NF=0,8                                                    FOPEN.........7500
C...........READ A FILE SPECIFICATION                                    FOPEN.........7600
            READ(K0,'(A)',IOSTAT=INERR(1),END=99) INTFIL                 FOPEN.........7700
            IF (INERR(1).NE.0) THEN                                      FOPEN.........7800
               CALL NAFU(K00,0,ENDEF,IERROR)                                    FOPEN.........7900
	         IF (IERROR.NE.0) RETURN
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN.........8000
               ERRCOD = 'REA-FIL'                                        FOPEN.........8100
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  FOPEN.........8200
	         RETURN
            END IF                                                       FOPEN.........8300
            IF (VERIFY(INTFIL,' ').EQ.0) GOTO 99                         FOPEN.........8400
            READ(INTFIL,*,IOSTAT=INERR(1)) FT, IU, FN                    FOPEN.........8500
            IF (INERR(1).NE.0) THEN                                      FOPEN.........8600
               CALL NAFU(K00,0,ENDEF,IERROR)                                    FOPEN.........8700
	         IF (IERROR.NE.0) RETURN
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN.........8800
               ERRCOD = 'REA-FIL'                                        FOPEN.........8900
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  FOPEN.........9000
	         RETURN
            END IF                                                       FOPEN.........9100
C...........CHECK FOR ILLEGAL SPECIFICATIONS                             FOPEN.........9200
            IF (FN.EQ.UNAME) THEN                                        FOPEN.........9300
               CALL NAFU(K00,0,ENDEF,IERROR)                                    FOPEN.........9400
	         IF (IERROR.NE.0) RETURN
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN.........9500
               ERRCOD = 'FIL-9'                                          FOPEN.........9600
               CHERR(1) = UNAME                                          FOPEN.........9700
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                  FOPEN.........9800
	         RETURN
            END IF                                                       FOPEN.........9900
C...........IF THE SPECIFIED UNIT NUMBER IS LESS THAN IUNMIN,            FOPEN........10000
C              SET IT TO IUNMIN                                          FOPEN........10100
            IU = MAX(IU, IUNMIN)                                         FOPEN........10200
C...........STORE THE FILE INFORMATION, CHECKING FOR INVALID AND         FOPEN........10300
C              REPEATED FILE TYPE SPECIFICATIONS AND ASSIGNING UNIT      FOPEN........10400
C              NUMBERS TO NON-OBSERVATION FILES ALONG THE WAY            FOPEN........10500
            DO 50 NFT=0,8                                                FOPEN........10600
               IF (FT.EQ.FTSTR(NFT)) THEN                                FOPEN........10700
                  IF (IUNIT(NFT).EQ.-1) THEN                             FOPEN........10800
                     IF (NFT.LE.6) CALL NAFU(IU,0,FN,IERROR)                    FOPEN........10900
	               IF (IERROR.NE.0) RETURN
                     IUNIT(NFT) = IU                                     FOPEN........11000
                     FNAME(NFT) = FN                                     FOPEN........11100
                     GOTO 60                                             FOPEN........11200
                  ELSE                                                   FOPEN........11300
                     CALL NAFU(K00,0,ENDEF,IERROR)                              FOPEN........11400
	               IF (IERROR.NE.0) RETURN
                     OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')          FOPEN........11500
                     ERRCOD = 'FIL-6'                                    FOPEN........11600
                     CHERR(1) = UNAME                                    FOPEN........11700
                     CHERR(2) = FT                                       FOPEN........11800
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            FOPEN........11900
	               RETURN
                  END IF                                                 FOPEN........12000
               END IF                                                    FOPEN........12100
   50       CONTINUE                                                     FOPEN........12200
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN........12300
	      IF (IERROR.NE.0) RETURN
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN........12400
            ERRCOD = 'FIL-5'                                             FOPEN........12500
            CHERR(1) = UNAME                                             FOPEN........12600
            CHERR(2) = FT                                                FOPEN........12700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     FOPEN........12800
	      RETURN
   60       CONTINUE                                                     FOPEN........12900
   90    CONTINUE                                                        FOPEN........13000
   99    CLOSE(K0)                                                       FOPEN........13100
C                                                                        FOPEN........13200
C........OPEN THE SMY FILE.                                              FOPEN........13300
C                                                                        FOPEN........13400
C........IF NO SMY SPECIFICATION, USE THE DEFAULT.                       FOPEN........13500
         IF (IUNIT(0).EQ.-1) THEN                                        FOPEN........13600
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN........13700
	      IF (IERROR.NE.0) RETURN
            IUNIT(0) = K00                                               FOPEN........13800
            FNAME(0) = ENDEF                                             FOPEN........13900
         END IF                                                          FOPEN........14000
         IU = IUNIT(0)                                                   FOPEN........14100
         FN = FNAME(0)                                                   FOPEN........14200
         OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',IOSTAT=KERR)              FOPEN........14300
C........IN CASE OF ERROR WHILE OPENING SMY FILE, WRITE ERROR            FOPEN........14400
C           MESSAGE TO DEFAULT FILE                                      FOPEN........14500
         IF (KERR.GT.0) THEN                                             FOPEN........14600
            CALL NAFU(K00,0,ENDEF,IERROR)                                       FOPEN........14700
	      IF (IERROR.NE.0) RETURN
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN........14800
            GOTO 9000                                                    FOPEN........14900
         END IF                                                          FOPEN........15000
C........SET K00 AND ENAME                                               FOPEN........15100
         K00 = IU                                                        FOPEN........15200
         ENAME = FN                                                      FOPEN........15300
C                                                                        FOPEN........15400
C........CHECK FOR REPEATED FILENAMES (EXCEPT OBS AND OBC FILES)         FOPEN........15500
C           AND MISSING SPECIFICATIONS FOR REQUIRED FILE TYPES           FOPEN........15600
         DO 260 NF=0,6                                                   FOPEN........15700
            IF (IUNIT(NF).EQ.-1) THEN                                    FOPEN........15800
               IF ((NF.GE.1).AND.(NF.LE.4)) THEN                         FOPEN........15900
                  ERRCOD = 'FIL-7'                                       FOPEN........16000
                  CHERR(1) = UNAME                                       FOPEN........16100
                  CHERR(2) = FTSTR(NF)                                   FOPEN........16200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               FOPEN........16300
	            RETURN
               ELSE                                                      FOPEN........16400
                  CYCLE                                                  FOPEN........16500
               END IF                                                    FOPEN........16600
            END IF                                                       FOPEN........16700
            DO 250 NF2=NF+1,6                                            FOPEN........16800
               IF (FNAME(NF2).EQ.FNAME(NF)) THEN                         FOPEN........16900
                  ERRCOD = 'FIL-4'                                       FOPEN........17000
                  CHERR(1) = UNAME                                       FOPEN........17100
                  CHERR(2) = FNAME(NF)                                   FOPEN........17200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               FOPEN........17300
	            RETURN
               END IF                                                    FOPEN........17400
  250       CONTINUE                                                     FOPEN........17500
  260    CONTINUE                                                        FOPEN........17600
C                                                                        FOPEN........17700
C........SET UNIT NUMBERS K1 - K7.  (K00 HAS BEEN SET PREVIOUSLY.)       FOPEN........17800
         K1=IUNIT(1)                                                     FOPEN........17900
         K2=IUNIT(2)                                                     FOPEN........18000
         K3=IUNIT(3)                                                     FOPEN........18100
         K4=IUNIT(4)                                                     FOPEN........18200
         K5=IUNIT(5)                                                     FOPEN........18300
         K6=IUNIT(6)                                                     FOPEN........18400
         K7=IUNIT(7)                                                     FOPEN........18500
         K8=IUNIT(8)                                                     FOPEN........18600
C                                                                        FOPEN........18700
C........CHECK FOR EXISTENCE OF INPUT FILES AND OPEN INPUT AND OUTPUT    FOPEN........18800
C           FILES (EXCEPT SMY, OBS, AND OBC)                             FOPEN........18900
         DO 300 NF=1,6                                                   FOPEN........19000
            IU=IUNIT(NF)                                                 FOPEN........19100
            FN=FNAME(NF)                                                 FOPEN........19200
            IF (IU.EQ.-1) GOTO 300                                       FOPEN........19300
            IF(NF.LE.2) THEN                                             FOPEN........19400
               INQUIRE(FILE=FN,EXIST=IS)                                 FOPEN........19500
               IF(IS) THEN                                               FOPEN........19600
                  OPEN(UNIT=IU,FILE=FN,STATUS='OLD',FORM='FORMATTED',    FOPEN........19700
     1               IOSTAT=KERR)                                        FOPEN........19800
               ELSE                                                      FOPEN........19900
                  GOTO 8000                                              FOPEN........20000
               ENDIF                                                     FOPEN........20100
            ELSE                                                         FOPEN........20200
               OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',FORM='FORMATTED',   FOPEN........20300
     1            IOSTAT=KERR)                                           FOPEN........20400
            ENDIF                                                        FOPEN........20500
            IF(KERR.GT.0) GOTO 9000                                      FOPEN........20600
  300    CONTINUE                                                        FOPEN........20700
C                                                                        FOPEN........20800
C........SET FLAG TO INDICATE THAT FIRST PASS IS COMPLETED, THEN RETURN  FOPEN........20900
         ONCEFO = .TRUE.                                                 FOPEN........21000
         RETURN                                                          FOPEN........21100
C                                                                        FOPEN........21200
      ELSE                                                               FOPEN........21300
C RBW
         RETURN
C RBW
C                                                                        FOPEN........21400
C........INITIALIZE OBSERVATION-RELATED UNIT NUMBERS AND FILENAMES       FOPEN........21500
         DO 330 NFO=1,NFLOMX                                             FOPEN........21600
            IUNIO(NFO) = -1                                              FOPEN........21700
            FNAMO(NFO) = ""                                              FOPEN........21800
  330    CONTINUE                                                        FOPEN........21900
C                                                                        FOPEN........22000
C........OPEN OBS AND OBC FILES, AUTOMATICALLY GENERATING UNIT NUMBERS   FOPEN........22100
C           AND FILENAMES                                                FOPEN........22200
C                                                                        FOPEN........22300
C........LOOP OVER THE TWO FILE TYPES                                    FOPEN........22400
         DO 400 NF=7,8                                                   FOPEN........22500
C...........IF NO FILE SPECIFICATION OF THIS TYPE, MOVE ON               FOPEN........22600
            IF (IUNIT(NF).EQ.-1) CYCLE                                   FOPEN........22700
C...........DETERMINE LENGTH OF THE SPECIFIED FILENAME AND ITS ROOT      FOPEN........22800
            LNAME = LEN_TRIM(FNAME(NF))                                  FOPEN........22900
            LROOT = SCAN(FNAME(NF),'.',BACK=.TRUE.) - 1                  FOPEN........23000
C...........SET THE ROOT NAME AND EXTENSION THAT WILL BE USED FOR FILES  FOPEN........23100
C              OF THIS TYPE                                              FOPEN........23200
            IF (LROOT.NE.-1) THEN                                        FOPEN........23300
               IF (LROOT.NE.0) THEN                                      FOPEN........23400
                  FNROOT = FNAME(NF)(1:LROOT)                            FOPEN........23500
               ELSE                                                      FOPEN........23600
                  FNROOT = "SUTRA"                                       FOPEN........23700
               END IF                                                    FOPEN........23800
               IF (LROOT.NE.LNAME-1) THEN                                FOPEN........23900
                  FNEXTN = FNAME(NF)(LROOT+1:LNAME)                      FOPEN........24000
               ELSE                                                      FOPEN........24100
                  FNEXTN = "." // FTSTR(NF)                              FOPEN........24200
               END IF                                                    FOPEN........24300
            ELSE                                                         FOPEN........24400
               IF (LNAME.NE.0) THEN                                      FOPEN........24500
                  FNROOT = FNAME(NF)                                     FOPEN........24600
               ELSE                                                      FOPEN........24700
                  FNROOT = "SUTRA"                                       FOPEN........24800
               END IF                                                    FOPEN........24900
               FNEXTN = "." // FTSTR(NF)                                 FOPEN........25000
            END IF                                                       FOPEN........25100
C...........INITIALIZE UNIT NUMBER                                       FOPEN........25200
            IUNEXT = IUNIT(NF)                                           FOPEN........25300
C...........LOOP OVER OBSERVATION OUTPUT FILES                           FOPEN........25400
            DO 380 J=1,NFLOMX                                            FOPEN........25500
               JM1 = J - 1                                               FOPEN........25600
C..............IF FILE IS NOT OF THE TYPE CURRENTLY BEING PROCESSED,     FOPEN........25700
C                 SKIP FILE                                              FOPEN........25800
               IF (OFP(J)%FRMT.NE.FTSTR(NF)) CYCLE                       FOPEN........25900
C..............CONSTRUCT FILENAME FROM ROOT NAME, SCHEDULE NAME,         FOPEN........26000
C                 AND EXTENSION                                          FOPEN........26100
               IF (SCHDLS(OFP(J)%ISCHED)%NAME.NE."-") THEN               FOPEN........26200
                  FN = TRIM(FNROOT) // "_"                               FOPEN........26300
     1               // TRIM(SCHDLS(OFP(J)%ISCHED)%NAME) // FNEXTN       FOPEN........26400
               ELSE                                                      FOPEN........26500
                  FN = TRIM(FNROOT) // FNEXTN                            FOPEN........26600
               END IF                                                    FOPEN........26700
C..............CHECK FOR DUPLICATE FILENAME AMONG NON-OBSERVATION        FOPEN........26800
C                 FILES                                                  FOPEN........26900
               DO 350 NFF=0,6                                            FOPEN........27000
                  IF (FN.EQ.FNAME(NFF)) THEN                             FOPEN........27100
                     ERRCOD = 'FIL-4'                                    FOPEN........27200
                     CHERR(1) = UNAME                                    FOPEN........27300
                     CHERR(2) = FN                                       FOPEN........27400
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            FOPEN........27500
	               RETURN
                  END IF                                                 FOPEN........27600
  350          CONTINUE                                                  FOPEN........27700
C..............CHECK FOR DUPLICATE FILENAME AMONG PREVIOUSLY DEFINED     FOPEN........27800
C                 OBSERVATION OUTPUT FILES                               FOPEN........27900
               DO 355 NJ=1,J-1                                           FOPEN........28000
                  IF (FN.EQ.FNAMO(NJ)) THEN                              FOPEN........28100
                     ERRCOD = 'FIL-4'                                    FOPEN........28200
                     CHERR(1) = UNAME                                    FOPEN........28300
                     CHERR(2) = FN                                       FOPEN........28400
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            FOPEN........28500
	               RETURN
                  END IF                                                 FOPEN........28600
  355          CONTINUE                                                  FOPEN........28700
C..............ASSIGN NEXT AVAILABLE UNIT NUMBER, RECORD FILE            FOPEN........28800
C                 INFORMATION, AND OPEN THE FILE                         FOPEN........28900
               CALL NAFU(IUNEXT,JM1,FN,IERROR)                                  FOPEN........29000
	         IF (IERROR.NE.0) RETURN
               IU = IUNEXT                                               FOPEN........29100
               IUNIO(J) = IU                                             FOPEN........29200
               FNAMO(J) = FN                                             FOPEN........29300
               INQUIRE(UNIT=IU, OPENED=IS)                               FOPEN........29400
               OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',FORM='FORMATTED',   FOPEN........29500
     1            IOSTAT=KERR)                                           FOPEN........29600
               IF(KERR.GT.0) GOTO 9000                                   FOPEN........29700
  380       CONTINUE                                                     FOPEN........29800
  400    CONTINUE                                                        FOPEN........29900
C                                                                        FOPEN........30000
C........SECOND PASS IS COMPLETED, SO RETURN                             FOPEN........30100
         RETURN                                                          FOPEN........30200
C                                                                        FOPEN........30300
      END IF                                                             FOPEN........30400
C                                                                        FOPEN........30500
 8000 CONTINUE                                                           FOPEN........30600
C.....GENERATE ERROR                                                     FOPEN........30700
      ERRCOD = 'FIL-1'                                                   FOPEN........30800
      CHERR(1) = UNAME                                                   FOPEN........30900
      CHERR(2) = FN                                                      FOPEN........31000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           FOPEN........31100
	RETURN
C                                                                        FOPEN........31200
 9000 CONTINUE                                                           FOPEN........31300
C.....GENERATE ERROR                                                     FOPEN........31400
      ERRCOD = 'FIL-2'                                                   FOPEN........31500
      CHERR(1) = UNAME                                                   FOPEN........31600
      CHERR(2) = FN                                                      FOPEN........31700
      INERR(1) = IU                                                      FOPEN........31800
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                           FOPEN........31900
	RETURN
C                                                                        FOPEN........32000
      END                                                                FOPEN........32100
C                                                                        FOPEN........32200
C     FUNCTION          F  R  C  S  T  P           SUTRA VERSION 2.1     FRCSTP.........100
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
      COMMON /SCH/ NSCH,ISCHTS                                           FRCSTP........1600
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
C     SUBROUTINE        G  L  O  B  A  N           SUTRA VERSION 2.1     GLOBAN.........100
C                                                                        GLOBAN.........200
C *** PURPOSE :                                                          GLOBAN.........300
C ***  TO ASSEMBLE RESULTS OF ELEMENTWISE INTEGRATIONS INTO              GLOBAN.........400
C ***  A GLOBAL BANDED MATRIX AND GLOBAL VECTOR FOR BOTH                 GLOBAN.........500
C ***  FLOW AND TRANSPORT EQUATIONS.                                     GLOBAN.........600
C     SUBROUTINE        G  L  O  C  O  L           SUTRA VERSION 2.1     GLOCOL.........100
C                                                                        GLOCOL.........200
C *** PURPOSE :                                                          GLOCOL.........300
C ***  TO ASSEMBLE RESULTS OF ELEMENTWISE INTEGRATIONS INTO              GLOCOL.........400
C ***  A GLOBAL "SLAP COLUMN"-FORMAT MATRIX AND GLOBAL VECTOR            GLOCOL.........500
C ***  FOR BOTH FLOW AND TRANSPORT EQUATIONS.                            GLOCOL.........600
C     SUBROUTINE        I  N  D  A  T  0           SUTRA VERSION 2.1     INDAT0.........100
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
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     INDAT0........1600
      CHARACTER SCHTYP*12, CDUM10*10                                     INDAT0........1700
      CHARACTER*10 SCHNAM                                                INDAT0........1800
      CHARACTER CTICS*20, CREFT*8                                        INDAT0........1900
      DIMENSION INERR(10),RLERR(10)                                      INDAT0........2000
      DIMENSION KTYPE(2)                                                 INDAT0........2100
      ALLOCATABLE :: ISLIST(:), TLIST(:), DTMP1(:), DTMP2(:)             INDAT0........2200
      CHARACTER*10, ALLOCATABLE :: CTMP(:)                               INDAT0........2300
      CHARACTER*8 VERNUM, VERNIN                                         INDAT0........2400
      LOGICAL, ALLOCATABLE :: SBASED(:), ELAPSD(:)                       INDAT0........2500
      LOGICAL TSYES                                                      INDAT0........2600
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT0........2700
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT0........2800
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT0........2900
     1   NSOP,NSOU,NBCN                                                  INDAT0........3000
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        INDAT0........3100
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           INDAT0........3200
      COMMON /FNAMES/ UNAME,FNAME                                        INDAT0........3300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     INDAT0........3400
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INDAT0........3500
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      INDAT0........3600
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            INDAT0........3700
      COMMON /ITSOLR/ TOLP,TOLU                                          INDAT0........3800
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     INDAT0........3900
     1   KSCRN,KPAUSE                                                    INDAT0........4000
      COMMON /MODSOR/ ADSMOD                                             INDAT0........4100
      COMMON /SCH/ NSCH,ISCHTS                                           INDAT0........4200
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT0........4300
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT0........4400
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       INDAT0........4500
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           INDAT0........4600
      COMMON /SOLVN/ NSLVRS                                              INDAT0........4700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT0........4800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT0........4900
      COMMON /VER/ VERNUM, VERNIN                                        INDAT0........5000
C                                                                        INDAT0........5100
      INSTOP=0                                                           INDAT0........5200
C                                                                        INDAT0........5300
C.....INPUT DATASET 5: NUMERICAL CONTROL PARAMETERS                      INDAT0........5400
      ERRCOD = 'REA-INP-5'                                               INDAT0........5500
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT0........5600
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) UP,GNUP,GNUU                        INDAT0........5700
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0........5800
	   RETURN
	ENDIF
!      IF(ME.EQ.-1) WRITE(K3,70) UP,GNUP,GNUU                             INDAT0........5900
!   70 FORMAT(////11X,'N U M E R I C A L   C O N T R O L   D A T A'//     INDAT0........6000
!     1   11X,F15.5,5X,'"UPSTREAM WEIGHTING" FACTOR'/                     INDAT0........6100
!     2   11X,1PE15.4,5X,'SPECIFIED PRESSURE BOUNDARY CONDITION FACTOR'/  INDAT0........6200
!     3   11X,1PE15.4,5X,'SPECIFIED CONCENTRATION BOUNDARY CONDITION ',   INDAT0........6300
!     4   'FACTOR')                                                       INDAT0........6400
!      IF(ME.EQ.+1) WRITE(K3,80) UP,GNUP,GNUU                             INDAT0........6500
!   80 FORMAT(////11X,'N U M E R I C A L   C O N T R O L   D A T A'//     INDAT0........6600
!     1   11X,F15.5,5X,'"UPSTREAM WEIGHTING" FACTOR'/                     INDAT0........6700
!     2   11X,1PE15.4,5X,'SPECIFIED PRESSURE BOUNDARY CONDITION FACTOR'/  INDAT0........6800
!     3   11X,1PE15.4,5X,'SPECIFIED TEMPERATURE BOUNDARY CONDITION ',     INDAT0........6900
!     4   'FACTOR')                                                       INDAT0........7000
C                                                                        INDAT0........7100
C.....INPUT DATASET 6: TEMPORAL CONTROL AND SOLUTION CYCLING DATA        INDAT0........7200
C RBW 
      TICS = 0
	GOTO 100
!     DON'T READ THE ICS FILE.	
C RBW
      ERRCOD = 'REA-ICS-1'                                               INDAT0........7300
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT0........7400
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) TICS                                INDAT0........7500
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0........7600
	   RETURN
	ENDIF
      REWIND(K2)                                                         INDAT0........7700
C RBW
  100 CONTINUE
C RBW
!      WRITE(CTICS,'(E20.10)') TICS                                       INDAT0........7800
!      WRITE(K3,120)                                                      INDAT0........7900
!  120 FORMAT('1'////11X,'T E M P O R A L   C O N T R O L   A N D   ',    INDAT0........8000
!     1   'S O L U T I O N   C Y C L I N G   D A T A')                    INDAT0........8100
      ERRCOD = 'REA-INP-6'                                               INDAT0........8200
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT0........8300
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) NSCH                                INDAT0........8400
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0........8500
	   RETURN
	ENDIF
C.....IF VERSION 2.0 INPUT, RE-READ DATASET IN OLD FORMAT.  ELSE IF      INDAT0........8600
C        NSCH>0, RE-READ FIRST LINE OF DATASET IN NEW FORMAT.  ELSE      INDAT0........8700
C        IF NSCH<0, OR NSCH=0 AND TRANSPORT IS NOT STEADY-STATE,         INDAT0........8800
C        GENERATE ERROR.                                                 INDAT0........8900
      IF (VERNIN.EQ."2.0") THEN                                          INDAT0........9000
C........READ TEMPORAL AND SOLUTION CYCLING CONTROLS.                    INDAT0........9100
         READ(INTFIL,*,IOSTAT=INERR(1)) ITMAX,DELT,TMAX,ITCYC,DTMULT,    INDAT0........9200
     1      DTMAX,NPCYC,NUCYC                                            INDAT0........9300
         IF (INERR(1).NE.0) THEN 
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0........9400
	     RETURN
	   ENDIF
C........ERROR CHECKING SPECIFIC TO OLD FORMAT.                          INDAT0........9500
         IF (DELT.GT.DTMAX) THEN                                         INDAT0........9600
            ERRCOD = 'INP-6-3'                                           INDAT0........9700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0........9800
	      RETURN
         END IF                                                          INDAT0........9900
      ELSE IF (NSCH.GT.0) THEN                                           INDAT0.......10000
C........READ FIRST LINE OF DATASET.                                     INDAT0.......10100
         READ(INTFIL,*,IOSTAT=INERR(1)) NSCH, NPCYC, NUCYC               INDAT0.......10200
         IF (INERR(1).NE.0) THEN 
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......10300
	      RETURN
	   ENDIF
      ELSE IF (NSCH.LT.0) THEN                                           INDAT0.......10400
            ERRCOD = 'INP-6-8'                                           INDAT0.......10500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......10600
	      RETURN
      ELSE                                                               INDAT0.......10700
         IF (ISSTRA.EQ.0) THEN                                           INDAT0.......10800
            ERRCOD = 'INP-6-13'                                          INDAT0.......10900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......11000
	      RETURN
         END IF                                                          INDAT0.......11100
         NPCYC = 1                                                       INDAT0.......11200
         NUCYC = 1                                                       INDAT0.......11300
      END IF                                                             INDAT0.......11400
C.....ERROR CHECKING COMMON TO BOTH FORMATS.                             INDAT0.......11500
      IF (NPCYC.LT.1.OR.NUCYC.LT.1) THEN                                 INDAT0.......11600
         ERRCOD = 'INP-6-1'                                              INDAT0.......11700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......11800
	      RETURN
      ELSE IF (NPCYC.NE.1.AND.NUCYC.NE.1) THEN                           INDAT0.......11900
         ERRCOD = 'INP-6-2'                                              INDAT0.......12000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......12100
	      RETURN
      END IF                                                             INDAT0.......12200
C.....IF TRANSPORT IS STEADY-STATE, SKIP THROUGH THE REST OF THE         INDAT0.......12300
C        DATASET AND CREATE A TRIVIAL "TIME_STEPS" SCHEDULE.             INDAT0.......12400
C        (NOTE THAT IF TRANSPORT IS STEADY-STATE, SO IS FLOW.)           INDAT0.......12500
C        EVENTUALLY, A TRIVIAL SCHEDULE WILL ALSO BE CREATED FOR         INDAT0.......12600
C        OBSERVATION OUTPUT, SO SET NSCH=2.                              INDAT0.......12700
      IF (ISSTRA.EQ.1) THEN                                              INDAT0.......12800
         TSTART = TICS                                                   INDAT0.......12900
         IF (VERNIN.NE."2.0") THEN                                       INDAT0.......13000
            ERRCOD = 'REA-INP-6'                                         INDAT0.......13100
            CDUM10 = ''                                                  INDAT0.......13200
            DO WHILE (CDUM10.NE.'-')                                     INDAT0.......13300
               CALL READIF(K1, INTFIL, ERRCOD,IERROR)                           INDAT0.......13400
	         IF (IERROR.NE.0) RETURN
               IF (INERR(1).NE.0) THEN
                  CALL SUTERR(ERRCOD, CHERR, INERR,                      INDAT0.......13500
     1            RLERR,IERROR)                                                 INDAT0.......13600
	            RETURN
	         ENDIF
               READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                     INDAT0.......13700
            END DO                                                       INDAT0.......13800
            DELT = MAX(1D-1*DABS(TSTART), 1D0)                           INDAT0.......13900
         END IF                                                          INDAT0.......14000
         NSCH = 2                                                        INDAT0.......14100
         ALLOCATE(SCHDLS(NSCH))                                          INDAT0.......14200
         DO 135 NS=1,NSCH                                                INDAT0.......14300
            ALLOCATE(SCHDLS(NS)%SLIST)                                   INDAT0.......14400
            SCHDLS(NS)%LLEN = 0                                          INDAT0.......14500
  135    CONTINUE                                                        INDAT0.......14600
         TIME = TSTART                                                   INDAT0.......14700
         STEP = 0D0                                                      INDAT0.......14800
         LSTLEN = 0                                                      INDAT0.......14900
         CALL LLDINS(LSTLEN, SCHDLS(1)%SLIST, TIME, STEP)                INDAT0.......15000
         TIME = TIME + DELT                                              INDAT0.......15100
         STEP = 1D0                                                      INDAT0.......15200
         CALL LLDINS(LSTLEN, SCHDLS(1)%SLIST, TIME, STEP)                INDAT0.......15300
         SCHDLS(1)%LLEN = LSTLEN                                         INDAT0.......15400
         ISCHTS = 1                                                      INDAT0.......15500
         ITMAX = 1                                                       INDAT0.......15600
C........WRITE STEADY-STATE OUTPUT INFORMATION.                          INDAT0.......15700
!         WRITE(K3,138)                                                   INDAT0.......15800
!  138    FORMAT (/13X,'NOTE: BECAUSE FLOW AND TRANSPORT ARE STEADY-',    INDAT0.......15900
!     1      'STATE, USER-DEFINED SCHEDULES ARE NOT IN EFFECT.  '         INDAT0.......16000
!     2      /13X,'STEADY-STATE RESULTS WILL BE WRITTEN TO THE ',         INDAT0.......16100
!     3      'APPROPRIATE OUTPUT FILES.')                                 INDAT0.......16200
C........SKIP OVER PROCESSING AND WRITING OF TEMPORAL DATA.              INDAT0.......16300
         GOTO 850                                                        INDAT0.......16400
      END IF                                                             INDAT0.......16500
C.....IF DATASET IN OLD FORMAT, WRITE SPECIFICATIONS AND                 INDAT0.......16600
C        CREATE A CORRESPONDING SCHEDULE CALLED "TIME_STEPS".            INDAT0.......16700
C        IF NSCH=0, GENERATE AN ERROR.  IF IN NEW FORMAT, READ           INDAT0.......16800
C        AND PROCESS USER-DEFINED SCHEDULES.                             INDAT0.......16900
      IF (VERNIN.EQ."2.0") THEN                                          INDAT0.......17000
         TSTART = TICS                                                   INDAT0.......17100
!         WRITE(K3,150) ITMAX,DELT,TMAX,ITCYC,DTMULT,DTMAX                INDAT0.......17200
!  150    FORMAT (/13X,'NOTE: BECAUSE TEMPORAL CONTROL AND SOLUTION ',    INDAT0.......17300
!     1      'CYCLING DATA WERE ENTERED USING THE OLD (VERSION 2D3D.1) ', INDAT0.......17400
!     2      'INPUT FORMAT,'/13X,'A CORRESPONDING SCHEDULE, ',            INDAT0.......17500
!     3      '"TIME_STEPS", WAS CREATED AUTOMATICALLY FROM THE ',         INDAT0.......17600
!     4      'FOLLOWING PARAMETERS:'                                      INDAT0.......17700
!     5      //11X,I15,5X,'MAXIMUM ALLOWED NUMBER OF TIME STEPS'          INDAT0.......17800
!     6      /11X,1PE15.4,5X,'INITIAL TIME STEP (IN SECONDS)'             INDAT0.......17900
!     7      /11X,1PE15.4,5X,'MAXIMUM ALLOWED SIMULATION TIME ',          INDAT0.......18000
!     8      '(IN SECONDS)'                                               INDAT0.......18100
!     9      //11X,I15,5X,'TIME STEP MULTIPLIER CYCLE (IN TIME STEPS)'    INDAT0.......18200
!     1      /11X,0PF15.5,5X,'MULTIPLICATION FACTOR FOR TIME STEP CHANGE' INDAT0.......18300
!     2      /11X,1PE15.4,5X,'MAXIMUM ALLOWED TIME STEP (IN SECONDS)')    INDAT0.......18400
C........TWO DEFAULT SCHEDULES WILL EVENTUALLY BE DEFINED:               INDAT0.......18500
C           "TIME_STEPS", WHICH CONTROLS TIME STEPPING, AND              INDAT0.......18600
C           "OBS", WHICH CONTROLS TIMING OF OBSERVATION OUTPUT.          INDAT0.......18700
C           SET THE NUMBER OF SCHEDULES ACCORDINGLY AND ALLOCATE         INDAT0.......18800
C           THE SCHEDULE ARRAY AND ITS LINKED LISTS.                     INDAT0.......18900
         NSCH = 2                                                        INDAT0.......19000
         ALLOCATE(SCHDLS(NSCH))                                          INDAT0.......19100
         DO 185 NS=1,NSCH                                                INDAT0.......19200
            ALLOCATE(SCHDLS(NS)%SLIST)                                   INDAT0.......19300
            SCHDLS(NS)%LLEN = 0                                          INDAT0.......19400
  185    CONTINUE                                                        INDAT0.......19500
C........DEFINE THE DEFAULT "TIME_STEPS" SCHEDULE BASED ON THE           INDAT0.......19600
C           TEMPORAL CONTROLS.  NOTE THAT, FOR BACKWARD COMPATIBILITY    INDAT0.......19700
C           WITH OLD DATASETS, THE ORIGINAL METHOD OF HANDLING CHANGES   INDAT0.......19800
C           IN TIME STEP SIZE [BASED ON MOD(JT,ITCYC).EQ.0, NOT          INDAT0.......19900
C           MOD(JT-1,ITCYC).EQ.0] HAS BEEN RETAINED.                     INDAT0.......20000
         SCHDLS(1)%NAME = "TIME_STEPS"                                   INDAT0.......20100
         TIME = TSTART                                                   INDAT0.......20200
         STEP = 0D0                                                      INDAT0.......20300
         LSTLEN = 0                                                      INDAT0.......20400
         CALL LLDINS(LSTLEN, SCHDLS(1)%SLIST, TIME, STEP)                INDAT0.......20500
         DTIME = DELT                                                    INDAT0.......20600
         DO 580 JT=1,ITMAX                                               INDAT0.......20700
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DTIME=DTIME*DTMULT     INDAT0.......20800
            IF (DTIME.GT.DTMAX) DTIME = DTMAX                            INDAT0.......20900
            TIME = TIME + DTIME                                          INDAT0.......21000
            STEP = DBLE(JT)                                              INDAT0.......21100
            CALL LLDINS(LSTLEN, SCHDLS(1)%SLIST, TIME, STEP)             INDAT0.......21200
            IF (TIME.GE.TMAX) EXIT                                       INDAT0.......21300
  580    CONTINUE                                                        INDAT0.......21400
         SCHDLS(1)%LLEN = LSTLEN                                         INDAT0.......21500
         ITMAX = LSTLEN - 1
         ISCHTS = 1                                                      INDAT0.......21600
C........SKIP OVER THE CODE THAT READS SCHEDULE SPECIFICATIONS.          INDAT0.......21700
         GOTO 850                                                        INDAT0.......21800
      END IF                                                             INDAT0.......21900
C.....WRITE SCHEDULE PARAMETERS.                                         INDAT0.......22000
!      WRITE(K3,700) NSCH                                                 INDAT0.......22100
!  700 FORMAT(/13X,'THE ',I5,' SCHEDULES ARE LISTED BELOW.'               INDAT0.......22200
!     1   '  SCHEDULE "TIME_STEPS" CONTROLS TIME STEPPING.')              INDAT0.......22300
C.....ALLOCATE SCHEDULE-RELATED ARRAYS AND INITIALIZE SCHEDULE NUMBER    INDAT0.......22400
C        FOR "TIME_STEPS".                                               INDAT0.......22500
      ALLOCATE(SCHDLS(NSCH), SBASED(NSCH), ELAPSD(NSCH))                 INDAT0.......22600
      ISCHTS = 0                                                         INDAT0.......22700
C.....LOOP THROUGH THE LIST OF SCHEDULE SPECIFICATIONS, CONSTRUCTING     INDAT0.......22800
C        SCHEDULES.                                                      INDAT0.......22900
      DO 800 I=1,NSCH                                                    INDAT0.......23000
C........ALLOCATE HEAD OF LINKED LIST FOR THE CURRENT SCHEDULE AND SET   INDAT0.......23100
C           LIST LENGTH TO ZERO.                                         INDAT0.......23200
         ALLOCATE(SCHDLS(I)%SLIST)                                       INDAT0.......23300
         SCHDLS(I)%LLEN = 0                                              INDAT0.......23400
C........READ SCHEDULE NAME AND DO SOME ERROR CHECKING.                  INDAT0.......23500
         ERRCOD = 'REA-INP-6'                                            INDAT0.......23600
         CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                 INDAT0.......23700
	   IF (IERROR.NE.0) RETURN
         READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM                           INDAT0.......23800
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......23900
	      RETURN
	   ENDIF
         IF (SCHNAM.EQ."-") THEN                                         INDAT0.......24000
            ERRCOD = 'INP-6-4'                                           INDAT0.......24100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......24200
	      RETURN
         ELSE                                                            INDAT0.......24300
            DO 710 II=1,I-1                                              INDAT0.......24400
               IF (SCHNAM.EQ.SCHDLS(II)%NAME) THEN                       INDAT0.......24500
                  ERRCOD = 'INP-6-5'                                     INDAT0.......24600
                  CHERR(1) = SCHNAM                                      INDAT0.......24700
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0.......24800
                  RETURN
               END IF                                                    INDAT0.......24900
  710       CONTINUE                                                     INDAT0.......25000
         END IF                                                          INDAT0.......25100
C........(RE)READ SCHEDULE NAME AND TYPE.                                INDAT0.......25200
         READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP                   INDAT0.......25300
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......25400
	      RETURN
	   ENDIF
C........BASED ON THE SCHEDULE TYPE, READ IN THE SPECIFICATIONS AND      INDAT0.......25500
C           CONSTRUCT THE SCHEDULE.                                      INDAT0.......25600
         IF (SCHTYP.EQ."STEP CYCLE") THEN                                INDAT0.......25700
            SBASED(I) = .TRUE.                                           INDAT0.......25800
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......25900
            READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP,               INDAT0.......26000
     1         NSMAX, ISTEPI, ISTEPL, ISTEPC                             INDAT0.......26100
            IF (INERR(1).NE.0) THEN 
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           INDAT0.......26200
	         RETURN
	      ENDIF
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......26300
            ELAPSD(I) = .FALSE.                                          INDAT0.......26400
C...........CONSTRUCT THE SCHEDULE BY STEPPING THROUGH THE STEP CYCLE    INDAT0.......26500
C              AND STORING THE RESULTS IN THE LINKED LIST.  SET TIME     INDAT0.......26600
C              EQUAL TO STEP FOR NOW SO THAT THE LIST IS CONSTRUCTED     INDAT0.......26700
C              IN THE PROPER ORDER.                                      INDAT0.......26800
            NSTEP = ISTEPI                                               INDAT0.......26900
            NDSTEP = ISTEPC                                              INDAT0.......27000
            LSTLEN = 0                                                   INDAT0.......27100
            STEP = DNINT(DBLE(NSTEP))                                    INDAT0.......27200
            TIME = STEP                                                  INDAT0.......27300
            CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)             INDAT0.......27400
            DO 720 NS=1,NSMAX                                            INDAT0.......27500
               NSTEP = NSTEP + NDSTEP                                    INDAT0.......27600
               STEP = DNINT(DBLE(NSTEP))                                 INDAT0.......27700
               TIME = STEP                                               INDAT0.......27800
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......27900
               IF (NSTEP.GE.ISTEPL) EXIT                                 INDAT0.......28000
  720       CONTINUE                                                     INDAT0.......28100
            SCHDLS(I)%LLEN = LSTLEN                                      INDAT0.......28200
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......28300
!            WRITE(K3,722) SCHDLS(I)%NAME, NSMAX, ISTEPI, ISTEPL, ISTEPC  INDAT0.......28400
!  722       FORMAT(/16X,'SCHEDULE ',A, 3X,'STEP CYCLE WITH THE ',        INDAT0.......28500
!     1         'FOLLOWING SPECIFICATIONS:'                               INDAT0.......28600
!     2         /40X, I8, 5X, 'MAXIMUM NUMBER OF TIME STEPS AFTER ',      INDAT0.......28700
!     3            'INITIAL TIME STEP NUMBER'                             INDAT0.......28800
!     4         /40X, I8, 5X, 'INITIAL TIME STEP NUMBER'                  INDAT0.......28900
!     5         /40X, I8, 5X, 'LIMITING TIME STEP NUMBER'                 INDAT0.......29000
!     6         /40X, I8, 5X, 'TIME STEP INCREMENT')                      INDAT0.......29100
         ELSE IF (SCHTYP.EQ."TIME CYCLE") THEN                           INDAT0.......29200
            SBASED(I) = .FALSE.                                          INDAT0.......29300
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......29400
            READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,        INDAT0.......29500
     1         SCALT, NTMAX, TIMEI, TIMEL, TIMEC, NTCYC,                 INDAT0.......29600
     2         TCMULT, TCMIN, TCMAX                                      INDAT0.......29700
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           INDAT0.......29800
	         RETURN
	      ENDIF
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......29900
            IF (CREFT.EQ.'ELAPSED ') THEN                                INDAT0.......30000
               IF ((SCHNAM.EQ.'TIME_STEPS').AND.(TIMEI.NE.0D0)) THEN     INDAT0.......30100
                  ERRCOD = 'INP-6-7'                                     INDAT0.......30200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0.......30300
                  RETURN
               END IF                                                    INDAT0.......30400
               ELAPSD(I) = .TRUE.                                        INDAT0.......30500
            ELSE IF (CREFT.EQ.'ABSOLUTE') THEN                           INDAT0.......30600
               ELAPSD(I) = .FALSE.                                       INDAT0.......30700
            ELSE                                                         INDAT0.......30800
               ERRCOD = 'INP-6-6'                                        INDAT0.......30900
               CHERR(1) = CREFT                                          INDAT0.......31000
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  INDAT0.......31100
	         RETURN
            END IF                                                       INDAT0.......31200
C...........SCALE ALL TIME SPECIFICATIONS                                INDAT0.......31300
            TIMEI = TIMEI*SCALT                                          INDAT0.......31400
            TIMEL = TIMEL*SCALT                                          INDAT0.......31500
            TIMEC = TIMEC*SCALT                                          INDAT0.......31600
            TCMIN = TCMIN*SCALT                                          INDAT0.......31700
            TCMAX = TCMAX*SCALT                                          INDAT0.......31800
C...........CONSTRUCT THE SCHEDULE BY STEPPING THROUGH THE TIME CYCLE    INDAT0.......31900
C              AND STORING THE RESULTS IN THE LINKED LIST.               INDAT0.......32000
            TIME = TIMEI                                                 INDAT0.......32100
            STEP = FRCSTP(TIME)                                          INDAT0.......32200
            DTIME = TIMEC                                                INDAT0.......32300
            LSTLEN = 0                                                   INDAT0.......32400
            CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)             INDAT0.......32500
            DO 730 NT=1,NTMAX                                            INDAT0.......32600
               IF (MOD(NT-1,NTCYC).EQ.0 .AND. NT.GT.1)                   INDAT0.......32700
     1            DTIME=DTIME*TCMULT                                     INDAT0.......32800
               IF (DTIME.GT.TCMAX) DTIME = TCMAX                         INDAT0.......32900
               IF (DTIME.LT.TCMIN) DTIME = TCMIN                         INDAT0.......33000
               TIME = TIME + DTIME                                       INDAT0.......33100
               STEP = FRCSTP(TIME)                                       INDAT0.......33200
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......33300
               IF (TIME.GE.TIMEL) EXIT                                   INDAT0.......33400
  730       CONTINUE                                                     INDAT0.......33500
            SCHDLS(I)%LLEN = LSTLEN                                      INDAT0.......33600
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......33700
!            WRITE(K3,732) SCHDLS(I)%NAME, TRIM(CREFT), NTMAX, TIMEI,     INDAT0.......33800
!     1          TIMEL, TIMEC, NTCYC, TCMULT, TCMIN, TCMAX                INDAT0.......33900
!  732       FORMAT(/16X,'SCHEDULE ',A, 3X,'TIME CYCLE WITH THE ',        INDAT0.......34000
!     1         'FOLLOWING SPECIFICATIONS IN TERMS OF ', A, ' TIMES:'     INDAT0.......34100
!     2         /46X, I8, 5X, 'MAXIMUM NUMBER OF TIMES AFTER ',           INDAT0.......34200
!     3            'INITIAL TIME'                                         INDAT0.......34300
!     4         /39X, 1PE15.7, 5X, 'INITIAL TIME'                         INDAT0.......34400
!     5         /39X, 1PE15.7, 5X, 'LIMITING TIME'                        INDAT0.......34500
!     6         /39X, 1PE15.7, 5X, 'INITIAL TIME INCREMENT'               INDAT0.......34600
!     7         /46X, I8, 5X, 'TIME INCREMENT CHANGE CYCLE '              INDAT0.......34700
!     8         /39X, 1PE15.7, 5X, 'TIME INCREMENT MULTIPLIER'            INDAT0.......34800
!     9         /39X, 1PE15.7, 5X, 'MINIMUM TIME INCREMENT'               INDAT0.......34900
!     1         /39X, 1PE15.7, 5X, 'MAXIMUM TIME INCREMENT')              INDAT0.......35000
         ELSE IF (SCHTYP.EQ."STEP LIST") THEN                            INDAT0.......35100
            SBASED(I) = .TRUE.                                           INDAT0.......35200
C...........READ THE SCHEDULE NAME, TYPE, AND LENGTH.                    INDAT0.......35300
            BACKSPACE(K1)                                                INDAT0.......35400
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, NSLIST            INDAT0.......35500
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)            INDAT0.......35600
	         RETURN
	      ENDIF
C...........ALLOCATE A TEMPORARY ARRAY TO HOLD THE STEP LIST.            INDAT0.......35700
            ALLOCATE (ISLIST(NSLIST))                                    INDAT0.......35800
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......35900
            BACKSPACE(K1)                                                INDAT0.......36000
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP,                   INDAT0.......36100
     1         NSLIST, (ISLIST(NS),NS=1,NSLIST)                          INDAT0.......36200
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           INDAT0.......36300
	         RETURN
	      ENDIF
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......36400
            ELAPSD(I) = .FALSE.                                          INDAT0.......36500
C...........CONSTRUCT THE SCHEDULE BY TRANSFERRING THE LIST FROM ARRAY   INDAT0.......36600
C              ISLIST TO THE LINKED LIST.  SET TIME EQUAL TO STEP FOR    INDAT0.......36700
C              NOW SO THAT THE LIST IS CONSTRUCTED IN THE PROPER ORDER.  INDAT0.......36800
            LSTLEN = 0                                                   INDAT0.......36900
            DO 740 NS=1,NSLIST                                           INDAT0.......37000
               NSTEP = ISLIST(NS)                                        INDAT0.......37100
               STEP = DNINT(DBLE(NSTEP))                                 INDAT0.......37200
               TIME = STEP                                               INDAT0.......37300
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......37400
  740       CONTINUE                                                     INDAT0.......37500
            SCHDLS(I)%LLEN = NSLIST                                      INDAT0.......37600
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......37700
!            WRITE(K3,742) SCHDLS(I)%NAME, (ISLIST(NS),NS=1,NSLIST)       INDAT0.......37800
!  742       FORMAT(/16X,'SCHEDULE ',A, 3X,'STEP LIST THAT INCLUDES ',    INDAT0.......37900
!     1         'THE FOLLOWING TIME STEPS:'/:(38X,8(2X,I8)))              INDAT0.......38000
C...........DEALLOCATE THE TEMPORARY ARRAY.                              INDAT0.......38100
            DEALLOCATE (ISLIST)                                          INDAT0.......38200
         ELSE IF (SCHTYP.EQ."TIME LIST") THEN                            INDAT0.......38300
            SBASED(I) = .FALSE.                                          INDAT0.......38400
C...........READ THE SCHEDULE NAME, TYPE, SCALE FACTOR, AND LENGTH.      INDAT0.......38500
            BACKSPACE(K1)                                                INDAT0.......38600
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,            INDAT0.......38700
     1         SCALT, NTLIST                                             INDAT0.......38800
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           INDAT0.......38900
	         RETURN
	      ENDIF
C...........ALLOCATE A TEMPORARY ARRAY TO HOLD THE TIME LIST.            INDAT0.......39000
            ALLOCATE (TLIST(NTLIST))                                     INDAT0.......39100
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......39200
            BACKSPACE(K1)                                                INDAT0.......39300
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,            INDAT0.......39400
     1         SCALT, NTLIST, (TLIST(NT),NT=1,NTLIST)                    INDAT0.......39500
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           INDAT0.......39600
	         RETURN
	      ENDIF
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......39700
            IF (CREFT.EQ.'ELAPSED ') THEN                                INDAT0.......39800
               IF ((SCHNAM.EQ.'TIME_STEPS').AND.(TLIST(1).NE.0D0)) THEN  INDAT0.......39900
                  ERRCOD = 'INP-6-7'                                     INDAT0.......40000
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0.......40100
	            RETURN
               END IF                                                    INDAT0.......40200
               ELAPSD(I) = .TRUE.                                        INDAT0.......40300
            ELSE IF (CREFT.EQ.'ABSOLUTE') THEN                           INDAT0.......40400
               ELAPSD(I) = .FALSE.                                       INDAT0.......40500
            ELSE                                                         INDAT0.......40600
               ERRCOD = 'INP-6-6'                                        INDAT0.......40700
               CHERR(1) = CREFT                                          INDAT0.......40800
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  INDAT0.......40900
	         RETURN
            END IF                                                       INDAT0.......41000
C...........SCALE ALL TIME SPECIFICATIONS                                INDAT0.......41100
            DO 745 NT=1,NTLIST                                           INDAT0.......41200
               TLIST(NT) = TLIST(NT)*SCALT                               INDAT0.......41300
  745       CONTINUE                                                     INDAT0.......41400
C...........CONSTRUCT THE SCHEDULE BY TRANSFERRING THE LIST FROM ARRAY   INDAT0.......41500
C              TLIST TO THE LINKED LIST.                                 INDAT0.......41600
            LSTLEN = 0                                                   INDAT0.......41700
            DO 750 NT=1,NTLIST                                           INDAT0.......41800
               TIME = TLIST(NT)                                          INDAT0.......41900
               STEP = FRCSTP(TIME)                                       INDAT0.......42000
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......42100
  750       CONTINUE                                                     INDAT0.......42200
            SCHDLS(I)%LLEN = NTLIST                                      INDAT0.......42300
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......42400
!            WRITE(K3,752) SCHDLS(I)%NAME, TRIM(CREFT),                   INDAT0.......42500
!     1         (TLIST(NT),NT=1,NTLIST)                                   INDAT0.......42600
!  752       FORMAT(/16X,'SCHEDULE ',A, 3X,'TIME LIST THAT INCLUDES ',    INDAT0.......42700
!     1         'THE FOLLOWING ', A, ' TIMES (SEC):'                      INDAT0.......42800
!     2         /:(38X,4(1X,1PE15.7)))                                    INDAT0.......42900
            DEALLOCATE (TLIST)                                           INDAT0.......43000
         ELSE                                                            INDAT0.......43100
C...........THE SPECIFIED SCHEDULE TYPE IS INVALID, SO GENERATE AN       INDAT0.......43200
C              ERROR.                                                    INDAT0.......43300
            ERRCOD = 'INP-6-9'                                           INDAT0.......43400
            CHERR(1) = SCHTYP                                            INDAT0.......43500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......43600
	         RETURN
         END IF                                                          INDAT0.......43700
  800 CONTINUE                                                           INDAT0.......43800
C.....READ ONE MORE LINE TO CHECK FOR THE END-OF-LIST MARKER ('-').      INDAT0.......43900
C        IF NOT FOUND, GENERATE AN ERROR.                                INDAT0.......44000
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT0.......44100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                              INDAT0.......44200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0.......44300
	   RETURN
	ENDIF
      IF (CDUM10.NE.'-') THEN                                            INDAT0.......44400
         ERRCOD = 'INP-6-4'                                              INDAT0.......44500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......44600
	   RETURN
      END IF                                                             INDAT0.......44700
C.....FIND SCHEDULE "TIME_STEPS".                                        INDAT0.......44800
      DO 810 I=1,NSCH                                                    INDAT0.......44900
         IF (SCHDLS(I)%NAME.EQ."TIME_STEPS") THEN                        INDAT0.......45000
            ISCHTS=I                                                     INDAT0.......45100
            TSYES = .TRUE.                                               INDAT0.......45200
            EXIT                                                         INDAT0.......45300
         END IF                                                          INDAT0.......45400
  810 CONTINUE                                                           INDAT0.......45500
C.....IF TRANSPORT IS TRANSIENT AND SCHEDULE "TIME_STEPS" HAS NOT        INDAT0.......45600
C        BEEN DEFINED, GENERATE ERROR                                    INDAT0.......45700
      IF ((ISSTRA.EQ.0).AND.(.NOT.TSYES)) THEN                           INDAT0.......45800
         ERRCOD = 'INP-6-14'                                             INDAT0.......45900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......46000
	   RETURN
      END IF                                                             INDAT0.......46100
C.....IF "TIME_STEPS" LENGTH IS <=1 (SCHEDULE CONTAINS, AT MOST,         INDAT0.......46200
C        ONLY THE INITIAL TIME, AND NO SUBSEQUENT TIME STEPS),           INDAT0.......46300
C        GENERATE AN ERROR.                                              INDAT0.......46400
      IF (SCHDLS(ISCHTS)%LLEN.LE.1) THEN                                 INDAT0.......46500
         ERRCOD = 'INP-6-10'                                             INDAT0.......46600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......46700
	   RETURN
      END IF                                                             INDAT0.......46800
C.....IN SCHEDULE TIME STEPS, FILL IN TIME STEP NUMBERS, ADDING          INDAT0.......46900
C        TICS TO ELAPSED TIMES IF NECESSARY.  GENERATE ERROR IF          INDAT0.......47000
C        ANY TIMES ARE REPEATED.                                         INDAT0.......47100
      NSMAX = SCHDLS(ISCHTS)%LLEN                                        INDAT0.......47200
      ALLOCATE(DTMP1(NSMAX),DTMP2(NSMAX))                                INDAT0.......47300
      CALL LLD2AR(NSMAX, SCHDLS(ISCHTS)%SLIST, DTMP1, DTMP2)             INDAT0.......47400
      DEALLOCATE (SCHDLS(ISCHTS)%SLIST)                                  INDAT0.......47500
      ALLOCATE (SCHDLS(ISCHTS)%SLIST)                                    INDAT0.......47600
      IF (ELAPSD(ISCHTS)) THEN                                           INDAT0.......47700
         TREF = TICS                                                     INDAT0.......47800
      ELSE                                                               INDAT0.......47900
         TREF = 0D0                                                      INDAT0.......48000
      END IF                                                             INDAT0.......48100
      ITMAX = NSMAX - 1                                                  INDAT0.......48200
      TSTART = TREF + DTMP1(1)                                           INDAT0.......48300
      TFINSH = TREF + DTMP1(NSMAX)                                       INDAT0.......48400
      DELT = DTMP1(2) - DTMP1(1)                                         INDAT0.......48500
      LSTLEN = 0                                                         INDAT0.......48600
      DO 820 NS=1,NSMAX                                                  INDAT0.......48700
         IF ((NS.GT.1).AND.(DTMP1(NS).EQ.DTMP1(NS-1))) THEN              INDAT0.......48800
            ERRCOD = 'INP-6-12'                                          INDAT0.......48900
            IF (ELAPSD(ISCHTS)) THEN                                     INDAT0.......49000
               CHERR(1) = "elapsed time"                                 INDAT0.......49100
            ELSE                                                         INDAT0.......49200
               CHERR(1) = "absolute time"                                INDAT0.......49300
            END IF                                                       INDAT0.......49400
            CHERR(2) = "TIME_STEPS"                                      INDAT0.......49500
            RLERR(1) = DTMP1(NS)                                         INDAT0.......49600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT0.......49700
	      RETURN
         END IF                                                          INDAT0.......49800
         NSTEP = NS - 1                                                  INDAT0.......49900
         TIME = TREF + DTMP1(NS)                                         INDAT0.......50000
         STEP = DNINT(DBLE(NSTEP))                                       INDAT0.......50100
         CALL LLDINS(LSTLEN, SCHDLS(ISCHTS)%SLIST, TIME, STEP)           INDAT0.......50200
  820 CONTINUE                                                           INDAT0.......50300
      DEALLOCATE(DTMP1,DTMP2)                                            INDAT0.......50400
C.....FILL IN TIMES OR STEPS FOR REMAINING SCHEDULES, SHIFTING           INDAT0.......50500
C        ELAPSED TIMES TO ABSOLUTE TIMES IF NECESSARY.  PRUNE ENTRIES    INDAT0.......50600
C        THAT ARE OUTSIDE THE RANGE OF SCHEDULE "TIME_STEPS".            INDAT0.......50700
C        GENERATE ERROR IF ANY TIME STEPS OR TIMES ARE REPEATED.         INDAT0.......50800
      DO 845 I=1,NSCH                                                    INDAT0.......50900
         IF (I.EQ.ISCHTS) CYCLE                                          INDAT0.......51000
         NSMAX = SCHDLS(I)%LLEN                                          INDAT0.......51100
         ALLOCATE(DTMP1(NSMAX),DTMP2(NSMAX))                             INDAT0.......51200
         CALL LLD2AR(NSMAX, SCHDLS(I)%SLIST, DTMP1, DTMP2)               INDAT0.......51300
         DEALLOCATE (SCHDLS(I)%SLIST)                                    INDAT0.......51400
         ALLOCATE (SCHDLS(I)%SLIST)                                      INDAT0.......51500
         IF (ELAPSD(I)) THEN                                             INDAT0.......51600
            TREF = TSTART                                                INDAT0.......51700
         ELSE                                                            INDAT0.......51800
            TREF = 0D0                                                   INDAT0.......51900
         END IF                                                          INDAT0.......52000
         LSTLEN = 0                                                      INDAT0.......52100
         IF (SBASED(I)) THEN                                             INDAT0.......52200
            DO 840 NS=1,NSMAX                                            INDAT0.......52300
               IF ((NS.GT.1).AND.(DTMP2(NS).EQ.DTMP2(NS-1))) THEN        INDAT0.......52400
                  ERRCOD = 'INP-6-12'                                    INDAT0.......52500
                  CHERR(1) = "time step"                                 INDAT0.......52600
                  CHERR(2) = SCHDLS(I)%NAME                              INDAT0.......52700
                  RLERR(1) = DTMP2(NS)                                   INDAT0.......52800
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0.......52900
	            RETURN
               END IF                                                    INDAT0.......53000
               STEP = DTMP2(NS)                                          INDAT0.......53100
               NSTEP = NINT(STEP)                                        INDAT0.......53200
               IF ((NSTEP.LT.0).OR.(NSTEP.GT.ITMAX)) CYCLE               INDAT0.......53300
               TIME = TIMETS(NSTEP)                                      INDAT0.......53400
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......53500
  840       CONTINUE                                                     INDAT0.......53600
         ELSE                                                            INDAT0.......53700
            DO 842 NS=1,NSMAX                                            INDAT0.......53800
               IF ((NS.GT.1).AND.(DTMP1(NS).EQ.DTMP1(NS-1))) THEN        INDAT0.......53900
                  ERRCOD = 'INP-6-12'                                    INDAT0.......54000
                  IF (ELAPSD(I)) THEN                                    INDAT0.......54100
                     CHERR(1) = "elapsed time"                           INDAT0.......54200
                  ELSE                                                   INDAT0.......54300
                     CHERR(1) = "absolute time"                          INDAT0.......54400
                  END IF                                                 INDAT0.......54500
                  CHERR(2) = SCHDLS(I)%NAME                              INDAT0.......54600
                  RLERR(1) = DTMP1(NS)                                   INDAT0.......54700
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT0.......54800
	            RETURN
               END IF                                                    INDAT0.......54900
               TIME = TREF + DTMP1(NS)                                   INDAT0.......55000
               IF ((TIME.LT.TSTART).OR.(TIME.GT.TFINSH)) CYCLE           INDAT0.......55100
               STEP = FRCSTP(TIME)                                       INDAT0.......55200
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......55300
  842       CONTINUE                                                     INDAT0.......55400
         END IF                                                          INDAT0.......55500
         SCHDLS(I)%LLEN = LSTLEN                                         INDAT0.......55600
         DEALLOCATE(DTMP1,DTMP2)                                         INDAT0.......55700
  845 CONTINUE                                                           INDAT0.......55800
C.....DEALLOCATE ARRAY THAT INDICATES METHODS OF SCHEDULE SPECIFICATION  INDAT0.......55900
      DEALLOCATE(SBASED)                                                 INDAT0.......56000
C.....WRITE THE SOLUTION CYCLING CONTROLS.                               INDAT0.......56100
  850 CONTINUE
!  850 WRITE(K3,874) NPCYC,NUCYC                                          INDAT0.......56200
!  874 FORMAT (/13X,'SOLUTION CYCLING DATA:'                              INDAT0.......56300
!     1      //11X,I15,5X,'FLOW SOLUTION CYCLE (IN TIME STEPS)'           INDAT0.......56400
!     2      /11X,I15,5X,'TRANSPORT SOLUTION CYCLE (IN TIME STEPS)')      INDAT0.......56500
C.....SET SOLUTION CYCLING FOR STEADY-STATE FLOW                         INDAT0.......56600
      IF(ISSFLO.EQ.1) THEN                                               INDAT0.......56700
         NPCYC=ITMAX+1                                                   INDAT0.......56800
         NUCYC=1                                                         INDAT0.......56900
      END IF                                                             INDAT0.......57000
C                                                                        INDAT0.......57100
C.....INPUT DATASET 7A:  ITERATION CONTROLS FOR RESOLVING NONLINEARITIES INDAT0.......57200
      ERRCOD = 'REA-INP-7A'                                              INDAT0.......57300
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT0.......57400
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) ITRMAX                              INDAT0.......57500
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0.......57600
	   RETURN
	ENDIF
      IF (ITRMAX.GT.1) THEN                                              INDAT0.......57700
         ERRCOD = 'REA-INP-7A'                                           INDAT0.......57800
         READ(INTFIL,*,IOSTAT=INERR(1)) ITRMAX,RPMAX,RUMAX               INDAT0.......57900
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......58000
	      RETURN
	   ENDIF
      END IF                                                             INDAT0.......58100
!      IF(ITRMAX-1) 1192,1192,1194                                        INDAT0.......58200
! 1192 WRITE(K3,1193)                                                     INDAT0.......58300
! 1193 FORMAT(////11X,'I T E R A T I O N   C O N T R O L   D A T A',      INDAT0.......58400
!     1   //11X,'  NON-ITERATIVE SOLUTION')                               INDAT0.......58500
!      GOTO 1196                                                          INDAT0.......58600
! 1194 WRITE(K3,1195) ITRMAX,RPMAX,RUMAX                                  INDAT0.......58700
! 1195 FORMAT(////11X,'I T E R A T I O N   C O N T R O L   D A T A',      INDAT0.......58800
!     1   //11X,I15,5X,'MAXIMUM NUMBER OF ITERATIONS PER TIME STEP',      INDAT0.......58900
!     2   /11X,1PE15.4,5X,'ABSOLUTE CONVERGENCE CRITERION FOR FLOW',      INDAT0.......59000
!     3   ' SOLUTION'/11X,1PE15.4,5X,'ABSOLUTE CONVERGENCE CRITERION',    INDAT0.......59100
!     4   ' FOR TRANSPORT SOLUTION')                                      INDAT0.......59200
! 1196 CONTINUE                                                           INDAT0.......59300
C                                                                        INDAT0.......59400
C.....INPUT DATASETS 7B & 7C:  MATRIX EQUATION SOLVER CONTROLS FOR       INDAT0.......59500
C        PRESSURE AND TRANSPORT SOLUTIONS                                INDAT0.......59600
      ERRCOD = 'REA-INP-7B'                                              INDAT0.......59700
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT0.......59800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVP                              INDAT0.......59900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0.......60000
	   RETURN
	ENDIF
      IF ((CSOLVP.NE.SOLWRD(0))) THEN                                    INDAT0.......60100
         ERRCOD = 'REA-INP-7B'                                           INDAT0.......60200
         READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVP,ITRMXP,TOLP               INDAT0.......60300
         IF (INERR(1).NE.0) THEN 
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......60400
	      RETURN
	   ENDIF
      END IF                                                             INDAT0.......60500
      ERRCOD = 'REA-INP-7C'                                              INDAT0.......60600
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT0.......60700
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVU                              INDAT0.......60800
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT0.......60900
	   RETURN
	ENDIF
      IF ((CSOLVU.NE.SOLWRD(0))) THEN                                    INDAT0.......61000
         ERRCOD = 'REA-INP-7C'                                           INDAT0.......61100
         READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVU,ITRMXU,TOLU               INDAT0.......61200
         IF (INERR(1).NE.0) THEN 
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT0.......61300
	      RETURN
	   ENDIF
      END IF                                                             INDAT0.......61400
      KSOLVP = -1                                                        INDAT0.......61500
      KSOLVU = -1                                                        INDAT0.......61600
      DO 1250 M=0,NSLVRS-1                                               INDAT0.......61700
         IF (CSOLVP.EQ.SOLWRD(M)) KSOLVP = M                             INDAT0.......61800
         IF (CSOLVU.EQ.SOLWRD(M)) KSOLVU = M                             INDAT0.......61900
 1250 CONTINUE                                                           INDAT0.......62000
      IF ((KSOLVP.LT.0).OR.(KSOLVU.LT.0)) THEN                           INDAT0.......62100
         ERRCOD = 'INP-7B&C-1'                                           INDAT0.......62200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......62300
	      RETURN
      ELSE IF ((KSOLVP*KSOLVU.EQ.0).AND.(KSOLVP+KSOLVU.NE.0)) THEN       INDAT0.......62400
         ERRCOD = 'INP-7B&C-2'                                           INDAT0.......62500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......62600
	      RETURN
      ELSE IF ((KSOLVU.EQ.1).OR.((KSOLVP.EQ.1).AND.(UP.NE.0D0))) THEN    INDAT0.......62700
         ERRCOD = 'INP-7B&C-3'                                           INDAT0.......62800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT0.......62900
	      RETURN
      END IF                                                             INDAT0.......63000
      IF (KSOLVP.EQ.2) THEN                                              INDAT0.......63100
         ITOLP = 0                                                       INDAT0.......63200
      ELSE                                                               INDAT0.......63300
         ITOLP = 1                                                       INDAT0.......63400
      END IF                                                             INDAT0.......63500
      IF (KSOLVU.EQ.2) THEN                                              INDAT0.......63600
         ITOLU = 0                                                       INDAT0.......63700
      ELSE                                                               INDAT0.......63800
         ITOLU = 1                                                       INDAT0.......63900
      END IF                                                             INDAT0.......64000
      NSAVEP = 10                                                        INDAT0.......64100
      NSAVEU = 10                                                        INDAT0.......64200
C                                                                        INDAT0.......64300
C                                                                        INDAT0.......64400
      RETURN                                                             INDAT0.......64500
      END                                                                INDAT0.......64600
C                                                                        INDAT0.......64700
C     SUBROUTINE        I  N  D  A  T  1           SUTRA VERSION 2.1     INDAT1.........100
C                                                                        INDAT1.........200
C *** PURPOSE :                                                          INDAT1.........300
C ***  TO INPUT, OUTPUT, AND ORGANIZE A MAJOR PORTION OF INP FILE        INDAT1.........400
C ***  INPUT DATA (DATASETS 8 THROUGH 15)                                INDAT1.........500
C                                                                        INDAT1.........600
      SUBROUTINE INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,         INDAT1.........700
     1   ATMIN,PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,                       INDAT1.........800
     2   PERMYZ,PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG, INDAT1.........900
     3   OBSPTS, 
     4   ElementValues, IElementValueCount, NodeValues, 
     5   INodeValueCount, IERROR)                                        INDAT1........1000
      USE ALLARR, ONLY : OBSDAT                                          INDAT1........1100
      USE LLDEF                                                          INDAT1........1200
      USE EXPINT                                                         INDAT1........1300
      USE SCHDEF                                                         INDAT1........1400
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT1........1500
      PARAMETER (NCOLMX=9)                                               INDAT1........1600
      CHARACTER*10 ADSMOD,CDUM10                                         INDAT1........1700
      CHARACTER*6 STYPE(2)                                               INDAT1........1800
      CHARACTER K5SYM(7)*1, NCOL(NCOLMX)*1, VARNK5(7)*25                 INDAT1........1900
      CHARACTER K6SYM(7)*2, LCOL(NCOLMX)*2, VARNK6(7)*25                 INDAT1........2000
      CHARACTER*1 CNODAL,CELMNT,CINCID,CVEL,CBUDG,CSCRN,CPAUSE           INDAT1........2100
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     INDAT1........2200
      CHARACTER INTFIL*1000,DOTS45*45                                    INDAT1........2300
      CHARACTER OBSNAM*40, OBSSCH*10, OBSFMT*3                           INDAT1........2400
      CHARACTER*8 VERNUM, VERNIN                                         INDAT1........2500
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         INDAT1........2600
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             INDAT1........2700
      DIMENSION X(NN),Y(NN),Z(NN),POR(NN),SOP(NN),NREG(NN)               INDAT1........2800
      DIMENSION PERMXX(NE),PERMXY(NE),PERMXZ(NEX),PERMYX(NE),PERMYY(NE), INDAT1........2900
     1   PERMYZ(NEX),PERMZX(NEX),PERMZY(NEX),PERMZZ(NEX),PANGL1(NE),     INDAT1........3000
     2   PANGL2(NEX),PANGL3(NEX),ALMAX(NE),ALMID(NEX),ALMIN(NE),         INDAT1........3100
     3   ATMAX(NE),ATMID(NEX),ATMIN(NE),LREG(NE)                         INDAT1........3200
      DIMENSION INERR(10),RLERR(10)                                      INDAT1........3300
      DIMENSION KTYPE(2)                                                 INDAT1........3400
      DIMENSION IUNIT(0:8)                                               INDAT1........3500
      ALLOCATABLE :: INOB(:)                                             INDAT1........3600
      TYPE (LLD), POINTER :: DENTS                                       INDAT1........3700
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT1........3800
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT1........3900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT1........4000
     1   NSOP,NSOU,NBCN                                                  INDAT1........4100
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        INDAT1........4200
      COMMON /FNAMES/ UNAME,FNAME                                        INDAT1........4300
      COMMON /FUNITA/ IUNIT                                              INDAT1........4400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     INDAT1........4500
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INDAT1........4600
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      INDAT1........4700
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        INDAT1........4800
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     INDAT1........4900
     1   KSCRN,KPAUSE                                                    INDAT1........5000
      COMMON /MODSOR/ ADSMOD                                             INDAT1........5100
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      INDAT1........5200
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT1........5300
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT1........5400
      COMMON /SCH/ NSCH,ISCHTS                                           INDAT1........5500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT1........5600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT1........5700
      COMMON /VER/ VERNUM, VERNIN                                        INDAT1........5800
      DATA STYPE(1)/'ENERGY'/,STYPE(2)/'SOLUTE'/                         INDAT1........5900
      DATA (K5SYM(MM), MM=1,7) /'N', 'X', 'Y', 'Z', 'P', 'U', 'S'/       INDAT1........6000
      DATA (VARNK5(MM), MM=1,7) /'NODE NUMBER',                          INDAT1........6100
     1   'X-COORDINATE', 'Y-COORDINATE', 'Z-COORDINATE',                 INDAT1........6200
     2   'PRESSURE', 'CONCENTRATION/TEMPERATURE', 'SATURATION'/          INDAT1........6300
      DATA (K6SYM(MM), MM=1,7) /'E', 'X', 'Y', 'Z', 'VX', 'VY', 'VZ'/    INDAT1........6400
      DATA (VARNK6(MM), MM=1,7) /'ELEMENT NUMBER',                       INDAT1........6500
     1   'X-COORDINATE OF CENTROID', 'Y-COORDINATE OF CENTROID',         INDAT1........6600
     2   'Z-COORDINATE OF CENTROID', 'X-VELOCITY', 'Y-VELOCITY',         INDAT1........6700
     3   'Z-VELOCITY'/                                                   INDAT1........6800
      DATA DOTS45 /'.............................................'/      INDAT1........6900
      SAVE STYPE,K5SYM,VARNK5,K6SYM,VARNK6                               INDAT1........7000
C RBW
      INTEGER IERROR
      INTEGER IElementValueCount
      INTEGER INodeValueCount
	REAL (KIND = 4) ElementValues(IElementValueCount)
	REAL (KIND = 4) NodeValues(INodeValueCount)
C RBW
C                                                                        INDAT1........7100
      INSTOP=0                                                           INDAT1........7200
C                                                                        INDAT1........7300
C.....INPUT DATASET 8A:  OUTPUT CONTROLS AND OPTIONS FOR LST FILE        INDAT1........7400
C        AND SCREEN                                                      INDAT1........7500
      ERRCOD = 'REA-INP-8A'                                              INDAT1........7600
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1........7700
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) NPRINT,CNODAL,CELMNT,CINCID,        INDAT1........7800
     1   CVEL,CBUDG,CSCRN,CPAUSE                                         INDAT1........7900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1........8000
	   RETURN
	ENDIF
      IF (CNODAL.EQ.'Y') THEN                                            INDAT1........8100
         KNODAL = +1                                                     INDAT1........8200
      ELSE IF (CNODAL.EQ.'N') THEN                                       INDAT1........8300
         KNODAL = 0                                                      INDAT1........8400
      ELSE                                                               INDAT1........8500
         ERRCOD = 'INP-8A-1'                                             INDAT1........8600
         CHERR(1) = 'CNODAL '                                            INDAT1........8700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1........8800
	   RETURN
      END IF                                                             INDAT1........8900
      IF (CELMNT.EQ.'Y') THEN                                            INDAT1........9000
         KELMNT = +1                                                     INDAT1........9100
      ELSE IF (CELMNT.EQ.'N') THEN                                       INDAT1........9200
         KELMNT = 0                                                      INDAT1........9300
      ELSE                                                               INDAT1........9400
         ERRCOD = 'INP-8A-2'                                             INDAT1........9500
         CHERR(1) = 'CELMNT'                                             INDAT1........9600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1........9700
	   RETURN
      END IF                                                             INDAT1........9800
      IF (CINCID.EQ.'Y') THEN                                            INDAT1........9900
         KINCID = +1                                                     INDAT1.......10000
      ELSE IF (CINCID.EQ.'N') THEN                                       INDAT1.......10100
         KINCID = 0                                                      INDAT1.......10200
      ELSE                                                               INDAT1.......10300
         ERRCOD = 'INP-8A-3'                                             INDAT1.......10400
         CHERR(1) = 'CINCID'                                             INDAT1.......10500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......10600
	   RETURN
      END IF                                                             INDAT1.......10700
      IF (CVEL.EQ.'Y') THEN                                              INDAT1.......10800
         KVEL = +1                                                       INDAT1.......10900
      ELSE IF (CVEL.EQ.'N') THEN                                         INDAT1.......11000
         KVEL = 0                                                        INDAT1.......11100
      ELSE                                                               INDAT1.......11200
         ERRCOD = 'INP-8A-4'                                             INDAT1.......11300
         CHERR(1) = 'CVEL  '                                             INDAT1.......11400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......11500
	   RETURN
      END IF                                                             INDAT1.......11600
      IF (CBUDG.EQ.'Y') THEN                                             INDAT1.......11700
         KBUDG = +1                                                      INDAT1.......11800
      ELSE IF (CBUDG.EQ.'N') THEN                                        INDAT1.......11900
         KBUDG = 0                                                       INDAT1.......12000
      ELSE                                                               INDAT1.......12100
         ERRCOD = 'INP-8A-5'                                             INDAT1.......12200
         CHERR(1) = 'CBUDG '                                             INDAT1.......12300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......12400
	   RETURN
      END IF                                                             INDAT1.......12500
      IF (CSCRN.EQ.'Y') THEN                                             INDAT1.......12600
         KSCRN = +1                                                      INDAT1.......12700
      ELSE IF (CSCRN.EQ.'N') THEN                                        INDAT1.......12800
         KSCRN = 0                                                       INDAT1.......12900
      ELSE                                                               INDAT1.......13000
         ERRCOD = 'INP-8A-6'                                             INDAT1.......13100
         CHERR(1) = 'CSCRN '                                             INDAT1.......13200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......13300
	   RETURN
      END IF                                                             INDAT1.......13400
      IF (CPAUSE.EQ.'Y') THEN                                            INDAT1.......13500
         KPAUSE = +1                                                     INDAT1.......13600
      ELSE IF (CPAUSE.EQ.'N') THEN                                       INDAT1.......13700
         KPAUSE = 0                                                      INDAT1.......13800
      ELSE                                                               INDAT1.......13900
         ERRCOD = 'INP-8A-7'                                             INDAT1.......14000
         CHERR(1) = 'CPAUSE'                                             INDAT1.......14100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......14200
	   RETURN
      END IF                                                             INDAT1.......14300
C                                                                        INDAT1.......14400
!      WRITE(K3,72) NPRINT                                                INDAT1.......14500
!   72 FORMAT(////11X,'O U T P U T   C O N T R O L S   A N D   ',         INDAT1.......14600
!     1   'O P T I O N S'//13X,'.LST FILE'/13X,'---------'                INDAT1.......14700
!     2   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)')             INDAT1.......14800
!      IF(KNODAL.EQ.+1) WRITE(K3,74)                                      INDAT1.......14900
!      IF(KNODAL.EQ.0) WRITE(K3,75)                                       INDAT1.......15000
!   74 FORMAT(/13X,'- PRINT NODE COORDINATES, THICKNESSES AND',           INDAT1.......15100
!     1   ' POROSITIES')                                                  INDAT1.......15200
!   75 FORMAT(/13X,'- CANCEL PRINT OF NODE COORDINATES, THICKNESSES AND', INDAT1.......15300
!     1   ' POROSITIES')                                                  INDAT1.......15400
!      IF(KELMNT.EQ.+1) WRITE(K3,76)                                      INDAT1.......15500
!      IF(KELMNT.EQ.0) WRITE(K3,77)                                       INDAT1.......15600
!   76 FORMAT(13X,'- PRINT ELEMENT PERMEABILITIES AND DISPERSIVITIES')    INDAT1.......15700
!   77 FORMAT(13X,'- CANCEL PRINT OF ELEMENT PERMEABILITIES AND ',        INDAT1.......15800
!     1   'DISPERSIVITIES')                                               INDAT1.......15900
!      IF(KINCID.EQ.+1) WRITE(K3,78)                                      INDAT1.......16000
!      IF(KINCID.EQ.0) WRITE(K3,79)                                       INDAT1.......16100
!   78 FORMAT(13X,'- PRINT NODE INCIDENCES IN EACH ELEMENT')              INDAT1.......16200
!   79 FORMAT(13X,'- CANCEL PRINT OF NODE INCIDENCES IN EACH ELEMENT')    INDAT1.......16300
      IME=2                                                              INDAT1.......16400
      IF(ME.EQ.+1) IME=1                                                 INDAT1.......16500
!      IF(KVEL.EQ.+1) WRITE(K3,84)                                        INDAT1.......16600
!      IF(KVEL.EQ.0) WRITE(K3,85)                                         INDAT1.......16700
!   84 FORMAT(/13X,'- CALCULATE AND PRINT VELOCITIES AT ELEMENT ',        INDAT1.......16800
!     1   'CENTROIDS ON EACH TIME STEP WITH OUTPUT')                      INDAT1.......16900
!   85 FORMAT(/13X,'- CANCEL PRINT OF VELOCITIES')                        INDAT1.......17000
!      IF(KBUDG.EQ.+1) WRITE(K3,86) STYPE(IME)                            INDAT1.......17100
!      IF(KBUDG.EQ.0) WRITE(K3,87)                                        INDAT1.......17200
!   86 FORMAT(/13X,'- CALCULATE AND PRINT FLUID AND ',A6,' BUDGETS ',     INDAT1.......17300
!     1   'ON EACH TIME STEP WITH OUTPUT')                                INDAT1.......17400
!   87 FORMAT(/13X,'- CANCEL PRINT OF BUDGETS')                           INDAT1.......17500
C                                                                        INDAT1.......17600
C.....INPUT DATASET 8B:  OUTPUT CONTROLS AND OPTIONS FOR NOD FILE        INDAT1.......17700
      ERRCOD = 'REA-INP-8B'                                              INDAT1.......17800
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......17900
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) NCOLPR                              INDAT1.......18000
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......18100
	   RETURN
	ENDIF
      DO 140 M=1,NCOLMX                                                  INDAT1.......18200
         READ(INTFIL,*,IOSTAT=INERR(1)) NCOLPR, (NCOL(MM), MM=1,M)       INDAT1.......18300
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT1.......18400
	      RETURN
	   ENDIF
         IF (NCOL(M).EQ.'-') THEN                                        INDAT1.......18500
            NCOLS5 = M - 1                                               INDAT1.......18600
            GOTO 142                                                     INDAT1.......18700
         END IF                                                          INDAT1.......18800
  140 CONTINUE                                                           INDAT1.......18900
      NCOLS5 = NCOLMX                                                    INDAT1.......19000
  142 CONTINUE                                                           INDAT1.......19100
!      WRITE(K3,144) NCOLPR                                               INDAT1.......19200
!  144 FORMAT (//13X,'.NOD FILE'/13X,'---------'                          INDAT1.......19300
!     1   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......19400
      DO 148 M=1,NCOLS5                                                  INDAT1.......19500
         DO 146 MM=1,7                                                   INDAT1.......19600
            IF (NCOL(M).EQ.K5SYM(MM)) THEN                               INDAT1.......19700
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN                          INDAT1.......19800
                  ERRCOD = 'INP-8B-1'                                    INDAT1.......19900
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......20000
	            RETURN
               END IF                                                    INDAT1.......20100
               IF ((MM.EQ.4).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......20200
                  ERRCOD = 'INP-8B-2'                                    INDAT1.......20300
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......20400
	            RETURN
               END IF                                                    INDAT1.......20500
               J5COL(M) = MM                                             INDAT1.......20600
               GOTO 148                                                  INDAT1.......20700
            END IF                                                       INDAT1.......20800
  146    CONTINUE                                                        INDAT1.......20900
         ERRCOD = 'INP-8B-3'                                             INDAT1.......21000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......21100
	   RETURN
  148 CONTINUE                                                           INDAT1.......21200
!      WRITE(K3,150) (M,VARNK5(J5COL(M)),M=1,NCOLS5)                      INDAT1.......21300
!  150 FORMAT (13X,'COLUMN ',I1,':',2X,A)                                 INDAT1.......21400
C                                                                        INDAT1.......21500
C.....INPUT DATASET 8C:  OUTPUT CONTROLS AND OPTIONS FOR ELE FILE        INDAT1.......21600
      ERRCOD = 'REA-INP-8C'                                              INDAT1.......21700
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......21800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) LCOLPR                              INDAT1.......21900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......22000
	   RETURN
	ENDIF
      DO 160 M=1,NCOLMX                                                  INDAT1.......22100
         READ(INTFIL,*,IOSTAT=INERR(1)) LCOLPR, (LCOL(MM), MM=1,M)       INDAT1.......22200
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT1.......22300
	      RETURN
	   ENDIF
         IF (LCOL(M).EQ.'-') THEN                                        INDAT1.......22400
            NCOLS6 = M - 1                                               INDAT1.......22500
            GOTO 162                                                     INDAT1.......22600
         END IF                                                          INDAT1.......22700
  160 CONTINUE                                                           INDAT1.......22800
      NCOLS6 = NCOLMX                                                    INDAT1.......22900
  162 CONTINUE                                                           INDAT1.......23000
!      WRITE(K3,164) LCOLPR                                               INDAT1.......23100
!  164 FORMAT (//13X,'.ELE FILE'/13X,'---------'                          INDAT1.......23200
!     1   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......23300
      DO 168 M=1,NCOLS6                                                  INDAT1.......23400
         DO 166 MM=1,7                                                   INDAT1.......23500
            IF (LCOL(M).EQ.K6SYM(MM)) THEN                               INDAT1.......23600
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN                          INDAT1.......23700
                  ERRCOD = 'INP-8C-1'                                    INDAT1.......23800
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......23900
	            RETURN
               END IF                                                    INDAT1.......24000
               IF ((MM.EQ.4).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......24100
                  ERRCOD = 'INP-8C-2'                                    INDAT1.......24200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......24300
	            RETURN
               END IF                                                    INDAT1.......24400
               IF ((MM.EQ.7).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......24500
                  ERRCOD = 'INP-8C-4'                                    INDAT1.......24600
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)               INDAT1.......24700
	            RETURN
               END IF                                                    INDAT1.......24800
               J6COL(M) = MM                                             INDAT1.......24900
               GOTO 168                                                  INDAT1.......25000
            END IF                                                       INDAT1.......25100
  166    CONTINUE                                                        INDAT1.......25200
         ERRCOD = 'INP-8C-3'                                             INDAT1.......25300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......25400
	   RETURN
  168 CONTINUE                                                           INDAT1.......25500
!      WRITE(K3,170) (M,VARNK6(J6COL(M)),M=1,NCOLS6)                      INDAT1.......25600
!  170 FORMAT (13X,'COLUMN ',I1,':',2X,A)                                 INDAT1.......25700
C                                                                        INDAT1.......25800
C.....INPUT DATASET 8D:  OUTPUT CONTROLS AND OPTIONS FOR OBSERVATIONS    INDAT1.......25900
      NOBCYC = ITMAX + 1                                                 INDAT1.......26000
      IF (NOBSN-1.EQ.0) GOTO 1199                                        INDAT1.......26100
C.....NOBS IS ACTUAL NUMBER OF OBSERVATION POINTS                        INDAT1.......26200
C.....NTOBS IS MAXIMUM NUMBER OF TIME STEPS WITH OBSERVATIONS            INDAT1.......26300
      NOBS=NOBSN-1                                                       INDAT1.......26400
C.....READ IN OBSERVATION POINTS                                         INDAT1.......26500
      ERRCOD = 'REA-INP-8D'                                              INDAT1.......26600
C.....DO THIS READ NOW TO SKIP ANY COMMENTS AND BLANK LINES.             INDAT1.......26700
C        (BACKSPACE LATER IF IT MUST BE REREAD IN OLD FORMAT.)           INDAT1.......26800
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......26900
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) NOBLIN                              INDAT1.......27000
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......27100
	   RETURN
	ENDIF
C.....IF TRANSPORT IS STEADY-STATE, CONSTRUCT A TRIVIAL SCHEDULE.        INDAT1.......27200
      IF (ISSTRA.EQ.1) THEN                                              INDAT1.......27300
         SCHDLS(2)%NAME = "-"                                            INDAT1.......27400
         TIME = TSTART                                                   INDAT1.......27500
         STEP = 0D0                                                      INDAT1.......27600
         LSTLEN = 0                                                      INDAT1.......27700
         CALL LLDINS(LSTLEN, SCHDLS(2)%SLIST, TIME, STEP)                INDAT1.......27800
         STEP = 1D0                                                      INDAT1.......27900
         CALL LLDINS(LSTLEN, SCHDLS(2)%SLIST, TIME, STEP)                INDAT1.......28000
         SCHDLS(2)%LLEN = LSTLEN                                         INDAT1.......28100
      END IF                                                             INDAT1.......28200
C.....IF OLD (VERSION 2.0) INPUT FORMAT IS BEING USED, CONSTRUCT A       INDAT1.......28300
C        CORRESPONDING OBSERVATION OUTPUT SCHEDULE IF TRANSPORT IS       INDAT1.......28400
C        TRANSIENT.                                                      INDAT1.......28500
      IF (VERNIN.EQ."2.0") THEN                                          INDAT1.......28600
C........SET THE MAX NUMBER OF OBSERVATIONS PER LINE TO THE TOTAL        INDAT1.......28700
C           NUMBER OF OBSERVATIONS.                                      INDAT1.......28800
         NOBLIN = NOBS                                                   INDAT1.......28900
C........SET UP A TEMPORARY ARRAY TO HOLD OBSERVATION NODES.             INDAT1.......29000
         ALLOCATE(INOB(NOBSN))                                           INDAT1.......29100
C........BACKSPACE AND REREAD DATASET IN OLD FORMAT                      INDAT1.......29200
         BACKSPACE(K1)                                                   INDAT1.......29300
         READ(K1,*,IOSTAT=INERR(1)) NOBCYC, (INOB(JJ), JJ=1,NOBSN)       INDAT1.......29400
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT1.......29500
	      RETURN
	   ENDIF
C........IF THE LAST NODE NUMBER IS NOT ZERO, GENERATE AN ERROR.         INDAT1.......29600
         IF (INOB(NOBSN).NE.0) THEN                                      INDAT1.......29700
            ERRCOD = 'INP-8D-1'                                          INDAT1.......29800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT1.......29900
	      RETURN
         END IF                                                          INDAT1.......30000
C........IF A NODE NUMBER IS INVALID, GENERATE AN ERROR.                 INDAT1.......30100
         DO 510 JJ=1,NOBS                                                INDAT1.......30200
            IF ((INOB(JJ).LT.1).OR.(INOB(JJ).GT.NN)) THEN                INDAT1.......30300
               ERRCOD = 'INP-8D-2'                                       INDAT1.......30400
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  INDAT1.......30500
	         RETURN
            END IF                                                       INDAT1.......30600
  510    CONTINUE                                                        INDAT1.......30700
C........IF TRANSPORT IS TRANSIENT, CONSTRUCT A SCHEDULE THAT            INDAT1.......30800
C           CORRESPONDS TO THE CYCLE SPECIFIED BY NOBCYC.                INDAT1.......30900
         IF (ISSTRA.EQ.0) THEN                                           INDAT1.......31000
            SCHDLS(2)%LLEN = 0                                           INDAT1.......31100
            SCHDLS(2)%NAME = "-"                                         INDAT1.......31200
            ISTEPI = 0                                                   INDAT1.......31300
            ISTEPL = ITMAX                                               INDAT1.......31400
            ITERM = ISTEPL - ISTEPI                                      INDAT1.......31500
C...........IF NOBCYC=0, SET THE CYCLE TO THE TOTAL NUMBER OF TIME       INDAT1.......31600
C              STEPS, SO THAT THE SCHEDULE CONSISTS OF TIME STEPS        INDAT1.......31700
C              0, 1, AND ITMAX.  (VERSION 2.0 SIMPLY BOMBS IF            INDAT1.......31800
C              NOBCYC=0.)                                                INDAT1.......31900
            IF (NOBCYC.EQ.0) THEN                                        INDAT1.......32000
               ISTEPC = ITERM                                            INDAT1.......32100
            ELSE                                                         INDAT1.......32200
               ISTEPC = IABS(NOBCYC)                                     INDAT1.......32300
            END IF                                                       INDAT1.......32400
            NTORS = INT(ITERM/ISTEPC) + 1                                INDAT1.......32500
            IF (MOD(ITERM,ISTEPC).NE.0) NTORS = NTORS + 1                INDAT1.......32600
            NSTEP = ISTEPI                                               INDAT1.......32700
            LSTLEN = 0                                                   INDAT1.......32800
            DENTS => SCHDLS(ISCHTS)%SLIST                                INDAT1.......32900
            JT = 0                                                       INDAT1.......33000
            DO 580 NT=1,NTORS                                            INDAT1.......33100
               NSTEP = MIN(ISTEPL, ISTEPI + (NT - 1)*ISTEPC)             INDAT1.......33200
               DO WHILE (NSTEP.GT.JT)                                    INDAT1.......33300
                  DENTS => DENTS%NENT                                    INDAT1.......33400
                  JT = JT + 1                                            INDAT1.......33500
               END DO                                                    INDAT1.......33600
               STEP = DENTS%DVALU2                                       INDAT1.......33700
               TIME = DENTS%DVALU1                                       INDAT1.......33800
               CALL LLDINS(LSTLEN, SCHDLS(2)%SLIST, TIME, STEP)          INDAT1.......33900
  580       CONTINUE                                                     INDAT1.......34000
C...........IF NOBCYC>=0, INCLUDE TIME STEP 1 IF NOT ALREADY INCLUDED.   INDAT1.......34100
            IF ((NOBCYC.GE.0).AND.(NOBCYC.NE.1)) THEN                    INDAT1.......34200
               DENTS => SCHDLS(ISCHTS)%SLIST                             INDAT1.......34300
               STEP = DNINT(DBLE(1))                                     INDAT1.......34400
               TIME = DENTS%NENT%DVALU1                                  INDAT1.......34500
               CALL LLDINS(LSTLEN, SCHDLS(2)%SLIST, TIME, STEP)          INDAT1.......34600
            END IF                                                       INDAT1.......34700
            SCHDLS(2)%LLEN = LSTLEN                                      INDAT1.......34800
         END IF                                                          INDAT1.......34900
C........CONVERT NODES TO GENERALIZED OBSERVATION POINTS.  THE POINTS    INDAT1.......35000
C           ARE NAMED "NODE_#", WHERE # IS THE NODE NUMBER.              INDAT1.......35100
         DO 540 I=1,NOBS                                                 INDAT1.......35200
            WRITE(OBSPTS(I)%NAME,*) INOB(I)                              INDAT1.......35300
            OBSPTS(I)%NAME = "NODE_" // ADJUSTL(OBSPTS(I)%NAME)          INDAT1.......35400
            OBSPTS(I)%SCHED = "-"                                        INDAT1.......35500
            OBSPTS(I)%FRMT = "OBS"                                       INDAT1.......35600
            OBSPTS(I)%L = INOB(I)                                        INDAT1.......35700
  540    CONTINUE                                                        INDAT1.......35800
C........DEALLOCATE TEMPORARY ARRAY.                                     INDAT1.......35900
         DEALLOCATE(INOB)                                                INDAT1.......36000
C........SKIP PAST THE CODE THAT READS A LIST OF GENERALIZED             INDAT1.......36100
C           OBSERVATION POINTS.                                          INDAT1.......36200
         GOTO 820                                                        INDAT1.......36300
      END IF                                                             INDAT1.......36400
C.....READ THE LIST OF GENERALIZED OBSERVATION POINTS.                   INDAT1.......36500
      NOBCYC = -1                                                        INDAT1.......36600
      DO 690 I=1,NOBS                                                    INDAT1.......36700
C........READ THE OBSERVATION NAME.                                      INDAT1.......36800
         CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                 INDAT1.......36900
	   IF (IERROR.NE.0) RETURN
         READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM                           INDAT1.......37000
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT1.......37100
	      RETURN
	   ENDIF
C........IF END-OF-LIST MARKER ENCOUNTERED TOO SOON, GENERATE ERROR.     INDAT1.......37200
         IF (OBSNAM.EQ.'-') THEN                                         INDAT1.......37300
            ERRCOD = 'INP-8D-4'                                          INDAT1.......37400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT1.......37500
	      RETURN
         END IF                                                          INDAT1.......37600
C........READ IN (X,Y,Z) OR (X,Y) COORDINATES, DEPENDING ON PROBLEM      INDAT1.......37700
C           DIMENSIONALITY, AS WELL AS OUTPUT SCHEDULE AND FORMAT.       INDAT1.......37800
         IF (KTYPE(1).EQ.3) THEN                                         INDAT1.......37900
            READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM, XOBS, YOBS, ZOBS,     INDAT1.......38000
     1         OBSSCH, OBSFMT                                            INDAT1.......38100
         ELSE                                                            INDAT1.......38200
            READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM, XOBS, YOBS,           INDAT1.......38300
     1         OBSSCH, OBSFMT                                            INDAT1.......38400
         END IF                                                          INDAT1.......38500
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT1.......38600
	      RETURN
	   ENDIF
         OBSPTS(I)%NAME = OBSNAM                                         INDAT1.......38700
         OBSPTS(I)%X = XOBS                                              INDAT1.......38800
         OBSPTS(I)%Y = YOBS                                              INDAT1.......38900
         OBSPTS(I)%Z = ZOBS                                              INDAT1.......39000
         IF (ISSTRA.EQ.1) THEN                                           INDAT1.......39100
            OBSPTS(I)%SCHED = "-"                                        INDAT1.......39200
         ELSE                                                            INDAT1.......39300
            OBSPTS(I)%SCHED = OBSSCH                                     INDAT1.......39400
         END IF                                                          INDAT1.......39500
         OBSPTS(I)%FRMT = OBSFMT                                         INDAT1.......39600
  690 CONTINUE                                                           INDAT1.......39700
C.....READ ONE MORE LINE TO CHECK FOR THE END-OF-LIST MARKER ('-').      INDAT1.......39800
C        IF NOT FOUND, GENERATE ERROR.                                   INDAT1.......39900
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......40000
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM                              INDAT1.......40100
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......40200
	   RETURN
	ENDIF
      IF (OBSNAM.NE.'-') THEN                                            INDAT1.......40300
         ERRCOD = 'INP-8D-4'                                             INDAT1.......40400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......40500
	   RETURN
      END IF                                                             INDAT1.......40600
C                                                                        INDAT1.......40700
C.....CONDENSE SCHEDULE AND FILE TYPE INFORMATION FROM OBSDAT INTO       INDAT1.......40800
C        ARRAY OFP, CHECKING FOR UNDEFINED SCHEDULES                     INDAT1.......40900
  820 ALLOCATE (OFP(NSCH*2))                                             INDAT1.......41000
      IF (ISSTRA.EQ.1) THEN                                              INDAT1.......41100
         NFLOMX=0                                                        INDAT1.......41200
         DO 840 I=1,NOBS                                                 INDAT1.......41300
            DO 835 J=1,NFLOMX                                            INDAT1.......41400
               IF (OBSPTS(I)%FRMT.EQ.OFP(J)%FRMT) GOTO 840               INDAT1.......41500
  835       CONTINUE                                                     INDAT1.......41600
            NFLOMX = NFLOMX + 1                                          INDAT1.......41700
            OFP(NFLOMX)%ISCHED = 2                                       INDAT1.......41800
            OFP(NFLOMX)%FRMT = OBSPTS(I)%FRMT                            INDAT1.......41900
  840    CONTINUE                                                        INDAT1.......42000
      ELSE                                                               INDAT1.......42100
         NFLOMX = 0                                                      INDAT1.......42200
         DO 860 I=1,NOBS                                                 INDAT1.......42300
            DO 850 NS=1,NSCH                                             INDAT1.......42400
               IF (OBSPTS(I)%SCHED.EQ.SCHDLS(NS)%NAME) THEN              INDAT1.......42500
                  INS = NS                                               INDAT1.......42600
                  GOTO 852                                               INDAT1.......42700
               END IF                                                    INDAT1.......42800
  850       CONTINUE                                                     INDAT1.......42900
            ERRCOD = 'INP-8D-5'                                          INDAT1.......43000
            CHERR(1) = OBSPTS(I)%SCHED                                   INDAT1.......43100
            CHERR(2) = OBSPTS(I)%NAME                                    INDAT1.......43200
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT1.......43300
	      RETURN
  852       DO 855 J=1,NFLOMX                                            INDAT1.......43400
               IF ((OBSPTS(I)%SCHED.EQ.SCHDLS(OFP(J)%ISCHED)%NAME).AND.  INDAT1.......43500
     1             (OBSPTS(I)%FRMT.EQ.OFP(J)%FRMT)) GOTO 860             INDAT1.......43600
  855       CONTINUE                                                     INDAT1.......43700
            NFLOMX = NFLOMX + 1                                          INDAT1.......43800
            OFP(NFLOMX)%ISCHED = INS                                     INDAT1.......43900
            OFP(NFLOMX)%FRMT = OBSPTS(I)%FRMT                            INDAT1.......44000
  860    CONTINUE                                                        INDAT1.......44100
      END IF                                                             INDAT1.......44200
C                                                                        INDAT1.......44300
C.....ASSIGN UNIT NUMBERS AND OPEN FILE UNITS FOR OBSERVATION OUTPUT     INDAT1.......44400
C        FILES.                                                          INDAT1.......44500
      ALLOCATE(IUNIO(NFLOMX),FNAMO(NFLOMX))                              INDAT1.......44600
      CALL FOPEN(IERROR)                                                       INDAT1.......44700
	IF (IERROR.NE.0) RETURN
C                                                                        INDAT1.......44800
C.....OUTPUT OBSERVATION FILE INFORMATION                                INDAT1.......44900
!      IF (ISSTRA.EQ.1) THEN                                              INDAT1.......45000
!         WRITE(K3,868) IUNIO(1),FNAMO(1)                                 INDAT1.......45100
!  868    FORMAT (//13X,'.OBS AND .OBC FILES'/13X,'-------------------'// INDAT1.......45200
!     1      (13X,'UNIT ',I7,4X,'ASSIGNED TO ',A80))                      INDAT1.......45300
!         WRITE(K3,869)                                                   INDAT1.......45400
!  869    FORMAT (/13X,'NOTE: BECAUSE FLOW AND TRANSPORT ARE STEADY-',    INDAT1.......45500
!     1      'STATE, USER-DEFINED SCHEDULES ARE NOT IN EFFECT.  '         INDAT1.......45600
!     2      /13X,'STEADY-STATE OBSERVATIONS WILL BE WRITTEN TO THE ',    INDAT1.......45700
!     3      'APPROPRIATE OUTPUT FILES.')                                 INDAT1.......45800
!      ELSE IF (VERNIN.NE."2.0") THEN                                     INDAT1.......45900
!         WRITE(K3,870) (SCHDLS(OFP(J)%ISCHED)%NAME,OFP(J)%FRMT,          INDAT1.......46000
!     1      IUNIO(J),FNAMO(J),J=1,NFLOMX)                                INDAT1.......46100
!  870    FORMAT (//13X,'.OBS AND .OBC FILES'/13X,'-------------------'// INDAT1.......46200
!     1      (13X,'SCHEDULE ',A,', FORMAT ',A,', UNIT ',I7,4X,            INDAT1.......46300
!     2      'ASSIGNED TO ',A80))                                         INDAT1.......46400
!      ELSE                                                               INDAT1.......46500
!         WRITE(K3,868) IUNIO(1),FNAMO(1)                                 INDAT1.......46600
!         WRITE(K3,872) NOBCYC, SCHDLS(2)%LLEN                            INDAT1.......46700
!  872    FORMAT (/13X,'NOTE: OBSERVATION OUTPUT CYCLING ',               INDAT1.......46800
!     1      'INFORMATION WAS ENTERED USING THE OLD (VERSION 2D3D.1) '    INDAT1.......46900
!     2      'INPUT FORMAT.'/13X,'OBSERVATIONS WILL BE MADE EVERY ',I8,   INDAT1.......47000
!     3      ' TIME STEPS, AS WELL AS ON THE FIRST AND LAST TIME STEP,'   INDAT1.......47100
!     4      /13X,'FOR A TOTAL OF ',I8,' TIME STEPS.')                    INDAT1.......47200
!      END IF                                                             INDAT1.......47300
C                                                                        INDAT1.......47400
C.....OUTPUT GENERALIZED OBSERVATION POINT INFORMATION.                  INDAT1.......47500
!      WRITE(K3,1182)                                                     INDAT1.......47600
! 1182 FORMAT(////11X,'O B S E R V A T I O N   P O I N T S')              INDAT1.......47700
C.....3D PROBLEM.                                                        INDAT1.......47800
!      IF (KTYPE(1).EQ.3) THEN                                            INDAT1.......47900
C........WRITE HEADER.                                                   INDAT1.......48000
!         WRITE(K3,1187)                                                  INDAT1.......48100
! 1187    FORMAT(                                                         INDAT1.......48200
!     1        //13X,'NAME',42X,'COORDINATES',37X,'SCHEDULE',4X,'FORMAT'  INDAT1.......48300
!     2         /13X,'----',42X,'-----------',37X,'--------',4X,'------') INDAT1.......48400
C........PRINT INFORMATION FOR EACH POINT.  IF POINTS WERE CONVERTED     INDAT1.......48500
C           FROM NODES, COORDINATES HAVE YET TO BE READ IN, SO PUT IN    INDAT1.......48600
C           A PLACEHOLDER.                                               INDAT1.......48700
!         IF (NOBCYC.NE.-1) THEN                                          INDAT1.......48800
!            DO 1189 JJ=1,NOBS                                            INDAT1.......48900
!               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......49000
!               WRITE(K3,1188) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),   INDAT1.......49100
!     1            OBSPTS(JJ)%SCHED,OBSPTS(JJ)%FRMT                       INDAT1.......49200
! 1188          FORMAT(13X,A,1X,A,1X,                                     INDAT1.......49300
!     1            '( _______ TO BE READ FROM DATASET 14 _______ )',      INDAT1.......49400
!     2            3X,A,2X,A)                                             INDAT1.......49500
! 1189       CONTINUE                                                     INDAT1.......49600
!         ELSE                                                            INDAT1.......49700
!            DO 1191 JJ=1,NOBS                                            INDAT1.......49800
!               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......49900
!               WRITE(K3,1190) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),   INDAT1.......50000
!     1            OBSPTS(JJ)%X,OBSPTS(JJ)%Y,OBSPTS(JJ)%Z,                INDAT1.......50100
!     2            OBSPTS(JJ)%SCHED,OBSPTS(JJ)%FRMT                       INDAT1.......50200
! 1190          FORMAT(13X,A,1X,A,1X,'(',2(1PE14.7,','),1PE14.7,')',      INDAT1.......50300
!     1            3X,A,2X,A)                                             INDAT1.......50400
! 1191       CONTINUE                                                     INDAT1.......50500
!         END IF                                                          INDAT1.......50600
C.....2D PROBLEM.                                                        INDAT1.......50700
!      ELSE                                                               INDAT1.......50800
C........WRITE HEADER.                                                   INDAT1.......50900
!         WRITE(K3,1193)                                                  INDAT1.......51000
! 1193    FORMAT(                                                         INDAT1.......51100
!     1        //13X,'NAME',42X,'COORDINATES',22X,'SCHEDULE',4X,'FORMAT'  INDAT1.......51200
!     2         /13X,'----',42X,'-----------',22X,'--------',4X,'------') INDAT1.......51300
C........PRINT INFORMATION FOR EACH POINT.  IF POINTS WERE CONVERTED     INDAT1.......51400
C           FROM NODES, COORDINATES HAVE YET TO BE READ IN, SO PUT IN    INDAT1.......51500
C           A PLACEHOLDER.                                               INDAT1.......51600
!         IF (NOBCYC.NE.-1) THEN                                          INDAT1.......51700
!            DO 1195 JJ=1,NOBS                                            INDAT1.......51800
!               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......51900
!               WRITE(K3,1194) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),   INDAT1.......52000
!     1            OBSPTS(JJ)%SCHED,OBSPTS(JJ)%FRMT                       INDAT1.......52100
! 1194          FORMAT(13X,A,1X,A,1X,'( TO BE READ FROM DATASET 14  )',   INDAT1.......52200
!     1            3X,A,2X,A)                                             INDAT1.......52300
! 1195       CONTINUE                                                     INDAT1.......52400
!         ELSE                                                            INDAT1.......52500
!            DO 1197 JJ=1,NOBS                                            INDAT1.......52600
!               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......52700
!               WRITE(K3,1196) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),   INDAT1.......52800
!     1            OBSPTS(JJ)%X,OBSPTS(JJ)%Y,                             INDAT1.......52900
!     2            OBSPTS(JJ)%SCHED,OBSPTS(JJ)%FRMT                       INDAT1.......53000
! 1196          FORMAT(13X,A,1X,A,1X,'(',1PE14.7,',',1PE14.7,')',         INDAT1.......53100
!     1            3X,A,2X,A)                                             INDAT1.......53200
! 1197       CONTINUE                                                     INDAT1.......53300
!         END IF                                                          INDAT1.......53400
!      END IF                                                             INDAT1.......53500
 1199 CONTINUE                                                           INDAT1.......53600
C                                                                        INDAT1.......53700
C.....INPUT DATASET 9:  FLUID PROPERTIES                                 INDAT1.......53800
      ERRCOD = 'REA-INP-9'                                               INDAT1.......53900
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......54000
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) COMPFL,CW,SIGMAW,RHOW0,URHOW0,      INDAT1.......54100
     1   DRWDU,VISC0                                                     INDAT1.......54200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......54300
	   RETURN
	ENDIF
C.....INPUT DATASET 10:  SOLID MATRIX PROPERTIES                         INDAT1.......54400
      ERRCOD = 'REA-INP-10'                                              INDAT1.......54500
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......54600
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) COMPMA,CS,SIGMAS,RHOS               INDAT1.......54700
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......54800
	   RETURN
	ENDIF
!      IF(ME.EQ.+1)                                                       INDAT1.......54900
!     1  WRITE(K3,1210) COMPFL,COMPMA,CW,CS,VISC0,RHOS,RHOW0,DRWDU,       INDAT1.......55000
!     2     URHOW0,SIGMAW,SIGMAS                                          INDAT1.......55100
! 1210 FORMAT('1'////11X,'C O N S T A N T   P R O P E R T I E S   O F',   INDAT1.......55200
!     1   '   F L U I D   A N D   S O L I D   M A T R I X'                INDAT1.......55300
!     2   //11X,1PE15.4,5X,'COMPRESSIBILITY OF FLUID'/11X,1PE15.4,5X,     INDAT1.......55400
!     3   'COMPRESSIBILITY OF POROUS MATRIX'//11X,1PE15.4,5X,             INDAT1.......55500
!     4   'SPECIFIC HEAT CAPACITY OF FLUID',/11X,1PE15.4,5X,              INDAT1.......55600
!     5   'SPECIFIC HEAT CAPACITY OF SOLID GRAIN'//13X,'FLUID VISCOSITY', INDAT1.......55700
!     6   ' IS CALCULATED BY SUTRA AS A FUNCTION OF TEMPERATURE IN ',     INDAT1.......55800
!     7   'UNITS OF {kg/(m*s)}'//11X,1PE15.4,5X,'VISC0, CONVERSION ',     INDAT1.......55900
!     8   'FACTOR FOR VISCOSITY UNITS,  {desired units} = VISC0*',        INDAT1.......56000
!     9   '{kg/(m*s)}'//11X,1PE15.4,5X,'DENSITY OF A SOLID GRAIN'         INDAT1.......56100
!     *   //13X,'FLUID DENSITY, RHOW'/13X,'CALCULATED BY ',               INDAT1.......56200
!     1   'SUTRA IN TERMS OF TEMPERATURE, U, AS:'/13X,'RHOW = RHOW0 + ',  INDAT1.......56300
!     2   'DRWDU*(U-URHOW0)'//11X,1PE15.4,5X,'FLUID BASE DENSITY, RHOW0'  INDAT1.......56400
!     3   /11X,1PE15.4,5X,'COEFFICIENT OF DENSITY CHANGE WITH ',          INDAT1.......56500
!     4   'TEMPERATURE, DRWDU'/11X,1PE15.4,5X,'TEMPERATURE, URHOW0, ',    INDAT1.......56600
!     5   'AT WHICH FLUID DENSITY IS AT BASE VALUE, RHOW0'                INDAT1.......56700
!     6   //11X,1PE15.4,5X,'THERMAL CONDUCTIVITY OF FLUID'                INDAT1.......56800
!     7   /11X,1PE15.4,5X,'THERMAL CONDUCTIVITY OF SOLID GRAIN')          INDAT1.......56900
!      IF(ME.EQ.-1)                                                       INDAT1.......57000
!     1  WRITE(K3,1220) COMPFL,COMPMA,VISC0,RHOS,RHOW0,DRWDU,             INDAT1.......57100
!     2     URHOW0,SIGMAW                                                 INDAT1.......57200
! 1220 FORMAT('1'////11X,'C O N S T A N T   P R O P E R T I E S   O F',   INDAT1.......57300
!     1   '   F L U I D   A N D   S O L I D   M A T R I X'                INDAT1.......57400
!     2   //11X,1PE15.4,5X,'COMPRESSIBILITY OF FLUID'/11X,1PE15.4,5X,     INDAT1.......57500
!     3   'COMPRESSIBILITY OF POROUS MATRIX'                              INDAT1.......57600
!     4   //11X,1PE15.4,5X,'FLUID VISCOSITY'                              INDAT1.......57700
!     4   //11X,1PE15.4,5X,'DENSITY OF A SOLID GRAIN'                     INDAT1.......57800
!     5   //13X,'FLUID DENSITY, RHOW'/13X,'CALCULATED BY ',               INDAT1.......57900
!     6   'SUTRA IN TERMS OF SOLUTE CONCENTRATION, U, AS:',               INDAT1.......58000
!     7   /13X,'RHOW = RHOW0 + DRWDU*(U-URHOW0)'                          INDAT1.......58100
!     8   //11X,1PE15.4,5X,'FLUID BASE DENSITY, RHOW0'                    INDAT1.......58200
!     9   /11X,1PE15.4,5X,'COEFFICIENT OF DENSITY CHANGE WITH ',          INDAT1.......58300
!     *   'SOLUTE CONCENTRATION, DRWDU'                                   INDAT1.......58400
!     1   /11X,1PE15.4,5X,'SOLUTE CONCENTRATION, URHOW0, ',               INDAT1.......58500
!     4   'AT WHICH FLUID DENSITY IS AT BASE VALUE, RHOW0'                INDAT1.......58600
!     5   //11X,1PE15.4,5X,'MOLECULAR DIFFUSIVITY OF SOLUTE IN FLUID')    INDAT1.......58700
C                                                                        INDAT1.......58800
C.....INPUT DATASET 11:  ADSORPTION PARAMETERS                           INDAT1.......58900
      ERRCOD = 'REA-INP-11'                                              INDAT1.......59000
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......59100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) ADSMOD                              INDAT1.......59200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......59300
	   RETURN
	ENDIF
      IF (ADSMOD.NE.'NONE      ') THEN                                   INDAT1.......59400
         READ(INTFIL,*,IOSTAT=INERR(1)) ADSMOD,CHI1,CHI2                 INDAT1.......59500
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              INDAT1.......59600
	      RETURN
	   ENDIF
      END IF                                                             INDAT1.......59700
      IF(ME.EQ.+1) GOTO 1248                                             INDAT1.......59800
      IF(ADSMOD.EQ.'NONE      ') GOTO 1234                               INDAT1.......59900
!      WRITE(K3,1232) ADSMOD                                              INDAT1.......60000
! 1232 FORMAT(////11X,'A D S O R P T I O N   P A R A M E T E R S'         INDAT1.......60100
!     1   //16X,A10,5X,'EQUILIBRIUM SORPTION ISOTHERM')                   INDAT1.......60200
      GOTO 1236                                                          INDAT1.......60300
 1234 CONTINUE
! 1234 WRITE(K3,1235)                                                     INDAT1.......60400
! 1235 FORMAT(////11X,'A D S O R P T I O N   P A R A M E T E R S'         INDAT1.......60500
!     1   //16X,'NON-SORBING SOLUTE')                                     INDAT1.......60600
 1236 IF((ADSMOD.EQ.'NONE ').OR.(ADSMOD.EQ.'LINEAR    ').OR.             INDAT1.......60700
     1   (ADSMOD.EQ.'FREUNDLICH').OR.(ADSMOD.EQ.'LANGMUIR  ')) GOTO 1238 INDAT1.......60800
      ERRCOD = 'INP-11-1'                                                INDAT1.......60900
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                           INDAT1.......61000
	RETURN
 1238 CONTINUE
! 1238 IF(ADSMOD.EQ.'LINEAR    ') WRITE(K3,1242) CHI1                     INDAT1.......61100
! 1242 FORMAT(11X,1PE15.4,5X,'LINEAR DISTRIBUTION COEFFICIENT')           INDAT1.......61200
!      IF(ADSMOD.EQ.'FREUNDLICH') WRITE(K3,1244) CHI1,CHI2                INDAT1.......61300
! 1244 FORMAT(11X,1PE15.4,5X,'FREUNDLICH DISTRIBUTION COEFFICIENT'        INDAT1.......61400
!     1   /11X,1PE15.4,5X,'SECOND FREUNDLICH COEFFICIENT')                INDAT1.......61500
      IF(ADSMOD.EQ.'FREUNDLICH'.AND.CHI2.LE.0.D0) THEN                   INDAT1.......61600
         ERRCOD = 'INP-11-2'                                             INDAT1.......61700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......61800
	   RETURN
      ENDIF                                                              INDAT1.......61900
!      IF(ADSMOD.EQ.'LANGMUIR  ') WRITE(K3,1246) CHI1,CHI2                INDAT1.......62000
! 1246 FORMAT(11X,1PE15.4,5X,'LANGMUIR DISTRIBUTION COEFFICIENT'          INDAT1.......62100
!     1   /11X,1PE15.4,5X,'SECOND LANGMUIR COEFFICIENT')                  INDAT1.......62200
C                                                                        INDAT1.......62300
C.....INPUT DATASET 12:  PRODUCTION OF ENERGY OR SOLUTE MASS             INDAT1.......62400
 1248 ERRCOD = 'REA-INP-12'                                              INDAT1.......62500
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......62600
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) PRODF0,PRODS0,PRODF1,PRODS1         INDAT1.......62700
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......62800
	   RETURN
	ENDIF
!      IF(ME.EQ.-1) WRITE(K3,1250) PRODF0,PRODS0,PRODF1,PRODS1            INDAT1.......62900
! 1250 FORMAT(////11X,'P R O D U C T I O N   A N D   D E C A Y   O F   ', INDAT1.......63000
!     1   'S P E C I E S   M A S S'//13X,'PRODUCTION RATE (+)'/13X,       INDAT1.......63100
!     2   'DECAY RATE (-)'//11X,1PE15.4,5X,'ZERO-ORDER RATE OF SOLUTE ',  INDAT1.......63200
!     3   'MASS PRODUCTION/DECAY IN FLUID'/11X,1PE15.4,5X,                INDAT1.......63300
!     4   'ZERO-ORDER RATE OF ADSORBATE MASS PRODUCTION/DECAY IN ',       INDAT1.......63400
!     5   'IMMOBILE PHASE'/11X,1PE15.4,5X,'FIRST-ORDER RATE OF SOLUTE ',  INDAT1.......63500
!     3   'MASS PRODUCTION/DECAY IN FLUID'/11X,1PE15.4,5X,                INDAT1.......63600
!     4   'FIRST-ORDER RATE OF ADSORBATE MASS PRODUCTION/DECAY IN ',      INDAT1.......63700
!     5   'IMMOBILE PHASE')                                               INDAT1.......63800
!      IF(ME.EQ.+1) WRITE(K3,1260) PRODF0,PRODS0                          INDAT1.......63900
! 1260 FORMAT(////11X,'P R O D U C T I O N   A N D   L O S S   O F   ',   INDAT1.......64000
!     1   'E N E R G Y'//13X,'PRODUCTION RATE (+)'/13X,                   INDAT1.......64100
!     2   'LOSS RATE (-)'//11X,1PE15.4,5X,'ZERO-ORDER RATE OF ENERGY ',   INDAT1.......64200
!     3   'PRODUCTION/LOSS IN FLUID'/11X,1PE15.4,5X,                      INDAT1.......64300
!     4   'ZERO-ORDER RATE OF ENERGY PRODUCTION/LOSS IN ',                INDAT1.......64400
!     5   'SOLID GRAINS')                                                 INDAT1.......64500
C.....SET PARAMETER SWITCHES FOR EITHER ENERGY OR SOLUTE TRANSPORT       INDAT1.......64600
      IF(ME) 1272,1272,1274                                              INDAT1.......64700
C     FOR SOLUTE TRANSPORT:                                              INDAT1.......64800
 1272 CS=0.0D0                                                           INDAT1.......64900
      CW=1.D00                                                           INDAT1.......65000
      SIGMAS=0.0D0                                                       INDAT1.......65100
      GOTO 1278                                                          INDAT1.......65200
C     FOR ENERGY TRANSPORT:                                              INDAT1.......65300
 1274 ADSMOD='NONE      '                                                INDAT1.......65400
      CHI1=0.0D0                                                         INDAT1.......65500
      CHI2=0.0D0                                                         INDAT1.......65600
      PRODF1=0.0D0                                                       INDAT1.......65700
      PRODS1=0.0D0                                                       INDAT1.......65800
 1278 CONTINUE                                                           INDAT1.......65900
C                                                                        INDAT1.......66000
      IF (KTYPE(1).EQ.3) THEN                                            INDAT1.......66100
C.....READ 3D INPUT FROM DATASETS 13 - 15.                               INDAT1.......66200
C                                                                        INDAT1.......66300
C.....INPUT DATASET 13:  ORIENTATION OF COORDINATES TO GRAVITY           INDAT1.......66400
      ERRCOD = 'REA-INP-13'                                              INDAT1.......66500
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......66600
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) GRAVX,GRAVY,GRAVZ                   INDAT1.......66700
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......66800
	   RETURN
	ENDIF
!      WRITE(K3,1320) GRAVX,GRAVY,GRAVZ                                   INDAT1.......66900
! 1320 FORMAT(////11X,'C O O R D I N A T E   O R I E N T A T I O N   ',   INDAT1.......67000
!     1   'T O   G R A V I T Y'//13X,'COMPONENT OF GRAVITY VECTOR',       INDAT1.......67100
!     2   /13X,'IN +X DIRECTION, GRAVX'/11X,1PE15.4,5X,                   INDAT1.......67200
!     3   'GRAVX = -GRAV * D(ELEVATION)/DX'//13X,'COMPONENT OF GRAVITY',  INDAT1.......67300
!     4   ' VECTOR'/13X,'IN +Y DIRECTION, GRAVY'/11X,1PE15.4,5X,          INDAT1.......67400
!     5   'GRAVY = -GRAV * D(ELEVATION)/DY'//13X,'COMPONENT OF GRAVITY',  INDAT1.......67500
!     6   ' VECTOR'/13X,'IN +Z DIRECTION, GRAVZ'/11X,1PE15.4,5X,          INDAT1.......67600
!     7   'GRAVZ = -GRAV * D(ELEVATION)/DZ')                              INDAT1.......67700
C                                                                        INDAT1.......67800
C.....INPUT DATASETS 14A & 14B:  NODEWISE DATA                           INDAT1.......67900
      ERRCOD = 'REA-INP-14A'                                             INDAT1.......68000
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......68100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,SCALX,SCALY,SCALZ,PORFAC     INDAT1.......68200
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......68300
	   RETURN
	ENDIF
      IF (CDUM10.NE.'NODE      ') THEN                                   INDAT1.......68400
         ERRCOD = 'INP-14A-1'                                            INDAT1.......68500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......68600
	   RETURN
      END IF                                                             INDAT1.......68700
      NRTEST=1                                                           INDAT1.......68800
      DO 1450 I=1,NN                                                     INDAT1.......68900
      ERRCOD = 'REA-INP-14B'                                             INDAT1.......69000
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......69100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) II,NREG(II),X(II),Y(II),Z(II),      INDAT1.......69200
     1   POR(II)                                                         INDAT1.......69300
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......69400
	   RETURN
	ENDIF
      X(II)=X(II)*SCALX                                                  INDAT1.......69500
      Y(II)=Y(II)*SCALY                                                  INDAT1.......69600
      Z(II)=Z(II)*SCALZ                                                  INDAT1.......69700
      POR(II)=POR(II)*PORFAC                                             INDAT1.......69800
C RBW
	IF (INodeValueCount.GT.0) THEN
	  if (II.GT.INodeValueCount) then
	    IERROR = 1
	    return
	  endif
	  NodeValues(II) = POR(II)
	ENDIF
C RBW
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                   INDAT1.......69900
      NROLD=NREG(II)                                                     INDAT1.......70000
C.....SET SPECIFIC PRESSURE STORATIVITY, SOP.                            INDAT1.......70100
 1450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                       INDAT1.......70200
! 1460 IF(KNODAL.EQ.0) WRITE(K3,1461) SCALX,SCALY,SCALZ,PORFAC            INDAT1.......70300
! 1461 FORMAT('1'////11X,'N O D E   I N F O R M A T I O N'//16X,          INDAT1.......70400
!     1   'PRINTOUT OF NODE COORDINATES AND POROSITIES ',                 INDAT1.......70500
!     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'X-SCALE'/   INDAT1.......70600
!     3   33X,1PE15.4,5X,'Y-SCALE'/33X,1PE15.4,5X,'Z-SCALE'/              INDAT1.......70700
!     4   33X,1PE15.4,5X,'POROSITY FACTOR')                               INDAT1.......70800
!      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.NE.1) WRITE(K3,1463)     INDAT1.......70900
!      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.EQ.1) WRITE(K3,1465)     INDAT1.......71000
! 1463 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......71100
!     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......71200
! 1465 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......71300
!     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......71400
!      IF(KNODAL.EQ.+1.AND.IUNSAT.NE.1)                                   INDAT1.......71500
!     1   WRITE(K3,1470)(I,X(I),Y(I),Z(I),POR(I),I=1,NN)                  INDAT1.......71600
! 1470 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,            INDAT1.......71700
!     1   'NODE',7X,'X',16X,'Y',16X,'Z',15X,'POROSITY'//                  INDAT1.......71800
!     2   (9X,I9,3(3X,1PE14.5),6X,0PF8.5))                                INDAT1.......71900
!      IF(KNODAL.EQ.+1.AND.IUNSAT.EQ.1)                                   INDAT1.......72000
!     1   WRITE(K3,1480)(I,NREG(I),X(I),Y(I),Z(I),POR(I),I=1,NN)          INDAT1.......72100
! 1480 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,'NODE',3X,  INDAT1.......72200
!     1   'REGION',7X,'X',16X,'Y',16X,'Z',15X,'POROSITY'//                INDAT1.......72300
!     2   (9X,I9,3X,I6,3(3X,1PE14.5),6X,0PF8.5))                          INDAT1.......72400
C                                                                        INDAT1.......72500
C.....INPUT DATASETS 15A & 15B:  ELEMENTWISE DATA                        INDAT1.......72600
      ERRCOD = 'REA-INP-15A'                                             INDAT1.......72700
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......72800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,PMAXFA,PMIDFA,PMINFA,        INDAT1.......72900
     1   ANG1FA,ANG2FA,ANG3FA,ALMAXF,ALMIDF,ALMINF,                      INDAT1.......73000
     1   ATMXF,ATMDF,ATMNF                                               INDAT1.......73100
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......73200
	   RETURN
	ENDIF
      IF (CDUM10.NE.'ELEMENT   ') THEN                                   INDAT1.......73300
         ERRCOD = 'INP-15A-1'                                            INDAT1.......73400
         CHERR(1) = '3D'                                                 INDAT1.......73500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......73600
	   RETURN
      END IF                                                             INDAT1.......73700
!      IF(KELMNT.EQ.+1) THEN                                              INDAT1.......73800
!         IF (IUNSAT.EQ.1) THEN                                           INDAT1.......73900
!            WRITE(K3,1500)                                               INDAT1.......74000
! 1500       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......74100
!     1         11X,'ELEMENT',3X,'REGION',4X,                             INDAT1.......74200
!     2         'MAXIMUM',9X,'MIDDLE',10X,'MINIMUM',18X,                  INDAT1.......74300
!     2         'ANGLE1',9X,'ANGLE2',9X,'ANGLE3',4X,                      INDAT1.......74400
!     2         'LONGITUDINAL',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,    INDAT1.......74500
!     2         'TRANSVERSE',5X,'TRANSVERSE',5X,'TRANSVERSE'/             INDAT1.......74600
!     3         31X,'PERMEABILITY',4X,'PERMEABILITY',4X,'PERMEABILITY',   INDAT1.......74700
!     4         8X,'(IN DEGREES)',3X,'(IN DEGREES)',3X,'(IN DEGREES)',3X, INDAT1.......74800
!     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY',3X,    INDAT1.......74900
!     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY'/       INDAT1.......75000
!     4         128X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM',  INDAT1.......75100
!     4         3X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM'/    INDAT1.......75200
!     1         128X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION',  INDAT1.......75300
!     2         3X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)   INDAT1.......75400
!         ELSE                                                            INDAT1.......75500
!            WRITE(K3,1501)                                               INDAT1.......75600
! 1501       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......75700
!     1         11X,'ELEMENT',4X,                                         INDAT1.......75800
!     2         'MAXIMUM',9X,'MIDDLE',10X,'MINIMUM',18X,                  INDAT1.......75900
!     2         'ANGLE1',9X,'ANGLE2',9X,'ANGLE3',4X,                      INDAT1.......76000
!     2         'LONGITUDINAL',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,    INDAT1.......76100
!     2         'TRANSVERSE',5X,'TRANSVERSE',5X,'TRANSVERSE'/             INDAT1.......76200
!     3         22X,'PERMEABILITY',4X,'PERMEABILITY',4X,'PERMEABILITY',   INDAT1.......76300
!     4         8X,'(IN DEGREES)',3X,'(IN DEGREES)',3X,'(IN DEGREES)',3X, INDAT1.......76400
!     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY',3X,    INDAT1.......76500
!     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY'/       INDAT1.......76600
!     4         119X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM',  INDAT1.......76700
!     4         3X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM'/    INDAT1.......76800
!     1         119X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION',  INDAT1.......76900
!     2         3X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)   INDAT1.......77000
!         END IF                                                          INDAT1.......77100
!      END IF                                                             INDAT1.......77200
      LRTEST=1                                                           INDAT1.......77300
      DO 1550 LL=1,NE                                                    INDAT1.......77400
      ERRCOD = 'REA-INP-15B'                                             INDAT1.......77500
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......77600
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) L,LREG(L),PMAX,PMID,PMIN,           INDAT1.......77700
     1   ANGLE1,ANGLE2,ANGLE3,ALMAX(L),ALMID(L),ALMIN(L),                INDAT1.......77800
     1   ATMAX(L),ATMID(L),ATMIN(L)                                      INDAT1.......77900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......78000
	   RETURN
	ENDIF
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                   INDAT1.......78100
      LROLD=LREG(L)                                                      INDAT1.......78200
      PMAX=PMAX*PMAXFA                                                   INDAT1.......78300
      PMID=PMID*PMIDFA                                                   INDAT1.......78400
      PMIN=PMIN*PMINFA                                                   INDAT1.......78500
      ANGLE1=ANGLE1*ANG1FA                                               INDAT1.......78600
      ANGLE2=ANGLE2*ANG2FA                                               INDAT1.......78700
      ANGLE3=ANGLE3*ANG3FA                                               INDAT1.......78800
      ALMAX(L)=ALMAX(L)*ALMAXF                                           INDAT1.......78900
      ALMID(L)=ALMID(L)*ALMIDF                                           INDAT1.......79000
      ALMIN(L)=ALMIN(L)*ALMINF                                           INDAT1.......79100
      ATMAX(L)=ATMAX(L)*ATMXF                                            INDAT1.......79200
      ATMID(L)=ATMID(L)*ATMDF                                            INDAT1.......79300
      ATMIN(L)=ATMIN(L)*ATMNF                                            INDAT1.......79400
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
!      IF(KELMNT.EQ.+1.AND.IUNSAT.NE.1) WRITE(K3,1520) L,                 INDAT1.......79500
!     1   PMAX,PMID,PMIN,ANGLE1,ANGLE2,ANGLE3,                            INDAT1.......79600
!     2   ALMAX(L),ALMID(L),ALMIN(L),ATMAX(L),ATMID(L),ATMIN(L)           INDAT1.......79700
! 1520 FORMAT(9X,I9,2X,3(1PE14.5,2X),7X,9(G11.4,4X))                      INDAT1.......79800
!      IF(KELMNT.EQ.+1.AND.IUNSAT.EQ.1) WRITE(K3,1530) L,LREG(L),         INDAT1.......79900
!     1   PMAX,PMID,PMIN,ANGLE1,ANGLE2,ANGLE3,                            INDAT1.......80000
!     2   ALMAX(L),ALMID(L),ALMIN(L),ATMAX(L),ATMID(L),ATMIN(L)           INDAT1.......80100
! 1530 FORMAT(9X,I9,4X,I5,2X,3(1PE14.5,2X),7X,9(G11.4,4X))                INDAT1.......80200
C                                                                        INDAT1.......80300
C.....ROTATE PERMEABILITY FROM MAX/MID/MIN TO X/Y/Z DIRECTIONS.          INDAT1.......80400
C        BASED ON CODE WRITTEN BY DAVID POLLOCK (USGS).                  INDAT1.......80500
      D2R=1.745329252D-2                                                 INDAT1.......80600
      PANGL1(L)=D2R*ANGLE1                                               INDAT1.......80700
      PANGL2(L)=D2R*ANGLE2                                               INDAT1.......80800
      PANGL3(L)=D2R*ANGLE3                                               INDAT1.......80900
      ZERO = 0D0                                                         INDAT1.......81000
!      CALL ROTMAT(PANGL1(L),PANGL2(L),PANGL3(L),Q11,Q12,Q13,             INDAT1.......81100
!     1   Q21,Q22,Q23,Q31,Q32,Q33)                                        INDAT1.......81200
!      CALL TENSYM(PMAX,PMID,PMIN,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q33,    INDAT1.......81300
!     1   PERMXX(L),PERMXY(L),PERMXZ(L),PERMYX(L),PERMYY(L),PERMYZ(L),    INDAT1.......81400
!     2   PERMZX(L),PERMZY(L),PERMZZ(L))                                  INDAT1.......81500
 1550 CONTINUE                                                           INDAT1.......81600
!      IF(KELMNT.EQ.0)                                                    INDAT1.......81700
!     1   WRITE(K3,1569) PMAXFA,PMIDFA,PMINFA,ANG1FA,ANG2FA,ANG3FA,       INDAT1.......81800
!     2      ALMAXF,ALMIDF,ALMINF,ATMXF,ATMDF,ATMNF                       INDAT1.......81900
! 1569 FORMAT(////11X,'E L E M E N T   I N F O R M A T I O N'//           INDAT1.......82000
!     1   16X,'PRINTOUT OF ELEMENT PERMEABILITIES AND DISPERSIVITIES ',   INDAT1.......82100
!     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'MAXIMUM ',  INDAT1.......82200
!     3   'PERMEABILITY FACTOR'/33X,1PE15.4,5X,'MIDDLE PERMEABILITY ',    INDAT1.......82300
!     4   'FACTOR '/33X,1PE15.4,5X,'MINIMUM PERMEABILITY FACTOR'/         INDAT1.......82400
!     5   33X,1PE15.4,5X,'ANGLE1 FACTOR'/33X,1PE15.4,5X,'ANGLE2 FACTOR'/  INDAT1.......82500
!     6   33X,1PE15.4,5X,'ANGLE3 FACTOR'/                                 INDAT1.......82600
!     7   33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL DISPERSIVITY IN ',      INDAT1.......82700
!     8   'MAX-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL ', INDAT1.......82800
!     9   'DISPERSIVITY IN MID-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR ',  INDAT1.......82900
!     T   'FOR LONGITUDINAL DISPERSIVITY IN MIN-PERM DIRECTION'/          INDAT1.......83000
!     1   33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE DISPERSIVITY IN ',        INDAT1.......83100
!     2   'MAX-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE ',   INDAT1.......83200
!     3   'DISPERSIVITY IN MID-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR',   INDAT1.......83300
!     4   ' FOR TRANSVERSE DISPERSIVITY IN MIN-PERM DIRECTION')           INDAT1.......83400
!      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.NE.1) WRITE(K3,1573)     INDAT1.......83500
!      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.EQ.1) WRITE(K3,1575)     INDAT1.......83600
! 1573 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......83700
!     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......83800
! 1575 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......83900
!     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......84000
C                                                                        INDAT1.......84100
      ELSE                                                               INDAT1.......84200
C.....READ 2D INPUT FROM DATASETS 13 - 15.                               INDAT1.......84300
C.....NOTE THAT Z = THICKNESS AND PANGL1 = PANGLE.                       INDAT1.......84400
C                                                                        INDAT1.......84500
C.....INPUT DATASET 13:  ORIENTATION OF COORDINATES TO GRAVITY           INDAT1.......84600
      ERRCOD = 'REA-INP-13'                                              INDAT1.......84700
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......84800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) GRAVX,GRAVY                         INDAT1.......84900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......85000
	   RETURN
	ENDIF
      GRAVZ = 0D0                                                        INDAT1.......85100
!      WRITE(K3,2320) GRAVX,GRAVY                                         INDAT1.......85200
! 2320 FORMAT(////11X,'C O O R D I N A T E   O R I E N T A T I O N   ',   INDAT1.......85300
!     1   'T O   G R A V I T Y'//13X,'COMPONENT OF GRAVITY VECTOR',       INDAT1.......85400
!     2   /13X,'IN +X DIRECTION, GRAVX'/11X,1PE15.4,5X,                   INDAT1.......85500
!     3   'GRAVX = -GRAV * D(ELEVATION)/DX'//13X,'COMPONENT OF GRAVITY',  INDAT1.......85600
!     4   ' VECTOR'/13X,'IN +Y DIRECTION, GRAVY'/11X,1PE15.4,5X,          INDAT1.......85700
!     5   'GRAVY = -GRAV * D(ELEVATION)/DY')                              INDAT1.......85800
C                                                                        INDAT1.......85900
C.....INPUT DATASETS 14A & 14B:  NODEWISE DATA                           INDAT1.......86000
      ERRCOD = 'REA-INP-14A'                                             INDAT1.......86100
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......86200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,SCALX,SCALY,SCALTH,PORFAC    INDAT1.......86300
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......86400
	   RETURN
	ENDIF
      IF (CDUM10.NE.'NODE      ') THEN                                   INDAT1.......86500
         ERRCOD = 'INP-14A-1'                                            INDAT1.......86600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......86700
	   RETURN
      END IF                                                             INDAT1.......86800
      NRTEST=1                                                           INDAT1.......86900
      DO 2450 I=1,NN                                                     INDAT1.......87000
      ERRCOD = 'REA-INP-14B'                                             INDAT1.......87100
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......87200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) II,NREG(II),X(II),Y(II),Z(II),      INDAT1.......87300
     1   POR(II)                                                         INDAT1.......87400
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......87500
	   RETURN
	ENDIF
      X(II)=X(II)*SCALX                                                  INDAT1.......87600
      Y(II)=Y(II)*SCALY                                                  INDAT1.......87700
      Z(II)=Z(II)*SCALTH                                                 INDAT1.......87800
      POR(II)=POR(II)*PORFAC                                             INDAT1.......87900
C RBW
	IF (INodeValueCount.GT.0) THEN
	  if (II.GT.INodeValueCount) then
	    IERROR = 1
	    return
	  endif
	  NodeValues(II) = POR(II)
	ENDIF
C RBW
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                   INDAT1.......88000
      NROLD=NREG(II)                                                     INDAT1.......88100
C.....SET SPECIFIC PRESSURE STORATIVITY, SOP.                            INDAT1.......88200
 2450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                       INDAT1.......88300
! 2460 IF(KNODAL.EQ.0) WRITE(K3,2461) SCALX,SCALY,SCALTH,PORFAC           INDAT1.......88400
! 2461 FORMAT('1'////11X,'N O D E   I N F O R M A T I O N'//16X,          INDAT1.......88500
!     1   'PRINTOUT OF NODE COORDINATES, THICKNESSES AND POROSITIES ',    INDAT1.......88600
!     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'X-SCALE'/   INDAT1.......88700
!     1   33X,1PE15.4,5X,'Y-SCALE'/33X,1PE15.4,5X,'THICKNESS FACTOR'/     INDAT1.......88800
!     2   33X,1PE15.4,5X,'POROSITY FACTOR')                               INDAT1.......88900
!      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.NE.1) WRITE(K3,2463)     INDAT1.......89000
!      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.EQ.1) WRITE(K3,2465)     INDAT1.......89100
! 2463 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......89200
!     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......89300
! 2465 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......89400
!     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......89500
!      IF(KNODAL.EQ.+1.AND.IUNSAT.NE.1)                                   INDAT1.......89600
!     1   WRITE(K3,2470)(I,X(I),Y(I),Z(I),POR(I),I=1,NN)                  INDAT1.......89700
! 2470 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,            INDAT1.......89800
!     1   'NODE',7X,'X',16X,'Y',17X,'THICKNESS',6X,'POROSITY'//           INDAT1.......89900
!     2   (9X,I9,3(3X,1PE14.5),6X,0PF8.5))                                INDAT1.......90000
!      IF(KNODAL.EQ.+1.AND.IUNSAT.EQ.1)                                   INDAT1.......90100
!     1   WRITE(K3,2480)(I,NREG(I),X(I),Y(I),Z(I),POR(I),I=1,NN)          INDAT1.......90200
! 2480 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,'NODE',3X,  INDAT1.......90300
!     1   'REGION',7X,'X',16X,'Y',17X,'THICKNESS',6X,'POROSITY'//         INDAT1.......90400
!     2   (9X,I9,3X,I6,3(3X,1PE14.5),6X,0PF8.5))                          INDAT1.......90500
C                                                                        INDAT1.......90600
C.....INPUT DATASETS 15A & 15B:  ELEMENTWISE DATA                        INDAT1.......90700
      ERRCOD = 'REA-INP-15A'                                             INDAT1.......90800
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......90900
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,PMAXFA,PMINFA,ANGFAC,        INDAT1.......91000
     1   ALMAXF,ALMINF,ATMAXF,ATMINF                                     INDAT1.......91100
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......91200
	   RETURN
	ENDIF
      IF (CDUM10.NE.'ELEMENT   ') THEN                                   INDAT1.......91300
         ERRCOD = 'INP-15A-1'                                            INDAT1.......91400
         CHERR(1) = '2D'                                                 INDAT1.......91500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT1.......91600
	   RETURN
      END IF                                                             INDAT1.......91700
!      IF (KELMNT.EQ.+1) THEN                                             INDAT1.......91800
!         IF (IUNSAT.EQ.1) THEN                                           INDAT1.......91900
!            WRITE(K3,2500)                                               INDAT1.......92000
! 2500       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......92100
!     1         11X,'ELEMENT',3X,'REGION',4X,'MAXIMUM',9X,'MINIMUM',12X,  INDAT1.......92200
!     2         'ANGLE BETWEEN',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,   INDAT1.......92300
!     3         'TRANSVERSE',5X,'TRANSVERSE'/                             INDAT1.......92400
!     4         31X,'PERMEABILITY',4X,'PERMEABILITY',4X,                  INDAT1.......92500
!     5         '+X-DIRECTION AND',3X,'DISPERSIVITY',3X,'DISPERSIVITY',   INDAT1.......92600
!     6         3X,'DISPERSIVITY',3X,'DISPERSIVITY'/                      INDAT1.......92700
!     7         59X,'MAXIMUM PERMEABILITY',3X,' IN MAX-PERM',             INDAT1.......92800
!     8         3X,' IN MIN-PERM',3X,' IN MAX-PERM',3X,' IN MIN-PERM'/    INDAT1.......92900
!     9         67X,'(IN DEGREES)',3X,'   DIRECTION',3X,                  INDAT1.......93000
!     1         '   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)      INDAT1.......93100
!         ELSE                                                            INDAT1.......93200
!            WRITE(K3,2501)                                               INDAT1.......93300
! 2501       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......93400
!     1         11X,'ELEMENT',4X,'MAXIMUM',9X,'MINIMUM',12X,              INDAT1.......93500
!     2         'ANGLE BETWEEN',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,   INDAT1.......93600
!     3         'TRANSVERSE',5X,'TRANSVERSE'/                             INDAT1.......93700
!     4         22X,'PERMEABILITY',4X,'PERMEABILITY',4X,                  INDAT1.......93800
!     5         '+X-DIRECTION AND',3X,'DISPERSIVITY',3X,'DISPERSIVITY',   INDAT1.......93900
!     6         3X,'DISPERSIVITY',3X,'DISPERSIVITY'/                      INDAT1.......94000
!     7         50X,'MAXIMUM PERMEABILITY',3X,' IN MAX-PERM',             INDAT1.......94100
!     8         3X,' IN MIN-PERM',3X,' IN MAX-PERM',3X,' IN MIN-PERM'/    INDAT1.......94200
!     9         58X,'(IN DEGREES)',3X,'   DIRECTION',3X,                  INDAT1.......94300
!     1         '   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)      INDAT1.......94400
!         END IF                                                          INDAT1.......94500
!      END IF                                                             INDAT1.......94600
      LRTEST=1                                                           INDAT1.......94700
      DO 2550 LL=1,NE                                                    INDAT1.......94800
      ERRCOD = 'REA-INP-15B'                                             INDAT1.......94900
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    INDAT1.......95000
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) L,LREG(L),PMAX,PMIN,ANGLEX,         INDAT1.......95100
     1   ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)                             INDAT1.......95200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT1.......95300
	   RETURN
	ENDIF
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                   INDAT1.......95400
      LROLD=LREG(L)                                                      INDAT1.......95500
      PMAX=PMAX*PMAXFA                                                   INDAT1.......95600
      PMIN=PMIN*PMINFA                                                   INDAT1.......95700
      ANGLEX=ANGLEX*ANGFAC                                               INDAT1.......95800
      ALMAX(L)=ALMAX(L)*ALMAXF                                           INDAT1.......95900
      ALMIN(L)=ALMIN(L)*ALMINF                                           INDAT1.......96000
      ATMAX(L)=ATMAX(L)*ATMAXF                                           INDAT1.......96100
      ATMIN(L)=ATMIN(L)*ATMINF                                           INDAT1.......96200
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
!      IF(KELMNT.EQ.+1.AND.IUNSAT.NE.1) WRITE(K3,2520) L,                 INDAT1.......96300
!     1   PMAX,PMIN,ANGLEX,ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)            INDAT1.......96400
! 2520 FORMAT(9X,I9,2X,2(1PE14.5,2X),7X,5(G11.4,4X))                      INDAT1.......96500
!      IF(KELMNT.EQ.+1.AND.IUNSAT.EQ.1) WRITE(K3,2530) L,LREG(L),         INDAT1.......96600
!     1   PMAX,PMIN,ANGLEX,ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)            INDAT1.......96700
! 2530 FORMAT(9X,I9,4X,I5,2X,2(1PE14.5,2X),7X,5(G11.4,4X))                INDAT1.......96800
C                                                                        INDAT1.......96900
C.....ROTATE PERMEABILITY FROM MAXIMUM/MINIMUM TO X/Y DIRECTIONS         INDAT1.......97000
      RADIAX=1.745329D-2*ANGLEX                                          INDAT1.......97100
      SINA=DSIN(RADIAX)                                                  INDAT1.......97200
      COSA=DCOS(RADIAX)                                                  INDAT1.......97300
      SINA2=SINA*SINA                                                    INDAT1.......97400
      COSA2=COSA*COSA                                                    INDAT1.......97500
      PERMXX(L)=PMAX*COSA2+PMIN*SINA2                                    INDAT1.......97600
      PERMYY(L)=PMAX*SINA2+PMIN*COSA2                                    INDAT1.......97700
      PERMXY(L)=(PMAX-PMIN)*SINA*COSA                                    INDAT1.......97800
      PERMYX(L)=PERMXY(L)                                                INDAT1.......97900
      PANGL1(L)=RADIAX                                                   INDAT1.......98000
 2550 CONTINUE                                                           INDAT1.......98100
!      IF(KELMNT.EQ.0)                                                    INDAT1.......98200
!     1   WRITE(K3,2569) PMAXFA,PMINFA,ANGFAC,ALMAXF,ALMINF,ATMAXF,ATMINF INDAT1.......98300
! 2569 FORMAT(////11X,'E L E M E N T   I N F O R M A T I O N'//           INDAT1.......98400
!     1   16X,'PRINTOUT OF ELEMENT PERMEABILITIES AND DISPERSIVITIES ',   INDAT1.......98500
!     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'MAXIMUM ',  INDAT1.......98600
!     3   'PERMEABILITY FACTOR'/33X,1PE15.4,5X,'MINIMUM PERMEABILITY ',   INDAT1.......98700
!     4   'FACTOR'/33X,1PE15.4,5X,'ANGLE FROM +X TO MAXIMUM DIRECTION',   INDAT1.......98800
!     5   ' FACTOR'/33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL DISPERSIVITY' INDAT1.......98900
!     6  ,' IN MAX-PERM DIRECTION'/33X,1PE15.4,5X,                        INDAT1.......99000
!     7   'FACTOR FOR LONGITUDINAL DISPERSIVITY IN MIN-PERM DIRECTION',   INDAT1.......99100
!     8   /33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE DISPERSIVITY',           INDAT1.......99200
!     9   ' IN MAX-PERM DIRECTION'/33X,1PE15.4,5X,                        INDAT1.......99300
!     *   'FACTOR FOR TRANSVERSE DISPERSIVITY IN MIN-PERM DIRECTION')     INDAT1.......99400
!      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.NE.1) WRITE(K3,2573)     INDAT1.......99500
!      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.EQ.1) WRITE(K3,2575)     INDAT1.......99600
! 2573 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......99700
!     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......99800
! 2575 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......99900
!     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1......100000
C                                                                        INDAT1......100100
      END IF                                                             INDAT1......100200
C                                                                        INDAT1......100300
      RETURN                                                             INDAT1......100400
      END                                                                INDAT1......100500
C                                                                        INDAT1......100600
C     SUBROUTINE        I  N  D  A  T  2           SUTRA VERSION 2.1     INDAT2.........100
C                                                                        INDAT2.........200
C *** PURPOSE :                                                          INDAT2.........300
C ***  TO READ INITIAL CONDITIONS FROM ICS FILE, AND TO                  INDAT2.........400
C ***  INITIALIZE DATA FOR EITHER WARM OR COLD START OF                  INDAT2.........500
C ***  THE SIMULATION.                                                   INDAT2.........600
C                                                                        INDAT2.........700
      SUBROUTINE INDAT2(PVEC,UVEC,PM1,UM1,UM2,CS1,CS2,CS3,SL,SR,RCIT,    INDAT2.........800
     1   SW,DSWDP,PBC,IPBC,IPBCT,NREG,QIN,DPDTITR, IERROR)                       INDAT2.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT2........1000
      CHARACTER*10 CPUNI,CUUNI                                           INDAT2........1100
      CHARACTER INTFIL*1000                                              INDAT2........1200
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     INDAT2........1300
      DIMENSION INERR(10),RLERR(10)                                      INDAT2........1400
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),PM1(NN),UM1(NN),UM2(NN),SL(NN),  INDAT2........1500
     1   SR(NN),CS1(NN),CS2(NN),CS3(NN),RCIT(NN),SW(NN),DSWDP(NN),       INDAT2........1600
     2   PBC(NBCN),IPBC(NBCN),NREG(NN),QIN(NN),DPDTITR(NN)               INDAT2........1700
      DIMENSION KTYPE(2)                                                 INDAT2........1800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT2........1900
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT2........2000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT2........2100
     1   NSOP,NSOU,NBCN                                                  INDAT2........2200
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           INDAT2........2300
      COMMON /FNAMES/ UNAME,FNAME                                        INDAT2........2400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     INDAT2........2500
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT2........2600
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT2........2700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT2........2800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT2........2900
C                                                                        INDAT2........3000
C                                                                        INDAT2........3100
C RBW
      RETURN
C RBW
      IF(IREAD) 500,500,620                                              INDAT2........3200
C.....INPUT INITIAL CONDITIONS FOR WARM START                            INDAT2........3300
  500 ERRCOD = 'REA-ICS-1'                                               INDAT2........3400
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2........3500
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) DUM,DELTP,DELTU                     INDAT2........3600
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........3700
	   RETURN
	ENDIF
      ERRCOD = 'REA-ICS-2'                                               INDAT2........3800
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2........3900
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CPUNI                               INDAT2........4000
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........4100
	   RETURN
	ENDIF
      IF (CPUNI.NE.'NONUNIFORM') THEN                                    INDAT2........4200
         ERRCOD = 'ICS-2-2'                                              INDAT2........4300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT2........4400
	   RETURN
      END IF                                                             INDAT2........4500
      ERRCOD = 'REA-ICS-2'                                               INDAT2........4600
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2........4700
	IF (IERROR.NE.0) RETURN
      BACKSPACE(K2)                                                      INDAT2........4800
      READ(K2,*,IOSTAT=INERR(1)) (PVEC(I),I=1,NN)                        INDAT2........4900
      IF (INERR(1).NE.0) THEN                                            INDAT2........5000
         ERRCOD = 'REA-ICS-2'                                            INDAT2........5100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT2........5200
	   RETURN
      END IF                                                             INDAT2........5300
      ERRCOD = 'REA-ICS-3'                                               INDAT2........5400
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2........5500
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CUUNI                               INDAT2........5600
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........5700
	   RETURN
	ENDIF
      IF (CUUNI.NE.'NONUNIFORM') THEN                                    INDAT2........5800
         ERRCOD = 'ICS-3-2'                                              INDAT2........5900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT2........6000
	   RETURN
      END IF                                                             INDAT2........6100
      ERRCOD = 'REA-ICS-3'                                               INDAT2........6200
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2........6300
	IF (IERROR.NE.0) RETURN
      BACKSPACE(K2)                                                      INDAT2........6400
      READ(K2,*,IOSTAT=INERR(1)) (UVEC(I),I=1,NN)                        INDAT2........6500
      IF (INERR(1).NE.0) THEN                                            INDAT2........6600
         ERRCOD = 'REA-ICS-3'                                            INDAT2........6700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT2........6800
	   RETURN
      END IF                                                             INDAT2........6900
      ERRCOD = 'REA-ICS-4'                                               INDAT2........7000
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2........7100
	IF (IERROR.NE.0) RETURN
      BACKSPACE(K2)                                                      INDAT2........7200
      READ(K2,*,IOSTAT=INERR(1)) (PM1(I),I=1,NN)                         INDAT2........7300
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........7400
	   RETURN
	ENDIF
      READ(K2,*,IOSTAT=INERR(1)) (UM1(I),I=1,NN)                         INDAT2........7500
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........7600
	   RETURN
	ENDIF
      READ(K2,*,IOSTAT=INERR(1)) (CS1(I),I=1,NN)                         INDAT2........7700
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........7800
	   RETURN
	ENDIF
      READ(K2,*,IOSTAT=INERR(1)) (RCIT(I),I=1,NN)                        INDAT2........7900
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........8000
	   RETURN
	ENDIF
      READ(K2,*,IOSTAT=INERR(1)) (SW(I),I=1,NN)                          INDAT2........8100
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........8200
	   RETURN
	ENDIF
      READ(K2,*,IOSTAT=INERR(1)) (QIN(I),I=1,NN)                         INDAT2........8300
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........8400
	   RETURN
	ENDIF
      READ(K2,*,IOSTAT=INERR(1)) (PBC(IPU),IPU=1,NBCN)                   INDAT2........8500
      IF (INERR(1).NE.0) THEN 
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2........8600
	   RETURN
	ENDIF
C     CALL ZERO(CS2,NN,0.0D0)                                            INDAT2........8700
C     CALL ZERO(CS3,NN,0.0D0)                                            INDAT2........8800
      CALL ZERO(SL,NN,0.0D0)                                             INDAT2........8900
      CALL ZERO(SR,NN,0.0D0)                                             INDAT2........9000
      CALL ZERO(DSWDP,NN,0.0D0)                                          INDAT2........9100
      DO 550 I=1,NN                                                      INDAT2........9200
  550 UM2(I)=UM1(I)                                                      INDAT2........9300
      GOTO 1000                                                          INDAT2........9400
C                                                                        INDAT2........9500
C.....INPUT INITIAL CONDITIONS FOR COLD START                            INDAT2........9600
  620 ERRCOD = 'REA-ICS-1'                                               INDAT2........9700
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2........9800
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) DUM                                 INDAT2........9900
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2.......10000
	   RETURN
	ENDIF
      ERRCOD = 'REA-ICS-2'                                               INDAT2.......10100
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2.......10200
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CPUNI                               INDAT2.......10300
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2.......10400
	   RETURN
	ENDIF
      IF (CPUNI.EQ.'UNIFORM') THEN                                       INDAT2.......10500
         ERRCOD = 'REA-ICS-2'                                            INDAT2.......10600
         CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                 INDAT2.......10700
	IF (IERROR.NE.0) RETURN
         BACKSPACE(K2)                                                   INDAT2.......10800
         READ(K2,*,IOSTAT=INERR(1)) PUNI                                 INDAT2.......10900
         IF (INERR(1).NE.0) THEN                                         INDAT2.......11000
            ERRCOD = 'REA-ICS-2'                                         INDAT2.......11100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT2.......11200
	      RETURN
         END IF                                                          INDAT2.......11300
         DO 625 I=1,NN                                                   INDAT2.......11400
            PVEC(I) = PUNI                                               INDAT2.......11500
  625    CONTINUE                                                        INDAT2.......11600
      ELSE IF (CPUNI.EQ.'NONUNIFORM') THEN                               INDAT2.......11700
         ERRCOD = 'REA-ICS-2'                                            INDAT2.......11800
         CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                 INDAT2.......11900
	   IF (IERROR.NE.0) RETURN
         BACKSPACE(K2)                                                   INDAT2.......12000
         READ(K2,*,IOSTAT=INERR(1)) (PVEC(I),I=1,NN)                     INDAT2.......12100
         IF (INERR(1).NE.0) THEN                                         INDAT2.......12200
            ERRCOD = 'REA-ICS-2'                                         INDAT2.......12300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT2.......12400
	      RETURN
         END IF                                                          INDAT2.......12500
      ELSE                                                               INDAT2.......12600
         ERRCOD = 'ICS-2-1'                                              INDAT2.......12700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT2.......12800
	   RETURN
      END IF                                                             INDAT2.......12900
      ERRCOD = 'REA-ICS-3'                                               INDAT2.......13000
      CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                    INDAT2.......13100
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) CUUNI                               INDAT2.......13200
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 INDAT2.......13300
	   RETURN
      END IF                                                             INDAT2.......12900
      IF (CUUNI.EQ.'UNIFORM') THEN                                       INDAT2.......13400
         ERRCOD = 'REA-ICS-3'                                            INDAT2.......13500
         CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                 INDAT2.......13600
	   IF (IERROR.NE.0) RETURN
         BACKSPACE(K2)                                                   INDAT2.......13700
         READ(K2,*,IOSTAT=INERR(1)) UUNI                                 INDAT2.......13800
         IF (INERR(1).NE.0) THEN                                         INDAT2.......13900
            ERRCOD = 'REA-ICS-3'                                         INDAT2.......14000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT2.......14100
	      RETURN
         END IF                                                          INDAT2.......14200
         DO 630 I=1,NN                                                   INDAT2.......14300
            UVEC(I) = UUNI                                               INDAT2.......14400
  630    CONTINUE                                                        INDAT2.......14500
      ELSE IF (CUUNI.EQ.'NONUNIFORM') THEN                               INDAT2.......14600
         ERRCOD = 'REA-ICS-3'                                            INDAT2.......14700
         CALL READIF(K2, INTFIL, ERRCOD,IERROR)                                 INDAT2.......14800
	   IF (IERROR.NE.0) RETURN
         BACKSPACE(K2)                                                   INDAT2.......14900
         READ(K2,*,IOSTAT=INERR(1)) (UVEC(I),I=1,NN)                     INDAT2.......15000
         IF (INERR(1).NE.0) THEN                                         INDAT2.......15100
            ERRCOD = 'REA-ICS-3'                                         INDAT2.......15200
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     INDAT2.......15300
	      RETURN
         END IF                                                          INDAT2.......15400
      ELSE                                                               INDAT2.......15500
         ERRCOD = 'ICS-3-1'                                              INDAT2.......15600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        INDAT2.......15700
	   RETURN
      END IF                                                             INDAT2.......15800
C.....START-UP WITH NO PROJECTIONS BY SETTING DELTP AND DELTU            INDAT2.......15900
C        SUCH THAT BDELP=BDELU=0.5D-16 IN PROJECTION FORMULAE FOUND      INDAT2.......16000
C        IN SUBROUTINE SUTRA.                                            INDAT2.......16100
      DELTP=DELT*1.D16                                                   INDAT2.......16200
      DELTU=DELT*1.D16                                                   INDAT2.......16300
C.....INITIALIZE SPECIFIED TIME-VARYING PRESSURES TO INITIAL PRESSURE    INDAT2.......16400
C        VALUES FOR START-UP CALCULATION OF INFLOWS OR OUTFLOWS          INDAT2.......16500
C        (SET QPLITR=0)                                                  INDAT2.......16600
      IF(IPBCT) 680,740,740                                              INDAT2.......16700
  680 DO 730 IP=1,NPBC                                                   INDAT2.......16800
      I=IPBC(IP)                                                         INDAT2.......16900
      IF(I) 700,700,730                                                  INDAT2.......17000
  700 PBC(IP)=PVEC(-I)                                                   INDAT2.......17100
  730 CONTINUE                                                           INDAT2.......17200
C.....INITIALIZE P, U, AND CONSISTENT DENSITY                            INDAT2.......17300
  740 DO 800 I=1,NN                                                      INDAT2.......17400
      PM1(I)=PVEC(I)                                                     INDAT2.......17500
      UM1(I)=UVEC(I)                                                     INDAT2.......17600
      UM2(I)=UVEC(I)                                                     INDAT2.......17700
      RCIT(I)=RHOW0+DRWDU*(UVEC(I)-URHOW0)                               INDAT2.......17800
  800 CONTINUE                                                           INDAT2.......17900
C.....INITIALIZE SATURATION, SW(I)                                       INDAT2.......18000
      CALL ZERO(SW,NN,1.0D0)                                             INDAT2.......18100
      CALL ZERO(DSWDP,NN,0.0D0)                                          INDAT2.......18200
      IF(IUNSAT.NE.1) GOTO 990                                           INDAT2.......18300
      IUNSAT=3                                                           INDAT2.......18400
!      DO 900 I=1,NN                                                      INDAT2.......18500
!  900 IF(PVEC(I).LT.0) CALL UNSAT(SW(I),DSWDP(I),RELK,PVEC(I),NREG(I))   INDAT2.......18600
  990 CONTINUE                                                           INDAT2.......18700
      CALL ZERO(CS1,NN,CS)                                               INDAT2.......18800
C     CALL ZERO(CS2,NN,0.0D0)                                            INDAT2.......18900
C     CALL ZERO(CS3,NN,0.0D0)                                            INDAT2.......19000
      CALL ZERO(SL,NN,0.0D0)                                             INDAT2.......19100
      CALL ZERO(SR,NN,0.0D0)                                             INDAT2.......19200
      CALL ZERO(DPDTITR,NN,0.0D0)                                        INDAT2.......19300
 1000 CONTINUE                                                           INDAT2.......19400
C                                                                        INDAT2.......19500
C.....SET STARTING TIME OF SIMULATION CLOCK, TSEC.  NOTE THAT THE VALUE  INDAT2.......19600
C        OF TSTART WAS SET IN SUBROUTINE INDAT0, WHERE THE "TIME_STEPS"  INDAT2.......19700
C        SCHEDULE IS DEFINED.                                            INDAT2.......19800
      TSEC=TSTART                                                        INDAT2.......19900
C                                                                        INDAT2.......20000
C                                                                        INDAT2.......20100
      RETURN                                                             INDAT2.......20200
      END                                                                INDAT2.......20300
C                                                                        INDAT2.......20400
C     SUBROUTINE        L  L  D  2  A  R           SUTRA VERSION 2.1     LLD2AR.........100
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
C     SUBROUTINE        L  L  D  I  N  S           SUTRA VERSION 2.1     LLDINS.........100
C                                                                        LLDINS.........200
C *** PURPOSE :                                                          LLDINS.........300
C ***  TO INSERT A PAIR OF DOUBLE-PRECISION VALUES INTO A LINKED         LLDINS.........400
C ***  LIST, IN ASCENDING ORDER BASED ON THE FIRST VALUE IN THE PAIR.    LLDINS.........500
C                                                                        LLDINS.........600
      SUBROUTINE LLDINS(LSTLEN, DLIST, DNUM1, DNUM2)                     LLDINS.........700
      USE LLDEF                                                          LLDINS.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LLDINS.........900
      TYPE (LLD), POINTER :: DEN, DENPV, DENPI, DENNW, DLIST             LLDINS........1000
C                                                                        LLDINS........1100
C.....IF LIST IS EMPTY, PLACE PAIR AT HEAD OF LIST, ELSE INSERT          LLDINS........1200
C        INTO LIST IN ASCENDING ORDER BASED ON FIRST VALUE               LLDINS........1300
      IF (LSTLEN.EQ.0) THEN                                              LLDINS........1400
C........PLACE AT HEAD                                                   LLDINS........1500
         DLIST%DVALU1 = DNUM1                                            LLDINS........1600
         DLIST%DVALU2 = DNUM2                                            LLDINS........1700
         GOTO 780                                                        LLDINS........1800
      ELSE                                                               LLDINS........1900
C........INSERT INTO LISTS                                               LLDINS........2000
         ALLOCATE(DENPV)                                                 LLDINS........2100
         DENPI => DENPV                                                  LLDINS........2200
         DENPV%NENT => DLIST                                             LLDINS........2300
         DO 770 K=1,LSTLEN                                               LLDINS........2400
            DEN => DENPV%NENT                                            LLDINS........2500
            IF (DNUM1.LT.DEN%DVALU1) THEN                                LLDINS........2600
               ALLOCATE(DENNW)                                           LLDINS........2700
               DENNW%DVALU1 = DNUM1                                      LLDINS........2800
               DENNW%DVALU2 = DNUM2                                      LLDINS........2900
               DENNW%NENT => DEN                                         LLDINS........3000
               IF (K.EQ.1) THEN                                          LLDINS........3100
                  DLIST => DENNW                                         LLDINS........3200
               ELSE                                                      LLDINS........3300
                  DENPV%NENT => DENNW                                    LLDINS........3400
               END IF                                                    LLDINS........3500
               DEALLOCATE(DENPI)                                         LLDINS........3600
               GOTO 780                                                  LLDINS........3700
            END IF                                                       LLDINS........3800
            DENPV => DEN                                                 LLDINS........3900
  770    CONTINUE                                                        LLDINS........4000
C........APPEND TO TAIL                                                  LLDINS........4100
         ALLOCATE(DENNW)                                                 LLDINS........4200
         DENNW%DVALU1 = DNUM1                                            LLDINS........4300
         DENNW%DVALU2 = DNUM2                                            LLDINS........4400
         DEN%NENT => DENNW                                               LLDINS........4500
         DEALLOCATE(DENPI)                                               LLDINS........4600
      END IF                                                             LLDINS........4700
C                                                                        LLDINS........4800
  780 LSTLEN = LSTLEN + 1                                                LLDINS........4900
      RETURN                                                             LLDINS........5000
      END                                                                LLDINS........5100
C                                                                        LLDINS........5200
C     SUBROUTINE        L  O  D  O  B  S           SUTRA VERSION 2.1     LODOBS.........100
C                                                                        LODOBS.........200
C *** PURPOSE :                                                          LODOBS.........300
C ***  TO LOAD OBSERVATION POINT INDICES NOBLIN AT A TIME, STARTING      LODOBS.........400
C ***  WITH INDEX JNEXT, INTO ARRAY JSET.  ONLY OBSERVATION POINTS       LODOBS.........500
C ***  WHOSE SCHEDULE AND OUTPUT FORMAT MATCH THOSE THAT CORRESPOND      LODOBS.........600
C ***  TO FILE INDEX NFLO ARE LOADED.  THE NUMBER OF OBSERVATIONS        LODOBS.........700
C ***  LOADED, JLOAD, CAN BE LESS THAN NOBLIN IF THE LIST OF             LODOBS.........800
C ***  OBSERVATION POINT INDICES IS EXHAUSTED.                           LODOBS.........900
C                                                                        LODOBS........1000
C     SUBROUTINE        N  A  F  U                 SUTRA VERSION 2.1     NAFU...........100
C                                                                        NAFU...........200
C *** PURPOSE :                                                          NAFU...........300
C ***  TO FIND THE NEXT AVAILABLE FORTRAN UNIT.  ON INPUT, IUNEXT IS     NAFU...........400
C ***  THE UNIT NUMBER FROM WHICH THE SEARCH IS TO BEGIN.  ON OUTPUT,    NAFU...........500
C ***  IUNEXT IS THE NEXT AVAILABLE UNIT NUMBER.                         NAFU...........600
C                                                                        NAFU...........700
      SUBROUTINE NAFU(IUNEXT,NJMAX,FN, IERROR)                                   NAFU...........800
      USE SCHDEF, ONLY : IUNIO                                           NAFU...........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                NAFU..........1000
      CHARACTER*80 FN,UNAME                                              NAFU..........1100
      CHARACTER*80 ERRCOD,CHERR(10)                                      NAFU..........1200
      LOGICAL EXST                                                       NAFU..........1300
      DIMENSION INERR(10),RLERR(10)                                      NAFU..........1400
      DIMENSION IUNIT(0:8), NKS(2), KLIST(2,20)                          NAFU..........1500
      COMMON /FNAMES/ UNAME,FNAME                                        NAFU..........1600
      COMMON /FUNINS/ NKS,KLIST                                          NAFU..........1700
      COMMON /FUNITA/ IUNIT                                              NAFU..........1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     NAFU..........1900
C                                                                        NAFU..........2000
C.....CHECK "SUTRA.FIL" (UNIT K0)                                        NAFU..........2100
  100 IF (IUNEXT.EQ.K0) IUNEXT = IUNEXT + 1                              NAFU..........2200
C.....CHECK NON-INSERTED, NON-OBSERVATION FILES                          NAFU..........2300
  200 DO 300 NFF=0,6                                                     NAFU..........2400
         IF (IUNEXT.EQ.IUNIT(NFF)) THEN                                  NAFU..........2500
            IUNEXT = IUNEXT + 1                                          NAFU..........2600
            GOTO 100                                                     NAFU..........2700
         END IF                                                          NAFU..........2800
  300 CONTINUE                                                           NAFU..........2900
C.....CHECK OBSERVATION FILES                                            NAFU..........3000
  400 DO 500 NJ=1,NJMAX                                                  NAFU..........3100
         IF (IUNEXT.EQ.IUNIO(NJ)) THEN                                   NAFU..........3200
            IUNEXT = IUNEXT + 1                                          NAFU..........3300
            GOTO 100                                                     NAFU..........3400
         END IF                                                          NAFU..........3500
  500 CONTINUE                                                           NAFU..........3600
C.....CHECK INSERTED FILES                                               NAFU..........3700
      IF ((IUNEXT.EQ.K1).OR.(IUNEXT.EQ.K2)) THEN                         NAFU..........3800
         IUNEXT = IUNEXT + 1                                             NAFU..........3900
         GOTO 100                                                        NAFU..........4000
      END IF                                                             NAFU..........4100
      DO 600 I=1,2                                                       NAFU..........4200
      DO 600 K=1,NKS(I)                                                  NAFU..........4300
         IF (IUNEXT.EQ.KLIST(I,K)) THEN                                  NAFU..........4400
            IUNEXT = IUNEXT + 1                                          NAFU..........4500
            GOTO 100                                                     NAFU..........4600
         END IF                                                          NAFU..........4700
  600 CONTINUE                                                           NAFU..........4800
C.....IF THE UNIT NUMBER SELECTED IS NOT VALID, GENERATE ERROR           NAFU..........4900
      INQUIRE(UNIT=IUNEXT, EXIST=EXST)                                   NAFU..........5000
      IF (.NOT.EXST) THEN                                                NAFU..........5100
         ERRCOD = 'FIL-10'                                               NAFU..........5200
         INERR(1) = IUNEXT                                               NAFU..........5300
         CHERR(1) = UNAME                                                NAFU..........5400
         CHERR(2) = FN                                                   NAFU..........5500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        NAFU..........5600
	   RETURN
      END IF                                                             NAFU..........5700
C                                                                        NAFU..........5800
      RETURN                                                             NAFU..........5900
      END                                                                NAFU..........6000
C                                                                        NAFU..........6100
C     SUBROUTINE        N  O  D  A  L              SUTRA VERSION 2.1     NODAL..........100
C                                                                        NODAL..........200
C *** PURPOSE :                                                          NODAL..........300
C ***  (1) TO CARRY OUT ALL CELLWISE CALCULATIONS AND TO ADD CELLWISE    NODAL..........400
C ***      TERMS TO THE GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW    NODAL..........500
C ***      AND TRANSPORT EQUATIONS.                                      NODAL..........600
C ***  (2) TO ADD FLUID SOURCE AND SOLUTE MASS OR ENERGY SOURCE TERMS    NODAL..........700
C ***      TO THE MATRIX EQUATIONS.                                      NODAL..........800
C                                                                        NODAL..........900
C     SUBROUTINE        O  U  T  E  L  E           SUTRA VERSION 2.1     OUTELE.........100
C                                                                        OUTELE.........200
C *** PURPOSE :                                                          OUTELE.........300
C ***  TO PRINT ELEMENT CENTROID COORDINATES AND VELOCITY COMPONENTS     OUTELE.........400
C ***  IN A FLEXIBLE, COLUMNWISE FORMAT.  OUTPUT IS TO THE ELE FILE.     OUTELE.........500
C     SUBROUTINE        O  U  T  L  S  T  2        SUTRA VERSION 2.1     OUTLST2........100
C                                                                        OUTLST2........200
C *** PURPOSE :                                                          OUTLST2........300
C ***  TO PRINT PRESSURE AND TEMPERATURE OR CONCENTRATION                OUTLST2........400
C ***  SOLUTIONS AND TO OUTPUT INFORMATION ON TIME STEP, ITERATIONS,     OUTLST2........500
C ***  SATURATIONS, AND FLUID VELOCITIES FOR 2D PROBLEMS.                OUTLST2........600
C ***  OUTPUT IS TO THE LST FILE.                                        OUTLST2........700
C     SUBROUTINE        O  U  T  L  S  T  3        SUTRA VERSION 2.1     OUTLST3........100
C                                                                        OUTLST3........200
C *** PURPOSE :                                                          OUTLST3........300
C ***  TO PRINT PRESSURE AND TEMPERATURE OR CONCENTRATION                OUTLST3........400
C ***  SOLUTIONS AND TO OUTPUT INFORMATION ON TIME STEP, ITERATIONS,     OUTLST3........500
C ***  SATURATIONS, AND FLUID VELOCITIES FOR 3D PROBLEMS.                OUTLST3........600
C ***  OUTPUT IS TO THE LST FILE.                                        OUTLST3........700
C     SUBROUTINE        O  U  T  N  O  D           SUTRA VERSION 2.1     OUTNOD.........100
C                                                                        OUTNOD.........200
C *** PURPOSE :                                                          OUTNOD.........300
C ***  TO PRINT NODE COORDINATES, PRESSURES, CONCENTRATIONS OR           OUTNOD.........400
C ***  TEMPERATURES, AND SATURATIONS IN A FLEXIBLE, COLUMNWISE FORMAT.   OUTNOD.........500
C ***  OUTPUT IS TO THE NOD FILE.                                        OUTNOD.........600
C                                                                        OUTNOD.........700
C     SUBROUTINE        O  U  T  O  B  C           SUTRA VERSION 2.1     OUTOBC.........100
C                                                                        OUTOBC.........200
C *** PURPOSE :                                                          OUTOBC.........300
C ***  TO PRINT THE SOLUTION AT OBSERVATION POINTS.  SPECIFICALLY,       OUTOBC.........400
C ***  TO PRINT PRESSURES, CONCENTRATIONS OR TEMPERATURES, AND           OUTOBC.........500
C ***  SATURATIONS IN A COLUMNWISE FORMAT SIMILAR TO THAT USED IN THE    OUTOBC.........600
C ***  NODEWISE AND ELEMENTWISE OUTPUT FILES.                            OUTOBC.........700
C                                                                        OUTOBC.........800
C     SUBROUTINE        O  U  T  O  B  S           SUTRA VERSION 2.1     OUTOBS.........100
C                                                                        OUTOBS.........200
C *** PURPOSE :                                                          OUTOBS.........300
C ***  TO PRINT THE SOLUTION AT OBSERVATION POINTS.  SPECIFICALLY,       OUTOBS.........400
C ***  TO PRINT PRESSURES, CONCENTRATIONS OR TEMPERATURES, AND           OUTOBS.........500
C ***  SATURATIONS IN A COLUMNWISE FORMAT.                               OUTOBS.........600
C     SUBROUTINE        O  U  T  R  S  T           SUTRA VERSION 2.1     OUTRST.........100
C                                                                        OUTRST.........200
C *** PURPOSE :                                                          OUTRST.........300
C ***  TO STORE RESULTS THAT MAY LATER BE USED TO RESTART                OUTRST.........400
C ***  THE SIMULATION.                                                   OUTRST.........500
C                                                                        OUTRST.........600
C     SUBROUTINE        P  R  S  W  D  S           SUTRA VERSION 2.1     PRSWDS.........100
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
C     SUBROUTINE        P  T  R  S  E  T           SUTRA VERSION 2.1     PTRSET.........100
C                                                                        PTRSET.........200
C *** PURPOSE :                                                          PTRSET.........300
C ***  TO SET UP POINTER ARRAYS NEEDED TO SPECIFY THE MATRIX STRUCTURE.  PTRSET.........400
C                                                                        PTRSET.........500
      SUBROUTINE PTRSET()                                                PTRSET.........600
      USE ALLARR                                                         PTRSET.........700
      USE PTRDEF                                                         PTRSET.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                PTRSET.........900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              PTRSET........1000
     1   NSOP,NSOU,NBCN                                                  PTRSET........1100
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
                  DEALLOCATE(DENTPI)                                     PTRSET........4900
                  GOTO 500                                               PTRSET........5000
               ELSE IF (JC.LT.DENT%NODNUM) THEN                          PTRSET........5100
                  ALLOCATE(DENTNW)                                       PTRSET........5200
                  DENTNW%NODNUM = JC                                     PTRSET........5300
                  DENTNW%NENT => DENT                                    PTRSET........5400
                  IF (K.EQ.1) THEN                                       PTRSET........5500
                     HLIST(IC)%PL => DENTNW                              PTRSET........5600
                  ELSE                                                   PTRSET........5700
                     DENTPV%NENT => DENTNW                               PTRSET........5800
                  END IF                                                 PTRSET........5900
                  DEALLOCATE(DENTPI)                                     PTRSET........6000
                  GOTO 498                                               PTRSET........6100
               END IF                                                    PTRSET........6200
               DENTPV => DENT                                            PTRSET........6300
  495       CONTINUE                                                     PTRSET........6400
C...........APPEND TO TAIL.                                              PTRSET........6500
            ALLOCATE(DENTNW)                                             PTRSET........6600
            DENTNW%NODNUM = JC                                           PTRSET........6700
            DENT%NENT => DENTNW                                          PTRSET........6800
            DEALLOCATE(DENTPI)                                           PTRSET........6900
         END IF                                                          PTRSET........7000
  498    LLIST(IC) = LLIST(IC) + 1                                       PTRSET........7100
  500 CONTINUE                                                           PTRSET........7200
C.....COMPUTE THE ARRAY DIMENSION NELT AND ALLOCATE ARRAY IA.            PTRSET........7300
      NELT = 0                                                           PTRSET........7400
      DO 600 I=1,NN                                                      PTRSET........7500
  600    NELT = NELT + LLIST(I) + 1                                      PTRSET........7600
      NDIMIA = NELT                                                      PTRSET........7700
      ALLOCATE(IA(NDIMIA))                                               PTRSET........7800
C.....TRANSFER THE LINKED LISTS TO ARRAYS IA AND JA IN SLAP COLUMN       PTRSET........7900
C        FORMAT.  DEALLOCATE POINTERS AS THEY ARE TRANSFERRED.           PTRSET........8000
      JASTRT = 1                                                         PTRSET........8100
      DO 660 I=1,NN                                                      PTRSET........8200
         JA(I) = JASTRT                                                  PTRSET........8300
         IA(JASTRT) = I                                                  PTRSET........8400
         DENT => HLIST(I)%PL                                             PTRSET........8500
         DO 650 K=1,LLIST(I)                                             PTRSET........8600
            IA(JASTRT + K) = DENT%NODNUM                                 PTRSET........8700
            DENTPV => DENT                                               PTRSET........8800
            DENT => DENT%NENT                                            PTRSET........8900
            DEALLOCATE(DENTPV)                                           PTRSET........9000
  650    CONTINUE                                                        PTRSET........9100
         JASTRT = JASTRT + LLIST(I) + 1                                  PTRSET........9200
  660 CONTINUE                                                           PTRSET........9300
      JA(NN + 1) = NELT + 1                                              PTRSET........9400
      DEALLOCATE(HLIST, LLIST)                                           PTRSET........9500
C                                                                        PTRSET........9600
      RETURN                                                             PTRSET........9700
      END                                                                PTRSET........9800
C                                                                        PTRSET........9900
C                                                                        PTRSET.......10000
C     SUBROUTINE        P  U                       SUTRA VERSION 2.1     PU.............100
C                                                                        PU.............200
C *** PURPOSE :                                                          PU.............300
C ***  TO EVALUATE P AND U AT SPECIFIED LOCAL COORDINATES WITHIN A       PU.............400
C ***  2D OR 3D ELEMENT.  ADAPTED FROM SUBROUTINES BASIS2 AND BASIS3.    PU.............500
C                                                                        PU.............600
C     FUNCTION          P  U  S  W  F              SUTRA VERSION 2.1     PUSWF..........100
C                                                                        PUSWF..........200
C *** PURPOSE :                                                          PUSWF..........300
C ***  TO INTERPOLATE P, U, AND SW AT A FRACTIONAL TIME STEP (BETWEEN    PUSWF..........400
C ***  THE CURRENT AND PREVIOUS TIME STEPS) AND RETURN THE VALUES IN     PUSWF..........500
C ***  AN ARRAY.                                                         PUSWF..........600
C                                                                        PUSWF..........700
C     SUBROUTINE        R  E  A  D  I  F           SUTRA VERSION 2.1     READIF.........100
C                                                                        READIF.........200
C *** PURPOSE :                                                          READIF.........300
C ***  TO READ A LINE FROM AN INPUT FILE INTO THE CHARACTER VARIABLE     READIF.........400
C ***  INTFIL.  HANDLE OPENING AND CLOSING OF INSERTED FILES AS          READIF.........500
C ***  NECESSARY.                                                        READIF.........600
C                                                                        READIF.........700
      SUBROUTINE READIF(KUU, INTFIL, ERRCOD, IERROR)                             READIF.........800
      USE SCHDEF, ONLY : IUNIO, FNAMO                                    READIF.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                READIF........1000
      PARAMETER (KINMIN=10)                                              READIF........1100
      CHARACTER INTFIL*1000                                              READIF........1200
      CHARACTER*80 ERRCOD,CHERR(10)                                      READIF........1300
      CHARACTER*80 UNAME,FNAME,FNAIN                                     READIF........1400
      CHARACTER ERRF*3, FINS*80                                          READIF........1500
      LOGICAL IS                                                         READIF........1600
      DIMENSION INERR(10),RLERR(10)                                      READIF........1700
      DIMENSION NKS(2), KLIST(2,20), IUNIT(0:8)                          READIF........1800
      DIMENSION FNAME(0:8), FNAIN(2,20)                                  READIF........1900
      COMMON /FNAINS/ FNAIN                                              READIF........2000
      COMMON /FNAMES/ UNAME,FNAME                                        READIF........2100
      COMMON /FUNINS/ NKS,KLIST                                          READIF........2200
      COMMON /FUNITA/ IUNIT                                              READIF........2300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     READIF........2400
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      READIF........2500
C                                                                        READIF........2600
C.....COPY KUU INTO KU. SHOULD AVOID CHANGING KUU, SINCE IT IS ALREADY   READIF........2700
C        LINKED TO K1 OR K2 THROUGH THE ARGUMENT LIST, AND THE LATTER    READIF........2800
C        ARE ALSO PASSED IN THROUGH COMMON BLOCK FUNITS.                 READIF........2900
      KU = KUU                                                           READIF........3000
C                                                                        READIF........3100
C.....READ A LINE OF INPUT (UP TO 1000 CHARACTERS) FROM UNIT KU          READIF........3200
C        INTO INTFIL                                                     READIF........3300
100   READ(KU,'(A)',IOSTAT=INERR(1)) INTFIL                              READIF........3400
C.....IF THE END OF AN INSERTED FILE IS REACHED, CLOSE THAT FILE AND     READIF........3500
C        CONTINUE READING FROM THE NEXT-LEVEL-UP FILE                    READIF........3600
      IF (INERR(1).LT.0) THEN                                            READIF........3700
C........SET FLAG IK TO INDICATE WHETHER THE READ WAS ATTEMPTED FROM     READIF........3800
C           AN INP DATASET (IK=1) OR AN ICS DATASET (IK=2).              READIF........3900
         IF (KU.EQ.K1) THEN                                              READIF........4000
            IK = 1                                                       READIF........4100
         ELSE                                                            READIF........4200
            IK = 2                                                       READIF........4300
         END IF                                                          READIF........4400
C........IF READING FROM AN INSERTED FILE, CLOSE THAT FILE, UPDATE       READIF........4500
C           UNIT NUMBERS, FILENAME, AND COUNTER TO INDICATE THE          READIF........4600
C           NEXT-LEVEL-UP FILE, AND CONTINUE READING                     READIF........4700
         IF (NKS(IK).GT.0) THEN                                          READIF........4800
            CLOSE(KU)                                                    READIF........4900
            IF (KU.EQ.K1) THEN                                           READIF........5000
               K1 = KLIST(IK, NKS(IK))                                   READIF........5100
            ELSE                                                         READIF........5200
               K2 = KLIST(IK, NKS(IK))                                   READIF........5300
            END IF                                                       READIF........5400
            KU = KLIST(IK, NKS(IK))                                      READIF........5500
            FNAME(IK) = FNAIN(IK, NKS(IK))                               READIF........5600
            NKS(IK) = NKS(IK) - 1                                        READIF........5700
            GOTO 100                                                     READIF........5800
         END IF                                                          READIF........5900
C.....ELSE IF THE READ RESULTS IN A DIFFERENT KIND OF ERROR, GENERATE    READIF........6000
C        ERROR MESSAGE                                                   READIF........6100
      ELSE IF (INERR(1).GT.0) THEN                                       READIF........6200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        READIF........6300
	   RETURN
      END IF                                                             READIF........6400
C                                                                        READIF........6500
C.....IF BLANK OR COMMENT LINE, SKIP IT.                                 READIF........6600
      IF ((INTFIL(1:1).EQ.'#').OR.(INTFIL.EQ.'')) GOTO 100               READIF........6700
C                                                                        READIF........6800
C.....IF INSERT STATEMENT, OPEN THE FILE AND CONTINUE READING            READIF........6900
      IF (INTFIL(1:7).EQ.'@INSERT') THEN                                 READIF........7000
C........SET FLAG IK TO INDICATE WHETHER THE READ WAS DONE FROM          READIF........7100
C           AN INP DATASET (IK=1) OR AN ICS DATASET (IK=2).              READIF........7200
C           SET ERRF TO THE FILE TYPE ('INP' OR 'ICS').                  READIF........7300
         IF (KU.EQ.K1) THEN                                              READIF........7400
            IK = 1                                                       READIF........7500
            ERRF = 'INP'                                                 READIF........7600
         ELSE                                                            READIF........7700
            IK = 2                                                       READIF........7800
            ERRF = 'ICS'                                                 READIF........7900
         END IF                                                          READIF........8000
C........READ THE FILE SPECIFICATION FOR THE INSERTED FILE               READIF........8100
         READ(INTFIL(8:),*,IOSTAT=INERR(1)) KINS, FINS                   READIF........8200
         IF (INERR(1).NE.0) THEN                                         READIF........8300
            CHERR(1) = ERRCOD                                            READIF........8400
            ERRCOD = 'REA-' // ERRF // '-INS'                            READIF........8500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     READIF........8600
	      RETURN
         END IF                                                          READIF........8700
C........CHECK FOR DUPLICATE FILENAME AMONG INSERTED FILES               READIF........8800
         DO 550 I=1,2                                                    READIF........8900
         DO 550 K=1,NKS(I)                                               READIF........9000
            IF (FINS.EQ.FNAIN(I, K)) THEN                                READIF........9100
               ERRCOD = 'FIL-4'                                          READIF........9200
               INERR(1) = KINS                                           READIF........9300
               CHERR(1) = FNAME(IK)                                      READIF........9400
               CHERR(2) = FINS                                           READIF........9500
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF........9600
	         RETURN
            END IF                                                       READIF........9700
  550    CONTINUE                                                        READIF........9800
C........CHECK FOR DUPLICATE FILENAME AMONG NON-INSERTED,                READIF........9900
C           NON-OBSERVATION FILES                                        READIF.......10000
         DO 560 NFF=0,6                                                  READIF.......10100
            IF (FINS.EQ.FNAME(NFF)) THEN                                 READIF.......10200
               ERRCOD = 'FIL-4'                                          READIF.......10300
               INERR(1) = KINS                                           READIF.......10400
               CHERR(1) = FNAME(IK)                                      READIF.......10500
               CHERR(2) = FINS                                           READIF.......10600
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF.......10700
	         RETURN
            END IF                                                       READIF.......10800
  560    CONTINUE                                                        READIF.......10900
C........CHECK FOR DUPLICATE FILENAME AMONG OBSERVATION FILES            READIF.......11000
         DO 570 NJ=1,NFLOMX                                              READIF.......11100
            IF (FINS.EQ.FNAMO(NJ)) THEN                                  READIF.......11200
               ERRCOD = 'FIL-4'                                          READIF.......11300
               INERR(1) = KINS                                           READIF.......11400
               CHERR(1) = FNAME(IK)                                      READIF.......11500
               CHERR(2) = FINS                                           READIF.......11600
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF.......11700
	         RETURN
            END IF                                                       READIF.......11800
  570    CONTINUE                                                        READIF.......11900
C........IF THE SPECIFIED UNIT NUMBER IS LESS THAN KINMIN,               READIF.......12000
C           SET IT TO KINMIN                                             READIF.......12100
         KINS = MAX(KINS, KINMIN)                                        READIF.......12200
C........IF THE FILE TO BE INSERTED EXISTS, ASSIGN IT A UNIT NUMBER      READIF.......12300
C           AND OPEN IT                                                  READIF.......12400
         INQUIRE(FILE=FINS,EXIST=IS)                                     READIF.......12500
         IF (IS) THEN                                                    READIF.......12600
            CALL NAFU(KINS,NFLOMX,FINS,IERROR)                                  READIF.......12700
	      IF (IERROR.NE.0) RETURN
            OPEN(UNIT=KINS,FILE=FINS,STATUS='OLD',FORM='FORMATTED',      READIF.......12800
     1         IOSTAT=KERR)                                              READIF.......12900
            IF (KERR.GT.0) THEN                                          READIF.......13000
               CHERR(1) = FNAME(IK)                                      READIF.......13100
               CHERR(2) = FINS                                           READIF.......13200
               INERR(1) = KINS                                           READIF.......13300
               ERRCOD = 'FIL-2'                                          READIF.......13400
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                  READIF.......13500
	         RETURN
            END IF                                                       READIF.......13600
         ELSE                                                            READIF.......13700
            CHERR(1) = FNAME(IK)                                         READIF.......13800
            CHERR(2) = FINS                                              READIF.......13900
            ERRCOD = 'FIL-1'                                             READIF.......14000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     READIF.......14100
	      RETURN
         END IF                                                          READIF.......14200
C........UPDATE THE INSERTION COUNTER.  IF THE COUNT EXCEEDS 20,         READIF.......14300
C           GENERATE AN ERROR                                            READIF.......14400
         NKS(IK) = NKS(IK) + 1                                           READIF.......14500
         IF (NKS(IK).GT.20) THEN                                         READIF.......14600
            CHERR(1) = FNAME(IK)                                         READIF.......14700
            CHERR(2) = FINS                                              READIF.......14800
            ERRCOD = 'FIL-8'                                             READIF.......14900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                     READIF.......15000
	      RETURN
         END IF                                                          READIF.......15100
C........UPDATE UNIT NUMBERS AND FILENAMES TO INDICATE THE NEWLY         READIF.......15200
C           INSERTED FILE, AND CONTINUE READING                          READIF.......15300
         IF (KU.EQ.K1) THEN                                              READIF.......15400
            K1 = KINS                                                    READIF.......15500
         ELSE                                                            READIF.......15600
            K2 = KINS                                                    READIF.......15700
         END IF                                                          READIF.......15800
         KLIST(IK, NKS(IK)) = KU                                         READIF.......15900
         FNAIN(IK, NKS(IK)) = FNAME(IK)                                  READIF.......16000
         KU = KINS                                                       READIF.......16100
         FNAME(IK) = FINS                                                READIF.......16200
         GOTO 100                                                        READIF.......16300
      END IF                                                             READIF.......16400
C                                                                        READIF.......16500
      RETURN                                                             READIF.......16600
      END                                                                READIF.......16700
C                                                                        READIF.......16800
C     SUBROUTINE        R  O  T  A  T  E           SUTRA VERSION 2.1     ROTATE.........100
C                                                                        ROTATE.........200
C *** PURPOSE :                                                          ROTATE.........300
C ***  TO TRANSFORM THE COORDINATES OF A VECTOR, {x}, BY APPLYING THE    ROTATE.........400
C ***  ROTATION MATRIX, [G]:  {xp}=[G]{x}.                               ROTATE.........500
C                                                                        ROTATE........1800
C     SUBROUTINE        R  O  T  M  A  T           SUTRA VERSION 2.1     ROTMAT.........100
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
C     SUBROUTINE        S  O  L  V  E  B           SUTRA VERSION 2.1     SOLVEB.........100
C                                                                        SOLVEB.........200
C *** PURPOSE :                                                          SOLVEB.........300
C ***  TO SOLVE THE MATRIX EQUATION BY:                                  SOLVEB.........400
C ***   (1) DECOMPOSING THE MATRIX                                       SOLVEB.........500
C ***   (2) MODIFYING THE RIGHT-HAND SIDE                                SOLVEB.........600
C ***   (3) BACK-SUBSTITUTING FOR THE SOLUTION                           SOLVEB.........700
C                                                                        SOLVEB.........800
C     SUBROUTINE        S  O  L  V  E  R           SUTRA VERSION 2.1     SOLVER.........100
C                                                                        SOLVER.........200
C *** PURPOSE :                                                          SOLVER.........300
C ***  TO CALL THE APPROPRIATE MATRIX EQUATION SOLVER.                   SOLVER.........400
C                                                                        SOLVER.........500

C     SUBROUTINE        S  O  L  W  R  P           SUTRA VERSION 2.1     SOLWRP.........100
C                                                                        SOLWRP.........200
C *** PURPOSE :                                                          SOLWRP.........300
C ***  TO SERVE AS A WRAPPER FOR THE ITERATIVE SOLVERS, PERFORMING       SOLWRP.........400
C ***  SOME PRELIMINARIES ON VECTORS BEFORE CALLING A SOLVER.            SOLWRP.........500
C                                                                        SOLWRP.........600
C                                                                        SOLWRP........9600
C     SUBROUTINE        S  O  U  R  C  E           SUTRA VERSION 2.1     SOURCE.........100
C                                                                        SOURCE.........200
C *** PURPOSE :                                                          SOURCE.........300
C ***  TO READ AND ORGANIZE FLUID MASS SOURCE DATA AND ENERGY OR         SOURCE.........400
C ***  SOLUTE MASS SOURCE DATA.                                          SOURCE.........500
C                                                                        SOURCE.........600
      SUBROUTINE SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT, 
     1   IERROR, IBOUSZ, IBNODE, IPOS)                                   SOURCE.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOURCE.........800
      CHARACTER INTFIL*1000                                              SOURCE.........900
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     SOURCE........1000
      DIMENSION KTYPE(2)                                                 SOURCE........1100
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SOURCE........1200
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SOURCE........1300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SOURCE........1400
     1   NSOP,NSOU,NBCN                                                  SOURCE........1500
      COMMON /FNAMES/ UNAME,FNAME                                        SOURCE........1600
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SOURCE........1700
      DIMENSION QIN(NN),UIN(NN),IQSOP(NSOP),QUIN(NN),IQSOU(NSOU)         SOURCE........1800
      DIMENSION INERR(10),RLERR(10)                                      SOURCE........1900
C RBW
      INTEGER IERROR
	INTEGER IBOUSZ, IPOS
	INTEGER IBNODE(IBOUSZ)
C RBW
C                                                                        SOURCE........2000
C.....NSOPI IS ACTUAL NUMBER OF FLUID SOURCE NODES.                      SOURCE........2100
C.....NSOUI IS ACTUAL NUMBER OF SOLUTE MASS OR ENERGY SOURCE NODES.      SOURCE........2200
      NSOPI=NSOP-1                                                       SOURCE........2300
      NSOUI=NSOU-1                                                       SOURCE........2400
      IQSOPT=1                                                           SOURCE........2500
      IQSOUT=1                                                           SOURCE........2600
      NIQP=0                                                             SOURCE........2700
      NIQU=0                                                             SOURCE........2800
      IF(NSOPI.EQ.0) then
	  ipos = 1
	  ibnode(ipos) = 0
        GOTO 1000          
      end if                                               
      IF(NSOPI.EQ.0) GOTO 1000                                           SOURCE........2900
!      IF(ME) 50,50,150                                                   SOURCE........3000
!   50 WRITE(K3,100)                                                      SOURCE........3100
!  100 FORMAT('1'////11X,'F L U I D   S O U R C E   D A T A'              SOURCE........3200
!     1   ////11X,'**** NODES AT WHICH FLUID INFLOWS OR OUTFLOWS ARE ',   SOURCE........3300
!     2   'SPECIFIED ****'//11X,'NODE NUMBER',10X,                        SOURCE........3400
!     3   'FLUID INFLOW(+)/OUTFLOW(-)',5X,'SOLUTE CONCENTRATION OF'       SOURCE........3500
!     4   /11X,'(MINUS INDICATES',5X,'(FLUID MASS/SECOND)',               SOURCE........3600
!     5   12X,'INFLOWING FLUID'/12X,'TIME-VARYING',39X,                   SOURCE........3700
!     6   '(MASS SOLUTE/MASS WATER)'/12X,'FLOW RATE OR'/12X,              SOURCE........3800
!     7   'CONCENTRATION)'//)                                             SOURCE........3900
!      GOTO 300                                                           SOURCE........4000
!  150 WRITE(K3,200)                                                      SOURCE........4100
!  200 FORMAT('1'////11X,'F L U I D   S O U R C E   D A T A'              SOURCE........4200
!     1   ////11X,'**** NODES AT WHICH FLUID INFLOWS OR OUTFLOWS ARE ',   SOURCE........4300
!     2   'SPECIFIED ****'//11X,'NODE NUMBER',10X,                        SOURCE........4400
!     3   'FLUID INFLOW(+)/OUTFLOW(-)',5X,'TEMPERATURE {DEGREES CELSIUS}' SOURCE........4500
!     4   /11X,'(MINUS INDICATES',5X,'(FLUID MASS/SECOND)',12X,           SOURCE........4600
!     5   'OF INFLOWING FLUID'/12X,'TIME-VARYING'/12X,'FLOW OR'/12X,      SOURCE........4700
!     6   'TEMPERATURE)'//)                                               SOURCE........4800
C                                                                        SOURCE........4900
C.....INPUT DATASET 17:  DATA FOR FLUID SOURCES AND SINKS                SOURCE........5000
  300 CONTINUE                                                           SOURCE........5100
      IPOS = 1 
      IBNODE(IPOS) = NSOPI
  305 NIQP=NIQP+1                                                        SOURCE........5200
      ERRCOD = 'REA-INP-17'                                              SOURCE........5300
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    SOURCE........5400
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) IQCP                                SOURCE........5500
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 SOURCE........5600
	   RETURN
	ENDIF
C RBW
      IQCPA = IABS(IQCP)                                                 SOURCE........5700
	IF (IQCPA.GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IQCPA - 1  
	ENDIF
C RBW
      IF (IQCP.EQ.0) THEN                                                SOURCE........5800
         GOTO 700                                                        SOURCE........5900
      ELSE IF (IQCPA.GT.NN) THEN                                         SOURCE........6000
         ERRCOD = 'INP-17-1'                                             SOURCE........6100
         INERR(1) = IQCPA                                                SOURCE........6200
         INERR(2) = NN                                                   SOURCE........6300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        SOURCE........6400
	   RETURN
      ELSE IF (NIQP.GT.NSOPI) THEN                                       SOURCE........6500
         GOTO 305                                                        SOURCE........6600
      END IF                                                             SOURCE........6700
      ERRCOD = 'REA-INP-17'                                              SOURCE........6800
      IF (IQCP.GT.0) THEN                                                SOURCE........6900
         READ(INTFIL,*,IOSTAT=INERR(1)) IQCP,QINC                        SOURCE........7000
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              SOURCE........7100
	      RETURN
	   ENDIF
         IF (QINC.GT.0D0) THEN                                           SOURCE........7200
            READ(INTFIL,*,IOSTAT=INERR(1)) IQCP,QINC,UINC                SOURCE........7300
            IF (INERR(1).NE.0) THEN
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)           SOURCE........7400
	         RETURN
	      ENDIF
         END IF                                                          SOURCE........7500
      END IF                                                             SOURCE........7600
      IQSOP(NIQP)=IQCP                                                   SOURCE........7700
      IF(IQCP.LT.0) IQSOPT=-1                                            SOURCE........7800
      IQP=IABS(IQCP)                                                     SOURCE........7900
      QIN(IQP)=QINC                                                      SOURCE........8000
      UIN(IQP)=UINC                                                      SOURCE........8100
      IF(IQCP.GT.0) GOTO 450                                             SOURCE........8200
!      WRITE(K3,500) IQCP                                                 SOURCE........8300
      GOTO 600                                                           SOURCE........8400
  450 IF(QINC.GT.0) GOTO 460                                             SOURCE........8500
!      WRITE(K3,500) IQCP,QINC                                            SOURCE........8600
      GOTO 600                                                           SOURCE........8700
  460 CONTINUE
!  460 WRITE(K3,500) IQCP,QINC,UINC                                       SOURCE........8800
!  500 FORMAT(11X,I10,13X,1PE14.7,16X,1PE14.7)                            SOURCE........8900
  600 GOTO 305                                                           SOURCE........9000
  700 NIQP = NIQP - 1                                                    SOURCE........9100
      IF(NIQP.EQ.NSOPI) GOTO 890                                         SOURCE........9200
         ERRCOD = 'INP-3,17-1'                                           SOURCE........9300
         INERR(1) = NIQP                                                 SOURCE........9400
         INERR(2) = NSOPI                                                SOURCE........9500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        SOURCE........9600
	   RETURN
  890 CONTINUE
C RBW
      IF(NSOUI.EQ.0) THEN
	   IPOS = IPOS + 1 
         IBNODE(IPOS) = 0
	END IF
C RBW
!  890 IF(IQSOPT.EQ.-1) WRITE(K3,900)                                     SOURCE........9700
!  900 FORMAT(////11X,'THE SPECIFIED TIME VARIATIONS ARE ',               SOURCE........9800
!     1   'USER-PROGRAMMED IN SUBROUTINE  B C T I M E .')                 SOURCE........9900
C                                                                        SOURCE.......10000
C                                                                        SOURCE.......10100
 1000 IF(NSOUI.EQ.0) GOTO 9000                                           SOURCE.......10200
!      IF(ME) 1050,1050,1150                                              SOURCE.......10300
! 1050 WRITE(K3,1100)                                                     SOURCE.......10400
! 1100 FORMAT(////////11X,'S O L U T E   S O U R C E   D A T A'           SOURCE.......10500
!     1   ////11X,'**** NODES AT WHICH SOURCES OR SINKS OF SOLUTE ',      SOURCE.......10600
!     2   'MASS ARE SPECIFIED ****'//11X,'NODE NUMBER',10X,               SOURCE.......10700
!     3   'SOLUTE SOURCE(+)/SINK(-)'/11X,'(MINUS INDICATES',5X,           SOURCE.......10800
!     4   '(SOLUTE MASS/SECOND)'/12X,'TIME-VARYING'/12X,                  SOURCE.......10900
!     5   'SOURCE OR SINK)'//)                                            SOURCE.......11000
!      GOTO 1305                                                          SOURCE.......11100
! 1150 WRITE(K3,1200)                                                     SOURCE.......11200
! 1200 FORMAT(////////11X,'E N E R G Y   S O U R C E   D A T A'           SOURCE.......11300
!     1   ////11X,'**** NODES AT WHICH SOURCES OR SINKS OF ',             SOURCE.......11400
!     2   'ENERGY ARE SPECIFIED ****'//11X,'NODE NUMBER',10X,             SOURCE.......11500
!     3   'ENERGY SOURCE(+)/SINK(-)'/11X,'(MINUS INDICATES',5X,           SOURCE.......11600
!     4   '(ENERGY/SECOND)'/12X,'TIME-VARYING'/12X,                       SOURCE.......11700
!     5   'SOURCE OR SINK)'//)                                            SOURCE.......11800
C                                                                        SOURCE.......11900
C.....INPUT DATASET 18:  DATA FOR ENERGY OR SOLUTE MASS SOURCES OR SINKS SOURCE.......12000
      IPOS = IPOS + 1 
      IBNODE(IPOS) = NSOUI
 1305 NIQU=NIQU+1                                                        SOURCE.......12100
      ERRCOD = 'REA-INP-18'                                              SOURCE.......12200
      CALL READIF(K1, INTFIL, ERRCOD,IERROR)                                    SOURCE.......12300
	IF (IERROR.NE.0) RETURN
      READ(INTFIL,*,IOSTAT=INERR(1)) IQCU                                SOURCE.......12400
      IF (INERR(1).NE.0) THEN
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                 SOURCE.......12500
	   RETURN
      END IF                                                             SOURCE.......13600
      IQCUA = IABS(IQCU)                                                 SOURCE.......12600
C RBW
	IF (IQCUA.GT.0) THEN
        IPOS = IPOS + 1
        IBNODE(IPOS) = IQCUA - 1   
	ENDIF
C RBW
      IF (IQCU.EQ.0) THEN                                                SOURCE.......12700
         GOTO 1700                                                       SOURCE.......12800
      ELSE IF (IQCUA.GT.NN) THEN                                         SOURCE.......12900
         ERRCOD = 'INP-18-1'                                             SOURCE.......13000
         INERR(1) = IQCUA                                                SOURCE.......13100
         INERR(2) = NN                                                   SOURCE.......13200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        SOURCE.......13300
	   RETURN
      ELSE IF (NIQU.GT.NSOUI) THEN                                       SOURCE.......13400
         GOTO 1305                                                       SOURCE.......13500
      END IF                                                             SOURCE.......13600
      IF (IQCU.GT.0) THEN                                                SOURCE.......13700
         ERRCOD = 'REA-INP-18'                                           SOURCE.......13800
         READ(INTFIL,*,IOSTAT=INERR(1)) IQCU,QUINC                       SOURCE.......13900
         IF (INERR(1).NE.0) THEN
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)              SOURCE.......14000
	      RETURN
	   ENDIF
      END IF                                                             SOURCE.......14100
      IQSOU(NIQU)=IQCU                                                   SOURCE.......14200
      IF(IQCU.LT.0) IQSOUT=-1                                            SOURCE.......14300
      IQU=IABS(IQCU)                                                     SOURCE.......14400
      QUIN(IQU)=QUINC                                                    SOURCE.......14500
      IF(IQCU.GT.0) GOTO 1450                                            SOURCE.......14600
!      WRITE(K3,1500) IQCU                                                SOURCE.......14700
      GOTO 1600                                                          SOURCE.......14800
 1450 CONTINUE
! 1450 WRITE(K3,1500) IQCU,QUINC                                          SOURCE.......14900
! 1500 FORMAT(11X,I10,13X,1PE14.7)                                        SOURCE.......15000
 1600 GOTO 1305                                                          SOURCE.......15100
 1700 NIQU = NIQU - 1                                                    SOURCE.......15200
      IF(NIQU.EQ.NSOUI) GOTO 1890                                        SOURCE.......15300
         ERRCOD = 'INP-3,18-1'                                           SOURCE.......15400
         IF (ME.EQ.1) THEN                                               SOURCE.......15500
            CHERR(1) = 'energy'                                          SOURCE.......15600
         ELSE                                                            SOURCE.......15700
            CHERR(1) = 'solute'                                          SOURCE.......15800
         END IF                                                          SOURCE.......15900
         INERR(1) = NIQU                                                 SOURCE.......16000
         INERR(2) = NSOUI                                                SOURCE.......16100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        SOURCE.......16200
	   RETURN
 1890 CONTINUE
! 1890 IF(IQSOUT.EQ.-1) WRITE(K3,900)                                     SOURCE.......16300
C                                                                        SOURCE.......16400
 9000 RETURN                                                             SOURCE.......16500
C                                                                        SOURCE.......16600
      END                                                                SOURCE.......16700
C                                                                        SOURCE.......16800
C     SUBROUTINE        S  U  T  E  R  R           SUTRA VERSION 2.1     SUTERR.........100
C                                                                        SUTERR.........200
C *** PURPOSE :                                                          SUTERR.........300
C ***  TO HANDLE SUTRA AND FORTRAN ERRORS.                               SUTERR.........400
C                                                                        SUTERR.........500
      SUBROUTINE SUTERR(ERRCOD, CHERR, INERR, RLERR, IERROR)                     SUTERR.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTERR.........700
      CHARACTER*80 ERRCOD,CHERR(10),CODE(3),CODUM(3),UNAME,FNAME(0:8)    SUTERR.........800
      CHARACTER*70 DS(50),EX(50)                                         SUTERR.........900
      CHARACTER CDUM80*80                                                SUTERR........1000
      CHARACTER CINERR(10)*9,CRLERR(10)*15                               SUTERR........1100
      CHARACTER SOLNAM(0:10)*40,SOLWRD(0:10)*10                          SUTERR........1200
      CHARACTER*8 VERNUM, VERNIN                                         SUTERR........1300
      DIMENSION INERR(10), RLERR(10)                                     SUTERR........1400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTERR........1500
     1   NSOP,NSOU,NBCN                                                  SUTERR........1600
      COMMON /FNAMES/ UNAME,FNAME                                        SUTERR........1700
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SUTERR........1800
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTERR........1900
     1   KSCRN,KPAUSE                                                    SUTERR........2000
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       SUTERR........2100
      COMMON /SOLVN/ NSLVRS                                              SUTERR........2200
      COMMON /VER/ VERNUM, VERNIN                                        SUTERR........2300
	IERROR = 1
	RETURN
C                                                                        SUTERR........2400
C.....PARSE THE ERROR CODE                                               SUTERR........2500
      CALL PRSWDS(ERRCOD, '-', 3, CODE, NWORDS)                          SUTERR........2600
C                                                                        SUTERR........2700
C.....IF AN ERROR OTHER THAN A MATRIX SOLVER OR NONLINEAR CONVERGENCE    SUTERR........2800
C        ERROR HAS OCCURRED, OVERRIDE THE SCREEN OUTPUT CONTROLS SO      SUTERR........2900
C        THAT THE ERROR IS PRINTED TO THE SCREEN AND SUTRA PAUSES FOR    SUTERR........3000
C        A USER RESPONSE.                                                SUTERR........3100
      IF ((CODE(1).NE.'SOL').AND.(CODE(1).NE.'CON')) THEN                SUTERR........3200
         KSCRN = +1                                                      SUTERR........3300
         KPAUSE = +1                                                     SUTERR........3400
      END IF                                                             SUTERR........3500
C                                                                        SUTERR........3600
C.....COPY INTEGER AND REAL ERROR PARAMETERS INTO CHARACTER STRINGS      SUTERR........3700
      DO 150 I=1,10                                                      SUTERR........3800
         WRITE(UNIT=CINERR(I), FMT='(I9)') INERR(I)                      SUTERR........3900
         WRITE(UNIT=CRLERR(I), FMT='(1PE15.7)') RLERR(I)                 SUTERR........4000
  150 CONTINUE                                                           SUTERR........4100
C                                                                        SUTERR........4200
C.....INITIALIZE THE ERROR OUTPUT STRINGS                                SUTERR........4300
      DO 200 I=1,50                                                      SUTERR........4400
         DS(I) = "null_line"                                             SUTERR........4500
         EX(I) = "null_line"                                             SUTERR........4600
  200 CONTINUE                                                           SUTERR........4700
C                                                                        SUTERR........4800
C.....SET THE ERROR OUTPUT STRINGS ACCORDING TO THE TYPE OF ERROR        SUTERR........4900
      IF (ERRCOD.EQ.'INP-2A-1') THEN                                     SUTERR........5000
        DS(1)="The first word of SIMULA is not 'SUTRA'."                 SUTERR........5100
        EX(1)="In dataset 2A of the main input file, the first word"     SUTERR........5200
        EX(2)="of the variable SIMULA must be 'SUTRA'."                  SUTERR........5300
        EX(3)=" "                                                        SUTERR........5400
        EX(4)="Example of a valid dataset 2A:"                           SUTERR........5500
        EX(5)="'SUTRA SOLUTE TRANSPORT'"                                 SUTERR........5600
      ELSE IF (ERRCOD.EQ.'INP-2A-2') THEN                                SUTERR........5700
        DS(1)="The second word of SIMULA is not 'SOLUTE' or 'ENERGY'."   SUTERR........5800
        EX(1)="In dataset 2A of the main input file, when the second"    SUTERR........5900
        EX(2)="word is not 'VERSION', the version 2.0 input format is"   SUTERR........6000
        EX(3)="assumed, and the second word must be 'SOLUTE' or"         SUTERR........6100
        EX(4)="'ENERGY'."                                                SUTERR........6200
        EX(5)=" "                                                        SUTERR........6300
        EX(6)="Example of a valid (version 2.0) dataset 2A:"             SUTERR........6400
        EX(7)="'SUTRA SOLUTE TRANSPORT'"                                 SUTERR........6500
      ELSE IF (ERRCOD.EQ.'INP-2A-3') THEN                                SUTERR........6600
        DS(1)="The fourth word of SIMULA is not 'SOLUTE' or 'ENERGY'."   SUTERR........6700
        EX(1)="In dataset 2A of the main input file, the fourth word"    SUTERR........6800
        EX(2)="must be 'SOLUTE' or 'ENERGY' (unless the version 2.0"     SUTERR........6900
        EX(3)="input format is being used)."                             SUTERR........7000
        EX(4)=" "                                                        SUTERR........7100
        EX(5)="Example of a valid (version 2.0) dataset 2A:"             SUTERR........7200
        EX(6)="'SUTRA VERSION " // TRIM(VERNUM) // " SOLUTE TRANSPORT'"  SUTERR........7300
      ELSE IF (ERRCOD.EQ.'INP-2A-4') THEN                                SUTERR........7400
        DS(1)="Unsupported SUTRA version: " // CHERR(1)                  SUTERR........7500
        EX(1)="Input files from SUTRA version " // TRIM(CHERR(1))        SUTERR........7600
        EX(2)="are not supported by version " // TRIM(VERNUM) // "."     SUTERR........7700
      ELSE IF (ERRCOD.EQ.'INP-2B-1') THEN                                SUTERR........7800
        DS(1)="The first word of MSHSTR is not '2D' or '3D'."            SUTERR........7900
        EX(1)="In dataset 2B of the main input file, the first word"     SUTERR........8000
        EX(2)="of the variable MSHSTR must be '2D' or '3D'."             SUTERR........8100
        EX(3)=" "                                                        SUTERR........8200
        EX(4)="Example of a valid dataset 2B:"                           SUTERR........8300
        EX(5)="'3D REGULAR MESH'  10  20  30"                            SUTERR........8400
C.....ERROR CODE 'INP-2B-2' IS NO LONGER USED.                           SUTERR........8500
      ELSE IF (ERRCOD.EQ.'INP-2B-3') THEN                                SUTERR........8600
        DS(1)="At least one of the rectangular dimensions NN1, NN2,"     SUTERR........8700
        DS(2)="and NN3 is set improperly."                               SUTERR........8800
        EX(1)="In dataset 2B of the main input file, the rectangular"    SUTERR........8900
        EX(2)="dimensions NN1, NN2, and (for 3D problems) NN3 must"      SUTERR........9000
        EX(3)="each be greater than 1."                                  SUTERR........9100
        EX(4)=" "                                                        SUTERR........9200
        EX(5)="Example of a valid dataset 2B:"                           SUTERR........9300
        EX(6)="'3D BLOCKWISE MESH'  10  20  30"                          SUTERR........9400
      ELSE IF (ERRCOD.EQ.'INP-2B-4') THEN                                SUTERR........9500
        DS(1)="The second word of MSHSTR is not 'IRREGULAR', 'REGULAR'," SUTERR........9600
        DS(2)="'BLOCKWISE', or 'LAYERED'."                               SUTERR........9700
        EX(1)="In dataset 2B of the main input file, the second word"    SUTERR........9800
        EX(2)="of the variable MSHSTR must be 'IRREGULAR', 'REGULAR',"   SUTERR........9900
        EX(3)="'BLOCKWISE' or 'LAYERED'.  By definition, only 3D meshes" SUTERR.......10000
        EX(4)="can be LAYERED."                                          SUTERR.......10100
        EX(5)=" "                                                        SUTERR.......10200
        EX(6)="Example of a valid dataset 2B:"                           SUTERR.......10300
        EX(7)="'3D REGULAR MESH'  10  20  30"                            SUTERR.......10400
      ELSE IF (ERRCOD.EQ.'INP-2B-5') THEN                                SUTERR.......10500
        DS(1)="A 2D LAYERED mesh has been specified."                    SUTERR.......10600
        EX(1)="By definition, only 3D meshes can be LAYERED."            SUTERR.......10700
        EX(2)=" "                                                        SUTERR.......10800
        EX(3)="Example of a valid dataset 2B:"                           SUTERR.......10900
        EX(4)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......11000
      ELSE IF (ERRCOD.EQ.'INP-2B-6') THEN                                SUTERR.......11100
        DS(1)="The first word of LAYSTR is not 'ACROSS' or 'WITHIN'."    SUTERR.......11200
        EX(1)="In dataset 2B of the main input file, the first word"     SUTERR.......11300
        EX(2)="of the variable LAYSTR must be 'ACROSS' or 'WITHIN'."     SUTERR.......11400
        EX(3)=" "                                                        SUTERR.......11500
        EX(4)="Example of a valid dataset 2B:"                           SUTERR.......11600
        EX(5)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......11700
      ELSE IF (ERRCOD.EQ.'INP-2B-7') THEN                                SUTERR.......11800
        DS(1)="At least one of the layer dimensions NLAYS, NNLAY,"       SUTERR.......11900
        DS(2)="and NELAY is set improperly."                             SUTERR.......12000
        EX(1)="In dataset 2B of the main input file, the layer"          SUTERR.......12100
        EX(2)="dimensions are subject to the following constraints:"     SUTERR.......12200
        EX(3)="NLAYS>1, NNLAY>3, and NELAY>0."                           SUTERR.......12300
        EX(4)=" "                                                        SUTERR.......12400
        EX(5)="Example of a valid dataset 2B:"                           SUTERR.......12500
        EX(6)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......12600
      ELSE IF (ERRCOD.EQ.'INP-2B,3-1') THEN                              SUTERR.......12700
        DS(1)="The number of nodes, NN, does not match the rectangular"  SUTERR.......12800
        DS(2)="dimensions, NN1*NN2(*NN3)."                               SUTERR.......12900
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......13000
        EX(2)="number of nodes, NN, must equal the product of the"       SUTERR.......13100
        EX(3)="rectangular dimensions, NN1*NN2 in 2D, or NN1*NN2*NN3"    SUTERR.......13200
        EX(4)="in 3D."                                                   SUTERR.......13300
        EX(5)=" "                                                        SUTERR.......13400
        EX(6)="Example:"                                                 SUTERR.......13500
        EX(7)="If NN1=10, NN2=20, and NN3=30 (dataset 2B), then"         SUTERR.......13600
        EX(8)="NN=10*20*30=6000 (dataset 3)."                            SUTERR.......13700
      ELSE IF (ERRCOD.EQ.'INP-2B,3-2') THEN                              SUTERR.......13800
        DS(1)="The number of elements, NE, does not match the"           SUTERR.......13900
        DS(2)="rectangular dimensions, (NN1-1)*(NN2-1)[*(NN3-1)]."       SUTERR.......14000
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......14100
        EX(2)="number of elements, NE, must equal the product of the"    SUTERR.......14200
        EX(3)="rectangular dimensions, (NN1-1)*(NN2-1) in 2D, or"        SUTERR.......14300
        EX(4)="(NN1-1)*(NN2-1)*(NN3-1) in 3D."                           SUTERR.......14400
        EX(5)=" "                                                        SUTERR.......14500
        EX(6)="Example:"                                                 SUTERR.......14600
        EX(7)="If NN1=10, NN2=20, and NN3=30 (dataset 2B), then"         SUTERR.......14700
        EX(8)="NE=9*19*29=4959 (dataset 3)."                             SUTERR.......14800
      ELSE IF (ERRCOD.EQ.'INP-2B,3-3') THEN                              SUTERR.......14900
        DS(1)="The number of nodes, NN, does not match the layered"      SUTERR.......15000
        DS(2)="dimensions, NLAYS*NNLAY."                                 SUTERR.......15100
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......15200
        EX(2)="number of nodes, NN, must equal the product of the"       SUTERR.......15300
        EX(3)="layered dimensions, NLAYS*NNLAY."                         SUTERR.......15400
        EX(4)=" "                                                        SUTERR.......15500
        EX(5)="Example:"                                                 SUTERR.......15600
        EX(6)="If NLAYS=10 and NNLAY=2560 (dataset 2B), then"            SUTERR.......15700
        EX(7)="NN=10*2560=25600 (dataset 3)."                            SUTERR.......15800
      ELSE IF (ERRCOD.EQ.'INP-2B,3-4') THEN                              SUTERR.......15900
        DS(1)="The number of nodes, NE, does not match the layered"      SUTERR.......16000
        DS(2)="dimensions, (NLAYS-1)*NELAY."                             SUTERR.......16100
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......16200
        EX(2)="number of nodes, NE, must equal the product of the"       SUTERR.......16300
        EX(3)="layered dimensions, (NLAYS-1)*NELAY."                     SUTERR.......16400
        EX(4)=" "                                                        SUTERR.......16500
        EX(5)="Example:"                                                 SUTERR.......16600
        EX(6)="If NLAYS=10 and NELAY=2210 (dataset 2B), then"            SUTERR.......16700
        EX(7)="NN=9*2210=19890 (dataset 3)."                             SUTERR.......16800
      ELSE IF (ERRCOD.EQ.'INP-4-1') THEN                                 SUTERR.......16900
        DS(1)="The first word of CUNSAT is not 'SATURATED' or"           SUTERR.......17000
        DS(2)="'UNSATURATED'."                                           SUTERR.......17100
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......17200
        EX(2)="of the variable CUNSAT must be 'SATURATED' or"            SUTERR.......17300
        EX(3)="'UNSATURATED'."                                           SUTERR.......17400
        EX(4)=" "                                                        SUTERR.......17500
        EX(5)="Example of a valid dataset 4:"                            SUTERR.......17600
        EX(6)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......17700
     1        " 'COLD' 10"                                               SUTERR.......17800
      ELSE IF (ERRCOD.EQ.'INP-4-2') THEN                                 SUTERR.......17900
        DS(1)="The first word of CSSFLO is not 'STEADY' or 'TRANSIENT'." SUTERR.......18000
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......18100
        EX(2)="of the variable CSSFLO must be 'STEADY' or 'TRANSIENT'."  SUTERR.......18200
        EX(3)=" "                                                        SUTERR.......18300
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......18400
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......18500
     1        " 'COLD' 10"                                               SUTERR.......18600
      ELSE IF (ERRCOD.EQ.'INP-4-3') THEN                                 SUTERR.......18700
        DS(1)="The first word of CSSTRA is not 'STEADY' or 'TRANSIENT'." SUTERR.......18800
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......18900
        EX(2)="of the variable CSSTRA must be 'STEADY' or 'TRANSIENT'."  SUTERR.......19000
        EX(3)=" "                                                        SUTERR.......19100
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......19200
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......19300
     1        " 'COLD' 10"                                               SUTERR.......19400
      ELSE IF (ERRCOD.EQ.'INP-4-4') THEN                                 SUTERR.......19500
        DS(1)="The first word of CREAD is not 'COLD' or 'WARM'."         SUTERR.......19600
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......19700
        EX(2)="of the variable CREAD must be 'COLD' or 'WARM'."          SUTERR.......19800
        EX(3)=" "                                                        SUTERR.......19900
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......20000
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......20100
     1        " 'COLD' 10"                                               SUTERR.......20200
      ELSE IF (ERRCOD.EQ.'INP-4-5') THEN                                 SUTERR.......20300
        DS(1)="Specified TRANSIENT flow with STEADY transport."          SUTERR.......20400
        EX(1)="In dataset 4 of the main input file, TRANSIENT flow"      SUTERR.......20500
        EX(2)="requires TRANSIENT transport.  Likewise, STEADY"          SUTERR.......20600
        EX(3)="transport requires STEADY flow.  The following are"       SUTERR.......20700
        EX(4)="valid combinations:"                                      SUTERR.......20800
        EX(5)=" "                                                        SUTERR.......20900
        EX(6)="     CSSFLO      CSSTRA"                                  SUTERR.......21000
        EX(7)="   ----------- -----------"                               SUTERR.......21100
        EX(8)="    'STEADY'    'STEADY'"                                 SUTERR.......21200
        EX(9)="    'STEADY'   'TRANSIENT'"                               SUTERR.......21300
        EX(10)="   'TRANSIENT' 'TRANSIENT'"                              SUTERR.......21400
        EX(11)=" "                                                       SUTERR.......21500
        EX(12)="Example of a valid dataset 4:"                           SUTERR.......21600
        EX(13)="'SATURATED FLOW' 'STEADY FLOW' 'STEADY TRANSPORT'" //    SUTERR.......21700
     1        " 'COLD' 10"                                               SUTERR.......21800
      ELSE IF (ERRCOD.EQ.'INP-7B&C-1') THEN                              SUTERR.......21900
        DS(1)="Unrecognized solver name."                                SUTERR.......22000
        EX(1)="In datasets 7B&C, valid solver selections are:"           SUTERR.......22100
        EX(2)=" "                                                        SUTERR.......22200
        DO 400 M=0,NSLVRS-1                                              SUTERR.......22300
           EX(M+3)=SOLWRD(M) // " --> " // SOLNAM(M)                     SUTERR.......22400
  400   CONTINUE                                                         SUTERR.......22500
        EX(NSLVRS+3)=" "                                                 SUTERR.......22600
        EX(NSLVRS+4)="Note that solver selections for P and U must be"   SUTERR.......22700
        EX(NSLVRS+5)="both DIRECT or both iterative."                    SUTERR.......22800
      ELSE IF (ERRCOD.EQ.'INP-7B&C-2') THEN                              SUTERR.......22900
        DS(1)="Solver selections for P and U are not both DIRECT or"     SUTERR.......23000
        DS(2)="both iterative."                                          SUTERR.......23100
        EX(1)="The solver selections for P and U must be both"           SUTERR.......23200
        EX(2)="DIRECT or both iterative."                                SUTERR.......23300
      ELSE IF (ERRCOD.EQ.'INP-7B&C-3') THEN                              SUTERR.......23400
        DS(1)="Invalid selection of the CG solver."                      SUTERR.......23500
        EX(1)="The CG solver may be used only for the flow (P) equation" SUTERR.......23600
        EX(2)="with no upstream weighting (UP=0.0).  It may not be used" SUTERR.......23700
        EX(3)="for the transport (U) equation."                          SUTERR.......23800
C.....ERROR CODE 'INP-7B&C-4' IS NO LONGER USED.                         SUTERR.......23900
      ELSE IF (ERRCOD.EQ.'INP-3,19-1') THEN                              SUTERR.......24000
        DS(1)="The actual number of specified pressure nodes, "          SUTERR.......24100
     1        // CINERR(1) // ","                                        SUTERR.......24200
        DS(2)="does not equal the input value,                "          SUTERR.......24300
     1        // CINERR(2) // "."                                        SUTERR.......24400
        EX(1)="In dataset 3 of the main input file, the variable NPBC"   SUTERR.......24500
        EX(2)="must specify the exact number of specified pressure"      SUTERR.......24600
        EX(3)="nodes listed in dataset 19."                              SUTERR.......24700
      ELSE IF (ERRCOD.EQ.'INP-3,20-1') THEN                              SUTERR.......24800
        DS(1)="The actual number of specified conc. nodes, "             SUTERR.......24900
     1        // CINERR(1) // ","                                        SUTERR.......25000
        DS(2)="does not equal the input value,             "             SUTERR.......25100
     1        // CINERR(2) // "."                                        SUTERR.......25200
        EX(1)="In dataset 3 of the main input file, the variable NUBC"   SUTERR.......25300
        EX(2)="must specify the exact number of specified concentration" SUTERR.......25400
        EX(3)="nodes listed in dataset 20."                              SUTERR.......25500
      ELSE IF (ERRCOD.EQ.'INP-3,20-2') THEN                              SUTERR.......25600
        DS(1)="The actual number of specified temp. nodes, "             SUTERR.......25700
     1        // CINERR(1) // ","                                        SUTERR.......25800
        DS(2)="does not equal the input value,             "             SUTERR.......25900
     1        // CINERR(2) // "."                                        SUTERR.......26000
        EX(1)="In dataset 3 of the main input file, the variable NUBC"   SUTERR.......26100
        EX(2)="must specify the exact number of specified temperature"   SUTERR.......26200
        EX(3)="nodes listed in dataset 20."                              SUTERR.......26300
      ELSE IF (ERRCOD.EQ.'INP-22-1') THEN                                SUTERR.......26400
        DS(1)="Line 1 of the element incidence data does not begin with" SUTERR.......26500
        DS(2)="the word 'INCIDENCE'."                                    SUTERR.......26600
        EX(1)="In dataset 22 of the main input file, the first line"     SUTERR.......26700
        EX(2)="must begin with the word 'INCIDENCE'."                    SUTERR.......26800
      ELSE IF (ERRCOD.EQ.'INP-22-2') THEN                                SUTERR.......26900
        DS(1)="The incidence data for element " // CINERR(1)             SUTERR.......27000
        DS(2)="are not in numerical order in the dataset."               SUTERR.......27100
        EX(1)="In dataset 22 of the main input file, incidence data"     SUTERR.......27200
        EX(2)="must be listed in order of increasing element number."    SUTERR.......27300
        EX(3)="Note that the numbering of elements must begin at 1"      SUTERR.......27400
        EX(4)="and be continuous; element numbers may not be skipped."   SUTERR.......27500
      ELSE IF (ERRCOD.EQ.'INP-14B,22-1') THEN                            SUTERR.......27600
        DS(1)="At least one element has incorrect geometry."             SUTERR.......27700
        EX(1)="Incorrect element geometry can result from improper"      SUTERR.......27800
        EX(2)="specification of node coordinates in dataset 14B of the"  SUTERR.......27900
        EX(3)="main input file, or from improper ordering of nodes in"   SUTERR.......28000
        EX(4)="a node incidence list in dataset 22 of the same file."    SUTERR.......28100
      ELSE IF (ERRCOD.EQ.'FIL-1') THEN                                   SUTERR.......28200
        DS(1)="The file " // CHERR(2)                                    SUTERR.......28300
        DS(2)="does not exist."                                          SUTERR.......28400
        EX(1)="One of the files required by SUTRA does not exist."       SUTERR.......28500
        EX(2)="Check the filename and the directory path."               SUTERR.......28600
      ELSE IF (ERRCOD.EQ.'FIL-2') THEN                                   SUTERR.......28700
        DS(1)="The file " // CHERR(2)                                    SUTERR.......28800
        DS(2)="could not be opened on FORTRAN unit " // CINERR(1) // "." SUTERR.......28900
        EX(1)="One of the files required by SUTRA could not be opened."  SUTERR.......29000
        EX(2)="Check to make sure the file is not protected or in use"   SUTERR.......29100
        EX(3)="by another application, and that the FORTRAN unit number" SUTERR.......29200
        EX(4)="is valid."                                                SUTERR.......29300
C.....ERROR CODE 'FIL-3' IS NO LONGER USED.                              SUTERR.......29400
      ELSE IF (ERRCOD.EQ.'FIL-4') THEN                                   SUTERR.......29500
        DS(1)="An attempt was made to use the file"                      SUTERR.......29600
        DS(2)=CHERR(2)                                                   SUTERR.......29700
        DS(3)="for more than one purpose simultaneously."                SUTERR.......29800
        EX(1)='Each filename listed in "SUTRA.FIL" must be unique'       SUTERR.......29900
        EX(2)='and may not be reused in an "@INSERT" statement.'         SUTERR.......30000
        EX(3)='Also, if you have nested "@INSERT" statements'            SUTERR.......30100
        EX(4)='(i.e., a file inserted into a file, which is itself'      SUTERR.......30200
        EX(5)='inserted into a file, etc.), a given file may be'         SUTERR.......30300
        EX(6)='used only once in the nested sequence.'                   SUTERR.......30400
      ELSE IF (ERRCOD.EQ.'FIL-5') THEN                                   SUTERR.......30500
        DS(1)="Invalid file type: " // CHERR(2)                          SUTERR.......30600
        EX(1)="Valid file types are:"                                    SUTERR.......30700
        EX(2)='   INP (".inp" input file)'                               SUTERR.......30800
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......30900
        EX(4)='   SMY (".smy" output file)'                              SUTERR.......31000
        EX(5)='   LST (".lst" output file)'                              SUTERR.......31100
        EX(6)='   RST (".rst" output file)'                              SUTERR.......31200
        EX(7)='   NOD (".nod" output file)'                              SUTERR.......31300
        EX(8)='   ELE (".ele" output file)'                              SUTERR.......31400
        EX(9)='   OBS (".obs" output file)'                              SUTERR.......31500
        EX(10)='   OBC (".obc" output file)'                             SUTERR.......31600
      ELSE IF (ERRCOD.EQ.'FIL-6') THEN                                   SUTERR.......31700
        DS(1)="File type " // CHERR(2)                                   SUTERR.......31800
        DS(2)="has been assigned more than once."                        SUTERR.......31900
        EX(1)="The following file types must be assigned:"               SUTERR.......32000
        EX(2)='   INP (".inp" input file)'                               SUTERR.......32100
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......32200
        EX(4)='   LST (".lst" output file)'                              SUTERR.......32300
        EX(5)='   RST (".rst" output file)'                              SUTERR.......32400
        EX(6)="The following file types are optional:"                   SUTERR.......32500
        EX(7)='   SMY (".smy" output file; defaults to "SUTRA.SMY")'     SUTERR.......32600
        EX(8)='   NOD (".nod" output file)'                              SUTERR.......32700
        EX(9)='   ELE (".ele" output file)'                              SUTERR.......32800
        EX(10)='   OBS (".obs" output file)'                             SUTERR.......32900
        EX(11)='   OBC (".obc" output file; defaults to "SUTRA.OBC")'    SUTERR.......33000
        EX(12)="No file type may be assigned more than once."            SUTERR.......33100
      ELSE IF (ERRCOD.EQ.'FIL-7') THEN                                   SUTERR.......33200
        DS(1)="Required file type " // CHERR(2)                          SUTERR.......33300
        DS(2)="has not been assigned."                                   SUTERR.......33400
        EX(1)="The following file types must be assigned:"               SUTERR.......33500
        EX(2)='   INP (".inp" input file)'                               SUTERR.......33600
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......33700
        EX(4)='   LST (".lst" output file)'                              SUTERR.......33800
        EX(5)='   RST (".rst" output file)'                              SUTERR.......33900
        EX(6)="The following file types are optional:"                   SUTERR.......34000
        EX(7)='   SMY (".smy" output file; defaults to "SUTRA.SMY")'     SUTERR.......34100
        EX(8)='   NOD (".nod" output file)'                              SUTERR.......34200
        EX(9)='   ELE (".ele" output file)'                              SUTERR.......34300
        EX(10)='   OBS (".obs" output file)'                             SUTERR.......34400
        EX(11)='   OBC (".obc" output file; defaults to "SUTRA.OBC")'    SUTERR.......34500
        EX(12)="No file type may be assigned more than once."            SUTERR.......34600
      ELSE IF (ERRCOD.EQ.'FIL-8') THEN                                   SUTERR.......34700
        DS(1)="The file " // CHERR(2)                                    SUTERR.......34800
        DS(2)="could not be inserted."                                   SUTERR.......34900
        EX(1)="Inserts cannot be nested more than 20 levels deep."       SUTERR.......35000
      ELSE IF (ERRCOD.EQ.'FIL-9') THEN                                   SUTERR.......35100
        DS(1)="A file listed in 'SUTRA.FIL' is named 'SUTRA.FIL'."       SUTERR.......35200
        EX(1)="The filename 'SUTRA.FIL' is reserved by SUTRA."           SUTERR.......35300
        EX(2)="Files listed in 'SUTRA.FIL' may not be named"             SUTERR.......35400
        EX(3)="'SUTRA.FIL'."                                             SUTERR.......35500
      ELSE IF (ERRCOD.EQ.'FIL-10') THEN                                  SUTERR.......35600
        DS(1)="SUTRA was unable to automatically"                        SUTERR.......35700
        DS(2)="assign unit number " // CINERR(1)                         SUTERR.......35800
        DS(3)="to file " // CHERR(2)                                     SUTERR.......35900
        EX(1)="SUTRA attempted to automatically assign to one of the "   SUTERR.......36000
        EX(2)="files listed in 'SUTRA.FIL' a unit number that is not"    SUTERR.......36100
        EX(3)="allowed on this computer.  Please check the unit"         SUTERR.......36200
        EX(4)="number assignments in 'SUTRA.FIL'.  It may be possible"   SUTERR.......36300
        EX(5)="to avoid this problem by explicitly assigning a"          SUTERR.......36400
        EX(6)="different unit number to the file in question or by"      SUTERR.......36500
        EX(7)="reducing the number of optional files listed in"          SUTERR.......36600
        EX(8)="'SUTRA.FIL'."                                             SUTERR.......36700
      ELSE IF (ERRCOD.EQ.'INP-6-1') THEN                                 SUTERR.......36800
        DS(1)="NPCYC<1 and/or NUCYC<1."                                  SUTERR.......36900
        EX(1)="In dataset 6 of the main input file, both NPCYC and"      SUTERR.......37000
        EX(2)="NUCYC must be set greater than or equal to 1."            SUTERR.......37100
      ELSE IF (ERRCOD.EQ.'INP-6-2') THEN                                 SUTERR.......37200
        DS(1)="Neither NPCYC nor NUCYC is set to 1."                     SUTERR.......37300
        EX(1)="In dataset 6 of the main input file, either NPCYC or"     SUTERR.......37400
        EX(2)="NUCYC (or both) must be set to 1."                        SUTERR.......37500
      ELSE IF (ERRCOD.EQ.'INP-6-3') THEN                                 SUTERR.......37600
        DS(1)="DELT is greater than DTMAX."                              SUTERR.......37700
        EX(1)="In dataset 6 of the main input file, DELT must be set"    SUTERR.......37800
        EX(2)="less than or equal to DTMAX."                             SUTERR.......37900
      ELSE IF (ERRCOD.EQ.'INP-6-4') THEN                                 SUTERR.......38000
        DS(1)="The actual number of schedules listed does not equal"     SUTERR.......38100
        DS(2)="the input value, or the schedule list does not end"       SUTERR.......38200
        DS(3)="with '-'."                                                SUTERR.......38300
        EX(1)="In dataset 6 of the main input file, the number of"       SUTERR.......38400
        EX(2)="schedules listed must equal the number, NSCH, specified"  SUTERR.......38500
        EX(3)="in dataset 6 of the same file, and the final entry in"    SUTERR.......38600
        EX(4)="the list must be '-'."                                    SUTERR.......38700
        EX(5)=" "                                                        SUTERR.......38800
        EX(6)="Example of a valid dataset 6 with two schedules:"         SUTERR.......38900
        EX(7)="2   1   1"                                                SUTERR.......39000
        EX(8)="'TIME_STEPS' 'TIME CYCLE' 'ELAPSED'  1. 100   0. " //     SUTERR.......39100
     1     "3.e+9 3.e+7 999 1. 0. 1.e+99"                                SUTERR.......39200
        EX(9)="'SCHED_A'    'STEP LIST'        4   20  40  60  80"       SUTERR.......39300
        EX(10)="'-'"                                                     SUTERR.......39400
      ELSE IF (ERRCOD.EQ.'INP-6-5') THEN                                 SUTERR.......39500
        DS(1)="Multiple definitions of schedule " // CHERR(1)            SUTERR.......39600
        EX(1)="A given schedule name may not be defined more than once"  SUTERR.......39700
        EX(2)="in dataset 6 of the main input file."                     SUTERR.......39800
      ELSE IF (ERRCOD.EQ.'INP-6-6') THEN                                 SUTERR.......39900
        DS(1)="Invalid time descriptor " // CHERR(1)                     SUTERR.......40000
        EX(1)="Time-based schedules must be defined in terms of either"  SUTERR.......40100
        EX(2)="ABSOLUTE or ELAPSED times."                               SUTERR.......40200
      ELSE IF (ERRCOD.EQ.'INP-6-7') THEN                                 SUTERR.......40300
        DS(1)="ELAPSED times in TIME_STEPS schedule,"                    SUTERR.......40400
        DS(2)="but initial elapsed time is not zero."                    SUTERR.......40500
        EX(1)="When the TIME_STEPS schedule is defined in terms of"      SUTERR.......40600
        EX(2)="ELAPSED times, the first (initial) elapsed time in the"   SUTERR.......40700
        EX(3)="schedule must be set to zero."                            SUTERR.......40800
      ELSE IF (ERRCOD.EQ.'INP-6-8') THEN                                 SUTERR.......40900
        DS(1)="Invalid number of schedules (NSCH<0)."                    SUTERR.......41000
        EX(1)="The number of schedules, NSCH, must be non-negative."     SUTERR.......41100
        EX(2)="NSCH=0 is allowed only if flow and transport are both"    SUTERR.......41200
        EX(3)="steady-state."                                            SUTERR.......41300
      ELSE IF (ERRCOD.EQ.'INP-6-9') THEN                                 SUTERR.......41400
        DS(1)="Invalid schedule type " // CHERR(1)                       SUTERR.......41500
        EX(1)="An invalid schedule type has been specified."             SUTERR.......41600
        EX(2)="Valid schedule types are:"                                SUTERR.......41700
        EX(3)="   'TIME CYCLE'"                                          SUTERR.......41800
        EX(4)="   'TIME LIST'"                                           SUTERR.......41900
        EX(5)="   'STEP CYCLE'"                                          SUTERR.......42000
        EX(6)="   'STEP LIST'"                                           SUTERR.......42100
      ELSE IF (ERRCOD.EQ.'INP-6-10') THEN                                SUTERR.......42200
        DS(1)="Incomplete TIME_STEPS schedule."                          SUTERR.......42300
        EX(1)="The TIME_STEPS schedule must contain at least two"        SUTERR.......42400
        EX(2)="distinct times, including the starting time."             SUTERR.......42500
C.....ERROR CODE INP-6-11 HAS NEVER BEEN USED IN AN OFFICIAL RELEASE OF  SUTERR.......42600
C        SUTRA AND IS THEREFORE STILL AVAILABLE.                         SUTERR.......42700
      ELSE IF (ERRCOD.EQ.'INP-6-12') THEN                                SUTERR.......42800
        DS(1)="Repeated " // TRIM(CHERR(1)) // " " // CRLERR(1)          SUTERR.......42900
        DS(2)="in schedule " // CHERR(2)                                 SUTERR.......43000
        EX(1)="A time or time step value may not appear more than once"  SUTERR.......43100
        EX(2)="in a given schedule."                                     SUTERR.......43200
      ELSE IF (ERRCOD.EQ.'INP-6-13') THEN                                SUTERR.......43300
        DS(1)="Invalid number of schedules (NSCH=0)."                    SUTERR.......43400
        EX(1)="NSCH=0 is allowed only if flow and transport are both"    SUTERR.......43500
        EX(2)="steady-state."                                            SUTERR.......43600
      ELSE IF (ERRCOD.EQ.'INP-6-14') THEN                                SUTERR.......43700
        DS(1)="Missing TIME_STEPS schedule."                             SUTERR.......43800
        EX(1)="When transport is transient, a TIME_STEPS schedule must"  SUTERR.......43900
        EX(2)="be defined by the user in dataset 6."                     SUTERR.......44000
      ELSE IF ((ERRCOD.EQ.'INP-8A-1').OR.(ERRCOD.EQ.'INP-8A-2')          SUTERR.......44100
     1     .OR.(ERRCOD.EQ.'INP-8A-3').OR.(ERRCOD.EQ.'INP-8A-4')          SUTERR.......44200
     1     .OR.(ERRCOD.EQ.'INP-8A-5').OR.(ERRCOD.EQ.'INP-8A-6')          SUTERR.......44300
     1     .OR.(ERRCOD.EQ.'INP-8A-7')) THEN                              SUTERR.......44400
        DS(1)=CHERR(1)(1:6) // " is not 'Y' or 'N'."                     SUTERR.......44500
        EX(1)="In dataset 8A of the main input file, " // CHERR(1)(1:6)  SUTERR.......44600
        EX(2)="must be set to either 'Y' or 'N'."                        SUTERR.......44700
        EX(3)=" "                                                        SUTERR.......44800
        EX(4)="Example of a valid dataset 8A:"                           SUTERR.......44900
        EX(5)="10   'N'   'N'   'N'   'Y'   'Y'   'Y'   'Y'"             SUTERR.......45000
      ELSE IF (ERRCOD.EQ.'INP-8B-1') THEN                                SUTERR.......45100
        DS(1)="Node number listed in column other than column 1."        SUTERR.......45200
        EX(1)="In dataset 8B of the main input file, if the node number" SUTERR.......45300
        EX(2)="is to appear, it must appear only in column 1, i.e.,"     SUTERR.......45400
        EX(3)="only NCOL(1) can be set to 'N'."                          SUTERR.......45500
      ELSE IF (ERRCOD.EQ.'INP-8B-2') THEN                                SUTERR.......45600
        DS(1)="Specified that 'Z' be output for a 2D problem."           SUTERR.......45700
        EX(1)="In dataset 8B of the main input file, 'Z' can be listed"  SUTERR.......45800
        EX(2)="only if the problem is 3D."                               SUTERR.......45900
      ELSE IF (ERRCOD.EQ.'INP-8B-3') THEN                                SUTERR.......46000
        DS(1)="Unrecognized value for NCOL."                             SUTERR.......46100
        EX(1)="In dataset 8B of the main input file, the following"      SUTERR.......46200
        EX(2)="variables may be listed:"                                 SUTERR.......46300
        EX(3)=" "                                                        SUTERR.......46400
        EX(4)="'N'  =  node number (if used, it must appear first)"      SUTERR.......46500
        EX(5)="'X'  =  X-coordinate of node"                             SUTERR.......46600
        EX(6)="'Y'  =  Y-coordinate of node"                             SUTERR.......46700
        EX(7)="'Z'  =  Z-coordinate of node (3D only)"                   SUTERR.......46800
        EX(8)="'P'  =  pressure"                                         SUTERR.......46900
        EX(9)="'U'  =  concentration or temperature"                     SUTERR.......47000
        EX(10)="'S'  =  saturation"                                      SUTERR.......47100
        EX(11)=" "                                                       SUTERR.......47200
        EX(12)="The symbol '-' (a single dash) is used to end the list." SUTERR.......47300
        EX(13)="Any symbols following '-' are ignored."                  SUTERR.......47400
        EX(14)=" "                                                       SUTERR.......47500
        EX(15)="Example of a valid dataset 8B for a 3D problem:"         SUTERR.......47600
        EX(16)="10  'N'  'X'  'Y'  'Z'  'S'  'U'  '-'"                   SUTERR.......47700
      ELSE IF (ERRCOD.EQ.'INP-8C-1') THEN                                SUTERR.......47800
        DS(1)="Element number listed in column other than column 1."     SUTERR.......47900
        EX(1)="In dataset 8C of the main input file, if the element"     SUTERR.......48000
        EX(2)="number is to appear, it must appear only in column 1,"    SUTERR.......48100
        EX(3)="i.e., only LCOL(1) can be set to 'E'."                    SUTERR.......48200
      ELSE IF (ERRCOD.EQ.'INP-8C-2') THEN                                SUTERR.......48300
        DS(1)="Specified that 'Z' be output for a 2D problem."           SUTERR.......48400
        EX(1)="In dataset 8C of the main input file, 'Z' can be listed"  SUTERR.......48500
        EX(2)="only if the problem is 3D."                               SUTERR.......48600
      ELSE IF (ERRCOD.EQ.'INP-8C-3') THEN                                SUTERR.......48700
        DS(1)="Unrecognized value for LCOL."                             SUTERR.......48800
        EX(1)="In dataset 8C of the main input file, the following"      SUTERR.......48900
        EX(2)="variables may be listed:"                                 SUTERR.......49000
        EX(3)=" "                                                        SUTERR.......49100
        EX(4)="'E'  =  element number (if used, it must appear first)"   SUTERR.......49200
        EX(5)="'X'  =  X-coordinate of element centroid"                 SUTERR.......49300
        EX(6)="'Y'  =  Y-coordinate of element centroid"                 SUTERR.......49400
        EX(7)="'Z'  =  Z-coordinate of element centroid (3D only)"       SUTERR.......49500
        EX(8)="'VX'  =  X-component of fluid velocity"                   SUTERR.......49600
        EX(9)="'VY'  =  Y-component of fluid velocity"                   SUTERR.......49700
        EX(10)="'VZ'  =  Z-component of fluid velocity (3D only)"        SUTERR.......49800
        EX(11)=" "                                                       SUTERR.......49900
        EX(12)="The symbol '-' (a single dash) is used to end the list." SUTERR.......50000
        EX(13)="Any symbols following '-' are ignored."                  SUTERR.......50100
        EX(14)=" "                                                       SUTERR.......50200
        EX(15)="Example of a valid dataset 8B for a 3D problem:"         SUTERR.......50300
        EX(16)="10  'E'  'X'  'Y'  'Z'  'VX'  'VY'  'VZ'  '-'"           SUTERR.......50400
      ELSE IF (ERRCOD.EQ.'INP-8C-4') THEN                                SUTERR.......50500
        DS(1)="Specified that 'VZ' be output for a 2D problem."          SUTERR.......50600
        EX(1)="In dataset 8C of the main input file, 'VZ' can be listed" SUTERR.......50700
        EX(2)="only if the problem is 3D."                               SUTERR.......50800
      ELSE IF (ERRCOD.EQ.'INP-8D-1') THEN                                SUTERR.......50900
        DS(1)="The actual number of observation points listed does not"  SUTERR.......51000
        DS(2)="equal the input value, or the observation point list"     SUTERR.......51100
        DS(3)="does not end with a zero."                                SUTERR.......51200
        EX(1)="In dataset 8D of the main input file, the number of"      SUTERR.......51300
        EX(2)="points listed must equal the number, NOBS, specified in"  SUTERR.......51400
        EX(3)="dataset 3 of the same file, and a zero must appear after" SUTERR.......51500
        EX(4)="the last point in the list when the old format is used."  SUTERR.......51600
        EX(5)="Any information appearing after the zero is ignored."     SUTERR.......51700
        EX(6)=" "                                                        SUTERR.......51800
        EX(7)="Example of a valid old-format dataset 8D with three"      SUTERR.......51900
        EX(8)="observation points (nodes 45, 46, and 7347),"             SUTERR.......52000
        EX(9)="assuming NN>=7347:"                                       SUTERR.......52100
        EX(10)="10   45   46   7347   0"                                 SUTERR.......52200
      ELSE IF (ERRCOD.EQ.'INP-8D-2') THEN                                SUTERR.......52300
        DS(1)="The observation node list contains an invalid node"       SUTERR.......52400
        DS(2)="number."                                                  SUTERR.......52500
        EX(1)="In dataset 8D of the main input file, all node numbers"   SUTERR.......52600
        EX(2)="must be greater than or equal to 1, and less than or"     SUTERR.......52700
        EX(3)="equal to NN, the total number of nodes.  The last entry"  SUTERR.......52800
        EX(4)="must be a zero, which signals the end of the list."       SUTERR.......52900
        EX(5)=" "                                                        SUTERR.......53000
        EX(6)="Example of a valid old-format dataset 8D with three"      SUTERR.......53100
        EX(7)="observation nodes (45, 46, and 7347),"                    SUTERR.......53200
        EX(8)="assuming NN>=7347:"                                       SUTERR.......53300
        EX(9)="10   45   46   7347   0"                                  SUTERR.......53400
      ELSE IF (ERRCOD.EQ.'INP-8D-3') THEN                                SUTERR.......53500
        DS(1)="Element not found for the following observation point:"   SUTERR.......53600
        DS(2)="   " // CHERR(1)                                          SUTERR.......53700
        DS(3)="   " // CHERR(2)                                          SUTERR.......53800
        EX(1)="SUTRA was unable to find an element that contains"        SUTERR.......53900
        EX(2)="the observation point named above.  Please check"         SUTERR.......54000
        EX(3)="to make sure the coordinates specified for that"          SUTERR.......54100
        EX(4)="observation point are within the model domain."           SUTERR.......54200
      ELSE IF (ERRCOD.EQ.'INP-8D-4') THEN                                SUTERR.......54300
        DS(1)="The actual number of observation points listed does not"  SUTERR.......54400
        DS(2)="equal the input value, or the observation point list"     SUTERR.......54500
        DS(3)="does not end with '-'."                                   SUTERR.......54600
        EX(1)="In dataset 8D of the main input file, the number of"      SUTERR.......54700
        EX(2)="points listed must equal the number, NOBS, specified in"  SUTERR.......54800
        EX(3)="dataset 3 of the same file, and the final entry in the"   SUTERR.......54900
        EX(4)="list must be '-'."                                        SUTERR.......55000
        EX(5)=" "                                                        SUTERR.......55100
        EX(6)="Example of a valid dataset 8D with two 3D observation"    SUTERR.......55200
        EX(7)="points, assuming schedules A and B have been defined:"    SUTERR.......55300
        EX(8)="'POINT_1'     0.   100.   500.   'A'   'OBS'"             SUTERR.......55400
        EX(9)="'POINT_2'   100.   200.   800.   'B'   'OBC'"             SUTERR.......55500
        EX(10)="'-'"                                                     SUTERR.......55600
      ELSE IF (ERRCOD.EQ.'INP-8D-5') THEN                                SUTERR.......55700
        DS(1)="Undefined schedule " // CHERR(1)                          SUTERR.......55800
        DS(2)="specified for observation " // CHERR(2)                   SUTERR.......55900
        EX(1)="The output schedule specified for one of the"             SUTERR.......56000
        EX(2)="observation points has not been defined in dataset 6"     SUTERR.......56100
        EX(3)="of the main input file."                                  SUTERR.......56200
      ELSE IF (ERRCOD.EQ.'INP-11-1') THEN                                SUTERR.......56300
        DS(1)="Unrecognized sorption model."                             SUTERR.......56400
        EX(1)="In dataset 11 of the main input file, the sorption model" SUTERR.......56500
        EX(2)="may be chosen from the following:"                        SUTERR.......56600
        EX(3)=" "                                                        SUTERR.......56700
        EX(4)="'NONE'       =  No sorption"                              SUTERR.......56800
        EX(5)="'LINEAR'     =  Linear sorption model"                    SUTERR.......56900
        EX(6)="'FREUNDLICH' =  Freundlich sorption model"                SUTERR.......57000
        EX(7)="'LANGMUIR'   =  Langmuir sorption model"                  SUTERR.......57100
      ELSE IF (ERRCOD.EQ.'INP-11-2') THEN                                SUTERR.......57200
        DS(1)="The second Freundlich sorption coefficient is less than"  SUTERR.......57300
        DS(2)="or equal to zero."                                        SUTERR.......57400
        EX(1)="In dataset 11 of the main input file, the second"         SUTERR.......57500
        EX(2)="coefficient, CHI2, must be positive if Freundlich"        SUTERR.......57600
        EX(3)="sorption is chosen."                                      SUTERR.......57700
      ELSE IF (ERRCOD.EQ.'INP-14A-1') THEN                               SUTERR.......57800
        DS(1)="Dataset 14A does not begin with the word 'NODE'."         SUTERR.......57900
        EX(1)="Dataset 14A of the main input file must begin with the"   SUTERR.......58000
        EX(2)="word 'NODE'."                                             SUTERR.......58100
        EX(3)=" "                                                        SUTERR.......58200
        EX(4)="Example of a valid dataset 14A:"                          SUTERR.......58300
        EX(5)="'NODE'  1000.  1000.  1.  0.1"                            SUTERR.......58400
      ELSE IF (ERRCOD.EQ.'INP-15A-1') THEN                               SUTERR.......58500
        DS(1)="Dataset 15A does not begin with the word 'ELEMENT'."      SUTERR.......58600
        EX(1)="Dataset 15A of the main input file must begin with the"   SUTERR.......58700
        EX(2)="word 'ELEMENT'."                                          SUTERR.......58800
        EX(3)=" "                                                        SUTERR.......58900
        EX(4)="Example of a valid dataset 15A for a " // CHERR(1)(1:2)   SUTERR.......59000
     1         // " problem:"                                            SUTERR.......59100
        IF (CHERR(1).EQ."3D") THEN                                       SUTERR.......59200
          EX(5)="'ELEMENT' 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1." SUTERR.......59300
        ELSE                                                             SUTERR.......59400
          EX(5)="'ELEMENT' 1. 1. 1. 1. 1. 1. 1."                         SUTERR.......59500
        END IF                                                           SUTERR.......59600
      ELSE IF (ERRCOD.EQ.'ICS-2-1') THEN                                 SUTERR.......59700
        DS(1)="Unrecognized initialization type."                        SUTERR.......59800
        EX(1)="In dataset 2 of the initial conditions input file,"       SUTERR.......59900
        EX(2)="the valid types of initializations for P are UNIFORM"     SUTERR.......60000
        EX(3)="and NONUNIFORM."                                          SUTERR.......60100
      ELSE IF (ERRCOD.EQ.'ICS-2-2') THEN                                 SUTERR.......60200
        DS(1)="Did not specify NONUNIFORM initial values during a WARM"  SUTERR.......60300
        DS(2)="start."                                                   SUTERR.......60400
        EX(1)="In dataset 2 of the initial conditions input file,"       SUTERR.......60500
        EX(2)="initial values for P must be specified as NONUNIFORM"     SUTERR.......60600
        EX(3)="during a WARM start (i.e., if CREAD='WARM' in dataset 4"  SUTERR.......60700
        EX(4)="of the main input file)."                                 SUTERR.......60800
      ELSE IF (ERRCOD.EQ.'ICS-3-1') THEN                                 SUTERR.......60900
        DS(1)="Unrecognized initialization type."                        SUTERR.......61000
        EX(1)="In dataset 3 of the initial conditions input file,"       SUTERR.......61100
        EX(2)="the valid types of initializations for U are UNIFORM"     SUTERR.......61200
        EX(3)="and NONUNIFORM."                                          SUTERR.......61300
      ELSE IF (ERRCOD.EQ.'ICS-3-2') THEN                                 SUTERR.......61400
        DS(1)="Did not specify NONUNIFORM initial values during a WARM"  SUTERR.......61500
        DS(2)="start."                                                   SUTERR.......61600
        EX(1)="In dataset 3 of the initial conditions input file,"       SUTERR.......61700
        EX(2)="initial values for U must be specified as NONUNIFORM"     SUTERR.......61800
        EX(3)="during a WARM start (i.e., if CREAD='WARM' in dataset 4"  SUTERR.......61900
        EX(4)="of the main input file)."                                 SUTERR.......62000
      ELSE IF (ERRCOD.EQ.'SOL-1') THEN                                   SUTERR.......62100
        DS(1)="Error returned by the " // CHERR(2)(1:10)                 SUTERR.......62200
        DS(2)="solver while solving for " // CHERR(1)(1:1) // "."        SUTERR.......62300
        EX(1)="The iterative solver has stopped because of an error."    SUTERR.......62400
        EX(2)="Error flag values are interpreted as follows:"            SUTERR.......62500
        EX(3)="  "                                                       SUTERR.......62600
        EX(4)="IERR = 2  =>  Method stalled or failed to converge in"    SUTERR.......62700
        EX(5)="              the maximum number of iterations allowed."  SUTERR.......62800
        EX(6)="IERR = 4  =>  Convergence tolerance set too tight for"    SUTERR.......62900
        EX(7)="              machine precision."                         SUTERR.......63000
        EX(8)="IERR = 5  =>  Method broke down because preconditioning"  SUTERR.......63100
        EX(9)="              matrix is non-positive-definite."           SUTERR.......63200
        EX(10)="IERR = 6  =>  Method broke down because matrix is non-"  SUTERR.......63300
        EX(11)="              positive-definite or nearly so."           SUTERR.......63400
        EX(12)=" "                                                       SUTERR.......63500
        EX(13)="If the P-solution resulted in a solver error, an"        SUTERR.......63600
        EX(14)="attempt was still made to obtain a U-solution."          SUTERR.......63700
        EX(15)="The last P and U solutions were written to the"          SUTERR.......63800
        EX(16)="appropriate output files (except the restart file)"      SUTERR.......63900
        EX(17)="whether or not they resulted in solver errors."          SUTERR.......64000
      ELSE IF (ERRCOD.EQ.'INP-3,17-1') THEN                              SUTERR.......64100
        DS(1)="The actual number of"                                     SUTERR.......64200
        DS(2)="specified fluid source nodes,   " // CINERR(1) // ","     SUTERR.......64300
        DS(3)="does not equal the input value, " // CINERR(2) // "."     SUTERR.......64400
        EX(1)="In dataset 3 of the main input file, the variable NSOP"   SUTERR.......64500
        EX(2)="must specify the exact number of specified fluid source"  SUTERR.......64600
        EX(3)="nodes listed in dataset 17."                              SUTERR.......64700
      ELSE IF (ERRCOD.EQ.'INP-3,18-1') THEN                              SUTERR.......64800
        DS(1)="The actual number of"                                     SUTERR.......64900
        DS(2)="specified " // CHERR(1)(1:6) // " source nodes,  "        SUTERR.......65000
     1         // CINERR(1) // ","                                       SUTERR.......65100
        DS(3)="does not equal the input value, " // CINERR(2) // "."     SUTERR.......65200
        EX(1)="In dataset 3 of the main input file, the variable NSOU"   SUTERR.......65300
        EX(2)="must specify the exact number of specified "              SUTERR.......65400
     1         // CHERR(1)(1:6) // " source"                             SUTERR.......65500
        EX(3)="nodes listed in dataset 18."                              SUTERR.......65600
      ELSE IF (ERRCOD.EQ.'INP-17-1') THEN                                SUTERR.......65700
        DS(1)="Invalid node number referenced in dataset 17: "           SUTERR.......65800
     1         // CINERR(1)                                              SUTERR.......65900
        EX(1)="Dataset 17 of the main input file contains a reference"   SUTERR.......66000
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......66100
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......66200
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......66300
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......66400
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......66500
      ELSE IF (ERRCOD.EQ.'INP-18-1') THEN                                SUTERR.......66600
        DS(1)="Invalid node number referenced in dataset 18: "           SUTERR.......66700
     1         // CINERR(1)                                              SUTERR.......66800
        EX(1)="Dataset 18 of the main input file contains a reference"   SUTERR.......66900
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......67000
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......67100
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......67200
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......67300
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......67400
      ELSE IF (ERRCOD.EQ.'INP-19-1') THEN                                SUTERR.......67500
        DS(1)="Invalid node number referenced in dataset 19: "           SUTERR.......67600
     1         // CINERR(1)                                              SUTERR.......67700
        EX(1)="Dataset 19 of the main input file contains a reference"   SUTERR.......67800
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......67900
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......68000
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......68100
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......68200
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......68300
      ELSE IF (ERRCOD.EQ.'INP-20-1') THEN                                SUTERR.......68400
        DS(1)="Invalid node number referenced in dataset 20: "           SUTERR.......68500
     1         // CINERR(1)                                              SUTERR.......68600
        EX(1)="Dataset 20 of the main input file contains a reference"   SUTERR.......68700
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......68800
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......68900
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......69000
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......69100
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......69200
      ELSE IF (ERRCOD.EQ.'CON-1') THEN                                   SUTERR.......69300
        CDUM80 = 's'                                                     SUTERR.......69400
        IF (INERR(4).GT.13) THEN                                         SUTERR.......69500
           LDUM = 1                                                      SUTERR.......69600
        ELSE                                                             SUTERR.......69700
           LDUM = 0                                                      SUTERR.......69800
        END IF                                                           SUTERR.......69900
        DS(1)="Simulation terminated due to unconverged non-linearity"   SUTERR.......70000
        DS(2)="iterations.  Tolerance" // CDUM80(1:LDUM)                 SUTERR.......70100
     1         // " for " // CHERR(1)(1:INERR(4))                        SUTERR.......70200
        DS(3)="not reached."                                             SUTERR.......70300
        EX(1)="The " // CHERR(1)(1:INERR(4)) // " solution"              SUTERR.......70400
     1         // CDUM80(1:LDUM) // " failed"                            SUTERR.......70500
        EX(2)="to converge to the specified tolerance"                   SUTERR.......70600
     1         // CDUM80(1:LDUM) // " within"                            SUTERR.......70700
        EX(3)="the maximum number of iterations allowed to resolve"      SUTERR.......70800
        EX(4)="non-linearities.  The parameters that control these"      SUTERR.......70900
        EX(5)="iterations are set in dataset 7A of the main input file." SUTERR.......71000
      ELSE IF ((CODE(1).EQ.'REA').AND.                                   SUTERR.......71100
     1         ((CODE(2).EQ.'INP').OR.(CODE(2).EQ.'ICS'))) THEN          SUTERR.......71200
        IF (CODE(2).EQ.'INP') THEN                                       SUTERR.......71300
           CDUM80 = 'main input'                                         SUTERR.......71400
           LDUM = 10                                                     SUTERR.......71500
        ELSE                                                             SUTERR.......71600
           CDUM80 = 'initial conditions'                                 SUTERR.......71700
           LDUM = 18                                                     SUTERR.......71800
        END IF                                                           SUTERR.......71900
        IF ((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')) THEN                SUTERR.......72000
          DS(1)="FORTRAN returned an error while reading the restart"    SUTERR.......72100
          DS(2)="information following dataset 3 of the initial"         SUTERR.......72200
          DS(3)="conditions."                                            SUTERR.......72300
        ELSE IF (CODE(3).EQ.'INS') THEN                                  SUTERR.......72400
          CALL PRSWDS(CHERR(1), '-', 3, CODUM, NWORDS)                   SUTERR.......72500
          DS(1)="FORTRAN returned an error while reading an '@INSERT'"   SUTERR.......72600
          DS(2)="statement in the vicinity of dataset " // CODUM(3)(1:3) SUTERR.......72700
          DS(3)="of the " // CDUM80(1:LDUM) // "."                       SUTERR.......72800
        ELSE                                                             SUTERR.......72900
          DS(1)="FORTRAN returned an error while reading"                SUTERR.......73000
          DS(2)="dataset " // CODE(3)(1:3)                               SUTERR.......73100
     1           // " of the " // CDUM80(1:LDUM) // "."                  SUTERR.......73200
        END IF                                                           SUTERR.......73300
        EX(1)="A FORTRAN error has occurred while reading input data."   SUTERR.......73400
        EX(2)="Error status flag values are interpreted as follows:"     SUTERR.......73500
        EX(3)=" "                                                        SUTERR.......73600
        EX(4)="IOSTAT < 0  =>  The end of a line was reached before"     SUTERR.......73700
        EX(5)="                all the required data were read from"     SUTERR.......73800
        EX(6)="                that line.  Check the specified dataset"  SUTERR.......73900
        EX(7)="                for missing data or lines of data that"   SUTERR.......74000
        EX(8)="                exceed 1000 characters."                  SUTERR.......74100
        EX(9)="IOSTAT > 0  =>  An error occurred while the specified"    SUTERR.......74200
        EX(10)="                dataset was being read.  Usually, this"  SUTERR.......74300
        EX(11)="                indicates that the READ statement"       SUTERR.......74400
        EX(12)="                encountered data of a type that is"      SUTERR.......74500
        EX(13)="                incompatible with the type it expected." SUTERR.......74600
        EX(14)="                Check the dataset for typographical"     SUTERR.......74700
        EX(15)="                errors and missing or extraneous data."  SUTERR.......74800
      ELSE IF ((CODE(1).EQ.'REA').AND.(CODE(2).EQ.'FIL')) THEN           SUTERR.......74900
        DS(1)='FORTRAN returned an error while reading "SUTRA.FIL".'     SUTERR.......75000
        EX(1)='A FORTRAN error has occurred while reading "SUTRA.FIL".'  SUTERR.......75100
        EX(2)="Error status flag values are interpreted as follows:"     SUTERR.......75200
        EX(3)=" "                                                        SUTERR.......75300
        EX(4)="IOSTAT < 0  =>  The end of a line was reached before"     SUTERR.......75400
        EX(5)="                all the required data were read from"     SUTERR.......75500
        EX(6)='                that line.  Check "SUTRA.FIL" for'        SUTERR.......75600
        EX(7)="                missing data."                            SUTERR.......75700
        EX(8)="IOSTAT > 0  =>  An error occurred while the input"        SUTERR.......75800
        EX(9)="                file was being read.  Usually, this"      SUTERR.......75900
        EX(10)="                indicates that the READ statement"       SUTERR.......76000
        EX(11)="                encountered data of a type that is"      SUTERR.......76100
        EX(12)="                incompatible with the type it expected." SUTERR.......76200
        EX(13)='                Check "SUTRA.FIL" for typographical'     SUTERR.......76300
        EX(14)="                errors and missing or extraneous data."  SUTERR.......76400
      END IF                                                             SUTERR.......76500
C                                                                        SUTERR.......76600
C.....WRITE ERROR MESSAGE.  FORMAT DEPENDS ON THE TYPE OF ERROR.         SUTERR.......76700
      IF ((CODE(1).EQ.'INP').OR.(CODE(1).EQ.'ICS')) THEN                 SUTERR.......76800
C........ERROR TYPES 'INP' AND 'ICS' (INPUT DATA ERROR)                  SUTERR.......76900
         IF (KSCRN.EQ.1)                                                 SUTERR.......77000
     1      WRITE (*,1888) '           INPUT DATA ERROR           '      SUTERR.......77100
         WRITE (K00,1888) '           INPUT DATA ERROR           '       SUTERR.......77200
         IF (KSCRN.EQ.1) WRITE (*,1011)                                  SUTERR.......77300
         WRITE (K00,1011)                                                SUTERR.......77400
 1011    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......77500
         IF (CODE(1).EQ.'INP') THEN                                      SUTERR.......77600
            CDUM80 = FNAME(1)                                            SUTERR.......77700
         ELSE                                                            SUTERR.......77800
            CDUM80 = FNAME(2)                                            SUTERR.......77900
         END IF                                                          SUTERR.......78000
         IF (KSCRN.EQ.1) WRITE (*,1013) ERRCOD, CDUM80, CODE(2)          SUTERR.......78100
         WRITE (K00,1013) ERRCOD, CDUM80, CODE(2)                        SUTERR.......78200
 1013    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......78300
     1           /4X,'File:      ',2X,A40                                SUTERR.......78400
     1           /4X,'Dataset(s):',2X,A40/)                              SUTERR.......78500
         DO 1015 I=1,50                                                  SUTERR.......78600
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......78700
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......78800
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......78900
 1015    CONTINUE                                                        SUTERR.......79000
         IF (KSCRN.EQ.1) WRITE (*,1021)                                  SUTERR.......79100
         WRITE (K00,1021)                                                SUTERR.......79200
 1021    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......79300
         DO 1025 I=1,50                                                  SUTERR.......79400
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......79500
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......79600
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......79700
 1025    CONTINUE                                                        SUTERR.......79800
         IF (KSCRN.EQ.1) WRITE (*,1081)                                  SUTERR.......79900
         WRITE (K00,1081)                                                SUTERR.......80000
 1081    FORMAT (/1X,'GENERAL NOTE'/                                     SUTERR.......80100
     1     /4X,'If the dataset for which SUTRA has reported an error'    SUTERR.......80200
     1     /4X,'appears to be correct, check the preceding lines'        SUTERR.......80300
     1     /4X,'for missing data or extraneous characters.')             SUTERR.......80400
      ELSE IF (CODE(1).EQ.'FIL') THEN                                    SUTERR.......80500
C........ERROR TYPE 'FIL' (FILE ERROR)                                   SUTERR.......80600
         IF (KSCRN.EQ.1)                                                 SUTERR.......80700
     1      WRITE (*,1888)'              FILE ERROR              '       SUTERR.......80800
         WRITE (K00,1888) '              FILE ERROR              '       SUTERR.......80900
         IF (KSCRN.EQ.1) WRITE (*,1211)                                  SUTERR.......81000
         WRITE (K00,1211)                                                SUTERR.......81100
 1211    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......81200
         IF (KSCRN.EQ.1) WRITE (*,1213) ERRCOD, CHERR(1)                 SUTERR.......81300
         WRITE (K00,1213) ERRCOD, CHERR(1)                               SUTERR.......81400
 1213    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......81500
     1           /4X,'File:      ',2X,A40/)                              SUTERR.......81600
         DO 1215 I=1,50                                                  SUTERR.......81700
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......81800
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......81900
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......82000
 1215    CONTINUE                                                        SUTERR.......82100
         IF (KSCRN.EQ.1) WRITE (*,1221)                                  SUTERR.......82200
         WRITE (K00,1221)                                                SUTERR.......82300
 1221    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......82400
         DO 1225 I=1,50                                                  SUTERR.......82500
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......82600
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......82700
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......82800
 1225    CONTINUE                                                        SUTERR.......82900
      ELSE IF (CODE(1).EQ.'SOL') THEN                                    SUTERR.......83000
C........ERROR TYPE 'SOL' (MATRIX SOLVER ERROR)                          SUTERR.......83100
         IF (KSCRN.EQ.1)                                                 SUTERR.......83200
     1      WRITE (*,1888) '         MATRIX SOLVER ERROR          '      SUTERR.......83300
         WRITE (K00,1888) '         MATRIX SOLVER ERROR          '       SUTERR.......83400
         IF (KSCRN.EQ.1) WRITE (*,1311)                                  SUTERR.......83500
         WRITE (K00,1311)                                                SUTERR.......83600
 1311    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......83700
         IF (KSCRN.EQ.1) WRITE (*,1313) ERRCOD, CHERR(2),                SUTERR.......83800
     1      INERR(1), INERR(2), RLERR(1), RLERR(2)                       SUTERR.......83900
         WRITE (K00,1313) ERRCOD, CHERR(2), INERR(1), INERR(2),          SUTERR.......84000
     1      RLERR(1), RLERR(2)                                           SUTERR.......84100
 1313    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......84200
     1           /4X,'Solver:    ',2X,A40                                SUTERR.......84300
     1          //4X,'Error flag..........IERR = ',I3                    SUTERR.......84400
     1           /4X,'# of solver iters...ITRS = ',I5                    SUTERR.......84500
     1           /4X,'Error estimate.......ERR = ',1PE8.1                SUTERR.......84600
     1           /4X,'Error tolerance......TOL = ',1PE8.1/)              SUTERR.......84700
         DO 1315 I=1,50                                                  SUTERR.......84800
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......84900
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......85000
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......85100
 1315    CONTINUE                                                        SUTERR.......85200
         IF (KSCRN.EQ.1) WRITE (*,1321)                                  SUTERR.......85300
         WRITE (K00,1321)                                                SUTERR.......85400
 1321    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......85500
         DO 1325 I=1,50                                                  SUTERR.......85600
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......85700
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......85800
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......85900
 1325    CONTINUE                                                        SUTERR.......86000
      ELSE IF (CODE(1).EQ.'CON') THEN                                    SUTERR.......86100
C........ERROR TYPE 'CON' (CONVERGENCE ERROR)                            SUTERR.......86200
         IF (KSCRN.EQ.1)                                                 SUTERR.......86300
     1      WRITE (*,1888) '          CONVERGENCE ERROR           '      SUTERR.......86400
         WRITE (K00,1888) '         CONVERGENCE ERROR          '         SUTERR.......86500
         IF (KSCRN.EQ.1) WRITE (*,1411)                                  SUTERR.......86600
         WRITE (K00,1411)                                                SUTERR.......86700
 1411    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......86800
         IF (KSCRN.EQ.1) WRITE (*,1413) ERRCOD, CHERR(1), INERR(3),      SUTERR.......86900
     1       RLERR(1), INERR(1), RLERR(2), RLERR(3), INERR(2), RLERR(4)  SUTERR.......87000
         WRITE (K00,1413) ERRCOD, CHERR(1), INERR(3),                    SUTERR.......87100
     1       RLERR(1), INERR(1), RLERR(2), RLERR(3), INERR(2), RLERR(4)  SUTERR.......87200
 1413    FORMAT (/4X,'Error code: ',2X,A40                               SUTERR.......87300
     1           /4X,'Unconverged:',2X,A40                               SUTERR.......87400
     1      //4X,'# of iterations.....ITER = ',I5                        SUTERR.......87500
     1       /4X,'Maximum P change.....RPM = ',1PE14.5,' (node ',I9,')'  SUTERR.......87600
     1       /4X,'Tolerance for P....RPMAX = ',1PE14.5                   SUTERR.......87700
     1       /4X,'Maximum U change.....RUM = ',1PE14.5,' (node ',I9,')'  SUTERR.......87800
     1       /4X,'Tolerance for U....RUMAX = ',1PE14.5/)                 SUTERR.......87900
         DO 1415 I=1,50                                                  SUTERR.......88000
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......88100
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......88200
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......88300
 1415    CONTINUE                                                        SUTERR.......88400
         IF (KSCRN.EQ.1) WRITE (*,1421)                                  SUTERR.......88500
         WRITE (K00,1421)                                                SUTERR.......88600
 1421    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......88700
         DO 1425 I=1,50                                                  SUTERR.......88800
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......88900
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......89000
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......89100
 1425    CONTINUE                                                        SUTERR.......89200
      ELSE IF ((CODE(1).EQ.'REA').AND.                                   SUTERR.......89300
     1         ((CODE(2).EQ.'INP').OR.(CODE(2).EQ.'ICS'))) THEN          SUTERR.......89400
C........ERROR TYPE 'REA-INP' OR 'REA-ICS' (FORTRAN READ ERROR)          SUTERR.......89500
         IF (KSCRN.EQ.1)                                                 SUTERR.......89600
     1      WRITE (*,1888) '          FORTRAN READ ERROR          '      SUTERR.......89700
         WRITE (K00,1888) '          FORTRAN READ ERROR          '       SUTERR.......89800
         IF (KSCRN.EQ.1) WRITE (*,1511)                                  SUTERR.......89900
         WRITE (K00,1511)                                                SUTERR.......90000
 1511    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......90100
         IF (CODE(2).EQ.'INP') THEN                                      SUTERR.......90200
            CDUM80 = FNAME(1)                                            SUTERR.......90300
         ELSE                                                            SUTERR.......90400
            CDUM80 = FNAME(2)                                            SUTERR.......90500
         END IF                                                          SUTERR.......90600
         IF (((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')).OR.               SUTERR.......90700
     1       (CODE(3).EQ.'INS')) THEN                                    SUTERR.......90800
           IF (KSCRN.EQ.1) WRITE (*,1513) ERRCOD, CDUM80, INERR(1)       SUTERR.......90900
           WRITE (K00,1513) ERRCOD, CDUM80, INERR(1)                     SUTERR.......91000
 1513      FORMAT (/4X,'Error code:',2X,A40                              SUTERR.......91100
     1             /4X,'File:      ',2X,A40                              SUTERR.......91200
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR.......91300
         ELSE                                                            SUTERR.......91400
           IF (KSCRN.EQ.1) WRITE (*,1514) ERRCOD, CDUM80, CODE(3)(1:3),  SUTERR.......91500
     1        INERR(1)                                                   SUTERR.......91600
           WRITE (K00,1514) ERRCOD, CDUM80, CODE(3)(1:3), INERR(1)       SUTERR.......91700
 1514      FORMAT (/4X,'Error code:',2X,A40                              SUTERR.......91800
     1             /4X,'File:      ',2X,A40                              SUTERR.......91900
     1             /4X,'Dataset:   ',2X,A3                               SUTERR.......92000
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR.......92100
         END IF                                                          SUTERR.......92200
         DO 1515 I=1,50                                                  SUTERR.......92300
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......92400
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......92500
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......92600
 1515    CONTINUE                                                        SUTERR.......92700
         IF (KSCRN.EQ.1) WRITE (*,1521)                                  SUTERR.......92800
         WRITE (K00,1521)                                                SUTERR.......92900
 1521    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......93000
         DO 1525 I=1,50                                                  SUTERR.......93100
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......93200
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......93300
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......93400
 1525    CONTINUE                                                        SUTERR.......93500
         IF (KSCRN.EQ.1) WRITE (*,1581)                                  SUTERR.......93600
         WRITE (K00,1581)                                                SUTERR.......93700
 1581    FORMAT (/1X,'GENERAL NOTE'/                                     SUTERR.......93800
     1     /4X,'If the dataset for which SUTRA has reported an error'    SUTERR.......93900
     1     /4X,'appears to be correct, check the preceding lines'        SUTERR.......94000
     1     /4X,'for missing data or extraneous characters.')             SUTERR.......94100
      ELSE IF ((CODE(1).EQ.'REA').AND.(CODE(2).EQ.'FIL')) THEN           SUTERR.......94200
C........ERROR TYPE 'REA-FIL' (FORTRAN READ ERROR)                       SUTERR.......94300
         IF (KSCRN.EQ.1)                                                 SUTERR.......94400
     1      WRITE (*,1888) '          FORTRAN READ ERROR          '      SUTERR.......94500
         WRITE (K00,1888) '          FORTRAN READ ERROR          '       SUTERR.......94600
         IF (KSCRN.EQ.1) WRITE (*,1611)                                  SUTERR.......94700
         WRITE (K00,1611)                                                SUTERR.......94800
 1611    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......94900
         IF (KSCRN.EQ.1) WRITE (*,1613) ERRCOD, INERR(1)                 SUTERR.......95000
         WRITE (K00,1613) ERRCOD, INERR(1)                               SUTERR.......95100
 1613    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......95200
     1           /4X,'File:      ',2X,'SUTRA.FIL'                        SUTERR.......95300
     1          //4X,'Error status flag.....IOSTAT = ',I5/)              SUTERR.......95400
         DO 1615 I=1,50                                                  SUTERR.......95500
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......95600
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......95700
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......95800
 1615    CONTINUE                                                        SUTERR.......95900
         IF (KSCRN.EQ.1) WRITE (*,1621)                                  SUTERR.......96000
         WRITE (K00,1621)                                                SUTERR.......96100
 1621    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......96200
         DO 1625 I=1,50                                                  SUTERR.......96300
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......96400
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......96500
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......96600
 1625    CONTINUE                                                        SUTERR.......96700
      END IF                                                             SUTERR.......96800
 1888 FORMAT (                                                           SUTERR.......96900
     1   /1X,'+--------+',38('-'),'+--------+'                           SUTERR.......97000
     1   /1X,'| \\  // |',38('-'),'| \\  // |'                           SUTERR.......97100
     1   /1X,'|  \\//  |',38(' '),'|  \\//  |'                           SUTERR.......97200
     1   /1X,'|   //   |',A38     '|   //   |'                           SUTERR.......97300
     1   /1X,'|  //\\  |',38(' '),'|  //\\  |'                           SUTERR.......97400
     1   /1X,'| //  \\ |',38('-'),'| //  \\ |'                           SUTERR.......97500
     1   /1X,'+--------+',38('-'),'+--------+')                          SUTERR.......97600
C                                                                        SUTERR.......97700
C.....WRITE RUN TERMINATION MESSAGES AND CALL TERMINATION SEQUENCE       SUTERR.......97800
      IF (KSCRN.EQ.1) WRITE (*,8888)                                     SUTERR.......97900
      WRITE (K00,8888)                                                   SUTERR.......98000
      IF (K3.NE.-1) WRITE (K3,8889)                                      SUTERR.......98100
      IF (K5.NE.-1) WRITE (K5,8889)                                      SUTERR.......98200
      IF (K6.NE.-1) WRITE (K6,8889)                                      SUTERR.......98300
 8888 FORMAT (/1X,'+',56('-'),'+'/1X,'| ',54X,' |'/1X,'|',3X,            SUTERR.......98400
     1   8('*'),3X,'RUN TERMINATED DUE TO ERROR',3X,9('*'),              SUTERR.......98500
     1   3X,'|'/1X,'| ',54X,' |'/1X,'+',56('-'),'+')                     SUTERR.......98600
 8889 FORMAT (//13X,'+',56('-'),'+'/13X,'| ',54X,' |'/13X,'|',3X,        SUTERR.......98700
     1   8('*'),3X,'RUN TERMINATED DUE TO ERROR',3X,9('*'),              SUTERR.......98800
     1   3X,'|'/13X,'| ',54X,' |'/13X,'+',56('-'),'+')                   SUTERR.......98900
      IF (KSCRN.EQ.1) WRITE (*,8890)                                     SUTERR.......99000
 8890 FORMAT (/' The above error message also appears in the SMY file,'  SUTERR.......99100
     1        /' which may contain additional error information.')       SUTERR.......99200
      CALL TERSEQ()                                                      SUTERR.......99300
C                                                                        SUTERR.......99400
      RETURN                                                             SUTERR.......99500
      END                                                                SUTERR.......99600
C                                                                        SUTERR.......99700
C     SUBROUTINE        S  U  T  R  A              SUTRA VERSION 2.1     SUTRA..........100
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
     6   IN,IQSOP,IQSOU,IPBC,IUBC,OBSPTS,NREG,LREG,IWK,IA,JA,            SUTRA.........1400
     7   IQSOPT,IQSOUT,IPBCT,IUBCT,IERROR)                                      SUTRA.........1500
!     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,FWK,B,       SUTRA.........1300
      USE ALLARR, ONLY : OBSDAT                                          SUTRA.........1600
      USE LLDEF                                                          SUTRA.........1700
      USE EXPINT                                                         SUTRA.........1800
      USE SCHDEF                                                         SUTRA.........1900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTRA.........2000
      PARAMETER (NCOLMX=9)                                               SUTRA.........2100
      CHARACTER*8 VERNUM, VERNIN                                         SUTRA.........2200
      CHARACTER*1 TITLE1(80),TITLE2(80)                                  SUTRA.........2300
      CHARACTER*10 ADSMOD                                                SUTRA.........2400
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8),CDUM80              SUTRA.........2500
      CHARACTER*40 SOLNAM(0:10)                                          SUTRA.........2600
      CHARACTER*10 SOLWRD(0:10)                                          SUTRA.........2700
      LOGICAL PRNALL,PRN0,PRNDEF,PRNK3,PRNK5,PRNK6,PRNK78                SUTRA.........2800
      LOGICAL SCHSTP, TSPRTD                                             SUTRA.........2900
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                SUTRA.........3000
      DIMENSION INERR(10),RLERR(10)                                      SUTRA.........3100
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             SUTRA.........3200
!      DIMENSION PMAT(NELT,NCBI),UMAT(NELT,NCBI)                          SUTRA.........3300
      DIMENSION PITER(NN),UITER(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN), SUTRA.........3400
     1   PVEL(NN),SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),       SUTRA.........3500
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),       SUTRA.........3600
     3   QIN(NN),QINITR(NN),UIN(NN),QUIN(NN),RCIT(NN),RCITM1(NN)         SUTRA.........3700
      DIMENSION PVEC(NNVEC),UVEC(NNVEC)                                  SUTRA.........3800
      DIMENSION ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),        SUTRA.........3900
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),          SUTRA.........4000
     2   PANGL1(NE)                                                      SUTRA.........4100
      DIMENSION ALMID(NEX),ATMID(NEX),                                   SUTRA.........4200
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),                 SUTRA.........4300
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX)                 SUTRA.........4400
      DIMENSION PBC(NBCN),UBC(NBCN),QPLITR(NBCN)                         SUTRA.........4500
      DIMENSION GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48)                  SUTRA.........4600
!      DIMENSION FWK(NWF),B(NNNX)                                         SUTRA.........4700
      DIMENSION B(NNNX)                                                  SUTRA.........4700
      DIMENSION IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),   SUTRA.........4800
     1   NREG(NN),LREG(NE),IWK(NWI),IA(NDIMIA),JA(NDIMJA)                SUTRA.........4900
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         SUTRA.........5000
      DIMENSION KTYPE(2)                                                 SUTRA.........5100
      TYPE (LLD), POINTER :: DENTS                                       SUTRA.........5200
      TYPE (LLD), ALLOCATABLE :: DENOB(:)                                SUTRA.........5300
      DIMENSION LCNT(NFLOMX)                                             SUTRA.........5400
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SUTRA.........5500
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SUTRA.........5600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTRA.........5700
     1   NSOP,NSOU,NBCN                                                  SUTRA.........5800
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        SUTRA.........5900
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        SUTRA.........6000
      COMMON /FNAMES/ UNAME,FNAME                                        SUTRA.........6100
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SUTRA.........6200
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      SUTRA.........6300
      COMMON /ITSOLR/ TOLP,TOLU                                          SUTRA.........6400
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             SUTRA.........6500
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTRA.........6600
     1   KSCRN,KPAUSE                                                    SUTRA.........6700
      COMMON /MODSOR/ ADSMOD                                             SUTRA.........6800
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      SUTRA.........6900
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SUTRA.........7000
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SUTRA.........7100
      COMMON /PLT1/ ONCEK5, ONCEK6, ONCEK7, ONCEK8                       SUTRA.........7200
      COMMON /SCH/ NSCH,ISCHTS                                           SUTRA.........7300
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       SUTRA.........7400
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           SUTRA.........7500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SUTRA.........7600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  SUTRA.........7700
      COMMON /VER/ VERNUM, VERNIN                                        SUTRA.........7800
C                                                                        SUTRA.........7900
C.....WRITE TITLE TO CONSOLE                                             SUTRA.........8000
      DO 100 I=80,1,-1                                                   SUTRA.........8100
         IF (TITLE1(I).NE.' ') THEN                                      SUTRA.........8200
            LENT1 = I                                                    SUTRA.........8300
            GOTO 101                                                     SUTRA.........8400
         END IF                                                          SUTRA.........8500
  100 CONTINUE                                                           SUTRA.........8600
      LENT1 = 1                                                          SUTRA.........8700
  101 DO 105 I=80,1,-1                                                   SUTRA.........8800
         IF (TITLE2(I).NE.' ') THEN                                      SUTRA.........8900
            LENT2 = I                                                    SUTRA.........9000
            GOTO 106                                                     SUTRA.........9100
         END IF                                                          SUTRA.........9200
  105 CONTINUE                                                           SUTRA.........9300
      LENT2 = 1                                                          SUTRA.........9400
  106 CONTINUE                                                           SUTRA.........9500
!      IF (KSCRN.EQ.1) WRITE (*,121) VERNUM                               SUTRA.........9600
!      WRITE (K00,121) VERNUM                                             SUTRA.........9700
!  121 FORMAT (/1X,9X,53("=")//1X,25X,"S    U    T    R    A",//          SUTRA.........9800
!     1   31X,"Version ",A8//1X,9X,53("=")/)                              SUTRA.........9900
!      IF (KSCRN.EQ.1) WRITE (*,122) (TITLE1(I),I=1,LENT1)                SUTRA........10000
!      WRITE (K00,122) (TITLE1(I),I=1,LENT1)                              SUTRA........10100
!      IF (KSCRN.EQ.1) WRITE (*,122) (TITLE2(I),I=1,LENT2)                SUTRA........10200
!      WRITE (K00,122) (TITLE2(I),I=1,LENT2)                              SUTRA........10300
!  122 FORMAT (1X,80A1)                                                   SUTRA........10400
!      IF (KSCRN.EQ.1) WRITE (*,*)                                        SUTRA........10500
!      WRITE (K00,*)                                                      SUTRA........10600
C                                                                        SUTRA........10700
C.....DETERMINE ACTUAL NUMBER OF TIME STEPS AND DURATION FROM TIME       SUTRA........10800
C        STEP SCHEDULE.                                                  SUTRA........10900
      IF (ISSTRA.EQ.0) THEN                                              SUTRA........11000
         DENTS => SCHDLS(ISCHTS)%SLIST                                   SUTRA........11100
         DO 310 K=0,ITMAX                                                SUTRA........11200
            TMAX = DENTS%DVALU1                                          SUTRA........11300
            DENTS => DENTS%NENT                                          SUTRA........11400
  310    CONTINUE                                                        SUTRA........11500
      ELSE                                                               SUTRA........11600
         TMAX = TSTART                                                   SUTRA........11700
      END IF                                                             SUTRA........11800
      TEMAX = TMAX - TSTART                                              SUTRA........11900
C                                                                        SUTRA........12000
C.....INITIALIZE TIME STEP NUMBER AND SCHEDULE POINTERS                  SUTRA........12100
      IT=0                                                               SUTRA........12200
      DELTLC = DELT                                                      SUTRA........12300
      DENTS => SCHDLS(ISCHTS)%SLIST                                      SUTRA........12400
      ALLOCATE(DENOB(NFLOMX))                                            SUTRA........12500
      DO 400 NFLO=1,NFLOMX                                               SUTRA........12600
         DENOB(NFLO)%NENT => SCHDLS(OFP(NFLO)%ISCHED)%SLIST              SUTRA........12700
         LCNT(NFLO) = 1                                                  SUTRA........12800
  400 CONTINUE                                                           SUTRA........12900
C                                                                        SUTRA........13000
C.....SET FLAG FOR TIME-DEPENDENT SOURCES OR BOUNDARY CONDITIONS.        SUTRA........13100
C        WHEN IBCT=+4, THERE ARE NO TIME-DEPENDENT SPECIFICATIONS.       SUTRA........13200
      IBCT=IQSOPT+IQSOUT+IPBCT+IUBCT                                     SUTRA........13300
C                                                                        SUTRA........13400
C.....SET STARTING TIME OF SIMULATION CLOCK                              SUTRA........13500
C     TSEC=TSTART                                                        SUTRA........13600
      TSECP0=TSEC                                                        SUTRA........13700
      TSECU0=TSEC                                                        SUTRA........13800
      TMIN=TSEC/60.D0                                                    SUTRA........13900
      THOUR=TMIN/60.D0                                                   SUTRA........14000
      TDAY=THOUR/24.D0                                                   SUTRA........14100
      TWEEK=TDAY/7.D0                                                    SUTRA........14200
      TMONTH=TDAY/30.4375D0                                              SUTRA........14300
      TYEAR=TDAY/365.25D0                                                SUTRA........14400
C                                                                        SUTRA........14500
C.....OUTPUT INITIAL/STARTING CONDITIONS FOR TRANSIENT TRANSPORT         SUTRA........14600
      IF(ISSTRA.NE.1) THEN                                               SUTRA........14700
C........PRINT TO LST OUTPUT FILE                                        SUTRA........14800
!         IF (KTYPE(1).EQ.3) THEN                                         SUTRA........14900
!            CALL OUTLST3(0,0,0,0,0,0D0,0,0,0D0,PVEC,UVEC,VMAG,VANG1,     SUTRA........15000
!     1         VANG2,SW)                                                 SUTRA........15100
!         ELSE                                                            SUTRA........15200
!            CALL OUTLST2(0,0,0,0,0,0D0,0,0,0D0,PVEC,UVEC,VMAG,VANG1,SW)  SUTRA........15300
!         END IF                                                          SUTRA........15400
C........IF TRANSIENT FLOW, PRINT TO NODEWISE AND OBSERVATION OUTPUT     SUTRA........15500
C           FILES NOW.  (OTHERWISE, WAIT UNTIL STEADY-STATE FLOW         SUTRA........15600
C           SOLUTION IS COMPUTED.)                                       SUTRA........15700
         IF (ISSFLO.EQ.0) THEN                                           SUTRA........15800
!            IF (K5.NE.-1)                                                SUTRA........15900
!     1         CALL OUTNOD(PVEC,UVEC,SW,X,Y,Z,TITLE1,TITLE2)             SUTRA........16000
            DO 650 NFLO=1,NFLOMX                                         SUTRA........16100
               IF (IUNIO(NFLO).NE.-1) THEN                               SUTRA........16200
!                  IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                      SUTRA........16300
!                     CALL OUTOBS(NFLO,OBSPTS,TSTART,0D0,PM1,UM1,         SUTRA........16400
!     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG)                 SUTRA........16500
!                  ELSE                                                   SUTRA........16600
!                     CALL OUTOBC(NFLO,OBSPTS,TSTART,0D0,PM1,UM1,         SUTRA........16700
!     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG)                 SUTRA........16800
!                  END IF                                                 SUTRA........16900
                  STEP = DENOB(NFLO)%NENT%DVALU2                         SUTRA........17000
                  LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                 SUTRA........17100
                  IF ((STEP.EQ.0D0).AND.(LCNT(NFLO).LT.LENSCH)) THEN     SUTRA........17200
                     DENOB(NFLO)%NENT => DENOB(NFLO)%NENT%NENT           SUTRA........17300
                     LCNT(NFLO) = LCNT(NFLO) + 1                         SUTRA........17400
                  END IF                                                 SUTRA........17500
               END IF                                                    SUTRA........17600
  650       CONTINUE                                                     SUTRA........17700
         END IF                                                          SUTRA........17800
      END IF                                                             SUTRA........17900
C                                                                        SUTRA........18000
C.....SET SWITCHES AND PARAMETERS FOR SOLUTION WITH STEADY-STATE FLOW    SUTRA........18100
      IF(ISSFLO.NE.1) GOTO 1000                                          SUTRA........18200
      ML=1                                                               SUTRA........18300
      NOUMAT=0                                                           SUTRA........18400
      ISSFLO=2                                                           SUTRA........18500
      ITER=0                                                             SUTRA........18600
      DLTPM1=DELTP                                                       SUTRA........18700
      DLTUM1=DELTU                                                       SUTRA........18800
      BDELP1 = 1D0                                                       SUTRA........18900
      BDELP=0.0D0                                                        SUTRA........19000
      BDELU=0.0D0                                                        SUTRA........19100
      IF (ISSTRA.NE.0) THEN                                              SUTRA........19200
!         IF (KSCRN.EQ.1) WRITE (*,902) IT, ITMAX                         SUTRA........19300
!         WRITE (K00,902) IT, ITMAX                                       SUTRA........19400
      ELSE                                                               SUTRA........19500
         TELAPS = TSEC - TSTART                                          SUTRA........19600
!         IF (KSCRN.EQ.1) WRITE (*,903) IT, ITMAX, TELAPS, TEMAX          SUTRA........19700
!         WRITE (K00,903) IT, ITMAX, TSEC, TEMAX                          SUTRA........19800
      END IF                                                             SUTRA........19900
!  902 FORMAT (1X, 'TIME STEP ', I8, ' OF ', I8)                          SUTRA........20000
!  903 FORMAT (1X, 'TIME STEP ', I8, ' OF ', I8, ';',                     SUTRA........20100
!     1        3X, 'ELAPSED TIME: ', 1PE11.4, ' OF ', 1PE11.4, ' [s]')    SUTRA........20200
      GOTO 1100                                                          SUTRA........20300
C                                                                        SUTRA........20400
C                                                                        SUTRA........20500
C ********************************************************************** SUTRA........20600
C.....BEGIN TIME STEP ************************************************** SUTRA........20700
C ********************************************************************** SUTRA........20800
C.....INCREMENT TIME STEP NUMBER                                         SUTRA........20900
 1000 IT=IT+1                                                            SUTRA........21000
      DIT = DNINT(DBLE(IT))                                              SUTRA........21100
      DENTS => DENTS%NENT                                                SUTRA........21200
      ITER=0                                                             SUTRA........21300
      ML=0                                                               SUTRA........21400
      NOUMAT=0                                                           SUTRA........21500
C.....SET NOUMAT TO OBTAIN U SOLUTION BY SIMPLE BACK SUBSTITUTION        SUTRA........21600
C        BEGINNING ON SECOND TIME STEP AFTER A PRESSURE SOLUTION         SUTRA........21700
C        IF THE SOLUTION IS NON-ITERATIVE (ITRMAX=1)                     SUTRA........21800
      IF(MOD(IT-1,NPCYC).NE.0.AND.MOD(IT,NPCYC).NE.0.AND.IT.GT.2         SUTRA........21900
     1   .AND.ITRMAX.EQ.1) NOUMAT=1                                      SUTRA........22000
C.....CHOOSE SOLUTION VARIABLE ON THIS TIME STEP:                        SUTRA........22100
C        ML=0 FOR P AND U, ML=1 FOR P ONLY, AND ML=2 FOR U ONLY.         SUTRA........22200
      IF(IT.EQ.1.AND.ISSFLO.NE.2) GOTO 1005                              SUTRA........22300
      IF(MOD(IT,NPCYC).NE.0) ML=2                                        SUTRA........22400
      IF(MOD(IT,NUCYC).NE.0) ML=1                                        SUTRA........22500
C.....INCREMENT SIMULATION CLOCK, TSEC, TO END OF NEW TIME STEP          SUTRA........22600
 1005 TSECM1 = TSEC                                                      SUTRA........22700
      TSEC = DENTS%DVALU1                                                SUTRA........22800
      TMIN=TSEC/60.D0                                                    SUTRA........22900
      THOUR=TMIN/60.D0                                                   SUTRA........23000
      TDAY=THOUR/24.D0                                                   SUTRA........23100
      TWEEK=TDAY/7.D0                                                    SUTRA........23200
      TMONTH=TDAY/30.4375D0                                              SUTRA........23300
      TYEAR=TDAY/365.25D0                                                SUTRA........23400
C.....UPDATE TIME STEP SIZE                                              SUTRA........23500
      DELTM1 = DELT                                                      SUTRA........23600
      DELT = TSEC - TSECM1                                               SUTRA........23700
C.....NO SIMPLE BACK SUBSTITUTION FOR U IF TIME STEP HAS CHANGED         SUTRA........23800
C        BY MORE THAN A VERY SMALL TOLERANCE                             SUTRA........23900
      RELCHG = DABS((DELT - DELTLC)/DELTLC)                              SUTRA........24000
      IF (RELCHG.GT.1D-14) THEN                                          SUTRA........24100
         DELTLC = DELT                                                   SUTRA........24200
         NOUMAT = 0                                                      SUTRA........24300
      END IF                                                             SUTRA........24400
C                                                                        SUTRA........24500
C.....WRITE TIME STEP NUMBER AND ELAPSED TIME                            SUTRA........24600
      IF (ISSTRA.NE.0) THEN                                              SUTRA........24700
!         IF (KSCRN.EQ.1) WRITE (*,902) IT, ITMAX                         SUTRA........24800
!         WRITE (K00,902) IT, ITMAX                                       SUTRA........24900
      ELSE                                                               SUTRA........25000
         TELAPS = TSEC - TSTART                                          SUTRA........25100
!         IF (KSCRN.EQ.1) WRITE (*,903) IT, ITMAX, TELAPS, TEMAX          SUTRA........25200
!         WRITE (K00,903) IT, ITMAX, TSEC, TEMAX                          SUTRA........25300
      END IF                                                             SUTRA........25400
C                                                                        SUTRA........25500
C.....SET TIME STEP (DELTP AND/OR DELTU) AND INCREMENT CLOCK             SUTRA........25600
C        FOR WHICHEVER OF P AND/OR U ARE SOLVED FOR ON THIS TIME STEP    SUTRA........25700
      IF(ML-1) 1010,1020,1030                                            SUTRA........25800
 1010 DLTPM1=DELTP                                                       SUTRA........25900
      DLTUM1=DELTU                                                       SUTRA........26000
      DELTP=TSEC-TSECP0                                                  SUTRA........26100
      DELTU=TSEC-TSECU0                                                  SUTRA........26200
      TSECP0=TSEC                                                        SUTRA........26300
      TSECU0=TSEC                                                        SUTRA........26400
      GOTO 1040                                                          SUTRA........26500
 1020 DLTPM1=DELTP                                                       SUTRA........26600
      DELTP=TSEC-TSECP0                                                  SUTRA........26700
      TSECP0=TSEC                                                        SUTRA........26800
      GOTO 1040                                                          SUTRA........26900
 1030 DLTUM1=DELTU                                                       SUTRA........27000
      DELTU=TSEC-TSECU0                                                  SUTRA........27100
      TSECU0=TSEC                                                        SUTRA........27200
 1040 CONTINUE                                                           SUTRA........27300
C.....SET PROJECTION FACTORS USED ON FIRST ITERATION TO EXTRAPOLATE      SUTRA........27400
C        AHEAD ONE-HALF TIME STEP                                        SUTRA........27500
      BDELP=(DELTP/DLTPM1)*0.50D0                                        SUTRA........27600
      BDELU=(DELTU/DLTUM1)*0.50D0                                        SUTRA........27700
      BDELP1=BDELP+1.0D0                                                 SUTRA........27800
      BDELU1=BDELU+1.0D0                                                 SUTRA........27900
C                                                                        SUTRA........28000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........28100
C.....BEGIN ITERATION - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........28200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........28300
C.....INCREMENT ITERATION NUMBER                                         SUTRA........28400
 1100 ITER=ITER+1                                                        SUTRA........28500
C.....IF ITERATIVE SOLUTION, WRITE ITERATION NUMBER                      SUTRA........28600
!      IF (ITRMAX.NE.1) THEN                                              SUTRA........28700
!         IF (KSCRN.EQ.1) WRITE (*,1104) ITER                             SUTRA........28800
!         WRITE (K00,1104) ITER                                           SUTRA........28900
!      END IF                                                             SUTRA........29000
! 1104 FORMAT (1X, 3X, 'NON-LINEARITY ITERATION ', I5)                    SUTRA........29100
C                                                                        SUTRA........29200
!      IF(ML-1) 2000,2200,2400                                            SUTRA........29300
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH BOTH P AND U SOLUTIONS    SUTRA........29400
! 2000 DO 2025 I=1,NN                                                     SUTRA........29500
C.....SET DPDT-ITERATE TO VALUE FROM PREVIOUS ITERATION FOR PRESSURE     SUTRA........29600
C      COMING FROM THIS TIME STEP                                        SUTRA........29700
C       (THIS IS OVERWRITTEN ON THE FIRST ITERATION JUST BELOW)          SUTRA........29800
C     NOTE: DPDTITR IS USED ONLY IN THE BUDGET                           SUTRA........29900
!      DPDTITR(I)=(PVEC(I)-PM1(I))/DELTP                                  SUTRA........30000
!      PITER(I)=PVEC(I)                                                   SUTRA........30100
!      PVEL(I)=PVEC(I)                                                    SUTRA........30200
!      UITER(I)=UVEC(I)                                                   SUTRA........30300
!      RCITM1(I)=RCIT(I)                                                  SUTRA........30400
! 2025 RCIT(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                              SUTRA........30500
!      DO 2050 IP=1,NPBC                                                  SUTRA........30600
!      I=IABS(IPBC(IP))                                                   SUTRA........30700
!      QPLITR(IP)=GNUP*(PBC(IP)-PITER(I))                                 SUTRA........30800
! 2050 CONTINUE                                                           SUTRA........30900
C.....QINITR VALUE DIFFERS FROM QIN ONLY IF BCTIME CHANGED QIN           SUTRA........31000
!      IF (ITER.LE.2) THEN                                                SUTRA........31100
!         DO 2060 I=1,NN                                                  SUTRA........31200
! 2060    QINITR(I)=QIN(I)                                                SUTRA........31300
!      END IF                                                             SUTRA........31400
!      IF(ITER.GT.1) GOTO 2600                                            SUTRA........31500
!      DO 2075 I=1,NN                                                     SUTRA........31600
!      PITER(I)=BDELP1*PVEC(I)-BDELP*PM1(I)                               SUTRA........31700
!      UITER(I)=BDELU1*UVEC(I)-BDELU*UM1(I)                               SUTRA........31800
C.....RESETS DPDT-ITERATE TO VALUE FROM MOST RECENT PRESSURE TIME STEP   SUTRA........31900
C      ON THE FIRST ITERATION FOR THIS TIME STEP                         SUTRA........32000
!      DPDTITR(I)=(PVEC(I)-PM1(I))/DLTPM1                                 SUTRA........32100
!      PM1(I)=PVEC(I)                                                     SUTRA........32200
!      UM2(I)=UM1(I)                                                      SUTRA........32300
! 2075 UM1(I)=UVEC(I)                                                     SUTRA........32400
!      GOTO 2600                                                          SUTRA........32500
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH P SOLUTION ONLY           SUTRA........32600
! 2200 DO 2225 I=1,NN                                                     SUTRA........32700
!      PVEL(I)=PVEC(I)                                                    SUTRA........32800
! 2225 PITER(I)=PVEC(I)                                                   SUTRA........32900
!      IF(ITER.GT.1) GOTO 2600                                            SUTRA........33000
!      DO 2250 I=1,NN                                                     SUTRA........33100
!      PITER(I)=BDELP1*PVEC(I)-BDELP*PM1(I)                               SUTRA........33200
!      UITER(I)=UVEC(I)                                                   SUTRA........33300
!      RCITM1(I)=RCIT(I)                                                  SUTRA........33400
!      RCIT(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                              SUTRA........33500
! 2250 PM1(I)=PVEC(I)                                                     SUTRA........33600
!      GOTO 2600                                                          SUTRA........33700
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH U SOLUTION ONLY           SUTRA........33800
! 2400 IF (ITER.EQ.1) THEN                                                SUTRA........33900
!         DO 2405 I=1,NN                                                  SUTRA........34000
! 2405       UITER(I)=BDELU1*UVEC(I)-BDELU*UM1(I)                         SUTRA........34100
!      ELSE                                                               SUTRA........34200
!         DO 2410 I=1,NN                                                  SUTRA........34300
! 2410       UITER(I)=UVEC(I)                                             SUTRA........34400
!      END IF                                                             SUTRA........34500
!      IF(NOUMAT.EQ.1) GOTO 2480                                          SUTRA........34600
C.....SET PARAMETERS FROM MOST RECENT PRESSURE TIME STEP                 SUTRA........34700
!      IF(ITER.GT.1) GOTO 2600                                            SUTRA........34800
!      DO 2450 I=1,NN                                                     SUTRA........34900
!      DPDTITR(I)=(PVEC(I)-PM1(I))/DELTP                                  SUTRA........35000
!      QINITR(I)=QIN(I)                                                   SUTRA........35100
!      PITER(I)=PVEC(I)                                                   SUTRA........35200
!      PVEL(I)=PVEC(I)                                                    SUTRA........35300
! 2450 RCITM1(I)=RCIT(I)                                                  SUTRA........35400
!      DO 2475 IP=1,NPBC                                                  SUTRA........35500
!      I=IABS(IPBC(IP))                                                   SUTRA........35600
!      QPLITR(IP)=GNUP*(PBC(IP)-PITER(I))                                 SUTRA........35700
! 2475 CONTINUE                                                           SUTRA........35800
! 2480 DO 2500 I=1,NN                                                     SUTRA........35900
!      UM2(I)=UM1(I)                                                      SUTRA........36000
! 2500 UM1(I)=UVEC(I)                                                     SUTRA........36100
! 2600 CONTINUE                                                           SUTRA........36200
C                                                                        SUTRA........36300
C.....INITIALIZE ARRAYS WITH VALUE OF ZERO                               SUTRA........36400
!      MATDIM=NELT*NCBI                                                   SUTRA........36500
!      IF(ML-1) 3000,3000,3300                                            SUTRA........36600
! 3000 CALL ZERO(PMAT,MATDIM,0.0D0)                                       SUTRA........36700
!      CALL ZERO(PVEC,NNVEC,0.0D0)                                        SUTRA........36800
!      CALL ZERO(VOL,NN,0.0D0)                                            SUTRA........36900
!      IF(ML-1) 3300,3400,3300                                            SUTRA........37000
! 3300 IF(NOUMAT) 3350,3350,3375                                          SUTRA........37100
! 3350 CALL ZERO(UMAT,MATDIM,0.0D0)                                       SUTRA........37200
! 3375 CALL ZERO(UVEC,NNVEC,0.0D0)                                        SUTRA........37300
! 3400 CONTINUE                                                           SUTRA........37400
C                                                                        SUTRA........37500
C.....SET TIME-DEPENDENT BOUNDARY CONDITIONS, SOURCES AND SINKS          SUTRA........37600
C        FOR THIS TIME STEP                                              SUTRA........37700
!      IF(ITER.EQ.1.AND.IBCT.NE.4)                                        SUTRA........37800
!     1   CALL BCTIME(IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,IQSOU,         SUTRA........37900
!     2   IPBCT,IUBCT,IQSOPT,IQSOUT,X,Y,Z)                                SUTRA........38000
C                                                                        SUTRA........38100
C.....SET SORPTION PARAMETERS FOR THIS TIME STEP                         SUTRA........38200
!      IF(ML.NE.1.AND.ME.EQ.-1.AND.NOUMAT.EQ.0.AND.                       SUTRA........38300
!     1   ADSMOD.NE.'NONE      ') CALL ADSORB(CS1,CS2,CS3,SL,SR,UITER)    SUTRA........38400
C                                                                        SUTRA........38500
C.....DO ELEMENTWISE CALCULATIONS IN MATRIX EQUATION FOR P AND/OR U      SUTRA........38600
!      IF (NOUMAT.EQ.0) THEN                                              SUTRA........38700
!       IF (KTYPE(1).EQ.3) THEN                                           SUTRA........38800
C..... 3D PROBLEM                                                        SUTRA........38900
!       CALL ELEMN3(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,              SUTRA........39000
!     2   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,                            SUTRA........39100
!     3   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA........39200
!     4   PANGL1,PANGL2,PANGL3,VMAG,VANG1,VANG2,VOL,PMAT,PVEC,            SUTRA........39300
!     5   UMAT,UVEC,GXSI,GETA,GZET,PVEL,LREG,IA,JA)                       SUTRA........39400
!       ELSE                                                              SUTRA........39500
C..... 2D PROBLEM                                                        SUTRA........39600
!       CALL ELEMN2(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,              SUTRA........39700
!     2   ALMAX,ALMIN,ATMAX,ATMIN,PERMXX,PERMXY,PERMYX,PERMYY,PANGL1,     SUTRA........39800
!     3   VMAG,VANG1,VOL,PMAT,PVEC,UMAT,UVEC,GXSI,GETA,PVEL,LREG,IA,JA)   SUTRA........39900
!       END IF                                                            SUTRA........40000
!      END IF                                                             SUTRA........40100
C                                                                        SUTRA........40200
C.....DO NODEWISE CALCULATIONS IN MATRIX EQUATION FOR P AND/OR U         SUTRA........40300
!      CALL NODAL(ML,VOL,PMAT,PVEC,UMAT,UVEC,PITER,UITER,PM1,UM1,UM2,     SUTRA........40400
!     1   POR,QIN,UIN,QUIN,QINITR,CS1,CS2,CS3,SL,SR,SW,DSWDP,RHO,SOP,     SUTRA........40500
!     2   NREG,JA)                                                        SUTRA........40600
C                                                                        SUTRA........40700
C.....SET SPECIFIED P AND U CONDITIONS IN MATRIX EQUATION FOR P AND/OR U SUTRA........40800
!      CALL BC(ML,PMAT,PVEC,UMAT,UVEC,IPBC,PBC,IUBC,UBC,QPLITR,JA)        SUTRA........40900
C                                                                        SUTRA........41000
C.....MATRIX EQUATION FOR P AND/OR U COMPLETE.  SOLVE EQUATIONS:         SUTRA........41100
C        WITH DIRECT SOLVER,                                             SUTRA........41200
C           WHEN KMT=0, DECOMPOSE AND BACK-SUBSTITUTE,                   SUTRA........41300
C           WHEN KMT=2, BACK-SUBSTITUTE ONLY.                            SUTRA........41400
C        KPU=1 WHEN SOLVING FOR P,                                       SUTRA........41500
C        KPU=2 WHEN SOLVING FOR U.                                       SUTRA........41600
!      IHALFB=NBHALF-1                                                    SUTRA........41700
!      IERRP = 0                                                          SUTRA........41800
!      IERRU = 0                                                          SUTRA........41900
!      IF(ML-1) 5000,5000,5500                                            SUTRA........42000
C                                                                        SUTRA........42100
C.....SOLVE FOR P                                                        SUTRA........42200
! 5000 KMT=000000                                                         SUTRA........42300
!      KPU=1                                                              SUTRA........42400
!      KSOLVR = KSOLVP                                                    SUTRA........42500
!      CALL SOLVER(KMT,KPU,KSOLVR,PMAT,PVEC,PITER,B,NN,IHALFB,NELT,NCBI,  SUTRA........42600
!     1            IWK,FWK,IA,JA,IERRP,ITRSP,ERRP)                        SUTRA........42700
C.....P SOLUTION NOW IN PVEC                                             SUTRA........42800
C                                                                        SUTRA........42900
C.....IF STEADY FLOW, SET PM1=PVEC SO THAT INTERPOLATION AT FRACTIONAL   SUTRA........43000
C        TIME STEPS YIELDS THE STEADY-STATE PRESSURE.                    SUTRA........43100
!      IF (ISSFLO.NE.0) THEN                                              SUTRA........43200
!         DO 5200 I=1,NN                                                  SUTRA........43300
!            PM1(I) = PVEC(I)                                             SUTRA........43400
! 5200    CONTINUE                                                        SUTRA........43500
!      END IF                                                             SUTRA........43600
C                                                                        SUTRA........43700
!      IF(ML-1) 5500,6000,5500                                            SUTRA........43800
C                                                                        SUTRA........43900
C.....SOLVE FOR U                                                        SUTRA........44000
! 5500 KMT=000000                                                         SUTRA........44100
!      KPU=2                                                              SUTRA........44200
!      IF(NOUMAT) 5700,5700,5600                                          SUTRA........44300
! 5600 KMT=2                                                              SUTRA........44400
! 5700 KSOLVR = KSOLVU                                                    SUTRA........44500
!      CALL SOLVER(KMT,KPU,KSOLVR,UMAT,UVEC,UITER,B,NN,IHALFB,NELT,NCBI,  SUTRA........44600
!     1            IWK,FWK,IA,JA,IERRU,ITRSU,ERRU)                        SUTRA........44700
! 6000 CONTINUE                                                           SUTRA........44800
C.....U SOLUTION NOW IN UVEC                                             SUTRA........44900
C                                                                        SUTRA........45000
!      IERR = IABS(IERRP) + IABS(IERRU)                                   SUTRA........45100
C                                                                        SUTRA........45200
C.....CHECK PROGRESS AND CONVERGENCE OF NON-LINEARITY ITERATIONS         SUTRA........45300
C        AND SET STOP AND GO FLAGS:                                      SUTRA........45400
C           ISTOP = -1   NOT CONVERGED - STOP SIMULATION                 SUTRA........45500
C           ISTOP =  0   ITERATIONS LEFT OR CONVERGED - KEEP SIMULATING  SUTRA........45600
C           ISTOP =  1   LAST TIME STEP REACHED - STOP SIMULATION        SUTRA........45700
C           IGOI = 0   P AND U CONVERGED, OR NO ITERATIONS DONE          SUTRA........45800
C           IGOI = 1   ONLY P HAS NOT YET CONVERGED TO CRITERION         SUTRA........45900
C           IGOI = 2   ONLY U HAS NOT YET CONVERGED TO CRITERION         SUTRA........46000
C           IGOI = 3   BOTH P AND U HAVE NOT YET CONVERGED TO CRITERIA   SUTRA........46100
!      ISTOP=0                                                            SUTRA........46200
!      IGOI=0                                                             SUTRA........46300
!      IF(ITRMAX-1) 7500,7500,7000                                        SUTRA........46400
! 7000 RPM=0.D0                                                           SUTRA........46500
!      RUM=0.D0                                                           SUTRA........46600
!      IPWORS=0                                                           SUTRA........46700
!      IUWORS=0                                                           SUTRA........46800
!      IF(ML-1) 7050,7050,7150                                            SUTRA........46900
! 7050 DO 7100 I=1,NN                                                     SUTRA........47000
!      RP=DABS(PVEC(I)-PITER(I))                                          SUTRA........47100
!      IF(RP-RPM) 7100,7060,7060                                          SUTRA........47200
! 7060 RPM=RP                                                             SUTRA........47300
!      IPWORS=I                                                           SUTRA........47400
! 7100 CONTINUE                                                           SUTRA........47500
!      IF(RPM.GT.RPMAX) IGOI=IGOI+1                                       SUTRA........47600
! 7150 IF(ML-1) 7200,7350,7200                                            SUTRA........47700
 !7200 DO 7300 I=1,NN                                                     SUTRA........47800
 !     RU=DABS(UVEC(I)-UITER(I))                                          SUTRA........47900
 !     IF(RU-RUM) 7300,7260,7260                                          SUTRA........48000
 !7260 RUM=RU                                                             SUTRA........48100
!      IUWORS=I                                                           SUTRA........48200
! 7300 CONTINUE                                                           SUTRA........48300
!      IF(RUM.GT.RUMAX) IGOI=IGOI+2                                       SUTRA........48400
! 7350 CONTINUE                                                           SUTRA........48500
!      IF (KSCRN.EQ.1) WRITE (*,7377) RPM, RUM                            SUTRA........48600
!      WRITE (K00,7377) RPM, RUM                                          SUTRA........48700
! 7377 FORMAT (1X, 6X, 'Maximum changes in P, U: ',1PE8.1,", ",1PE8.1)    SUTRA........48800
!      IF(IGOI.GT.0.AND.ITER.EQ.ITRMAX) ISTOP=-1                          SUTRA........48900
!      IF(IGOI.GT.0.AND.ISTOP.EQ.0.AND.IERR.EQ.0) GOTO 1100               SUTRA........49000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........49100
C.....END ITERATION - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........49200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........49300
C                                                                        SUTRA........49400
 7500 CONTINUE                                                           SUTRA........49500
      IF(ISTOP.NE.-1.AND.IT.EQ.ITMAX) ISTOP=1                            SUTRA........49600
C                                                                        SUTRA........49700
C.....OUTPUT RESULTS FOR TIME STEP IN ACCORDANCE WITH PRINT CYCLES       SUTRA........49800
C                                                                        SUTRA........49900
C.....COMPUTE SOME LOGICAL CONDITIONS.  PRNALL=.TRUE. INDICATES THAT     SUTRA........50000
C        ALL RESULTS SHOULD BE PRINTED BECAUSE THIS IS THE LAST TIME     SUTRA........50100
C        STEP (EITHER BY DESIGN OR BECAUSE OF AN ERROR).  PRN0=.TRUE.    SUTRA........50200
C        INDICATES THAT INITIAL CONDITIONS ARE TO BE PRINTED FOR A       SUTRA........50300
C        STEADY-FLOW, TRANSIENT-TRANSPORT RUN.  PRNDEF=.TRUE. IF         SUTRA........50400
C        EITHER OF THE TWO PRECEDING CONDITIONS IS TRUE.                 SUTRA........50500
!      PRNALL = ((ISTOP.NE.0).OR.(IERR.NE.0))                             SUTRA........50600
!      PRN0 = ((IT.EQ.0).AND.(ISSFLO.NE.0).AND.(ISSTRA.NE.1))             SUTRA........50700
!      PRNDEF = (PRNALL.OR.PRN0)                                          SUTRA........50800
C.....PRINT RESULTS TO THE LST OUTPUT FILE                               SUTRA........50900
!      PRNK3 = (PRNDEF.OR.(MOD(IT,NPRINT).EQ.0)                           SUTRA........51000
!     1         .OR.((IT.EQ.1).AND.(NPRINT.GT.0)))                        SUTRA........51100
!      IF (PRNK3) THEN                                                    SUTRA........51200
!      IF (KTYPE(1).EQ.3) THEN                                            SUTRA........51300
!         CALL OUTLST3(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,IERRU,ITRSU,ERRU,   SUTRA........51400
!     1      PVEC,UVEC,VMAG,VANG1,VANG2,SW)                               SUTRA........51500
!      ELSE                                                               SUTRA........51600
!         CALL OUTLST2(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,IERRU,ITRSU,ERRU,   SUTRA........51700
!     1      PVEC,UVEC,VMAG,VANG1,SW)                                     SUTRA........51800
!      END IF                                                             SUTRA........51900
C.....CALCULATE AND PRINT FLUID MASS AND/OR ENERGY OR SOLUTE MASS BUDGET SUTRA........52000
!      IF(KBUDG.EQ.1)                                                     SUTRA........52100
!     1   CALL BUDGET(ML,IBCT,VOL,SW,DSWDP,RHO,SOP,QIN,PVEC,PM1,DPDTITR,  SUTRA........52200
!     2      PBC,QPLITR,IPBC,IQSOP,POR,UVEC,UM1,UM2,UIN,QUIN,QINITR,      SUTRA........52300
!     3      IQSOU,UBC,IUBC,CS1,CS2,CS3,SL,SR,NREG)                       SUTRA........52400
!      END IF                                                             SUTRA........52500
C.....PRINT NODEWISE AND ELEMENTWISE RESULTS TO OUTPUT FILES             SUTRA........52600
!      PRNK5 = ((PRNDEF.OR.((IT.NE.0).AND.(MOD(IT,NCOLPR).EQ.0))          SUTRA........52700
!     1         .OR.((IT.EQ.1).AND.(NCOLPR.GT.0))).AND.(K5.NE.-1))        SUTRA........52800
!      IF (PRNK5) CALL OUTNOD(PVEC,UVEC,SW,X,Y,Z,TITLE1,TITLE2)           SUTRA........52900
!      PRNK6 = ((PRNALL.OR.((IT.NE.0).AND.(MOD(IT,LCOLPR).EQ.0))          SUTRA........53000
!     1         .OR.(IT.EQ.1)).AND.(K6.NE.-1))                            SUTRA........53100
!      IF (PRNK6) CALL OUTELE(VMAG,VANG1,VANG2,IN,X,Y,Z,TITLE1,TITLE2)    SUTRA........53200
C.....PRINT RESULTS TO OBSERVATION OUTPUT FILES.  CHECK FOR OUTPUT       SUTRA........53300
C       SCHEDULED WITHIN THE CURRENT TIME STEP AND PRINT IT.  IF THIS    SUTRA........53400
C       IS THE INITIAL CONDITION (IT=0) OR THE FINAL TIME STEP, PRINT    SUTRA........53500
C       RESULTS IF THEY HAVE NOT ALREADY BEEN PRINTED.                   SUTRA........53600
C.....LOOP OVER OBSERVATION OUTPUT SCHEDULES.                            SUTRA........53700
!      DO 7650 NFLO=1,NFLOMX                                              SUTRA........53800
C........IF NO FILE FOR THIS OUTPUT, SKIP IT                             SUTRA........53900
!         IF (IUNIO(NFLO).EQ.-1) CYCLE                                    SUTRA........54000
C........SET FLAG INDICATING THAT OUTPUT HAS NOT (YET) BEEN PRINTED      SUTRA........54100
C           FOR THE END OF THE CURRENT TIME STEP                         SUTRA........54200
!         TSPRTD = .FALSE.                                                SUTRA........54300
C........IF TRANSPORT IS TRANSIENT AND THIS IS NOT THE INITIAL           SUTRA........54400
C            CONDITION, CHECK FOR SCHEDULED OUTPUT AND PRINT IT.         SUTRA........54500
!         IF ((ISSTRA.EQ.0).AND.(IT.NE.0)) THEN                           SUTRA........54600
C...........GET THE LENGTH OF THE SCHEDULE AND THE NEXT TIME/STEP        SUTRA........54700
C              SCHEDULED TO BE OUTPUT.                                   SUTRA........54800
!            LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                       SUTRA........54900
!            TIME = DENOB(NFLO)%NENT%DVALU1                               SUTRA........55000
!            STEP = DENOB(NFLO)%NENT%DVALU2                               SUTRA........55100
C...........LOOP THROUGH THE SCHEDULE, PRINTING ANY OUTPUT SCHEDULED     SUTRA........55200
C              WITHIN THE CURRENT TIME STEP.  (LOOP AS LONG AS THE       SUTRA........55300
C              SCHEDULED OUTPUT IS WITHIN THE CURRENT TIME STEP          SUTRA........55400
C              AND THE SCHEDULE HAS NOT BEEN EXHAUSTED.)                 SUTRA........55500
!            DO WHILE ((DIT.GE.STEP).AND.(LCNT(NFLO).LE.LENSCH))          SUTRA........55600
C..............IF THE SCHEDULED STEP IS NOT ZERO, PRINT RESULTS.         SUTRA........55700
!               IF (STEP.NE.0D0) THEN                                     SUTRA........55800
!                  IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                      SUTRA........55900
!                     CALL OUTOBS(NFLO,OBSPTS,TIME,STEP,PM1,UM1,          SUTRA........56000
!     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG)                 SUTRA........56100
!                  ELSE                                                   SUTRA........56200
!                     CALL OUTOBC(NFLO,OBSPTS,TIME,STEP,PM1,UM1,          SUTRA........56300
!     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG)                 SUTRA........56400
!                  END IF                                                 SUTRA........56500
C.................IF END OF TIME STEP HAS JUST BEEN PRINTED, SET FLAG.   SUTRA........56600
!                  IF (DIT.EQ.STEP) TSPRTD = .TRUE.                       SUTRA........56700
!               END IF                                                    SUTRA........56800
C..............GO TO THE NEXT ENTRY IN THE SCHEDULE, IF THERE IS ONE.    SUTRA........56900
!               IF (LCNT(NFLO).LT.LENSCH) THEN                            SUTRA........57000
!                  DENOB(NFLO)%NENT => DENOB(NFLO)%NENT%NENT              SUTRA........57100
!                  TIME = DENOB(NFLO)%NENT%DVALU1                         SUTRA........57200
!                  STEP = DENOB(NFLO)%NENT%DVALU2                         SUTRA........57300
!               END IF                                                    SUTRA........57400
C..............INCREMENT THE COUNTER.                                    SUTRA........57500
!               LCNT(NFLO) = LCNT(NFLO) + 1                               SUTRA........57600
!            END DO                                                       SUTRA........57700
!         END IF                                                          SUTRA........57800
C........IF THIS IS THE INITIAL OR FINAL CONDITION, PRINT IT IF IT       SUTRA........57900
C           HAS NOT ALREADY BEEN PRINTED.                                SUTRA........58000
!         IF (PRNDEF.AND.(.NOT.TSPRTD)) THEN                              SUTRA........58100
!            IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                            SUTRA........58200
!               CALL OUTOBS(NFLO,OBSPTS,TSEC,DIT,PM1,UM1,PVEC,UVEC,       SUTRA........58300
!     1            TITLE1,TITLE2,IN,LREG)                                 SUTRA........58400
!            ELSE                                                         SUTRA........58500
!               CALL OUTOBC(NFLO,OBSPTS,TSEC,DIT,PM1,UM1,PVEC,UVEC,       SUTRA........58600
!     1            TITLE1,TITLE2,IN,LREG)                                 SUTRA........58700
!            END IF                                                       SUTRA........58800
!         END IF                                                          SUTRA........58900
! 7650 CONTINUE                                                           SUTRA........59000
C                                                                        SUTRA........59100
C.....STORE RESULTS FOR POSSIBLE RESTART OF SIMULATION EACH              SUTRA........59200
C        ISTORE TIME STEPS AND AFTER LAST TIME STEP, THEN GO             SUTRA........59300
C        TO NEXT TIME STEP                                               SUTRA........59400
!      IF (IERR.EQ.0) THEN                                                SUTRA........59500
!         IF ((ISTORE.NE.0).AND.((ISTOP.NE.0).OR.(MOD(IT,ISTORE).EQ.0)))  SUTRA........59600
!     1      CALL OUTRST(PVEC,UVEC,PM1,UM1,CS1,RCIT,SW,QINITR,PBC)        SUTRA........59700
!         IF (ISTOP.EQ.0) GOTO 1000                                       SUTRA........59800
!      END IF                                                             SUTRA........59900
C                                                                        SUTRA........60000
C ********************************************************************** SUTRA........60100
C.....END TIME STEP **************************************************** SUTRA........60200
C ********************************************************************** SUTRA........60300
C                                                                        SUTRA........60400
C                                                                        SUTRA........60500
C.....DEALLOCATE ARRAY DENOB                                             SUTRA........60600
      DEALLOCATE (DENOB)                                                 SUTRA........60700
C                                                                        SUTRA........60800
C.....COMPLETE OUTPUT AND TERMINATE SIMULATION                           SUTRA........60900
!      IF (IERRP.NE.0) THEN                                               SUTRA........61000
!         ERRCOD = 'SOL-1'                                                SUTRA........61100
!         CHERR(1) = 'P'                                                  SUTRA........61200
!         CHERR(2) = SOLWRD(KSOLVP)                                       SUTRA........61300
!         INERR(1) = IERRP                                                SUTRA........61400
!         INERR(2) = ITRSP                                                SUTRA........61500
!         RLERR(1) = ERRP                                                 SUTRA........61600
!         RLERR(2) = TOLP                                                 SUTRA........61700
!         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        SUTRA........61800!
!	   RETURN
!      ELSE IF (IERRU.NE.0) THEN                                          SUTRA........61900
!         ERRCOD = 'SOL-1'                                                SUTRA........62000
!         CHERR(1) = 'U'                                                  SUTRA........62100
!         CHERR(2) = SOLWRD(KSOLVU)                                       SUTRA........62200
!         INERR(1) = IERRU                                                SUTRA........62300
!         INERR(2) = ITRSU                                                SUTRA........62400
!         RLERR(1) = ERRU                                                 SUTRA........62500
!         RLERR(2) = TOLU                                                 SUTRA........62600
!         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        SUTRA........62700!
!	   RETURN
!      END IF                                                             SUTRA........62800
C                                                                        SUTRA........62900
!      IF(ISTORE.GT.0) WRITE(K3,8100)                                     SUTRA........63000
! 8100 FORMAT(//////11X,'*** LAST SOLUTION HAS BEEN STORED ',             SUTRA........63100
!     1   'IN THE RESTART DATA FILE ***')                                 SUTRA........63200
C                                                                        SUTRA........63300
C.....OUTPUT END OF SIMULATION MESSAGE AND RETURN TO MAIN FOR STOP       SUTRA........63400
      IF(ISTOP.EQ.-1) THEN                                               SUTRA........63500
!         ERRCOD = 'CON-1'                                                SUTRA........63600
!         IF (ME.EQ.1) THEN                                               SUTRA........63700
!            CDUM80 = 'temperature'                                       SUTRA........63800
!            LENC = 11                                                    SUTRA........63900
!         ELSE                                                            SUTRA........64000
!            CDUM80 = 'concentration'                                     SUTRA........64100
!            LENC = 13                                                    SUTRA........64200
!         END IF                                                          SUTRA........64300
!         IF (IGOI.EQ.1) THEN                                             SUTRA........64400
!            CHERR(1) = 'pressure'                                        SUTRA........64500
!            LENC = 8                                                     SUTRA........64600
!         ELSE IF (IGOI.EQ.2) THEN                                        SUTRA........64700
!            CHERR(1) = CDUM80                                            SUTRA........64800
!         ELSE IF (IGOI.EQ.3) THEN                                        SUTRA........64900
!            CHERR(1) = 'pressure and ' // CDUM80(1:LENC)                 SUTRA........65000
!            LENC = 13 + LENC                                             SUTRA........65100
!         END IF                                                          SUTRA........65200
!         INERR(1) = IPWORS                                               SUTRA........65300
!         INERR(2) = IUWORS                                               SUTRA........65400
!         INERR(3) = ITER                                                 SUTRA........65500
!         INERR(4) = LENC                                                 SUTRA........65600
!         RLERR(1) = RPM                                                  SUTRA........65700
!         RLERR(2) = RPMAX                                                SUTRA........65800
!         RLERR(3) = RUM                                                  SUTRA........65900
!         RLERR(4) = RUMAX                                                SUTRA........66000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR,IERROR)                        SUTRA........66100
	   RETURN
!      ELSE IF (ISTOP.EQ.2) THEN                                          SUTRA........66200
!         WRITE(K3,8450)                                                  SUTRA........66300
! 8450    FORMAT(////////11X,'SUTRA SIMULATION TERMINATED AT',            SUTRA........66400
!     1      ' COMPLETION OF TIME PERIOD'/                                SUTRA........66500
!     2                  11X,'***** ********** ********** **',            SUTRA........66600
!     3      ' ********** ** **** ******')                                SUTRA........66700
!      ELSE                                                               SUTRA........66800
!         WRITE(K3,8550)                                                  SUTRA........66900
! 8550    FORMAT(////////11X,'SUTRA SIMULATION TERMINATED AT',            SUTRA........67000
!     1      ' COMPLETION OF TIME STEPS'/                                 SUTRA........67100
!     2                  11X,'***** ********** ********** **',            SUTRA........67200
!     3      ' ********** ** **** *****')                                 SUTRA........67300
      END IF                                                             SUTRA........67400
C                                                                        SUTRA........67500
!      IF (KSCRN.EQ.1) WRITE(*,8590)                                      SUTRA........67600
!      WRITE(K00,8590)                                                    SUTRA........67700
! 8590 FORMAT(/1X,'S I M U L A T I O N   E N D E D'/)                     SUTRA........67800
      RETURN                                                             SUTRA........67900
C                                                                        SUTRA........68000
      END                                                                SUTRA........68100
C                                                                        SUTRA........68200
C     SUBROUTINE        T  E  N  S  Y  M           SUTRA VERSION 2.1     TENSYM.........100
C                                                                        TENSYM.........200
C *** PURPOSE :                                                          TENSYM.........300
C ***  TO TRANSFORM A DIAGONAL MATRIX TO A NEW COORDINATE SYSTEM.        TENSYM.........400
C ***  [T] IS THE DIAGONAL MATRIX EXPRESSED IN THE FIRST (INPUT)         TENSYM.........500
C ***  COORDINATE SYSTEM; [P] IS THE (SYMMETRIC) MATRIX EXPRESSED        TENSYM.........600
C ***  IN THE SECOND (OUTPUT) COORDINATE SYSTEM; AND [Q] IS THE          TENSYM.........700
C ***  THE TRANSFORMATION MATRIX.                                        TENSYM.........800
C                                                                        TENSYM.........900
C                                                                        TENSYM........2700
C     SUBROUTINE        T  E  R  S  E  Q           SUTRA VERSION 2.1     TERSEQ.........100
C                                                                        TERSEQ.........200
C *** PURPOSE :                                                          TERSEQ.........300
C ***  TO GRACEFULLY TERMINATE A SUTRA RUN BY DEALLOCATING THE MAIN      TERSEQ.........400
C ***  ALLOCATABLE ARRAYS AND CLOSING ALL FILES.                         TERSEQ.........500
C                                                                        TERSEQ.........600
      SUBROUTINE TERSEQ()                                                TERSEQ.........700
      USE ALLARR                                                         TERSEQ.........800
      USE SCHDEF                                                         TERSEQ.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                TERSEQ........1000
      CHARACTER CDUM*1                                                   TERSEQ........1100
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     TERSEQ........1200
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     TERSEQ........1300
     1   KSCRN,KPAUSE                                                    TERSEQ........1400
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      TERSEQ........1500
C                                                                        TERSEQ........1600
C.....TERMINATION SEQUENCE: DEALLOCATE ARRAYS, CLOSE FILES, AND STOP     TERSEQ........1700
      IF (ALLO1) THEN                                                    TERSEQ........1800
         DEALLOCATE(PITER,UITER,PM1,DPDTITR,UM1,UM2,PVEL,SL,SR,X,Y,Z,    TERSEQ........1900
     1      VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,QIN,UIN,QUIN,QINITR,    TERSEQ........2000
     2      RCIT,RCITM1)                                                 TERSEQ........2100
         DEALLOCATE(PVEC,UVEC)                                           TERSEQ........2200
         DEALLOCATE(ALMAX,ALMIN,ATMAX,ATMIN,VMAG,VANG1,PERMXX,PERMXY,    TERSEQ........2300
     1      PERMYX,PERMYY,PANGL1)                                        TERSEQ........2400
         DEALLOCATE(ALMID,ATMID,VANG2,PERMXZ,PERMYZ,PERMZX,PERMZY,       TERSEQ........2500
     1      PERMZZ,PANGL2,PANGL3)                                        TERSEQ........2600
         DEALLOCATE(PBC,UBC,QPLITR)                                      TERSEQ........2700
         DEALLOCATE(GXSI,GETA,GZET)                                      TERSEQ........2800
         DEALLOCATE(B)                                                   TERSEQ........2900
         DEALLOCATE(IN,IQSOP,IQSOU,IPBC,IUBC,NREG,LREG,JA)               TERSEQ........3000
         DEALLOCATE(OBSPTS)                                              TERSEQ........3100
      END IF                                                             TERSEQ........3200
      IF (ALLO2) THEN                                                    TERSEQ........3300
!         DEALLOCATE(PMAT,UMAT,FWK)                                       TERSEQ........3400
         DEALLOCATE(IWK)                                                 TERSEQ........3500
      END IF                                                             TERSEQ........3600
      IF (ALLO3) THEN                                                    TERSEQ........3700
         DEALLOCATE(IA)                                                  TERSEQ........3800
      END IF                                                             TERSEQ........3900
      IF (ALLOCATED(SCHDLS)) DEALLOCATE(SCHDLS)                          TERSEQ........4000
      IF (ALLOCATED(OFP)) DEALLOCATE(OFP)                                TERSEQ........4100
      IF (ALLOCATED(FNAMO)) DEALLOCATE(FNAMO)                            TERSEQ........4200
      IF (ALLOCATED(ONCK78)) DEALLOCATE(ONCK78)                          TERSEQ........4300
C.....ARRAY IUNIO WILL BE DEALLOCATED AFTER THE OBSERVATION OUTPUT       TERSEQ........4400
C        FILES ARE CLOSED                                                TERSEQ........4500
      CLOSE(K00)                                                         TERSEQ........4600
      CLOSE(K0)                                                          TERSEQ........4700
      CLOSE(K1)                                                          TERSEQ........4800
      CLOSE(K2)                                                          TERSEQ........4900
      CLOSE(K3)                                                          TERSEQ........5000
      CLOSE(K4)                                                          TERSEQ........5100
      CLOSE(K5)                                                          TERSEQ........5200
      CLOSE(K6)                                                          TERSEQ........5300
      CLOSE(K7)                                                          TERSEQ........5400
      CLOSE(K8)                                                          TERSEQ........5500
      DO 8000 NFO=1,NFLOMX                                               TERSEQ........5600
         CLOSE(IUNIO(NFO))                                               TERSEQ........5700
 8000 CONTINUE                                                           TERSEQ........5800
      IF (ALLOCATED(IUNIO)) DEALLOCATE(IUNIO)                            TERSEQ........5900
!      IF ((KSCRN.EQ.1).AND.(KPAUSE.EQ.1)) THEN                           TERSEQ........6000
!         WRITE(*,9990)                                                   TERSEQ........6100
! 9990    FORMAT(/' Press ENTER to exit ...')                             TERSEQ........6200
!         READ(*,'(A1)') CDUM                                             TERSEQ........6300
!      END IF                                                             TERSEQ........6400
!      STOP ' '                                                           TERSEQ........6500
C                                                                        TERSEQ........6600
      RETURN                                                             TERSEQ........6700
      END                                                                TERSEQ........6800
C                                                                        TERSEQ........6900
C     FUNCTION          T  I  M  E  T  S           SUTRA VERSION 2.1     TIMETS.........100
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
      COMMON /SCH/ NSCH,ISCHTS                                           TIMETS........1600
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
C     SUBROUTINE        Z  E  R  O                 SUTRA VERSION 2.1     ZERO...........100
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
      SUBROUTINE SUTRA_LABELS21(FLABELS)
!DEC$ attributes dllexport :: SUTRA_LABELS21
C FLABELS CONTAINS THE NAMES OF THE BOUNDARY FEATURES. 
	CHARACTER(*) FLABELS
      FLABELS  = 'fluid src/sink                          energy/sol src
     &/sink                     spec. pressure                          
     &spec. conc/temp                         '
      RETURN
      END
C RBW
