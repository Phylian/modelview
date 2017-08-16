C     Last change:  ERB   4 Jan 2001   11:54 am
C
C  Some code in this file is compiler-specific.
C
C  To use parallel processing with the Lahey LF90 compiler,
C  the data statement containing 'DENY' should be uncommented
C  to enable file sharing.
C
C  If another Fortran-90 or -95 compiler is used, the data statement
C  without 'DENY' should be uncommented and the one with 'DENY' should
C  be commented out.
C
C  For FORTRAN 77 compilers, eliminate the ACTION= option from all
C  OPEN statements in the source-code files.
C
      CHARACTER*20 ACT(2)
      DATA (ACT(I),I=1,2)/'READ','READ'/
C      DATA (ACT(I),I=1,2)/'READ,DENYWRITE','READWRITE,DENYNONE'/

