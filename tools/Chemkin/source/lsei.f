C*****precision > double
C      SUBROUTINE DH12 (MODE, LPIVOT, L1, M, U, IUE, UP, C, ICE, ICV,
C     +   NCV)
CC***BEGIN PROLOGUE  DH12
CC***SUBSIDIARY
CC***PURPOSE  Subsidiary to DHFTI, DLSEI and DWNNLS
CC***LIBRARY   SLATEC
CC***TYPE      DOUBLE PRECISION (H12-S, DH12-D)
CC***AUTHOR  (UNKNOWN)
CC***DESCRIPTION
CC
CC      *** DOUBLE PRECISION VERSION OF H12 ******
CC
CC     C.L.Lawson and R.J.Hanson, Jet Propulsion Laboratory, 1973 Jun 12
CC     to appear in 'Solving Least Squares Problems', Prentice-Hall, 1974
CC
CC     Construction and/or application of a single
CC     Householder transformation..     Q = I + U*(U**T)/B
CC
CC     MODE    = 1 or 2   to select algorithm  H1  or  H2 .
CC     LPIVOT is the index of the pivot element.
CC     L1,M   If L1 .LE. M   the transformation will be constructed to
CC            zero elements indexed from L1 through M.   If L1 GT. M
CC            THE SUBROUTINE DOES AN IDENTITY TRANSFORMATION.
CC     U(),IUE,UP    On entry to H1 U() contains the pivot vector.
CC                   IUE is the storage increment between elements.
CC                                       On exit from H1 U() and UP
CC                   contain quantities defining the vector U of the
CC                   Householder transformation.   On entry to H2 U()
CC                   and UP should contain quantities previously computed
CC                   by H1.  These will not be modified by H2.
CC     C()    On entry to H1 or H2 C() contains a matrix which will be
CC            regarded as a set of vectors to which the Householder
CC            transformation is to be applied.  On exit C() contains the
CC            set of transformed vectors.
CC     ICE    Storage increment between elements of vectors in C().
CC     ICV    Storage increment between vectors in C().
CC     NCV    Number of vectors in C() to be transformed. If NCV .LE. 0
CC            no operations will be done on C().
CC
CC***SEE ALSO  DHFTI, DLSEI, DWNNLS
CC***ROUTINES CALLED  DAXPY, DDOT, DSWAP
CC***REVISION HISTORY  (YYMMDD)
CC   790101  DATE WRITTEN
CC   890531  Changed all specific intrinsics to generic.  (WRB)
CC   890831  Modified array declarations.  (WRB)
CC   891214  Prologue converted to Version 4.0 format.  (BAB)
CC   900328  Added TYPE section.  (WRB)
CC   900911  Added DDOT to DOUBLE PRECISION statement.  (WRB)
CC***END PROLOGUE  DH12
C      INTEGER I, I2, I3, I4, ICE, ICV, INCR, IUE, J, KL1, KL2, KLP,
C     *     L1, L1M1, LPIVOT, M, MML1P2, MODE, NCV
C      DOUBLE PRECISION B, C, CL, CLINV, ONE, UL1M1, SM, U, UP, DDOT
C      DIMENSION U(IUE,*), C(*)
CC     BEGIN BLOCK PERMITTING ...EXITS TO 140
CC***FIRST EXECUTABLE STATEMENT  DH12
C         ONE = 1.0D0
CC
CC     ...EXIT
C         IF (0 .GE. LPIVOT .OR. LPIVOT .GE. L1 .OR. L1 .GT. M) GO TO 140
C         CL = ABS(U(1,LPIVOT))
C         IF (MODE .EQ. 2) GO TO 40
CC           ****** CONSTRUCT THE TRANSFORMATION. ******
C            DO 10 J = L1, M
C               CL = MAX(ABS(U(1,J)),CL)
C   10       CONTINUE
C            IF (CL .GT. 0.0D0) GO TO 20
CC     .........EXIT
C               GO TO 140
C   20       CONTINUE
C            CLINV = ONE/CL
C            SM = (U(1,LPIVOT)*CLINV)**2
C            DO 30 J = L1, M
C               SM = SM + (U(1,J)*CLINV)**2
C   30       CONTINUE
C            CL = CL*SQRT(SM)
C            IF (U(1,LPIVOT) .GT. 0.0D0) CL = -CL
C            UP = U(1,LPIVOT) - CL
C            U(1,LPIVOT) = CL
C         GO TO 50
C   40    CONTINUE
CC        ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ******
CC
C         IF (CL .GT. 0.0D0) GO TO 50
CC     ......EXIT
C            GO TO 140
C   50    CONTINUE
CC     ...EXIT
C         IF (NCV .LE. 0) GO TO 140
C         B = UP*U(1,LPIVOT)
CC        B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN.
CC
C         IF (B .LT. 0.0D0) GO TO 60
CC     ......EXIT
C            GO TO 140
C   60    CONTINUE
C         B = ONE/B
C         MML1P2 = M - L1 + 2
C         IF (MML1P2 .LE. 20) GO TO 80
C            L1M1 = L1 - 1
C            KL1 = 1 + (L1M1 - 1)*ICE
C            KL2 = KL1
C            KLP = 1 + (LPIVOT - 1)*ICE
C            UL1M1 = U(1,L1M1)
C            U(1,L1M1) = UP
C            IF (LPIVOT .NE. L1M1) CALL DSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
C            DO 70 J = 1, NCV
C               SM = DDOT(MML1P2,U(1,L1M1),IUE,C(KL1),ICE)
C               SM = SM*B
C               CALL DAXPY(MML1P2,SM,U(1,L1M1),IUE,C(KL1),ICE)
C               KL1 = KL1 + ICV
C   70       CONTINUE
C            U(1,L1M1) = UL1M1
CC     ......EXIT
C            IF (LPIVOT .EQ. L1M1) GO TO 140
C            KL1 = KL2
C            CALL DSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
C         GO TO 130
C   80    CONTINUE
C            I2 = 1 - ICV + ICE*(LPIVOT - 1)
C            INCR = ICE*(L1 - LPIVOT)
C            DO 120 J = 1, NCV
C               I2 = I2 + ICV
C               I3 = I2 + INCR
C               I4 = I3
C               SM = C(I2)*UP
C               DO 90 I = L1, M
C                  SM = SM + C(I3)*U(1,I)
C                  I3 = I3 + ICE
C   90          CONTINUE
C               IF (SM .EQ. 0.0D0) GO TO 110
C                  SM = SM*B
C                  C(I2) = C(I2) + SM*UP
C                  DO 100 I = L1, M
C                     C(I4) = C(I4) + SM*U(1,I)
C                     I4 = I4 + ICE
C  100             CONTINUE
C  110          CONTINUE
C  120       CONTINUE
C  130    CONTINUE
C  140 CONTINUE
C      RETURN
C      END
C      SUBROUTINE DHFTI (A, MDA, M, N, B, MDB, NB, TAU, KRANK, RNORM, H,
C     +   G, IP)
CC***BEGIN PROLOGUE  DHFTI
CC***PURPOSE  Solve a least squares problem for banded matrices using
CC            sequential accumulation of rows of the data matrix.
CC            Exactly one right-hand side vector is permitted.
CC***LIBRARY   SLATEC
CC***CATEGORY  D9
CC***TYPE      DOUBLE PRECISION (HFTI-S, DHFTI-D)
CC***KEYWORDS  CURVE FITTING, LEAST SQUARES
CC***AUTHOR  Lawson, C. L., (JPL)
CC           Hanson, R. J., (SNLA)
CC***DESCRIPTION
CC
CC     DIMENSION A(MDA,N),(B(MDB,NB) or B(M)),RNORM(NB),H(N),G(N),IP(N)
CC
CC     This subroutine solves a linear least squares problem or a set of
CC     linear least squares problems having the same matrix but different
CC     right-side vectors.  The problem data consists of an M by N matrix
CC     A, an M by NB matrix B, and an absolute tolerance parameter TAU
CC     whose usage is described below.  The NB column vectors of B
CC     represent right-side vectors for NB distinct linear least squares
CC     problems.
CC
CC     This set of problems can also be written as the matrix least
CC     squares problem
CC
CC                       AX = B,
CC
CC     where X is the N by NB solution matrix.
CC
CC     Note that if B is the M by M identity matrix, then X will be the
CC     pseudo-inverse of A.
CC
CC     This subroutine first transforms the augmented matrix (A B) to a
CC     matrix (R C) using premultiplying Householder transformations with
CC     column interchanges.  All subdiagonal elements in the matrix R are
CC     zero and its diagonal elements satisfy
CC
CC                       ABS(R(I,I)).GE.ABS(R(I+1,I+1)),
CC
CC                       I = 1,...,L-1, where
CC
CC                       L = MIN(M,N).
CC
CC     The subroutine will compute an integer, KRANK, equal to the number
CC     of diagonal terms of R that exceed TAU in magnitude. Then a
CC     solution of minimum Euclidean length is computed using the first
CC     KRANK rows of (R C).
CC
CC     To be specific we suggest that the user consider an easily
CC     computable matrix norm, such as, the maximum of all column sums of
CC     magnitudes.
CC
CC     Now if the relative uncertainty of B is EPS, (norm of uncertainty/
CC     norm of B), it is suggested that TAU be set approximately equal to
CC     EPS*(norm of A).
CC
CC     The user must dimension all arrays appearing in the call list..
CC     A(MDA,N),(B(MDB,NB) or B(M)),RNORM(NB),H(N),G(N),IP(N).  This
CC     permits the solution of a range of problems in the same array
CC     space.
CC
CC     The entire set of parameters for DHFTI are
CC
CC     INPUT.. All TYPE REAL variables are DOUBLE PRECISION
CC
CC     A(*,*),MDA,M,N    The array A(*,*) initially contains the M by N
CC                       matrix A of the least squares problem AX = B.
CC                       The first dimensioning parameter of the array
CC                       A(*,*) is MDA, which must satisfy MDA.GE.M
CC                       Either M.GE.N or M.LT.N is permitted.  There
CC                       is no restriction on the rank of A.  The
CC                       condition MDA.LT.M is considered an error.
CC
CC     B(*),MDB,NB       If NB = 0 the subroutine will perform the
CC                       orthogonal decomposition but will make no
CC                       references to the array B(*).  If NB.GT.0
CC                       the array B(*) must initially contain the M by
CC                       NB matrix B of the least squares problem AX =
CC                       B.  If NB.GE.2 the array B(*) must be doubly
CC                       subscripted with first dimensioning parameter
CC                       MDB.GE.MAX(M,N).  If NB = 1 the array B(*) may
CC                       be either doubly or singly subscripted.  In
CC                       the latter case the value of MDB is arbitrary
CC                       but it should be set to some valid integer
CC                       value such as MDB = M.
CC
CC                       The condition of NB.GT.1.AND.MDB.LT. MAX(M,N)
CC                       is considered an error.
CC
CC     TAU               Absolute tolerance parameter provided by user
CC                       for pseudorank determination.
CC
CC     H(*),G(*),IP(*)   Arrays of working space used by DHFTI.
CC
CC     OUTPUT.. All TYPE REAL variables are DOUBLE PRECISION
CC
CC     A(*,*)            The contents of the array A(*,*) will be
CC                       modified by the subroutine. These contents
CC                       are not generally required by the user.
CC
CC     B(*)              On return the array B(*) will contain the N by
CC                       NB solution matrix X.
CC
CC     KRANK             Set by the subroutine to indicate the
CC                       pseudorank of A.
CC
CC     RNORM(*)          On return, RNORM(J) will contain the Euclidean
CC                       norm of the residual vector for the problem
CC                       defined by the J-th column vector of the array
CC                       B(*,*) for J = 1,...,NB.
CC
CC     H(*),G(*)         On return these arrays respectively contain
CC                       elements of the pre- and post-multiplying
CC                       Householder transformations used to compute
CC                       the minimum Euclidean length solution.
CC
CC     IP(*)             Array in which the subroutine records indices
CC                       describing the permutation of column vectors.
CC                       The contents of arrays H(*),G(*) and IP(*)
CC                       are not generally required by the user.
CC
CC***REFERENCES  C. L. Lawson and R. J. Hanson, Solving Least Squares
CC                 Problems, Prentice-Hall, Inc., 1974, Chapter 14.
CC***ROUTINES CALLED  D1MACH, DH12, XERMSG
CC***REVISION HISTORY  (YYMMDD)
CC   790101  DATE WRITTEN
CC   890531  Changed all specific intrinsics to generic.  (WRB)
CC   891006  Cosmetic changes to prologue.  (WRB)
CC   891006  REVISION DATE from Version 3.2
CC   891214  Prologue converted to Version 4.0 format.  (BAB)
CC   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
CC   901005  Replace usage of DDIFF with usage of D1MACH.  (RWC)
CC   920501  Reformatted the REFERENCES section.  (WRB)
CC***END PROLOGUE  DHFTI
C      INTEGER I, II, IOPT, IP(*), IP1, J, JB, JJ, K, KP1, KRANK, L,
C     *     LDIAG, LMAX, M, MDA, MDB, N, NB, NERR
C      DOUBLE PRECISION A, B, D1MACH, DZERO, FACTOR,
C     *     G, H, HMAX, RELEPS, RNORM, SM, SM1, SZERO, TAU, TMP
C      DIMENSION A(MDA,*),B(MDB,*),H(*),G(*),RNORM(*)
C      SAVE RELEPS
C      DATA RELEPS /0.D0/
CC     BEGIN BLOCK PERMITTING ...EXITS TO 360
CC***FIRST EXECUTABLE STATEMENT  DHFTI
C         IF (RELEPS.EQ.0.D0) RELEPS = D1MACH(4)
C         SZERO = 0.0D0
C         DZERO = 0.0D0
C         FACTOR = 0.001D0
CC
C         K = 0
C         LDIAG = MIN(M,N)
C         IF (LDIAG .LE. 0) GO TO 350
CC           BEGIN BLOCK PERMITTING ...EXITS TO 130
CC              BEGIN BLOCK PERMITTING ...EXITS TO 120
C                  IF (MDA .GE. M) GO TO 10
C                     NERR = 1
C                     IOPT = 2
C                     CALL XERMSG ('SLATEC', 'DHFTI',
C     +                  'MDA.LT.M, PROBABLE ERROR.',
C     +                  NERR, IOPT)
CC     ...............EXIT
C                     GO TO 360
C   10             CONTINUE
CC
C                  IF (NB .LE. 1 .OR. MAX(M,N) .LE. MDB) GO TO 20
C                     NERR = 2
C                     IOPT = 2
C                     CALL XERMSG ('SLATEC', 'DHFTI',
C     +                  'MDB.LT.MAX(M,N).AND.NB.GT.1. PROBABLE ERROR.',
C     +                  NERR, IOPT)
CC     ...............EXIT
C                     GO TO 360
C   20             CONTINUE
CC
C                  DO 100 J = 1, LDIAG
CC                    BEGIN BLOCK PERMITTING ...EXITS TO 70
C                        IF (J .EQ. 1) GO TO 40
CC
CC                           UPDATE SQUARED COLUMN LENGTHS AND FIND LMAX
CC                          ..
C                           LMAX = J
C                           DO 30 L = J, N
C                              H(L) = H(L) - A(J-1,L)**2
C                              IF (H(L) .GT. H(LMAX)) LMAX = L
C   30                      CONTINUE
CC                    ......EXIT
C                           IF (FACTOR*H(LMAX) .GT. HMAX*RELEPS) GO TO 70
C   40                   CONTINUE
CC
CC                        COMPUTE SQUARED COLUMN LENGTHS AND FIND LMAX
CC                       ..
C                        LMAX = J
C                        DO 60 L = J, N
C                           H(L) = 0.0D0
C                           DO 50 I = J, M
C                              H(L) = H(L) + A(I,L)**2
C   50                      CONTINUE
C                           IF (H(L) .GT. H(LMAX)) LMAX = L
C   60                   CONTINUE
C                        HMAX = H(LMAX)
C   70                CONTINUE
CC                    ..
CC                     LMAX HAS BEEN DETERMINED
CC
CC                     DO COLUMN INTERCHANGES IF NEEDED.
CC                    ..
C                     IP(J) = LMAX
C                     IF (IP(J) .EQ. J) GO TO 90
C                        DO 80 I = 1, M
C                           TMP = A(I,J)
C                           A(I,J) = A(I,LMAX)
C                           A(I,LMAX) = TMP
C   80                   CONTINUE
C                        H(LMAX) = H(J)
C   90                CONTINUE
CC
CC                     COMPUTE THE J-TH TRANSFORMATION AND APPLY IT TO A
CC                     AND B.
CC                    ..
C                     CALL DH12(1,J,J+1,M,A(1,J),1,H(J),A(1,J+1),1,MDA,
C     *                         N-J)
C                     CALL DH12(2,J,J+1,M,A(1,J),1,H(J),B,1,MDB,NB)
C  100             CONTINUE
CC
CC                  DETERMINE THE PSEUDORANK, K, USING THE TOLERANCE,
CC                  TAU.
CC                 ..
C                  DO 110 J = 1, LDIAG
CC              ......EXIT
C                     IF (ABS(A(J,J)) .LE. TAU) GO TO 120
C  110             CONTINUE
C                  K = LDIAG
CC           ......EXIT
C                  GO TO 130
C  120          CONTINUE
C               K = J - 1
C  130       CONTINUE
C            KP1 = K + 1
CC
CC           COMPUTE THE NORMS OF THE RESIDUAL VECTORS.
CC
C            IF (NB .LT. 1) GO TO 170
C            DO 160 JB = 1, NB
C               TMP = SZERO
C               IF (M .LT. KP1) GO TO 150
C               DO 140 I = KP1, M
C                  TMP = TMP + B(I,JB)**2
C  140          CONTINUE
C  150          CONTINUE
C               RNORM(JB) = SQRT(TMP)
C  160       CONTINUE
C  170       CONTINUE
CC           SPECIAL FOR PSEUDORANK = 0
C            IF (K .GT. 0) GO TO 210
C               IF (NB .LT. 1) GO TO 200
C               DO 190 JB = 1, NB
C                  DO 180 I = 1, N
C                     B(I,JB) = SZERO
C  180             CONTINUE
C  190          CONTINUE
C  200          CONTINUE
C            GO TO 340
C  210       CONTINUE
CC
CC               IF THE PSEUDORANK IS LESS THAN N COMPUTE HOUSEHOLDER
CC               DECOMPOSITION OF FIRST K ROWS.
CC              ..
C               IF (K .EQ. N) GO TO 230
C                  DO 220 II = 1, K
C                     I = KP1 - II
C                     CALL DH12(1,I,KP1,N,A(I,1),MDA,G(I),A,MDA,1,I-1)
C  220             CONTINUE
C  230          CONTINUE
CC
CC
C               IF (NB .LT. 1) GO TO 330
C               DO 320 JB = 1, NB
CC
CC                  SOLVE THE K BY K TRIANGULAR SYSTEM.
CC                 ..
C                  DO 260 L = 1, K
C                     SM = DZERO
C                     I = KP1 - L
C                     IP1 = I + 1
C                     IF (K .LT. IP1) GO TO 250
C                     DO 240 J = IP1, K
C                        SM = SM + A(I,J)*B(J,JB)
C  240                CONTINUE
C  250                CONTINUE
C                     SM1 = SM
C                     B(I,JB) = (B(I,JB) - SM1)/A(I,I)
C  260             CONTINUE
CC
CC                  COMPLETE COMPUTATION OF SOLUTION VECTOR.
CC                 ..
C                  IF (K .EQ. N) GO TO 290
C                     DO 270 J = KP1, N
C                        B(J,JB) = SZERO
C  270                CONTINUE
C                     DO 280 I = 1, K
C                        CALL DH12(2,I,KP1,N,A(I,1),MDA,G(I),B(1,JB),1,
C     *                            MDB,1)
C  280                CONTINUE
C  290             CONTINUE
CC
CC                   RE-ORDER THE SOLUTION VECTOR TO COMPENSATE FOR THE
CC                   COLUMN INTERCHANGES.
CC                 ..
C                  DO 310 JJ = 1, LDIAG
C                     J = LDIAG + 1 - JJ
C                     IF (IP(J) .EQ. J) GO TO 300
C                        L = IP(J)
C                        TMP = B(L,JB)
C                        B(L,JB) = B(J,JB)
C                        B(J,JB) = TMP
C  300                CONTINUE
C  310             CONTINUE
C  320          CONTINUE
C  330          CONTINUE
C  340       CONTINUE
C  350    CONTINUE
CC        ..
CC         THE SOLUTION VECTORS, X, ARE NOW
CC         IN THE FIRST  N  ROWS OF THE ARRAY B(,).
CC
C         KRANK = K
C  360 CONTINUE
C      RETURN
C      END
C      SUBROUTINE DLPDP (A, MDA, M, N1, N2, PRGOPT, X, WNORM, MODE, WS,
C     +   IS)
CC***BEGIN PROLOGUE  DLPDP
CC***SUBSIDIARY
CC***PURPOSE  Subsidiary to DLSEI
CC***LIBRARY   SLATEC
CC***TYPE      DOUBLE PRECISION (LPDP-S, DLPDP-D)
CC***AUTHOR  Hanson, R. J., (SNLA)
CC           Haskell, K. H., (SNLA)
CC***DESCRIPTION
CC
CC  **** Double Precision version of LPDP ****
CC     DIMENSION A(MDA,N+1),PRGOPT(*),X(N),WS((M+2)*(N+7)),IS(M+N+1),
CC     where N=N1+N2.  This is a slight overestimate for WS(*).
CC
CC     Determine an N1-vector W, and
CC               an N2-vector Z
CC     which minimizes the Euclidean length of W
CC     subject to G*W+H*Z .GE. Y.
CC     This is the least projected distance problem, LPDP.
CC     The matrices G and H are of respective
CC     dimensions M by N1 and M by N2.
CC
CC     Called by subprogram DLSI( ).
CC
CC     The matrix
CC                (G H Y)
CC
CC     occupies rows 1,...,M and cols 1,...,N1+N2+1 of A(*,*).
CC
CC     The solution (W) is returned in X(*).
CC                  (Z)
CC
CC     The value of MODE indicates the status of
CC     the computation after returning to the user.
CC
CC          MODE=1  The solution was successfully obtained.
CC
CC          MODE=2  The inequalities are inconsistent.
CC
CC***SEE ALSO  DLSEI
CC***ROUTINES CALLED  DCOPY, DDOT, DNRM2, DSCAL, DWNNLS
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890531  Changed all specific intrinsics to generic.  (WRB)
CC   891214  Prologue converted to Version 4.0 format.  (BAB)
CC   900328  Added TYPE section.  (WRB)
CC   910408  Updated the AUTHOR section.  (WRB)
CC***END PROLOGUE  DLPDP
CC
C      INTEGER I, IS(*), IW, IX, J, L, M, MDA, MODE, MODEW, N, N1, N2,
C     *     NP1
C      DOUBLE PRECISION A(MDA,*), DDOT, DNRM2, FAC, ONE,
C     *     PRGOPT(*), RNORM, SC, WNORM, WS(*), X(*), YNORM, ZERO
C      SAVE ZERO, ONE, FAC
C      DATA ZERO,ONE /0.0D0,1.0D0/, FAC /0.1D0/
CC***FIRST EXECUTABLE STATEMENT  DLPDP
C      N = N1 + N2
C      MODE = 1
C      IF (M .GT. 0) GO TO 20
C         IF (N .LE. 0) GO TO 10
C            X(1) = ZERO
C            CALL DCOPY(N,X,0,X,1)
C   10    CONTINUE
C         WNORM = ZERO
C      GO TO 200
C   20 CONTINUE
CC        BEGIN BLOCK PERMITTING ...EXITS TO 190
C            NP1 = N + 1
CC
CC           SCALE NONZERO ROWS OF INEQUALITY MATRIX TO HAVE LENGTH ONE.
C            DO 40 I = 1, M
C               SC = DNRM2(N,A(I,1),MDA)
C               IF (SC .EQ. ZERO) GO TO 30
C                  SC = ONE/SC
C                  CALL DSCAL(NP1,SC,A(I,1),MDA)
C   30          CONTINUE
C   40       CONTINUE
CC
CC           SCALE RT.-SIDE VECTOR TO HAVE LENGTH ONE (OR ZERO).
C            YNORM = DNRM2(M,A(1,NP1),1)
C            IF (YNORM .EQ. ZERO) GO TO 50
C               SC = ONE/YNORM
C               CALL DSCAL(M,SC,A(1,NP1),1)
C   50       CONTINUE
CC
CC           SCALE COLS OF MATRIX H.
C            J = N1 + 1
C   60       IF (J .GT. N) GO TO 70
C               SC = DNRM2(M,A(1,J),1)
C               IF (SC .NE. ZERO) SC = ONE/SC
C               CALL DSCAL(M,SC,A(1,J),1)
C               X(J) = SC
C               J = J + 1
C            GO TO 60
C   70       CONTINUE
C            IF (N1 .LE. 0) GO TO 130
CC
CC              COPY TRANSPOSE OF (H G Y) TO WORK ARRAY WS(*).
C               IW = 0
C               DO 80 I = 1, M
CC
CC                 MOVE COL OF TRANSPOSE OF H INTO WORK ARRAY.
C                  CALL DCOPY(N2,A(I,N1+1),MDA,WS(IW+1),1)
C                  IW = IW + N2
CC
CC                 MOVE COL OF TRANSPOSE OF G INTO WORK ARRAY.
C                  CALL DCOPY(N1,A(I,1),MDA,WS(IW+1),1)
C                  IW = IW + N1
CC
CC                 MOVE COMPONENT OF VECTOR Y INTO WORK ARRAY.
C                  WS(IW+1) = A(I,NP1)
C                  IW = IW + 1
C   80          CONTINUE
C               WS(IW+1) = ZERO
C               CALL DCOPY(N,WS(IW+1),0,WS(IW+1),1)
C               IW = IW + N
C               WS(IW+1) = ONE
C               IW = IW + 1
CC
CC              SOLVE EU=F SUBJECT TO (TRANSPOSE OF H)U=0, U.GE.0.  THE
CC              MATRIX E = TRANSPOSE OF (G Y), AND THE (N+1)-VECTOR
CC              F = TRANSPOSE OF (0,...,0,1).
C               IX = IW + 1
C               IW = IW + M
CC
CC              DO NOT CHECK LENGTHS OF WORK ARRAYS IN THIS USAGE OF
CC              DWNNLS( ).
C               IS(1) = 0
C               IS(2) = 0
C               CALL DWNNLS(WS,NP1,N2,NP1-N2,M,0,PRGOPT,WS(IX),RNORM,
C     *                     MODEW,IS,WS(IW+1))
CC
CC              COMPUTE THE COMPONENTS OF THE SOLN DENOTED ABOVE BY W.
C               SC = ONE - DDOT(M,A(1,NP1),1,WS(IX),1)
C               IF (ONE + FAC*ABS(SC) .EQ. ONE .OR. RNORM .LE. ZERO)
C     *            GO TO 110
C                  SC = ONE/SC
C                  DO 90 J = 1, N1
C                     X(J) = SC*DDOT(M,A(1,J),1,WS(IX),1)
C   90             CONTINUE
CC
CC                 COMPUTE THE VECTOR Q=Y-GW.  OVERWRITE Y WITH THIS
CC                 VECTOR.
C                  DO 100 I = 1, M
C                     A(I,NP1) = A(I,NP1) - DDOT(N1,A(I,1),MDA,X,1)
C  100             CONTINUE
C               GO TO 120
C  110          CONTINUE
C                  MODE = 2
CC        .........EXIT
C                  GO TO 190
C  120          CONTINUE
C  130       CONTINUE
C            IF (N2 .LE. 0) GO TO 180
CC
CC              COPY TRANSPOSE OF (H Q) TO WORK ARRAY WS(*).
C               IW = 0
C               DO 140 I = 1, M
C                  CALL DCOPY(N2,A(I,N1+1),MDA,WS(IW+1),1)
C                  IW = IW + N2
C                  WS(IW+1) = A(I,NP1)
C                  IW = IW + 1
C  140          CONTINUE
C               WS(IW+1) = ZERO
C               CALL DCOPY(N2,WS(IW+1),0,WS(IW+1),1)
C               IW = IW + N2
C               WS(IW+1) = ONE
C               IW = IW + 1
C               IX = IW + 1
C               IW = IW + M
CC
CC              SOLVE RV=S SUBJECT TO V.GE.0.  THE MATRIX R =(TRANSPOSE
CC              OF (H Q)), WHERE Q=Y-GW.  THE (N2+1)-VECTOR S =(TRANSPOSE
CC              OF (0,...,0,1)).
CC
CC              DO NOT CHECK LENGTHS OF WORK ARRAYS IN THIS USAGE OF
CC              DWNNLS( ).
C               IS(1) = 0
C               IS(2) = 0
C               CALL DWNNLS(WS,N2+1,0,N2+1,M,0,PRGOPT,WS(IX),RNORM,MODEW,
C     *                     IS,WS(IW+1))
CC
CC              COMPUTE THE COMPONENTS OF THE SOLN DENOTED ABOVE BY Z.
C               SC = ONE - DDOT(M,A(1,NP1),1,WS(IX),1)
C               IF (ONE + FAC*ABS(SC) .EQ. ONE .OR. RNORM .LE. ZERO)
C     *            GO TO 160
C                  SC = ONE/SC
C                  DO 150 J = 1, N2
C                     L = N1 + J
C                     X(L) = SC*DDOT(M,A(1,L),1,WS(IX),1)*X(L)
C  150             CONTINUE
C               GO TO 170
C  160          CONTINUE
C                  MODE = 2
CC        .........EXIT
C                  GO TO 190
C  170          CONTINUE
C  180       CONTINUE
CC
CC           ACCOUNT FOR SCALING OF RT.-SIDE VECTOR IN SOLUTION.
C            CALL DSCAL(N,YNORM,X,1)
C            WNORM = DNRM2(N1,X,1)
C  190    CONTINUE
C  200 CONTINUE
C      RETURN
C      END
C      SUBROUTINE DLSEI (W, MDW, ME, MA, MG, N, PRGOPT, X, RNORME,
C     +   RNORML, MODE, WS, IP)
CC***BEGIN PROLOGUE  DLSEI
CC***PURPOSE  Solve a linearly constrained least squares problem with
CC            equality and inequality constraints, and optionally compute
CC            a covariance matrix.
CC***LIBRARY   SLATEC
CC***CATEGORY  K1A2A, D9
CC***TYPE      DOUBLE PRECISION (LSEI-S, DLSEI-D)
CC***KEYWORDS  CONSTRAINED LEAST SQUARES, CURVE FITTING, DATA FITTING,
CC             EQUALITY CONSTRAINTS, INEQUALITY CONSTRAINTS,
CC             QUADRATIC PROGRAMMING
CC***AUTHOR  Hanson, R. J., (SNLA)
CC           Haskell, K. H., (SNLA)
CC***DESCRIPTION
CC
CC     Abstract
CC
CC     This subprogram solves a linearly constrained least squares
CC     problem with both equality and inequality constraints, and, if the
CC     user requests, obtains a covariance matrix of the solution
CC     parameters.
CC
CC     Suppose there are given matrices E, A and G of respective
CC     dimensions ME by N, MA by N and MG by N, and vectors F, B and H of
CC     respective lengths ME, MA and MG.  This subroutine solves the
CC     linearly constrained least squares problem
CC
CC                   EX = F, (E ME by N) (equations to be exactly
CC                                       satisfied)
CC                   AX = B, (A MA by N) (equations to be
CC                                       approximately satisfied,
CC                                       least squares sense)
CC                   GX .GE. H,(G MG by N) (inequality constraints)
CC
CC     The inequalities GX .GE. H mean that every component of the
CC     product GX must be .GE. the corresponding component of H.
CC
CC     In case the equality constraints cannot be satisfied, a
CC     generalized inverse solution residual vector length is obtained
CC     for F-EX.  This is the minimal length possible for F-EX.
CC
CC     Any values ME .GE. 0, MA .GE. 0, or MG .GE. 0 are permitted.  The
CC     rank of the matrix E is estimated during the computation.  We call
CC     this value KRANKE.  It is an output parameter in IP(1) defined
CC     below.  Using a generalized inverse solution of EX=F, a reduced
CC     least squares problem with inequality constraints is obtained.
CC     The tolerances used in these tests for determining the rank
CC     of E and the rank of the reduced least squares problem are
CC     given in Sandia Tech. Rept. SAND-78-1290.  They can be
CC     modified by the user if new values are provided in
CC     the option list of the array PRGOPT(*).
CC
CC     The user must dimension all arrays appearing in the call list..
CC     W(MDW,N+1),PRGOPT(*),X(N),WS(2*(ME+N)+K+(MG+2)*(N+7)),IP(MG+2*N+2)
CC     where K=MAX(MA+MG,N).  This allows for a solution of a range of
CC     problems in the given working space.  The dimension of WS(*)
CC     given is a necessary overestimate.  Once a particular problem
CC     has been run, the output parameter IP(3) gives the actual
CC     dimension required for that problem.
CC
CC     The parameters for DLSEI( ) are
CC
CC     Input.. All TYPE REAL variables are DOUBLE PRECISION
CC
CC     W(*,*),MDW,   The array W(*,*) is doubly subscripted with
CC     ME,MA,MG,N    first dimensioning parameter equal to MDW.
CC                   For this discussion let us call M = ME+MA+MG.  Then
CC                   MDW must satisfy MDW .GE. M.  The condition
CC                   MDW .LT. M is an error.
CC
CC                   The array W(*,*) contains the matrices and vectors
CC
CC                                  (E  F)
CC                                  (A  B)
CC                                  (G  H)
CC
CC                   in rows and columns 1,...,M and 1,...,N+1
CC                   respectively.
CC
CC                   The integers ME, MA, and MG are the
CC                   respective matrix row dimensions
CC                   of E, A and G.  Each matrix has N columns.
CC
CC     PRGOPT(*)    This real-valued array is the option vector.
CC                  If the user is satisfied with the nominal
CC                  subprogram features set
CC
CC                  PRGOPT(1)=1 (or PRGOPT(1)=1.0)
CC
CC                  Otherwise PRGOPT(*) is a linked list consisting of
CC                  groups of data of the following form
CC
CC                  LINK
CC                  KEY
CC                  DATA SET
CC
CC                  The parameters LINK and KEY are each one word.
CC                  The DATA SET can be comprised of several words.
CC                  The number of items depends on the value of KEY.
CC                  The value of LINK points to the first
CC                  entry of the next group of data within
CC                  PRGOPT(*).  The exception is when there are
CC                  no more options to change.  In that
CC                  case, LINK=1 and the values KEY and DATA SET
CC                  are not referenced.  The general layout of
CC                  PRGOPT(*) is as follows.
CC
CC               ...PRGOPT(1) = LINK1 (link to first entry of next group)
CC               .  PRGOPT(2) = KEY1 (key to the option change)
CC               .  PRGOPT(3) = data value (data value for this change)
CC               .       .
CC               .       .
CC               .       .
CC               ...PRGOPT(LINK1)   = LINK2 (link to the first entry of
CC               .                       next group)
CC               .  PRGOPT(LINK1+1) = KEY2 (key to the option change)
CC               .  PRGOPT(LINK1+2) = data value
CC               ...     .
CC               .       .
CC               .       .
CC               ...PRGOPT(LINK) = 1 (no more options to change)
CC
CC                  Values of LINK that are nonpositive are errors.
CC                  A value of LINK .GT. NLINK=100000 is also an error.
CC                  This helps prevent using invalid but positive
CC                  values of LINK that will probably extend
CC                  beyond the program limits of PRGOPT(*).
CC                  Unrecognized values of KEY are ignored.  The
CC                  order of the options is arbitrary and any number
CC                  of options can be changed with the following
CC                  restriction.  To prevent cycling in the
CC                  processing of the option array, a count of the
CC                  number of options changed is maintained.
CC                  Whenever this count exceeds NOPT=1000, an error
CC                  message is printed and the subprogram returns.
CC
CC                  Options..
CC
CC                  KEY=1
CC                         Compute in W(*,*) the N by N
CC                  covariance matrix of the solution variables
CC                  as an output parameter.  Nominally the
CC                  covariance matrix will not be computed.
CC                  (This requires no user input.)
CC                  The data set for this option is a single value.
CC                  It must be nonzero when the covariance matrix
CC                  is desired.  If it is zero, the covariance
CC                  matrix is not computed.  When the covariance matrix
CC                  is computed, the first dimensioning parameter
CC                  of the array W(*,*) must satisfy MDW .GE. MAX(M,N).
CC
CC                  KEY=10
CC                         Suppress scaling of the inverse of the
CC                  normal matrix by the scale factor RNORM**2/
CC                  MAX(1, no. of degrees of freedom).  This option
CC                  only applies when the option for computing the
CC                  covariance matrix (KEY=1) is used.  With KEY=1 and
CC                  KEY=10 used as options the unscaled inverse of the
CC                  normal matrix is returned in W(*,*).
CC                  The data set for this option is a single value.
CC                  When it is nonzero no scaling is done.  When it is
CC                  zero scaling is done.  The nominal case is to do
CC                  scaling so if option (KEY=1) is used alone, the
CC                  matrix will be scaled on output.
CC
CC                  KEY=2
CC                         Scale the nonzero columns of the
CC                         entire data matrix.
CC                  (E)
CC                  (A)
CC                  (G)
CC
CC                  to have length one.  The data set for this
CC                  option is a single value.  It must be
CC                  nonzero if unit length column scaling
CC                  is desired.
CC
CC                  KEY=3
CC                         Scale columns of the entire data matrix
CC                  (E)
CC                  (A)
CC                  (G)
CC
CC                  with a user-provided diagonal matrix.
CC                  The data set for this option consists
CC                  of the N diagonal scaling factors, one for
CC                  each matrix column.
CC
CC                  KEY=4
CC                         Change the rank determination tolerance for
CC                  the equality constraint equations from
CC                  the nominal value of SQRT(DRELPR).  This quantity can
CC                  be no smaller than DRELPR, the arithmetic-
CC                  storage precision.  The quantity DRELPR is the
CC                  largest positive number such that T=1.+DRELPR
CC                  satisfies T .EQ. 1.  The quantity used
CC                  here is internally restricted to be at
CC                  least DRELPR.  The data set for this option
CC                  is the new tolerance.
CC
CC                  KEY=5
CC                         Change the rank determination tolerance for
CC                  the reduced least squares equations from
CC                  the nominal value of SQRT(DRELPR).  This quantity can
CC                  be no smaller than DRELPR, the arithmetic-
CC                  storage precision.  The quantity used
CC                  here is internally restricted to be at
CC                  least DRELPR.  The data set for this option
CC                  is the new tolerance.
CC
CC                  For example, suppose we want to change
CC                  the tolerance for the reduced least squares
CC                  problem, compute the covariance matrix of
CC                  the solution parameters, and provide
CC                  column scaling for the data matrix.  For
CC                  these options the dimension of PRGOPT(*)
CC                  must be at least N+9.  The Fortran statements
CC                  defining these options would be as follows:
CC
CC                  PRGOPT(1)=4 (link to entry 4 in PRGOPT(*))
CC                  PRGOPT(2)=1 (covariance matrix key)
CC                  PRGOPT(3)=1 (covariance matrix wanted)
CC
CC                  PRGOPT(4)=7 (link to entry 7 in PRGOPT(*))
CC                  PRGOPT(5)=5 (least squares equas.  tolerance key)
CC                  PRGOPT(6)=... (new value of the tolerance)
CC
CC                  PRGOPT(7)=N+9 (link to entry N+9 in PRGOPT(*))
CC                  PRGOPT(8)=3 (user-provided column scaling key)
CC
CC                  CALL DCOPY (N, D, 1, PRGOPT(9), 1)  (Copy the N
CC                    scaling factors from the user array D(*)
CC                    to PRGOPT(9)-PRGOPT(N+8))
CC
CC                  PRGOPT(N+9)=1 (no more options to change)
CC
CC                  The contents of PRGOPT(*) are not modified
CC                  by the subprogram.
CC                  The options for WNNLS( ) can also be included
CC                  in this array.  The values of KEY recognized
CC                  by WNNLS( ) are 6, 7 and 8.  Their functions
CC                  are documented in the usage instructions for
CC                  subroutine WNNLS( ).  Normally these options
CC                  do not need to be modified when using DLSEI( ).
CC
CC     IP(1),       The amounts of working storage actually
CC     IP(2)        allocated for the working arrays WS(*) and
CC                  IP(*), respectively.  These quantities are
CC                  compared with the actual amounts of storage
CC                  needed by DLSEI( ).  Insufficient storage
CC                  allocated for either WS(*) or IP(*) is an
CC                  error.  This feature was included in DLSEI( )
CC                  because miscalculating the storage formulas
CC                  for WS(*) and IP(*) might very well lead to
CC                  subtle and hard-to-find execution errors.
CC
CC                  The length of WS(*) must be at least
CC
CC                  LW = 2*(ME+N)+K+(MG+2)*(N+7)
CC
CC                  where K = max(MA+MG,N)
CC                  This test will not be made if IP(1).LE.0.
CC
CC                  The length of IP(*) must be at least
CC
CC                  LIP = MG+2*N+2
CC                  This test will not be made if IP(2).LE.0.
CC
CC     Output.. All TYPE REAL variables are DOUBLE PRECISION
CC
CC     X(*),RNORME,  The array X(*) contains the solution parameters
CC     RNORML        if the integer output flag MODE = 0 or 1.
CC                   The definition of MODE is given directly below.
CC                   When MODE = 0 or 1, RNORME and RNORML
CC                   respectively contain the residual vector
CC                   Euclidean lengths of F - EX and B - AX.  When
CC                   MODE=1 the equality constraint equations EX=F
CC                   are contradictory, so RNORME .NE. 0.  The residual
CC                   vector F-EX has minimal Euclidean length.  For
CC                   MODE .GE. 2, none of these parameters is defined.
CC
CC     MODE          Integer flag that indicates the subprogram
CC                   status after completion.  If MODE .GE. 2, no
CC                   solution has been computed.
CC
CC                   MODE =
CC
CC                   0  Both equality and inequality constraints
CC                      are compatible and have been satisfied.
CC
CC                   1  Equality constraints are contradictory.
CC                      A generalized inverse solution of EX=F was used
CC                      to minimize the residual vector length F-EX.
CC                      In this sense, the solution is still meaningful.
CC
CC                   2  Inequality constraints are contradictory.
CC
CC                   3  Both equality and inequality constraints
CC                      are contradictory.
CC
CC                   The following interpretation of
CC                   MODE=1,2 or 3 must be made.  The
CC                   sets consisting of all solutions
CC                   of the equality constraints EX=F
CC                   and all vectors satisfying GX .GE. H
CC                   have no points in common.  (In
CC                   particular this does not say that
CC                   each individual set has no points
CC                   at all, although this could be the
CC                   case.)
CC
CC                   4  Usage error occurred.  The value
CC                      of MDW is .LT. ME+MA+MG, MDW is
CC                      .LT. N and a covariance matrix is
CC                      requested, or the option vector
CC                      PRGOPT(*) is not properly defined,
CC                      or the lengths of the working arrays
CC                      WS(*) and IP(*), when specified in
CC                      IP(1) and IP(2) respectively, are not
CC                      long enough.
CC
CC     W(*,*)        The array W(*,*) contains the N by N symmetric
CC                   covariance matrix of the solution parameters,
CC                   provided this was requested on input with
CC                   the option vector PRGOPT(*) and the output
CC                   flag is returned with MODE = 0 or 1.
CC
CC     IP(*)         The integer working array has three entries
CC                   that provide rank and working array length
CC                   information after completion.
CC
CC                      IP(1) = rank of equality constraint
CC                              matrix.  Define this quantity
CC                              as KRANKE.
CC
CC                      IP(2) = rank of reduced least squares
CC                              problem.
CC
CC                      IP(3) = the amount of storage in the
CC                              working array WS(*) that was
CC                              actually used by the subprogram.
CC                              The formula given above for the length
CC                              of WS(*) is a necessary overestimate.
CC                              If exactly the same problem matrices
CC                              are used in subsequent executions,
CC                              the declared dimension of WS(*) can
CC                              be reduced to this output value.
CC     User Designated
CC     Working Arrays..
CC
CC     WS(*),IP(*)              These are respectively type real
CC                              and type integer working arrays.
CC                              Their required minimal lengths are
CC                              given above.
CC
CC***REFERENCES  K. H. Haskell and R. J. Hanson, An algorithm for
CC                 linear least squares problems with equality and
CC                 nonnegativity constraints, Report SAND77-0552, Sandia
CC                 Laboratories, June 1978.
CC               K. H. Haskell and R. J. Hanson, Selected algorithms for
CC                 the linearly constrained least squares problem - a
CC                 users guide, Report SAND78-1290, Sandia Laboratories,
CC                 August 1979.
CC               K. H. Haskell and R. J. Hanson, An algorithm for
CC                 linear least squares problems with equality and
CC                 nonnegativity constraints, Mathematical Programming
CC                 21 (1981), pp. 98-118.
CC               R. J. Hanson and K. H. Haskell, Two algorithms for the
CC                 linearly constrained least squares problem, ACM
CC                 Transactions on Mathematical Software, September 1982.
CC***ROUTINES CALLED  D1MACH, DASUM, DAXPY, DCOPY, DDOT, DH12, DLSI,
CC                    DNRM2, DSCAL, DSWAP, XERMSG
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890531  Changed all specific intrinsics to generic.  (WRB)
CC   890618  Completely restructured and extensively revised (WRB & RWC)
CC   890831  REVISION DATE from Version 3.2
CC   891214  Prologue converted to Version 4.0 format.  (BAB)
CC   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
CC   900510  Convert XERRWV calls to XERMSG calls.  (RWC)
CC   900604  DP version created from SP version.  (RWC)
CC   920501  Reformatted the REFERENCES section.  (WRB)
CC***END PROLOGUE  DLSEI
C      INTEGER IP(3), MA, MDW, ME, MG, MODE, N
C      DOUBLE PRECISION PRGOPT(*), RNORME, RNORML, W(MDW,*), WS(*), X(*)
CC
C      EXTERNAL D1MACH, DASUM, DAXPY, DCOPY, DDOT, DH12, DLSI, DNRM2,
C     *   DSCAL, DSWAP, XERMSG
C      DOUBLE PRECISION D1MACH, DASUM, DDOT, DNRM2
CC
C      DOUBLE PRECISION DRELPR, ENORM, FNORM, GAM, RB, RN, RNMAX, SIZE,
C     *   SN, SNMAX, T, TAU, UJ, UP, VJ, XNORM, XNRME
C      INTEGER I, IMAX, J, JP1, K, KEY, KRANKE, LAST, LCHK, LINK, M,
C     *   MAPKE1, MDEQC, MEND, MEP1, N1, N2, NEXT, NLINK, NOPT, NP1,
C     *   NTIMES
C      LOGICAL COV, FIRST
C      CHARACTER*8 XERN1, XERN2, XERN3, XERN4
C      SAVE FIRST, DRELPR
CC
C      DATA FIRST /.TRUE./
CC***FIRST EXECUTABLE STATEMENT  DLSEI
CC
CC     Set the nominal tolerance used in the code for the equality
CC     constraint equations.
CC
C      IF (FIRST) DRELPR = D1MACH(4)
C      FIRST = .FALSE.
C      TAU = SQRT(DRELPR)
CC
CC     Check that enough storage was allocated in WS(*) and IP(*).
CC
C      MODE = 4
C      IF (MIN(N,ME,MA,MG) .LT. 0) THEN
C         WRITE (XERN1, '(I8)') N
C         WRITE (XERN2, '(I8)') ME
C         WRITE (XERN3, '(I8)') MA
C         WRITE (XERN4, '(I8)') MG
C         CALL XERMSG ('SLATEC', 'LSEI', 'ALL OF THE VARIABLES N, ME,' //
C     *      ' MA, MG MUST BE .GE. 0$$ENTERED ROUTINE WITH' //
C     *      '$$N  = ' // XERN1 //
C     *      '$$ME = ' // XERN2 //
C     *      '$$MA = ' // XERN3 //
C     *      '$$MG = ' // XERN4, 2, 1)
C         RETURN
C      ENDIF
CC
C      IF (IP(1).GT.0) THEN
C         LCHK = 2*(ME+N) + MAX(MA+MG,N) + (MG+2)*(N+7)
C         IF (IP(1).LT.LCHK) THEN
C            WRITE (XERN1, '(I8)') LCHK
C            CALL XERMSG ('SLATEC', 'DLSEI', 'INSUFFICIENT STORAGE ' //
C     *         'ALLOCATED FOR WS(*), NEED LW = ' // XERN1, 2, 1)
C            RETURN
C         ENDIF
C      ENDIF
CC
C      IF (IP(2).GT.0) THEN
C         LCHK = MG + 2*N + 2
C         IF (IP(2).LT.LCHK) THEN
C            WRITE (XERN1, '(I8)') LCHK
C            CALL XERMSG ('SLATEC', 'DLSEI', 'INSUFFICIENT STORAGE ' //
C     *         'ALLOCATED FOR IP(*), NEED LIP = ' // XERN1, 2, 1)
C            RETURN
C         ENDIF
C      ENDIF
CC
CC     Compute number of possible right multiplying Householder
CC     transformations.
CC
C      M = ME + MA + MG
C      IF (N.LE.0 .OR. M.LE.0) THEN
C         MODE = 0
C         RNORME = 0
C         RNORML = 0
C         RETURN
C      ENDIF
CC
C      IF (MDW.LT.M) THEN
C         CALL XERMSG ('SLATEC', 'DLSEI', 'MDW.LT.ME+MA+MG IS AN ERROR',
C     +      2, 1)
C         RETURN
C      ENDIF
CC
C      NP1 = N + 1
C      KRANKE = MIN(ME,N)
C      N1 = 2*KRANKE + 1
C      N2 = N1 + N
CC
CC     Set nominal values.
CC
CC     The nominal column scaling used in the code is
CC     the identity scaling.
CC
C      CALL DCOPY (N, 1.D0, 0, WS(N1), 1)
CC
CC     No covariance matrix is nominally computed.
CC
C      COV = .FALSE.
CC
CC     Process option vector.
CC     Define bound for number of options to change.
CC
C      NOPT = 1000
C      NTIMES = 0
CC
CC     Define bound for positive values of LINK.
CC
C      NLINK = 100000
C      LAST = 1
C      LINK = PRGOPT(1)
C      IF (LINK.EQ.0 .OR. LINK.GT.NLINK) THEN
C         CALL XERMSG ('SLATEC', 'DLSEI',
C     +      'THE OPTION VECTOR IS UNDEFINED', 2, 1)
C         RETURN
C      ENDIF
CC
C  100 IF (LINK.GT.1) THEN
C         NTIMES = NTIMES + 1
C         IF (NTIMES.GT.NOPT) THEN
C            CALL XERMSG ('SLATEC', 'DLSEI',
C     +         'THE LINKS IN THE OPTION VECTOR ARE CYCLING.', 2, 1)
C            RETURN
C         ENDIF
CC
C         KEY = PRGOPT(LAST+1)
C         IF (KEY.EQ.1) THEN
C            COV = PRGOPT(LAST+2) .NE. 0.D0
C         ELSEIF (KEY.EQ.2 .AND. PRGOPT(LAST+2).NE.0.D0) THEN
C            DO 110 J = 1,N
C               T = DNRM2(M,W(1,J),1)
C               IF (T.NE.0.D0) T = 1.D0/T
C               WS(J+N1-1) = T
C  110       CONTINUE
C         ELSEIF (KEY.EQ.3) THEN
C            CALL DCOPY (N, PRGOPT(LAST+2), 1, WS(N1), 1)
C         ELSEIF (KEY.EQ.4) THEN
C            TAU = MAX(DRELPR,PRGOPT(LAST+2))
C         ENDIF
CC
C         NEXT = PRGOPT(LINK)
C         IF (NEXT.LE.0 .OR. NEXT.GT.NLINK) THEN
C         CALL XERMSG ('SLATEC', 'DLSEI',
C     +      'THE OPTION VECTOR IS UNDEFINED', 2, 1)
C            RETURN
C         ENDIF
CC
C         LAST = LINK
C         LINK = NEXT
C         GO TO 100
C      ENDIF
CC
C      DO 120 J = 1,N
C         CALL DSCAL (M, WS(N1+J-1), W(1,J), 1)
C  120 CONTINUE
CC
C      IF (COV .AND. MDW.LT.N) THEN
C         CALL XERMSG ('SLATEC', 'DLSEI',
C     +      'MDW .LT. N WHEN COV MATRIX NEEDED, IS AN ERROR', 2, 1)
C         RETURN
C      ENDIF
CC
CC     Problem definition and option vector OK.
CC
C      MODE = 0
CC
CC     Compute norm of equality constraint matrix and right side.
CC
C      ENORM = 0.D0
C      DO 130 J = 1,N
C         ENORM = MAX(ENORM,DASUM(ME,W(1,J),1))
C  130 CONTINUE
CC
C      FNORM = DASUM(ME,W(1,NP1),1)
C      SNMAX = 0.D0
C      RNMAX = 0.D0
C      DO 150 I = 1,KRANKE
CC
CC        Compute maximum ratio of vector lengths. Partition is at
CC        column I.
CC
C         DO 140 K = I,ME
C            SN = DDOT(N-I+1,W(K,I),MDW,W(K,I),MDW)
C            RN = DDOT(I-1,W(K,1),MDW,W(K,1),MDW)
C            IF (RN.EQ.0.D0 .AND. SN.GT.SNMAX) THEN
C               SNMAX = SN
C               IMAX = K
C            ELSEIF (K.EQ.I .OR. SN*RNMAX.GT.RN*SNMAX) THEN
C               SNMAX = SN
C               RNMAX = RN
C               IMAX = K
C            ENDIF
C  140    CONTINUE
CC
CC        Interchange rows if necessary.
CC
C         IF (I.NE.IMAX) CALL DSWAP (NP1, W(I,1), MDW, W(IMAX,1), MDW)
C         IF (SNMAX.GT.RNMAX*TAU**2) THEN
CC
CC        Eliminate elements I+1,...,N in row I.
CC
C            CALL DH12 (1, I, I+1, N, W(I,1), MDW, WS(I), W(I+1,1), MDW,
C     +                1, M-I)
C         ELSE
C            KRANKE = I - 1
C            GO TO 160
C         ENDIF
C  150 CONTINUE
CC
CC     Save diagonal terms of lower trapezoidal matrix.
CC
C  160 CALL DCOPY (KRANKE, W, MDW+1, WS(KRANKE+1), 1)
CC
CC     Use Householder transformation from left to achieve
CC     KRANKE by KRANKE upper triangular form.
CC
C      IF (KRANKE.LT.ME) THEN
C         DO 170 K = KRANKE,1,-1
CC
CC           Apply transformation to matrix cols. 1,...,K-1.
CC
C            CALL DH12 (1, K, KRANKE+1, ME, W(1,K), 1, UP, W, 1, MDW,
C     *         K-1)
CC
CC           Apply to rt side vector.
CC
C            CALL DH12 (2, K, KRANKE+1, ME, W(1,K), 1, UP, W(1,NP1), 1,
C     +         1, 1)
C  170    CONTINUE
C      ENDIF
CC
CC     Solve for variables 1,...,KRANKE in new coordinates.
CC
C      CALL DCOPY (KRANKE, W(1, NP1), 1, X, 1)
C      DO 180 I = 1,KRANKE
C         X(I) = (X(I)-DDOT(I-1,W(I,1),MDW,X,1))/W(I,I)
C  180 CONTINUE
CC
CC     Compute residuals for reduced problem.
CC
C      MEP1 = ME + 1
C      RNORML = 0.D0
C      DO 190 I = MEP1,M
C         W(I,NP1) = W(I,NP1) - DDOT(KRANKE,W(I,1),MDW,X,1)
C         SN = DDOT(KRANKE,W(I,1),MDW,W(I,1),MDW)
C         RN = DDOT(N-KRANKE,W(I,KRANKE+1),MDW,W(I,KRANKE+1),MDW)
C         IF (RN.LE.SN*TAU**2 .AND. KRANKE.LT.N)
C     *      CALL DCOPY (N-KRANKE, 0.D0, 0, W(I,KRANKE+1), MDW)
C  190 CONTINUE
CC
CC     Compute equality constraint equations residual length.
CC
C      RNORME = DNRM2(ME-KRANKE,W(KRANKE+1,NP1),1)
CC
CC     Move reduced problem data upward if KRANKE.LT.ME.
CC
C      IF (KRANKE.LT.ME) THEN
C         DO 200 J = 1,NP1
C            CALL DCOPY (M-ME, W(ME+1,J), 1, W(KRANKE+1,J), 1)
C  200    CONTINUE
C      ENDIF
CC
CC     Compute solution of reduced problem.
CC
C      CALL DLSI(W(KRANKE+1, KRANKE+1), MDW, MA, MG, N-KRANKE, PRGOPT,
C     +         X(KRANKE+1), RNORML, MODE, WS(N2), IP(2))
CC
CC     Test for consistency of equality constraints.
CC
C      IF (ME.GT.0) THEN
C         MDEQC = 0
C         XNRME = DASUM(KRANKE,W(1,NP1),1)
C         IF (RNORME.GT.TAU*(ENORM*XNRME+FNORM)) MDEQC = 1
C         MODE = MODE + MDEQC
CC
CC        Check if solution to equality constraints satisfies inequality
CC        constraints when there are no degrees of freedom left.
CC
C         IF (KRANKE.EQ.N .AND. MG.GT.0) THEN
C            XNORM = DASUM(N,X,1)
C            MAPKE1 = MA + KRANKE + 1
C            MEND = MA + KRANKE + MG
C            DO 210 I = MAPKE1,MEND
C               SIZE = DASUM(N,W(I,1),MDW)*XNORM + ABS(W(I,NP1))
C               IF (W(I,NP1).GT.TAU*SIZE) THEN
C                  MODE = MODE + 2
C                  GO TO 290
C               ENDIF
C  210       CONTINUE
C         ENDIF
C      ENDIF
CC
CC     Replace diagonal terms of lower trapezoidal matrix.
CC
C      IF (KRANKE.GT.0) THEN
C         CALL DCOPY (KRANKE, WS(KRANKE+1), 1, W, MDW+1)
CC
CC        Reapply transformation to put solution in original coordinates.
CC
C         DO 220 I = KRANKE,1,-1
C            CALL DH12 (2, I, I+1, N, W(I,1), MDW, WS(I), X, 1, 1, 1)
C  220    CONTINUE
CC
CC        Compute covariance matrix of equality constrained problem.
CC
C         IF (COV) THEN
C            DO 270 J = MIN(KRANKE,N-1),1,-1
C               RB = WS(J)*W(J,J)
C               IF (RB.NE.0.D0) RB = 1.D0/RB
C               JP1 = J + 1
C               DO 230 I = JP1,N
C                  W(I,J) = RB*DDOT(N-J,W(I,JP1),MDW,W(J,JP1),MDW)
C  230          CONTINUE
CC
C               GAM = 0.5D0*RB*DDOT(N-J,W(JP1,J),1,W(J,JP1),MDW)
C               CALL DAXPY (N-J, GAM, W(J,JP1), MDW, W(JP1,J), 1)
C               DO 250 I = JP1,N
C                  DO 240 K = I,N
C                     W(I,K) = W(I,K) + W(J,I)*W(K,J) + W(I,J)*W(J,K)
C                     W(K,I) = W(I,K)
C  240             CONTINUE
C  250          CONTINUE
C               UJ = WS(J)
C               VJ = GAM*UJ
C               W(J,J) = UJ*VJ + UJ*VJ
C               DO 260 I = JP1,N
C                  W(J,I) = UJ*W(I,J) + VJ*W(J,I)
C  260          CONTINUE
C               CALL DCOPY (N-J, W(J, JP1), MDW, W(JP1,J), 1)
C  270       CONTINUE
C         ENDIF
C      ENDIF
CC
CC     Apply the scaling to the covariance matrix.
CC
C      IF (COV) THEN
C         DO 280 I = 1,N
C            CALL DSCAL (N, WS(I+N1-1), W(I,1), MDW)
C            CALL DSCAL (N, WS(I+N1-1), W(1,I), 1)
C  280    CONTINUE
C      ENDIF
CC
CC     Rescale solution vector.
CC
C  290 IF (MODE.LE.1) THEN
C         DO 300 J = 1,N
C            X(J) = X(J)*WS(N1+J-1)
C  300    CONTINUE
C      ENDIF
CC
C      IP(1) = KRANKE
C      IP(3) = IP(3) + 2*KRANKE + N
C      RETURN
C      END
C      SUBROUTINE DLSI (W, MDW, MA, MG, N, PRGOPT, X, RNORM, MODE, WS,
C     +   IP)
CC***BEGIN PROLOGUE  DLSI
CC***SUBSIDIARY
CC***PURPOSE  Subsidiary to DLSEI
CC***LIBRARY   SLATEC
CC***TYPE      DOUBLE PRECISION (LSI-S, DLSI-D)
CC***AUTHOR  Hanson, R. J., (SNLA)
CC***DESCRIPTION
CC
CC     This is a companion subprogram to DLSEI.  The documentation for
CC     DLSEI has complete usage instructions.
CC
CC     Solve..
CC              AX = B,  A  MA by N  (least squares equations)
CC     subject to..
CC
CC              GX.GE.H, G  MG by N  (inequality constraints)
CC
CC     Input..
CC
CC      W(*,*) contains  (A B) in rows 1,...,MA+MG, cols 1,...,N+1.
CC                       (G H)
CC
CC     MDW,MA,MG,N
CC              contain (resp) var. dimension of W(*,*),
CC              and matrix dimensions.
CC
CC     PRGOPT(*),
CC              Program option vector.
CC
CC     OUTPUT..
CC
CC      X(*),RNORM
CC
CC              Solution vector(unless MODE=2), length of AX-B.
CC
CC      MODE
CC              =0   Inequality constraints are compatible.
CC              =2   Inequality constraints contradictory.
CC
CC      WS(*),
CC              Working storage of dimension K+N+(MG+2)*(N+7),
CC              where K=MAX(MA+MG,N).
CC      IP(MG+2*N+1)
CC              Integer working storage
CC
CC***ROUTINES CALLED  D1MACH, DASUM, DAXPY, DCOPY, DDOT, DH12, DHFTI,
CC                    DLPDP, DSCAL, DSWAP
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890531  Changed all specific intrinsics to generic.  (WRB)
CC   890618  Completely restructured and extensively revised (WRB & RWC)
CC   891214  Prologue converted to Version 4.0 format.  (BAB)
CC   900328  Added TYPE section.  (WRB)
CC   900604  DP version created from SP version.  (RWC)
CC   920422  Changed CALL to DHFTI to include variable MA.  (WRB)
CC***END PROLOGUE  DLSI
C      INTEGER IP(*), MA, MDW, MG, MODE, N
C      DOUBLE PRECISION PRGOPT(*), RNORM, W(MDW,*), WS(*), X(*)
CC
C      EXTERNAL D1MACH, DASUM, DAXPY, DCOPY, DDOT, DH12, DHFTI, DLPDP,
C     *   DSCAL, DSWAP
C      DOUBLE PRECISION D1MACH, DASUM, DDOT
CC
C      DOUBLE PRECISION ANORM, DRELPR, FAC, GAM, RB, TAU, TOL, XNORM
C      INTEGER I, J, K, KEY, KRANK, KRM1, KRP1, L, LAST, LINK, M, MAP1,
C     *   MDLPDP, MINMAN, N1, N2, N3, NEXT, NP1
C      LOGICAL COV, FIRST, SCLCOV
CC
C      SAVE DRELPR, FIRST
C      DATA FIRST /.TRUE./
CC
CC***FIRST EXECUTABLE STATEMENT  DLSI
CC
CC     Set the nominal tolerance used in the code.
CC
C      IF (FIRST) DRELPR = D1MACH(4)
C      FIRST = .FALSE.
C      TOL = SQRT(DRELPR)
CC
C      MODE = 0
C      RNORM = 0.D0
C      M = MA + MG
C      NP1 = N + 1
C      KRANK = 0
C      IF (N.LE.0 .OR. M.LE.0) GO TO 370
CC
CC     To process option vector.
CC
C      COV = .FALSE.
C      SCLCOV = .TRUE.
C      LAST = 1
C      LINK = PRGOPT(1)
CC
C  100 IF (LINK.GT.1) THEN
C         KEY = PRGOPT(LAST+1)
C         IF (KEY.EQ.1) COV = PRGOPT(LAST+2) .NE. 0.D0
C         IF (KEY.EQ.10) SCLCOV = PRGOPT(LAST+2) .EQ. 0.D0
C         IF (KEY.EQ.5) TOL = MAX(DRELPR,PRGOPT(LAST+2))
C         NEXT = PRGOPT(LINK)
C         LAST = LINK
C         LINK = NEXT
C         GO TO 100
C      ENDIF
CC
CC     Compute matrix norm of least squares equations.
CC
C      ANORM = 0.D0
C      DO 110 J = 1,N
C         ANORM = MAX(ANORM,DASUM(MA,W(1,J),1))
C  110 CONTINUE
CC
CC     Set tolerance for DHFTI( ) rank test.
CC
C      TAU = TOL*ANORM
CC
CC     Compute Householder orthogonal decomposition of matrix.
CC
C      CALL DCOPY (N, 0.D0, 0, WS, 1)
C      CALL DCOPY (MA, W(1, NP1), 1, WS, 1)
C      K = MAX(M,N)
C      MINMAN = MIN(MA,N)
C      N1 = K + 1
C      N2 = N1 + N
C      CALL DHFTI (W, MDW, MA, N, WS, MA, 1, TAU, KRANK, RNORM, WS(N2),
C     +           WS(N1), IP)
C      FAC = 1.D0
C      GAM = MA - KRANK
C      IF (KRANK.LT.MA .AND. SCLCOV) FAC = RNORM**2/GAM
CC
CC     Reduce to DLPDP and solve.
CC
C      MAP1 = MA + 1
CC
CC     Compute inequality rt-hand side for DLPDP.
CC
C      IF (MA.LT.M) THEN
C         IF (MINMAN.GT.0) THEN
C            DO 120 I = MAP1,M
C               W(I,NP1) = W(I,NP1) - DDOT(N,W(I,1),MDW,WS,1)
C  120       CONTINUE
CC
CC           Apply permutations to col. of inequality constraint matrix.
CC
C            DO 130 I = 1,MINMAN
C               CALL DSWAP (MG, W(MAP1,I), 1, W(MAP1,IP(I)), 1)
C  130       CONTINUE
CC
CC           Apply Householder transformations to constraint matrix.
CC
C            IF (KRANK.GT.0 .AND. KRANK.LT.N) THEN
C               DO 140 I = KRANK,1,-1
C                  CALL DH12 (2, I, KRANK+1, N, W(I,1), MDW, WS(N1+I-1),
C     +                      W(MAP1,1), MDW, 1, MG)
C  140          CONTINUE
C            ENDIF
CC
CC           Compute permuted inequality constraint matrix times r-inv.
CC
C            DO 160 I = MAP1,M
C               DO 150 J = 1,KRANK
C                  W(I,J) = (W(I,J)-DDOT(J-1,W(1,J),1,W(I,1),MDW))/W(J,J)
C  150          CONTINUE
C  160       CONTINUE
C         ENDIF
CC
CC        Solve the reduced problem with DLPDP algorithm,
CC        the least projected distance problem.
CC
C         CALL DLPDP(W(MAP1,1), MDW, MG, KRANK, N-KRANK, PRGOPT, X,
C     +             XNORM, MDLPDP, WS(N2), IP(N+1))
CC
CC        Compute solution in original coordinates.
CC
C         IF (MDLPDP.EQ.1) THEN
C            DO 170 I = KRANK,1,-1
C               X(I) = (X(I)-DDOT(KRANK-I,W(I,I+1),MDW,X(I+1),1))/W(I,I)
C  170       CONTINUE
CC
CC           Apply Householder transformation to solution vector.
CC
C            IF (KRANK.LT.N) THEN
C               DO 180 I = 1,KRANK
C                  CALL DH12 (2, I, KRANK+1, N, W(I,1), MDW, WS(N1+I-1),
C     +                      X, 1, 1, 1)
C  180          CONTINUE
C            ENDIF
CC
CC           Repermute variables to their input order.
CC
C            IF (MINMAN.GT.0) THEN
C               DO 190 I = MINMAN,1,-1
C                  CALL DSWAP (1, X(I), 1, X(IP(I)), 1)
C  190          CONTINUE
CC
CC              Variables are now in original coordinates.
CC              Add solution of unconstrained problem.
CC
C               DO 200 I = 1,N
C                  X(I) = X(I) + WS(I)
C  200          CONTINUE
CC
CC              Compute the residual vector norm.
CC
C               RNORM = SQRT(RNORM**2+XNORM**2)
C            ENDIF
C         ELSE
C            MODE = 2
C         ENDIF
C      ELSE
C         CALL DCOPY (N, WS, 1, X, 1)
C      ENDIF
CC
CC     Compute covariance matrix based on the orthogonal decomposition
CC     from DHFTI( ).
CC
C      IF (.NOT.COV .OR. KRANK.LE.0) GO TO 370
C      KRM1 = KRANK - 1
C      KRP1 = KRANK + 1
CC
CC     Copy diagonal terms to working array.
CC
C      CALL DCOPY (KRANK, W, MDW+1, WS(N2), 1)
CC
CC     Reciprocate diagonal terms.
CC
C      DO 210 J = 1,KRANK
C         W(J,J) = 1.D0/W(J,J)
C  210 CONTINUE
CC
CC     Invert the upper triangular QR factor on itself.
CC
C      IF (KRANK.GT.1) THEN
C         DO 230 I = 1,KRM1
C            DO 220 J = I+1,KRANK
C               W(I,J) = -DDOT(J-I,W(I,I),MDW,W(I,J),1)*W(J,J)
C  220       CONTINUE
C  230    CONTINUE
C      ENDIF
CC
CC     Compute the inverted factor times its transpose.
CC
C      DO 250 I = 1,KRANK
C         DO 240 J = I,KRANK
C            W(I,J) = DDOT(KRANK+1-J,W(I,J),MDW,W(J,J),MDW)
C  240    CONTINUE
C  250 CONTINUE
CC
CC     Zero out lower trapezoidal part.
CC     Copy upper triangular to lower triangular part.
CC
C      IF (KRANK.LT.N) THEN
C         DO 260 J = 1,KRANK
C            CALL DCOPY (J, W(1,J), 1, W(J,1), MDW)
C  260    CONTINUE
CC
C         DO 270 I = KRP1,N
C            CALL DCOPY (I, 0.D0, 0, W(I,1), MDW)
C  270    CONTINUE
CC
CC        Apply right side transformations to lower triangle.
CC
C         N3 = N2 + KRP1
C         DO 330 I = 1,KRANK
C            L = N1 + I
C            K = N2 + I
C            RB = WS(L-1)*WS(K-1)
CC
CC           If RB.GE.0.D0, transformation can be regarded as zero.
CC
C            IF (RB.LT.0.D0) THEN
C               RB = 1.D0/RB
CC
CC              Store unscaled rank one Householder update in work array.
CC
C               CALL DCOPY (N, 0.D0, 0, WS(N3), 1)
C               L = N1 + I
C               K = N3 + I
C               WS(K-1) = WS(L-1)
CC
C               DO 280 J = KRP1,N
C                  WS(N3+J-1) = W(I,J)
C  280          CONTINUE
CC
C               DO 290 J = 1,N
C                  WS(J) = RB*(DDOT(J-I,W(J,I),MDW,WS(N3+I-1),1)+
C     +                    DDOT(N-J+1,W(J,J),1,WS(N3+J-1),1))
C  290          CONTINUE
CC
C               L = N3 + I
C               GAM = 0.5D0*RB*DDOT(N-I+1,WS(L-1),1,WS(I),1)
C               CALL DAXPY (N-I+1, GAM, WS(L-1), 1, WS(I), 1)
C               DO 320 J = I,N
C                  DO 300 L = 1,I-1
C                     W(J,L) = W(J,L) + WS(N3+J-1)*WS(L)
C  300             CONTINUE
CC
C                  DO 310 L = I,J
C                     W(J,L) = W(J,L) + WS(J)*WS(N3+L-1)+WS(L)*WS(N3+J-1)
C  310             CONTINUE
C  320          CONTINUE
C            ENDIF
C  330    CONTINUE
CC
CC        Copy lower triangle to upper triangle to symmetrize the
CC        covariance matrix.
CC
C         DO 340 I = 1,N
C            CALL DCOPY (I, W(I,1), MDW, W(1,I), 1)
C  340    CONTINUE
C      ENDIF
CC
CC     Repermute rows and columns.
CC
C      DO 350 I = MINMAN,1,-1
C         K = IP(I)
C         IF (I.NE.K) THEN
C            CALL DSWAP (1, W(I,I), 1, W(K,K), 1)
C            CALL DSWAP (I-1, W(1,I), 1, W(1,K), 1)
C            CALL DSWAP (K-I-1, W(I,I+1), MDW, W(I+1,K), 1)
C            CALL DSWAP (N-K, W(I, K+1), MDW, W(K, K+1), MDW)
C         ENDIF
C  350 CONTINUE
CC
CC     Put in normalized residual sum of squares scale factor
CC     and symmetrize the resulting covariance matrix.
CC
C      DO 360 J = 1,N
C         CALL DSCAL (J, FAC, W(1,J), 1)
C         CALL DCOPY (J, W(1,J), 1, W(J,1), MDW)
C  360 CONTINUE
CC
C  370 IP(1) = KRANK
C      IP(2) = N + MAX(M,N) + (MG+2)*(N+7)
C      RETURN
C      END
C      SUBROUTINE DROTM (N,DX,INCX,DY,INCY,DPARAM)
CC
CC     APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
CC
CC     (DX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF DX ARE IN
CC     (DY**T)
CC
CC     DX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
CC     LX = (-INCX)*N, AND SIMILARLY FOR SY USING LY AND INCY.
CC     WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
CC
CC     DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
CC
CC       (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
CC     H=(          )    (          )    (          )    (          )
CC       (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
CC     SEE DROTMG FOR A DESCRIPTION OF DATA STORAGE IN DPARAM.
CC
C      DOUBLE PRECISION DFLAG,DH12,DH22,DX,TWO,Z,DH11,DH21,
C     1 DPARAM,DY,W,ZERO
C      DIMENSION DX(1),DY(1),DPARAM(5)
C      DATA ZERO,TWO/0.D0,2.D0/
CC
C      DFLAG=DPARAM(1)
C      IF(N .LE. 0 .OR.(DFLAG+TWO.EQ.ZERO)) GO TO 140
C          IF(.NOT.(INCX.EQ.INCY.AND. INCX .GT.0)) GO TO 70
CC
C               NSTEPS=N*INCX
C               IF(DFLAG) 50,10,30
C   10          CONTINUE
C               DH12=DPARAM(4)
C               DH21=DPARAM(3)
C                    DO 20 I=1,NSTEPS,INCX
C                    W=DX(I)
C                    Z=DY(I)
C                    DX(I)=W+Z*DH12
C                    DY(I)=W*DH21+Z
C   20               CONTINUE
C               GO TO 140
C   30          CONTINUE
C               DH11=DPARAM(2)
C               DH22=DPARAM(5)
C                    DO 40 I=1,NSTEPS,INCX
C                    W=DX(I)
C                    Z=DY(I)
C                    DX(I)=W*DH11+Z
C                    DY(I)=-W+DH22*Z
C   40               CONTINUE
C               GO TO 140
C   50          CONTINUE
C               DH11=DPARAM(2)
C               DH12=DPARAM(4)
C               DH21=DPARAM(3)
C               DH22=DPARAM(5)
C                    DO 60 I=1,NSTEPS,INCX
C                    W=DX(I)
C                    Z=DY(I)
C                    DX(I)=W*DH11+Z*DH12
C                    DY(I)=W*DH21+Z*DH22
C   60               CONTINUE
C               GO TO 140
C   70     CONTINUE
C          KX=1
C          KY=1
C          IF(INCX .LT. 0) KX=1+(1-N)*INCX
C          IF(INCY .LT. 0) KY=1+(1-N)*INCY
CC
C          IF(DFLAG)120,80,100
C   80     CONTINUE
C          DH12=DPARAM(4)
C          DH21=DPARAM(3)
C               DO 90 I=1,N
C               W=DX(KX)
C               Z=DY(KY)
C               DX(KX)=W+Z*DH12
C               DY(KY)=W*DH21+Z
C               KX=KX+INCX
C               KY=KY+INCY
C   90          CONTINUE
C          GO TO 140
C  100     CONTINUE
C          DH11=DPARAM(2)
C          DH22=DPARAM(5)
C               DO 110 I=1,N
C               W=DX(KX)
C               Z=DY(KY)
C               DX(KX)=W*DH11+Z
C               DY(KY)=-W+DH22*Z
C               KX=KX+INCX
C               KY=KY+INCY
C  110          CONTINUE
C          GO TO 140
C  120     CONTINUE
C          DH11=DPARAM(2)
C          DH12=DPARAM(4)
C          DH21=DPARAM(3)
C          DH22=DPARAM(5)
C               DO 130 I=1,N
C               W=DX(KX)
C               Z=DY(KY)
C               DX(KX)=W*DH11+Z*DH12
C               DY(KY)=W*DH21+Z*DH22
C               KX=KX+INCX
C               KY=KY+INCY
C  130          CONTINUE
C  140     CONTINUE
C          RETURN
C          END
C      SUBROUTINE DROTMG (DD1,DD2,DX1,DY1,DPARAM)
CC
CC     CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
CC     THE SECOND COMPONENT OF THE 2-VECTOR  (DSQRT(DD1)*DX1,DSQRT(DD2)*
CC     DY2)**T.
CC     WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
CC
CC     DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
CC
CC       (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
CC     H=(          )    (          )    (          )    (          )
CC       (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
CC     LOCATIONS 2-4 OF DPARAM CONTAIN DH11, DH21, DH12, AND DH22
CC     RESPECTIVELY. (VALUES OF 1.D0, -1.D0, OR 0.D0 IMPLIED BY THE
CC     VALUE OF DPARAM(1) ARE NOT STORED IN DPARAM.)
CC
CC     THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
CC     INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
CC     OF DD1 AND DD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
CC
C      DOUBLE PRECISION GAM,ONE,RGAMSQ,DD2,DH11,DH21,DPARAM,DP2,
C     1 DQ2,DU,DY1,ZERO,GAMSQ,DD1,DFLAG,DH12,DH22,DP1,DQ1,
C     2 DTEMP,DX1,TWO
C      DIMENSION DPARAM(5)
CC
C      DATA ZERO,ONE,TWO /0.D0,1.D0,2.D0/
C      DATA GAM,GAMSQ,RGAMSQ/4096.D0,16777216.D0,5.9604645D-8/
C      IF(.NOT. DD1 .LT. ZERO) GO TO 10
CC       GO ZERO-H-D-AND-DX1..
C          GO TO 60
C   10 CONTINUE
CC     CASE-DD1-NONNEGATIVE
C      DP2=DD2*DY1
C      IF(.NOT. DP2 .EQ. ZERO) GO TO 20
C          DFLAG=-TWO
C          GO TO 260
CC     REGULAR-CASE..
C   20 CONTINUE
C      DP1=DD1*DX1
C      DQ2=DP2*DY1
C      DQ1=DP1*DX1
CC
C      IF(.NOT. DABS(DQ1) .GT. DABS(DQ2)) GO TO 40
C          DH21=-DY1/DX1
C          DH12=DP2/DP1
CC
C          DU=ONE-DH12*DH21
CC
C          IF(.NOT. DU .LE. ZERO) GO TO 30
CC         GO ZERO-H-D-AND-DX1..
C               GO TO 60
C   30     CONTINUE
C               DFLAG=ZERO
C               DD1=DD1/DU
C               DD2=DD2/DU
C               DX1=DX1*DU
CC         GO SCALE-CHECK..
C               GO TO 100
C   40 CONTINUE
C          IF(.NOT. DQ2 .LT. ZERO) GO TO 50
CC         GO ZERO-H-D-AND-DX1..
C               GO TO 60
C   50     CONTINUE
C               DFLAG=ONE
C               DH11=DP1/DP2
C               DH22=DX1/DY1
C               DU=ONE+DH11*DH22
C               DTEMP=DD2/DU
C               DD2=DD1/DU
C               DD1=DTEMP
C               DX1=DY1*DU
CC         GO SCALE-CHECK
C               GO TO 100
CC     PROCEDURE..ZERO-H-D-AND-DX1..
C   60 CONTINUE
C          DFLAG=-ONE
C          DH11=ZERO
C          DH12=ZERO
C          DH21=ZERO
C          DH22=ZERO
CC
C          DD1=ZERO
C          DD2=ZERO
C          DX1=ZERO
CC         RETURN..
C          GO TO 220
CC     PROCEDURE..FIX-H..
C   70 CONTINUE
C      IF(.NOT. DFLAG .GE. ZERO) GO TO 90
CC
C          IF(.NOT. DFLAG .EQ. ZERO) GO TO 80
C          DH11=ONE
C          DH22=ONE
C          DFLAG=-ONE
C          GO TO 90
C   80     CONTINUE
C          DH21=-ONE
C          DH12=ONE
C          DFLAG=-ONE
C   90 CONTINUE
C      GO TO IGO,(120,150,180,210)
CC     PROCEDURE..SCALE-CHECK
C  100 CONTINUE
C  110     CONTINUE
C          IF(.NOT. DD1 .LE. RGAMSQ) GO TO 130
C               IF(DD1 .EQ. ZERO) GO TO 160
C               ASSIGN 120 TO IGO
CC              FIX-H..
C               GO TO 70
C  120          CONTINUE
C               DD1=DD1*GAM**2
C               DX1=DX1/GAM
C               DH11=DH11/GAM
C               DH12=DH12/GAM
C          GO TO 110
C  130 CONTINUE
C  140     CONTINUE
C          IF(.NOT. DD1 .GE. GAMSQ) GO TO 160
C               ASSIGN 150 TO IGO
CC              FIX-H..
C               GO TO 70
C  150          CONTINUE
C               DD1=DD1/GAM**2
C               DX1=DX1*GAM
C               DH11=DH11*GAM
C               DH12=DH12*GAM
C          GO TO 140
C  160 CONTINUE
C  170     CONTINUE
C          IF(.NOT. DABS(DD2) .LE. RGAMSQ) GO TO 190
C               IF(DD2 .EQ. ZERO) GO TO 220
C               ASSIGN 180 TO IGO
CC              FIX-H..
C               GO TO 70
C  180          CONTINUE
C               DD2=DD2*GAM**2
C               DH21=DH21/GAM
C               DH22=DH22/GAM
C          GO TO 170
C  190 CONTINUE
C  200     CONTINUE
C          IF(.NOT. DABS(DD2) .GE. GAMSQ) GO TO 220
C               ASSIGN 210 TO IGO
CC              FIX-H..
C               GO TO 70
C  210          CONTINUE
C               DD2=DD2/GAM**2
C               DH21=DH21*GAM
C               DH22=DH22*GAM
C          GO TO 200
C  220 CONTINUE
C          IF(DFLAG)250,230,240
C  230     CONTINUE
C               DPARAM(3)=DH21
C               DPARAM(4)=DH12
C               GO TO 260
C  240     CONTINUE
C               DPARAM(2)=DH11
C               DPARAM(5)=DH22
C               GO TO 260
C  250     CONTINUE
C               DPARAM(2)=DH11
C               DPARAM(3)=DH21
C               DPARAM(4)=DH12
C               DPARAM(5)=DH22
C  260 CONTINUE
C          DPARAM(1)=DFLAG
C          RETURN
C      END
C      SUBROUTINE DWNLIT (W, MDW, M, N, L, IPIVOT, ITYPE, H, SCALE,
C     +   RNORM, IDOPE, DOPE, DONE)
CC***BEGIN PROLOGUE  DWNLIT
CC***SUBSIDIARY
CC***PURPOSE  Subsidiary to DWNNLS
CC***LIBRARY   SLATEC
CC***TYPE      DOUBLE PRECISION (WNLIT-S, DWNLIT-D)
CC***AUTHOR  Hanson, R. J., (SNLA)
CC           Haskell, K. H., (SNLA)
CC***DESCRIPTION
CC
CC     This is a companion subprogram to DWNNLS( ).
CC     The documentation for DWNNLS( ) has complete usage instructions.
CC
CC     Note  The M by (N+1) matrix W( , ) contains the rt. hand side
CC           B as the (N+1)st col.
CC
CC     Triangularize L1 by L1 subsystem, where L1=MIN(M,L), with
CC     col interchanges.
CC
CC***SEE ALSO  DWNNLS
CC***ROUTINES CALLED  DCOPY, DH12, DROTM, DROTMG, DSCAL, DSWAP, DWNLT1,
CC                    DWNLT2, DWNLT3, IDAMAX
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890531  Changed all specific intrinsics to generic.  (WRB)
CC   890618  Completely restructured and revised.  (WRB & RWC)
CC   890620  Revised to make WNLT1, WNLT2, and WNLT3 subroutines.  (RWC)
CC   891214  Prologue converted to Version 4.0 format.  (BAB)
CC   900328  Added TYPE section.  (WRB)
CC   900604  DP version created from SP version. .  (RWC)
CC***END PROLOGUE  DWNLIT
C      INTEGER IDOPE(*), IPIVOT(*), ITYPE(*), L, M, MDW, N
C      DOUBLE PRECISION DOPE(*), H(*), RNORM, SCALE(*), W(MDW,*)
C      LOGICAL DONE
CC
C      EXTERNAL DCOPY, DH12, DROTM, DROTMG, DSCAL, DSWAP, DWNLT1,
C     *   DWNLT2, DWNLT3, IDAMAX
C      INTEGER IDAMAX
C      LOGICAL DWNLT2
CC
C      DOUBLE PRECISION ALSQ, AMAX, EANORM, FACTOR, HBAR, RN, SPARAM(5),
C     *   T, TAU
C      INTEGER I, I1, IMAX, IR, J, J1, JJ, JP, KRANK, L1, LB, LEND, ME,
C     *   MEND, NIV, NSOLN
C      LOGICAL INDEP, RECALC
CC
CC***FIRST EXECUTABLE STATEMENT  DWNLIT
C      ME    = IDOPE(1)
C      NSOLN = IDOPE(2)
C      L1    = IDOPE(3)
CC
C      ALSQ   = DOPE(1)
C      EANORM = DOPE(2)
C      TAU    = DOPE(3)
CC
C      LB     = MIN(M-1,L)
C      RECALC = .TRUE.
C      RNORM  = 0.D0
C      KRANK  = 0
CC
CC     We set FACTOR=1.0 so that the heavy weight ALAMDA will be
CC     included in the test for column independence.
CC
C      FACTOR = 1.D0
C      LEND = L
C      DO 180 I=1,LB
CC
CC        Set IR to point to the I-th row.
CC
C         IR = I
C         MEND = M
C         CALL DWNLT1 (I, LEND, M, IR, MDW, RECALC, IMAX, HBAR, H, SCALE,
C     +                W)
CC
CC        Update column SS and find pivot column.
CC
C         CALL DWNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
CC
CC        Perform column interchange.
CC        Test independence of incoming column.
CC
C  130    IF (DWNLT2(ME, MEND, IR, FACTOR, TAU, SCALE, W(1,I))) THEN
CC
CC           Eliminate I-th column below diagonal using modified Givens
CC           transformations applied to (A B).
CC
CC           When operating near the ME line, use the largest element
CC           above it as the pivot.
CC
C            DO 160 J=M,I+1,-1
C               JP = J-1
C               IF (J.EQ.ME+1) THEN
C                  IMAX = ME
C                  AMAX = SCALE(ME)*W(ME,I)**2
C                  DO 150 JP=J-1,I,-1
C                     T = SCALE(JP)*W(JP,I)**2
C                     IF (T.GT.AMAX) THEN
C                        IMAX = JP
C                        AMAX = T
C                     ENDIF
C  150             CONTINUE
C                  JP = IMAX
C               ENDIF
CC
C               IF (W(J,I).NE.0.D0) THEN
C                  CALL DROTMG (SCALE(JP), SCALE(J), W(JP,I), W(J,I),
C     +                         SPARAM)
C                  W(J,I) = 0.D0
C                  CALL DROTM (N+1-I, W(JP,I+1), MDW, W(J,I+1), MDW,
C     +                        SPARAM)
C               ENDIF
C  160       CONTINUE
C         ELSE IF (LEND.GT.I) THEN
CC
CC           Column I is dependent.  Swap with column LEND.
CC           Perform column interchange,
CC           and find column in remaining set with largest SS.
CC
C            CALL DWNLT3 (I, LEND, M, MDW, IPIVOT, H, W)
C            LEND = LEND - 1
C            IMAX = IDAMAX(LEND-I+1, H(I), 1) + I - 1
C            HBAR = H(IMAX)
C            GO TO 130
C         ELSE
C            KRANK = I - 1
C            GO TO 190
C         ENDIF
C  180 CONTINUE
C      KRANK = L1
CC
C  190 IF (KRANK.LT.ME) THEN
C         FACTOR = ALSQ
C         DO 200 I=KRANK+1,ME
C            CALL DCOPY (L, 0.D0, 0, W(I,1), MDW)
C  200    CONTINUE
CC
CC        Determine the rank of the remaining equality constraint
CC        equations by eliminating within the block of constrained
CC        variables.  Remove any redundant constraints.
CC
C         RECALC = .TRUE.
C         LB = MIN(L+ME-KRANK, N)
C         DO 270 I=L+1,LB
C            IR = KRANK + I - L
C            LEND = N
C            MEND = ME
C            CALL DWNLT1 (I, LEND, ME, IR, MDW, RECALC, IMAX, HBAR, H,
C     +                   SCALE, W)
CC
CC           Update col ss and find pivot col
CC
C            CALL DWNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
CC
CC           Perform column interchange
CC           Eliminate elements in the I-th col.
CC
C            DO 240 J=ME,IR+1,-1
C               IF (W(J,I).NE.0.D0) THEN
C                 CALL DROTMG (SCALE(J-1), SCALE(J), W(J-1,I), W(J,I),
C     +                        SPARAM)
C                  W(J,I) = 0.D0
C                  CALL DROTM (N+1-I, W(J-1,I+1), MDW,W(J,I+1), MDW,
C     +                        SPARAM)
C               ENDIF
C  240       CONTINUE
CC
CC           I=column being eliminated.
CC           Test independence of incoming column.
CC           Remove any redundant or dependent equality constraints.
CC
C            IF (.NOT.DWNLT2(ME, MEND, IR, FACTOR,TAU,SCALE,W(1,I))) THEN
C               JJ = IR
C               DO 260 IR=JJ,ME
C                  CALL DCOPY (N, 0.D0, 0, W(IR,1), MDW)
C                  RNORM = RNORM + (SCALE(IR)*W(IR,N+1)/ALSQ)*W(IR,N+1)
C                  W(IR,N+1) = 0.D0
C                  SCALE(IR) = 1.D0
CC
CC                 Reclassify the zeroed row as a least squares equation.
CC
C                  ITYPE(IR) = 1
C  260          CONTINUE
CC
CC              Reduce ME to reflect any discovered dependent equality
CC              constraints.
CC
C               ME = JJ - 1
C               GO TO 280
C            ENDIF
C  270    CONTINUE
C      ENDIF
CC
CC     Try to determine the variables KRANK+1 through L1 from the
CC     least squares equations.  Continue the triangularization with
CC     pivot element W(ME+1,I).
CC
C  280 IF (KRANK.LT.L1) THEN
C         RECALC = .TRUE.
CC
CC        Set FACTOR=ALSQ to remove effect of heavy weight from
CC        test for column independence.
CC
C         FACTOR = ALSQ
C         DO 350 I=KRANK+1,L1
CC
CC           Set IR to point to the ME+1-st row.
CC
C            IR = ME+1
C            LEND = L
C            MEND = M
C            CALL DWNLT1 (I, L, M, IR, MDW, RECALC, IMAX, HBAR, H, SCALE,
C     +                   W)
CC
CC           Update column SS and find pivot column.
CC
C            CALL DWNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
CC
CC           Perform column interchange.
CC           Eliminate I-th column below the IR-th element.
CC
C            DO 320 J=M,IR+1,-1
C               IF (W(J,I).NE.0.D0) THEN
C                 CALL DROTMG (SCALE(J-1), SCALE(J), W(J-1,I), W(J,I),
C     +                        SPARAM)
C                  W(J,I) = 0.D0
C                  CALL DROTM (N+1-I, W(J-1,I+1),  MDW, W(J,I+1), MDW,
C     +                        SPARAM)
C               ENDIF
C  320       CONTINUE
CC
CC           Test if new pivot element is near zero.
CC           If so, the column is dependent.
CC           Then check row norm test to be classified as independent.
CC
C            T = SCALE(IR)*W(IR,I)**2
C            INDEP = T .GT. (TAU*EANORM)**2
C            IF (INDEP) THEN
C               RN = 0.D0
C               DO 340 I1=IR,M
C                  DO 330 J1=I+1,N
C                     RN = MAX(RN, SCALE(I1)*W(I1,J1)**2)
C  330             CONTINUE
C  340          CONTINUE
C               INDEP = T .GT. RN*TAU**2
C            ENDIF
CC
CC           If independent, swap the IR-th and KRANK+1-th rows to
CC           maintain the triangular form.  Update the rank indicator
CC           KRANK and the equality constraint pointer ME.
CC
C            IF (.NOT.INDEP) GO TO 360
C            CALL DSWAP(N+1, W(KRANK+1,1), MDW, W(IR,1), MDW)
C            CALL DSWAP(1, SCALE(KRANK+1), 1, SCALE(IR), 1)
CC
CC           Reclassify the least square equation as an equality
CC           constraint and rescale it.
CC
C            ITYPE(IR) = 0
C            T = SQRT(SCALE(KRANK+1))
C            CALL DSCAL(N+1, T, W(KRANK+1,1), MDW)
C            SCALE(KRANK+1) = ALSQ
C            ME = ME+1
C            KRANK = KRANK+1
C  350    CONTINUE
C      ENDIF
CC
CC     If pseudorank is less than L, apply Householder transformation.
CC     from right.
CC
C  360 IF (KRANK.LT.L) THEN
C         DO 370 J=KRANK,1,-1
C            CALL DH12 (1, J, KRANK+1, L, W(J,1), MDW, H(J), W, MDW, 1,
C     +                J-1)
C  370    CONTINUE
C      ENDIF
CC
C      NIV = KRANK + NSOLN - L
C      IF (L.EQ.N) DONE = .TRUE.
CC
CC     End of initial triangularization.
CC
C      IDOPE(1) = ME
C      IDOPE(2) = KRANK
C      IDOPE(3) = NIV
C      RETURN
C      END
C      SUBROUTINE DWNLSM (W, MDW, MME, MA, N, L, PRGOPT, X, RNORM, MODE,
C     +   IPIVOT, ITYPE, WD, H, SCALE, Z, TEMP, D)
CC***BEGIN PROLOGUE  DWNLSM
CC***SUBSIDIARY
CC***PURPOSE  Subsidiary to DWNNLS
CC***LIBRARY   SLATEC
CC***TYPE      DOUBLE PRECISION (WNLSM-S, DWNLSM-D)
CC***AUTHOR  Hanson, R. J., (SNLA)
CC           Haskell, K. H., (SNLA)
CC***DESCRIPTION
CC
CC     This is a companion subprogram to DWNNLS.
CC     The documentation for DWNNLS has complete usage instructions.
CC
CC     In addition to the parameters discussed in the prologue to
CC     subroutine DWNNLS, the following work arrays are used in
CC     subroutine DWNLSM  (they are passed through the calling
CC     sequence from DWNNLS for purposes of variable dimensioning).
CC     Their contents will in general be of no interest to the user.
CC
CC     Variables of type REAL are DOUBLE PRECISION.
CC
CC         IPIVOT(*)
CC            An array of length N.  Upon completion it contains the
CC         pivoting information for the cols of W(*,*).
CC
CC         ITYPE(*)
CC            An array of length M which is used to keep track
CC         of the classification of the equations.  ITYPE(I)=0
CC         denotes equation I as an equality constraint.
CC         ITYPE(I)=1 denotes equation I as a least squares
CC         equation.
CC
CC         WD(*)
CC            An array of length N.  Upon completion it contains the
CC         dual solution vector.
CC
CC         H(*)
CC            An array of length N.  Upon completion it contains the
CC         pivot scalars of the Householder transformations performed
CC         in the case KRANK.LT.L.
CC
CC         SCALE(*)
CC            An array of length M which is used by the subroutine
CC         to store the diagonal matrix of weights.
CC         These are used to apply the modified Givens
CC         transformations.
CC
CC         Z(*),TEMP(*)
CC            Working arrays of length N.
CC
CC         D(*)
CC            An array of length N that contains the
CC         column scaling for the matrix (E).
CC                                       (A)
CC
CC***SEE ALSO  DWNNLS
CC***ROUTINES CALLED  D1MACH, DASUM, DAXPY, DCOPY, DH12, DNRM2, DROTM,
CC                    DROTMG, DSCAL, DSWAP, DWNLIT, IDAMAX, XERMSG
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890531  Changed all specific intrinsics to generic.  (WRB)
CC   890618  Completely restructured and revised.  (WRB & RWC)
CC   891214  Prologue converted to Version 4.0 format.  (BAB)
CC   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
CC   900328  Added TYPE section.  (WRB)
CC   900510  Fixed an error message.  (RWC)
CC   900604  DP version created from SP version.  (RWC)
CC   900911  Restriction on value of ALAMDA included.  (WRB)
CC***END PROLOGUE  DWNLSM
C      INTEGER IPIVOT(*), ITYPE(*), L, MA, MDW, MME, MODE, N
C      DOUBLE PRECISION D(*), H(*), PRGOPT(*), RNORM, SCALE(*), TEMP(*),
C     *   W(MDW,*), WD(*), X(*), Z(*)
CC
C      EXTERNAL D1MACH, DASUM, DAXPY, DCOPY, DH12, DNRM2, DROTM, DROTMG,
C     *   DSCAL, DSWAP, DWNLIT, IDAMAX, XERMSG
C      DOUBLE PRECISION D1MACH, DASUM, DNRM2
C      INTEGER IDAMAX
CC
C      DOUBLE PRECISION ALAMDA, ALPHA, ALSQ, AMAX, BLOWUP, BNORM,
C     *   DOPE(3), DRELPR, EANORM, FAC, SM, SPARAM(5), T, TAU, WMAX, Z2,
C     *   ZZ
C      INTEGER I, IDOPE(3), IMAX, ISOL, ITEMP, ITER, ITMAX, IWMAX, J,
C     *   JCON, JP, KEY, KRANK, L1, LAST, LINK, M, ME, NEXT, NIV, NLINK,
C     *   NOPT, NSOLN, NTIMES
C      LOGICAL DONE, FEASBL, FIRST, HITCON, POS
CC
C      SAVE DRELPR, FIRST
C      DATA FIRST /.TRUE./
CC***FIRST EXECUTABLE STATEMENT  DWNLSM
CC
CC     Initialize variables.
CC     DRELPR is the precision for the particular machine
CC     being used.  This logic avoids resetting it every entry.
CC
C      IF (FIRST) DRELPR = D1MACH(4)
C      FIRST = .FALSE.
CC
CC     Set the nominal tolerance used in the code.
CC
C      TAU = SQRT(DRELPR)
CC
C      M = MA + MME
C      ME = MME
C      MODE = 2
CC
CC     To process option vector
CC
C      FAC = 1.D-4
CC
CC     Set the nominal blow up factor used in the code.
CC
C      BLOWUP = TAU
CC
CC     The nominal column scaling used in the code is
CC     the identity scaling.
CC
C      CALL DCOPY (N, 1.D0, 0, D, 1)
CC
CC     Define bound for number of options to change.
CC
C      NOPT = 1000
CC
CC     Define bound for positive value of LINK.
CC
C      NLINK = 100000
C      NTIMES = 0
C      LAST = 1
C      LINK = PRGOPT(1)
C      IF (LINK.LE.0 .OR. LINK.GT.NLINK) THEN
C         CALL XERMSG ('SLATEC', 'DWNLSM',
C     +      'IN DWNNLS, THE OPTION VECTOR IS UNDEFINED', 3, 1)
C         RETURN
C      ENDIF
CC
C  100 IF (LINK.GT.1) THEN
C         NTIMES = NTIMES + 1
C         IF (NTIMES.GT.NOPT) THEN
C         CALL XERMSG ('SLATEC', 'DWNLSM',
C     +      'IN DWNNLS, THE LINKS IN THE OPTION VECTOR ARE CYCLING.',
C     +      3, 1)
C            RETURN
C         ENDIF
CC
C         KEY = PRGOPT(LAST+1)
C         IF (KEY.EQ.6 .AND. PRGOPT(LAST+2).NE.0.D0) THEN
C            DO 110 J = 1,N
C               T = DNRM2(M,W(1,J),1)
C               IF (T.NE.0.D0) T = 1.D0/T
C               D(J) = T
C  110       CONTINUE
C         ENDIF
CC
C         IF (KEY.EQ.7) CALL DCOPY (N, PRGOPT(LAST+2), 1, D, 1)
C         IF (KEY.EQ.8) TAU = MAX(DRELPR,PRGOPT(LAST+2))
C         IF (KEY.EQ.9) BLOWUP = MAX(DRELPR,PRGOPT(LAST+2))
CC
C         NEXT = PRGOPT(LINK)
C         IF (NEXT.LE.0 .OR. NEXT.GT.NLINK) THEN
C            CALL XERMSG ('SLATEC', 'DWNLSM',
C     +         'IN DWNNLS, THE OPTION VECTOR IS UNDEFINED', 3, 1)
C            RETURN
C         ENDIF
CC
C         LAST = LINK
C         LINK = NEXT
C         GO TO 100
C      ENDIF
CC
C      DO 120 J = 1,N
C         CALL DSCAL (M, D(J), W(1,J), 1)
C  120 CONTINUE
CC
CC     Process option vector
CC
C      DONE = .FALSE.
C      ITER = 0
C      ITMAX = 3*(N-L)
C      MODE = 0
C      NSOLN = L
C      L1 = MIN(M,L)
CC
CC     Compute scale factor to apply to equality constraint equations.
CC
C      DO 130 J = 1,N
C         WD(J) = DASUM(M,W(1,J),1)
C  130 CONTINUE
CC
C      IMAX = IDAMAX(N,WD,1)
C      EANORM = WD(IMAX)
C      BNORM = DASUM(M,W(1,N+1),1)
C      ALAMDA = EANORM/(DRELPR*FAC)
CC
CC     On machines, such as the VAXes using D floating, with a very
CC     limited exponent range for double precision values, the previously
CC     computed value of ALAMDA may cause an overflow condition.
CC     Therefore, this code further limits the value of ALAMDA.
CC
C      ALAMDA = MIN(ALAMDA,SQRT(D1MACH(2)))
CC
CC     Define scaling diagonal matrix for modified Givens usage and
CC     classify equation types.
CC
C      ALSQ = ALAMDA**2
C      DO 140 I = 1,M
CC
CC        When equation I is heavily weighted ITYPE(I)=0,
CC        else ITYPE(I)=1.
CC
C         IF (I.LE.ME) THEN
C            T = ALSQ
C            ITEMP = 0
C         ELSE
C            T = 1.D0
C            ITEMP = 1
C         ENDIF
C         SCALE(I) = T
C         ITYPE(I) = ITEMP
C  140 CONTINUE
CC
CC     Set the solution vector X(*) to zero and the column interchange
CC     matrix to the identity.
CC
C      CALL DCOPY (N, 0.D0, 0, X, 1)
C      DO 150 I = 1,N
C         IPIVOT(I) = I
C  150 CONTINUE
CC
CC     Perform initial triangularization in the submatrix
CC     corresponding to the unconstrained variables.
CC     Set first L components of dual vector to zero because
CC     these correspond to the unconstrained variables.
CC
C      CALL DCOPY (L, 0.D0, 0, WD, 1)
CC
CC     The arrays IDOPE(*) and DOPE(*) are used to pass
CC     information to DWNLIT().  This was done to avoid
CC     a long calling sequence or the use of COMMON.
CC
C      IDOPE(1) = ME
C      IDOPE(2) = NSOLN
C      IDOPE(3) = L1
CC
C      DOPE(1) = ALSQ
C      DOPE(2) = EANORM
C      DOPE(3) = TAU
C      CALL DWNLIT (W, MDW, M, N, L, IPIVOT, ITYPE, H, SCALE, RNORM,
C     +            IDOPE, DOPE, DONE)
C      ME    = IDOPE(1)
C      KRANK = IDOPE(2)
C      NIV   = IDOPE(3)
CC
CC     Perform WNNLS algorithm using the following steps.
CC
CC     Until(DONE)
CC        compute search direction and feasible point
CC        when (HITCON) add constraints
CC        else perform multiplier test and drop a constraint
CC        fin
CC     Compute-Final-Solution
CC
CC     To compute search direction and feasible point,
CC     solve the triangular system of currently non-active
CC     variables and store the solution in Z(*).
CC
CC     To solve system
CC     Copy right hand side into TEMP vector to use overwriting method.
CC
C  160 IF (DONE) GO TO 330
C      ISOL = L + 1
C      IF (NSOLN.GE.ISOL) THEN
C         CALL DCOPY (NIV, W(1,N+1), 1, TEMP, 1)
C         DO 170 J = NSOLN,ISOL,-1
C            IF (J.GT.KRANK) THEN
C               I = NIV - NSOLN + J
C            ELSE
C               I = J
C            ENDIF
CC
C            IF (J.GT.KRANK .AND. J.LE.L) THEN
C               Z(J) = 0.D0
C            ELSE
C               Z(J) = TEMP(I)/W(I,J)
C               CALL DAXPY (I-1, -Z(J), W(1,J), 1, TEMP, 1)
C            ENDIF
C  170    CONTINUE
C      ENDIF
CC
CC     Increment iteration counter and check against maximum number
CC     of iterations.
CC
C      ITER = ITER + 1
C      IF (ITER.GT.ITMAX) THEN
C         MODE = 1
C         DONE = .TRUE.
C      ENDIF
CC
CC     Check to see if any constraints have become active.
CC     If so, calculate an interpolation factor so that all
CC     active constraints are removed from the basis.
CC
C      ALPHA = 2.D0
C      HITCON = .FALSE.
C      DO 180 J = L+1,NSOLN
C         ZZ = Z(J)
C         IF (ZZ.LE.0.D0) THEN
C            T = X(J)/(X(J)-ZZ)
C            IF (T.LT.ALPHA) THEN
C               ALPHA = T
C               JCON = J
C            ENDIF
C            HITCON = .TRUE.
C         ENDIF
C  180 CONTINUE
CC
CC     Compute search direction and feasible point
CC
C      IF (HITCON) THEN
CC
CC        To add constraints, use computed ALPHA to interpolate between
CC        last feasible solution X(*) and current unconstrained (and
CC        infeasible) solution Z(*).
CC
C         DO 190 J = L+1,NSOLN
C            X(J) = X(J) + ALPHA*(Z(J)-X(J))
C  190    CONTINUE
C         FEASBL = .FALSE.
CC
CC        Remove column JCON and shift columns JCON+1 through N to the
CC        left.  Swap column JCON into the N th position.  This achieves
CC        upper Hessenberg form for the nonactive constraints and
CC        leaves an upper Hessenberg matrix to retriangularize.
CC
C  200    DO 210 I = 1,M
C            T = W(I,JCON)
C            CALL DCOPY (N-JCON, W(I, JCON+1), MDW, W(I, JCON), MDW)
C            W(I,N) = T
C  210    CONTINUE
CC
CC        Update permuted index vector to reflect this shift and swap.
CC
C         ITEMP = IPIVOT(JCON)
C         DO 220 I = JCON,N - 1
C            IPIVOT(I) = IPIVOT(I+1)
C  220    CONTINUE
C         IPIVOT(N) = ITEMP
CC
CC        Similarly permute X(*) vector.
CC
C         CALL DCOPY (N-JCON, X(JCON+1), 1, X(JCON), 1)
C         X(N) = 0.D0
C         NSOLN = NSOLN - 1
C         NIV = NIV - 1
CC
CC        Retriangularize upper Hessenberg matrix after adding
CC        constraints.
CC
C         I = KRANK + JCON - L
C         DO 230 J = JCON,NSOLN
C            IF (ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.0) THEN
CC
CC              Zero IP1 to I in column J
CC
C               IF (W(I+1,J).NE.0.D0) THEN
C                  CALL DROTMG (SCALE(I), SCALE(I+1), W(I,J), W(I+1,J),
C     +                         SPARAM)
C                  W(I+1,J) = 0.D0
C                  CALL DROTM (N+1-J, W(I,J+1), MDW, W(I+1,J+1), MDW,
C     +                        SPARAM)
C               ENDIF
C            ELSEIF (ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.1) THEN
CC
CC              Zero IP1 to I in column J
CC
C               IF (W(I+1,J).NE.0.D0) THEN
C                  CALL DROTMG (SCALE(I), SCALE(I+1), W(I,J), W(I+1,J),
C     +                         SPARAM)
C                  W(I+1,J) = 0.D0
C                  CALL DROTM (N+1-J, W(I,J+1), MDW, W(I+1,J+1), MDW,
C     +                        SPARAM)
C               ENDIF
C            ELSEIF (ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.0) THEN
C               CALL DSWAP (N+1, W(I,1), MDW, W(I+1,1), MDW)
C               CALL DSWAP (1, SCALE(I), 1, SCALE(I+1), 1)
C               ITEMP = ITYPE(I+1)
C               ITYPE(I+1) = ITYPE(I)
C               ITYPE(I) = ITEMP
CC
CC              Swapped row was formerly a pivot element, so it will
CC              be large enough to perform elimination.
CC              Zero IP1 to I in column J.
CC
C               IF (W(I+1,J).NE.0.D0) THEN
C                  CALL DROTMG (SCALE(I), SCALE(I+1), W(I,J), W(I+1,J),
C     +                         SPARAM)
C                  W(I+1,J) = 0.D0
C                  CALL DROTM (N+1-J, W(I,J+1), MDW, W(I+1,J+1), MDW,
C     +                        SPARAM)
C               ENDIF
C            ELSEIF (ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.1) THEN
C               IF (SCALE(I)*W(I,J)**2/ALSQ.GT.(TAU*EANORM)**2) THEN
CC
CC                 Zero IP1 to I in column J
CC
C                  IF (W(I+1,J).NE.0.D0) THEN
C                     CALL DROTMG (SCALE(I), SCALE(I+1), W(I,J),
C     +                            W(I+1,J), SPARAM)
C                     W(I+1,J) = 0.D0
C                     CALL DROTM (N+1-J, W(I,J+1), MDW, W(I+1,J+1), MDW,
C     +                           SPARAM)
C                  ENDIF
C               ELSE
C                  CALL DSWAP (N+1, W(I,1), MDW, W(I+1,1), MDW)
C                  CALL DSWAP (1, SCALE(I), 1, SCALE(I+1), 1)
C                  ITEMP = ITYPE(I+1)
C                  ITYPE(I+1) = ITYPE(I)
C                  ITYPE(I) = ITEMP
C                  W(I+1,J) = 0.D0
C               ENDIF
C            ENDIF
C            I = I + 1
C  230    CONTINUE
CC
CC        See if the remaining coefficients in the solution set are
CC        feasible.  They should be because of the way ALPHA was
CC        determined.  If any are infeasible, it is due to roundoff
CC        error.  Any that are non-positive will be set to zero and
CC        removed from the solution set.
CC
C         DO 240 JCON = L+1,NSOLN
C            IF (X(JCON).LE.0.D0) GO TO 250
C  240    CONTINUE
C         FEASBL = .TRUE.
C  250    IF (.NOT.FEASBL) GO TO 200
C      ELSE
CC
CC        To perform multiplier test and drop a constraint.
CC
C         CALL DCOPY (NSOLN, Z, 1, X, 1)
C         IF (NSOLN.LT.N) CALL DCOPY (N-NSOLN, 0.D0, 0, X(NSOLN+1), 1)
CC
CC        Reclassify least squares equations as equalities as necessary.
CC
C         I = NIV + 1
C  260    IF (I.LE.ME) THEN
C            IF (ITYPE(I).EQ.0) THEN
C               I = I + 1
C            ELSE
C               CALL DSWAP (N+1, W(I,1), MDW, W(ME,1), MDW)
C               CALL DSWAP (1, SCALE(I), 1, SCALE(ME), 1)
C               ITEMP = ITYPE(I)
C               ITYPE(I) = ITYPE(ME)
C               ITYPE(ME) = ITEMP
C               ME = ME - 1
C            ENDIF
C            GO TO 260
C         ENDIF
CC
CC        Form inner product vector WD(*) of dual coefficients.
CC
C         DO 280 J = NSOLN+1,N
C            SM = 0.D0
C            DO 270 I = NSOLN+1,M
C               SM = SM + SCALE(I)*W(I,J)*W(I,N+1)
C  270       CONTINUE
C            WD(J) = SM
C  280    CONTINUE
CC
CC        Find J such that WD(J)=WMAX is maximum.  This determines
CC        that the incoming column J will reduce the residual vector
CC        and be positive.
CC
C  290    WMAX = 0.D0
C         IWMAX = NSOLN + 1
C         DO 300 J = NSOLN+1,N
C            IF (WD(J).GT.WMAX) THEN
C               WMAX = WD(J)
C               IWMAX = J
C            ENDIF
C  300    CONTINUE
C         IF (WMAX.LE.0.D0) GO TO 330
CC
CC        Set dual coefficients to zero for incoming column.
CC
C         WD(IWMAX) = 0.D0
CC
CC        WMAX .GT. 0.D0, so okay to move column IWMAX to solution set.
CC        Perform transformation to retriangularize, and test for near
CC        linear dependence.
CC
CC        Swap column IWMAX into NSOLN-th position to maintain upper
CC        Hessenberg form of adjacent columns, and add new column to
CC        triangular decomposition.
CC
C         NSOLN = NSOLN + 1
C         NIV = NIV + 1
C         IF (NSOLN.NE.IWMAX) THEN
C            CALL DSWAP (M, W(1,NSOLN), 1, W(1,IWMAX), 1)
C            WD(IWMAX) = WD(NSOLN)
C            WD(NSOLN) = 0.D0
C            ITEMP = IPIVOT(NSOLN)
C            IPIVOT(NSOLN) = IPIVOT(IWMAX)
C            IPIVOT(IWMAX) = ITEMP
C         ENDIF
CC
CC        Reduce column NSOLN so that the matrix of nonactive constraints
CC        variables is triangular.
CC
C         DO 320 J = M,NIV+1,-1
C            JP = J - 1
CC
CC           When operating near the ME line, test to see if the pivot
CC           element is near zero.  If so, use the largest element above
CC           it as the pivot.  This is to maintain the sharp interface
CC           between weighted and non-weighted rows in all cases.
CC
C            IF (J.EQ.ME+1) THEN
C               IMAX = ME
C               AMAX = SCALE(ME)*W(ME,NSOLN)**2
C               DO 310 JP = J - 1,NIV,-1
C                  T = SCALE(JP)*W(JP,NSOLN)**2
C                  IF (T.GT.AMAX) THEN
C                     IMAX = JP
C                     AMAX = T
C                  ENDIF
C  310          CONTINUE
C               JP = IMAX
C            ENDIF
CC
C            IF (W(J,NSOLN).NE.0.D0) THEN
C               CALL DROTMG (SCALE(JP), SCALE(J), W(JP,NSOLN),
C     +                      W(J,NSOLN), SPARAM)
C               W(J,NSOLN) = 0.D0
C               CALL DROTM (N+1-NSOLN, W(JP,NSOLN+1), MDW, W(J,NSOLN+1),
C     +                     MDW, SPARAM)
C            ENDIF
C  320    CONTINUE
CC
CC        Solve for Z(NSOLN)=proposed new value for X(NSOLN).  Test if
CC        this is nonpositive or too large.  If this was true or if the
CC        pivot term was zero, reject the column as dependent.
CC
C         IF (W(NIV,NSOLN).NE.0.D0) THEN
C            ISOL = NIV
C            Z2 = W(ISOL,N+1)/W(ISOL,NSOLN)
C            Z(NSOLN) = Z2
C            POS = Z2 .GT. 0.D0
C            IF (Z2*EANORM.GE.BNORM .AND. POS) THEN
C               POS = .NOT. (BLOWUP*Z2*EANORM.GE.BNORM)
C            ENDIF
CC
CC           Try to add row ME+1 as an additional equality constraint.
CC           Check size of proposed new solution component.
CC           Reject it if it is too large.
CC
C         ELSEIF (NIV.LE.ME .AND. W(ME+1,NSOLN).NE.0.D0) THEN
C            ISOL = ME + 1
C            IF (POS) THEN
CC
CC              Swap rows ME+1 and NIV, and scale factors for these rows.
CC
C               CALL DSWAP (N+1, W(ME+1,1), MDW, W(NIV,1), MDW)
C               CALL DSWAP (1, SCALE(ME+1), 1, SCALE(NIV), 1)
C               ITEMP = ITYPE(ME+1)
C               ITYPE(ME+1) = ITYPE(NIV)
C               ITYPE(NIV) = ITEMP
C               ME = ME + 1
C            ENDIF
C         ELSE
C            POS = .FALSE.
C         ENDIF
CC
C         IF (.NOT.POS) THEN
C            NSOLN = NSOLN - 1
C            NIV = NIV - 1
C         ENDIF
C         IF (.NOT.(POS.OR.DONE)) GO TO 290
C      ENDIF
C      GO TO 160
CC
CC     Else perform multiplier test and drop a constraint.  To compute
CC     final solution.  Solve system, store results in X(*).
CC
CC     Copy right hand side into TEMP vector to use overwriting method.
CC
C  330 ISOL = 1
C      IF (NSOLN.GE.ISOL) THEN
C         CALL DCOPY (NIV, W(1,N+1), 1, TEMP, 1)
C         DO 340 J = NSOLN,ISOL,-1
C            IF (J.GT.KRANK) THEN
C               I = NIV - NSOLN + J
C            ELSE
C               I = J
C            ENDIF
CC
C            IF (J.GT.KRANK .AND. J.LE.L) THEN
C               Z(J) = 0.D0
C            ELSE
C               Z(J) = TEMP(I)/W(I,J)
C               CALL DAXPY (I-1, -Z(J), W(1,J), 1, TEMP, 1)
C            ENDIF
C  340    CONTINUE
C      ENDIF
CC
CC     Solve system.
CC
C      CALL DCOPY (NSOLN, Z, 1, X, 1)
CC
CC     Apply Householder transformations to X(*) if KRANK.LT.L
CC
C      IF (KRANK.LT.L) THEN
C         DO 350 I = 1,KRANK
C            CALL DH12 (2, I, KRANK+1, L, W(I,1), MDW, H(I), X, 1, 1, 1)
C  350    CONTINUE
C      ENDIF
CC
CC     Fill in trailing zeroes for constrained variables not in solution.
CC
C      IF (NSOLN.LT.N) CALL DCOPY (N-NSOLN, 0.D0, 0, X(NSOLN+1), 1)
CC
CC     Permute solution vector to natural order.
CC
C      DO 380 I = 1,N
C         J = I
C  360    IF (IPIVOT(J).EQ.I) GO TO 370
C         J = J + 1
C         GO TO 360
CC
C  370    IPIVOT(J) = IPIVOT(I)
C         IPIVOT(I) = J
C         CALL DSWAP (1, X(J), 1, X(I), 1)
C  380 CONTINUE
CC
CC     Rescale the solution using the column scaling.
CC
C      DO 390 J = 1,N
C         X(J) = X(J)*D(J)
C  390 CONTINUE
CC
C      DO 400 I = NSOLN+1,M
C         T = W(I,N+1)
C         IF (I.LE.ME) T = T/ALAMDA
C         T = (SCALE(I)*T)*T
C         RNORM = RNORM + T
C  400 CONTINUE
CC
C      RNORM = SQRT(RNORM)
C      RETURN
C      END
C      SUBROUTINE DWNLT1 (I, LEND, MEND, IR, MDW, RECALC, IMAX, HBAR, H,
C     +   SCALE, W)
CC***BEGIN PROLOGUE  DWNLT1
CC***SUBSIDIARY
CC***PURPOSE  Subsidiary to WNLIT
CC***LIBRARY   SLATEC
CC***TYPE      DOUBLE PRECISION (WNLT1-S, DWNLT1-D)
CC***AUTHOR  Hanson, R. J., (SNLA)
CC           Haskell, K. H., (SNLA)
CC***DESCRIPTION
CC
CC     To update the column Sum Of Squares and find the pivot column.
CC     The column Sum of Squares Vector will be updated at each step.
CC     When numerically necessary, these values will be recomputed.
CC
CC***SEE ALSO  DWNLIT
CC***ROUTINES CALLED  IDAMAX
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890620  Code extracted from WNLIT and made a subroutine.  (RWC))
CC   900604  DP version created from SP version.  (RWC)
CC***END PROLOGUE  DWNLT1
C      INTEGER I, IMAX, IR, LEND, MDW, MEND
C      DOUBLE PRECISION H(*), HBAR, SCALE(*), W(MDW,*)
C      LOGICAL RECALC
CC
C      EXTERNAL IDAMAX
C      INTEGER IDAMAX
CC
C      INTEGER J, K
CC
CC***FIRST EXECUTABLE STATEMENT  DWNLT1
C      IF (IR.NE.1 .AND. (.NOT.RECALC)) THEN
CC
CC        Update column SS=sum of squares.
CC
C         DO 10 J=I,LEND
C            H(J) = H(J) - SCALE(IR-1)*W(IR-1,J)**2
C   10    CONTINUE
CC
CC        Test for numerical accuracy.
CC
C         IMAX = IDAMAX(LEND-I+1, H(I), 1) + I - 1
C         RECALC = (HBAR+1.E-3*H(IMAX)) .EQ. HBAR
C      ENDIF
CC
CC     If required, recalculate column SS, using rows IR through MEND.
CC
C      IF (RECALC) THEN
C         DO 30 J=I,LEND
C            H(J) = 0.D0
C            DO 20 K=IR,MEND
C               H(J) = H(J) + SCALE(K)*W(K,J)**2
C   20       CONTINUE
C   30    CONTINUE
CC
CC        Find column with largest SS.
CC
C         IMAX = IDAMAX(LEND-I+1, H(I), 1) + I - 1
C         HBAR = H(IMAX)
C      ENDIF
C      RETURN
C      END
C      LOGICAL FUNCTION DWNLT2 (ME, MEND, IR, FACTOR, TAU, SCALE, WIC)
CC***BEGIN PROLOGUE  DWNLT2
CC***SUBSIDIARY
CC***PURPOSE  Subsidiary to WNLIT
CC***LIBRARY   SLATEC
CC***TYPE      DOUBLE PRECISION (WNLT2-S, DWNLT2-D)
CC***AUTHOR  Hanson, R. J., (SNLA)
CC           Haskell, K. H., (SNLA)
CC***DESCRIPTION
CC
CC     To test independence of incoming column.
CC
CC     Test the column IC to determine if it is linearly independent
CC     of the columns already in the basis.  In the initial tri. step,
CC     we usually want the heavy weight ALAMDA to be included in the
CC     test for independence.  In this case, the value of FACTOR will
CC     have been set to 1.E0 before this procedure is invoked.
CC     In the potentially rank deficient problem, the value of FACTOR
CC     will have been set to ALSQ=ALAMDA**2 to remove the effect of the
CC     heavy weight from the test for independence.
CC
CC     Write new column as partitioned vector
CC           (A1)  number of components in solution so far = NIV
CC           (A2)  M-NIV components
CC     And compute  SN = inverse weighted length of A1
CC                  RN = inverse weighted length of A2
CC     Call the column independent when RN .GT. TAU*SN
CC
CC***SEE ALSO  DWNLIT
CC***ROUTINES CALLED  (NONE)
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890620  Code extracted from WNLIT and made a subroutine.  (RWC))
CC   900604  DP version created from SP version.  (RWC)
CC***END PROLOGUE  DWNLT2
C      DOUBLE PRECISION FACTOR, SCALE(*), TAU, WIC(*)
C      INTEGER IR, ME, MEND
CC
C      DOUBLE PRECISION RN, SN, T
C      INTEGER J
CC
CC***FIRST EXECUTABLE STATEMENT  DWNLT2
C      SN = 0.E0
C      RN = 0.E0
C      DO 10 J=1,MEND
C         T = SCALE(J)
C         IF (J.LE.ME) T = T/FACTOR
C         T = T*WIC(J)**2
CC
C         IF (J.LT.IR) THEN
C            SN = SN + T
C         ELSE
C            RN = RN + T
C         ENDIF
C   10 CONTINUE
C      DWNLT2 = RN .GT. SN*TAU**2
C      RETURN
C      END
C      SUBROUTINE DWNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
CC***BEGIN PROLOGUE  DWNLT3
CC***SUBSIDIARY
CC***PURPOSE  Subsidiary to WNLIT
CC***LIBRARY   SLATEC
CC***TYPE      DOUBLE PRECISION (WNLT3-S, DWNLT3-D)
CC***AUTHOR  Hanson, R. J., (SNLA)
CC           Haskell, K. H., (SNLA)
CC***DESCRIPTION
CC
CC     Perform column interchange.
CC     Exchange elements of permuted index vector and perform column
CC     interchanges.
CC
CC***SEE ALSO  DWNLIT
CC***ROUTINES CALLED  DSWAP
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890620  Code extracted from WNLIT and made a subroutine.  (RWC))
CC   900604  DP version created from SP version.  (RWC)
CC***END PROLOGUE  DWNLT3
C      INTEGER I, IMAX, IPIVOT(*), M, MDW
C      DOUBLE PRECISION H(*), W(MDW,*)
CC
C      EXTERNAL DSWAP
CC
C      DOUBLE PRECISION T
C      INTEGER ITEMP
CC
CC***FIRST EXECUTABLE STATEMENT  DWNLT3
C      IF (IMAX.NE.I) THEN
C         ITEMP        = IPIVOT(I)
C         IPIVOT(I)    = IPIVOT(IMAX)
C         IPIVOT(IMAX) = ITEMP
CC
C         CALL DSWAP(M, W(1,IMAX), 1, W(1,I), 1)
CC
C         T       = H(IMAX)
C         H(IMAX) = H(I)
C         H(I)    = T
C      ENDIF
C      RETURN
C      END
C      SUBROUTINE DWNNLS (W, MDW, ME, MA, N, L, PRGOPT, X, RNORM, MODE,
C     +   IWORK, WORK)
CC***BEGIN PROLOGUE  DWNNLS
CC***PURPOSE  Solve a linearly constrained least squares problem with
CC            equality constraints and nonnegativity constraints on
CC            selected variables.
CC***LIBRARY   SLATEC
CC***CATEGORY  K1A2A
CC***TYPE      DOUBLE PRECISION (WNNLS-S, DWNNLS-D)
CC***KEYWORDS  CONSTRAINED LEAST SQUARES, CURVE FITTING, DATA FITTING,
CC             EQUALITY CONSTRAINTS, INEQUALITY CONSTRAINTS,
CC             NONNEGATIVITY CONSTRAINTS, QUADRATIC PROGRAMMING
CC***AUTHOR  Hanson, R. J., (SNLA)
CC           Haskell, K. H., (SNLA)
CC***DESCRIPTION
CC
CC     Abstract
CC
CC     This subprogram solves a linearly constrained least squares
CC     problem.  Suppose there are given matrices E and A of
CC     respective dimensions ME by N and MA by N, and vectors F
CC     and B of respective lengths ME and MA.  This subroutine
CC     solves the problem
CC
CC               EX = F, (equations to be exactly satisfied)
CC
CC               AX = B, (equations to be approximately satisfied,
CC                        in the least squares sense)
CC
CC               subject to components L+1,...,N nonnegative
CC
CC     Any values ME.GE.0, MA.GE.0 and 0.LE. L .LE.N are permitted.
CC
CC     The problem is reposed as problem DWNNLS
CC
CC               (WT*E)X = (WT*F)
CC               (   A)    (   B), (least squares)
CC               subject to components L+1,...,N nonnegative.
CC
CC     The subprogram chooses the heavy weight (or penalty parameter) WT.
CC
CC     The parameters for DWNNLS are
CC
CC     INPUT.. All TYPE REAL variables are DOUBLE PRECISION
CC
CC     W(*,*),MDW,  The array W(*,*) is double subscripted with first
CC     ME,MA,N,L    dimensioning parameter equal to MDW.  For this
CC                  discussion let us call M = ME + MA.  Then MDW
CC                  must satisfy MDW.GE.M.  The condition MDW.LT.M
CC                  is an error.
CC
CC                  The array W(*,*) contains the matrices and vectors
CC
CC                       (E  F)
CC                       (A  B)
CC
CC                  in rows and columns 1,...,M and 1,...,N+1
CC                  respectively.  Columns 1,...,L correspond to
CC                  unconstrained variables X(1),...,X(L).  The
CC                  remaining variables are constrained to be
CC                  nonnegative. The condition L.LT.0 or L.GT.N is
CC                  an error.
CC
CC     PRGOPT(*)    This double precision array is the option vector.
CC                  If the user is satisfied with the nominal
CC                  subprogram features set
CC
CC                  PRGOPT(1)=1 (or PRGOPT(1)=1.0)
CC
CC                  Otherwise PRGOPT(*) is a linked list consisting of
CC                  groups of data of the following form
CC
CC                  LINK
CC                  KEY
CC                  DATA SET
CC
CC                  The parameters LINK and KEY are each one word.
CC                  The DATA SET can be comprised of several words.
CC                  The number of items depends on the value of KEY.
CC                  The value of LINK points to the first
CC                  entry of the next group of data within
CC                  PRGOPT(*).  The exception is when there are
CC                  no more options to change.  In that
CC                  case LINK=1 and the values KEY and DATA SET
CC                  are not referenced. The general layout of
CC                  PRGOPT(*) is as follows.
CC
CC               ...PRGOPT(1)=LINK1 (link to first entry of next group)
CC               .  PRGOPT(2)=KEY1 (key to the option change)
CC               .  PRGOPT(3)=DATA VALUE (data value for this change)
CC               .       .
CC               .       .
CC               .       .
CC               ...PRGOPT(LINK1)=LINK2 (link to the first entry of
CC               .                       next group)
CC               .  PRGOPT(LINK1+1)=KEY2 (key to the option change)
CC               .  PRGOPT(LINK1+2)=DATA VALUE
CC               ...     .
CC               .       .
CC               .       .
CC               ...PRGOPT(LINK)=1 (no more options to change)
CC
CC                  Values of LINK that are nonpositive are errors.
CC                  A value of LINK.GT.NLINK=100000 is also an error.
CC                  This helps prevent using invalid but positive
CC                  values of LINK that will probably extend
CC                  beyond the program limits of PRGOPT(*).
CC                  Unrecognized values of KEY are ignored.  The
CC                  order of the options is arbitrary and any number
CC                  of options can be changed with the following
CC                  restriction.  To prevent cycling in the
CC                  processing of the option array a count of the
CC                  number of options changed is maintained.
CC                  Whenever this count exceeds NOPT=1000 an error
CC                  message is printed and the subprogram returns.
CC
CC                  OPTIONS..
CC
CC                  KEY=6
CC                         Scale the nonzero columns of the
CC                  entire data matrix
CC                  (E)
CC                  (A)
CC                  to have length one. The DATA SET for
CC                  this option is a single value.  It must
CC                  be nonzero if unit length column scaling is
CC                  desired.
CC
CC                  KEY=7
CC                         Scale columns of the entire data matrix
CC                  (E)
CC                  (A)
CC                  with a user-provided diagonal matrix.
CC                  The DATA SET for this option consists
CC                  of the N diagonal scaling factors, one for
CC                  each matrix column.
CC
CC                  KEY=8
CC                         Change the rank determination tolerance from
CC                  the nominal value of SQRT(SRELPR).  This quantity
CC                  can be no smaller than SRELPR, The arithmetic-
CC                  storage precision.  The quantity used
CC                  here is internally restricted to be at
CC                  least SRELPR.  The DATA SET for this option
CC                  is the new tolerance.
CC
CC                  KEY=9
CC                         Change the blow-up parameter from the
CC                  nominal value of SQRT(SRELPR).  The reciprocal of
CC                  this parameter is used in rejecting solution
CC                  components as too large when a variable is
CC                  first brought into the active set.  Too large
CC                  means that the proposed component times the
CC                  reciprocal of the parameter is not less than
CC                  the ratio of the norms of the right-side
CC                  vector and the data matrix.
CC                  This parameter can be no smaller than SRELPR,
CC                  the arithmetic-storage precision.
CC
CC                  For example, suppose we want to provide
CC                  a diagonal matrix to scale the problem
CC                  matrix and change the tolerance used for
CC                  determining linear dependence of dropped col
CC                  vectors.  For these options the dimensions of
CC                  PRGOPT(*) must be at least N+6.  The FORTRAN
CC                  statements defining these options would
CC                  be as follows.
CC
CC                  PRGOPT(1)=N+3 (link to entry N+3 in PRGOPT(*))
CC                  PRGOPT(2)=7 (user-provided scaling key)
CC
CC                  CALL DCOPY(N,D,1,PRGOPT(3),1) (copy the N
CC                  scaling factors from a user array called D(*)
CC                  into PRGOPT(3)-PRGOPT(N+2))
CC
CC                  PRGOPT(N+3)=N+6 (link to entry N+6 of PRGOPT(*))
CC                  PRGOPT(N+4)=8 (linear dependence tolerance key)
CC                  PRGOPT(N+5)=... (new value of the tolerance)
CC
CC                  PRGOPT(N+6)=1 (no more options to change)
CC
CC
CC     IWORK(1),    The amounts of working storage actually allocated
CC     IWORK(2)     for the working arrays WORK(*) and IWORK(*),
CC                  respectively.  These quantities are compared with
CC                  the actual amounts of storage needed for DWNNLS( ).
CC                  Insufficient storage allocated for either WORK(*)
CC                  or IWORK(*) is considered an error.  This feature
CC                  was included in DWNNLS( ) because miscalculating
CC                  the storage formulas for WORK(*) and IWORK(*)
CC                  might very well lead to subtle and hard-to-find
CC                  execution errors.
CC
CC                  The length of WORK(*) must be at least
CC
CC                  LW = ME+MA+5*N
CC                  This test will not be made if IWORK(1).LE.0.
CC
CC                  The length of IWORK(*) must be at least
CC
CC                  LIW = ME+MA+N
CC                  This test will not be made if IWORK(2).LE.0.
CC
CC     OUTPUT.. All TYPE REAL variables are DOUBLE PRECISION
CC
CC     X(*)         An array dimensioned at least N, which will
CC                  contain the N components of the solution vector
CC                  on output.
CC
CC     RNORM        The residual norm of the solution.  The value of
CC                  RNORM contains the residual vector length of the
CC                  equality constraints and least squares equations.
CC
CC     MODE         The value of MODE indicates the success or failure
CC                  of the subprogram.
CC
CC                  MODE = 0  Subprogram completed successfully.
CC
CC                       = 1  Max. number of iterations (equal to
CC                            3*(N-L)) exceeded. Nearly all problems
CC                            should complete in fewer than this
CC                            number of iterations. An approximate
CC                            solution and its corresponding residual
CC                            vector length are in X(*) and RNORM.
CC
CC                       = 2  Usage error occurred.  The offending
CC                            condition is noted with the error
CC                            processing subprogram, XERMSG( ).
CC
CC     User-designated
CC     Working arrays..
CC
CC     WORK(*)      A double precision working array of length at least
CC                  M + 5*N.
CC
CC     IWORK(*)     An integer-valued working array of length at least
CC                  M+N.
CC
CC***REFERENCES  K. H. Haskell and R. J. Hanson, An algorithm for
CC                 linear least squares problems with equality and
CC                 nonnegativity constraints, Report SAND77-0552, Sandia
CC                 Laboratories, June 1978.
CC               K. H. Haskell and R. J. Hanson, Selected algorithms for
CC                 the linearly constrained least squares problem - a
CC                 users guide, Report SAND78-1290, Sandia Laboratories,
CC                 August 1979.
CC               K. H. Haskell and R. J. Hanson, An algorithm for
CC                 linear least squares problems with equality and
CC                 nonnegativity constraints, Mathematical Programming
CC                 21 (1981), pp. 98-118.
CC               R. J. Hanson and K. H. Haskell, Two algorithms for the
CC                 linearly constrained least squares problem, ACM
CC                 Transactions on Mathematical Software, September 1982.
CC               C. L. Lawson and R. J. Hanson, Solving Least Squares
CC                 Problems, Prentice-Hall, Inc., 1974.
CC***ROUTINES CALLED  DWNLSM, XERMSG
CC***REVISION HISTORY  (YYMMDD)
CC   790701  DATE WRITTEN
CC   890531  Changed all specific intrinsics to generic.  (WRB)
CC   890618  Completely restructured and revised.  (WRB & RWC)
CC   891006  REVISION DATE from Version 3.2
CC   891214  Prologue converted to Version 4.0 format.  (BAB)
CC   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
CC   900510  Convert XERRWV calls to XERMSG calls, change Prologue
CC           comments to agree with WNNLS.  (RWC)
CC   920501  Reformatted the REFERENCES section.  (WRB)
CC***END PROLOGUE  DWNNLS
C      INTEGER IWORK(*), L, L1, L2, L3, L4, L5, LIW, LW, MA, MDW, ME,
C     *     MODE, N
C      DOUBLE PRECISION  PRGOPT(*), RNORM, W(MDW,*), WORK(*), X(*)
C      CHARACTER*8 XERN1
CC***FIRST EXECUTABLE STATEMENT  DWNNLS
C      MODE = 0
C      IF (MA+ME.LE.0 .OR. N.LE.0) RETURN
CC
C      IF (IWORK(1).GT.0) THEN
C         LW = ME + MA + 5*N
C         IF (IWORK(1).LT.LW) THEN
C            WRITE (XERN1, '(I8)') LW
C            CALL XERMSG ('SLATEC', 'DWNNLS', 'INSUFFICIENT STORAGE ' //
C     *         'ALLOCATED FOR WORK(*), NEED LW = ' // XERN1, 2, 1)
C            MODE = 2
C            RETURN
C         ENDIF
C      ENDIF
CC
C      IF (IWORK(2).GT.0) THEN
C         LIW = ME + MA + N
C         IF (IWORK(2).LT.LIW) THEN
C            WRITE (XERN1, '(I8)') LIW
C            CALL XERMSG ('SLATEC', 'DWNNLS', 'INSUFFICIENT STORAGE ' //
C     *         'ALLOCATED FOR IWORK(*), NEED LIW = ' // XERN1, 2, 1)
C            MODE = 2
C            RETURN
C         ENDIF
C      ENDIF
CC
C      IF (MDW.LT.ME+MA) THEN
C         CALL XERMSG ('SLATEC', 'DWNNLS',
C     *      'THE VALUE MDW.LT.ME+MA IS AN ERROR', 1, 1)
C         MODE = 2
C         RETURN
C      ENDIF
CC
C      IF (L.LT.0 .OR. L.GT.N) THEN
C         CALL XERMSG ('SLATEC', 'DWNNLS',
C     *      'L.GE.0 .AND. L.LE.N IS REQUIRED', 2, 1)
C         MODE = 2
C         RETURN
C      ENDIF
CC
CC     THE PURPOSE OF THIS SUBROUTINE IS TO BREAK UP THE ARRAYS
CC     WORK(*) AND IWORK(*) INTO SEPARATE WORK ARRAYS
CC     REQUIRED BY THE MAIN SUBROUTINE DWNLSM( ).
CC
C      L1 = N + 1
C      L2 = L1 + N
C      L3 = L2 + ME + MA
C      L4 = L3 + N
C      L5 = L4 + N
CC
C      CALL DWNLSM(W, MDW, ME, MA, N, L, PRGOPT, X, RNORM, MODE, IWORK,
C     *            IWORK(L1), WORK(1), WORK(L1), WORK(L2), WORK(L3),
C     *            WORK(L4), WORK(L5))
C      RETURN
C      END
C      double precision function dnrm2 ( n, dx, incx)
C      integer i, incx, ix, j, n, next
C      double precision   dx(1), cutlo, cuthi, hitest, sum, xmax,zero,one
C      data   zero, one /0.0d0, 1.0d0/
Cc
Cc     euclidean norm of the n-vector stored in dx() with storage
Cc     increment incx .
Cc     if    n .le. 0 return with result = 0.
Cc     if n .ge. 1 then incx must be .ge. 1
Cc
Cc           c.l.lawson, 1978 jan 08
Cc     modified to correct failure to update ix, 1/25/92.
Cc     modified 3/93 to return if incx .le. 0.
Cc
Cc     four phase method     using two built-in constants that are
Cc     hopefully applicable to all machines.
Cc         cutlo = maximum of  dsqrt(u/eps)  over all known machines.
Cc         cuthi = minimum of  dsqrt(v)      over all known machines.
Cc     where
Cc         eps = smallest no. such that eps + 1. .gt. 1.
Cc         u   = smallest positive no.   (underflow limit)
Cc         v   = largest  no.            (overflow  limit)
Cc
Cc     brief outline of algorithm..
Cc
Cc     phase 1    scans zero components.
Cc     move to phase 2 when a component is nonzero and .le. cutlo
Cc     move to phase 3 when a component is .gt. cutlo
Cc     move to phase 4 when a component is .ge. cuthi/m
Cc     where m = n for x() real and m = 2*n for complex.
Cc
Cc     values for cutlo and cuthi..
Cc     from the environmental parameters listed in the imsl converter
Cc     document the limiting values are as follows..
Cc     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
Cc                   univac and dec at 2**(-103)
Cc                   thus cutlo = 2**(-51) = 4.44089e-16
Cc     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
Cc                   thus cuthi = 2**(63.5) = 1.30438e19
Cc     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
Cc                   thus cutlo = 2**(-33.5) = 8.23181d-11
Cc     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
Cc     data cutlo, cuthi / 8.232d-11,  1.304d19 /
Cc     data cutlo, cuthi / 4.441e-16,  1.304e19 /
C      data cutlo, cuthi / 8.232d-11,  1.304d19 /
Cc
C      if(n .gt. 0 .and. incx.gt.0) go to 10
C         dnrm2  = zero
C         go to 300
Cc
C   10 assign 30 to next
C      sum = zero
C      i = 1
C      ix = 1
Cc                                                 begin main loop
C   20    go to next,(30, 50, 70, 110)
C   30 if( dabs(dx(i)) .gt. cutlo) go to 85
C      assign 50 to next
C      xmax = zero
Cc
Cc                        phase 1.  sum is zero
Cc
C   50 if( dx(i) .eq. zero) go to 200
C      if( dabs(dx(i)) .gt. cutlo) go to 85
Cc
Cc                                prepare for phase 2.
C      assign 70 to next
C      go to 105
Cc
Cc                                prepare for phase 4.
Cc
C  100 continue
C      ix = j
C      assign 110 to next
C      sum = (sum / dx(i)) / dx(i)
C  105 xmax = dabs(dx(i))
C      go to 115
Cc
Cc                   phase 2.  sum is small.
Cc                             scale to avoid destructive underflow.
Cc
C   70 if( dabs(dx(i)) .gt. cutlo ) go to 75
Cc
Cc                     common code for phases 2 and 4.
Cc                     in phase 4 sum is large.  scale to avoid overflow.
Cc
C  110 if( dabs(dx(i)) .le. xmax ) go to 115
C         sum = one + sum * (xmax / dx(i))**2
C         xmax = dabs(dx(i))
C         go to 200
Cc
C  115 sum = sum + (dx(i)/xmax)**2
C      go to 200
Cc
Cc
Cc                  prepare for phase 3.
Cc
C   75 sum = (sum * xmax) * xmax
Cc
Cc
Cc     for real or d.p. set hitest = cuthi/n
Cc     for complex      set hitest = cuthi/(2*n)
Cc
C   85 hitest = cuthi/float( n )
Cc
Cc                   phase 3.  sum is mid-range.  no scaling.
Cc
C      do 95 j = ix,n
C      if(dabs(dx(i)) .ge. hitest) go to 100
C         sum = sum + dx(i)**2
C         i = i + incx
C   95 continue
C      dnrm2 = dsqrt( sum )
C      go to 300
Cc
C  200 continue
C      ix = ix + 1
C      i = i + incx
C      if( ix .le. n ) go to 20
Cc
Cc              end of main loop.
Cc
Cc              compute square root and adjust for scaling.
Cc
C      dnrm2 = xmax * dsqrt(sum)
C  300 continue
C      return
C      end
C*****END precision > double
C
C*****precision > single
      SUBROUTINE H12 (MODE, LPIVOT, L1, M, U, IUE, UP, C, ICE, ICV, NCV)
C***BEGIN PROLOGUE  H12
C***SUBSIDIARY
C***PURPOSE  Subsidiary to HFTI, LSEI and WNNLS
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (H12-S, DH12-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     C.L.Lawson and R.J.Hanson, Jet Propulsion Laboratory, 1973 Jun 12
C     to appear in 'Solving Least Squares Problems', Prentice-Hall, 1974
C
C     Construction and/or application of a single
C     Householder transformation..     Q = I + U*(U**T)/B
C
C     MODE    = 1 or 2   to select algorithm  H1  or  H2 .
C     LPIVOT is the index of the pivot element.
C     L1,M   If L1 .LE. M   the transformation will be constructed to
C            zero elements indexed from L1 through M.   If L1 GT. M
C            THE SUBROUTINE DOES AN IDENTITY TRANSFORMATION.
C     U(),IUE,UP    On entry to H1 U() contains the pivot vector.
C                   IUE is the storage increment between elements.
C                                       On exit from H1 U() and UP
C                   contain quantities defining the vector U of the
C                   Householder transformation.   On entry to H2 U()
C                   and UP should contain quantities previously computed
C                   by H1.  These will not be modified by H2.
C     C()    On entry to H1 or H2 C() contains a matrix which will be
C            regarded as a set of vectors to which the Householder
C            transformation is to be applied.  On exit C() contains the
C            set of transformed vectors.
C     ICE    Storage increment between elements of vectors in C().
C     ICV    Storage increment between vectors in C().
C     NCV    Number of vectors in C() to be transformed. If NCV .LE. 0
C            no operations will be done on C().
C
C***SEE ALSO  HFTI, LSEI, WNNLS
C***ROUTINES CALLED  SAXPY, SDOT, SSWAP
C***REVISION HISTORY  (YYMMDD)
C   790101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  H12
      DIMENSION U(IUE,*), C(*)
C***FIRST EXECUTABLE STATEMENT  H12
      ONE=1.
C
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN
      CL=ABS(U(1,LPIVOT))
      IF (MODE.EQ.2) GO TO 60
C                            ****** CONSTRUCT THE TRANSFORMATION. ******
          DO 10 J=L1,M
   10     CL=MAX(ABS(U(1,J)),CL)
      IF (CL) 130,130,20
   20 CLINV=ONE/CL
      SM=(U(1,LPIVOT)*CLINV)**2
          DO 30 J=L1,M
   30     SM=SM+(U(1,J)*CLINV)**2
      CL=CL*SQRT(SM)
      IF (U(1,LPIVOT)) 50,50,40
   40 CL=-CL
   50 UP=U(1,LPIVOT)-CL
      U(1,LPIVOT)=CL
      GO TO 70
C            ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ******
C
   60 IF (CL) 130,130,70
   70 IF (NCV.LE.0) RETURN
      B=UP*U(1,LPIVOT)
C                       B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN.
C
      IF (B) 80,130,130
   80 B=ONE/B
      MML1P2=M-L1+2
      IF (MML1P2.GT.20) GO TO 140
      I2=1-ICV+ICE*(LPIVOT-1)
      INCR=ICE*(L1-LPIVOT)
          DO 120 J=1,NCV
          I2=I2+ICV
          I3=I2+INCR
          I4=I3
          SM=C(I2)*UP
              DO 90 I=L1,M
              SM=SM+C(I3)*U(1,I)
   90         I3=I3+ICE
          IF (SM) 100,120,100
  100     SM=SM*B
          C(I2)=C(I2)+SM*UP
              DO 110 I=L1,M
              C(I4)=C(I4)+SM*U(1,I)
  110         I4=I4+ICE
  120     CONTINUE
  130 RETURN
  140 CONTINUE
      L1M1=L1-1
      KL1=1+(L1M1-1)*ICE
      KL2=KL1
      KLP=1+(LPIVOT-1)*ICE
      UL1M1=U(1,L1M1)
      U(1,L1M1)=UP
      IF (LPIVOT.EQ.L1M1) GO TO 150
      CALL SSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
  150 CONTINUE
          DO 160 J=1,NCV
          SM=SDOT(MML1P2,U(1,L1M1),IUE,C(KL1),ICE)
          SM=SM*B
          CALL SAXPY (MML1P2,SM,U(1,L1M1),IUE,C(KL1),ICE)
          KL1=KL1+ICV
  160 CONTINUE
      U(1,L1M1)=UL1M1
      IF (LPIVOT.EQ.L1M1) RETURN
      KL1=KL2
      CALL SSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
      RETURN
      END
      SUBROUTINE HFTI (A, MDA, M, N, B, MDB, NB, TAU, KRANK, RNORM, H,
     +   G, IP)
C***BEGIN PROLOGUE  HFTI
C***PURPOSE  Solve a linear least squares problems by performing a QR
C            factorization of the matrix using Householder
C            transformations.
C***LIBRARY   SLATEC
C***CATEGORY  D9
C***TYPE      SINGLE PRECISION (HFTI-S, DHFTI-D)
C***KEYWORDS  CURVE FITTING, LINEAR LEAST SQUARES, QR FACTORIZATION
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C***DESCRIPTION
C
C     DIMENSION A(MDA,N),(B(MDB,NB) or B(M)),RNORM(NB),H(N),G(N),IP(N)
C
C     This subroutine solves a linear least squares problem or a set of
C     linear least squares problems having the same matrix but different
C     right-side vectors.  The problem data consists of an M by N matrix
C     A, an M by NB matrix B, and an absolute tolerance parameter TAU
C     whose usage is described below.  The NB column vectors of B
C     represent right-side vectors for NB distinct linear least squares
C     problems.
C
C     This set of problems can also be written as the matrix least
C     squares problem
C
C                       AX = B,
C
C     where X is the N by NB solution matrix.
C
C     Note that if B is the M by M identity matrix, then X will be the
C     pseudo-inverse of A.
C
C     This subroutine first transforms the augmented matrix (A B) to a
C     matrix (R C) using premultiplying Householder transformations with
C     column interchanges.  All subdiagonal elements in the matrix R are
C     zero and its diagonal elements satisfy
C
C                       ABS(R(I,I)).GE.ABS(R(I+1,I+1)),
C
C                       I = 1,...,L-1, where
C
C                       L = MIN(M,N).
C
C     The subroutine will compute an integer, KRANK, equal to the number
C     of diagonal terms of R that exceed TAU in magnitude. Then a
C     solution of minimum Euclidean length is computed using the first
C     KRANK rows of (R C).
C
C     To be specific we suggest that the user consider an easily
C     computable matrix norm, such as, the maximum of all column sums of
C     magnitudes.
C
C     Now if the relative uncertainty of B is EPS, (norm of uncertainty/
C     norm of B), it is suggested that TAU be set approximately equal to
C     EPS*(norm of A).
C
C     The user must dimension all arrays appearing in the call list..
C     A(MDA,N),(B(MDB,NB) or B(M)),RNORM(NB),H(N),G(N),IP(N).  This
C     permits the solution of a range of problems in the same array
C     space.
C
C     The entire set of parameters for HFTI are
C
C     INPUT..
C
C     A(*,*),MDA,M,N    The array A(*,*) initially contains the M by N
C                       matrix A of the least squares problem AX = B.
C                       The first dimensioning parameter of the array
C                       A(*,*) is MDA, which must satisfy MDA.GE.M
C                       Either M.GE.N or M.LT.N is permitted.  There
C                       is no restriction on the rank of A.  The
C                       condition MDA.LT.M is considered an error.
C
C     B(*),MDB,NB       If NB = 0 the subroutine will perform the
C                       orthogonal decomposition but will make no
C                       references to the array B(*).  If NB.GT.0
C                       the array B(*) must initially contain the M by
C                       NB matrix B of the least squares problem AX =
C                       B.  If NB.GE.2 the array B(*) must be doubly
C                       subscripted with first dimensioning parameter
C                       MDB.GE.MAX(M,N).  If NB = 1 the array B(*) may
C                       be either doubly or singly subscripted.  In
C                       the latter case the value of MDB is arbitrary
C                       but it should be set to some valid integer
C                       value such as MDB = M.
C
C                       The condition of NB.GT.1.AND.MDB.LT. MAX(M,N)
C                       is considered an error.
C
C     TAU               Absolute tolerance parameter provided by user
C                       for pseudorank determination.
C
C     H(*),G(*),IP(*)   Arrays of working space used by HFTI.
C
C     OUTPUT..
C
C     A(*,*)            The contents of the array A(*,*) will be
C                       modified by the subroutine. These contents
C                       are not generally required by the user.
C
C     B(*)              On return the array B(*) will contain the N by
C                       NB solution matrix X.
C
C     KRANK             Set by the subroutine to indicate the
C                       pseudorank of A.
C
C     RNORM(*)          On return, RNORM(J) will contain the Euclidean
C                       norm of the residual vector for the problem
C                       defined by the J-th column vector of the array
C                       B(*,*) for J = 1,...,NB.
C
C     H(*),G(*)         On return these arrays respectively contain
C                       elements of the pre- and post-multiplying
C                       Householder transformations used to compute
C                       the minimum Euclidean length solution.
C
C     IP(*)             Array in which the subroutine records indices
C                       describing the permutation of column vectors.
C                       The contents of arrays H(*),G(*) and IP(*)
C                       are not generally required by the user.
C
C***REFERENCES  C. L. Lawson and R. J. Hanson, Solving Least Squares
C                 Problems, Prentice-Hall, Inc., 1974, Chapter 14.
C***ROUTINES CALLED  H12, R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   901005  Replace usage of DIFF with usage of R1MACH.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  HFTI
      DIMENSION A(MDA,*),B(MDB,*),H(*),G(*),RNORM(*)
      INTEGER IP(*)
      DOUBLE PRECISION SM,DZERO
      SAVE RELEPS
      DATA RELEPS /0.E0/
C***FIRST EXECUTABLE STATEMENT  HFTI
      IF (RELEPS.EQ.0) RELEPS = R1MACH(4)
      SZERO=0.
      DZERO=0.D0
      FACTOR=0.001
C
      K=0
      LDIAG=MIN(M,N)
      IF (LDIAG.LE.0) GO TO 270
      IF (.NOT.MDA.LT.M) GO TO 5
      NERR=1
      IOPT=2
      CALL XERMSG ('SLATEC', 'HFTI', 'MDA.LT.M, PROBABLE ERROR.',
     +   NERR, IOPT)
      RETURN
    5 CONTINUE
C
      IF (.NOT.(NB.GT.1.AND.MAX(M,N).GT.MDB)) GO TO 6
      NERR=2
      IOPT=2
      CALL XERMSG ('SLATEC', 'HFTI',
     +   'MDB.LT.MAX(M,N).AND.NB.GT.1. PROBABLE ERROR.', NERR, IOPT)
      RETURN
    6 CONTINUE
C
          DO 80 J=1,LDIAG
          IF (J.EQ.1) GO TO 20
C
C     UPDATE SQUARED COLUMN LENGTHS AND FIND LMAX
C    ..
          LMAX=J
              DO 10 L=J,N
              H(L)=H(L)-A(J-1,L)**2
              IF (H(L).GT.H(LMAX)) LMAX=L
   10         CONTINUE
          IF (FACTOR*H(LMAX) .GT. HMAX*RELEPS) GO TO 50
C
C     COMPUTE SQUARED COLUMN LENGTHS AND FIND LMAX
C    ..
   20     LMAX=J
              DO 40 L=J,N
              H(L)=0.
                  DO 30 I=J,M
   30             H(L)=H(L)+A(I,L)**2
              IF (H(L).GT.H(LMAX)) LMAX=L
   40         CONTINUE
          HMAX=H(LMAX)
C    ..
C     LMAX HAS BEEN DETERMINED
C
C     DO COLUMN INTERCHANGES IF NEEDED.
C    ..
   50     CONTINUE
          IP(J)=LMAX
          IF (IP(J).EQ.J) GO TO 70
              DO 60 I=1,M
              TMP=A(I,J)
              A(I,J)=A(I,LMAX)
   60         A(I,LMAX)=TMP
          H(LMAX)=H(J)
C
C     COMPUTE THE J-TH TRANSFORMATION AND APPLY IT TO A AND B.
C    ..
   70     CALL H12 (1,J,J+1,M,A(1,J),1,H(J),A(1,J+1),1,MDA,N-J)
   80     CALL H12 (2,J,J+1,M,A(1,J),1,H(J),B,1,MDB,NB)
C
C     DETERMINE THE PSEUDORANK, K, USING THE TOLERANCE, TAU.
C    ..
          DO 90 J=1,LDIAG
          IF (ABS(A(J,J)).LE.TAU) GO TO 100
   90     CONTINUE
      K=LDIAG
      GO TO 110
  100 K=J-1
  110 KP1=K+1
C
C     COMPUTE THE NORMS OF THE RESIDUAL VECTORS.
C
      IF (NB.LE.0) GO TO 140
          DO 130 JB=1,NB
          TMP=SZERO
          IF (KP1.GT.M) GO TO 130
              DO 120 I=KP1,M
  120         TMP=TMP+B(I,JB)**2
  130     RNORM(JB)=SQRT(TMP)
  140 CONTINUE
C                                           SPECIAL FOR PSEUDORANK = 0
      IF (K.GT.0) GO TO 160
      IF (NB.LE.0) GO TO 270
          DO 150 JB=1,NB
              DO 150 I=1,N
  150         B(I,JB)=SZERO
      GO TO 270
C
C     IF THE PSEUDORANK IS LESS THAN N COMPUTE HOUSEHOLDER
C     DECOMPOSITION OF FIRST K ROWS.
C    ..
  160 IF (K.EQ.N) GO TO 180
          DO 170 II=1,K
          I=KP1-II
  170     CALL H12 (1,I,KP1,N,A(I,1),MDA,G(I),A,MDA,1,I-1)
  180 CONTINUE
C
C
      IF (NB.LE.0) GO TO 270
          DO 260 JB=1,NB
C
C     SOLVE THE K BY K TRIANGULAR SYSTEM.
C    ..
              DO 210 L=1,K
              SM=DZERO
              I=KP1-L
              IF (I.EQ.K) GO TO 200
              IP1=I+1
                  DO 190 J=IP1,K
  190             SM=SM+A(I,J)*DBLE(B(J,JB))
  200         SM1=SM
  210         B(I,JB)=(B(I,JB)-SM1)/A(I,I)
C
C     COMPLETE COMPUTATION OF SOLUTION VECTOR.
C    ..
          IF (K.EQ.N) GO TO 240
              DO 220 J=KP1,N
  220         B(J,JB)=SZERO
              DO 230 I=1,K
  230         CALL H12 (2,I,KP1,N,A(I,1),MDA,G(I),B(1,JB),1,MDB,1)
C
C      RE-ORDER THE SOLUTION VECTOR TO COMPENSATE FOR THE
C      COLUMN INTERCHANGES.
C    ..
  240         DO 250 JJ=1,LDIAG
              J=LDIAG+1-JJ
              IF (IP(J).EQ.J) GO TO 250
              L=IP(J)
              TMP=B(L,JB)
              B(L,JB)=B(J,JB)
              B(J,JB)=TMP
  250         CONTINUE
  260     CONTINUE
C    ..
C     THE SOLUTION VECTORS, X, ARE NOW
C     IN THE FIRST  N  ROWS OF THE ARRAY B(,).
C
  270 KRANK=K
      RETURN
      END
      SUBROUTINE LPDP (A, MDA, M, N1, N2, PRGOPT, X, WNORM, MODE, WS,
     +   IS)
C***BEGIN PROLOGUE  LPDP
C***SUBSIDIARY
C***PURPOSE  Subsidiary to LSEI
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (LPDP-S, DLPDP-D)
C***AUTHOR  Hanson, R. J., (SNLA)
C           Haskell, K. H., (SNLA)
C***DESCRIPTION
C
C     DIMENSION A(MDA,N+1),PRGOPT(*),X(N),WS((M+2)*(N+7)),IS(M+N+1),
C     where N=N1+N2.  This is a slight overestimate for WS(*).
C
C     Determine an N1-vector W, and
C               an N2-vector Z
C     which minimizes the Euclidean length of W
C     subject to G*W+H*Z .GE. Y.
C     This is the least projected distance problem, LPDP.
C     The matrices G and H are of respective
C     dimensions M by N1 and M by N2.
C
C     Called by subprogram LSI( ).
C
C     The matrix
C                (G H Y)
C
C     occupies rows 1,...,M and cols 1,...,N1+N2+1 of A(*,*).
C
C     The solution (W) is returned in X(*).
C                  (Z)
C
C     The value of MODE indicates the status of
C     the computation after returning to the user.
C
C          MODE=1  The solution was successfully obtained.
C
C          MODE=2  The inequalities are inconsistent.
C
C***SEE ALSO  LSEI
C***ROUTINES CALLED  SCOPY, SDOT, SNRM2, SSCAL, WNNLS
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated the AUTHOR section.  (WRB)
C***END PROLOGUE  LPDP
C
C     SUBROUTINES CALLED
C
C     WNNLS         SOLVES A NONNEGATIVELY CONSTRAINED LINEAR LEAST
C                   SQUARES PROBLEM WITH LINEAR EQUALITY CONSTRAINTS.
C                   PART OF THIS PACKAGE.
C
C++
C     SDOT,         SUBROUTINES FROM THE BLAS PACKAGE.
C     SSCAL,SNRM2,  SEE TRANS. MATH. SOFT., VOL. 5, NO. 3, P. 308.
C     SCOPY
C
      REAL             A(MDA,*), PRGOPT(*), WS(*), WNORM, X(*)
      INTEGER IS(*)
      REAL             FAC, ONE, RNORM, SC, YNORM, ZERO
      REAL             SDOT, SNRM2
      SAVE ZERO, ONE, FAC
      DATA ZERO, ONE /0.E0,1.E0/, FAC /0.1E0/
C***FIRST EXECUTABLE STATEMENT  LPDP
      N = N1 + N2
      MODE = 1
      IF (.NOT.(M.LE.0)) GO TO 20
      IF (.NOT.(N.GT.0)) GO TO 10
      X(1) = ZERO
      CALL SCOPY(N, X, 0, X, 1)
   10 WNORM = ZERO
      RETURN
   20 NP1 = N + 1
C
C     SCALE NONZERO ROWS OF INEQUALITY MATRIX TO HAVE LENGTH ONE.
      DO 40 I=1,M
        SC = SNRM2(N,A(I,1),MDA)
        IF (.NOT.(SC.NE.ZERO)) GO TO 30
        SC = ONE/SC
        CALL SSCAL(NP1, SC, A(I,1), MDA)
   30   CONTINUE
   40 CONTINUE
C
C     SCALE RT.-SIDE VECTOR TO HAVE LENGTH ONE (OR ZERO).
      YNORM = SNRM2(M,A(1,NP1),1)
      IF (.NOT.(YNORM.NE.ZERO)) GO TO 50
      SC = ONE/YNORM
      CALL SSCAL(M, SC, A(1,NP1), 1)
C
C     SCALE COLS OF MATRIX H.
   50 J = N1 + 1
   60 IF (.NOT.(J.LE.N)) GO TO 70
      SC = SNRM2(M,A(1,J),1)
      IF (SC.NE.ZERO) SC = ONE/SC
      CALL SSCAL(M, SC, A(1,J), 1)
      X(J) = SC
      J = J + 1
      GO TO 60
   70 IF (.NOT.(N1.GT.0)) GO TO 130
C
C     COPY TRANSPOSE OF (H G Y) TO WORK ARRAY WS(*).
      IW = 0
      DO 80 I=1,M
C
C     MOVE COL OF TRANSPOSE OF H INTO WORK ARRAY.
        CALL SCOPY(N2, A(I,N1+1), MDA, WS(IW+1), 1)
        IW = IW + N2
C
C     MOVE COL OF TRANSPOSE OF G INTO WORK ARRAY.
        CALL SCOPY(N1, A(I,1), MDA, WS(IW+1), 1)
        IW = IW + N1
C
C     MOVE COMPONENT OF VECTOR Y INTO WORK ARRAY.
        WS(IW+1) = A(I,NP1)
        IW = IW + 1
   80 CONTINUE
      WS(IW+1) = ZERO
      CALL SCOPY(N, WS(IW+1), 0, WS(IW+1), 1)
      IW = IW + N
      WS(IW+1) = ONE
      IW = IW + 1
C
C     SOLVE EU=F SUBJECT TO (TRANSPOSE OF H)U=0, U.GE.0.  THE
C     MATRIX E = TRANSPOSE OF (G Y), AND THE (N+1)-VECTOR
C     F = TRANSPOSE OF (0,...,0,1).
      IX = IW + 1
      IW = IW + M
C
C     DO NOT CHECK LENGTHS OF WORK ARRAYS IN THIS USAGE OF WNNLS( ).
      IS(1) = 0
      IS(2) = 0
      CALL WNNLS(WS, NP1, N2, NP1-N2, M, 0, PRGOPT, WS(IX), RNORM,
     1 MODEW, IS, WS(IW+1))
C
C     COMPUTE THE COMPONENTS OF THE SOLN DENOTED ABOVE BY W.
      SC = ONE - SDOT(M,A(1,NP1),1,WS(IX),1)
      IF (.NOT.(ONE+FAC*ABS(SC).NE.ONE .AND. RNORM.GT.ZERO)) GO TO 110
      SC = ONE/SC
      DO 90 J=1,N1
        X(J) = SC*SDOT(M,A(1,J),1,WS(IX),1)
   90 CONTINUE
C
C     COMPUTE THE VECTOR Q=Y-GW.  OVERWRITE Y WITH THIS VECTOR.
      DO 100 I=1,M
        A(I,NP1) = A(I,NP1) - SDOT(N1,A(I,1),MDA,X,1)
  100 CONTINUE
      GO TO 120
  110 MODE = 2
      RETURN
  120 CONTINUE
  130 IF (.NOT.(N2.GT.0)) GO TO 180
C
C     COPY TRANSPOSE OF (H Q) TO WORK ARRAY WS(*).
      IW = 0
      DO 140 I=1,M
        CALL SCOPY(N2, A(I,N1+1), MDA, WS(IW+1), 1)
        IW = IW + N2
        WS(IW+1) = A(I,NP1)
        IW = IW + 1
  140 CONTINUE
      WS(IW+1) = ZERO
      CALL SCOPY(N2, WS(IW+1), 0, WS(IW+1), 1)
      IW = IW + N2
      WS(IW+1) = ONE
      IW = IW + 1
      IX = IW + 1
      IW = IW + M
C
C     SOLVE RV=S SUBJECT TO V.GE.0.  THE MATRIX R =(TRANSPOSE
C     OF (H Q)), WHERE Q=Y-GW.  THE (N2+1)-VECTOR S =(TRANSPOSE
C     OF (0,...,0,1)).
C
C     DO NOT CHECK LENGTHS OF WORK ARRAYS IN THIS USAGE OF WNNLS( ).
      IS(1) = 0
      IS(2) = 0
      CALL WNNLS(WS, N2+1, 0, N2+1, M, 0, PRGOPT, WS(IX), RNORM, MODEW,
     1 IS, WS(IW+1))
C
C     COMPUTE THE COMPONENTS OF THE SOLN DENOTED ABOVE BY Z.
      SC = ONE - SDOT(M,A(1,NP1),1,WS(IX),1)
      IF (.NOT.(ONE+FAC*ABS(SC).NE.ONE .AND. RNORM.GT.ZERO)) GO TO 160
      SC = ONE/SC
      DO 150 J=1,N2
        L = N1 + J
        X(L) = SC*SDOT(M,A(1,L),1,WS(IX),1)*X(L)
  150 CONTINUE
      GO TO 170
  160 MODE = 2
      RETURN
  170 CONTINUE
C
C     ACCOUNT FOR SCALING OF RT.-SIDE VECTOR IN SOLUTION.
  180 CALL SSCAL(N, YNORM, X, 1)
      WNORM = SNRM2(N1,X,1)
      RETURN
      END
      SUBROUTINE LSEI (W, MDW, ME, MA, MG, N, PRGOPT, X, RNORME, RNORML,
     +   MODE, WS, IP)
C***BEGIN PROLOGUE  LSEI
C***PURPOSE  Solve a linearly constrained least squares problem with
C            equality and inequality constraints, and optionally compute
C            a covariance matrix.
C***LIBRARY   SLATEC
C***CATEGORY  K1A2A, D9
C***TYPE      SINGLE PRECISION (LSEI-S, DLSEI-D)
C***KEYWORDS  CONSTRAINED LEAST SQUARES, CURVE FITTING, DATA FITTING,
C             EQUALITY CONSTRAINTS, INEQUALITY CONSTRAINTS,
C             QUADRATIC PROGRAMMING
C***AUTHOR  Hanson, R. J., (SNLA)
C           Haskell, K. H., (SNLA)
C***DESCRIPTION
C
C     Abstract
C
C     This subprogram solves a linearly constrained least squares
C     problem with both equality and inequality constraints, and, if the
C     user requests, obtains a covariance matrix of the solution
C     parameters.
C
C     Suppose there are given matrices E, A and G of respective
C     dimensions ME by N, MA by N and MG by N, and vectors F, B and H of
C     respective lengths ME, MA and MG.  This subroutine solves the
C     linearly constrained least squares problem
C
C                   EX = F, (E ME by N) (equations to be exactly
C                                       satisfied)
C                   AX = B, (A MA by N) (equations to be
C                                       approximately satisfied,
C                                       least squares sense)
C                   GX .GE. H,(G MG by N) (inequality constraints)
C
C     The inequalities GX .GE. H mean that every component of the
C     product GX must be .GE. the corresponding component of H.
C
C     In case the equality constraints cannot be satisfied, a
C     generalized inverse solution residual vector length is obtained
C     for F-EX.  This is the minimal length possible for F-EX.
C
C     Any values ME .GE. 0, MA .GE. 0, or MG .GE. 0 are permitted.  The
C     rank of the matrix E is estimated during the computation.  We call
C     this value KRANKE.  It is an output parameter in IP(1) defined
C     below.  Using a generalized inverse solution of EX=F, a reduced
C     least squares problem with inequality constraints is obtained.
C     The tolerances used in these tests for determining the rank
C     of E and the rank of the reduced least squares problem are
C     given in Sandia Tech. Rept. SAND-78-1290.  They can be
C     modified by the user if new values are provided in
C     the option list of the array PRGOPT(*).
C
C     The user must dimension all arrays appearing in the call list..
C     W(MDW,N+1),PRGOPT(*),X(N),WS(2*(ME+N)+K+(MG+2)*(N+7)),IP(MG+2*N+2)
C     where K=MAX(MA+MG,N).  This allows for a solution of a range of
C     problems in the given working space.  The dimension of WS(*)
C     given is a necessary overestimate.  Once a particular problem
C     has been run, the output parameter IP(3) gives the actual
C     dimension required for that problem.
C
C     The parameters for LSEI( ) are
C
C     Input..
C
C     W(*,*),MDW,   The array W(*,*) is doubly subscripted with
C     ME,MA,MG,N    first dimensioning parameter equal to MDW.
C                   For this discussion let us call M = ME+MA+MG.  Then
C                   MDW must satisfy MDW .GE. M.  The condition
C                   MDW .LT. M is an error.
C
C                   The array W(*,*) contains the matrices and vectors
C
C                                  (E  F)
C                                  (A  B)
C                                  (G  H)
C
C                   in rows and columns 1,...,M and 1,...,N+1
C                   respectively.
C
C                   The integers ME, MA, and MG are the
C                   respective matrix row dimensions
C                   of E, A and G.  Each matrix has N columns.
C
C     PRGOPT(*)    This real-valued array is the option vector.
C                  If the user is satisfied with the nominal
C                  subprogram features set
C
C                  PRGOPT(1)=1 (or PRGOPT(1)=1.0)
C
C                  Otherwise PRGOPT(*) is a linked list consisting of
C                  groups of data of the following form
C
C                  LINK
C                  KEY
C                  DATA SET
C
C                  The parameters LINK and KEY are each one word.
C                  The DATA SET can be comprised of several words.
C                  The number of items depends on the value of KEY.
C                  The value of LINK points to the first
C                  entry of the next group of data within
C                  PRGOPT(*).  The exception is when there are
C                  no more options to change.  In that
C                  case, LINK=1 and the values KEY and DATA SET
C                  are not referenced.  The general layout of
C                  PRGOPT(*) is as follows.
C
C               ...PRGOPT(1) = LINK1 (link to first entry of next group)
C               .  PRGOPT(2) = KEY1 (key to the option change)
C               .  PRGOPT(3) = data value (data value for this change)
C               .       .
C               .       .
C               .       .
C               ...PRGOPT(LINK1)   = LINK2 (link to the first entry of
C               .                       next group)
C               .  PRGOPT(LINK1+1) = KEY2 (key to the option change)
C               .  PRGOPT(LINK1+2) = data value
C               ...     .
C               .       .
C               .       .
C               ...PRGOPT(LINK) = 1 (no more options to change)
C
C                  Values of LINK that are nonpositive are errors.
C                  A value of LINK .GT. NLINK=100000 is also an error.
C                  This helps prevent using invalid but positive
C                  values of LINK that will probably extend
C                  beyond the program limits of PRGOPT(*).
C                  Unrecognized values of KEY are ignored.  The
C                  order of the options is arbitrary and any number
C                  of options can be changed with the following
C                  restriction.  To prevent cycling in the
C                  processing of the option array, a count of the
C                  number of options changed is maintained.
C                  Whenever this count exceeds NOPT=1000, an error
C                  message is printed and the subprogram returns.
C
C                  Options..
C
C                  KEY=1
C                         Compute in W(*,*) the N by N
C                  covariance matrix of the solution variables
C                  as an output parameter.  Nominally the
C                  covariance matrix will not be computed.
C                  (This requires no user input.)
C                  The data set for this option is a single value.
C                  It must be nonzero when the covariance matrix
C                  is desired.  If it is zero, the covariance
C                  matrix is not computed.  When the covariance matrix
C                  is computed, the first dimensioning parameter
C                  of the array W(*,*) must satisfy MDW .GE. MAX(M,N).
C
C                  KEY=10
C                         Suppress scaling of the inverse of the
C                  normal matrix by the scale factor RNORM**2/
C                  MAX(1, no. of degrees of freedom).  This option
C                  only applies when the option for computing the
C                  covariance matrix (KEY=1) is used.  With KEY=1 and
C                  KEY=10 used as options the unscaled inverse of the
C                  normal matrix is returned in W(*,*).
C                  The data set for this option is a single value.
C                  When it is nonzero no scaling is done.  When it is
C                  zero scaling is done.  The nominal case is to do
C                  scaling so if option (KEY=1) is used alone, the
C                  matrix will be scaled on output.
C
C                  KEY=2
C                         Scale the nonzero columns of the
C                         entire data matrix.
C                  (E)
C                  (A)
C                  (G)
C
C                  to have length one.  The data set for this
C                  option is a single value.  It must be
C                  nonzero if unit length column scaling
C                  is desired.
C
C                  KEY=3
C                         Scale columns of the entire data matrix
C                  (E)
C                  (A)
C                  (G)
C
C                  with a user-provided diagonal matrix.
C                  The data set for this option consists
C                  of the N diagonal scaling factors, one for
C                  each matrix column.
C
C                  KEY=4
C                         Change the rank determination tolerance for
C                  the equality constraint equations from
C                  the nominal value of SQRT(SRELPR).  This quantity can
C                  be no smaller than SRELPR, the arithmetic-
C                  storage precision.  The quantity SRELPR is the
C                  largest positive number such that T=1.+SRELPR
C                  satisfies T .EQ. 1.  The quantity used
C                  here is internally restricted to be at
C                  least SRELPR.  The data set for this option
C                  is the new tolerance.
C
C                  KEY=5
C                         Change the rank determination tolerance for
C                  the reduced least squares equations from
C                  the nominal value of SQRT(SRELPR).  This quantity can
C                  be no smaller than SRELPR, the arithmetic-
C                  storage precision.  The quantity used
C                  here is internally restricted to be at
C                  least SRELPR.  The data set for this option
C                  is the new tolerance.
C
C                  For example, suppose we want to change
C                  the tolerance for the reduced least squares
C                  problem, compute the covariance matrix of
C                  the solution parameters, and provide
C                  column scaling for the data matrix.  For
C                  these options the dimension of PRGOPT(*)
C                  must be at least N+9.  The Fortran statements
C                  defining these options would be as follows:
C
C                  PRGOPT(1)=4 (link to entry 4 in PRGOPT(*))
C                  PRGOPT(2)=1 (covariance matrix key)
C                  PRGOPT(3)=1 (covariance matrix wanted)
C
C                  PRGOPT(4)=7 (link to entry 7 in PRGOPT(*))
C                  PRGOPT(5)=5 (least squares equas.  tolerance key)
C                  PRGOPT(6)=... (new value of the tolerance)
C
C                  PRGOPT(7)=N+9 (link to entry N+9 in PRGOPT(*))
C                  PRGOPT(8)=3 (user-provided column scaling key)
C
C                  CALL SCOPY (N, D, 1, PRGOPT(9), 1)  (Copy the N
C                    scaling factors from the user array D(*)
C                    to PRGOPT(9)-PRGOPT(N+8))
C
C                  PRGOPT(N+9)=1 (no more options to change)
C
C                  The contents of PRGOPT(*) are not modified
C                  by the subprogram.
C                  The options for WNNLS( ) can also be included
C                  in this array.  The values of KEY recognized
C                  by WNNLS( ) are 6, 7 and 8.  Their functions
C                  are documented in the usage instructions for
C                  subroutine WNNLS( ).  Normally these options
C                  do not need to be modified when using LSEI( ).
C
C     IP(1),       The amounts of working storage actually
C     IP(2)        allocated for the working arrays WS(*) and
C                  IP(*), respectively.  These quantities are
C                  compared with the actual amounts of storage
C                  needed by LSEI( ).  Insufficient storage
C                  allocated for either WS(*) or IP(*) is an
C                  error.  This feature was included in LSEI( )
C                  because miscalculating the storage formulas
C                  for WS(*) and IP(*) might very well lead to
C                  subtle and hard-to-find execution errors.
C
C                  The length of WS(*) must be at least
C
C                  LW = 2*(ME+N)+K+(MG+2)*(N+7)
C
C                  where K = max(MA+MG,N)
C                  This test will not be made if IP(1).LE.0.
C
C                  The length of IP(*) must be at least
C
C                  LIP = MG+2*N+2
C                  This test will not be made if IP(2).LE.0.
C
C     Output..
C
C     X(*),RNORME,  The array X(*) contains the solution parameters
C     RNORML        if the integer output flag MODE = 0 or 1.
C                   The definition of MODE is given directly below.
C                   When MODE = 0 or 1, RNORME and RNORML
C                   respectively contain the residual vector
C                   Euclidean lengths of F - EX and B - AX.  When
C                   MODE=1 the equality constraint equations EX=F
C                   are contradictory, so RNORME .NE. 0.  The residual
C                   vector F-EX has minimal Euclidean length.  For
C                   MODE .GE. 2, none of these parameters is defined.
C
C     MODE          Integer flag that indicates the subprogram
C                   status after completion.  If MODE .GE. 2, no
C                   solution has been computed.
C
C                   MODE =
C
C                   0  Both equality and inequality constraints
C                      are compatible and have been satisfied.
C
C                   1  Equality constraints are contradictory.
C                      A generalized inverse solution of EX=F was used
C                      to minimize the residual vector length F-EX.
C                      In this sense, the solution is still meaningful.
C
C                   2  Inequality constraints are contradictory.
C
C                   3  Both equality and inequality constraints
C                      are contradictory.
C
C                   The following interpretation of
C                   MODE=1,2 or 3 must be made.  The
C                   sets consisting of all solutions
C                   of the equality constraints EX=F
C                   and all vectors satisfying GX .GE. H
C                   have no points in common.  (In
C                   particular this does not say that
C                   each individual set has no points
C                   at all, although this could be the
C                   case.)
C
C                   4  Usage error occurred.  The value
C                      of MDW is .LT. ME+MA+MG, MDW is
C                      .LT. N and a covariance matrix is
C                      requested, or the option vector
C                      PRGOPT(*) is not properly defined,
C                      or the lengths of the working arrays
C                      WS(*) and IP(*), when specified in
C                      IP(1) and IP(2) respectively, are not
C                      long enough.
C
C     W(*,*)        The array W(*,*) contains the N by N symmetric
C                   covariance matrix of the solution parameters,
C                   provided this was requested on input with
C                   the option vector PRGOPT(*) and the output
C                   flag is returned with MODE = 0 or 1.
C
C     IP(*)         The integer working array has three entries
C                   that provide rank and working array length
C                   information after completion.
C
C                      IP(1) = rank of equality constraint
C                              matrix.  Define this quantity
C                              as KRANKE.
C
C                      IP(2) = rank of reduced least squares
C                              problem.
C
C                      IP(3) = the amount of storage in the
C                              working array WS(*) that was
C                              actually used by the subprogram.
C                              The formula given above for the length
C                              of WS(*) is a necessary overestimate.
C                              If exactly the same problem matrices
C                              are used in subsequent executions,
C                              the declared dimension of WS(*) can
C                              be reduced to this output value.
C     User Designated
C     Working Arrays..
C
C     WS(*),IP(*)              These are respectively type real
C                              and type integer working arrays.
C                              Their required minimal lengths are
C                              given above.
C
C***REFERENCES  K. H. Haskell and R. J. Hanson, An algorithm for
C                 linear least squares problems with equality and
C                 nonnegativity constraints, Report SAND77-0552, Sandia
C                 Laboratories, June 1978.
C               K. H. Haskell and R. J. Hanson, Selected algorithms for
C                 the linearly constrained least squares problem - a
C                 users guide, Report SAND78-1290, Sandia Laboratories,
C                 August 1979.
C               K. H. Haskell and R. J. Hanson, An algorithm for
C                 linear least squares problems with equality and
C                 nonnegativity constraints, Mathematical Programming
C                 21 (1981), pp. 98-118.
C               R. J. Hanson and K. H. Haskell, Two algorithms for the
C                 linearly constrained least squares problem, ACM
C                 Transactions on Mathematical Software, September 1982.
C***ROUTINES CALLED  H12, LSI, R1MACH, SASUM, SAXPY, SCOPY, SDOT, SNRM2,
C                    SSCAL, SSWAP, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890618  Completely restructured and extensively revised (WRB & RWC)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900510  Convert XERRWV calls to XERMSG calls.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  LSEI
      INTEGER IP(3), MA, MDW, ME, MG, MODE, N
      REAL             PRGOPT(*), RNORME, RNORML, W(MDW,*), WS(*), X(*)
C
      EXTERNAL H12, LSI, R1MACH, SASUM, SAXPY, SCOPY, SDOT, SNRM2,
     *   SSCAL, SSWAP, XERMSG
      REAL             R1MACH, SASUM, SDOT, SNRM2
C
      REAL             ENORM, FNORM, GAM, RB, RN, RNMAX, SIZE, SN,
     *   SNMAX, SRELPR, T, TAU, UJ, UP, VJ, XNORM, XNRME
      INTEGER I, IMAX, J, JP1, K, KEY, KRANKE, LAST, LCHK, LINK, M,
     *   MAPKE1, MDEQC, MEND, MEP1, N1, N2, NEXT, NLINK, NOPT, NP1,
     *   NTIMES
      LOGICAL COV, FIRST
      CHARACTER*8 XERN1, XERN2, XERN3, XERN4
      SAVE FIRST, SRELPR
C
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  LSEI
C
C     Set the nominal tolerance used in the code for the equality
C     constraint equations.
C
      IF (FIRST) SRELPR = R1MACH(4)
      FIRST = .FALSE.
      TAU = SQRT(SRELPR)
C
C     Check that enough storage was allocated in WS(*) and IP(*).
C
      MODE = 4
      IF (MIN(N,ME,MA,MG) .LT. 0) THEN
         WRITE (XERN1, '(I8)') N
         WRITE (XERN2, '(I8)') ME
         WRITE (XERN3, '(I8)') MA
         WRITE (XERN4, '(I8)') MG
         CALL XERMSG ('SLATEC', 'LSEI', 'ALL OF THE VARIABLES N, ME,' //
     *      ' MA, MG MUST BE .GE. 0$$ENTERED ROUTINE WITH' //
     *      '$$N  = ' // XERN1 //
     *      '$$ME = ' // XERN2 //
     *      '$$MA = ' // XERN3 //
     *      '$$MG = ' // XERN4, 2, 1)
         RETURN
      ENDIF
C
      IF (IP(1).GT.0) THEN
         LCHK = 2*(ME+N) + MAX(MA+MG,N) + (MG+2)*(N+7)
         IF (IP(1).LT.LCHK) THEN
            WRITE (XERN1, '(I8)') LCHK
            CALL XERMSG ('SLATEC', 'LSEI', 'INSUFFICIENT STORAGE ' //
     *         'ALLOCATED FOR WS(*), NEED LW = ' // XERN1, 2, 1)
            RETURN
         ENDIF
      ENDIF
C
      IF (IP(2).GT.0) THEN
         LCHK = MG + 2*N + 2
         IF (IP(2).LT.LCHK) THEN
            WRITE (XERN1, '(I8)') LCHK
            CALL XERMSG ('SLATEC', 'LSEI', 'INSUFFICIENT STORAGE ' //
     *         'ALLOCATED FOR IP(*), NEED LIP = ' // XERN1, 2, 1)
            RETURN
         ENDIF
      ENDIF
C
C     Compute number of possible right multiplying Householder
C     transformations.
C
      M = ME + MA + MG
      IF (N.LE.0 .OR. M.LE.0) THEN
         MODE = 0
         RNORME = 0
         RNORML = 0
         RETURN
      ENDIF
C
      IF (MDW.LT.M) THEN
         CALL XERMSG ('SLATEC', 'LSEI', 'MDW.LT.ME+MA+MG IS AN ERROR',
     +      2, 1)
         RETURN
      ENDIF
C
      NP1 = N + 1
      KRANKE = MIN(ME,N)
      N1 = 2*KRANKE + 1
      N2 = N1 + N
C
C     Set nominal values.
C
C     The nominal column scaling used in the code is
C     the identity scaling.
C
      CALL SCOPY (N, 1.E0, 0, WS(N1), 1)
C
C     No covariance matrix is nominally computed.
C
      COV = .FALSE.
C
C     Process option vector.
C     Define bound for number of options to change.
C
      NOPT = 1000
      NTIMES = 0
C
C     Define bound for positive values of LINK.
C
      NLINK = 100000
      LAST = 1
      LINK = PRGOPT(1)
      IF (LINK.EQ.0 .OR. LINK.GT.NLINK) THEN
         CALL XERMSG ('SLATEC', 'LSEI',
     +      'THE OPTION VECTOR IS UNDEFINED', 2, 1)
         RETURN
      ENDIF
C
  100 IF (LINK.GT.1) THEN
         NTIMES = NTIMES + 1
         IF (NTIMES.GT.NOPT) THEN
            CALL XERMSG ('SLATEC', 'LSEI',
     +         'THE LINKS IN THE OPTION VECTOR ARE CYCLING.', 2, 1)
            RETURN
         ENDIF
C
         KEY = PRGOPT(LAST+1)
         IF (KEY.EQ.1) THEN
            COV = PRGOPT(LAST+2) .NE. 0.E0
         ELSEIF (KEY.EQ.2 .AND. PRGOPT(LAST+2).NE.0.E0) THEN
            DO 110 J = 1,N
               T = SNRM2(M,W(1,J),1)
               IF (T.NE.0.E0) T = 1.E0/T
               WS(J+N1-1) = T
  110       CONTINUE
         ELSEIF (KEY.EQ.3) THEN
            CALL SCOPY (N, PRGOPT(LAST+2), 1, WS(N1), 1)
         ELSEIF (KEY.EQ.4) THEN
            TAU = MAX(SRELPR,PRGOPT(LAST+2))
         ENDIF
C
         NEXT = PRGOPT(LINK)
         IF (NEXT.LE.0 .OR. NEXT.GT.NLINK) THEN
         CALL XERMSG ('SLATEC', 'LSEI',
     +      'THE OPTION VECTOR IS UNDEFINED', 2, 1)
            RETURN
         ENDIF
C
         LAST = LINK
         LINK = NEXT
         GO TO 100
      ENDIF
C
      DO 120 J = 1,N
         CALL SSCAL (M, WS(N1+J-1), W(1,J), 1)
  120 CONTINUE
C
      IF (COV .AND. MDW.LT.N) THEN
         CALL XERMSG ('SLATEC', 'LSEI',
     +      'MDW .LT. N WHEN COV MATRIX NEEDED, IS AN ERROR', 2, 1)
         RETURN
      ENDIF
C
C     Problem definition and option vector OK.
C
      MODE = 0
C
C     Compute norm of equality constraint matrix and right side.
C
      ENORM = 0.E0
      DO 130 J = 1,N
         ENORM = MAX(ENORM,SASUM(ME,W(1,J),1))
  130 CONTINUE
C
      FNORM = SASUM(ME,W(1,NP1),1)
      SNMAX = 0.E0
      RNMAX = 0.E0
      DO 150 I = 1,KRANKE
C
C        Compute maximum ratio of vector lengths. Partition is at
C        column I.
C
         DO 140 K = I,ME
            SN = SDOT(N-I+1,W(K,I),MDW,W(K,I),MDW)
            RN = SDOT(I-1,W(K,1),MDW,W(K,1),MDW)
            IF (RN.EQ.0.E0 .AND. SN.GT.SNMAX) THEN
               SNMAX = SN
               IMAX = K
            ELSEIF (K.EQ.I .OR. SN*RNMAX.GT.RN*SNMAX) THEN
               SNMAX = SN
               RNMAX = RN
               IMAX = K
            ENDIF
  140    CONTINUE
C
C        Interchange rows if necessary.
C
         IF (I.NE.IMAX) CALL SSWAP (NP1, W(I,1), MDW, W(IMAX,1), MDW)
         IF (SNMAX.GT.RNMAX*TAU**2) THEN
C
C        Eliminate elements I+1,...,N in row I.
C
            CALL H12 (1, I, I+1, N, W(I,1), MDW, WS(I), W(I+1,1), MDW,
     +                1, M-I)
         ELSE
            KRANKE = I - 1
            GO TO 160
         ENDIF
  150 CONTINUE
C
C     Save diagonal terms of lower trapezoidal matrix.
C
  160 CALL SCOPY (KRANKE, W, MDW+1, WS(KRANKE+1), 1)
C
C     Use Householder transformation from left to achieve
C     KRANKE by KRANKE upper triangular form.
C
      IF (KRANKE.LT.ME) THEN
         DO 170 K = KRANKE,1,-1
C
C           Apply transformation to matrix cols. 1,...,K-1.
C
            CALL H12 (1, K, KRANKE+1, ME, W(1,K), 1, UP, W, 1, MDW, K-1)
C
C           Apply to rt side vector.
C
            CALL H12 (2, K, KRANKE+1, ME, W(1,K), 1, UP, W(1,NP1), 1, 1,
     +                1)
  170    CONTINUE
      ENDIF
C
C     Solve for variables 1,...,KRANKE in new coordinates.
C
      CALL SCOPY (KRANKE, W(1, NP1), 1, X, 1)
      DO 180 I = 1,KRANKE
         X(I) = (X(I)-SDOT(I-1,W(I,1),MDW,X,1))/W(I,I)
  180 CONTINUE
C
C     Compute residuals for reduced problem.
C
      MEP1 = ME + 1
      RNORML = 0.E0
      DO 190 I = MEP1,M
         W(I,NP1) = W(I,NP1) - SDOT(KRANKE,W(I,1),MDW,X,1)
         SN = SDOT(KRANKE,W(I,1),MDW,W(I,1),MDW)
         RN = SDOT(N-KRANKE,W(I,KRANKE+1),MDW,W(I,KRANKE+1),MDW)
         IF (RN.LE.SN*TAU**2 .AND. KRANKE.LT.N)
     *      CALL SCOPY (N-KRANKE, 0.E0, 0, W(I,KRANKE+1), MDW)
  190 CONTINUE
C
C     Compute equality constraint equations residual length.
C
      RNORME = SNRM2(ME-KRANKE,W(KRANKE+1,NP1),1)
C
C     Move reduced problem data upward if KRANKE.LT.ME.
C
      IF (KRANKE.LT.ME) THEN
         DO 200 J = 1,NP1
            CALL SCOPY (M-ME, W(ME+1,J), 1, W(KRANKE+1,J), 1)
  200    CONTINUE
      ENDIF
C
C     Compute solution of reduced problem.
C
      CALL LSI(W(KRANKE+1, KRANKE+1), MDW, MA, MG, N-KRANKE, PRGOPT,
     +         X(KRANKE+1), RNORML, MODE, WS(N2), IP(2))
C
C     Test for consistency of equality constraints.
C
      IF (ME.GT.0) THEN
         MDEQC = 0
         XNRME = SASUM(KRANKE,W(1,NP1),1)
         IF (RNORME.GT.TAU*(ENORM*XNRME+FNORM)) MDEQC = 1
         MODE = MODE + MDEQC
C
C        Check if solution to equality constraints satisfies inequality
C        constraints when there are no degrees of freedom left.
C
         IF (KRANKE.EQ.N .AND. MG.GT.0) THEN
            XNORM = SASUM(N,X,1)
            MAPKE1 = MA + KRANKE + 1
            MEND = MA + KRANKE + MG
            DO 210 I = MAPKE1,MEND
               SIZE = SASUM(N,W(I,1),MDW)*XNORM + ABS(W(I,NP1))
               IF (W(I,NP1).GT.TAU*SIZE) THEN
                  MODE = MODE + 2
                  GO TO 290
               ENDIF
  210       CONTINUE
         ENDIF
      ENDIF
C
C     Replace diagonal terms of lower trapezoidal matrix.
C
      IF (KRANKE.GT.0) THEN
         CALL SCOPY (KRANKE, WS(KRANKE+1), 1, W, MDW+1)
C
C        Reapply transformation to put solution in original coordinates.
C
         DO 220 I = KRANKE,1,-1
            CALL H12 (2, I, I+1, N, W(I,1), MDW, WS(I), X, 1, 1, 1)
  220    CONTINUE
C
C        Compute covariance matrix of equality constrained problem.
C
         IF (COV) THEN
            DO 270 J = MIN(KRANKE,N-1),1,-1
               RB = WS(J)*W(J,J)
               IF (RB.NE.0.E0) RB = 1.E0/RB
               JP1 = J + 1
               DO 230 I = JP1,N
                  W(I,J) = RB*SDOT(N-J,W(I,JP1),MDW,W(J,JP1),MDW)
  230          CONTINUE
C
               GAM = 0.5E0*RB*SDOT(N-J,W(JP1,J),1,W(J,JP1),MDW)
               CALL SAXPY (N-J, GAM, W(J,JP1), MDW, W(JP1,J), 1)
               DO 250 I = JP1,N
                  DO 240 K = I,N
                     W(I,K) = W(I,K) + W(J,I)*W(K,J) + W(I,J)*W(J,K)
                     W(K,I) = W(I,K)
  240             CONTINUE
  250          CONTINUE
               UJ = WS(J)
               VJ = GAM*UJ
               W(J,J) = UJ*VJ + UJ*VJ
               DO 260 I = JP1,N
                  W(J,I) = UJ*W(I,J) + VJ*W(J,I)
  260          CONTINUE
               CALL SCOPY (N-J, W(J, JP1), MDW, W(JP1,J), 1)
  270       CONTINUE
         ENDIF
      ENDIF
C
C     Apply the scaling to the covariance matrix.
C
      IF (COV) THEN
         DO 280 I = 1,N
            CALL SSCAL (N, WS(I+N1-1), W(I,1), MDW)
            CALL SSCAL (N, WS(I+N1-1), W(1,I), 1)
  280    CONTINUE
      ENDIF
C
C     Rescale solution vector.
C
  290 IF (MODE.LE.1) THEN
         DO 300 J = 1,N
            X(J) = X(J)*WS(N1+J-1)
  300    CONTINUE
      ENDIF
C
      IP(1) = KRANKE
      IP(3) = IP(3) + 2*KRANKE + N
      RETURN
      END
      SUBROUTINE LSI (W, MDW, MA, MG, N, PRGOPT, X, RNORM, MODE, WS, IP)
C***BEGIN PROLOGUE  LSI
C***SUBSIDIARY
C***PURPOSE  Subsidiary to LSEI
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (LSI-S, DLSI-D)
C***AUTHOR  Hanson, R. J., (SNLA)
C***DESCRIPTION
C
C     This is a companion subprogram to LSEI.  The documentation for
C     LSEI has complete usage instructions.
C
C     Solve..
C              AX = B,  A  MA by N  (least squares equations)
C     subject to..
C
C              GX.GE.H, G  MG by N  (inequality constraints)
C
C     Input..
C
C      W(*,*) contains  (A B) in rows 1,...,MA+MG, cols 1,...,N+1.
C                       (G H)
C
C     MDW,MA,MG,N
C              contain (resp) var. dimension of W(*,*),
C              and matrix dimensions.
C
C     PRGOPT(*),
C              Program option vector.
C
C     OUTPUT..
C
C      X(*),RNORM
C
C              Solution vector(unless MODE=2), length of AX-B.
C
C      MODE
C              =0   Inequality constraints are compatible.
C              =2   Inequality constraints contradictory.
C
C      WS(*),
C              Working storage of dimension K+N+(MG+2)*(N+7),
C              where K=MAX(MA+MG,N).
C      IP(MG+2*N+1)
C              Integer working storage
C
C***ROUTINES CALLED  H12, HFTI, LPDP, R1MACH, SASUM, SAXPY, SCOPY, SDOT,
C                    SSCAL, SSWAP
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890618  Completely restructured and extensively revised (WRB & RWC)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   920422  Changed CALL to HFTI to include variable MA.  (WRB)
C***END PROLOGUE  LSI
      INTEGER IP(*), MA, MDW, MG, MODE, N
      REAL             PRGOPT(*), RNORM, W(MDW,*), WS(*), X(*)
C
      EXTERNAL H12, HFTI, LPDP, R1MACH, SASUM, SAXPY, SCOPY, SDOT,
     *   SSCAL, SSWAP
      REAL             R1MACH, SASUM, SDOT
C
      REAL             ANORM, FAC, GAM, RB, SRELPR, TAU, TOL, XNORM
      INTEGER I, J, K, KEY, KRANK, KRM1, KRP1, L, LAST, LINK, M, MAP1,
     *   MDLPDP, MINMAN, N1, N2, N3, NEXT, NP1
      LOGICAL COV, FIRST, SCLCOV
C
      SAVE SRELPR, FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  LSI
C
C     Set the nominal tolerance used in the code.
C
      IF (FIRST) SRELPR = R1MACH(4)
      FIRST = .FALSE.
      TOL = SQRT(SRELPR)
C
      MODE = 0
      RNORM = 0.E0
      M = MA + MG
      NP1 = N + 1
      KRANK = 0
      IF (N.LE.0 .OR. M.LE.0) GO TO 370
C
C     To process option vector.
C
      COV = .FALSE.
      SCLCOV = .TRUE.
      LAST = 1
      LINK = PRGOPT(1)
C
  100 IF (LINK.GT.1) THEN
         KEY = PRGOPT(LAST+1)
         IF (KEY.EQ.1) COV = PRGOPT(LAST+2) .NE. 0.E0
         IF (KEY.EQ.10) SCLCOV = PRGOPT(LAST+2) .EQ. 0.E0
         IF (KEY.EQ.5) TOL = MAX(SRELPR,PRGOPT(LAST+2))
         NEXT = PRGOPT(LINK)
         LAST = LINK
         LINK = NEXT
         GO TO 100
      ENDIF
C
C     Compute matrix norm of least squares equations.
C
      ANORM = 0.E0
      DO 110 J = 1,N
         ANORM = MAX(ANORM,SASUM(MA,W(1,J),1))
  110 CONTINUE
C
C     Set tolerance for HFTI( ) rank test.
C
      TAU = TOL*ANORM
C
C     Compute Householder orthogonal decomposition of matrix.
C
      CALL SCOPY (N, 0.E0, 0, WS, 1)
      CALL SCOPY (MA, W(1, NP1), 1, WS, 1)
      K = MAX(M,N)
      MINMAN = MIN(MA,N)
      N1 = K + 1
      N2 = N1 + N
      CALL HFTI (W, MDW, MA, N, WS, MA, 1, TAU, KRANK, RNORM, WS(N2),
     +           WS(N1), IP)
      FAC = 1.E0
      GAM = MA - KRANK
      IF (KRANK.LT.MA .AND. SCLCOV) FAC = RNORM**2/GAM
C
C     Reduce to LPDP and solve.
C
      MAP1 = MA + 1
C
C     Compute inequality rt-hand side for LPDP.
C
      IF (MA.LT.M) THEN
         IF (MINMAN.GT.0) THEN
            DO 120 I = MAP1,M
               W(I,NP1) = W(I,NP1) - SDOT(N,W(I,1),MDW,WS,1)
  120       CONTINUE
C
C           Apply permutations to col. of inequality constraint matrix.
C
            DO 130 I = 1,MINMAN
               CALL SSWAP (MG, W(MAP1,I), 1, W(MAP1,IP(I)), 1)
  130       CONTINUE
C
C           Apply Householder transformations to constraint matrix.
C
            IF (KRANK.GT.0 .AND. KRANK.LT.N) THEN
               DO 140 I = KRANK,1,-1
                  CALL H12 (2, I, KRANK+1, N, W(I,1), MDW, WS(N1+I-1),
     +                      W(MAP1,1), MDW, 1, MG)
  140          CONTINUE
            ENDIF
C
C           Compute permuted inequality constraint matrix times r-inv.
C
            DO 160 I = MAP1,M
               DO 150 J = 1,KRANK
                  W(I,J) = (W(I,J)-SDOT(J-1,W(1,J),1,W(I,1),MDW))/W(J,J)
  150          CONTINUE
  160       CONTINUE
         ENDIF
C
C        Solve the reduced problem with LPDP algorithm,
C        the least projected distance problem.
C
         CALL LPDP(W(MAP1,1), MDW, MG, KRANK, N-KRANK, PRGOPT, X,
     +             XNORM, MDLPDP, WS(N2), IP(N+1))
C
C        Compute solution in original coordinates.
C
         IF (MDLPDP.EQ.1) THEN
            DO 170 I = KRANK,1,-1
               X(I) = (X(I)-SDOT(KRANK-I,W(I,I+1),MDW,X(I+1),1))/W(I,I)
  170       CONTINUE
C
C           Apply Householder transformation to solution vector.
C
            IF (KRANK.LT.N) THEN
               DO 180 I = 1,KRANK
                  CALL H12 (2, I, KRANK+1, N, W(I,1), MDW, WS(N1+I-1),
     +                      X, 1, 1, 1)
  180          CONTINUE
            ENDIF
C
C           Repermute variables to their input order.
C
            IF (MINMAN.GT.0) THEN
               DO 190 I = MINMAN,1,-1
                  CALL SSWAP (1, X(I), 1, X(IP(I)), 1)
  190          CONTINUE
C
C              Variables are now in original coordinates.
C              Add solution of unconstrained problem.
C
               DO 200 I = 1,N
                  X(I) = X(I) + WS(I)
  200          CONTINUE
C
C              Compute the residual vector norm.
C
               RNORM = SQRT(RNORM**2+XNORM**2)
            ENDIF
         ELSE
            MODE = 2
         ENDIF
      ELSE
         CALL SCOPY (N, WS, 1, X, 1)
      ENDIF
C
C     Compute covariance matrix based on the orthogonal decomposition
C     from HFTI( ).
C
      IF (.NOT.COV .OR. KRANK.LE.0) GO TO 370
      KRM1 = KRANK - 1
      KRP1 = KRANK + 1
C
C     Copy diagonal terms to working array.
C
      CALL SCOPY (KRANK, W, MDW+1, WS(N2), 1)
C
C     Reciprocate diagonal terms.
C
      DO 210 J = 1,KRANK
         W(J,J) = 1.E0/W(J,J)
  210 CONTINUE
C
C     Invert the upper triangular QR factor on itself.
C
      IF (KRANK.GT.1) THEN
         DO 230 I = 1,KRM1
            DO 220 J = I+1,KRANK
               W(I,J) = -SDOT(J-I,W(I,I),MDW,W(I,J),1)*W(J,J)
  220       CONTINUE
  230    CONTINUE
      ENDIF
C
C     Compute the inverted factor times its transpose.
C
      DO 250 I = 1,KRANK
         DO 240 J = I,KRANK
            W(I,J) = SDOT(KRANK+1-J,W(I,J),MDW,W(J,J),MDW)
  240    CONTINUE
  250 CONTINUE
C
C     Zero out lower trapezoidal part.
C     Copy upper triangular to lower triangular part.
C
      IF (KRANK.LT.N) THEN
         DO 260 J = 1,KRANK
            CALL SCOPY (J, W(1,J), 1, W(J,1), MDW)
  260    CONTINUE
C
         DO 270 I = KRP1,N
            CALL SCOPY (I, 0.E0, 0, W(I,1), MDW)
  270    CONTINUE
C
C        Apply right side transformations to lower triangle.
C
         N3 = N2 + KRP1
         DO 330 I = 1,KRANK
            L = N1 + I
            K = N2 + I
            RB = WS(L-1)*WS(K-1)
C
C           If RB.GE.0.E0, transformation can be regarded as zero.
C
            IF (RB.LT.0.E0) THEN
               RB = 1.E0/RB
C
C              Store unscaled rank one Householder update in work array.
C
               CALL SCOPY (N, 0.E0, 0, WS(N3), 1)
               L = N1 + I
               K = N3 + I
               WS(K-1) = WS(L-1)
C
               DO 280 J = KRP1,N
                  WS(N3+J-1) = W(I,J)
  280          CONTINUE
C
               DO 290 J = 1,N
                  WS(J) = RB*(SDOT(J-I,W(J,I),MDW,WS(N3+I-1),1)+
     +                    SDOT(N-J+1,W(J,J),1,WS(N3+J-1),1))
  290          CONTINUE
C
               L = N3 + I
               GAM = 0.5E0*RB*SDOT(N-I+1,WS(L-1),1,WS(I),1)
               CALL SAXPY (N-I+1, GAM, WS(L-1), 1, WS(I), 1)
               DO 320 J = I,N
                  DO 300 L = 1,I-1
                     W(J,L) = W(J,L) + WS(N3+J-1)*WS(L)
  300             CONTINUE
C
                  DO 310 L = I,J
                     W(J,L) = W(J,L) + WS(J)*WS(N3+L-1)+WS(L)*WS(N3+J-1)
  310             CONTINUE
  320          CONTINUE
            ENDIF
  330    CONTINUE
C
C        Copy lower triangle to upper triangle to symmetrize the
C        covariance matrix.
C
         DO 340 I = 1,N
            CALL SCOPY (I, W(I,1), MDW, W(1,I), 1)
  340    CONTINUE
      ENDIF
C
C     Repermute rows and columns.
C
      DO 350 I = MINMAN,1,-1
         K = IP(I)
         IF (I.NE.K) THEN
            CALL SSWAP (1, W(I,I), 1, W(K,K), 1)
            CALL SSWAP (I-1, W(1,I), 1, W(1,K), 1)
            CALL SSWAP (K-I-1, W(I,I+1), MDW, W(I+1,K), 1)
            CALL SSWAP (N-K, W(I, K+1), MDW, W(K, K+1), MDW)
         ENDIF
  350 CONTINUE
C
C     Put in normalized residual sum of squares scale factor
C     and symmetrize the resulting covariance matrix.
C
      DO 360 J = 1,N
         CALL SSCAL (J, FAC, W(1,J), 1)
         CALL SCOPY (J, W(1,J), 1, W(J,1), MDW)
  360 CONTINUE
C
  370 IP(1) = KRANK
      IP(2) = N + MAX(M,N) + (MG+2)*(N+7)
      RETURN
      END
      SUBROUTINE SROTM (N,SX,INCX,SY,INCY,SPARAM)
C
C     APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
C
C     (SX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF SX ARE IN
C     (DX**T)
C
C     SX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
C     LX = (-INCX)*N, AND SIMILARLY FOR SY USING USING LY AND INCY.
C     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..
C
C     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0
C
C       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)
C     H=(          )    (          )    (          )    (          )
C       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
C     SEE  SROTMG FOR A DESCRIPTION OF DATA STORAGE IN SPARAM.
C
      DIMENSION SX(1),SY(1),SPARAM(5)
      DATA ZERO,TWO/0.E0,2.E0/
C
      SFLAG=SPARAM(1)
      IF(N .LE. 0 .OR.(SFLAG+TWO.EQ.ZERO)) GO TO 140
          IF(.NOT.(INCX.EQ.INCY.AND. INCX .GT.0)) GO TO 70
C
               NSTEPS=N*INCX
               IF(SFLAG) 50,10,30
   10          CONTINUE
               SH12=SPARAM(4)
               SH21=SPARAM(3)
                    DO 20 I=1,NSTEPS,INCX
                    W=SX(I)
                    Z=SY(I)
                    SX(I)=W+Z*SH12
                    SY(I)=W*SH21+Z
   20               CONTINUE
               GO TO 140
   30          CONTINUE
               SH11=SPARAM(2)
               SH22=SPARAM(5)
                    DO 40 I=1,NSTEPS,INCX
                    W=SX(I)
                    Z=SY(I)
                    SX(I)=W*SH11+Z
                    SY(I)=-W+SH22*Z
   40               CONTINUE
               GO TO 140
   50          CONTINUE
               SH11=SPARAM(2)
               SH12=SPARAM(4)
               SH21=SPARAM(3)
               SH22=SPARAM(5)
                    DO 60 I=1,NSTEPS,INCX
                    W=SX(I)
                    Z=SY(I)
                    SX(I)=W*SH11+Z*SH12
                    SY(I)=W*SH21+Z*SH22
   60               CONTINUE
               GO TO 140
   70     CONTINUE
          KX=1
          KY=1
          IF(INCX .LT. 0) KX=1+(1-N)*INCX
          IF(INCY .LT. 0) KY=1+(1-N)*INCY
C
          IF(SFLAG)120,80,100
   80     CONTINUE
          SH12=SPARAM(4)
          SH21=SPARAM(3)
               DO 90 I=1,N
               W=SX(KX)
               Z=SY(KY)
               SX(KX)=W+Z*SH12
               SY(KY)=W*SH21+Z
               KX=KX+INCX
               KY=KY+INCY
   90          CONTINUE
          GO TO 140
  100     CONTINUE
          SH11=SPARAM(2)
          SH22=SPARAM(5)
               DO 110 I=1,N
               W=SX(KX)
               Z=SY(KY)
               SX(KX)=W*SH11+Z
               SY(KY)=-W+SH22*Z
               KX=KX+INCX
               KY=KY+INCY
  110          CONTINUE
          GO TO 140
  120     CONTINUE
          SH11=SPARAM(2)
          SH12=SPARAM(4)
          SH21=SPARAM(3)
          SH22=SPARAM(5)
               DO 130 I=1,N
               W=SX(KX)
               Z=SY(KY)
               SX(KX)=W*SH11+Z*SH12
               SY(KY)=W*SH21+Z*SH22
               KX=KX+INCX
               KY=KY+INCY
  130          CONTINUE
  140     CONTINUE
          RETURN
          END
      SUBROUTINE SROTMG (SD1,SD2,SX1,SY1,SPARAM)
C
C     CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
C     THE SECOND COMPONENT OF THE 2-VECTOR  (SQRT(SD1)*SX1,SQRT(SD2)*
C     SY2)**T.
C     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..
C
C     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0
C
C       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)
C     H=(          )    (          )    (          )    (          )
C       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
C     LOCATIONS 2-4 OF SPARAM CONTAIN SH11,SH21,SH12, AND SH22
C     RESPECTIVELY. (VALUES OF 1.E0, -1.E0, OR 0.E0 IMPLIED BY THE
C     VALUE OF SPARAM(1) ARE NOT STORED IN SPARAM.)
C
C     THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
C     INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
C     OF SD1 AND SD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
C
      DIMENSION SPARAM(5)
C
      DATA ZERO,ONE,TWO /0.E0,1.E0,2.E0/
      DATA GAM,GAMSQ,RGAMSQ/4096.E0,1.67772E7,5.96046E-8/
      IF(.NOT. SD1 .LT. ZERO) GO TO 10
C       GO ZERO-H-D-AND-SX1..
          GO TO 60
   10 CONTINUE
C     CASE-SD1-NONNEGATIVE
      SP2=SD2*SY1
      IF(.NOT. SP2 .EQ. ZERO) GO TO 20
          SFLAG=-TWO
          GO TO 260
C     REGULAR-CASE..
   20 CONTINUE
      SP1=SD1*SX1
      SQ2=SP2*SY1
      SQ1=SP1*SX1
C
      IF(.NOT. ABS(SQ1) .GT. ABS(SQ2)) GO TO 40
          SH21=-SY1/SX1
          SH12=SP2/SP1
C
          SU=ONE-SH12*SH21
C
          IF(.NOT. SU .LE. ZERO) GO TO 30
C         GO ZERO-H-D-AND-SX1..
               GO TO 60
   30     CONTINUE
               SFLAG=ZERO
               SD1=SD1/SU
               SD2=SD2/SU
               SX1=SX1*SU
C         GO SCALE-CHECK..
               GO TO 100
   40 CONTINUE
          IF(.NOT. SQ2 .LT. ZERO) GO TO 50
C         GO ZERO-H-D-AND-SX1..
               GO TO 60
   50     CONTINUE
               SFLAG=ONE
               SH11=SP1/SP2
               SH22=SX1/SY1
               SU=ONE+SH11*SH22
               STEMP=SD2/SU
               SD2=SD1/SU
               SD1=STEMP
               SX1=SY1*SU
C         GO SCALE-CHECK
               GO TO 100
C     PROCEDURE..ZERO-H-D-AND-SX1..
   60 CONTINUE
          SFLAG=-ONE
          SH11=ZERO
          SH12=ZERO
          SH21=ZERO
          SH22=ZERO
C
          SD1=ZERO
          SD2=ZERO
          SX1=ZERO
C         RETURN..
          GO TO 220
C     PROCEDURE..FIX-H..
   70 CONTINUE
      IF(.NOT. SFLAG .GE. ZERO) GO TO 90
C
          IF(.NOT. SFLAG .EQ. ZERO) GO TO 80
          SH11=ONE
          SH22=ONE
          SFLAG=-ONE
          GO TO 90
   80     CONTINUE
          SH21=-ONE
          SH12=ONE
          SFLAG=-ONE
   90 CONTINUE
      GO TO IGO,(120,150,180,210)
C     PROCEDURE..SCALE-CHECK
  100 CONTINUE
  110     CONTINUE
          IF(.NOT. SD1 .LE. RGAMSQ) GO TO 130
               IF(SD1 .EQ. ZERO) GO TO 160
               ASSIGN 120 TO IGO
C              FIX-H..
               GO TO 70
  120          CONTINUE
               SD1=SD1*GAM**2
               SX1=SX1/GAM
               SH11=SH11/GAM
               SH12=SH12/GAM
          GO TO 110
  130 CONTINUE
  140     CONTINUE
          IF(.NOT. SD1 .GE. GAMSQ) GO TO 160
               ASSIGN 150 TO IGO
C              FIX-H..
               GO TO 70
  150          CONTINUE
               SD1=SD1/GAM**2
               SX1=SX1*GAM
               SH11=SH11*GAM
               SH12=SH12*GAM
          GO TO 140
  160 CONTINUE
  170     CONTINUE
          IF(.NOT. ABS(SD2) .LE. RGAMSQ) GO TO 190
               IF(SD2 .EQ. ZERO) GO TO 220
               ASSIGN 180 TO IGO
C              FIX-H..
               GO TO 70
  180          CONTINUE
               SD2=SD2*GAM**2
               SH21=SH21/GAM
               SH22=SH22/GAM
          GO TO 170
  190 CONTINUE
  200     CONTINUE
          IF(.NOT. ABS(SD2) .GE. GAMSQ) GO TO 220
               ASSIGN 210 TO IGO
C              FIX-H..
               GO TO 70
  210          CONTINUE
               SD2=SD2/GAM**2
               SH21=SH21*GAM
               SH22=SH22*GAM
          GO TO 200
  220 CONTINUE
          IF(SFLAG)250,230,240
  230     CONTINUE
               SPARAM(3)=SH21
               SPARAM(4)=SH12
               GO TO 260
  240     CONTINUE
               SPARAM(2)=SH11
               SPARAM(5)=SH22
               GO TO 260
  250     CONTINUE
               SPARAM(2)=SH11
               SPARAM(3)=SH21
               SPARAM(4)=SH12
               SPARAM(5)=SH22
  260 CONTINUE
          SPARAM(1)=SFLAG
          RETURN
      END
      real function snrm2 ( n, sx, incx)
      integer i, incx, ix, j, n, next
      real   sx(1),  cutlo, cuthi, hitest, sum, xmax, zero, one
      data   zero, one /0.0e0, 1.0e0/
c
c     euclidean norm of the n-vector stored in sx() with storage
c     increment incx .
c     if    n .le. 0 return with result = 0.
c     if n .ge. 1 then incx must be .ge. 1
c
c           c.l.lawson, 1978 jan 08
c     modified to correct failure to update ix, 1/25/92.
c     modified 3/93 to return if incx .le. 0.
c
c     four phase method     using two built-in constants that are
c     hopefully applicable to all machines.
c         cutlo = maximum of  sqrt(u/eps)  over all known machines.
c         cuthi = minimum of  sqrt(v)      over all known machines.
c     where
c         eps = smallest no. such that eps + 1. .gt. 1.
c         u   = smallest positive no.   (underflow limit)
c         v   = largest  no.            (overflow  limit)
c
c     brief outline of algorithm..
c
c     phase 1    scans zero components.
c     move to phase 2 when a component is nonzero and .le. cutlo
c     move to phase 3 when a component is .gt. cutlo
c     move to phase 4 when a component is .ge. cuthi/m
c     where m = n for x() real and m = 2*n for complex.
c
c     values for cutlo and cuthi..
c     from the environmental parameters listed in the imsl converter
c     document the limiting values are as follows..
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
c                   univac and dec at 2**(-103)
c                   thus cutlo = 2**(-51) = 4.44089e-16
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
c                   thus cuthi = 2**(63.5) = 1.30438e19
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
c                   thus cutlo = 2**(-33.5) = 8.23181d-11
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /
      data cutlo, cuthi / 4.441e-16,  1.304e19 /
c
      if(n .gt. 0 .and. incx.gt.0) go to 10
         snrm2  = zero
         go to 300
c
   10 assign 30 to next
      sum = zero
      i = 1
      ix = 1
c                                                 begin main loop
   20    go to next,(30, 50, 70, 110)
   30 if( abs(sx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
c
c                        phase 1.  sum is zero
c
   50 if( sx(i) .eq. zero) go to 200
      if( abs(sx(i)) .gt. cutlo) go to 85
c
c                                prepare for phase 2.
      assign 70 to next
      go to 105
c
c                                prepare for phase 4.
c
  100 continue
      ix = j
      assign 110 to next
      sum = (sum / sx(i)) / sx(i)
  105 xmax = abs(sx(i))
      go to 115
c
c                   phase 2.  sum is small.
c                             scale to avoid destructive underflow.
c
   70 if( abs(sx(i)) .gt. cutlo ) go to 75
c
c                     common code for phases 2 and 4.
c                     in phase 4 sum is large.  scale to avoid overflow.
c
  110 if( abs(sx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / sx(i))**2
         xmax = abs(sx(i))
         go to 200
c
  115 sum = sum + (sx(i)/xmax)**2
      go to 200
c
c
c                  prepare for phase 3.
c
   75 sum = (sum * xmax) * xmax
c
c
c     for real or d.p. set hitest = cuthi/n
c     for complex      set hitest = cuthi/(2*n)
c
   85 hitest = cuthi/float( n )
c
c                   phase 3.  sum is mid-range.  no scaling.
c
      do 95 j = ix, n
         if(abs(sx(i)) .ge. hitest) go to 100
         sum = sum + sx(i)**2
         i = i + incx
   95 continue
      snrm2 = sqrt( sum )
      go to 300
c
  200 continue
      ix = ix + 1
      i = i + incx
      if( ix .le. n ) go to 20
c
c              end of main loop.
c
c              compute square root and adjust for scaling.
c
      snrm2 = xmax * sqrt(sum)
  300 continue
      return
      end
      SUBROUTINE WNLIT (W, MDW, M, N, L, IPIVOT, ITYPE, H, SCALE, RNORM,
     +   IDOPE, DOPE, DONE)
C***BEGIN PROLOGUE  WNLIT
C***SUBSIDIARY
C***PURPOSE  Subsidiary to WNNLS
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (WNLIT-S, DWNLIT-D)
C***AUTHOR  Hanson, R. J., (SNLA)
C           Haskell, K. H., (SNLA)
C***DESCRIPTION
C
C     This is a companion subprogram to WNNLS( ).
C     The documentation for WNNLS( ) has complete usage instructions.
C
C     Note  The M by (N+1) matrix W( , ) contains the rt. hand side
C           B as the (N+1)st col.
C
C     Triangularize L1 by L1 subsystem, where L1=MIN(M,L), with
C     col interchanges.
C
C***SEE ALSO  WNNLS
C***ROUTINES CALLED  H12, ISAMAX, SCOPY, SROTM, SROTMG, SSCAL, SSWAP,
C                    WNLT1, WNLT2, WNLT3
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890618  Completely restructured and revised.  (WRB & RWC)
C   890620  Revised to make WNLT1, WNLT2, and WNLT3 subroutines.  (RWC)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  WNLIT
      INTEGER IDOPE(*), IPIVOT(*), ITYPE(*), L, M, MDW, N
      REAL             DOPE(*), H(*), RNORM, SCALE(*), W(MDW,*)
      LOGICAL DONE
C
      EXTERNAL H12, ISAMAX, SCOPY, SROTM, SROTMG, SSCAL, SSWAP, WNLT1,
     *   WNLT2, WNLT3
      INTEGER ISAMAX
      LOGICAL WNLT2
C
      REAL             ALSQ, AMAX, EANORM, FACTOR, HBAR, RN, SPARAM(5),
     *   T, TAU
      INTEGER I, I1, IMAX, IR, J, J1, JJ, JP, KRANK, L1, LB, LEND, ME,
     *   MEND, NIV, NSOLN
      LOGICAL INDEP, RECALC
C
C***FIRST EXECUTABLE STATEMENT  WNLIT
      ME    = IDOPE(1)
      NSOLN = IDOPE(2)
      L1    = IDOPE(3)
C
      ALSQ   = DOPE(1)
      EANORM = DOPE(2)
      TAU    = DOPE(3)
C
      LB     = MIN(M-1,L)
      RECALC = .TRUE.
      RNORM  = 0.E0
      KRANK  = 0
C
C     We set FACTOR=1.0 so that the heavy weight ALAMDA will be
C     included in the test for column independence.
C
      FACTOR = 1.E0
      LEND = L
      DO 180 I=1,LB
C
C        Set IR to point to the I-th row.
C
         IR = I
         MEND = M
         CALL WNLT1 (I, LEND, M, IR, MDW, RECALC, IMAX, HBAR, H, SCALE,
     +                W)
C
C        Update column SS and find pivot column.
C
         CALL WNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
C
C        Perform column interchange.
C        Test independence of incoming column.
C
  130    IF (WNLT2(ME, MEND, IR, FACTOR, TAU, SCALE, W(1,I))) THEN
C
C           Eliminate I-th column below diagonal using modified Givens
C           transformations applied to (A B).
C
C           When operating near the ME line, use the largest element
C           above it as the pivot.
C
            DO 160 J=M,I+1,-1
               JP = J-1
               IF (J.EQ.ME+1) THEN
                  IMAX = ME
                  AMAX = SCALE(ME)*W(ME,I)**2
                  DO 150 JP=J-1,I,-1
                     T = SCALE(JP)*W(JP,I)**2
                     IF (T.GT.AMAX) THEN
                        IMAX = JP
                        AMAX = T
                     ENDIF
  150             CONTINUE
                  JP = IMAX
               ENDIF
C
               IF (W(J,I).NE.0.E0) THEN
                  CALL SROTMG (SCALE(JP), SCALE(J), W(JP,I), W(J,I),
     +                         SPARAM)
                  W(J,I) = 0.E0
                  CALL SROTM (N+1-I, W(JP,I+1), MDW, W(J,I+1), MDW,
     +                        SPARAM)
               ENDIF
  160       CONTINUE
         ELSE IF (LEND.GT.I) THEN
C
C           Column I is dependent.  Swap with column LEND.
C           Perform column interchange,
C           and find column in remaining set with largest SS.
C
            CALL WNLT3 (I, LEND, M, MDW, IPIVOT, H, W)
            LEND = LEND - 1
            IMAX = ISAMAX(LEND-I+1, H(I), 1) + I - 1
            HBAR = H(IMAX)
            GO TO 130
         ELSE
            KRANK = I - 1
            GO TO 190
         ENDIF
  180 CONTINUE
      KRANK = L1
C
  190 IF (KRANK.LT.ME) THEN
         FACTOR = ALSQ
         DO 200 I=KRANK+1,ME
            CALL SCOPY (L, 0.E0, 0, W(I,1), MDW)
  200    CONTINUE
C
C        Determine the rank of the remaining equality constraint
C        equations by eliminating within the block of constrained
C        variables.  Remove any redundant constraints.
C
         RECALC = .TRUE.
         LB = MIN(L+ME-KRANK, N)
         DO 270 I=L+1,LB
            IR = KRANK + I - L
            LEND = N
            MEND = ME
            CALL WNLT1 (I, LEND, ME, IR, MDW, RECALC, IMAX, HBAR, H,
     +                   SCALE, W)
C
C           Update col ss and find pivot col
C
            CALL WNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
C
C           Perform column interchange
C           Eliminate elements in the I-th col.
C
            DO 240 J=ME,IR+1,-1
               IF (W(J,I).NE.0.E0) THEN
                 CALL SROTMG (SCALE(J-1), SCALE(J), W(J-1,I), W(J,I),
     +                        SPARAM)
                  W(J,I) = 0.E0
                  CALL SROTM (N+1-I, W(J-1,I+1), MDW,W(J,I+1), MDW,
     +                        SPARAM)
               ENDIF
  240       CONTINUE
C
C           I=column being eliminated.
C           Test independence of incoming column.
C           Remove any redundant or dependent equality constraints.
C
            IF (.NOT.WNLT2(ME, MEND, IR, FACTOR,TAU,SCALE,W(1,I))) THEN
               JJ = IR
               DO 260 IR=JJ,ME
                  CALL SCOPY (N, 0.E0, 0, W(IR,1), MDW)
                  RNORM = RNORM + (SCALE(IR)*W(IR,N+1)/ALSQ)*W(IR,N+1)
                  W(IR,N+1) = 0.E0
                  SCALE(IR) = 1.E0
C
C                 Reclassify the zeroed row as a least squares equation.
C
                  ITYPE(IR) = 1
  260          CONTINUE
C
C              Reduce ME to reflect any discovered dependent equality
C              constraints.
C
               ME = JJ - 1
               GO TO 280
            ENDIF
  270    CONTINUE
      ENDIF
C
C     Try to determine the variables KRANK+1 through L1 from the
C     least squares equations.  Continue the triangularization with
C     pivot element W(ME+1,I).
C
  280 IF (KRANK.LT.L1) THEN
         RECALC = .TRUE.
C
C        Set FACTOR=ALSQ to remove effect of heavy weight from
C        test for column independence.
C
         FACTOR = ALSQ
         DO 350 I=KRANK+1,L1
C
C           Set IR to point to the ME+1-st row.
C
            IR = ME+1
            LEND = L
            MEND = M
            CALL WNLT1 (I, L, M, IR, MDW, RECALC, IMAX, HBAR, H, SCALE,
     +                   W)
C
C           Update column SS and find pivot column.
C
            CALL WNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
C
C           Perform column interchange.
C           Eliminate I-th column below the IR-th element.
C
            DO 320 J=M,IR+1,-1
               IF (W(J,I).NE.0.E0) THEN
                 CALL SROTMG (SCALE(J-1), SCALE(J), W(J-1,I), W(J,I),
     +                        SPARAM)
                  W(J,I) = 0.E0
                  CALL SROTM (N+1-I, W(J-1,I+1),  MDW, W(J,I+1), MDW,
     +                        SPARAM)
               ENDIF
  320       CONTINUE
C
C           Test if new pivot element is near zero.
C           If so, the column is dependent.
C           Then check row norm test to be classified as independent.
C
            T = SCALE(IR)*W(IR,I)**2
            INDEP = T .GT. (TAU*EANORM)**2
            IF (INDEP) THEN
               RN = 0.E0
               DO 340 I1=IR,M
                  DO 330 J1=I+1,N
                     RN = MAX(RN, SCALE(I1)*W(I1,J1)**2)
  330             CONTINUE
  340          CONTINUE
               INDEP = T .GT. RN*TAU**2
            ENDIF
C
C           If independent, swap the IR-th and KRANK+1-th rows to
C           maintain the triangular form.  Update the rank indicator
C           KRANK and the equality constraint pointer ME.
C
            IF (.NOT.INDEP) GO TO 360
            CALL SSWAP(N+1, W(KRANK+1,1), MDW, W(IR,1), MDW)
            CALL SSWAP(1, SCALE(KRANK+1), 1, SCALE(IR), 1)
C
C           Reclassify the least square equation as an equality
C           constraint and rescale it.
C
            ITYPE(IR) = 0
            T = SQRT(SCALE(KRANK+1))
            CALL SSCAL(N+1, T, W(KRANK+1,1), MDW)
            SCALE(KRANK+1) = ALSQ
            ME = ME+1
            KRANK = KRANK+1
  350    CONTINUE
      ENDIF
C
C     If pseudorank is less than L, apply Householder transformation.
C     from right.
C
  360 IF (KRANK.LT.L) THEN
         DO 370 J=KRANK,1,-1
            CALL H12 (1, J, KRANK+1, L, W(J,1), MDW, H(J), W, MDW, 1,
     +                J-1)
  370    CONTINUE
      ENDIF
C
      NIV = KRANK + NSOLN - L
      IF (L.EQ.N) DONE = .TRUE.
C
C     End of initial triangularization.
C
      IDOPE(1) = ME
      IDOPE(2) = KRANK
      IDOPE(3) = NIV
      RETURN
      END
      SUBROUTINE WNLSM (W, MDW, MME, MA, N, L, PRGOPT, X, RNORM, MODE,
     +   IPIVOT, ITYPE, WD, H, SCALE, Z, TEMP, D)
C***BEGIN PROLOGUE  WNLSM
C***SUBSIDIARY
C***PURPOSE  Subsidiary to WNNLS
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (WNLSM-S, DWNLSM-D)
C***AUTHOR  Hanson, R. J., (SNLA)
C           Haskell, K. H., (SNLA)
C***DESCRIPTION
C
C     This is a companion subprogram to WNNLS.
C     The documentation for WNNLS has complete usage instructions.
C
C     In addition to the parameters discussed in the prologue to
C     subroutine WNNLS, the following work arrays are used in
C     subroutine WNLSM  (they are passed through the calling
C     sequence from WNNLS for purposes of variable dimensioning).
C     Their contents will in general be of no interest to the user.
C
C         IPIVOT(*)
C            An array of length N.  Upon completion it contains the
C         pivoting information for the cols of W(*,*).
C
C         ITYPE(*)
C            An array of length M which is used to keep track
C         of the classification of the equations.  ITYPE(I)=0
C         denotes equation I as an equality constraint.
C         ITYPE(I)=1 denotes equation I as a least squares
C         equation.
C
C         WD(*)
C            An array of length N.  Upon completion it contains the
C         dual solution vector.
C
C         H(*)
C            An array of length N.  Upon completion it contains the
C         pivot scalars of the Householder transformations performed
C         in the case KRANK.LT.L.
C
C         SCALE(*)
C            An array of length M which is used by the subroutine
C         to store the diagonal matrix of weights.
C         These are used to apply the modified Givens
C         transformations.
C
C         Z(*),TEMP(*)
C            Working arrays of length N.
C
C         D(*)
C            An array of length N that contains the
C         column scaling for the matrix (E).
C                                       (A)
C
C***SEE ALSO  WNNLS
C***ROUTINES CALLED  H12, ISAMAX, R1MACH, SASUM, SAXPY, SCOPY, SNRM2,
C                    SROTM, SROTMG, SSCAL, SSWAP, WNLIT, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890618  Completely restructured and revised.  (WRB & RWC)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   900510  Fixed an error message.  (RWC)
C***END PROLOGUE  WNLSM
      INTEGER IPIVOT(*), ITYPE(*), L, MA, MDW, MME, MODE, N
      REAL             D(*), H(*), PRGOPT(*), RNORM, SCALE(*), TEMP(*),
     *   W(MDW,*), WD(*), X(*), Z(*)
C
      EXTERNAL H12, ISAMAX, R1MACH, SASUM, SAXPY, SCOPY, SNRM2, SROTM,
     *   SROTMG, SSCAL, SSWAP, WNLIT, XERMSG
      REAL             R1MACH, SASUM, SNRM2
      INTEGER ISAMAX
C
      REAL             ALAMDA, ALPHA, ALSQ, AMAX, BLOWUP, BNORM,
     *   DOPE(3), EANORM, FAC, SM, SPARAM(5), SRELPR, T, TAU, WMAX, Z2,
     *   ZZ
      INTEGER I, IDOPE(3), IMAX, ISOL, ITEMP, ITER, ITMAX, IWMAX, J,
     *   JCON, JP, KEY, KRANK, L1, LAST, LINK, M, ME, NEXT, NIV, NLINK,
     *   NOPT, NSOLN, NTIMES
      LOGICAL DONE, FEASBL, FIRST, HITCON, POS
C
      SAVE SRELPR, FIRST
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  WNLSM
C
C     Initialize variables.
C     SRELPR is the precision for the particular machine
C     being used.  This logic avoids resetting it every entry.
C
      IF (FIRST) SRELPR = R1MACH(4)
      FIRST = .FALSE.
C
C     Set the nominal tolerance used in the code.
C
      TAU = SQRT(SRELPR)
C
      M = MA + MME
      ME = MME
      MODE = 2
C
C     To process option vector
C
      FAC = 1.E-4
C
C     Set the nominal blow up factor used in the code.
C
      BLOWUP = TAU
C
C     The nominal column scaling used in the code is
C     the identity scaling.
C
      CALL SCOPY (N, 1.E0, 0, D, 1)
C
C     Define bound for number of options to change.
C
      NOPT = 1000
C
C     Define bound for positive value of LINK.
C
      NLINK = 100000
      NTIMES = 0
      LAST = 1
      LINK = PRGOPT(1)
      IF (LINK.LE.0 .OR. LINK.GT.NLINK) THEN
         CALL XERMSG ('SLATEC', 'WNLSM',
     +      'WNNLS, THE OPTION VECTOR IS UNDEFINED', 3, 1)
         RETURN
      ENDIF
C
  100 IF (LINK.GT.1) THEN
         NTIMES = NTIMES + 1
         IF (NTIMES.GT.NOPT) THEN
         CALL XERMSG ('SLATEC', 'WNLSM',
     +      'WNNLS, THE LINKS IN THE OPTION VECTOR ARE CYCLING.', 3, 1)
            RETURN
         ENDIF
C
         KEY = PRGOPT(LAST+1)
         IF (KEY.EQ.6 .AND. PRGOPT(LAST+2).NE.0.E0) THEN
            DO 110 J = 1,N
               T = SNRM2(M,W(1,J),1)
               IF (T.NE.0.E0) T = 1.E0/T
               D(J) = T
  110       CONTINUE
         ENDIF
C
         IF (KEY.EQ.7) CALL SCOPY (N, PRGOPT(LAST+2), 1, D, 1)
         IF (KEY.EQ.8) TAU = MAX(SRELPR,PRGOPT(LAST+2))
         IF (KEY.EQ.9) BLOWUP = MAX(SRELPR,PRGOPT(LAST+2))
C
         NEXT = PRGOPT(LINK)
         IF (NEXT.LE.0 .OR. NEXT.GT.NLINK) THEN
            CALL XERMSG ('SLATEC', 'WNLSM',
     +         'WNNLS, THE OPTION VECTOR IS UNDEFINED', 3, 1)
            RETURN
         ENDIF
C
         LAST = LINK
         LINK = NEXT
         GO TO 100
      ENDIF
C
      DO 120 J = 1,N
         CALL SSCAL (M, D(J), W(1,J), 1)
  120 CONTINUE
C
C     Process option vector
C
      DONE = .FALSE.
      ITER = 0
      ITMAX = 3*(N-L)
      MODE = 0
      NSOLN = L
      L1 = MIN(M,L)
C
C     Compute scale factor to apply to equality constraint equations.
C
      DO 130 J = 1,N
         WD(J) = SASUM(M,W(1,J),1)
  130 CONTINUE
C
      IMAX = ISAMAX(N,WD,1)
      EANORM = WD(IMAX)
      BNORM = SASUM(M,W(1,N+1),1)
      ALAMDA = EANORM/(SRELPR*FAC)
C
C     Define scaling diagonal matrix for modified Givens usage and
C     classify equation types.
C
      ALSQ = ALAMDA**2
      DO 140 I = 1,M
C
C        When equation I is heavily weighted ITYPE(I)=0,
C        else ITYPE(I)=1.
C
         IF (I.LE.ME) THEN
            T = ALSQ
            ITEMP = 0
         ELSE
            T = 1.E0
            ITEMP = 1
         ENDIF
         SCALE(I) = T
         ITYPE(I) = ITEMP
  140 CONTINUE
C
C     Set the solution vector X(*) to zero and the column interchange
C     matrix to the identity.
C
      CALL SCOPY (N, 0.E0, 0, X, 1)
      DO 150 I = 1,N
         IPIVOT(I) = I
  150 CONTINUE
C
C     Perform initial triangularization in the submatrix
C     corresponding to the unconstrained variables.
C     Set first L components of dual vector to zero because
C     these correspond to the unconstrained variables.
C
      CALL SCOPY (L, 0.E0, 0, WD, 1)
C
C     The arrays IDOPE(*) and DOPE(*) are used to pass
C     information to WNLIT().  This was done to avoid
C     a long calling sequence or the use of COMMON.
C
      IDOPE(1) = ME
      IDOPE(2) = NSOLN
      IDOPE(3) = L1
C
      DOPE(1) = ALSQ
      DOPE(2) = EANORM
      DOPE(3) = TAU
      CALL WNLIT (W, MDW, M, N, L, IPIVOT, ITYPE, H, SCALE, RNORM,
     +            IDOPE, DOPE, DONE)
      ME    = IDOPE(1)
      KRANK = IDOPE(2)
      NIV   = IDOPE(3)
C
C     Perform WNNLS algorithm using the following steps.
C
C     Until(DONE)
C        compute search direction and feasible point
C        when (HITCON) add constraints
C        else perform multiplier test and drop a constraint
C        fin
C     Compute-Final-Solution
C
C     To compute search direction and feasible point,
C     solve the triangular system of currently non-active
C     variables and store the solution in Z(*).
C
C     To solve system
C     Copy right hand side into TEMP vector to use overwriting method.
C
  160 IF (DONE) GO TO 330
      ISOL = L + 1
      IF (NSOLN.GE.ISOL) THEN
         CALL SCOPY (NIV, W(1,N+1), 1, TEMP, 1)
         DO 170 J = NSOLN,ISOL,-1
            IF (J.GT.KRANK) THEN
               I = NIV - NSOLN + J
            ELSE
               I = J
            ENDIF
C
            IF (J.GT.KRANK .AND. J.LE.L) THEN
               Z(J) = 0.E0
            ELSE
               Z(J) = TEMP(I)/W(I,J)
               CALL SAXPY (I-1, -Z(J), W(1,J), 1, TEMP, 1)
            ENDIF
  170    CONTINUE
      ENDIF
C
C     Increment iteration counter and check against maximum number
C     of iterations.
C
      ITER = ITER + 1
      IF (ITER.GT.ITMAX) THEN
         MODE = 1
         DONE = .TRUE.
      ENDIF
C
C     Check to see if any constraints have become active.
C     If so, calculate an interpolation factor so that all
C     active constraints are removed from the basis.
C
      ALPHA = 2.E0
      HITCON = .FALSE.
      DO 180 J = L+1,NSOLN
         ZZ = Z(J)
         IF (ZZ.LE.0.E0) THEN
            T = X(J)/(X(J)-ZZ)
            IF (T.LT.ALPHA) THEN
               ALPHA = T
               JCON = J
            ENDIF
            HITCON = .TRUE.
         ENDIF
  180 CONTINUE
C
C     Compute search direction and feasible point
C
      IF (HITCON) THEN
C
C        To add constraints, use computed ALPHA to interpolate between
C        last feasible solution X(*) and current unconstrained (and
C        infeasible) solution Z(*).
C
         DO 190 J = L+1,NSOLN
            X(J) = X(J) + ALPHA*(Z(J)-X(J))
  190    CONTINUE
         FEASBL = .FALSE.
C
C        Remove column JCON and shift columns JCON+1 through N to the
C        left.  Swap column JCON into the N th position.  This achieves
C        upper Hessenberg form for the nonactive constraints and
C        leaves an upper Hessenberg matrix to retriangularize.
C
  200    DO 210 I = 1,M
            T = W(I,JCON)
            CALL SCOPY (N-JCON, W(I, JCON+1), MDW, W(I, JCON), MDW)
            W(I,N) = T
  210    CONTINUE
C
C        Update permuted index vector to reflect this shift and swap.
C
         ITEMP = IPIVOT(JCON)
         DO 220 I = JCON,N - 1
            IPIVOT(I) = IPIVOT(I+1)
  220    CONTINUE
         IPIVOT(N) = ITEMP
C
C        Similarly permute X(*) vector.
C
         CALL SCOPY (N-JCON, X(JCON+1), 1, X(JCON), 1)
         X(N) = 0.E0
         NSOLN = NSOLN - 1
         NIV = NIV - 1
C
C        Retriangularize upper Hessenberg matrix after adding
C        constraints.
C
         I = KRANK + JCON - L
         DO 230 J = JCON,NSOLN
            IF (ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.0) THEN
C
C              Zero IP1 to I in column J
C
               IF (W(I+1,J).NE.0.E0) THEN
                  CALL SROTMG (SCALE(I), SCALE(I+1), W(I,J), W(I+1,J),
     +                         SPARAM)
                  W(I+1,J) = 0.E0
                  CALL SROTM (N+1-J, W(I,J+1), MDW, W(I+1,J+1), MDW,
     +                        SPARAM)
               ENDIF
            ELSEIF (ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.1) THEN
C
C              Zero IP1 to I in column J
C
               IF (W(I+1,J).NE.0.E0) THEN
                  CALL SROTMG (SCALE(I), SCALE(I+1), W(I,J), W(I+1,J),
     +                         SPARAM)
                  W(I+1,J) = 0.E0
                  CALL SROTM (N+1-J, W(I,J+1), MDW, W(I+1,J+1), MDW,
     +                        SPARAM)
               ENDIF
            ELSEIF (ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.0) THEN
               CALL SSWAP (N+1, W(I,1), MDW, W(I+1,1), MDW)
               CALL SSWAP (1, SCALE(I), 1, SCALE(I+1), 1)
               ITEMP = ITYPE(I+1)
               ITYPE(I+1) = ITYPE(I)
               ITYPE(I) = ITEMP
C
C              Swapped row was formerly a pivot element, so it will
C              be large enough to perform elimination.
C              Zero IP1 to I in column J.
C
               IF (W(I+1,J).NE.0.E0) THEN
                  CALL SROTMG (SCALE(I), SCALE(I+1), W(I,J), W(I+1,J),
     +                         SPARAM)
                  W(I+1,J) = 0.E0
                  CALL SROTM (N+1-J, W(I,J+1), MDW, W(I+1,J+1), MDW,
     +                        SPARAM)
               ENDIF
            ELSEIF (ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.1) THEN
               IF (SCALE(I)*W(I,J)**2/ALSQ.GT.(TAU*EANORM)**2) THEN
C
C                 Zero IP1 to I in column J
C
                  IF (W(I+1,J).NE.0.E0) THEN
                     CALL SROTMG (SCALE(I), SCALE(I+1), W(I,J),
     +                            W(I+1,J), SPARAM)
                     W(I+1,J) = 0.E0
                     CALL SROTM (N+1-J, W(I,J+1), MDW, W(I+1,J+1), MDW,
     +                           SPARAM)
                  ENDIF
               ELSE
                  CALL SSWAP (N+1, W(I,1), MDW, W(I+1,1), MDW)
                  CALL SSWAP (1, SCALE(I), 1, SCALE(I+1), 1)
                  ITEMP = ITYPE(I+1)
                  ITYPE(I+1) = ITYPE(I)
                  ITYPE(I) = ITEMP
                  W(I+1,J) = 0.E0
               ENDIF
            ENDIF
            I = I + 1
  230    CONTINUE
C
C        See if the remaining coefficients in the solution set are
C        feasible.  They should be because of the way ALPHA was
C        determined.  If any are infeasible, it is due to roundoff
C        error.  Any that are non-positive will be set to zero and
C        removed from the solution set.
C
         DO 240 JCON = L+1,NSOLN
            IF (X(JCON).LE.0.E0) GO TO 250
  240    CONTINUE
         FEASBL = .TRUE.
  250    IF (.NOT.FEASBL) GO TO 200
      ELSE
C
C        To perform multiplier test and drop a constraint.
C
         CALL SCOPY (NSOLN, Z, 1, X, 1)
         IF (NSOLN.LT.N) CALL SCOPY (N-NSOLN, 0.E0, 0, X(NSOLN+1), 1)
C
C        Reclassify least squares equations as equalities as necessary.
C
         I = NIV + 1
  260    IF (I.LE.ME) THEN
            IF (ITYPE(I).EQ.0) THEN
               I = I + 1
            ELSE
               CALL SSWAP (N+1, W(I,1), MDW, W(ME,1), MDW)
               CALL SSWAP (1, SCALE(I), 1, SCALE(ME), 1)
               ITEMP = ITYPE(I)
               ITYPE(I) = ITYPE(ME)
               ITYPE(ME) = ITEMP
               ME = ME - 1
            ENDIF
            GO TO 260
         ENDIF
C
C        Form inner product vector WD(*) of dual coefficients.
C
         DO 280 J = NSOLN+1,N
            SM = 0.E0
            DO 270 I = NSOLN+1,M
               SM = SM + SCALE(I)*W(I,J)*W(I,N+1)
  270       CONTINUE
            WD(J) = SM
  280    CONTINUE
C
C        Find J such that WD(J)=WMAX is maximum.  This determines
C        that the incoming column J will reduce the residual vector
C        and be positive.
C
  290    WMAX = 0.E0
         IWMAX = NSOLN + 1
         DO 300 J = NSOLN+1,N
            IF (WD(J).GT.WMAX) THEN
               WMAX = WD(J)
               IWMAX = J
            ENDIF
  300    CONTINUE
         IF (WMAX.LE.0.E0) GO TO 330
C
C        Set dual coefficients to zero for incoming column.
C
         WD(IWMAX) = 0.E0
C
C        WMAX .GT. 0.E0, so okay to move column IWMAX to solution set.
C        Perform transformation to retriangularize, and test for near
C        linear dependence.
C
C        Swap column IWMAX into NSOLN-th position to maintain upper
C        Hessenberg form of adjacent columns, and add new column to
C        triangular decomposition.
C
         NSOLN = NSOLN + 1
         NIV = NIV + 1
         IF (NSOLN.NE.IWMAX) THEN
            CALL SSWAP (M, W(1,NSOLN), 1, W(1,IWMAX), 1)
            WD(IWMAX) = WD(NSOLN)
            WD(NSOLN) = 0.E0
            ITEMP = IPIVOT(NSOLN)
            IPIVOT(NSOLN) = IPIVOT(IWMAX)
            IPIVOT(IWMAX) = ITEMP
         ENDIF
C
C        Reduce column NSOLN so that the matrix of nonactive constraints
C        variables is triangular.
C
         DO 320 J = M,NIV+1,-1
            JP = J - 1
C
C           When operating near the ME line, test to see if the pivot
C           element is near zero.  If so, use the largest element above
C           it as the pivot.  This is to maintain the sharp interface
C           between weighted and non-weighted rows in all cases.
C
            IF (J.EQ.ME+1) THEN
               IMAX = ME
               AMAX = SCALE(ME)*W(ME,NSOLN)**2
               DO 310 JP = J - 1,NIV,-1
                  T = SCALE(JP)*W(JP,NSOLN)**2
                  IF (T.GT.AMAX) THEN
                     IMAX = JP
                     AMAX = T
                  ENDIF
  310          CONTINUE
               JP = IMAX
            ENDIF
C
            IF (W(J,NSOLN).NE.0.E0) THEN
               CALL SROTMG (SCALE(JP), SCALE(J), W(JP,NSOLN),
     +                      W(J,NSOLN), SPARAM)
               W(J,NSOLN) = 0.E0
               CALL SROTM (N+1-NSOLN, W(JP,NSOLN+1), MDW, W(J,NSOLN+1),
     +                     MDW, SPARAM)
            ENDIF
  320    CONTINUE
C
C        Solve for Z(NSOLN)=proposed new value for X(NSOLN).  Test if
C        this is nonpositive or too large.  If this was true or if the
C        pivot term was zero, reject the column as dependent.
C
         IF (W(NIV,NSOLN).NE.0.E0) THEN
            ISOL = NIV
            Z2 = W(ISOL,N+1)/W(ISOL,NSOLN)
            Z(NSOLN) = Z2
            POS = Z2 .GT. 0.E0
            IF (Z2*EANORM.GE.BNORM .AND. POS) THEN
               POS = .NOT. (BLOWUP*Z2*EANORM.GE.BNORM)
            ENDIF
C
C           Try to add row ME+1 as an additional equality constraint.
C           Check size of proposed new solution component.
C           Reject it if it is too large.
C
         ELSEIF (NIV.LE.ME .AND. W(ME+1,NSOLN).NE.0.E0) THEN
            ISOL = ME + 1
            IF (POS) THEN
C
C              Swap rows ME+1 and NIV, and scale factors for these rows.
C
               CALL SSWAP (N+1, W(ME+1,1), MDW, W(NIV,1), MDW)
               CALL SSWAP (1, SCALE(ME+1), 1, SCALE(NIV), 1)
               ITEMP = ITYPE(ME+1)
               ITYPE(ME+1) = ITYPE(NIV)
               ITYPE(NIV) = ITEMP
               ME = ME + 1
            ENDIF
         ELSE
            POS = .FALSE.
         ENDIF
C
         IF (.NOT.POS) THEN
            NSOLN = NSOLN - 1
            NIV = NIV - 1
         ENDIF
         IF (.NOT.(POS.OR.DONE)) GO TO 290
      ENDIF
      GO TO 160
C
C     Else perform multiplier test and drop a constraint.  To compute
C     final solution.  Solve system, store results in X(*).
C
C     Copy right hand side into TEMP vector to use overwriting method.
C
  330 ISOL = 1
      IF (NSOLN.GE.ISOL) THEN
         CALL SCOPY (NIV, W(1,N+1), 1, TEMP, 1)
         DO 340 J = NSOLN,ISOL,-1
            IF (J.GT.KRANK) THEN
               I = NIV - NSOLN + J
            ELSE
               I = J
            ENDIF
C
            IF (J.GT.KRANK .AND. J.LE.L) THEN
               Z(J) = 0.E0
            ELSE
               Z(J) = TEMP(I)/W(I,J)
               CALL SAXPY (I-1, -Z(J), W(1,J), 1, TEMP, 1)
            ENDIF
  340    CONTINUE
      ENDIF
C
C     Solve system.
C
      CALL SCOPY (NSOLN, Z, 1, X, 1)
C
C     Apply Householder transformations to X(*) if KRANK.LT.L
C
      IF (KRANK.LT.L) THEN
         DO 350 I = 1,KRANK
            CALL H12 (2, I, KRANK+1, L, W(I,1), MDW, H(I), X, 1, 1, 1)
  350    CONTINUE
      ENDIF
C
C     Fill in trailing zeroes for constrained variables not in solution.
C
      IF (NSOLN.LT.N) CALL SCOPY (N-NSOLN, 0.E0, 0, X(NSOLN+1), 1)
C
C     Permute solution vector to natural order.
C
      DO 380 I = 1,N
         J = I
  360    IF (IPIVOT(J).EQ.I) GO TO 370
         J = J + 1
         GO TO 360
C
  370    IPIVOT(J) = IPIVOT(I)
         IPIVOT(I) = J
         CALL SSWAP (1, X(J), 1, X(I), 1)
  380 CONTINUE
C
C     Rescale the solution using the column scaling.
C
      DO 390 J = 1,N
         X(J) = X(J)*D(J)
  390 CONTINUE
C
      DO 400 I = NSOLN+1,M
         T = W(I,N+1)
         IF (I.LE.ME) T = T/ALAMDA
         T = (SCALE(I)*T)*T
         RNORM = RNORM + T
  400 CONTINUE
C
      RNORM = SQRT(RNORM)
      RETURN
      END
      SUBROUTINE WNLT1 (I, LEND, MEND, IR, MDW, RECALC, IMAX, HBAR, H,
     +   SCALE, W)
C***BEGIN PROLOGUE  WNLT1
C***SUBSIDIARY
C***PURPOSE  Subsidiary to WNLIT
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (WNLT1-S, DWNLT1-D)
C***AUTHOR  Hanson, R. J., (SNLA)
C           Haskell, K. H., (SNLA)
C***DESCRIPTION
C
C     To update the column Sum Of Squares and find the pivot column.
C     The column Sum of Squares Vector will be updated at each step.
C     When numerically necessary, these values will be recomputed.
C
C***SEE ALSO  WNLIT
C***ROUTINES CALLED  ISAMAX
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   890620  Code extracted from WNLIT and made a subroutine.  (RWC))
C***END PROLOGUE  WNLT1
      INTEGER I, IMAX, IR, LEND, MDW, MEND
      REAL             H(*), HBAR, SCALE(*), W(MDW,*)
      LOGICAL RECALC
C
      EXTERNAL ISAMAX
      INTEGER ISAMAX
C
      INTEGER J, K
C
C***FIRST EXECUTABLE STATEMENT  WNLT1
      IF (IR.NE.1 .AND. (.NOT.RECALC)) THEN
C
C        Update column SS=sum of squares.
C
         DO 10 J=I,LEND
            H(J) = H(J) - SCALE(IR-1)*W(IR-1,J)**2
   10    CONTINUE
C
C        Test for numerical accuracy.
C
         IMAX = ISAMAX(LEND-I+1, H(I), 1) + I - 1
         RECALC = (HBAR+1.E-3*H(IMAX)) .EQ. HBAR
      ENDIF
C
C     If required, recalculate column SS, using rows IR through MEND.
C
      IF (RECALC) THEN
         DO 30 J=I,LEND
            H(J) = 0.E0
            DO 20 K=IR,MEND
               H(J) = H(J) + SCALE(K)*W(K,J)**2
   20       CONTINUE
   30    CONTINUE
C
C        Find column with largest SS.
C
         IMAX = ISAMAX(LEND-I+1, H(I), 1) + I - 1
         HBAR = H(IMAX)
      ENDIF
      RETURN
      END
      LOGICAL FUNCTION WNLT2 (ME, MEND, IR, FACTOR, TAU, SCALE, WIC)
C***BEGIN PROLOGUE  WNLT2
C***SUBSIDIARY
C***PURPOSE  Subsidiary to WNLIT
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (WNLT2-S, DWNLT2-D)
C***AUTHOR  Hanson, R. J., (SNLA)
C           Haskell, K. H., (SNLA)
C***DESCRIPTION
C
C     To test independence of incoming column.
C
C     Test the column IC to determine if it is linearly independent
C     of the columns already in the basis.  In the initial tri. step,
C     we usually want the heavy weight ALAMDA to be included in the
C     test for independence.  In this case, the value of FACTOR will
C     have been set to 1.E0 before this procedure is invoked.
C     In the potentially rank deficient problem, the value of FACTOR
C     will have been set to ALSQ=ALAMDA**2 to remove the effect of the
C     heavy weight from the test for independence.
C
C     Write new column as partitioned vector
C           (A1)  number of components in solution so far = NIV
C           (A2)  M-NIV components
C     And compute  SN = inverse weighted length of A1
C                  RN = inverse weighted length of A2
C     Call the column independent when RN .GT. TAU*SN
C
C***SEE ALSO  WNILT
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   890620  Code extracted from WNLIT and made a subroutine.  (RWC))
C***END PROLOGUE  WNLT2
      REAL             FACTOR, SCALE(*), TAU, WIC(*)
      INTEGER IR, ME, MEND
C
      REAL             RN, SN, T
      INTEGER J
C
C***FIRST EXECUTABLE STATEMENT  WNLT2
      SN = 0.E0
      RN = 0.E0
      DO 10 J=1,MEND
         T = SCALE(J)
         IF (J.LE.ME) T = T/FACTOR
         T = T*WIC(J)**2
C
         IF (J.LT.IR) THEN
            SN = SN + T
         ELSE
            RN = RN + T
         ENDIF
   10 CONTINUE
      WNLT2 = RN .GT. SN*TAU**2
      RETURN
      END
      SUBROUTINE WNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
C***BEGIN PROLOGUE  WNLT3
C***SUBSIDIARY
C***PURPOSE  Subsidiary to WNLIT
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (WNLT3-S, DWNLT3-D)
C***AUTHOR  Hanson, R. J., (SNLA)
C           Haskell, K. H., (SNLA)
C***DESCRIPTION
C
C     Perform column interchange.
C     Exchange elements of permuted index vector and perform column
C     interchanges.
C
C***SEE ALSO  WNLIT
C***ROUTINES CALLED  SSWAP
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   890620  Code extracted from WNLT and made a subroutine.  (RWC))
C***END PROLOGUE  WNLT3
      INTEGER I, IMAX, IPIVOT(*), M, MDW
      REAL             H(*), W(MDW,*)
C
      EXTERNAL SSWAP
C
      REAL             T
      INTEGER ITEMP
C
C***FIRST EXECUTABLE STATEMENT  WNLT3
      IF (IMAX.NE.I) THEN
         ITEMP        = IPIVOT(I)
         IPIVOT(I)    = IPIVOT(IMAX)
         IPIVOT(IMAX) = ITEMP
C
         CALL SSWAP(M, W(1,IMAX), 1, W(1,I), 1)
C
         T       = H(IMAX)
         H(IMAX) = H(I)
         H(I)    = T
      ENDIF
      RETURN
      END
      SUBROUTINE WNNLS (W, MDW, ME, MA, N, L, PRGOPT, X, RNORM, MODE,
     +   IWORK, WORK)
C***BEGIN PROLOGUE  WNNLS
C***PURPOSE  Solve a linearly constrained least squares problem with
C            equality constraints and nonnegativity constraints on
C            selected variables.
C***LIBRARY   SLATEC
C***CATEGORY  K1A2A
C***TYPE      SINGLE PRECISION (WNNLS-S, DWNNLS-D)
C***KEYWORDS  CONSTRAINED LEAST SQUARES, CURVE FITTING, DATA FITTING,
C             EQUALITY CONSTRAINTS, INEQUALITY CONSTRAINTS,
C             NONNEGATIVITY CONSTRAINTS, QUADRATIC PROGRAMMING
C***AUTHOR  Hanson, R. J., (SNLA)
C           Haskell, K. H., (SNLA)
C***DESCRIPTION
C
C     Abstract
C
C     This subprogram solves a linearly constrained least squares
C     problem.  Suppose there are given matrices E and A of
C     respective dimensions ME by N and MA by N, and vectors F
C     and B of respective lengths ME and MA.  This subroutine
C     solves the problem
C
C               EX = F, (equations to be exactly satisfied)
C
C               AX = B, (equations to be approximately satisfied,
C                        in the least squares sense)
C
C               subject to components L+1,...,N nonnegative
C
C     Any values ME.GE.0, MA.GE.0 and 0.LE. L .LE.N are permitted.
C
C     The problem is reposed as problem WNNLS
C
C               (WT*E)X = (WT*F)
C               (   A)    (   B), (least squares)
C               subject to components L+1,...,N nonnegative.
C
C     The subprogram chooses the heavy weight (or penalty parameter) WT.
C
C     The parameters for WNNLS are
C
C     INPUT..
C
C     W(*,*),MDW,  The array W(*,*) is double subscripted with first
C     ME,MA,N,L    dimensioning parameter equal to MDW.  For this
C                  discussion let us call M = ME + MA.  Then MDW
C                  must satisfy MDW.GE.M.  The condition MDW.LT.M
C                  is an error.
C
C                  The array W(*,*) contains the matrices and vectors
C
C                       (E  F)
C                       (A  B)
C
C                  in rows and columns 1,...,M and 1,...,N+1
C                  respectively.  Columns 1,...,L correspond to
C                  unconstrained variables X(1),...,X(L).  The
C                  remaining variables are constrained to be
C                  nonnegative. The condition L.LT.0 or L.GT.N is
C                  an error.
C
C     PRGOPT(*)    This real-valued array is the option vector.
C                  If the user is satisfied with the nominal
C                  subprogram features set
C
C                  PRGOPT(1)=1 (or PRGOPT(1)=1.0)
C
C                  Otherwise PRGOPT(*) is a linked list consisting of
C                  groups of data of the following form
C
C                  LINK
C                  KEY
C                  DATA SET
C
C                  The parameters LINK and KEY are each one word.
C                  The DATA SET can be comprised of several words.
C                  The number of items depends on the value of KEY.
C                  The value of LINK points to the first
C                  entry of the next group of data within
C                  PRGOPT(*).  The exception is when there are
C                  no more options to change.  In that
C                  case LINK=1 and the values KEY and DATA SET
C                  are not referenced. The general layout of
C                  PRGOPT(*) is as follows.
C
C               ...PRGOPT(1)=LINK1 (link to first entry of next group)
C               .  PRGOPT(2)=KEY1 (key to the option change)
C               .  PRGOPT(3)=DATA VALUE (data value for this change)
C               .       .
C               .       .
C               .       .
C               ...PRGOPT(LINK1)=LINK2 (link to the first entry of
C               .                       next group)
C               .  PRGOPT(LINK1+1)=KEY2 (key to the option change)
C               .  PRGOPT(LINK1+2)=DATA VALUE
C               ...     .
C               .       .
C               .       .
C               ...PRGOPT(LINK)=1 (no more options to change)
C
C                  Values of LINK that are nonpositive are errors.
C                  A value of LINK.GT.NLINK=100000 is also an error.
C                  This helps prevent using invalid but positive
C                  values of LINK that will probably extend
C                  beyond the program limits of PRGOPT(*).
C                  Unrecognized values of KEY are ignored.  The
C                  order of the options is arbitrary and any number
C                  of options can be changed with the following
C                  restriction.  To prevent cycling in the
C                  processing of the option array a count of the
C                  number of options changed is maintained.
C                  Whenever this count exceeds NOPT=1000 an error
C                  message is printed and the subprogram returns.
C
C                  OPTIONS..
C
C                  KEY=6
C                         Scale the nonzero columns of the
C                  entire data matrix
C                  (E)
C                  (A)
C                  to have length one. The DATA SET for
C                  this option is a single value.  It must
C                  be nonzero if unit length column scaling is
C                  desired.
C
C                  KEY=7
C                         Scale columns of the entire data matrix
C                  (E)
C                  (A)
C                  with a user-provided diagonal matrix.
C                  The DATA SET for this option consists
C                  of the N diagonal scaling factors, one for
C                  each matrix column.
C
C                  KEY=8
C                         Change the rank determination tolerance from
C                  the nominal value of SQRT(SRELPR).  This quantity
C                  can be no smaller than SRELPR, The arithmetic-
C                  storage precision.  The quantity used
C                  here is internally restricted to be at
C                  least SRELPR.  The DATA SET for this option
C                  is the new tolerance.
C
C                  KEY=9
C                         Change the blow-up parameter from the
C                  nominal value of SQRT(SRELPR).  The reciprocal of
C                  this parameter is used in rejecting solution
C                  components as too large when a variable is
C                  first brought into the active set.  Too large
C                  means that the proposed component times the
C                  reciprocal of the parameter is not less than
C                  the ratio of the norms of the right-side
C                  vector and the data matrix.
C                  This parameter can be no smaller than SRELPR,
C                  the arithmetic-storage precision.
C
C                  For example, suppose we want to provide
C                  a diagonal matrix to scale the problem
C                  matrix and change the tolerance used for
C                  determining linear dependence of dropped col
C                  vectors.  For these options the dimensions of
C                  PRGOPT(*) must be at least N+6.  The FORTRAN
C                  statements defining these options would
C                  be as follows.
C
C                  PRGOPT(1)=N+3 (link to entry N+3 in PRGOPT(*))
C                  PRGOPT(2)=7 (user-provided scaling key)
C
C                  CALL SCOPY(N,D,1,PRGOPT(3),1) (copy the N
C                  scaling factors from a user array called D(*)
C                  into PRGOPT(3)-PRGOPT(N+2))
C
C                  PRGOPT(N+3)=N+6 (link to entry N+6 of PRGOPT(*))
C                  PRGOPT(N+4)=8 (linear dependence tolerance key)
C                  PRGOPT(N+5)=... (new value of the tolerance)
C
C                  PRGOPT(N+6)=1 (no more options to change)
C
C
C     IWORK(1),    The amounts of working storage actually allocated
C     IWORK(2)     for the working arrays WORK(*) and IWORK(*),
C                  respectively.  These quantities are compared with
C                  the actual amounts of storage needed for WNNLS( ).
C                  Insufficient storage allocated for either WORK(*)
C                  or IWORK(*) is considered an error.  This feature
C                  was included in WNNLS( ) because miscalculating
C                  the storage formulas for WORK(*) and IWORK(*)
C                  might very well lead to subtle and hard-to-find
C                  execution errors.
C
C                  The length of WORK(*) must be at least
C
C                  LW = ME+MA+5*N
C                  This test will not be made if IWORK(1).LE.0.
C
C                  The length of IWORK(*) must be at least
C
C                  LIW = ME+MA+N
C                  This test will not be made if IWORK(2).LE.0.
C
C     OUTPUT..
C
C     X(*)         An array dimensioned at least N, which will
C                  contain the N components of the solution vector
C                  on output.
C
C     RNORM        The residual norm of the solution.  The value of
C                  RNORM contains the residual vector length of the
C                  equality constraints and least squares equations.
C
C     MODE         The value of MODE indicates the success or failure
C                  of the subprogram.
C
C                  MODE = 0  Subprogram completed successfully.
C
C                       = 1  Max. number of iterations (equal to
C                            3*(N-L)) exceeded. Nearly all problems
C                            should complete in fewer than this
C                            number of iterations. An approximate
C                            solution and its corresponding residual
C                            vector length are in X(*) and RNORM.
C
C                       = 2  Usage error occurred.  The offending
C                            condition is noted with the error
C                            processing subprogram, XERMSG( ).
C
C     User-designated
C     Working arrays..
C
C     WORK(*)      A real-valued working array of length at least
C                  M + 5*N.
C
C     IWORK(*)     An integer-valued working array of length at least
C                  M+N.
C
C***REFERENCES  K. H. Haskell and R. J. Hanson, An algorithm for
C                 linear least squares problems with equality and
C                 nonnegativity constraints, Report SAND77-0552, Sandia
C                 Laboratories, June 1978.
C               K. H. Haskell and R. J. Hanson, Selected algorithms for
C                 the linearly constrained least squares problem - a
C                 users guide, Report SAND78-1290, Sandia Laboratories,
C                 August 1979.
C               K. H. Haskell and R. J. Hanson, An algorithm for
C                 linear least squares problems with equality and
C                 nonnegativity constraints, Mathematical Programming
C                 21 (1981), pp. 98-118.
C               R. J. Hanson and K. H. Haskell, Two algorithms for the
C                 linearly constrained least squares problem, ACM
C                 Transactions on Mathematical Software, September 1982.
C               C. L. Lawson and R. J. Hanson, Solving Least Squares
C                 Problems, Prentice-Hall, Inc., 1974.
C***ROUTINES CALLED  WNLSM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790701  DATE WRITTEN
C   890206  REVISION DATE from Version 3.2
C   890618  Completely restructured and revised.  (WRB & RWC)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900510  Convert XERRWV calls to XERMSG calls.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  WNNLS
      REAL              PRGOPT(*), RNORM, W(MDW,*), WORK(*), X(*)
      INTEGER IWORK(*)
      CHARACTER*8 XERN1
C
C
C***FIRST EXECUTABLE STATEMENT  WNNLS
      MODE = 0
      IF (MA+ME.LE.0 .OR. N.LE.0) RETURN
      IF (IWORK(1).GT.0) THEN
         LW = ME + MA + 5*N
         IF (IWORK(1).LT.LW) THEN
            WRITE (XERN1, '(I8)') LW
            CALL XERMSG ('SLATEC', 'WNNLS', 'INSUFFICIENT STORAGE ' //
     *         'ALLOCATED FOR WORK(*), NEED LW = ' // XERN1, 2, 1)
            MODE = 2
            RETURN
         ENDIF
      ENDIF
C
      IF (IWORK(2).GT.0) THEN
         LIW = ME + MA + N
         IF (IWORK(2).LT.LIW) THEN
            WRITE (XERN1, '(I8)') LIW
            CALL XERMSG ('SLATEC', 'WNNLS', 'INSUFFICIENT STORAGE ' //
     *         'ALLOCATED FOR IWORK(*), NEED LIW = ' // XERN1, 2, 1)
            MODE = 2
            RETURN
         ENDIF
      ENDIF
C
      IF (MDW.LT.ME+MA) THEN
         CALL XERMSG ('SLATEC', 'WNNLS',
     *      'THE VALUE MDW.LT.ME+MA IS AN ERROR', 1, 1)
         MODE = 2
         RETURN
      ENDIF
C
      IF (L.LT.0 .OR. L.GT.N) THEN
         CALL XERMSG ('SLATEC', 'WNNLS',
     *      'L.GE.0 .AND. L.LE.N IS REQUIRED', 2, 1)
         MODE = 2
         RETURN
      ENDIF
C
C     THE PURPOSE OF THIS SUBROUTINE IS TO BREAK UP THE ARRAYS
C     WORK(*) AND IWORK(*) INTO SEPARATE WORK ARRAYS
C     REQUIRED BY THE MAIN SUBROUTINE WNLSM( ).
C
      L1 = N + 1
      L2 = L1 + N
      L3 = L2 + ME + MA
      L4 = L3 + N
      L5 = L4 + N
C
      CALL WNLSM(W, MDW, ME, MA, N, L, PRGOPT, X, RNORM, MODE, IWORK,
     *           IWORK(L1), WORK(1), WORK(L1), WORK(L2), WORK(L3),
     *           WORK(L4), WORK(L5))
      RETURN
      END
C*****END precision > single
