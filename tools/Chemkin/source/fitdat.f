      PROGRAM THMFIT
C
      PARAMETER (IDIM=102, JDIM=11, IP1=200, IP2=22, NCP=5,
     *           R=1.987, XPAGE=7.5, YPAGE=9.5, XINCH=3.5, NPTS=100,
     *           YINCH=2.5, XORG=2.0, YORG=6.75, DELTX=0.0, DELTY=-3.0,
     *           I1=1, I2=2, I3=3, I4=4, LOUT=6, LIN=5)
C
      DIMENSION IST(6), T(IP1), CP(IP1), S(IP1), H(IP1), X(10),
     *          W(IDIM,JDIM), PRGOPT(13), TP(IP1), CPP(IP1), Y(2),
     *          Z(2), HP(IP1), SP(IP1), IP(IP2), WS(IP1), ILEG(100)
C
      CHARACTER IEL(6)*2, KSYM*45, KNAME*45
      LOGICAL IEND, IERR, IPLOT
      DATA (PRGOPT(I),I=1,4) /4,2,1.0,1/, KDATE/0/,
     *      IEND,IERR,IPLOT/3*.FALSE./
C----------------------------------------------------------------------C
C     Program THRMFIT fits thermodynamic species data, T, CP, S, and   C
C     H arrays obtained from a formatted data file.                    C
C     Output to UNIT7 is the NASA-formatted input required by CHEMKIN. C
C                                                                      C
C     The constrained least squares problem is done with the code LSEI.C
C     The primary input is put into the matrix W(L,NN).  The first     C
C     NPAR columns of W are the coefficients to the parameters for the C
C     least squares problem AX=B, where A is the coefficient matrix,   C
C     and X is the parameter vector. The last column of W is the       C
C     right hand side vector, B.  The first row is for the equality    C
C     constraints, and the remaining L rows are for the least squares  C
C     problem.  If only one temperature range is used, then there are  C
C     no equality constraints. We never use any inequality constraints.C
C                                                                      C
C     LSEI requires length of W(*,*) to be (IDIM,NPAR+1),              C
C                             X(*)   to be (NPAR),                     C
C                             WS(*)  to be (2*(ME+NPAR)                C
C                                           + MAX(MA+MG,NPAR)          C
C                                           + (MG+2)*(NPAR+7))         C
C                             IP(*)  to be (MG + 2*NPAR + 2)           C
C                   and IP(1) to be length of WS,                      C
C                       IP(2) to be length of IP                       C
C----------------------------------------------------------------------C
C*****vax
C      CALL IDATE (IMO,IDA,IYR)
C      KDATE = IMO*10000 + IDA*100 + IYR
C*****END vax
C
   50 CONTINUE
C        Initialize arrays
      KSYM = ' '
      KNAME = ' '
      II = 0
      DO 25 L=1,6
         IST(L) = 0
         IEL(L) = ' '
   25 CONTINUE
      H298 = 0.0
      DO 35 L=1,IP1
         T(I) = 0.0
         CP(I) = 0.0
         H(I) = 0.0
         S(I) = 0.0
   35 CONTINUE
C
C     Get data for fitting
      CALL GETDAT (LIN, LOUT, R, KSYM, KNAME, IEL, IST, TBREAK,
     1             H298, II, T, CP, S, H, IEND, IERR)
C
      IF (II.LE.0 .OR. IERR) THEN
C*****graphics
C         IF (IPLOT) CALL DONEPL
C*****END graphics
         STOP
      ENDIF
      H0 = H298
C----------------------------------------------------------------------C
C      LEAST SQUARES PROBLEM FOR CP/R                                  C
C      Determine the number of parameters for the CP fit               C
C      NN - number of columns of W(L,*), W(L,NN) is the last column    C
C      of W, and hence where the right hand side vector is stored.     C
C      The first NN-1 columns contain coefficients of the parameters.  C
C      For the normal case NN=10, 5 parameters for CP in the low       C
C      temperature range and 5 for the high.  When only one            C
C      temperature range is used, NN=5, since only the 5 CP parameters C
C      are used.                                                       C
C      L - row counter for W(L,*)                                      C
C----------------------------------------------------------------------C
      CALL MINMAX (CP, II, CMIN, CMAX)
      IF (CMIN .EQ. CMAX) THEN
C
C        CP IS CONSTANT FOR KR, AR, NE, ETC.
C
         DO 101 N=1,10
            X(N) = 0.0
  101    CONTINUE
          X(1) = CP(1)
          X(6) = X(1)
C
      ELSE
         DO 95 I=1,IDIM
            DO 95 J=1,JDIM
               W(I,J) = 0.0
   95    CONTINUE
C
         IF (TBREAK .LT. T(II)) THEN   ! Fit two temperature ranges
            NN = NCP*2 + 1
            L = 2
            DO 500 I=1,II
               IF (T(I) .LE. TBREAK) THEN   ! Low temperature range
                  L = L + 1
                  DO 100 J=1,NCP
                     W(L,J) = T(I)**(J-1)
  100             CONTINUE
                  W(L,NN) = CP(I)
               ENDIF
               IF (T(I) .GE. TBREAK) THEN
                  L = L + 1
                  DO 400 J=1,NCP
                     W(L,J+NCP) = T(I)**(J-1)
  400             CONTINUE
                  W(L,NN) = CP(I)
               ENDIF
  500       CONTINUE
C
            DO 600 J=1,NCP
               W(1,J)     = TBREAK**(J-1)
               W(2,J)     = (J-1)*TBREAK**(J-2)
               W(1,J+NCP) = -W(1,J)
               W(2,J+NCP) = -W(2,J)
  600       CONTINUE
            W(1,NN) = 0.
            W(2,NN) = 0.
            ME   = 2              ! Number of equality constraints
            MA   = L-2         ! Number of least squares equations
            MG   = 0            ! Number of inequality constraints
            NPAR = 10
C
         ELSE                          ! Fit one temperature range
            NN = NCP+1
            L  = 0
            DO 800 I=1,II
               L = L + 1
               W(L,NN) = CP(I)
               DO 800 J=1,NCP
                  W(L,J) = T(I)**(J-1)
  800       CONTINUE
            ME   = 0              ! Number of equality constraints
            MA   = L           ! Number of least squares equations
            MG   = 0            ! Number of inequality constraints
            NPAR = 5
         ENDIF
C
         IP(1) = IP1
         IP(2) = IP2
         CALL LSEI (W, IDIM, ME, MA, MG, NPAR, PRGOPT, X, RNORME,
     1              RNORML, MODE, WS, IP)
         IF (MODE .NE. 0) THEN
            PRINT 7020,MODE
            STOP
         ENDIF
      ENDIF
C
C----------------------------------------------------------------------C
C     DO THE H FIT                                                     C
C     For both the H and S fits, the number of parameters is only two  C
C     (1 for the 1 temperature range case).  We hold the 10 (5) CP     C
C     parameters fixed and determine the best fit for the constants of C
C     integration, A6, "formation enthalpy",                           C
C     and A7, "formation entropy"                                      C
C----------------------------------------------------------------------C
      DO 1000 I=1,IDIM
         DO 1000 J=1,JDIM
            W(I,J) = 0.0
 1000 CONTINUE
C
      IF (TBREAK .LT. T(II)) THEN        ! Fit two temperature ranges
         L    = 1
         NPAR = 2
C
         DO 1100 I=1,II
            T2 = T(I)**2/2.0
            T3 = T(I)**3/3.0
            T4 = T(I)**4/4.0
            T5 = T(I)**5/5.0
            IF (T(I) .LE. TBREAK) THEN        ! Low temperature range
               L = L + 1
               W(L,1) = 1.0
               W(L,NPAR+1) = H(I) - (X(1)*T(I) + X(2)*T2
     1                     + X(3)*T3 + X(4)*T4 + X(5)*T5)
            ENDIF
            IF (T(I) .GE. TBREAK) THEN
               L = L + 1
               W(L,2) = 1.0               ! High temperature range
               W(L,NPAR+1) = H(I) - (X(6)*T(I) + X(7)*T2
     1                     + X(8)*T3 + X(9)*T4 + X(10)*T5)
            ENDIF
 1100    CONTINUE
C
         W(1,1) = 1.0
         W(1,2) = -1.0
         W(1,3) = 0.0
         DO 1200 J=1,NCP
            W(1,3) = W(1,3) + (X(J+NCP)-X(J))*TBREAK**J / J
 1200    CONTINUE
         ME = 1                ! Number of equality constraints
         MA = L-1           ! Number of least squares equations
         MG = 0              ! Number of inequality constraints
C
      ELSE                          ! Fit one temperature range
         L    = 0
         NPAR = 1
         DO 1300 I=1,II
            L = L + 1
            W(L,1) = 1.0
            W(L,NPAR+1) = H(I) - (X(1)*T(I) + 0.5*X(2)*T(I)**2
     1                  + X(3)/3.0*T(I)**3 + X(4)/4.*T(I)**4
     2                  + X(5)/5.0*T(I)**5)
 1300    CONTINUE
         ME = 0                ! Number of equality constraints
         MA = L             ! Number of least squares equations
         MG = 0              ! Number of inequality constraints
      ENDIF
C
      IP(1) = IP1
      IP(2) = IP2
      CALL LSEI (W, IDIM, ME, MA, MG, NPAR, PRGOPT, Y, RNORME,
     1           RNORML, MODE, WS, IP)
      IF (MODE .NE. 0) THEN
         PRINT 7020,MODE
         STOP
      ENDIF
C----------------------------------------------------------------------C
C        DO THE S FIT                                                  C
C----------------------------------------------------------------------C
      DO 2000 I=1,IDIM
         DO 2000 J=1,JDIM
            W(I,J) = 0.0
 2000 CONTINUE
C
      IF (TBREAK .LT. T(II)) THEN  ! Fit two temperature ranges
         L    = 1
         NPAR = 2
C
         DO 2100 I=1,II
            T2 = T(I)**2/2.0
            T3 = T(I)**3/3.0
            T4 = T(I)**4/4.0
            IF (T(I) .LE. TBREAK) THEN  ! Low temperature range
               L  = L + 1
               W(L,1) = 1.0
               W(L,NPAR+1) = S(I) - (X(1)*ALOG(T(I)) + X(2)*T(I)
     1                     + X(3)*T2 + X(4)*T3 + X(5)*T4)
            ENDIF
            IF (T(I) .GE. TBREAK) THEN
               L  = L + 1
               W(L,2) = 1.0               ! High temperature range
               W(L,NPAR+1) = S(I) - (X(6)*ALOG(T(I)) + X(7)*T(I)
     1                     + X(8)*T2 + X(9)*T3 + X(10)*T4)
            ENDIF
 2100    CONTINUE
C
         W(1,1) = 1.0
         W(1,2) = -1.0
         W(1,3) = (X(6)-X(1))*ALOG(TBREAK)
         DO 2200 J=2,NCP
            W(1,3) = W(1,3) +(X(J+NCP)-X(J))*TBREAK**(J-1)/(J-1)
 2200    CONTINUE
         ME   = 1              ! Number of equality constraints
         MA   = L-1         ! Number of least squares equations
         MG   = 0            ! Number of inequality constraints
      ELSE                             ! High temperature range
         L    = 0
         NPAR = 1
         DO 2300 I=1,II
            L      = L + 1
            W(L,1) = 1.0
            W(L,NPAR+1) = S(I) - (X(1)*ALOG(T(I)) + X(2)*T(I)
     1                  + X(3)/2.0*T(I)**2 + X(4)/3.0*T(I)**3
     2                  + X(5)/4.0*T(I)**4)
 2300    CONTINUE
         ME   = 0              ! Number of equality constraints
         MA   = L           ! Number of least squares equations
         MG   = 0            ! Number of inequality constraints
      ENDIF
C
      IP(1) = IP1
      IP(2) = IP2
      CALL LSEI (W, IDIM, ME, MA, MG, NPAR, PRGOPT, Z, RNORME,
     1           RNORML, MODE, WS, IP)
      IF (MODE .NE. 0) THEN
         PRINT 7020,MODE
         STOP
      ENDIF
C
      IF (TBREAK .GE. T(II)) THEN  ! One temperature range only
         Y(2) = 0.
         Z(2) = 0.
         DO 3000 N=1,5
            X(N+5) = 0.
 3000    CONTINUE
         T2 = T(II)**2
         T3 = T(II)**3
         T4 = T(II)**4
         T5 = T(II)**5
         X(6) = X(1) + X(2)*T(II) + X(3)*T2 + X(4)*T3 + X(5)*T4
         Y(2) = (X(1)-X(6))*T(II) + X(2)*T2/2.0 + X(3)*T3/3.0
     1        + X(4)*T4/4.0 + X(5)*T5/5.0 + Y(1)
         Z(2) = (X(1)-X(6))*ALOG(T(II)) + X(2)*T(II)
     1        + X(3)*T2/2.0 + X(4)*T3/3.0 + X(5)*T4/4.0 + Z(1)
      ENDIF
C
C     WRITE FIT COEFFICIENTS
C
      WRITE (LOUT,8050) KSYM(:12),KDATE,(IEL(L)(:2),IST(L),L=1,4),
     1                  'G',T(1),T(II),TBREAK,IEL(5)(:2),IST(5),I1
      WRITE (LOUT,8060) X(6), X(7), X(8), X(9), X(10), I2
      WRITE (LOUT,8060) Y(2), Z(2), X(1), X(2), X(3), I3
      WRITE (LOUT,8070) X(4), X(5), Y(1), Z(1), I4
C
C*****graphics
CC
C      DT = (T(II)-T(1))/(NPTS-1)
C      MAD = 0
C      LAD = 0
CC
C      DO 2500 M=1,NPTS
C         TP(M) = T(1) + DT*(M-1)
C         TP2 = TP(M)**2
C         TP3 = TP(M)**3
C         TP4 = TP(M)**4
C         TP5 = TP(M)**5
C         IF (TBREAK.LT.T(II) .AND. TP(M).GT.TBREAK) THEN
C            LAD = 5
C            MAD = 1
C         ENDIF
C         CPP(M) = X(1+LAD) + X(2+LAD)*TP(M) + X(3+LAD)*TP2
C     1                     + X(4+LAD)*TP3   + X(5+LAD)*TP4
C         HP(M)  = X(1+LAD) + X(2+LAD)*TP(M)/2.0
C     1          + X(3+LAD)*TP2/3.0  + X(4+LAD)*TP3/4.0
C     2          + X(5+LAD)*TP4/5.0  + Y(1+MAD)/TP(M)
C         SP(M)  = X(1+LAD)*ALOG(TP(M)) + X(2+LAD)*TP(M)
C     1          + X(3+LAD)*TP2/2.0  + X(4+LAD)*TP3/3.0
C     2          + X(5+LAD)*TP4/4.0  + Z(1+MAD)
C 2500 CONTINUE
CC
C      IF (.NOT. IPLOT) THEN
C         CALL STRTPL                       ! Initialize DISSPLA
C         CALL SETDEV (0, 0)
C         CALL HWROT ('MOVIE')
C         CALL NOBRDR
C         CALL DUPLX
C         CALL BASALF ('STANDARD')
C         CALL PAGE   (XPAGE, YPAGE)
C         CALL HEIGHT (0.15)
C         CALL YAXANG (0.0)
C         CALL INTAXS
C         CALL MYLEGN ('$',100)
C         CALL LEGLIN
C         IPLOT = .TRUE.
C      ENDIF
CC
C      CALL PHYSOR  (XORG, YORG)                ! Plot CP/R vs T
C      CALL COMPLOT (XINCH,YINCH,' ','CP/R',T,CP,II,TP,CPP,NPTS)
C      IL = ILAST(KSYM)
C      CALL MESSAG  (%REF(KSYM), IL, 0.0, YINCH+.1)
C      XPOS = XMESS (%REF(KSYM), IL) + .2
C      IL = ILAST(KNAME)
C      CALL MESSAG (%REF(KNAME), IL, XPOS, YINCH+.1)
C      CALL LINES  ('Data$', ILEG, 1)
C      CALL LINES  ('Fit Coefficients$', ILEG, 2)
C      XPOS = XINCH - XLEGND (ILEG,2) - .1
C      IF (T(II) .LT. T(1)) THEN
C         YPOS = YINCH - YLEGND (ILEG,2)
C      ELSE
C         YPOS = YLEGND (ILEG,2)
C      ENDIF
C      CALL LEGEND (ILEG, 2, XPOS, YPOS)
C      CALL ENDGR  (0)
CC
C      DO 2600 I=1,II                          ! Plot H/RT vs T
C         H(I) = H(I)/T(I)
C 2600 CONTINUE
CC
C      CALL OREL    (DELTX, DELTY)
C      CALL COMPLOT (XINCH,YINCH,' ','H/RT',T,H,II,TP,HP,NPTS)
C      CALL ENDGR   (0)
CC
C      CALL OREL    (DELTX, DELTY)                ! Plot S/R vs T
C      CALL COMPLOT (XINCH,YINCH,'TEMPERATURE','S/R',T,S,II,TP,SP,NPTS)
C      CALL RESET   ('PHYSOR')
C      CALL ENDPL   (0)
C*****END graphics
C
      IF (IEND) THEN
C*****graphics
C         IF (IPLOT) CALL DONEPL
C*****END graphics
         STOP
      ENDIF
C
      GO TO 50
C
 7020 FORMAT(10X,'ERROR IN LSEI, MODE= ',I4)
 8050 FORMAT (A12,6X,I6,4(A2,I3),A1,2F10.3,F8.2,A2,I3,I2)
 8060 FORMAT (5E15.8,I5)
 8070 FORMAT (4E15.8,I20)
      END
C----------------------------------------------------------------------C
      SUBROUTINE GETDAT (LIN, LOUT, R, KSYM, KNAME, IEL, IST, TBREAK,
     1                   H298, II, T, CP, S, H, IEND, IERR)
      DIMENSION IST(1), T(1), CP(1), S(1), H(1)
      CHARACTER KSYM*(*), KNAME*(*), IEL(1)*(*), ISTR*10, FMT(5)*4
      CHARACTER LINE*80
      LOGICAL IEND,IERR
      DATA FMT /'(I1)','(I2)','(I3)','(I4)','(I5)'/
C
      II = 0
      READ (LIN, '(A)', END=500) LINE
      IN = INDEX(LINE,' ')
      KSYM = LINE(:IN-1)
      KNAME = LINE(IN+1:)
      L = 1
C
   75 CONTINUE
      IF (IEL(L)(:1) .NE. '*') THEN
         READ (LIN, '(A2,A10)', END=500) IEL(L), ISTR
         IF (IEL(L)(:1) .NE. '*') THEN
            DO 10 N=1,LEN(ISTR)
               IF (ISTR(N:N).NE.' ') ILEN=N
   10       CONTINUE
C            DECODE (10,FMT(ILEN),ISTR) IST(L)
            READ (ISTR, FMT(ILEN)) IST(L)
            L = L + 1
         ENDIF
         GO TO 75
      ENDIF
      IEL(L) = ' '
C
      READ (LIN, *, END=500) TBREAK
      READ (LIN, *, END=500) H298                ! Enthalpy @298.
      DO 25 M=1,200
         READ (LIN, *, END=80, ERR=77) TDUM, CPDUM, SDUM, HDUM
         IF (TDUM .LE. 0.0) RETURN
         IF (TDUM.GE.300 .AND. TDUM .LE.4000) THEN
            II = II + 1
            T(II) = TDUM
            CP(II) = CPDUM/R
            H(II) = (HDUM+H298)*1000./R
            S(II) = SDUM/R
         ENDIF
   25 CONTINUE
   77 CONTINUE
      WRITE (LOUT,'(A,A)')
     1' Error Reading Data, Species name: ',KSYM
      IERR = .TRUE.
   80 CONTINUE
      RETURN
  500 CONTINUE
      IEND = .TRUE.
      RETURN
      END
C
      SUBROUTINE MINMAX(V,NN,VMIN,VMAX)
      DIMENSION V(1)
      VMIN=V(1)
      VMAX=V(1)
      DO 100 N=1,NN
         VMAX=AMAX1(V(N),VMAX)
         VMIN=AMIN1(V(N),VMIN)
  100 CONTINUE
      RETURN
      END
C
C*****graphics
CC
C      SUBROUTINE COMPLOT (XLEN,YLEN,XLAB,YLAB,XD,YD,ND,X,Y,NN)
C      DIMENSION X(1),Y(1),XD(1),YD(1)
C      CHARACTER*(*) XLAB,YLAB
CC
C      CALL AREA2D (XLEN, YLEN)
C      IYL = MAX(ILAST(YLAB),1)
C      IXL = MAX(ILAST(XLAB),1)
C      CALL YNAME  (%REF(YLAB),IYL)
C      CALL XNAME  (%REF(XLAB),IXL)
C      CALL MINMAX (Y, NN, YMIN, YMAX)
C      CALL MINMAX (YD,ND,YDMIN,YDMAX)
C      YMIN = MIN(YMIN,YDMIN)
C      YMAX = MAX(YMAX,YDMAX)
C      IF (YMIN .EQ. YMAX) THEN
C         YMIN = YMIN - .1*YMIN
C         YMAX = YMAX + .1*YMAX
C      ENDIF
C      CALL MINMAX (X, NN, XMIN, XMAX)
C      CALL GRAF   (XMIN, 'SCALE', XMAX, YMIN, 'SCALE', YMAX)
C      CALL CURVE  (XD,YD,ND,-2)
C      CALL CURVE  (X,Y,NN,0)
C      CALL FRAME
C      RETURN
C      END
C*****END graphics
C
      INTEGER FUNCTION ILAST (ISTR)
      CHARACTER*(*) ISTR
C
      ILAST = 0
      DO 10 I=LEN(ISTR),1,-1
         IF (ISTR(I:) .NE. ' ') THEN
            ILAST = I
            RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END
