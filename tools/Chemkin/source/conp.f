      PROGRAM CONP
C
C     Integration of adiabatic, constant pressure kinetics problems
C
C     VERSION 1.2:
C     1.  Implement new VODE solver
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER(I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      PARAMETER (LENIWK=40000, LENRWK=40000, LENCWK=500, NK=5, NLMAX=55,
     1           LIN=5, LOUT=6, LINKCK=25, KMAX=500, ITOL=1, IOPT=0,
     2           RTOL=1.0E-6, ITASK=1, ATOL=1.0E-15)
C
      DIMENSION IWORK(LENIWK), RWORK(LENRWK), X(KMAX), Z(KMAX)
      CHARACTER CWORK(LENCWK)*16, KSYM(KMAX)*16, LINE*80
      LOGICAL KERR, IERR
      EXTERNAL FUN
C
      COMMON /RCONS/ P, RU
      COMMON /ICONS/ KK, NWT, NH, NWDOT
C
      DATA KERR/.FALSE./, X/KMAX*0.0/, KSYM/KMAX*' '/
C
      WRITE (LOUT, 15)
   15 FORMAT (
     1/' CONP:  CHEMKIN-II Version 1.2, Aug. 1992',
C*****precision > double
     2/'        DOUBLE PRECISION')
C*****END precision > double
C*****precision > single
C     2/'        SINGLE PRECISION')
C*****END precision > single
C
C     Initialize CHEMKIN
C
      OPEN (LINKCK, FORM='UNFORMATTED', STATUS='UNKNOWN',
     1              FILE='chem.bin')
      CALL CKLEN  (LINKCK, LOUT, LENI, LENR, LENC)
      CALL CKINIT (LENIWK, LENRWK, LENCWK, LINKCK, LOUT, IWORK,
     1             RWORK, CWORK)
      CLOSE (LINKCK)
      CALL CKINDX (IWORK, RWORK, MM, KK, II, NFIT)
C
      NEQ   = KK + 1
      LRW   = 22 + 9*NEQ + 2*NEQ**2
      NVODE = LENR + 1
      NWT   = NVODE + LRW
      NH    = NWT  + KK
      NWDOT = NH   + KK
      NTOT  = NWDOT+ KK - 1
C
      LIW   = 30 + NEQ
      IVODE = LENI + 1
      ITOT  = IVODE + LIW - 1
C
      IF (KK .GT. KMAX) THEN
         WRITE (LOUT, *)
     1   ' Error...KMAX too small...must be at least ',KK
         KERR = .TRUE.
      ENDIF
C
      IF (LENRWK .LT. NTOT) THEN
         KERR = .TRUE.
         WRITE (LOUT, *)
     1   ' Error...LENRWK too small...must be at least', NTOT
      ENDIF
C
      IF (LENIWK .LT. ITOT) THEN
         KERR = .TRUE.
         WRITE (LOUT, *)
     1   ' Error...LENIWK too small...must be at least', ITOT
      ENDIF
C
      IF (KERR) STOP
C
      CALL CKSYMS (CWORK, LOUT, KSYM, IERR)
      IF (IERR) KERR = .TRUE.
      CALL CKWT   (IWORK, RWORK, RWORK(NWT))
      CALL CKRP   (IWORK, RWORK, RU, RUC, PATM)
C
C     Pressure and temperature
C
      WRITE (LOUT, '(/A)') ' ADIABATIC FIXED PRESSURE PROBLEM'
      WRITE (LOUT, '(/A)') ' INPUT PRESSURE(ATM) AND TEMPERATURE(K)'
      READ  (LIN,    *) PA, T
      WRITE (LOUT,7105) PA, T
      P = PA*PATM
C
C     Initial non-zero moles
C
   40 CONTINUE
      LINE = ' '
      WRITE (LOUT, '(/A)') ' INPUT MOLES OF NEXT SPECIES'
      READ  (LIN,  '(A)', END=45)   LINE
      WRITE (LOUT, '(1X,A)') LINE
      ILEN = INDEX (LINE, '!')
      IF (ILEN .EQ. 1) GO TO 40
C
      ILEN = ILEN - 1
      IF (ILEN .LE. 0) ILEN = LEN(LINE)
      IF (INDEX(LINE(:ILEN), 'END') .EQ. 0) THEN
         IF (LINE(:ILEN) .NE. ' ') THEN
            CALL CKSNUM (LINE(:ILEN), 1, LOUT, KSYM, KK, KNUM,
     1                   NVAL, VAL, IERR)
            IF (IERR) THEN
               WRITE (LOUT,*) ' Error reading moles...'
               KERR = .TRUE.
            ELSE
               X(KNUM) = VAL
            ENDIF
         ENDIF
         GO TO 40
      ENDIF
C
   45 CONTINUE
C
C     Final time and print interval
C
      WRITE (LOUT, '(/A)') ' INPUT FINAL TIME AND DT'
      READ  (LIN,    *) T2, DT
      WRITE (LOUT,7105) T2, DT
C
      IF (KERR) STOP
C
C     Normalize the mole fractions
C
      XTOT = 0.00
      DO 50 K = 1, KK
         XTOT = XTOT + X(K)
   50 CONTINUE
      DO 55 K = 1, KK
         X(K) = X(K) / XTOT
   55 CONTINUE
C
C     Initial conditions and mass fractions
C
      TT1  = 0.0
      Z(1) = T
      CALL CKXTY (X, IWORK, RWORK, Z(2))
C
C     Integration control parameters for VODE
C
      TT2   = TT1
      MF = 22
      ISTATE= 1
      NLINES=NLMAX + 1
C
C     Integration loop
C
  250 CONTINUE
      IF (NLINES .GE. NLMAX) THEN
C
C        Print page heading
C
         WRITE (LOUT, 7003)
         WRITE (LOUT, 7100) (KSYM(K)(:10), K=1,MIN(NK,KK))
         NLINES = 1
C
         DO 200 K1 = NK+1, KK, NK
            WRITE (LOUT, 7110) (KSYM(K)(:10), K=K1, MIN(K1+NK-1, KK))
            NLINES = NLINES + 1
  200    CONTINUE
      ENDIF
C
C     Print the solution
C
      T = Z(1)
      CALL CKYTX (Z(2), IWORK, RWORK, X)
C
      WRITE (LOUT, 7105) TT1, T, (X(K), K=1,MIN(NK,KK))
      NLINES = NLINES + 1
C
      DO 300 K1 = NK+1, KK, NK
         WRITE (LOUT, 7115) (X(K), K=K1, MIN(K1+NK-1,KK))
         NLINES = NLINES + 1
  300 CONTINUE
C
      IF (TT2 .GE. T2) STOP
      TT2 = MIN(TT2 + DT, T2)
C
C     Call the differential equation solver
C
  350 CONTINUE
C*****precision > single
C      CALL SVODE
C*****END precision > single
C*****precision > double
      CALL DVODE
C*****END precision > double
     *           (FUN, NEQ, Z, TT1, TT2, ITOL, RTOL, ATOL, ITASK,
     1            ISTATE, IOPT, RWORK(NVODE), LRW, IWORK(IVODE),
     2            LIW, JAC, MF, RWORK, IWORK)
C
      IF (ISTATE .LE. -2) THEN
         IF (ISTATE .EQ. -1) THEN
            ISTATE = 2
            GO TO 350
         ELSE
            WRITE (LOUT,*) ' ISTATE=',ISTATE
            STOP
         ENDIF
      ENDIF
      GO TO 250
C
C         FORMATS
C
 7003 FORMAT (1H1)
 7100 FORMAT (2X, 'T(SEC)', 6X, 'TMP(K)', 6X, 5(1X,A10))
 7105 FORMAT (12E11.3)
 7110 FORMAT (26X, 5(1X,A10))
 7115 FORMAT (22X, 10E11.3)
      END
C
      SUBROUTINE FUN (N, TIME, Z, ZP, RPAR, IPAR)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION(A-H,O-Z), INTEGER(I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER(I-N)
C*****END precision > single
C
      COMMON /RCONS/ P, RU
      COMMON /ICONS/ KK, NWT, NH, NWDOT
      DIMENSION Z(*), ZP(*), RPAR(*), IPAR(*)
C
C     Variables in Z are:  Z(1)   = T
C                          Z(K+1) = Y(K)
C
C     Call CHEMKIN subroutines
C
      CALL CKRHOY (P, Z(1), Z(2), IPAR, RPAR, RHO)
      CALL CKCPBS (Z(1), Z(2), IPAR, RPAR, CPB)
      CALL CKWYP  (P, Z(1), Z(2), IPAR, RPAR, RPAR(NWDOT))
      CALL CKHMS  (Z(1), IPAR, RPAR, RPAR(NH))
C
C     Form governing equation
C
      SUM = 0.0
      DO 100 K = 1, KK
         H    = RPAR(NH    + K - 1)
         WDOT = RPAR(NWDOT + K - 1)
         WT   = RPAR(NWT   + K - 1)
         ZP(K+1) = WDOT * WT / RHO
         SUM = SUM + H * WDOT * WT
 100  CONTINUE
      ZP(1) = -SUM / (RHO*CPB)
C
      RETURN
      END
