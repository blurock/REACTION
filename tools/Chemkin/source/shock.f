      PROGRAM SHOCK
C
C     VERSION 1.7
C     CHANGES SINCE VERSION 1.0:
C     1.  binary file now contains array lengths
C     2.  changed REAL*8 to DOUBLE PRECISION
C     3.  allow upper/lower case input
C     4.  add cray ctss option
C     5.  error handling in character string routines
C     CHANGES SINCE VERSION 1.1:
C     1.  modified OPEN statements for unix
C     CHANGES FOR VERSION 1.3
C     1.  READ (LINKCK) VERS, PREC, KERR
C     CHANGES FOR VERSION 1.4
C     1.  FORMAT 290 in RUNIT changed to reduce length of line below
C         132 characters.
C     CHANGES FOR VERSION 1.5
C     1.  Implement new VODE solver
C     CHANGES FOR VERSION 1.6 (6/17/93 F. Rupley)
C     1.  Add binary file data to solution file to facilitate
C         post-processing
C     CHANGES FOR VERSION 1.7 (3/15/94 F. Rupley)
C     1.  DOS/PC compatibility effort includes adding file names to
C         OPEN statements, removing unused variables in CALL lists,
C         unusued but possibly initialized variables.
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
C         ALL THE STORAGE SPACE NEEDED IS IN RPAR, IPAR, AND CPAR
C
      PARAMETER (LRPAR=4000, LIPAR=4000, LCPAR=100, LIN=5, LOUT=6,
     1           LINKCK=25, LSAVE=55)
      DIMENSION RPAR(LRPAR), IPAR(LIPAR)
      CHARACTER CPAR(LCPAR)*16, ITITL*76
      LOGICAL LGSCAL
C
C         POINTERS TO THE STARTING LOCATIONS IN A AND IPAR
C
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
      COMMON /RCONS/ HO, XLM, DIA, VISCOF, BETA, RU, PATM, VIS
      COMMON /ICONS/ IPRB, IGOT, IMOLF
C
C         SET ALL SHOCK VARIABLES TO ZERO
C
      DATA P1A, P2A, P3A/ 3*0.0/
C
      T1  = 0.0
      T2  = 0.0
      T3  = 0.0
      RHO1 = 0.0
      RHO2 = 0.0
      RHO3 = 0.0
      V1  = 0.0
      V2  = 0.0
      V3  = 0.0
      XM1 = 0.0
      XM2 = 0.0
      XM3 = 0.0
      VRS = 0.0
C
      WRITE (LOUT, '(1X,A)')
     1'SHOCK TUBE CODE:  CHEMKIN-II Version 1.7 March 1994,',
C*****precision > double
     2'                  DOUBLE PRECISION'
C*****END precision > double
C*****precision > single
C     2'                  SINGLE PRECISION'
C*****END precision > single
C
C          READ THE CHEMKIN LINK FILE TO DETERMINE MECHANISM SIZE
C
      OPEN (LINKCK, FORM='UNFORMATTED', STATUS='UNKNOWN', 
     1              FILE='chem.bin')
      OPEN (LSAVE,  FORM='UNFORMATTED', STATUS='UNKNOWN', 
     1              FILE='save.bin')
C
C        SET UP POINTERS FOR THE WORK ARRAYS
C
      CALL SETWRK (LINKCK, LSAVE, LOUT, LIPAR, LRPAR, LCPAR,
     1             IPAR, RPAR, CPAR, NEQ)
      CLOSE (LINKCK)
C
C
  110 CONTINUE
C
C        READ KEYWORD INPUT
C
      CALL READIN (LIN, LOUT, KK, LGSCAL, CPAR(IKS), IPAR(ICWK), 
     1             RPAR(NCWK), RPAR(NXX), TINIT, TEND, DT,
     2             ATOL, RTOL, ITITL)
C
C          WRITE PROBLEM INFORMATION ON FILE LSAVE FOR LATER PROCESSING,
C           FOR EXAMPLE, PLOTTING.
C
      WRITE (LSAVE) 'SOLUTION        '
      WRITE (LSAVE) ITITL, IPRB, IMOLF
C
C        FIRST, PRINT HEADING INDICATING PROBLEM TYPE
C
C-----------------------------------------------------------------------
C        IPRB IS A FLAG INDICATING THE TYPE OF PROBLEM TO BE SOLVED.
C
C        IPRB = 1  INCIDENT SHOCK PROBLEM WITH BOUNDARY LAYER
C                   CORRECTION
C        IPRB = 2  INCIDENT SHOCK PROBLEM WITHOUT BL CORRECTION
C        IPRB = 3  REFLECTED SHOCK PROBLEM
C-----------------------------------------------------------------------
C
      IF (IPRB .EQ. 1) THEN
         WRITE (LOUT, '(/1X,A)')
     1   'INCIDENT SHOCK WITH BOUNDARY LAYER PROBLEM CORRECTION'
      ELSEIF (IPRB .EQ. 2) THEN
         WRITE (LOUT, '(/1X,A)')
     1   'INCIDENT SHOCK WITHOUT BOUNDARY LAYER PROBLEM CORRECTION'
      ELSEIF (IPRB .EQ. 3) THEN
         WRITE (LOUT, '(/1X,A)')
     1   'REFLECTED SHOCK PROBLEM'
      ENDIF
C
C           PRINT INTEGRATION CONTROL AND INITIAL MOLE FRACTIONS
C
      WRITE (LOUT, '(/1X,A,4(/5X,A,1PE12.4))')
     1'INTEGRATION PARAMETERS:',
     2'RTOL            ', RTOL,
     3'ATOL            ', ATOL,
     4'T1              ', TINIT,
     5'T2              ', TEND
      IF (LGSCAL) THEN
         WRITE (LOUT,'(5X,A,1PE12.4))')
     6   'LGDT            ',DT
      ELSE
         WRITE (LOUT,'(5X,A,1PE12.4))')
     6   'DT              ',  DT
      ENDIF
C
      WRITE (LOUT, '(/1X,A,/)') 'MOLE FRACTIONS'
      WRITE (LOUT, '(5X,A,1PE12.4)')
     1      (CPAR(IKS+K-1), RPAR(NXX+K-1), K = 1, KK)
C
C        SET UP INITIAL CONDITIONS; CONVERT TO MASS FRACTIONS
C
      CALL SHKIC (LOUT, IPAR, RPAR)
      P1A = P1/PATM
      P2A = P2/PATM
C
      IF (IPRB .EQ. 3) THEN
         P3A = P3/PATM
C
         WRITE (LOUT, '(/2(/T24,A,T44,A,T64,A),/)')
     1   'CONDITIONS BEFORE','CONDITIONS BEHIND','CONDITIONS BEHIND',
     2   'INCIDENT SHOCK'   ,'INCIDENT SHOCK',   'REFLECTED SHOCK'
C
         WRITE (LOUT,'(5(/1X, A, T24,1PE10.4,T44,1PE10.4,T64,1PE10.4))')
     1   'PRESSURE    (atm)  ', P1A,  P2A,  P3A,
     2   'TEMPERATURE (K)    ', T1,   T2,   T3,
     3   'DENSITY     (g/cc) ', RHO1, RHO2, RHO3,
     4   'VELOCITY    (cm/s) ', V1,   V2,   V3,
     5   'MACH No.           ', XM1,  XM2,  XM3
C
         WRITE (LOUT, '(/1X,A,1PE10.4)')
     1  'REFLECTED SHOCK VELOCITY (cm/s): ',VRS

      ELSE
         WRITE (LOUT, '(/2(/T24,A,T44,A),/)')
     1   'CONDITONS',        'CONDITONS',
     2   'BEFORE THE SHOCK', 'AFTER THE SHOCK'
C
         WRITE (LOUT, '(1X, A, T24, 1PE10.4, T44,1PE10.4)')
     1   'PRESSURE (atm)  ', P1A,  P2A,
     2   'TEMPERATURE (K) ', T1,   T2,
     3   'DENSITY (g/cc)  ', RHO1, RHO2,
     4   'VELOCITY (cm/s) ', V1,   V2,
     5   'MACH NO.        ', XM1,  XM2
C
         IF (IPRB .EQ. 1) WRITE (LOUT, '(3(/1X,A,1PE10.4))')
     1   'GAS VISCOSITY BEFORE SHOCK (g/cm/s) ', VIS,
     2   'BOUNDARY LAYER PARAMETER BETA       ', BETA,
     3   'LIMITING SEPARATION BETWEEN SHOCK AND CONTACT SURFACE (cm) ',
     4   XLM
      ENDIF
C
C        CALL SUBROUTINE THAT SOLVES THE PROBLEM
C
      CALL RUNIT (NEQ, LGSCAL, LOUT, LSAVE, IPAR, TINIT, TEND, DT, ATOL,
     1            RTOL, RPAR, CPAR(IKS))
C
C        READ ANOTHER CASE
C
      GO TO 110
      END
C
      SUBROUTINE SETWRK (LINKCK, LSAVE, LOUT, LIPAR, LRPAR, LCPAR,
     1                   IPAR, RPAR, CPAR, NEQ)
C
C         THIS ROUTINE SETS POINTERS FOR STORAGE RPAR, IPAR, CPAR
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION IPAR(*), RPAR(*)
      CHARACTER CPAR(*)*(*)
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
      COMMON /RCONS/ HO, XLM, DIA, VISCOF, BETA, RU, PATM, VIS
C
      CALL CKLEN (LINKCK, LOUT, LENICK, LENRCK, LENCCK)
      IF (LIPAR.GE.LENICK .AND. LRPAR.GE.LENRCK .AND. LCPAR.GE.LENCCK)
     1   THEN
         CALL CKINIT (LENICK, LENRCK, LENCCK, LINKCK, LOUT, 
     1                IPAR, RPAR, CPAR)
      ELSE
         WRITE (LOUT, '(/1X,A,I6)')
     1   'ERROR...RPAR MUST BE DIMENSIONED AT LEAST ', LENICK,
     2   '        IPAR MUST BE DIMENSIONED AT LEAST ', LENRCK,
     3   '        CPAR MUST BE DIMENSIONED AT LEAST ', LENCCK
         STOP
      ENDIF
C
C          REAL WORK SPACE, RPAR
C         NZZ POINTS TO THE DEPENDENT VARIABLE VECTOR
      NZZ = 1
C         FIRST ELEMENT IS TEMPERATURE
      NT  = NZZ
C         SECOND ELEMENT IS DENSITY
      NRHO = NT + 1
C         SPECIES ELEMENTS
      NKK  = NRHO + 1
C         TL POINTER
      CALL CKINDX (IPAR, RPAR, MM, KK, II, NFIT)
      NTL = NKK + KK
C         VELOCITY
      NV  = NTL + 1
C         AREA
      NA  = NV + 1
C         NCWK POINTS TO THE CHEMKIN WORK SPACE
      NEQ = KK + 5
      NCWK = NZZ + NEQ
C         NXX POINTS TO THE MOLE FRACTION VECTOR
      NXX = NCWK + LENRCK
C         NYY POINTS TO THE MASS FRACTION VECTOR
      NYY = NXX + KK
C         NWT POINTS TO THE MOLECULAR WEIGHT VECTOR
      NWT = NYY + KK
C         NHMS POINTS TO THE ENTHALPIES IN MASS UNITS
      NHMS = NWT + KK
C         NVODE POINTS TO THE REAL WORK SPACE FOR VODE
      NVODE = NHMS + KK
      LRW = 22 + 9*NEQ + 2*NEQ**2
      NTOT = NVODE + LRW - 1
C
C          INTEGER SPACE IPAR
C
C         ICWK POINTS TO THE INTEGER WORK SPACE FOR CHEMKIN
      ICWK = 1
C         IVODE POINTS TO THE INTEGER WORK SPACE FOR VODE
      IVODE = ICWK + LENICK
      LIW = 30 + NEQ
      ITOT = IVODE + LIW - 1
C
      IKS = 1
      ICC = IKS + KK
      ICTOT = ICC + LENCCK
C
      IF (LRPAR.LE.NTOT .OR. LIPAR.LE.ITOT .OR. LCPAR.LE.ICTOT) THEN
         WRITE (LOUT, '(/1X,A,I6)')
     1   'ERROR...RPAR MUST BE DIMENSIONED AT LEAST ', NTOT,
     2   '        IPAR MUST BE DIMENSIONED AT LEAST ', ITOT,
     3   '        CPAR MUST BE DIMENSIONED AT LEAST ', ICTOT
         STOP
      ENDIF
C
C         INITIALIZE CHEMKIN
C
      REWIND (LINKCK)
      CALL CKINIT (LENICK, LENRCK, LENCCK, LINKCK, LOUT, IPAR(ICWK),
     1             RPAR(NCWK), CPAR(ICC))
      CALL CKSYMS (CPAR(ICC),  LOUT, CPAR(IKS), KERR)
      CALL CKRP   (IPAR(ICWK), RPAR(NCWK), RU, RUC, PATM)
      CALL CKWT   (IPAR(ICWK), RPAR(NCWK), RPAR(NWT))
C
      WRITE (LSAVE) 'CKLINK          '
      CALL CKSAVE (LOUT, LSAVE, IPAR(ICWK), RPAR(NCWK), CPAR(ICC))
C
      RETURN
      END
C
      SUBROUTINE RUNIT (NEQ, LGSCAL, LOUT, LSAVE, IPAR, TINIT, TEND,
     1                  DT, ATOL, RTOL, RPAR, KSYM)
C
C        THIS ROUTINE LOOPS OVER THE REQUESTED TIME STEPS, CALLS
C        THE STIFF ODE INTEGRATOR, VODE, AND PRINTS THE RESULTS
C        ON UNIT 6.
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION RPAR(*), IPAR(*)
      CHARACTER KSYM(*)*(*)
      LOGICAL LGSCAL
C
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
C
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
      COMMON /RCONS/ HO, XLM, DIA, VISCOF, BETA, RU, PATM, VIS
      COMMON /ICONS/ IPRB, IGOT, IMOLF
C
      EXTERNAL FUNSHK
      DATA ITOL,MF/1,22/
C
      HO = 1.0E-25
C
C        INITIALIZE OPTIONAL INPUTS FOR VODE
C
      DO 110 N = 5, 10
         RPAR(NVODE + N - 1) = 0.0
         IPAR(IVODE + N - 1) = 0
110   CONTINUE
C
      ISTATE   = 1
      ITASK    = 1
      IOPT     = 1
      RPAR(NVODE+4) = HO
      NLINES   = 100
C
      TT1 = TINIT
      TT2 = TINIT
      IF (LGSCAL) THEN
         TT2   = TINIT
         TINIT = 0.0
         TT2L  = LOG10(TT2)
         TT1L  = TT2L - DT
         TT1   = 10**TT1L
      ENDIF
C
C         BEGIN TIMESTEP LOOP
C
130   CONTINUE
C
      IF (NLINES .GE. 55) THEN
         NLINES = 0
C
         WRITE (LOUT, '(1H1,1X,8(A))')
     1   'T(sec)     ',
     2   'TEMP(K)    ',
     3   'PRES(atm)  ',
     4   'RHO(g/cc)  ',
     5   'MEAN WT    ',
     6   'AREA(cm**2)',
     7   'VEL(cm/s)  ',
     8   'LAB TIME(sec)'
C
         NLINES = NLINES + 1
         DO 140 K1 = 1, KK, 7
            WRITE (LOUT,'(13X,7(A))') (KSYM(K)(:11),K=K1,MIN(K1+6,KK))
            NLINES = NLINES + 1
140      CONTINUE
         WRITE (LOUT, '(1H )')
         NLINES = NLINES + 1
      ENDIF
C
      TT2 = TT1 + DT
      IF (LGSCAL) THEN
         TT1L = LOG10(TT1)
         TT2L = TT1L+DT
         TT2  = 10.0**TT2L
         IF (ISTATE.EQ.1) TT1 = 0.0
      ENDIF
160   CONTINUE
C
C         RETRIEVE LOCAL VARIABLES FROM SOLUTION
C
      T   = RPAR(NT)
      RHO = RPAR(NRHO)
      TL  = RPAR(NTL)
      CALL CKPY (RHO, T, RPAR(NKK), IPAR(ICWK), RPAR(NCWK), P)
C
      IF (IMOLF .EQ. 1) THEN
C           COMPUTE MOLE FRACTIONS
         CALL CKYTX (RPAR(NKK), IPAR(ICWK), RPAR(NCWK), RPAR(NXX))
      ELSE
C           COMPUTE MOLAR CONCENTRATIONS
         CALL CKYTCP (P, T, RPAR(NKK), IPAR(ICWK), RPAR(NCWK),
     1                RPAR(NXX))
      ENDIF
C
      V  = RPAR(NV)
      CALL CKMMWY (RPAR(NKK), IPAR(ICWK), RPAR(NCWK), WTM)
      AREA = ASHOCK
      IF (IPRB .EQ. 1) THEN
         XX = RPAR(NA)
         AREA = ASHOCK/(1.0-SQRT(XX/XLM))
         IF (XX/XLM .GE. 0.90) THEN
            WRITE (LOUT, '(//10X,A,A)')
     1      '  MAXIMUM FLOW DURATION IS APPROACHED.',
     2      '  PROBLEM IS TERMINATED.'
            RETURN
         ENDIF
      ENDIF
C
C            WRITE SOLUTION
C
      WRITE (LOUT,  '(8(1PE11.3))')
     1       TT1, T, P/PATM, RHO, WTM, AREA, V, TL
C
C         WRITE POST PROCESSING TAPE
C
      WRITE (LSAVE) TT1, T, P/PATM, RHO, WTM, AREA, V, TL,
     1              (RPAR(NXX + K - 1),K=1,KK)
      NLINES = NLINES + 1
      DO 200 K1 = 1, KK, 7
         WRITE (LOUT, '(11X,7(1PE11.3))')
     1   (RPAR(NXX + K - 1), K=K1,MIN(K1+6,KK))
         NLINES = NLINES + 1
200   CONTINUE
      WRITE (LOUT, '(1H )')
      NLINES = NLINES + 1
         IF (TT1 .GE. TEND) RETURN
210   CONTINUE
C
C          CALL INTEGRATOR
C
C*****precision > single
C      CALL SVODE
C*****END precision > single
C*****precision > double
      CALL DVODE
C*****END precision > double
     *          (FUNSHK, NEQ, RPAR(NZZ), TT1, TT2, ITOL, RTOL, ATOL,
     1           ITASK, ISTATE, IOPT, RPAR(NVODE), LRW, IPAR(IVODE),
     2           LIW, JAC, MF, RPAR, IPAR)
C
      IF (ISTATE .EQ. 2) GO TO 130
      IF (ISTATE .NE. -1) THEN
         WRITE (LOUT, '(/5X,A,I3)') 'ISTATE = ',ISTATE
         STOP
      ENDIF
      ISTATE = 2
      GO TO 210
C
      END
C
      SUBROUTINE FUNSHK (NEQ, TIME, Z, ZP, RPAR, IPAR)
C
C         THIS ROUTINE IS WHERE THE GOVERNING EQUATIONS ARE DEFINED.
C         THE DEPENDENT VARIABLE VECTOR, Z, IS PASSED IN FROM VODE,
C         AND THE RIGHT HAND SIDES OF THE GOVERNING EQUATIONS ARE
C         EVALUATED AND PASSED BACK THROUGH ZP.
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION Z(*), ZP(*), RPAR(*), IPAR(*)
C
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
C
      COMMON /RCONS/ HO, XLM, DIA, VISCOF, BETA, RU, PATM, VIS
      COMMON /ICONS/ IPRB, IGOT, IMOLF
C
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
C
      AREA(AUG1) = ASHOCK / (1.0-SQRT(AUG1/XLM))
      DAREA(AUG1,AUG2) = AUG2**2 /2.0 /XLM /ASHOCK /SQRT(AUG1/XLM)
C
C        Z(1)=TEMPERATURE    ZP(1)=D(T)/DT
C        Z(2)=RHO            ZP(2)=D(RHO)/DT
C        Z(K+2)=Y(K)         ZP(K+2)=D(Y(K))/DT
C        Z(KK+3)=LAB TIME    ZP(KK+3)=D(LT)/DT
C        Z(KK+4)=VELOCITY    ZP(KK+4)=D(U)/DT
C        Z(KK+5)=DISTANCE    ZP(KK+5) = D(X)/DT
C
C           COMPUTE LOCAL VARIABLES
C
      T    = Z(NT)
      RHO  = Z(NRHO)
      U    = Z(NV)
      X    = Z(NA)
      A    = ASHOCK
      DADX = 0.0
      U2   = U**2
      CALL CKPY   (RHO, T, Z(NKK), IPAR(ICWK), RPAR(NCWK), P)
      CALL CKMMWY (Z(NKK), IPAR(ICWK), RPAR(NCWK), WTM)
      CALL CKWYP  (P, T, Z(NKK), IPAR(ICWK), RPAR(NCWK), ZP(NKK))
      CALL CKCPBS (T, Z(NKK), IPAR(ICWK), RPAR(NCWK), CPB)
      CALL CKHMS  (T, IPAR(ICWK), RPAR(NCWK), RPAR(NHMS))
      CPBT = CPB*T
C
      IF (IPRB .EQ. 1) THEN
C
C        AREA PROFILE
C
         A = AREA(X)
         IF (X .NE. 0.0) THEN
            DADX = DAREA(X,A)
         ELSE
C
C     AT TIME=0 THE DERIVATIVE FUNCTION IS SINGULAR. EVALUATE DADX
C     AT A TIME OF  HO   SEC (THE SMALLEST TIME STEP EMPLOYED).
C
            X0 = HO*V2
            A = AREA(X0)
            DADX = DAREA(X0,A)
         ENDIF
      ENDIF
C
      SUM = 0.0
      DO 130 K = 1, KK
         WT  = RPAR(NWT + K - 1)
         HMS = RPAR(NHMS + K - 1)
         SUM = SUM + WT * ZP(K+2) * (HMS - (WTM/WT)*CPBT)
130   CONTINUE
      T1    = RU * RHO * SUM / (WTM * CPB)
      T2    = (1.0 - RU/(WTM*CPB)) * U2 * RHO**2 * U * DADX/A
      TTHREE    = P * (1.0+U2/CPBT) - RHO*U2
      ZP(NRHO) = (T1+T2) / TTHREE
C
      SUM = 0.0
      DO 140 K = 1, KK
         WT  = RPAR(NWT + K - 1)
         HMS = RPAR(NHMS + K - 1)
         SUM = SUM + WT * ZP(NKK + K - 1) * HMS
         ZP(NKK + K - 1) = WT * ZP(NKK + K - 1) / RHO
140   CONTINUE
      ZP(NT) = (U2*ZP(NRHO) - SUM)/(CPB*RHO) + U2*U*DADX/(A*CPB)
C
      IF (IPRB .EQ. 3) THEN
         ZP(NTL) = 1.0
      ELSE
         ZP(NTL) = RHO1*ASHOCK/RHO/A
      ENDIF
C
      ZP(NV) = -U*ZP(NRHO)/RHO-U2*DADX/A
      ZP(NA) = U
      RETURN
      END
C
      SUBROUTINE READIN (LIN, LOUT, KK, LGSCAL, KSYM, ICKWRK, RCKWRK, 
     1                   X, TINIT, TEND, DT, ATOL, RTOL, ITITL)
C
C         THIS ROUTINE READS KEYWORD INPUT, AND PASSES THE PROBLEM
C         PARAMETERS BACK TO THE MAIN CODE.
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION ICKWRK(*), RCKWRK(*), X(*)
C
      CHARACTER LINE*80, KEYWRD*4
      CHARACTER*(*) KSYM(*), ITITL
      LOGICAL IERR, KERR, LGSCAL, NEC(5), N1(4), N2(4), N3(4), NB(2)
C
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
C
      COMMON /RCONS/ HO, XLM, DIA, VISCOF, BETA, RU, PATM, VIS
      COMMON /ICONS/ IPRB, IGOT, IMOLF
C
      DATA NEC,N1,N2,N3,NB,KERR/20*.FALSE./
C
C         INITIALIZE VARIABLES
C
      DO 110 K = 1, KK
         X(K) = 0.0
110   CONTINUE
C
      IMOLF  = 1
      TINIT  = 0.0
      DT     = 1.0E-6
      LGSCAL = .FALSE.
      RTOL   = 1.E-6
      ATOL   = 1.E-10
      ASHOCK = 1.0
      DIA    = SQRT(4.0*ASHOCK/3.1415926535)
C
      WRITE(LOUT, '(/1X,A,/)') ' KEYWORD INPUT:'
C        READ INPUT CARD
C
130   CONTINUE
      KEYWRD = ' '
      LINE = ' '
      READ  (LIN,  '(A,A)', END=135) KEYWRD, LINE
      WRITE (LOUT, '(5X,A,A)') KEYWRD, LINE
      CALL UPCASE (KEYWRD)
C
C         DETERMINE WHICH KEYWORD WAS READ
C
      IF (KEYWRD .EQ. 'TSTR') THEN
C
C         INITIAL TIME
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, TINIT, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'TEND') THEN
C
C         FINAL TIME
C
         NEC(1) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, TEND, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'DT') THEN
C
C         OUTPUT TIME INTERVAL
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, DT, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'LGDT') THEN
C
C         LOG OUTPUT TIME INTERVAL
C
         LGSCAL = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, DT, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'ATOL') THEN
C
C         FLOOR VALUE FOR INTEGRATION
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, ATOL, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'RTOL') THEN
C
C         ERROR TOLERANCE FOR INTEGRATION
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, RTOL, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'T1') THEN
C
C         TEMPERATURE BEFORE INCIDENT SHOCK
C
         N1(1) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, T1, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'P1A') THEN
C
C         PRESSURE BEFORE INCIDENT SHOCK (ATM)
C
         N1(2) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, P1, IERR)
         KERR = KERR.OR.IERR
         P1 = P1*PATM
      ELSEIF (KEYWRD .EQ. 'RHO1') THEN
C
C         DENSITY BEFORE INCIDENT SHOCK
C
         N1(3) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, RHO1, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'VSHK') THEN
C
C         INCIDENT SHOCK VELOCITY
C
         N1(4) = .TRUE.
         N2(4) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, V1, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'T2') THEN
C
C         TEMPERATURE BEHIND INCIDENT SHOCK
C
         N2(1) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, T2, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'P2A') THEN
C
C         PRESSURE BEHIND INCIDENT SHOCK (ATM)
C
         N2(2) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, P2, IERR)
         KERR = KERR.OR.IERR
         P2 = P2*PATM
      ELSEIF (KEYWRD .EQ. 'RHO2') THEN
C
C         DENSITY BEHIND INCIDENT SHOCK
C
         N2(3) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, RHO2, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'T3') THEN
C
C         TEMPERATURE BEHIND REFLECTED SHOCK
C
         N3(1) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, T3, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'P3A') THEN
C
C         PRESSURE BEHIND REFLECTED SHOCK (ATM)
C
         N3(2) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, P3, IERR)
         KERR = KERR.OR.IERR
         P3 = P3*PATM
      ELSEIF (KEYWRD .EQ. 'RHO3') THEN
C
C         DENSITY BEHIND REFLECTED SHOCK
C
         N3(3) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, RHO3, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'VRS') THEN
C
C         REFLECTED SHOCK VELOCITY
C
         N3(4) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, VRS, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'CONC') THEN
C
C         OUTPUT IN CONCENTRATION UNITS (MOLE/CC)
C
         IMOLF = 0
      ELSEIF (KEYWRD .EQ. 'ISHK') THEN
C
C         INCIDENT SHOCK PROBLEM
C
         NEC(2) = .TRUE.
         IPRB = 2
      ELSEIF (KEYWRD .EQ. 'ISKB') THEN
C
C         INCIDENT SHOCK WITH BOUNDARY LAYER CORRECTION
C
         NEC(2) = .TRUE.
         IPRB = 1
      ELSEIF (KEYWRD .EQ. 'RSHK') THEN
C
C         REFLECTED SHOCK
C
         NEC(2) = .TRUE.
         IPRB = 3
      ELSEIF (KEYWRD .EQ. 'INIT') THEN
C
C         INITIAL MOLE FRACTIONS
C
         IERR = .FALSE.
         CALL CKSNUM (LINE, 1, LOUT, KSYM, KK, KSPEC, NVAL, VALUE,
     1                IERR)
         IF (IERR) THEN
            WRITE (LOUT,'(A)') ' ERROR READING KEYWORD '//KEYWRD
            KERR = .TRUE.
         ENDIF
         X(KSPEC) = VALUE
      ELSEIF (KEYWRD .EQ. 'DIA') THEN
C
C         DIAMETER OF TUBE
C
         NB(1) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, DIA, IERR)
         KERR = KERR.OR.IERR
         ASHOCK = 3.1415926535*DIA**2/4.0
      ELSEIF (KEYWRD .EQ. 'VISC') THEN
C
C         VISCOSITY COEFFICIENT
C
         NB(2) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, VISCOF, IERR)
         KERR = KERR.OR.IERR
      ELSEIF (KEYWRD .EQ. 'TITL') THEN
C
C        TITLE CARD
C
         ITITL = ' '
         ITITL = LINE
C
      ELSEIF (KEYWRD .EQ. 'END') THEN
C
C         END CARD
C
         GO TO 480
      ELSE
         WRITE (LOUT, 750) 'ERROR...ILLEGAL KEYWORD'
         KERR = .TRUE.
      ENDIF
C
C         GO BACK TO READ NEXT CARD
C
      GO TO 130
C
C         CHECK TO SEE IF INPUT IS COMPLETE
C
480   CONTINUE
      SUM = 0.0
      DO 490 K = 1, KK
         SUM = SUM + X(K)
490   CONTINUE
      IF (ABS(SUM-1.0) .GE. 1.E-6) THEN
         WRITE (LOUT, 750)
     1   'ERROR...MOLE FRACTIONS DO NOT SUM TO 1.0'
         KERR = .TRUE.
      ENDIF
C
      IF (.NOT. NEC(1)) THEN
         WRITE (LOUT, 750) 'ERROR...FINAL TIME NOT SPECIFIED'
         KERR = .TRUE.
      ENDIF
      IF (.NOT. NEC(2)) THEN
         WRITE (LOUT, 750) 'ERROR...PROBLEM TYPE NOT SPECIFIED'

         KERR = .TRUE.
      ENDIF
C
C         CHECK INCIDENT SHOCK WITH BOUNDARY LAYER
C
      IF (IPRB .EQ. 1) THEN
         IF (.NOT. NB(1)) THEN
            WRITE (LOUT, 750) 'ERROR...DIAMETER NOT SPECIFIED'

            KERR = .TRUE.
         ENDIF
         IF (.NOT. NB(2)) THEN
            WRITE (LOUT, 750)
     1      'ERROR...VISCOSITY COEFFICIENT NOT SPECIFIED'

            KERR = .TRUE.
         ENDIF
      ENDIF
C
C         CHECK TO SEE IF CONDITIONS BEFORE (1) OR
C         AFTER (2) THE SHOCK ARE GIVEN
C
      IF (IPRB .NE. 3) THEN
         DO 570 N = 1, 3
            IF (N1(N)) I1 = 1
            IF (N2(N)) I2 = 1
570      CONTINUE
         IF (I1.EQ.1 .AND. I2.EQ.1) THEN
            WRITE (LOUT, 750)
     1'ERROR...CONDITIONS BEFORE AND BEHIND INCIDENT SHOCK SPECIFIED'
            KERR = .TRUE.
         ENDIF
         IF (.NOT. N1(4)) THEN
            WRITE (LOUT, 850)
            KERR = .TRUE.
         ENDIF
C
         IF (KERR) STOP
C
C          DETERMINE P,T AND RHO FOR CONDITIONS 1 OR 2
C
         IGOT = 1
         CALL SETCON (N1, X, RU, ICKWRK, RCKWRK, T1, P1, RHO1, IER)
         IF (IER .NE. 0) THEN
            CALL SETCON (N2, X, RU, ICKWRK, RCKWRK, T2, P2, RHO2, IER)
            IF (IER .NE. 0) THEN
               WRITE (LOUT, 860)
               STOP
            ENDIF
            IGOT = 2
         ENDIF
C
C         REFLECTED SHOCK
C
      ELSE
         DO 650 N = 1, 3
            IF (N1(N)) I1 = 1
            IF (N3(N)) I5 = 1
650      CONTINUE
         IF (I1.EQ.1 .AND. I5.EQ.1) THEN
            WRITE (LOUT, 870)
            KERR = .TRUE.
         ENDIF
C
         IF (I5.NE.1 .AND. (.NOT.N1(4))) THEN
            WRITE (LOUT, 850)
            KERR = .TRUE.
         ENDIF
C
         IF (KERR) STOP
C
         IGOT = 1
         CALL SETCON (N1, X, RU, ICKWRK, RCKWRK, T1, P1, RHO1, IER)
         IF (IER .NE. 0) THEN
            CALL SETCON (N3, X, RU, ICKWRK, RCKWRK, T3, P3, RHO3, IER)
            IF (IER .NE. 0) THEN
               WRITE (LOUT, 880)
               STOP
            ENDIF
            IGOT = 5
         ENDIF
      ENDIF
C
      RETURN
  135 CONTINUE
      WRITE (LOUT, *) ' END OF KEYWORD INPUT '
      STOP
C
C         FORMATS
C
C
750   FORMAT (10X, A)
850   FORMAT (10X,'ERROR...SHOCK SPEED NOT SPECIFIED')
860   FORMAT (10X,'ERROR...ANY TWO OF (T1,P1,RHO1) ',
     1                 'OR ANY TWO OF (T2,P2,RHO2) MUST BE SPECIFIED')
870   FORMAT (10X,'ERROR...CONDITIONS BEFORE INCIDENT SHOCK AND BEHIND',
     1 ' REFLECTED SHOCK GIVEN')
880   FORMAT (10X,'ERROR...ANY TWO OF (T1,P1,RHO1) ',
     1                 'OR ANY TWO OF (T3,P3,RHO3) MUST BE SPECIFIED')
      END
C
      SUBROUTINE SETCON (NF, X, RU, ICKWRK, RCKWRK, T, P, RHO, IER)
C
C        THIS ROUTINE DETERMINES WHICH STATE VARIABLES WERE DEFINED
C        BY THE KEYWORD INPUT, AND COMPUTES THE REMAINING ONES.
C        FOR EXAMPLE IF PRESSURE AND TEMPERATURE WERE GIVEN DENSITY
C        IS CALCULATED.  THE ROUTINE ALSO DOES ERROR CHECKS TO MAKE
C        SURE SUFFICIENT DATA WAS PROVIDED.
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION X(*), ICKWRK(*), RCKWRK(*)
      LOGICAL NF(*)
C
      IER = 0
C
      IF (.NOT. NF(1)) THEN
C
C         TEMPERATURE NOT GIVEN
C
         IF (.NOT.NF(2) .AND. .NOT.NF(3)) THEN
            IER = 1
         ELSE
C
C         PRESSURE AND DENSITY GIVEN
C
            CALL CKMMWX (X, ICKWRK, RCKWRK, WTM)
            T = P*WTM/RHO/RU
         ENDIF
C
      ELSE
C
C         TEMPERATURE GIVEN
C
         IF (NF(2)) THEN
C
C         TEMPERATURE AND PRESSURE GIVEN
C
            CALL CKRHOX (P, T, X, ICKWRK, RCKWRK, RHO)
         ELSE
C
C           PRESSURE NOT GIVEN
C
            IF (.NOT. NF(3)) THEN
               IER = 1
            ELSE
C
C            TEMPERATURE AND DENSITY GIVEN
C
               CALL CKPX (RHO, T, X, ICKWRK, RCKWRK, P)
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE SHKIC (LOUT, IPAR, RPAR)
C
C        THIS ROUTINE COMPUTES CONDITIONS IMMEDIATELY AFTER EITHER
C        AN INCIDENT OR A REFLECTED SHOCK FROM THE RANKINE-HUGONIOT
C        RELATIONS.  IF NEEDED, IT ALSO SETS UP PARAMETERS REQUIRED
C        TO MAKE THE BOUNDARY LAYER CORRECTIONS FOR INCIDENT SHOCK
C        PROBLEMS.
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION IPAR(*), RPAR(*)
C
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
      COMMON /RCONS/ HO, XLM, DIA, VISCOF, BETA, RU, PATM, VIS
      COMMON /ICONS/ IPRB, IGOT, IMOLF
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
C
      EXTERNAL FUN1, FUN2, FUN3, FUN3A
      DATA RE,AE/2*1.0E-06/
C
      CALL CKXTY  (RPAR(NXX), IPAR(ICWK), RPAR(NCWK), RPAR(NYY))
      CALL CKXTY  (RPAR(NXX), IPAR(ICWK), RPAR(NCWK), RPAR(NKK))
      CALL CKMMWX (RPAR(NXX), IPAR(ICWK), RPAR(NCWK), WTM)
C
C     CONDITIONS BEFORE INCIDENT SHOCK GIVEN
C
      IF (IGOT .EQ. 1) THEN
         CALL CKHBMS (T1, RPAR(NKK), IPAR(ICWK), RPAR(NCWK), H1)
         CALL CKCPBL (T1, RPAR(NXX), IPAR(ICWK), RPAR(NCWK), CPB)
         GAMMA1 = CPB/(CPB-RU)
         XM1 = V1/SQRT(GAMMA1*RU*T1/WTM)
         IF (XM1 .LE. 1.0) THEN
            WRITE (LOUT, '(//20X,A)')
     1      'SHOCK WAVE SPEED IS LESS THAN MACH 1'
            STOP
         ENDIF
C
C        ALPHA IS THE TEMPERATURE RATIO ACROSS THE SHOCK
C
         ALPHA = 1.0
         BETA  = (2.0*GAMMA1*XM1**2-GAMMA1+1.0)/(GAMMA1+1.0)
         ALPHI = (BETA*((2.0/XM1**2)+GAMMA1-1.0))/(GAMMA1+1.0)
         CALL ZROIN (FUN1, ALPHA, ALPHI, RE, AE, IFLAG, IPAR, RPAR)
C
         IF (IFLAG .NE. 1) THEN
            WRITE (LOUT, 230) 'FUN1',IFLAG
            IF (IFLAG .NE. 4) STOP
         ENDIF
C
         T2     = ALPHA*T1
         B      = -(1.0+RHO1*V1**2/P1)
         C      = RHO1*V1**2/P1*ALPHA
         BETA   = 0.5*(-B+SQRT(B**2-4.0*C))
         P2     = BETA*P1
         RHO2   = P2*WTM/RU/T2
         V2     = RHO1*V1/RHO2
         CALL CKCPBL (T2, RPAR(NXX), IPAR(ICWK), RPAR(NCWK), CPB)
         GAMMA2 = CPB/(CPB-RU)
         XM2    = V2/SQRT(GAMMA2*RU*T2/WTM)
         CALL CKHBMS (T2, RPAR(NKK), IPAR(ICWK), RPAR(NCWK), H2)
C
         IF (IPRB .NE. 3) GO TO 140
C
C        CONDITIONS BEHIND REFLECTED SHOCK
C
         ALPHA = 1.0
         ETA   = RHO2/RHO1
         XI    = ETA*(XM1**2*(ETA-1.0)*GAMMA1+ETA)/
     1           (XM1**2*(ETA-1.0)*(GAMMA1-1.0)+ETA)
         T3OT1 = 1.0+XM1**2*(GAMMA1-1.0)*(XI-1.0)*(ETA-1.0)
     1              /ETA/(XI-ETA)
         ALPHI = T3OT1*T1/T2
C
         IF (VRS .EQ. 0.0) THEN
            CALL ZROIN (FUN3, ALPHA, ALPHI, RE, AE, IFLAG, IPAR, RPAR)
            IF (IFLAG .NE. 1) THEN
               WRITE (LOUT, 230) 'FUN3',IFLAG
               IF (IFLAG .NE. 4) STOP
            ENDIF
            T3   = ALPHA*T2
            B    = -(1.0+RHO2*(V1-V2)**2/P2+ALPHA)
            BETA = 0.5*(-B+SQRT(B**2-4.0*ALPHA))
            P3   = BETA*P2
            VRS  = ALPHA/BETA*(V1-V2)/(1.0-ALPHA/BETA)
         ELSE
C
            CALL ZROIN (FUN3A, ALPHA, ALPHI, RE, AE, IFLAG, IPAR, RPAR)
            IF (IFLAG .NE. 1) THEN
               WRITE (LOUT, 230) 'FUN3',IFLAG
               IF (IFLAG .NE. 4) STOP
            ENDIF
C
            T3   = ALPHA*T2
            V2P  = VRS + V1 - V2
            B    = -(1.0 + RHO2/P2 * V2P**2)
            BETA = 0.5*(-B +SQRT(B**2 - 4.0*ALPHA*RHO2/P2*V2P**2))
            P3   = BETA*P2
         ENDIF
C
         RHO3   = P3*WTM/RU/T3
         V3     = RHO2/RHO3*(VRS+V1-V2)
         CALL CKCPBL (T3, RPAR(NXX), IPAR(ICWK), RPAR(NCWK), CPB)
         CALL CKHBMS (T3, RPAR(NKK), IPAR(ICWK), RPAR(NCWK), H3)
         GAMMA5 = CPB/(CPB-RU)
         XM3    = V3/SQRT(GAMMA5*RU*T3/WTM)
C
      ELSEIF (IGOT .EQ. 2) THEN
C
C        INCIDENT SHOCK VELOCITY AND CONDITIONS BEHIND INCIDENT SHOCK
C        GIVEN
C
         CALL CKRHOX (P2, T2, RPAR(NXX), IPAR(ICWK), RPAR(NCWK), RHO2)
         CALL CKHBMS (T2, RPAR(NKK), IPAR(ICWK), RPAR(NCWK), H2)
         CALL CKCPBL (T2, RPAR(NXX), IPAR(ICWK), RPAR(NCWK), CPB)
         GAMMA2 = CPB/(CPB-RU)
         ALPHA  = 1.0
         ALPHI  = T2/298.0
C
         CALL ZROIN (FUN2, ALPHA, ALPHI, RE, AE, IFLAG, IPAR, RPAR)
         IF (IFLAG .NE. 1) THEN
            WRITE (LOUT, 230) 'FUN2',IFLAG
            IF (IFLAG .NE. 4) STOP
         ENDIF
C
         T1   = T2/ALPHA
         B    = -(1.0+RHO2/P2*V1**2*ALPHA)
         C    = RHO2/P2*V1**2*ALPHA**2
         BETA = 0.5*(-B+SQRT(B**2-4.0*C))
         P1   = P2/BETA
         RHO1 = P1*WTM/RU/T1
         V2   = RHO1*V1/RHO2
         XM2  = V2/SQRT(GAMMA2*RU*T2/WTM)
         CALL CKCPBL (T1, RPAR(NXX), IPAR(ICWK), RPAR(NCWK), CPB)
         GAMMA1 = CPB/(CPB-RU)
         XM1  = V1/SQRT(GAMMA1*RU*T1/WTM)
         CALL CKHBMS (T1, RPAR(NKK), IPAR(ICWK), RPAR(NCWK), H1)
         GO TO 140
C
      ELSE
C
C        CONDITIONS BEHIND REFLECTED SHOCK GIVEN
C
         CALL CKHBMS (T3, RPAR(NKK), IPAR(ICWK), RPAR(NCWK), H3)
         XM3 = 0.0
C
C        CONDITIONS BEFORE AND BEHIND INCIDENT SHOCK NOT NEEDED
C        VALUES DEFAULTED TO ZERO
C
         P1   = 0.0
         T1   = 0.0
         RHO1 = 0.0
         H1   = 0.0
         XM1  = 0.0
         V1   = 0.0
         P2   = 0.0
         T2   = 0.0
         RHO2 = 0.0
         H2   = 0.0
         V2   = 0.0
         XM2  = 0.0
      ENDIF
C
C     INITIAL CONDITIONS FOR INTEGRATION BEHIND REFLECTED SHOCK
C
      RPAR(NT)    = T3
      RPAR(NRHO)    = RHO3
      RPAR(NTL) = 0.0
      RPAR(NV) = 0.0
      RPAR(NA) = 0.0
      RETURN
C
C     INITIAL CONDITIONS FOR INTEGRATION BEHIND INCIDENT SHOCK
C
  140 CONTINUE
      RPAR(NT)    = T2
      RPAR(NRHO)    = RHO2
      RPAR(NTL) = 0.0
      RPAR(NV) = V2
      RPAR(NA) = 0.0
      IF (IPRB .NE. 2) THEN
C
C        BOUNDARY LAYER PARAMETERS
C
         ASHOCK = 3.1415926535*DIA**2/4.0
         VIS    = VISCOF*(T1/300.0)**0.6756
         CALL CKRHOX (P2, T1, RPAR(NXX), IPAR(ICWK), RPAR(NCWK), RHOW)
         WW     = RHO2/RHO1
         VIS2   = VISCOF*(T2/300.0)**0.6756
         CEO    = (RHO2*VIS2/RHOW/VIS)**0.37
         ZETA   = (GAMMA1+1.0)/(GAMMA1-1.0)
         IF (WW .GE. ZETA) ZETA = WW
         BETA   = 1.59*(1.0+(1.796+0.802*WW)/(ZETA*WW-1.0))*CEO
         XLM    = (DIA*RHO2/4.0/BETA/RHOW)**2/(WW-1.0)*V2*RHOW/VIS
      ENDIF
      RETURN
C
C         FORMATS
C
230   FORMAT (//20X,'IFLAG FOR ',A,' IS',I4)
      END
C
C*****precision > double
      DOUBLE PRECISION FUNCTION FUN1 (ALPHA, IPAR, RPAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      REAL FUNCTION FUN1 (ALPHA, IPAR, RPAR)
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
C         THIS ROUTINE IS CALLED BY ZROIN AS PART OF THE
C         ITERATION PROCESS TO DETERMINE CONDITIONS BEHIND
C         AN INCIDENT SHOCK GIVEN CONDITIONS BEFORE THE SHOCK.
C
      DIMENSION IPAR(*), RPAR(*)
C
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
C
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
C
C
      T    = ALPHA*T1
      CALL CKHBMS (T, RPAR(NYY), IPAR(ICWK), RPAR(NCWK), H)
      B    = -(1.0 + RHO1*V1**2/P1)
      C    = RHO1*V1**2/P1*ALPHA
      BETA = 0.5*(-B+SQRT(B**2-4.0*C))
      FUN1 = H1+0.5*V1**2*(1.0-ALPHA**2/BETA**2)-H
      RETURN
      END
C
C*****precision > double
      DOUBLE PRECISION FUNCTION FUN2 (ALPHA, IPAR, RPAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      REAL FUNCTION FUN2 (ALPHA, IPAR, RPAR)
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
C         THIS ROUTINE IS CALLED BY ZROIN AS PART OF THE
C         ITERATION PROCESS TO DETERMINE CONDITIONS ABOUT
C         AN INCIDENT SHOCK GIVEN THE SHOCK
C         VELOCITY, AND STATE VARIABLES BEHIND THE SHOCK.
C
      DIMENSION IPAR(*), RPAR(*)
C
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
C
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
C
C
      T    = T2/ALPHA
      CALL CKHBMS (T, RPAR(NYY), IPAR(ICWK), RPAR(NCWK), H)
      B    = -(1.0+RHO2*V1**2*ALPHA/P2)
      C    = RHO2*V1**2*ALPHA**2/P2
      BETA = (-B+SQRT(B**2-4.0*C))/2.0
      FUN2 = H + 0.5*V1**2*(1.0-ALPHA**2/BETA**2) - H2
      RETURN
      END
C
C*****precision > double
      DOUBLE PRECISION FUNCTION FUN3 (ALPHA, IPAR, RPAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      REAL FUNCTION FUN3 (ALPHA, IPAR, RPAR)
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
C         THIS ROUTINE IS CALLED BY ZROIN AS PART OF THE
C         ITERATION PROCESS TO DETERMINE CONDITIONS BEHIND
C         A REFLECTED SHOCK GIVEN CONDITIONS BEFORE THE
C         INCIDENT SHOCK.
C
      DIMENSION IPAR(*), RPAR(*)
C
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
C
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
C
C
      T    = ALPHA*T2
      CALL CKHBMS (T, RPAR(NYY), IPAR(ICWK), RPAR(NCWK), H)
      B    = -(1.0+RHO2/P2*(V1-V2)**2+ALPHA)
      BETA = 0.5*(-B+SQRT(B**2-4.0*ALPHA))
      FUN3 = H2 + 0.5*(V1-V2)**2/(1.0-ALPHA/BETA)*(1.0+ALPHA/BETA) - H
      RETURN
      END
C
C*****precision > double
      DOUBLE PRECISION FUNCTION FUN3A (ALPHA, IPAR, RPAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      REAL FUNCTION FUN3A (ALPHA, IPAR, RPAR)
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
C         THIS ROUTINE IS CALLED BY ZROIN AS PART OF THE
C         ITERATION PROCESS TO DETERMINE CONDITIONS BEHIND
C         A REFLECTED SHOCK GIVEN CONDITIONS BEFORE THE
C         INCIDENT SHOCK, AND THE REFLECTED SHOCK VELOCITY.
C
      DIMENSION IPAR(*), RPAR(*)
C
      COMMON /SHKVAR/ P1, T1, RHO1, H1, V1, XM1, P2, T2, RHO2, H2, V2,
     1                XM2, P3, T3, RHO3, H3, V3, XM3, VRS, ASHOCK
C
      COMMON /LEN/ KK, NZZ, NT, NRHO, NKK, NTL, NV, NA, NCWK,
     1             NXX, NYY, NWT, NHMS, NGW, LRW, ICWK,
     2             IKS, ICC, IVODE, NVODE, LIW
C
C
      T = ALPHA*T2
      CALL CKHBMS (T, RPAR(NYY), IPAR(ICWK), RPAR(NCWK), H)
      V2P = VRS + V1 - V2
      B = -(1.0+RHO2/P2*V2P**2)
      BETA = 0.5*(-B+SQRT(B**2-4.0*ALPHA*RHO2/P2*V2P**2))
      FUN3A = H2 + 0.5*V2P**2*(1.0-ALPHA**2/BETA**2) - H
      RETURN
      END
C
      SUBROUTINE ZROIN (F, B, C, RE, AE, IFLAG, IPAR, RPAR)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION IPAR(*),RPAR(*)
      EXTERNAL F
      DATA ONE,ZERO/1.0,0.0/
C
C     SANDIA MATHEMATICAL PROGRAM LIBRARY
C     APPLIED MATHEMATICS DIVISION 2646
C     SANDIA LABORATORIES
C     ALBUQUERQUE, NEW MEXICO  87185
C     CONTROL DATA 6600/7600  VERSION 8.1  AUGUST 1980
C                   *************************
C                   *       ISSUED BY       *
C                   *  SANDIA LABORATORIES, *
C                   *   A PRIME CONTRACTOR  *
C                   ********     TO THE     *
C                          *  UNITED STATES *
C                          *   DEPARTMENT   *
C                          *       OF       *
C                          *     ENERGY     *
C      *********************  ---NOTICE---  *********************
C      *THIS REPORT WAS PREPARED AS AN ACCOUNT OF WORK SPONSORED*
C      *  BY THE UNITED STATES GOVERNMENT.  NEITHER THE UNITED  *
C      *   STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY,   *
C      *               NOR ANY OF THEIR EMPLOYEES,              *
C      * NOR ANY OF THEIR CONTRACTORS, SUBCONTRACTORS, OR THEIR *
C      * EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR  *
C      * ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE  *
C      *          **********    ACCURACY,   **********          *
C      *          *        *  COMPLETENESS  *        *          *
C      *          *        *  OR USEFULNESS *        *          *
C      *          *        *     OF ANY     *        *          *
C      *          *        *  INFORMATION,  *        *          *
C      *          *        *   APPARATUS,   *        *          *
C      *       ****        *     PRODUCT    *        ****       *
C      *       *           *   OR PROCESS   *           *       *
C      *       *           *   DISCLOSED,   *           *       *
C      *       *           *  OR REPRESENTS *           *       *
C      *       *          **    THAT ITS    **          *       *
C      *       *          **  USE WOULD NOT **          *       *
C      *********          **    INFRINGE    **          *********
C                         **    PRIVATELY   **
C                         **      OWNED     **
C                         **     RIGHTS.    **
C                         **                **
C                         **                **
C                         **                **
C                         ********************
C
C     BASED ON A METHOD BY T J DEKKER
C     WRITTEN BY L F SHAMPINE AND H A WATTS
C     MODIFIED FOR THE MATH LIBRARY BY C B BAILEY
C
C     ABSTRACT
C        ZROIN SEARCHES FOR A ZERO OF A FUNCTION F(X) BETWEEN
C        THE GIVEN VALUES B AND C UNTIL THE WIDTH OF THE INTERVAL
C        (B,C) HAS COLLAPSED TO WITHIN A TOLERANCE SPECIFIED BY
C        THE STOPPING CRITERION, ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
C        THE METHOD USED IS AN EFFICIENT COMBINATION OF BISECTION AND
C        THE SECANT RULE.  IN ORDER TO INSURE THAT ZROIN WILL CONVERGE
C        TO A ZERO, THE USER SHOULD PICK VALUES FOR B AND C AT WHICH
C        THE FUNCTION DIFFERS IN SIGN.
C
C     DESCRIPTION OF ARGUMENTS
C     F,B,C,RE AND AE ARE INPUT PARAMETERS
C     B,C AND IFLAG ARE OUTPUT PARAMETERS
C        F     - NAME OF THE REAL VALUED EXTERNAL FUNCTION.  THIS NAME
C                MUST BE IN AN EXTERNAL STATEMENT IN THE CALLING
C                PROGRAM.  F MUST BE A FUNCTION OF ONE REAL ARGUMENT.
C        B     - ONE END OF THE INTERVAL (B,C).  THE VALUE RETURNED FOR
C                B USUALLY IS THE BETTER APPROXIMATION TO A ZERO OF F.
C        C     - THE OTHER END OF THE INTERVAL (B,C)
C        RE    - RELATIVE ERROR USED FOR RW IN THE STOPPING CRITERION.
C                IF THE REQUESTED RE IS LESS THAN MACHINE PRECISION,
C                THEN RW IS SET TO APPROXIMATELY MACHINE PRECISION.
C        AE    - ABSOLUTE ERROR USED IN THE STOPPING CRITERION.  IF THE
C                GIVEN INTERVAL (B,C) CONTAINS THE ORIGIN, THEN A
C                NONZERO VALUE SHOULD BE CHOSEN FOR AE.
C        IFLAG - A STATUS CODE.  USER MUST CHECK IFLAG AFTER EACH CALL.
C                CONTROL RETURNS TO THE USER FROM ZROIN IN ALL CASES.
C                XERROR DOES NOT PROCESS DIAGNOSTICS IN THESE CASES.
C                 1 B IS WITHIN THE REQUESTED TOLERANCE OF A ZERO.
C                   THE INTERVAL (B,C) COLLAPSED TO THE REQUESTED
C                   TOLERANCE, THE FUNCTION CHANGES SIGN IN (B,C), AND
C                   F(X) DECREASED IN MAGNITUDE AS (B,C) COLLAPSED.
C                 2 F(B) = 0.  HOWEVER, THE INTERVAL (B,C) MAY NOT HAVE
C                   COLLAPSED TO THE REQUESTED TOLERANCE.
C                 3 B MAY BE NEAR A SINGULAR POINT OF F(X).
C                   THE INTERVAL (B,C) COLLAPSED TO THE REQUESTED
C                   TOLERANCE AND THE FUNCTION CHANGES SIGN IN (B,C) BUT
C                   F(X) INCREASED IN MAGNITUDE AS (B,C) COLLAPSED,I.E.
C                     ABS(F(B OUT)) .GT. MAX(ABS(F(B IN)),ABS(F(C IN)))
C                 4 NO CHANGE IN SIGN OF F(X) WAS FOUND ALTHOUGH THE
C                   INTERVAL (B,C) COLLAPSED TO THE REQUESTED TOLERANCE.
C                   THE USER MUST EXAMINE THIS CASE AND DECIDE WHETHER
C                   B IS NEAR A LOCAL MINIMUM OF F(X), OR B IS NEAR A
C                   ZERO OF EVEN MULTIPLICITY, OR NEITHER OF THESE.
C                 5 TOO MANY (.GT. 500) FUNCTION EVALUATIONS USED.
C
C     REFERENCES
C       1.  L F SHAMPINE AND H A WATTS, ZEROIN, A ROOT-SOLVING CODE,
C           SC-TM-70-631, SEPT 1970.
C       2.  T J DEKKER, FINDING A ZERO BY MEANS OF SUCCESSIVE LINEAR
C           INTERPOLATION, *CONSTRUCTIVE ASPECTS OF THE FUNDAMENTAL
C           THEOREM OF ALGEBRA*, EDITED BY B DEJON AND P HENRICI, 1969.
C
C     ER IS TWO TIMES THE COMPUTER UNIT ROUNDOFF VALUE WHICH IS
C     DEFINED HERE BY THE FUNCTION D1MACH.
C
C*****precision > double
      ER = 2.0 * D1MACH(4)
C*****END precision > double
C*****precision > single
C      ER = 2.0 * R1MACH(4)
C*****END precision > single
C
C     INITIALIZE
C
      RW = MAX(RE,ER)
      AW = MAX(AE,ZERO)
      IC = 0
      ACBS = ABS(B-C)
      A = C
      T = A
      FA = F(T,IPAR,RPAR)
      T = B
      FB = F(T,IPAR,RPAR)
      FC = FA
      KOUNT = 2
      FX = MAX(ABS(FB),ABS(FC))
C
    1 CONTINUE
      IF (ABS(FC) .LT. ABS(FB)) THEN
C        PERFORM INTERCHANGE
         A  = B
         FA = FB
         B  = C
         FB = FC
         C  = A
         FC = FA
      ENDIF
C
      IF (FB .EQ. ZERO) THEN
         IFLAG = 2
         RETURN
      ENDIF
      CMB  = 0.5*(C-B)
      ACMB = ABS(CMB)
      TOL  = RW*ABS(B)+AW
C
C     TEST STOPPING CRITERION
      IF (ACMB .LE. TOL) GO TO 10
C
C     CALCULATE NEW ITERATE IMPLICITLY AS B+P/Q
C     WHERE WE ARRANGE P .GE. 0.
C     THE IMPLICIT FORM IS USED TO PREVENT OVERFLOW.
      P = (B-A)*FB
      Q = FA - FB
      IF (P .LE. ZERO) THEN
         P = -P
         Q = -Q
      ENDIF
C
C     UPDATE A AND CHECK FOR SATISFACTORY REDUCTION
C     IN THE SIZE OF OUR BOUNDING INTERVAL.
      A  = B
      FA = FB
      IC = IC + 1
      IF (IC .GE. 4) THEN
         IF (8.0*ACMB .GE. ACBS) GO TO 6
         IC   = 0
         ACBS = ACMB
      ENDIF
C
C     TEST FOR TOO SMALL A CHANGE
C
      IF (P .LE. ABS(Q)*TOL) THEN
C
C     INCREMENT BY TOLERANCE
         B = B + SIGN(TOL,CMB)
         GO TO 7
      ENDIF
C
C     ROOT OUGHT TO BE BETWEEN B AND (C+B)/2.0
C
      IF (P .LT. CMB*Q) THEN
C
C     INTERPOLATE
         B = B + P/Q
         GO TO 7
      ENDIF
C
    6 CONTINUE
      B = 0.5*(C+B)
C     BISECT
C
C     HAVE COMPLETED COMPUTATION FOR NEW ITERATE B
    7 CONTINUE
      T = B
      FB = F(T,IPAR,RPAR)
      IF (FB .EQ. ZERO) THEN
         IFLAG = 2
         RETURN
      ENDIF
C
C     DECIDE WHETHER NEXT STEP IS INTERPOLATION OR EXTRAPOLATION
      IF (SIGN(ONE,FB) .EQ. SIGN(ONE,FC)) THEN
         C = A
         FC = FA
      ENDIF
C
      KOUNT = KOUNT + 1
      IF (KOUNT .GT. 500) THEN
         IFLAG = 5
         RETURN
      ENDIF
      GO TO 1
C
C     FINISHED. PROCESS RESULTS FOR PROPER SETTING OF IFLAG
C
   10 CONTINUE
      IF (SIGN(ONE,FB) .EQ. SIGN(ONE,FC)) THEN
         IFLAG = 4
         RETURN
      ENDIF
      IF (ABS(FB) .GT. FX) THEN
         IFLAG = 3
         RETURN
      ENDIF
      IFLAG = 1
      RETURN
      END
C
      SUBROUTINE UPCASE(STR)
      CHARACTER STR*(*), LCASE(26)*1, UCASE(26)*1
      DATA LCASE /'a','b','c','d','e','f','g','h','i','j','k','l','m',
     1            'n','o','p','q','r','s','t','u','v','w','x','y','z'/,
     2     UCASE /'A','B','C','D','E','F','G','H','I','J','K','L','M',
     3            'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
C
      DO 10 L = 1, LEN(STR)
         DO 10 N = 1, 26
            IF (STR(L:L) .EQ. LCASE(N)) STR(L:L) = UCASE(N)
   10 CONTINUE
      RETURN
      END
