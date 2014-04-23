      SUBROUTINE PSRABS
C///////////////////////////////////////////////////////////////////
C
C            PSR: PERFECTLY STIRRED REACTOR
C
C     WRITTEN BY:
C         PETER GLARBORG
C         LABORATORY FOR HEATING AND AIR CONDITIONING
C         TECHNICAL UNIVERSITY OF DENMARK
C         2800 LYNGBY
C         DENMARK
C     AND
C         ROBERT J. KEE
C         COMPUTATIONAL MECHANICS DIVISION
C         SANDIA NATIONAL LABORATORIES
C         LIVERMORE, CA  94550
C         (415) 294-3272
C
C/////////////////////////////////////////////////////////////////////
C
C     VERSION 2.4
C     CHANGES FROM VERSION 1.0:
C     1.  changed REAL*8 to DOUBLE PRECISION
C     2.  read binary file for array lengths
C     3.  allow upper/lower case input
C     CHANGES FROM VERSION 1.1:
C     1.  Chemkin binary file has additional variables KERR,MAXTB
C     CHANGES FROM VERSION 1.2:
C     1.  Modified OPEN statements for unicos
C     CHANGES FOR VERSION 1.4:
C     1.  Modified PSPNT to call CKLEN instead of reading binary file
C     CHANGES FOR VERSION 1.5:
C     1.  Modified read statements for restarts
C     CHANGES FOR VERSION 1.6:
C     1.  Solution file and Restore file have binary file arrays.
C     CHANGES FOR VERSION 1.7
C     1. Do not use binary file information stored in a restart
C        solution, as mechanism may have changed.
C     2. Call list for TWOPNT requires additional input; optional
C        use of new keywords reset default values:
C        'ISTP' n - sets NINIT initial time steps before newton
C                   (default is 0)
C        'IRET' n - set retirement age IRETIR of old time step
C                   (default 50)
C        'NJAC' n - set retirement age NJAC of Jacobian during
C                   steady state newton (default 20)
C        'TJAC' n - set retirement age ITJAC of Jacobian during
C                   time stepping (default 20)
C        'DTMX' x - set maximum time step DTMAX (default 1.0E-4)
C     CHANGES FOR VERSION 1.8
C     1. Correction: set T=S(NT) properly for continuation
C     CHANGES FOR VERSION 1.9
C     1. New Equilibrium code requires changes in PSR calls -
C        CALL EQLEN and CALL EQUIL instead of old CALL EQINIT,EQOPT.
C     CHANGES FOR V.2.0 (2/18/91 F. Rupley per R. Kee)
C     1. In PSRSEN, changed CALL CKRAEX to CALL CKRDEX for
C        perturbation factors used to compute sensitivities.
C     CHANGES FOR V.2.1 (1/27/92 F. Rupley per A. Lutz)
C     1. Upgrade to changes to Equil V.2.2.
C     CHANGES FOR V.2.2 (4/17/92 F. Rupley)
C     1. Correct bug in equilibrium pressure input PQ(1)
C     CHANGES FOR V.2.3 (4/20/92 F. Rupley)
C     1. In CALL EQUIL, the array RHS(KK) must be initialized 0.0.
C     CHANGES FOR VERSION 2.4 (3/15/94 F. Rupley)
C     1.  DOS/PC compatibility effort includes adding file names to
C         OPEN statements, removing unused variables in CALL lists,
C         unusued but possibly initialized variables.
      END
C
C/////////////////////////////////////////////////////////////////////
C
      SUBROUTINE PSR (LIN, LOUT, LINKCK, LREST, LSAVE, LRECOV, LENLWK,
     1                L, LENIWK, I, LENRWK, R, LENCWK, C)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION I(*), R(*)
      LOGICAL   L(*)
      CHARACTER C(*)*(*)
C
      COMMON /PSPSPS/ NCKW, NWT, NFU, NOX, NADD, NSTO, NXIN, NYIN, NHIN,
     1                NXST, NSCR, NABV, NBLW, NBUF, NTWP, NS, NSN, NF,
     2                NFN, NA, NEQ, ICKW, ICC, IKS, IMM, INCF, IIP,
     3                IKPR, IKSP, IEQ, LACT, LIS, LIR
C
C          WRITE VERSION NUMBER
C
      WRITE (LOUT,15)
   15 FORMAT (
     1/' PSR:  Perfectly Stirred Reactor Code',
     2/'       CHEMKIN-II Version 2.4, March 1994',
C*****precision > double
     3/'       DOUBLE PRECISION')
C*****END precision > double
C*****precision > single
C     3/'       SINGLE PRECISION')
C*****END precision > single
C
C          SET UP INTERNAL WORK POINTERS
C
      CALL PSPNT (LINKCK, LOUT, LSAVE, MM, KK, II, NIK, NATJ,
     1            LENIWK, LENRWK, LENCWK, LENTWP, LENIEQ, LENREQ,
     2            LTOT, ITOT, NTOT, ICTOT, NT, NYS, NY, I, R, C)
C
C           CHECK FOR ENOUGH SPACE
C
      WRITE (LOUT, 7000) LENLWK, LTOT, LENIWK, ITOT, LENRWK, NTOT,
     1                   LENCWK, ICTOT
 7000 FORMAT (/,'               WORKING SPACE REQUIREMENTS',
     1        /,'                 PROVIDED        REQUIRED ',
     2        /,' LOGICAL  ', 2I15,
     3        /,' INTEGER  ', 2I15,
     4        /,' REAL     ', 2I15,
     5        /,' CHARACTER', 2I15,/)
C
      IF (LTOT.GT.LENLWK .OR. ITOT.GT.LENIWK .OR. NTOT.GT.LENRWK
     1                   .OR. ICTOT.GT.LENCWK) THEN
         WRITE (LOUT, *) '  FATAL ERROR, NOT ENOUGH WORK SPACE PROVIDED'
         STOP
      ENDIF
C
      CALL PSRDRV (LIN, LOUT, LREST, LSAVE, LRECOV, MM, KK, II,
     1             NIK, NATJ, LENTWP, I(ICKW), R(NCKW), R(NWT),
     2             C(IKS), C(IMM), C(ICC), I(INCF), LENIEQ, LENREQ,
     3             I(IEQ), R(NEQ), R(NFU), R(NOX),  R(NADD), R(NSTO),
     4             I(IKPR),I(IKSP),L(LIS), L(LIR),  I(IIP),  R(NXIN),
     5             R(NYIN),R(NHIN),R(NXST),R(NSCR), R(NS),   R(NSN),
     6             R(NF),  R(NFN), R(NA),  L(LACT), R(NABV), R(NBLW),
     7             R(NBUF),R(NTWP),NT, NYS, NY)
C
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE PSPNT (LINKCK, LOUT, LSAVE, MM, KK, II, NIK,
     1                  NATJ, LENIWK, LENRWK, LENCWK, LENTWP, LENIEQ,
     2                  LENREQ, LTOT, ITOT, NTOT, ICTOT, NT, NYS, NY,
     3                  I, R, C)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION I(*), R(*)
      CHARACTER C(*)*(*)
C
      COMMON /PSPSPS/ NCKW, NWT, NFU, NOX, NADD, NSTO, NXIN, NYIN, NHIN,
     1                NXST, NSCR, NABV, NBLW, NBUF, NTWP, NS, NSN, NF,
     2                NFN, NA, NEQ, ICKW, ICC, IKS, IMM, INCF, IIP,
     3                IKPR, IKSP, IEQ, LACT, LIS, LIR
C
      CALL CKLEN (LINKCK, LOUT, LENICK, LENRCK, LENCCK)
      IF (LENICK.LT.LENIWK .AND. LENRCK.LT.LENRWK .AND.
     1    LENCCK.LT.LENCWK) THEN
          CALL CKINIT (LENICK, LENRCK, LENCCK, LINKCK, LOUT, I, R, C)
          CLOSE (LINKCK)
          CALL CKINDX (I, R, MM, KK, II, NFIT)
          REWIND LSAVE
          CALL PSSAVE (I, R, C, LOUT, LSAVE)
      ENDIF
C
      NCON = 0
      CALL EQLEN (MM, KK, NCON, LENIEQ, LENREQ)
C
      NATJ = KK+1
C
C          SET THE POINTERS INTO THE SOLUTION VECTOR
C
      NT  = 1
      NYS = 1
      NY  = 2
C
      LENTWP = 7*NATJ + (7*NATJ + 2)
C
C          APPORTION THE FLOATING POINT SPACE
C
      NCKW = 1
      NWT  = NCKW + LENRCK
      NFU  = NWT  + KK
      NOX  = NFU  + KK
      NADD = NOX  + KK
      NSTO = NADD + KK
      NXIN = NSTO + KK
      NYIN = NXIN + KK
      NHIN = NYIN + KK
      NXST = NHIN + KK
      NSCR = NXST + KK
                    NIK = MAX(KK,II)
      NABV = NSCR + NIK * 8
      NBLW = NABV + NATJ
      NBUF = NBLW + NATJ
      NTWP = NBUF + NATJ
      NS   = NTWP + LENTWP
      NSN  = NS   + NATJ
      NF   = NSN  + NATJ
      NFN  = NF   + NATJ
      NA   = NFN  + NATJ
      NEQ  = NA   + NATJ * NATJ
      NTOT = NEQ + LENREQ
C
C           APPORTION THE INTEGER SPACE
C
      ICKW = 1
      INCF = ICKW + LENICK
      IIP  = INCF + MM * KK
      IKPR = IIP  + NATJ
      IKSP = IKPR + KK
      IEQ  = IKSP + KK
      ITOT = IEQ  + LENIEQ
C
C           APPORTION THE LOGICAL SPACE
C
      LACT = 1
      LIS  = LACT + NATJ
      LIR  = LIS  + KK
      LTOT = LIR  + KK
C
C           APPORTION THE CHARACTER SPACE
C
      ICC = 1
      IKS = ICC + LENCCK
      IMM = IKS + KK
      ICTOT = IMM + MM - 1
C
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE PSRDRV (LIN, LOUT, LREST, LSAVE, LRECOV, MM, KK, II, 
     1                   NIK, NATJ, LENTWP, ICKWRK, RCKWRK, WT, KSYM, 
     2                   ATOM, CCKWRK, NCF, LENIEQ, LENREQ, IEQWRK, 
     3                   REQWRK, FUEL, OXID, ADD, STOICH, KPROD, KSP, 
     4                   ISEN, IROP, IPVT, XIN, YIN, HIN, X, SCRTCH, 
     5                   S, SN, F, FN, A, ACTIVE, ABOVE, BELOW, 
     6                   BUFFER, TWPWK, NT, NYS, NY)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION ICKWRK(*), RCKWRK(*), WT(*), NCF(MM,*), IEQWRK(*),
     1          REQWRK(*), FUEL(*), OXID(*), ADD(*), STOICH(*),
     2          KPROD(*), KSP(*), IPVT(*), XIN(*), YIN(*), HIN(*),
     3          X(*), SCRTCH(NIK,*), S(*), SN(*), F(*), FN(*),
     4          A(NATJ,*), ABOVE(*), BELOW(*), BUFFER(*), TWPWK(*),
     6          LEVEL(2)
C
      CHARACTER KSYM(*)*(*), ATOM(*)*(*), CCKWRK(*)*(*)
C
      LOGICAL ISEN(*), IROP(*), ACTIVE(*), LENRGY, LFLRT,
     1        LEQUIV, LRSTRT, LCNTUE, RSTCNT, LSEN, LSENT, LROP,
     2        LPRTIC, KERR, IERR
C
      CHARACTER*16 ISOLUT, ICHR, ICKLNK, IROPRO
      DATA ISOLUT/'SOLUTION        '/, ICKLNK/'CKLINK          '/,
     1     IROPRO/'PRODUCTION RATE '/, LCNTUE/.FALSE./,
     2     KERR/.FALSE./
C
C          COMPUTE THE UNIT ROUNDOFF, AND THE RELATIVE AND ABSOLUTE
C            PERTURBATIONS FOR THE JACOBIAN EVALUATION.
C
      U = 1.0
   30 CONTINUE
      U = U*0.5
      COMP = 1.0 + U
      IF (COMP .NE. 1.0) GO TO 30
      ABSOL = SQRT(2.0*U)
      RELAT = SQRT(2.0*U)
C
      RSTCNT = .FALSE.
C
C         INITIALIZE CHEMKIN
C
      CALL CKSYMS (CCKWRK, LOUT, KSYM, IERR)
      KERR = KERR.OR.IERR
      CALL CKSYME (CCKWRK, LOUT, ATOM, IERR)
      KERR = KERR.OR.IERR
      CALL CKWT   (ICKWRK, RCKWRK, WT)
      CALL CKNCF  (MM, ICKWRK, RCKWRK, NCF)
      CALL CKRP   (ICKWRK, RCKWRK, RU, RUC, PATM)
C
C         RETURN HERE FOR A CONTINUATION PROBLEM
C
200   CONTINUE
C
C         CALL THE KEYWORD INPUT
C
      CALL PSRKEY (LIN, LOUT, MM, KK, NATJ, ICKWRK, RCKWRK, KSYM, 
     1             ATOM, NCF, LENRGY, LFLRT, LEQUIV, LRSTRT, LCNTUE, 
     2             MFILE, IPRNT, LSEN, LSENT, ISEN, EPSS, EPST, LROP,
     3             IROP, EPSR, TIN, XIN, T, X, EQUIV, PATM, PA, P, TAU,
     4             FLRT, V, Q, FUEL, OXID, ADD, STOICH, KPROD, KSP,
     5             ATOL, RTOL, ATIM, RTIM, NUMDT, DT1, NUMDT2, DT2,
     6             SFLR, UFAC, DFAC, DTMIN, LENIEQ, LENREQ, IEQWRK,
     7             REQWRK, IPVT, SCRTCH(1,1), A, 
     8             NINIT, NJAC, ITJAC, DTMAX, IRETIR)
C
C         SET THE SOLUTION BOUNDS
C
      BELOW(NT) = 200.
      ABOVE(NT) = 6000.0E0
      DO 100 K = 1, KK
         BELOW (NYS+K) = SFLR
         ABOVE (NYS+K) = 1.01
100   CONTINUE
C
      IF (LENRGY) CALL CKHMS (TIN, ICKWRK, RCKWRK, HIN)
      CALL CKXTY (XIN, ICKWRK, RCKWRK, YIN)
C
      IF (.NOT. LRSTRT) THEN
         CALL CKXTY (X, ICKWRK, RCKWRK, S(NY))
         S(NT) = T
      ENDIF
C
C*****precision > double
      CALL DCOPY (NATJ, S, 1, SN, 1)
C*****END precision > double
C*****precision > single
C      CALL SCOPY (NATJ, S, 1, SN, 1)
C*****END precision > single
C
      IF (IPRNT .LT. 10) THEN
         LEVEL(1) = MIN (2, IPRNT)
         LEVEL(2) = LEVEL(1)
      ELSE
         LEVEL(1) = IPRNT/10
         LEVEL(2) = IPRNT - 10*LEVEL(1)
         LEVEL(1) = MAX ( LEVEL(1), LEVEL(2) )
      ENDIF
C
      IF (LRSTRT) THEN
C
         IF (.NOT. RSTCNT) THEN
C
C           THIS IS A RESTART
C
            NREST = 0
  320       CONTINUE
            READ (LREST, END=350, ERR=350) ICHR
            IF (ICHR .EQ. ICKLNK) THEN
               DO 321 L = 1, 4
                  READ (LREST, END=350, ERR=350)
  321          CONTINUE
            ELSEIF (ICHR .EQ. ISOLUT) THEN
               READ (LREST, END=350, ERR=350) NNNN
               IF (NNNN .NE. NATJ) THEN
                 WRITE (LOUT, *)
     1           ' FATAL ERROR, INCOMPATIBLE RESTART FILE'
                 STOP
               ENDIF
               READ (LREST, END=350, ERR=350)
               READ (LREST, END=350, ERR=350)
     1               DUM1,(SCRTCH(M,1),M=1,KK)
               READ (LREST, END=350, ERR=350) (S(N), N=1,NNNN)
               NREST = NREST + 1
               IF (NREST .EQ. MFILE) GO TO 350
            ELSEIF (ICHR .EQ. IROPRO) THEN
               DO 315 K = 1, KK
                  READ (LREST, END=350, ERR=350)
  315          CONTINUE
            ELSE
               WRITE (LOUT, *)
     1                ' FATAL ERROR, NOT A SOLUTION ON RESTART FILE'
               STOP
            ENDIF
            GO TO 320
  350       CONTINUE
            CLOSE (LSAVE)
            IF (NREST .NE. MFILE) THEN
               WRITE (LOUT, *) ' Error reading solution file...'
               STOP
            ENDIF
         ENDIF
         IF (LENRGY) THEN
            T = S(NT)
         ELSE
            S(NT) = T
         ENDIF
      ENDIF
C
C        CALL THE DRIVER TO TWOPNT
C
      CALL PSRTWO (LOUT, LRECOV, KK, NATJ, LENRGY, LFLRT, LEQUIV, 
     1             LENTWP, LEVEL, ACTIVE, ICKWRK, RCKWRK, CCKWRK, 
     2             WT, KSYM, ATIM, RTIM, ATOL, RTOL, TIN, XIN, YIN, 
     3             HIN, ABSOL, RELAT, ABOVE, BELOW, BUFFER, TWPWK,
     4             IPVT, SCRTCH, T, X, S, SN, F, FN, A, NUMDT, 
     6             DT1, DT2, UFAC, DFAC, DTMIN, P, PA, TAU, FLRT,
     7             V, Q, EQUIV, NT, NYS, NY, NINIT, NJAC, ITJAC, DTMAX, 
     8             IRETIR)
C
C           PRINT FINAL SOLUTION, IF NO PRINTING THROUGHOUT ITERATION
C
      IF (IPRNT .EQ. 0) THEN
         LPRTIC = .TRUE.
         CALL PSPRNT (KK, LOUT, KSYM, LENRGY, LFLRT, LEQUIV,
     1                LPRTIC, EQUIV, PA, P, TAU, FLRT, V, Q, TIN,
     2                XIN, T, X, S, ICKWRK, RCKWRK, NT, NY)
      ENDIF
C
C              WRITE TO LSAVE WHEN SOLUTION IS COMPLETE
C
      WRITE (LSAVE) ISOLUT
      WRITE (LSAVE) NATJ
      WRITE (LSAVE) EQUIV, P, TAU, FLRT, V, Q
      WRITE (LSAVE) TIN, (XIN(M), M=1,KK)
      WRITE (LSAVE) (S(N), N=1,NATJ)
C
      IF (LSEN .OR. LSENT) THEN
C
         CALL PSRSEN (LOUT, LSAVE, KK, II, NATJ, KSYM, CCKWRK,
     1                LENRGY, LFLRT, ABSOL, RELAT, DT, WT, HIN, YIN,
     2                P, TAU, FLRT, V, Q, T, S, SN, F, FN, A,
     3                LSENT, ISEN, EPSS, EPST, ICKWRK, RCKWRK,
     4                IPVT, SCRTCH, NT, NYS, NY)
C
         IF (LEVEL(2) .GT. 0) WRITE (LOUT,'(/A/)')
     1      ' SENSITIVITY CALCULATION COMPLETE'
C
      ENDIF
C
      IF (LROP) THEN
C
         CALL PSRROP (LOUT, LSAVE, KK, II, KSYM, P, S, ICKWRK, 
     1                RCKWRK, CCKWRK, IROP, EPSR, SCRTCH(1,1), 
     2                SCRTCH(1,2), SCRTCH(1,3), NT, NY)
C
         IF (LEVEL(2) .GT. 0) WRITE (LOUT,'(/A/)')
     1      ' RATE-OF-PRODUCTION CALCULATION COMPLETE'
C
      ENDIF
C
C            CHECK FOR CONTINUATION
C
      IF (LCNTUE) THEN
C
         WRITE (LOUT,'(/////)')
         DO 1210 L = 1, 5
            WRITE (LOUT,*)
     1    ' ////////////////// CONTINUING TO NEW PROBLEM /////////////'
1210     CONTINUE
         WRITE (LOUT,'(/////)')
C
         RSTCNT = .TRUE.
         LRSTRT = .TRUE.
         GO TO 200
C
      ENDIF
C
      STOP
      END
C
C---------------------------------------------------------------
C
      SUBROUTINE PSSAVE (ICKWRK, RCKWRK, CCKWRK, LOUT, LSAVE)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION ICKWRK(*), RCKWRK(*)
      CHARACTER*(*) CCKWRK(*)
      CHARACTER*16 ILINK
C
      ILINK = 'CKLINK          '
      WRITE (LSAVE) ILINK
      CALL CKSAVE (LOUT, LSAVE, ICKWRK, RCKWRK, CCKWRK)
C
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE PSRTWO (LOUT, LRECOV, KK, NATJ, LENRGY, LFLRT, LEQUIV, 
     1                   LENTWP, LEVEL, ACTIVE, ICKWRK, RCKWRK, CCKWRK, 
     2                   WT, KSYM, ATIM, RTIM, ATOL, RTOL, TIN, XIN, 
     3                   YIN, HIN, ABSOL, RELAT, ABOVE, BELOW, BUFFER, 
     4                   TWPWK, IPVT, SCRTCH, T, X, S, SN, F, FN, A, 
     5                   NUMDT, DT1, DT2, UFAC, DFAC, 
     6                   DTMIN, P, PA, TAU, FLRT, V, Q, EQUIV, NT, NYS, 
     7                   NY, NINIT, NJAC, ITJAC, DTMAX, IRETIR)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION LEVEL(2), ICKWRK(*), RCKWRK(*), WT(*), XIN(*), YIN(*),
     1          HIN(*), ABOVE(*), BELOW(*), BUFFER(*), TWPWK(*),
     2          IPVT(*), SCRTCH(KK,*), ITWPWK(3), X(*), S(*), SN(*),
     3          F(*), FN(*), A(NATJ,*)
C
      INTEGER CALL, CALLS
      CHARACTER*(*) CCKWRK(*), KSYM(*)
C
      LOGICAL ACTIVE(*), MARK(1), LENRGY, LFLRT, LEQUIV, LPRTIC,
     1        ERROR, FUNCTN, JACOBN, REENTR, SOLVE, STORE, SUCCES,
     2        LTIME, ADAPT, SHOW, SAVE, UPDATE, ENERGY
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C INPUT-
C   LOUT   - UNIT FOR PRINTED OUTPUT
C   LRECOV - UNIT TO WHICH THE RECOVER FILE IS WRITTEN
C   KK     - NUMBER OF CHEMICAL SPECIES.
C   NATJ   - NUMBER OF DEPENDENT VARIABLES.  NATJ=KK+1.
C   LENRGY - IF LENRGY=.TRUE. THEN THE ENERGY EQUATION IS TO BE
C            SOLVED, OTHERWISE A FIXED TEMPERATURE IS USED.
C   LFLRT  - IF LFLRT=.TRUE. THEN THE MASS FLOW RATE IS SPECIFIED AND
C              THE RESIDENCE TIME HAS TO BE CALCULATED.
C            IF LFLRT=.FALSE. THEN THE RESIDENCE TIME IS SPECIFIED AND
C              THE MASS FLOW RATE HAS TO BE CALCULATED.
C   LEQUIV - IF LEQUIV=.TRUE. THEN THE INLET COMPOSITION IS DEFINED BY
C              THE FUEL EQUIVALENCE RATIO, THE FUEL AND OXIDIZER
C              COMPOSITIONS AND THE PRODUCTS.
C            IF LEQUIV=.FALSE. THEN THE INLET COMPOSITION IS DEFINED
C              DIRECTLY BY THE USER.
C            LEQUIV AND EQUIV ARE USED ONLY FOR PRINTING.  THEREFORE, IF
C            PSRTWO IS CALLED OUTSIDE OF PSR, THEN EQUIV MUST BE GIVEN
C            IF LEQUIV=.TRUE.
C   LEVEL  - LEVEL(1) CONTROLS THE PRINTED OUTPUT FROM TWOPNT AND
C            LEVEL(2) CONTROLS THE PRINTING FROM PSR.  LEVEL(1) MUST
C            ALWAYS BE GREATER THAN OR EQUAL TO LEVEL(2).  LEVEL MAY
C            HAVE VALUES OF 0, 1, 2, OR 3.
C              DIMENSION LEVEL(2).
C   ACTIVE - ACTIVE(*) IS A LOGICAL ARRAY THAT CONTROLS WHICH VARIABLES
C            TWOPNT WILL ATTEMPT ADAPTIVE MESHING.  FOR THE PSR PROBLEM
C            THERE IS NO MESH, THEREFORE ALL ACTIVE(*)=.FALSE.
C              LOGICAL ACTIVE(KK).
C   WT     - THE ARRAY OF SPECIES MOLECULAR WEIGHTS.
C              CGS UNITS - GM/MOLE
C              DIMENSION WT(*) AT LEAST KK.
C   KSYM   - CHEMKIN SPECIES NAMES.
C              DIMENSION KSYM AT LEAST KK.
C   ATIM   - ABSOLUTE CONVERGENCE CRITERIA FOR THE NEWTON ITERATION
C            AS USED FOR THE TIME STEPS.
C   ATOL   - ABSOLUTE CONVERGENCE CRITERIA FOR THE NEWTON ITERATION.
C   RTIM   - RELATIVE CONVERGENCE CRITERIA FOR THE NEWTON ITERATION
C            AS USED FOR THE TIME STEPS.
C   RTOL   - RELATIVE CONVERGENCE CRITERIA FOR THE NEWTON ITERATION.
C   TIN    - INLET TEMPERATURE.
C              CGS UNITS - K
C   XIN    - ARRAY OF INLET SPECIES MOLE FRACTIONS. USED FOR PRINTING.
C              DIMENSION XIN(*) AT LEAST KK.
C   YIN    - ARRAY OF INLET SPECIES MASS FRACTIONS.
C              DIMENSION YIN(*) AT LEAST KK.
C   HIN    - ARRAY OF SPECIES ENTHALPIES AT INLET TEMPERATURE TIN.
C              CGS UNITS - ERGS/MOLE
C              DIMENSION HIN(*) AT LEAST KK.
C   ABSOL  - ABSOLUTE PERTURBATION FOR COMPUTING JACOBIAN.
C   RELAT  - RELATIVE PERTURBATION FOR COMPUTING JACOBIAN.
C   ABOVE  - ARRAY OF UPPER BOUNDS FOR THE DEPENDENT VARIABLES.  USED
C            BY TWOPNT TO CONTROL THE DAMPING.  ABOVE HAS THE SAME
C            STRUCTURE AS THE SOLUTION VECTOR S(*).  ABOVE(*) SHOULD BE
C            SET TO VALUES THAT ARE ABOVE ACCEPTABLE SOLUTION VALUES.
C              DIMENSION ABOVE(*) AT LEAST NATJ.
C   BELOW  - ARRAY OF LOWER BOUNDS FOR THE DEPENDENT VARIABLES.  USED
C            BY TWOPNT TO CONTROL THE DAMPING.  BELOW HAS THE SAME
C            STRUCTURE AS THE SOLUTION VECTOR S(*).  BELOW(*) SHOULD BE
C            SET TO VALUES THAT ARE BELOW ACCEPTABLE SOLUTION VALUES.
C              DIMENSION BELOW(*) AT LEAST NATJ.
C   T      - THE TEMPERATURE. FOR FIXED-TEMPERATURE PROBLEMS THIS
C            IS THE USER-SPECIFIED TEMPERATURE.
C              CGS UNITS - K
C   S      - DEPENDENT VARIABLE ARRAY.  ON INPUT THE SOLUTION ESTIMATE
C            IS IN S.  THE TEMPERATURE IS STORED IN T=S(NT), AND THE
C            MASS FRACTIONS ARE IN Y(K)=S(NYS+K).
C              DIMENSION S(*) AT LEAST NATJ.
C   NUMDT  - THE NUMBER OF TIME STEPS TO BE TAKEN UPON FAILURE OF A
C            NEWTON ITERATION.  NUMDT IS IN EFFECT WHEN THE TEMPERATURE-
C            FIXED PROBLEM IS BEING SOLVED.
C   NUMDT2 - THE NUMBER OF TIME STEPS TO BE TAKEN UPON FAILURE OF A
C            NEWTON ITERATION.  NUMDT2 IS IN EFFECT WHEN THE ENERGY
C            EQUATION IS BEING SOLVED.
C   DT1    - THE VALUE OF THE TIMESTEP THAT IS TAKEN WHEN THE NEWTON
C            ITERATION FAILS IN THE TEMPERATURE-FIXED PROBLEM.
C              CGS UNITS - SEC
C   DT2    - THE VALUE OF THE TIMESTEP THAT IS TAKEN WHEN THE NEWTON
C            ITERATION FAILS WHEN THE ENERGY EQUATION IS BEING SOLVED.
C              CGS UNITS - SEC
C   P      - THE PRESSURE.
C              CGS UNITS - DYNES/CM**2
C   PA     - THE PRESSURE.
C              CGS UNITS - ATMOSPHERES
C   TAU    - THE NOMINAL RESIDENCE TIME OF THE REACTOR
C              CGS UNITS - SEC
C   FLRT   - THE MASS FLOW RATE.
C              CGS UNITS - GM/SEC
C   V      - THE VOLUME OF THE REACTOR
C              CGS UNITS - CM**3
C   Q      - THE HEAT LOSS OF THE REACTOR
C              CGS UNITS - ERGS/SEC
C   EQUIV  - FUEL/OXIDIZER EQUIVALENCE RATIO.
C   NT     - POINTER TO THE TEMPERATURE IN THE SOLUTION AND
C            RESIDUAL VECTORS.  S(NT) = TEMPERATURE
C   NYS    - POINTER TO THE SOLUTION ARRAY.  S(NYS+K) IS THE MASS
C            FRACTION OF THE KTH SPECIES.
C   NY     - POINTER TO THE SOLUTION ARRAY.  S(NY) IS THE MASS
C            FRACTION OF THE 1ST SPECIES.  USED IN SUBROUTINE CALLS.
C
C WORK AND SCRATCH SPACE-
C   RCKWRK - FLOATING POINT CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   ICKWRK - INTEGER CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   BUFFER - WORK SPACE INTO WHICH TWOPNT WRITES DATA AND THROUGH WHICH
C            DATA IS RETURNED TO TWOPNT THROUGH THE REVERSE
C            COMMUNICATION INTERFACE.  SEE TWOPNT DOCUMENTATION.
C              DIMENSION BELOW(*) AT LEAST NATJ.
C   TWPWK  - WORK SPACE FOR TWOPNT.
C              DIMENSION WNEWTN(LENTWP).
C   IPVT   - ARRAY OF PIVOTS USED BY LINPACK LU FACTORIZATION AND SOLVE
C            ROUTINES.
C              DIMENSION IPVT(*) AT LEAST NATJ.
C   SCRTCH - SCRATCH SPACE USED BY THE FUNCTION, JACOBIAN, AND PRINT.
C              DIMENSION SCRTCH(KK,5)
C   SN     - DEPENDENT VARIABLE ARRAY AT PREVIOUS TIMESTEP. THE
C            STRUCTURE IS THE SAME AS S.
C              DIMENSION SN(*) AT LEAST NATJ.
C   F      - RESIDUALS OF THE GOVERNING EQUATIONS AS EVALUATED AT
C            S(N).  THE RESIDUAL OF THE KK SPECIES EQUATIONS IS IN
C            F(NYS+K), THE ENERGY EQUATION IN F(NT,J).  IF
C            INERGY=.FALSE. THEN THE ENERGY EQUATION IS REPLACED
C            BY THE GIVEN TEMPERATURE.
C               DIMENSION F(*) AT LEAST NATJ.
C   A      - STORAGE SPACE FOR THE JACOBIAN MATRIX, AND ITS LU FACTORS.
C               DIMENSION A(NATJ,NATJ)
C
C OUTPUT-
C   X      - ARRAY OF MOLE FRACTIONS AT THE SOLUTION.
C              DIMENSION X(*) AT LEAST KK.
C   S      - DEPENDENT VARIABLE ARRAY.  ON OUTPUT S(*) CONTAINS THE
C            SOLUTION.  THE TEMPERATURE IS STORED IN T=S(NT), AND THE
C            MASS FRACTIONS ARE IN Y(K)=S(NYS+K)
C              DIMENSION S(*) AT LEAST NATJ.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      CHARACTER*16 ISOLUT
      DATA ISOLUT/'SOLUTION        '/, NMAX,JJ/2*1/,
     1     GRAD, CURV, ADAFLR /3*1.0/, ADAPT/.FALSE./
C
      DO 20 N = 1, NATJ
         ACTIVE(N) = .FALSE.
   20 CONTINUE
C
C        SET VDT FOR THE TEMPERATURE FIXED PROBLEM
C
      VDT = DT1
C
C          DECIDE HOW MANY TIMES TO CALL TWOPNT
C
      ENERGY = LENRGY
      IF (ENERGY) THEN
         CALLS = 2
      ELSE
         CALLS = 1
      ENDIF
C
C           RETURN TO PRINTING THE INLET CONDITIONS, THEY ARE TURNED OFF
C            AFTER BEING PRINTED ONCE BY PRINT.
C
      LPRTIC = .TRUE.
C
C          TOP OF THE LOOP OVER CALLS TO TWOPNT.
C
      DO 1000 CALL = 1, CALLS
C
         IF (CALL.EQ.2 .AND. LEVEL(2).GT.0) WRITE (LOUT,'(/A/)')
     1   '   PSRDRV: FINISHED FIXED TEMPERATURE, ADDING ENERGY EQUATION'
C
         IF (ENERGY) LENRGY = (CALL .NE. 1)
         IF (CALL .GT. 1) VDT = DT2
         REENTR = .FALSE.
         IPASSS = 1

500      CONTINUE
C
         CALL TWOPNT (ERROR, LOUT, LEVEL, 3, ITWPWK, LENTWP, TWPWK,
     1                ABOVE, ACTIVE, ADAPT, BELOW, 0, BUFFER, NATJ,
     2                CONDIT, FUNCTN, JACOBN, MARK, X, IPASS, IPASSS,
     3                NADP, NMAX, JJ, REENTR, IREPRT, SAVE, 0,
     4                SHOW, SOLVE, ATOL, NJAC, RTOL, NINIT, NUMDT,
     5                IRETIR, STORE, SUCCES, ATIM, ITJAC, DFAC, RTIM,
     6                LTIME, UFAC, DTMAX, DTMIN, ADAFLR, GRAD, CURV,
     7                VDT, DT, UPDATE, S)
C
         IF (ERROR) THEN
            STOP
         ELSEIF (.NOT.SUCCES .AND. .NOT.REENTR) THEN
            IF (IREPRT .EQ. 2) WRITE (LOUT,*)
     1      '     TWOPNT requires more mesh points, but NMAX too small'
C
C
         ELSEIF (REENTR) THEN
C
            IF (FUNCTN) THEN
C
               CALL PSRFUN (KK, LTIME, LENRGY, LFLRT, DT, HIN,
     1                      YIN, T, P, TAU, FLRT, V, Q, ICKWRK,
     2                      RCKWRK, WT, SCRTCH(1,1), SCRTCH(1,2), SN,
     3                      BUFFER, F, NT, NYS, NY)
C
C*****precision > double
               CALL DCOPY (NATJ, F, 1, BUFFER, 1)
C*****END precision > double
C
C*****precision > single
C               CALL SCOPY (NATJ, F, 1, BUFFER, 1)
C*****END precision > single
C
            ELSEIF (JACOBN) THEN
C
               CALL  PSRJAC (KK, NATJ, LTIME, LENRGY, LFLRT, ABSOL,
     1                       RELAT, DT, HIN, YIN, T, P, TAU, FLRT,
     2                       V, Q, ICKWRK, RCKWRK, WT, SCRTCH, SN, S,
     3                       F, FN, A, NT, NYS, NY)
C
C*****precision > double
               CALL DGECO (A, NATJ, NATJ, IPVT, RCOND, FN)
C*****END precision > double
C
C*****precision > single
C               CALL SGECO (A, NATJ, NATJ, IPVT, RCOND, FN)
C*****END precision > single
C
               IF (RCOND .LE. 0.0E0) THEN
                  WRITE (LOUT,*) ' FATAL ERROR, SINGULAR JACOBIAN '
                  STOP
               ENDIF
               CONDIT = 1.0 / RCOND
C
            ELSEIF (SOLVE) THEN
C*****precision > double
               CALL DGESL (A, NATJ, NATJ, IPVT, BUFFER, 0)
C*****END precision > double
C
C*****precision > single
C               CALL SGESL (A, NATJ, NATJ, IPVT, BUFFER, 0)
C*****END precision > single
C
            ELSEIF (STORE) THEN
C
C*****precision > double
               CALL DCOPY (NATJ, BUFFER, 1, SN, 1)
C*****END precision > double
C
C*****precision > single
C               CALL SCOPY (NATJ, BUFFER, 1, SN, 1)
C*****END precision > single
C
            ELSEIF (SHOW) THEN
               CALL PSPRNT (KK, LOUT, KSYM, LENRGY, LFLRT,
     1                      LEQUIV, LPRTIC, EQUIV, PA, P, TAU, FLRT, V,
     2                      Q, TIN, XIN, T, X, BUFFER, ICKWRK, RCKWRK,
     3                      NT, NY)
C
            ELSEIF (SAVE) THEN
               REWIND LRECOV
               CALL PSSAVE (ICKWRK, RCKWRK, CCKWRK, LOUT,
     1                      LRECOV)
               WRITE (LRECOV) ISOLUT
               WRITE (LRECOV) NATJ
               WRITE (LRECOV) EQUIV, P, TAU, FLRT, V, Q
               WRITE (LRECOV) TIN, (XIN(M), M=1,KK)
               WRITE (LRECOV) (BUFFER(N), N=1,NATJ)
C
            ELSEIF (UPDATE) THEN
               WRITE (LOUT,*)
     1          '  FATAL ERROR, UPDATE=.TRUE. IS RETURNED BY TWOPOINT'
               STOP
            ENDIF
C
            GO TO 500
C
         ENDIF
C
         IF (.NOT. SUCCES) STOP
C
1000  CONTINUE
C
      RETURN
      END
C
C------------------------------------------------------------
C
      SUBROUTINE PSRJAC (KK, NATJ, LTIME, LENRGY, LFLRT, ABSOL, RELAT,
     1                   DT, HIN, YIN, T, P, TAU, FLRT, V, Q,
     2                   ICKWRK, RCKWRK, WT, SCRTCH, SN, S, F, FN, A,
     3                   NT, NYS, NY)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION S(*), F(*), FN(*), A(NATJ,*), SN(*), SCRTCH(KK,*),
     1          WT(*), HIN(*), YIN(*), ICKWRK(*), RCKWRK(*)
      LOGICAL LENRGY, LFLRT, LTIME
C
C            ZERO THE MATRIX STORAGE SPACE.
C
      ZERO = 0.0
C*****precision > double
      CALL DCOPY (NATJ * NATJ, ZERO, 0, A, 1)
C*****END precision > double
C
C*****precision > single
C      CALL SCOPY (NATJ * NATJ, ZERO, 0, A, 1)
C*****END precision > single
C
C            CALL THE FUNCTION AT S AND STORE IN FN.
C
      CALL PSRFUN  (KK, LTIME, LENRGY, LFLRT, DT, HIN, YIN,
     1              T, P, TAU, FLRT, V, Q, ICKWRK, RCKWRK, WT,
     2              SCRTCH(1,1), SCRTCH(1,2), SN, S, FN, NT, NYS, NY)
C
C        TOP OF THE LOOPS OVER THE RESIDUE CLASSES AND
C                                              SOLUTION COMPONENTS.
C
      DO 0200 M = 1, NATJ
C
C            FOR A GIVEN RESIDUE CLASS AND A GIVEN SOLUTION COMPONENT,
C            PERTRB THE S VECTOR AT POINTS IN THE SAME RESIDUE CLASS.
C
         SAVE   = S(M)
         PERTRB = ABS(S(M)) * RELAT + ABSOL
         S(M)   = S(M) + PERTRB
C
C             CALL THE FUNCTION AT THE PERTURBED S AND STORE
C                                                  THE RESULT IN F.
C
         CALL PSRFUN (KK, LTIME, LENRGY, LFLRT, DT, HIN, YIN,
     1                T, P, TAU, FLRT, V, Q, ICKWRK, RCKWRK, WT,
     2                SCRTCH(1,1), SCRTCH(1,2), SN, S, F, NT, NYS, NY)
C
C              RESTORE S TO ITS ORIGINAL VALUE.
C
         S(M) = SAVE
C
C              DIFFERENCE TO GET THE COLUMNS OF THE JACOBIAN.
C
         DO 0100 N = 1, NATJ
            A(N, M) = (F(N) - FN(N)) / PERTRB
  100    CONTINUE
C
C          BOTTOM OF THE LOOPS OVER THE RESIDUE CLASSES AND SOLUTION
C          COMPONENTS.
C
  200 CONTINUE
C
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE PSRFUN (KK, LTIME, LENRGY, LFLRT, DT, HIN, YIN,
     1                   T, P, TAU, FLRT, V, Q, ICKWRK, RCKWRK, WT,
     2                   WDOT, H, SN, S, F, NT, NYS, NY)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION S(*), SN(*), F(*), WT(*), H(*), HIN(*),
     1          YIN(*), WDOT(*), ICKWRK(*), RCKWRK(*)
C
      LOGICAL LENRGY, LFLRT, LTIME
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   DT     - THE TIME STEP THAT IS USED IF LTIME=.TRUE.
C              CGS UNITS - SEC
C   FLRT   - THE MASS FLOW RATE.
C              CGS UNITS - GM/SEC
C   LTIME  - IF LTIME=.TRUE.  A TIME STEP OF DT WILL BE ADDED INTO
C            THE RESIDUAL.
C   HIN    - ARRAY OF SPECIES ENTHALPIES AT INLET TEMPERATURE.
C              CGS UNITS - ERGS/MOLE
C              DIMENSION H(*) AT LEAST KK.
C   KK     - NUMBER OF CHEMICAL SPECIES.
C   LENRGY - IF LENRGY=.TRUE. THEN THE ENERGY EQUATION IS TO BE
C            SOLVED, OTHERWISE A FIXED TEMPERATURE IS USED.
C   LFLRT  - IF LFLRT=.TRUE. THEN THE MASS FLOW RATE IS SPECIFIED AND
C              THE RESIDENCE TIME HAS TO BE CALCULATED.
C            IF LFLRT=.FALSE. THEN THE RESIDENCE TIME IS SPECIFIED AND
C              THE MASS FLOW RATE HAS TO BE CALCULATED.
C   P      - THE PRESSURE.
C              CGS UNITS - DYNES/CM**2
C   Q      - THE HEAT LOSS OF THE REACTOR
C              CGS UNITS - ERGS/SEC
C   S      - DEPENDENT VARIABLE ARRAY. THE TEMPERATURE IS STORED IN
C            T=S(NT), AND THE MASS FRACTIONS ARE IN Y(K)=S(NYS+K)
C              DIMENSION S(*) AT LEAST NATJ.
C   SN     - DEPENDENT VARIABLE ARRAY AT PREVIOUS TIMESTEP.
C   T      - THE TEMPERATURE. FOR FIXED-TEMPERATURE PROBLEMS THIS
C            IS THE USER-SPECIFIED TEMPERATURE.
C              CGS UNITS - K
C   TAU    - THE NOMINAL RESIDENCE TIME OF THE REACTOR
C              CGS UNITS - SEC
C   TIN    - THE INLET TEMPERATURE.
C              CGS UNITS - K
C   V      - THE VOLUME OF THE REACTOR
C              CGS UNITS - CM**3
C   WT     - THE ARRAY OF SPECIES MOLECULAR WEIGHTS.
C              CGS UNITS - GM/MOLE
C              DIMENSION WT(*) AT LEAST KK.
C   YIN    - ARRAY OF INLET SPECIES MASS FRACTIONS.
C              DIMENSION YIN(*) AT LEAST KK.
C
C WORK AND SCRATCH SPACE-
C   RCKWRK - FLOATING POINT CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   H      - ARRAY OF SPECIES ENTHALPIES.
C              CGS UNITS - ERGS/MOLE
C              DIMENSION H(*) AT LEAST KK.
C   ICKWRK - INTEGER CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   WDOT   - ARRAY OF CHEMICAL PRODUCTION RATES.
C              CGS UNITS - MOLES/(CM**3-SEC)
C              DIMENSION WDOT(*) AT LEAST KK.
C
C OUTPUT-
C   F      - RESIDUALS OF THE GOVERNING EQUATIONS AS EVALUATED AT
C            S(N).  THE RESIDUAL OF THE KK SPECIES EQUATIONS IS IN
C            F(NYS+K), THE ENERGY EQUATION IN F(NT,J).  IF
C            INERGY=.FALSE. THEN THE ENERGY EQUATION IS REPLACED
C            BY THE GIVEN TEMPERATURE.
C               DIMENSION F(*) AT LEAST NATJ.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C*****unicos timelimit
C      call tremain (seconds)
C      if (seconds .lt. 60.0) STOP
C*****END unicos timelimit
C
C             FORM THE CHEMICAL RATE TERMS
C
      CALL CKHMS  (S(NT), ICKWRK, RCKWRK, H)
      CALL CKCPBS (S(NT), S(NY), ICKWRK, RCKWRK, CPB)
      CALL CKWYP  (P, S(NT), S(NY), ICKWRK, RCKWRK, WDOT)
      CALL CKRHOY (P, S(NT), S(NY), ICKWRK, RCKWRK, RHO)
C
      IF (LFLRT) TAU = RHO * V / FLRT
C
C             SPECIES CONSERVATION EQUATION
C
      DO 100 K = 1, KK
         F(NYS+K) = - (S(NYS+K) - YIN(K))/TAU + (WT(K)/RHO)*WDOT(K)
  100 CONTINUE
C
C              ENERGY EQUATION
C
      IF (LENRGY) THEN
         SUM1 = 0.0
         SUM2 = 0.0
         DO 200 K = 1, KK
            SUM1 = SUM1 + YIN(K) * (HIN(K)-H(K))
            SUM2 = SUM2 + H(K) * WT(K) * WDOT(K)
  200    CONTINUE
         F(NT) = 1.0 / (CPB*TAU) * SUM1 - SUM2 / (RHO*CPB) -
     1           Q / (RHO*V*CPB)
C
      ELSE
         F(NT) = S(NT) - T
      ENDIF
C
C          ADD THE TIME STEP, IF NEEDED
C
      IF (LTIME) THEN
C
         DO 300 K = 1, KK
             DYDT = (S(NYS+K) - SN(NYS+K)) / DT
             F(NYS+K) = F(NYS+K) - DYDT
  300    CONTINUE
C
         IF (LENRGY) THEN
            DTDT = (S(NT) - SN(NT)) / DT
            F(NT) = F(NT) - DTDT
         ENDIF
C
      ENDIF
C
      RETURN
      END
C
C------------------------------------------------------------------
C
      SUBROUTINE PSPRNT (KK, LOUT, KSYM, LENRGY, LFLRT, LEQUIV,
     1                   LPRTIC, EQUIV, PA, P, TAU, FLRT, V, Q, TIN,
     2                   XIN, T, X, S, ICKWRK, RCKWRK, NT, NY)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION XIN(*), X(*), S(*), ICKWRK(*), RCKWRK(*)
      CHARACTER KSYM(*)*(*)
C
      LOGICAL LENRGY, LFLRT, LEQUIV, LPRTIC
C
      DATA CALERG /2.389E-8/
C
      CALL CKYTX  (S(NY), ICKWRK, RCKWRK, X)
      CALL CKRHOY (P, S(NT), S(NY), ICKWRK, RCKWRK, RHO)
C
      IF (LFLRT) THEN
         TAU = RHO * V / FLRT
      ELSE
         FLRT = RHO * V / TAU
      ENDIF
C
C           PRINT SOLUTION
C
      IF (LEQUIV) WRITE (LOUT,6020) EQUIV
      WRITE (LOUT,6030) TAU
      WRITE (LOUT,6040) FLRT
      WRITE (LOUT,6050) PA
      WRITE (LOUT,6055) RHO
      WRITE (LOUT,6060) V
C
      IF (LENRGY) THEN
         WRITE (LOUT,6070) TIN
         WRITE (LOUT,6080) S(NT)
         QCAL = Q * CALERG
         WRITE (LOUT,6090) QCAL
      ELSE
         WRITE (LOUT,6100) T
      ENDIF
C
      IF (LPRTIC) THEN
         WRITE (LOUT, 6110)
         CALL PSPRT1 (LOUT, KK, KSYM, XIN)
         LPRTIC = .FALSE.
      ENDIF
C
      WRITE (LOUT, 6120)
      CALL PSPRT1 (LOUT, KK, KSYM, X)
C
 6020 FORMAT (/15X,'FUEL EQUIVALENCE RATIO ',7X,F9.2)
 6030 FORMAT (/15X,'RESIDENCE TIME ',15X,1PE9.2,2X,'SEC')
 6040 FORMAT (15X,'MASS FLOW RATE ',15X,1PE9.2,2X,'GM/SEC')
 6050 FORMAT (15X,'PRESSURE ',21X,1PE9.2,2X,'ATM')
 6055 FORMAT (15X,'MASS DENSITY ',17X,1PE9.2,2X,'GM/CM3')
 6060 FORMAT (15X,'VOLUME ',23X,1PE9.2,2X,'CM3')
 6070 FORMAT (15X,'TEMPERATURE (INLET) ',10X,F9.2,2X,'K')
 6080 FORMAT (15X,'TEMPERATURE ',18X,F9.2,2X,'K')
 6090 FORMAT (15X,'HEAT LOSS ', 20X, 1PE9.2,2X,'CAL/SEC')
 6100 FORMAT (15X,'TEMPERATURE (FIXED) ',10X,F9.2,2X,'K')
 6110 FORMAT (//21X,'INLET MOLE FRACTIONS ',/)
 6120 FORMAT (//21X,'EXIT MOLE FRACTIONS ',/)
C
      RETURN
      END
C
C-------------------------------------------------------------------
C
      SUBROUTINE PSPRT1 (LOUT, KK, KSYM, X)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION X(*)
      CHARACTER KSYM(*)*(*)
C
      DO 10 K = 1, KK, 3
         WRITE (LOUT, 6010) (KSYM(L), X(L), L=K, MIN(K+2, KK))
   10 CONTINUE
 6010 FORMAT (3X,3(A10,'= ', 1PE9.2, 6X))
C
      RETURN
      END
C
C------------------------------------------------------------------
C
      SUBROUTINE PSRKEY (LIN, LOUT, MM, KK, NATJ, ICKWRK, RCKWRK, KSYM, 
     1                   ATOM, NCF, LENRGY, LFLRT, LEQUIV, LRSTRT,
     2                   LCNTUE, MFILE, IPRNT, LSEN, LSENT, ISEN,
     3                   EPSS, EPST, LROP, IROP, EPSR, TIN, XIN, T,
     4                   XEST, EQUIV, PATM, PA, P, TAU, FLRT, V, Q,
     5                   FUEL, OXID, ADD, STOICH, KPROD, KSP, ATOL,
     6                   RTOL, ATIM, RTIM, NUMDT, DT, NUMDT2, DT2, SFLR,
     7                   UFAC, DFAC, DTMIN, LENIEQ, LENREQ, IEQWRK,
     8                   REQWRK, IPVT, RHS, A,
     9                   NINIT, NJAC, ITJAC, DTMAX, IRETIR)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION NCF(MM,*), FUEL(*), OXID(*), ADD(*), STOICH(*),
     1          XIN(*), XEST(*), KPROD(*), IPVT(*), RHS(*),
     2          A(NATJ,*), KSP(*), ICKWRK(*), RCKWRK(*), IEQWRK(*),
     3          REQWRK(*), VALUE(5)
      DIMENSION XCON(1), KCON(1)
C
      LOGICAL LENRGY, LRSTRT, LCNTUE, LFLRT, LEQUIV, LSEN, LSENT,
     1        ISEN(*), LROP, IROP(*), NEC(13), CNTNUD, LESTIM,
     2        ENRGIN, IERR, KERR, LEQST
C
      CHARACTER KEYWRD*4, KSYM(*)*(*), ATOM(*)*(*), LINE*80
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   MM     - NUMBER OF ELEMENTS.
C   KK     - NUMBER OF CHEMICAL SPECIES.
C   KSYM   - CHEMKIN SPECIES NAMES.
C              DIMENSION KSYM(*) AT LEAST KK.
C   LENIEQ - LENGTH OF THE INTEGER WORKSPACE FOR THE EQUILIBRIUM
C            SUBROUTINES.
C   LENREQ  - LENGTH OF THE FLOATING POINT WORKSPACE FOR THE EQUILIBRIUM
C            SUBROUTINES.
C   LIN    - UNIT FOR READING KEYWORD INPUT.
C   LOUT   - UNIT FOR PRINTED OUTPUT.
C   NATJ   - NUMBER OF DEPENDENT VARIABLES. NATJ=KK+1.
C   PATM   - PRESSURE OF ONE ATMOSPHERE.
C              CGS UNITS - DYNES/CM**2
C
C WORK AND SCRATCH SPACE-
C   A      - JACOBIAN OF STOICHIOMETRIC EQUATION.
C              DIMENSION A(NATJ,*) EXACTLY NATJ FOR THE FIRST DIMENSION
C              AND AT LEAST NATJ FOR THE SECOND.
C   ADD    - ARRAY OF ADDED SPECIES MOLE FRACTIONS.
C              DIMENSION ADD(*) AT LEAST KK.
C   RCKWRK - FLOATING POINT CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   REQWRK - THE FLOATING POINT WORKSPACE FOR THE EQUILIBRIUM
C            SUBROUTINES.
C              DIMENSION REQWRK(*) AT LEAST LENREQ.
C   FUEL   - ARRAY OF FUEL SPECIES MOLE FRACTIONS.
C              DIMENSION FUEL(*) AT LEAST KK.
C   ICKWRK - INTEGER CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   IEQWRK - THE INTEGER WORKSPACE FOR THE EQUILIBRIUM SUBROUTINES.
C              DIMENSION IEQWRK(*) AT LEAST LENIEQ.
C   IPVT   - ARRAY OF PIVOTS FOR BALANCING THE STOICHIOMETRIC EQUATION.
C              DIMENSION IPVT(*) AT LEAST KK.
C   KPROD  - ARRAY OF PRODUCT SPECIES NUMBERS.
C              DIMENSION KPROD(*) AT LEAST KK.
C   KSP    - ARRAY CONTAINING SPECIES NUMBERS FOR SELECTED SPECIES.
C              DIMENSION KSP(*) AT LEAST KK.
C   OXID   - ARRAY OF OXIDIZER SPECIES MOLE FRACTIONS.
C              DIMENSION OXID(*) AT LEAST KK.
C   RHS    - ARRAY CONTAINING RIGHT-HAND-SIDE OF STOICHIOMETRIC
C            EQUATION.
C              DIMENSION AT LEAST KK.
C   SCRTCH - SCRATCH SPACE.
C              DIMENSION(KK, 5)
C   STOICH - ARRAY OF REACTANT SPECIES MOLE FRACTIONS AT STOICHIOMETRIC
C            CONDITIONS.
C              DIMENSION FUEL(*) AT LEAST KK.
C
C OUTPUT-
C   ATIM   - ABSOLUTE CONVERGENCE CRITERIA FOR THE NEWTON ITERATION
C            AS USED FOR THE TIME STEPS.
C   ATOL   - ABSOLUTE CONVERGENCE CRITERIA FOR THE NEWTON ITERATION.
C   DT     - SIZE OF THE TIME STEPS.
C              CGS UNITS - SEC
C   DT2    - SIZE OF THE TIME STEPS AFTER THE ENERGY EQUATION IS
C            INCLUDED.
C              CGS UNITS - SEC
C   EPSR   - TRESHOLD VALUE FOR RATE-OF-PRODUCTION COEFFICIENTS.
C   EPSS   - TRESHOLD VALUE FOR SPECIES SENSITIVITY COEFFICIENTS.
C   EPST   - TRESHOLD VALUE FOR TEMPERATURE SENSITIVITY COEFFICIENTS.
C   EQUIV  - THE FUEL EQUIVALENCE RATIO.
C   FLRT   - THE MASS FLOW RATE.
C              CGS UNITS - GM/SEC
C   IPRNT  - FLAG TO SPECIFY THE AMOUNT OF PRINTING.
C              IPRNT = 0, PRINT ONLY THE SOLUTION AFTER CONVERGENCE
C              IPRNT = 1, PRINT THE NORMS AFTER EACH ITERATION.
C              IPRNT = 2, PRINT THE FULL SOLUTION AFTER EACH ITERATION.
C   IROP   - LOGICAL ARRAY. IF IROP(K)=.TRUE. THEN THE RATE-OF-PRODUC-
C            TION COEFFICIENTS OF SPECIES K WILL BE PRINTED OUT.
C              DIMENSION IROP(*) AT LEAST KK.
C   ISEN   - LOGICAL ARRAY. IF ISEN(K)=.TRUE. THEN THE FIRST ORDER
C            SENSITIVITY COEFFICIENTS OF SPECIES K WITH RESPECT TO
C            RATE CONSTANTS WILL BE PRINTED OUT.
C              DIMENSION ISEN(*) AT LEAST KK.
C   LCNTUE - IF LCNTUE=.TRUE. THEN A CONTINUATION PROBLEM WILL FOLLOW.
C            IF LCNTUE=.FLASE. THEN THIS IS THE ONLY PROBLEM FOR THE
C            RUN.
C   LENRGY - IF LENRGY=.TRUE. THEN THE ENERGY EQUATION IS SOLVED.
C            IF LENRGY=.FALSE. THEN A SPECIFIED TEMPERATURE IS USED.
C   LEQUIV - IF LEQUIV=.TRUE. THEN THE INLET COMPOSITION IS DEFINED BY
C              THE FUEL EQUIVALENCE RATIO, THE FUEL AND OXIDIZER
C              COMPOSITIONS AND THE PRODUCTS.
C            IF LEQUIV=.FALSE. THEN THE INLET COMPOSITION IS DEFINED
C              DIRECTLY BY THE USER.
C   LFLRT  - IF LFLRT=.TRUE. THEN THE MASS FLOW RATE IS SPECIFIED AND
C              THE RESIDENCE TIME HAS TO BE CALCULATED.
C            IF LFLRT=.FALSE. THEN THE RESIDENCE TIME IS SPECIFIED AND
C              THE MASS FLOW RATE HAS TO BE CALCULATED.
C   LROP   - IF LROP=.TRUE. THEN A RATE-OF-PRODUCTION ANALYSIS
C            IS CARRIED OUT.
C   LRSTRT - IF LRSTRT=.TRUE. THEN START FROM A PREVIOUS PROFILE.
C            IF LRSTRT=.FALSE. THEN START FRESH.
C   LSEN   - IF LSEN=.TRUE. THEN A FIRST ORDER SENSITIVITY ANALYSIS
C            IS CARRIED OUT.
C   LSENT  - IF LSENT=.TRUE. THEN THE TEMPERATURE SENSITIVITY
C            COEFFICIENTS ARE PRINTED OUT.
C   NUMDT  - NUMBER OF TIME STEPS TO TAKE WHEN DOING A TIME START.
C   NUMDT2 - NUMBER OF TIME STEPS TO TAKE WHEN TIME STEPPING WITH THE
C            ENERGY EQUATION INCLUDED.
C   P      - THE PRESSURE.
C              CGS UNITS - DYNES/CM**2
C   PA     - THE PRESSURE.
C              UNITS - ATM
C   Q      - THE HEAT LOSS OF THE REACTOR
C              CGS UNITS - ERGS/SEC
C   RTIM   - RELATIVE CONVERGENCE CRITERIA FOR THE NEWTON ITERATION
C            AS USED FOR THE TIME STEPS.
C   RTOL   - RELATIVE CONVERGENCE CRITERIA FOR THE NEWTON ITERATION.
C   T      - THE USER-SPECIFIED TEMPERATURE.
C              CGS UNITS - K
C   TAU    - THE NOMINAL RESIDENCE TIME OF THE REACTOR
C              CGS UNITS - SEC
C   TIN    - THE INLET TEMPERATURE.
C              CGS UNITS - K
C   V      - THE VOLUME OF THE REACTOR
C              CGS UNITS - CM**3
C   XEST   - ARRAY OF SPECIES MOLE FRACTIONS. STARTING POINT FOR
C            ITERATIONS.
C              DIMENSION REAC(*) AT LEAST KK.
C   XIN    - ARRAY OF SPECIES INPUT MOLE FRACTIONS.
C              DIMENSION REAC(*) AT LEAST KK.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      DATA CALERG /2.389E-8/, CNTNUD/.FALSE./
C
C            INITIALIZE VARIABLES
C
      KERR   = .FALSE.
      ENRGIN = .FALSE.
      NPROD  = 0
C
      DO 100 L = 1, 13
         NEC(L) = .FALSE.
  100 CONTINUE
C
      IF (LCNTUE) THEN
C
         LCNTUE = .FALSE.
         CNTNUD = .TRUE.
         LESTIM = .TRUE.
C
      ELSE
C
         ATOL   = 1.0E-9
         RTOL   = 1.0E-4
         ATIM   = 1.0E-9
         RTIM   = 1.0E-4
         SFLR   = -1.E-3
         NUMDT  = 100
         NINIT  = 0
         NJAC   = 20
         IRETIR = 50
         DTMAX  = 1.0E-4
         ITJAC  = 20
         DT     = 1.0E-6
         NUMDT2 = 100
         DT2    = 1.0E-6
         UFAC   = 2.0
         DFAC   = 2.2
         DTMIN  = 1.E-10
         IPRNT  = 1
         MFILE  = 1
         LCNTUE = .FALSE.
         LRSTRT = .FALSE.
         LENRGY = .FALSE.
         LESTIM = .FALSE.
         LSEN   = .FALSE.
         LSENT  = .FALSE.
         LROP   = .FALSE.
         LEQST  = .FALSE.
         EPSS   = 0.01
         EPST   = 1.E-04
         EPSR   = 0.01
         EQUIV  = 0.0
         TAU    = 0.0
         FLRT   = 0.0
         Q      = 0.0
         DO 200 K = 1, KK
            XIN(K)   = 0.0
            XEST(K)  = 0.0
            STOICH(K)= 0.0
            FUEL(K)  = 0.0
            OXID(K)  = 0.0
            ADD(K)   = 0.0
            ISEN(K)  = .FALSE.
            IROP(K)  = .FALSE.
200      CONTINUE
      ENDIF
C
C--------------------------------------------------------------
C
C         READ NEXT INPUT LINE
C
      WRITE (LOUT,'(/A/)') '           KEYWORD INPUT '
C
 1000 CONTINUE
      KEYWRD = ' '
      LINE = ' '
      READ  (LIN,  7000) KEYWRD, LINE
      WRITE (LOUT, 8000) KEYWRD, LINE
C
C               IS THIS A KEYWORD COMMENT?
C
      IF (KEYWRD(1:1) .EQ. '.' .OR. KEYWRD(1:1) .EQ. '/'
     1                         .OR. KEYWRD(1:1) .EQ. '!') GO TO 1000
      CALL UPCASE (KEYWRD)
C
C--------------PROBLEM TYPE KEYWORDS--------------------
C
      IF (KEYWRD .EQ. 'TGIV') THEN
C
C         ENERGY EQUATION IS NOT INCLUDED
C
         IF (NEC(1)) WRITE (LOUT,*)
     1      'WARNING...BOTH "TGIV" AND "ENRG" GIVEN'
         NEC(1) = .TRUE.
         LENRGY   = .FALSE.
C
      ELSEIF (KEYWRD .EQ. 'ENRG') THEN
C
C         ENERGY EQUATION IS INCLUDED
C
         IF (NEC(1)) WRITE (LOUT,*)
     1      'WARNING...BOTH "TGIV" AND "ENRG" GIVEN'
         NEC(1)      = .TRUE.
         IF (CNTNUD .AND. (.NOT.LENRGY)) ENRGIN = .TRUE.
         LENRGY = .TRUE.
C
C--------------METHOD OPTIONS KEYWORDS--------------------
C
      ELSEIF (KEYWRD .EQ. 'ATOL') THEN
C
C       ABSOLUTE NEWTON ITERATION CONVERGENCE CRITERIA
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, ATOL, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'NJAC') THEN
C
C       RETIREMENT AGE OF JACOBIAN DURING STEADY-STATE NEWTON
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, VALUE, IERR)
         NJAC = INT (VALUE(1))
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'TJAC') THEN
C
C       RETIREMENT AGE OF JACOBIAN DURING TIM STEPPING
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, VALUE, IERR)
         ITJAC = INT (VALUE(1))
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'RTOL') THEN
C
C       RELATIVE NEWTON ITERATION CONVERGENCE CRITERIA
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, RTOL, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'ATIM') THEN
C
C       ABSOLUTE NEWTON CONVERGENCE CRITERIA FOR TIMESTEPS
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, ATIM, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'RTIM') THEN
C
C       RELATIVE NEWTON CONVERGENCE CRITERIA FOR TIMESTEPS
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, RTIM, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'TIME') THEN
C
C       TIME STEP STARTING PROCEDURE
C
         CALL CKXNUM (LINE, 2, LOUT, NVAL, VALUE, IERR)
         KERR = KERR.OR.IERR
         NUMDT = INT(VALUE(1))
         DT = VALUE(2)
C
      ELSEIF (KEYWRD .EQ. 'ISTP') THEN
C
C        NUMBER OF INITIAL TIME STEPS BEFORE NEWTON
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, VALUE, IERR)
         NINIT = INT (VALUE(1))
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'IRET') THEN
C
C        RETIRMENT AGE OF OLD TIME STEP (DEFAULT=50)
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, VALUE, IERR)
         IRETIR = INT (VALUE(1))
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'TIM2') THEN
C
C        TIME STEPPING, AFTER ADDING THE ENERGY EQUATION
C
         CALL CKXNUM (LINE, 2, LOUT, NVAL, VALUE, IERR)
         KERR = KERR.OR.IERR
         NUMDT2 = INT(VALUE(1))
         DT2 = VALUE(2)
C
      ELSEIF (KEYWRD .EQ. 'UFAC') THEN
C
C       TIMESTEP INCREASE WHEN TIMESTEP DOES NOT CHANGE SOLUTION
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, UFAC, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'DFAC') THEN
C
C       TIMESTEP DECREASE WHEN NEWTON FAILS CONVERGENCE ON TIMESTEP
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, DFAC, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'DTMN') THEN
C
C       MINIMUM TIMESTEP
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, DTMIN, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'DTMX') THEN
C
C        MAXIMUM TIMESTEP
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, DTMAX, IERR)
C
C--------------REACTOR DEFINITION KEYWORDS--------------------
C
      ELSEIF (KEYWRD .EQ. 'TEMP') THEN
C
C         TEMPERATURE
C
         NEC(2)  = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, T, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'PRES') THEN
C
C         PRESSURE
C
         NEC(3)  = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, PA, IERR)
         KERR = KERR.OR.IERR
         P = PA*PATM
C
      ELSEIF (KEYWRD .EQ. 'TAU ') THEN
C
C         RESIDENCE TIME
C
         IF (NEC(4)) WRITE (LOUT,*)
     1      'WARNING...BOTH TAU AND FLRT IS DEFINED'
         NEC(4)  = .TRUE.
         LFLRT = .FALSE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, TAU, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'FLRT') THEN
C
C         MASS FLOW RATE
C
         IF (NEC(4)) WRITE (LOUT,*)
     1      'WARNING...BOTH TAU AND FLRT IS DEFINED'
         NEC(4)  = .TRUE.
         LFLRT = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, FLRT, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'VOL ') THEN
C
C         VOLUME
C
         NEC(5)  = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, V, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'QLOS') THEN
C
C         HEAT LOSS
C
         NEC(6) = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, Q, IERR)
         KERR = KERR.OR.IERR
         Q = Q / CALERG
C
C--------------INLET CONDITIONS ------------------------------------
C
      ELSEIF (KEYWRD .EQ. 'TINL') THEN
C
C         INLET TEMPERATURE
C
         NEC(7)  = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, TIN, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'REAC') THEN
C
C         REACTANT
C
         IF (CNTNUD .AND. (.NOT. NEC(8))) THEN
            DO 300 K = 1, KK
               XIN(K) = 0.0
300         CONTINUE
         ENDIF
         NEC(8)  = .TRUE.
         CALL CKSNUM (LINE, 1, LOUT, KSYM, KK, KSPEC, NVAL,
     1                VALUE, IERR)
         IF (IERR) THEN
            WRITE (LOUT,'(A)')
     1      ' ERROR READING DATA FOR KEYWORD '//KEYWRD
            KERR = .TRUE.
         ELSE
            LEQUIV = .FALSE.
            XIN(KSPEC) = VALUE(1)
         ENDIF
C
      ELSEIF (KEYWRD .EQ. 'EQUI') THEN
C
C         EQUIVALENCE RATIO
C
         NEC(9)  = .TRUE.
         CALL CKXNUM (LINE, 1, LOUT, NVAL, EQUIV, IERR)
         KERR = KERR.OR.IERR
         LEQUIV = .TRUE.
C
      ELSEIF (KEYWRD .EQ. 'FUEL') THEN
C
C         FUEL DEFINITION
C
         NEC(10)  = .TRUE.
         CALL CKSNUM (LINE, 1, LOUT, KSYM, KK, KSPEC, NVAL,
     1                VALUE, IERR)
         IF (IERR) THEN
            WRITE (LOUT,'(A)')
     1      ' ERROR READING DATA FOR KEYWORD '//KEYWRD
            KERR = .TRUE.
         ELSE
            FUEL(KSPEC) = VALUE(1)
         ENDIF
         IF (CNTNUD) THEN
            WRITE (LOUT,*) 'ERROR..."FUEL" CANNOT BE REDEFINED'
            KERR = .TRUE.
         ENDIF
C
      ELSEIF (KEYWRD .EQ. 'OXID') THEN
C
C         OXIDIZER DEFINITION
C
         NEC(11)  = .TRUE.
         CALL CKSNUM (LINE, 1, LOUT, KSYM, KK, KSPEC, NVAL,
     1                VALUE, IERR)
         IF (IERR) THEN
            WRITE (LOUT,'(A)')
     1      ' ERROR READING DATA FOR KEYWORD '//KEYWRD
            KERR = .TRUE.
         ELSE
            OXID(KSPEC) = VALUE(1)
         ENDIF
         IF (CNTNUD) THEN
            WRITE (LOUT,*) 'ERROR..."OXID" CANNOT BE REDEFINED'
            KERR = .TRUE.
         ENDIF
C
      ELSEIF (KEYWRD .EQ. 'PROD') THEN
C
C         PRODUCT DEFINITION
C
         NEC(12)  = .TRUE.
         CALL CKCOMP (LINE, KSYM, KK, KSPEC)
         IF (KSPEC.LE.0 .OR. NPROD+1.GT.KK) THEN
            WRITE (LOUT,'(A)')
     1      ' ERROR READING DATA FOR KEYWORD '//KEYWRD
         ELSE
            NPROD = NPROD + 1
            KPROD(NPROD)=KSPEC
         ENDIF
         IF (CNTNUD) THEN
            WRITE (LOUT,*) 'ERROR..."PROD" CANNOT BE REDEFINED'
            STOP
         ENDIF
C
      ELSEIF (KEYWRD .EQ. 'ADD ') THEN
C
C         ADDED SPECIES
C
         IF (CNTNUD .AND. (.NOT. NEC(13))) THEN
            DO 350 K = 1, KK
               ADD(K) = 0.0
350         CONTINUE
         ENDIF
         NEC(13)  = .TRUE.
         CALL CKSNUM (LINE, 1, LOUT, KSYM, KK, KSPEC, NVAL,
     1                VALUE, IERR)
         IF (IERR) THEN
            WRITE (LOUT,'(A)')
     1      ' ERROR READING DATA FOR KEYWORD '//KEYWRD
            KERR = .TRUE.
         ELSE
            ADD(KSPEC) = VALUE(1)
         ENDIF
C
      ELSEIF (KEYWRD .EQ. 'XEST') THEN
C
C--------------SOLUTION ESTIMATE--------------------
C
         CALL CKSNUM (LINE, 1, LOUT, KSYM, KK, KSPEC, NVAL,
     1                VALUE, IERR)
         IF (IERR) THEN
            WRITE (LOUT,'(A)')
     1      ' ERROR READING DATA FOR KEYWORD '//KEYWRD
            KERR = .TRUE.
         ELSE
            LESTIM = .TRUE.
            XEST(KSPEC) = VALUE(1)
         ENDIF
C
C--------------SENSITIVITY KEYWORDS--------------------
C
      ELSEIF (KEYWRD .EQ. 'ASEN') THEN
C
C        ALL REACTION SENSITIVITY
C
         LSEN = .TRUE.
         LSENT = .TRUE.
         DO 400 K = 1, KK
            ISEN(K) = .TRUE.
400      CONTINUE
C
      ELSEIF (KEYWRD .EQ. 'SEN ') THEN
C
C         SPECIFIC SPECIES KEYWORDS
C
         LSEN = .TRUE.
         IERR = .FALSE.
         CALL CKCRAY (LINE, KK, KSYM, LOUT, KK, KSP, NS, IERR)
         IF (IERR) THEN
            WRITE (LOUT,'(A)')
     1      ' ERROR READING DATA FOR KEYWORD '//KEYWRD
            KERR = .TRUE.
         ELSE
            DO 450 N = 1, NS
               ISEN(KSP(N)) = .TRUE.
450         CONTINUE
         ENDIF
C
      ELSEIF (KEYWRD .EQ. 'SFLR') THEN
C
C        FLOOR VALUE FOR THE SPECIES BOUNDS
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, SFLR, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'SENT') THEN
C
C         TEMPERATURE SENSITIVITY KEYWORD
C
         LSENT = .TRUE.
C
      ELSEIF (KEYWRD .EQ. 'EPSS') THEN
C
C         SENSITIVITY PRINT OPTION
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, EPSS, IERR)
         KERR = KERR.OR.IERR
C
      ELSEIF (KEYWRD .EQ. 'EPST') THEN
C
C         SENSITIVITY PRINT OPTION (TEMPERATURE)
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, EPST, IERR)
         KERR = KERR.OR.IERR
C
C--------------RATE-OF-PRODUCTION KEYWORDS--------------------
C
      ELSEIF (KEYWRD .EQ. 'AROP') THEN
C
C        ALL SPECIES RATE-OF-PRODUCTION
C
         LROP = .TRUE.
         DO 500 K = 1, KK
            IROP(K) = .TRUE.
500      CONTINUE
C
      ELSEIF (KEYWRD .EQ. 'ROP ') THEN
C
C         SPECIFIC SPECIES KEYWORDS
C
         LROP = .TRUE.
         CALL CKCRAY (LINE, KK, KSYM, LOUT, KK, KSP, NS, IERR)
         IF (IERR) THEN
            WRITE (LOUT,'(A)')
     1      ' ERROR READING DATA FOR KEYWORD '//KEYWRD
            KERR = .TRUE.
         ELSE
            DO 550 N = 1, NS
               IROP(KSP(N)) = .TRUE.
550         CONTINUE
         ENDIF
C
      ELSEIF (KEYWRD .EQ. 'EPSR') THEN
C
C         RATE-OF-PRODUCTION PRINT OPTION
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, EPSR, IERR)
         KERR = KERR.OR.IERR
C
C--------------PRINTING AND RESTARTING KEYWORDS-------------------
C
      ELSEIF (KEYWRD .EQ. 'PRNT') THEN
C
C          PRINT CONTROL
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, VALUE, IERR)
         KERR  = KERR.OR.IERR
         IPRNT = INT(VALUE(1))
C
      ELSEIF (KEYWRD .EQ. 'SKIP') THEN
C
C          RESTART SKIPS
C
         CALL CKXNUM (LINE, 1, LOUT, NVAL, VALUE, IERR)
         KERR = KERR.OR.IERR
         MFILE = INT(VALUE(1)) + 1
C
      ELSEIF (KEYWRD .EQ. 'RSTR') THEN
C
C          RESTART CHECK
C
         LESTIM = .TRUE.
         LRSTRT = .TRUE.
C
      ELSEIF (KEYWRD .EQ. 'CNTN') THEN
C
C          CONTINUATION FLAG
C
         LCNTUE   = .TRUE.
C
C        LAST CARD
C
      ELSEIF (KEYWRD .EQ. 'END ') THEN
         GO TO 1100
      ELSE
C
C--------------END OF KEYWORDS--------------------
C
C        TO GET HERE, AN INVALID KEYWORD WAS READ
C
         WRITE (LOUT,*) ' ERROR...ILLEGAL KEYWORD'
         KERR = .TRUE.
      ENDIF
      GO TO 1000
 1100 CONTINUE
C
C          CHECK FOR NECESSARY INPUT
C
      IF (.NOT. CNTNUD) THEN
         IF ( .NOT. NEC(1) ) THEN
            WRITE (LOUT, *)
     1      ' ERROR..."ENRG" OR "TGIV" MUST BE PROVIDED '
            KERR = .TRUE.
C
         ELSEIF ( .NOT. NEC(2) ) THEN
            WRITE (LOUT, *)
     1      ' ERROR...NO REACTOR TEMPERATURE IS SPECIFIED OR ESTIMATED'
            KERR = .TRUE.
C
         ELSEIF ( .NOT. NEC(3) ) THEN
            WRITE (LOUT, *) ' ERROR...PRESSURE NOT GIVEN'
            KERR = .TRUE.
C
         ELSEIF ( .NOT. NEC(4) ) THEN
            WRITE (LOUT, *) ' ERROR..."FLRT" OR "TAU" MUST BE PROVIDED'
            KERR = .TRUE.
C
         ELSEIF ( .NOT. NEC(5) ) THEN
            WRITE (LOUT, *)
     1      ' ERROR...VOLUME MUST BE SPECIFIED.'
            KERR = .TRUE.
C
         ENDIF
C
         IF ( LENRGY ) THEN
            IF ( .NOT. NEC(6) ) THEN
               WRITE (LOUT, *)
     1       ' ERROR...HEAT LOSS MUST BE SPECIFIED FOR "ENRG" PROBLEMS'
               KERR = .TRUE.
            ELSEIF ( .NOT. NEC(7) ) THEN
               WRITE (LOUT, *)
     1        ' ERROR...T INLET MUST BE SPECIFIED FOR "ENRG" PROBLEMS'
               KERR = .TRUE.
            ENDIF
         ELSE
            IF ( NEC(6) ) WRITE (LOUT, *)
     1       ' WARNING...HEAT LOSS SPECIFIED FOR "TGIV" PROBLEM'
            IF ( NEC(7) ) WRITE (LOUT, *)
     1        ' WARNING..."TINL" SPECIFIED FOR "TGIV" PROBLEM'
         ENDIF
C
         IF ( NEC(8) ) THEN
            DO 1200 I = 9, 12
               IF (NEC(I)) THEN
                  WRITE (LOUT,*)
     1            'ERROR...MULTIPLE DEFINITION OF INLET COMPOSITION'
                  KERR = .TRUE.
               ENDIF
1200        CONTINUE
         ELSE
            DO 1300 I = 9, 12
               IF (.NOT. NEC(I)) THEN
                  WRITE (LOUT,*)
     1            'ERROR...INLET COMPOSITION NOT DEFINED'
                  KERR = .TRUE.
               ENDIF
1300        CONTINUE
         ENDIF
      ELSEIF (ENRGIN) THEN
         IF ( .NOT. NEC(6) ) THEN
            WRITE (LOUT, *)
     1     ' ERROR...HEAT LOSS MUST BE SPECIFIED FOR "ENRG" PROBLEMS'
            KERR = .TRUE.
         ELSEIF ( .NOT. NEC(7) ) THEN
            WRITE (LOUT, *)
     1        ' ERROR...T INLET MUST BE SPECIFIED FOR "ENRG" PROBLEMS'
            KERR = .TRUE.
         ENDIF
C
      ENDIF
C
C          NORMALIZE THE SOLUTION ESTIMATE
C
      IF (LESTIM) THEN
         SUM = 0.0
         DO 1400 K = 1, KK
            SUM = SUM + XEST(K)
1400     CONTINUE
         IF (SUM .GT. 0.0) THEN
            DO 1500 K = 1, KK
               XEST(K) = XEST(K) / SUM
1500        CONTINUE
            IF (ABS(SUM-1.0) .GT. 1.E-6) WRITE (LOUT, *)
     1        ' CAUTION...XEST MOLE FRACTIONS SUM TO ', SUM
         ENDIF
      ENDIF
C
C          CHECK THE SUM OF USER SUPPLIED REACTANTS
C
      IF (.NOT. LEQUIV) THEN
         SUM = 0.0
         DO 1600 K = 1, KK
            SUM = SUM + XIN(K)
1600     CONTINUE
         DO 1700 K = 1, KK
            XIN(K) = XIN(K) / SUM
1700     CONTINUE
C
         IF (ABS(SUM-1.0) .GT. 1.E-6) WRITE (LOUT, *)
     1      ' CAUTION...REACTANT MOLE FRACTIONS SUM TO ', SUM
      ENDIF
C
C           STOP IF ANY ERRORS ENCOUNTERED IN INPUT SO FAR
C
      IF (KERR) STOP
C
C         CHECK THE SUMS FOR FUEL, OXIDIZER AND ADDED SPECIES
C
      IF ((.NOT. CNTNUD) .AND. (LEQUIV)) THEN
         SUMF = 0.0
         SUMO = 0.0
         SUMA = 0.0
         DO 1800 K = 1, KK
            SUMF = SUMF + FUEL(K)
            SUMO = SUMO + OXID(K)
            SUMA = SUMA + ADD(K)
1800     CONTINUE
C
C         NORMALIZE FUEL AND OXIDIZER FRACTIONS
C
         DO 1900 K = 1, KK
            FUEL(K) = FUEL(K) / SUMF
            OXID(K) = OXID(K) / SUMO
1900     CONTINUE
C
         IF (ABS(SUMF-1.0) .GT. 1.E-6) WRITE (LOUT, *)
     1             ' CAUTION...FUEL FRACTIONS SUM TO ', SUMF
         IF (ABS(SUMO-1.0) .GT. 1.E-6) WRITE (LOUT, *)
     1             ' CAUTION...OXIDIZER FRACTIONS SUM TO ',  SUMO
         IF (SUMA .GT. 1.0) THEN
            WRITE (LOUT, *)
     1             ' ERROR...ADDED SPECIES SUM TO ',  SUMA
            STOP
         ENDIF
      ENDIF
C
C           BALANCE THE EQUATION FOR COMPLETE COMBUSTION
C
      IF (LEQUIV) THEN
         IF (.NOT. CNTNUD) THEN
            DO 2000 M = 1, MM
C
               RSUM = 0.0
               ASUM = 0.0
               DO 2100 K = 1, KK
                  RSUM = RSUM + FUEL(K) * NCF(M,K)
                  ASUM = ASUM + OXID(K) * NCF(M,K)
2100           CONTINUE
               RHS(M) = RSUM
               A(M,1) = -ASUM
C
               N = 1
               DO 2300 L = 1, NPROD
                  N = N + 1
                  A(M,N) = NCF(M,KPROD(L))
2300           CONTINUE
2000        CONTINUE
C
            IF (N .NE. MM) THEN
               WRITE (LOUT,*)
     1         ' THE BALANCE MATRIX HAS ',MM,' ROWS, AND ',N,' COLUMNS'
               STOP
            ENDIF
C
C            CALL LINPAC TO BALANCE THE REACTIONS
C
C*****precision > double
            CALL DGEFA(A, NATJ, MM, IPVT, INFO)
            IF (INFO .NE. 0) THEN
               WRITE (LOUT,*)
     1'ERROR..INCONSISTENT DEFINITION OF FUEL, OXIDIZER, AND PRODUCTS'
               STOP
            ENDIF
            CALL DGESL(A, NATJ, MM, IPVT, RHS, 0)
C*****END precision > double
C
C*****precision > single
C            CALL SGEFA(A, NATJ, MM, IPVT, INFO)
C            IF (INFO .NE. 0) THEN
C               WRITE (LOUT,*)
C     1'ERROR..INCONSISTENT DEFINITION OF FUEL, OXIDIZER, AND PRODUCTS'
C               STOP
C            ENDIF
C            CALL SGESL(A, NATJ, MM, IPVT, RHS, 0)
C*****END precision > single
C
            SUM = 0.0
            DO 2500 K = 1, KK
               STOICH(K) = FUEL(K) + RHS(1) * OXID(K)
               SUM = SUM + STOICH(K)
2500        CONTINUE
            DO 2600 K = 1, KK
               STOICH(K) = STOICH(K) / SUM
2600        CONTINUE
         ENDIF
C
         SUM = 0.0
         SUMA = 0.0
         DO 2700 K = 1, KK
            IF (OXID(K) .GT. 0.0) THEN
               XIN(K) = STOICH(K) / EQUIV
            ELSE
               XIN(K) = STOICH(K)
            ENDIF
            SUM  = SUM + XIN(K)
            SUMA = SUMA + ADD(K)
2700     CONTINUE
C
         DO 2800 K = 1, KK
            XIN(K) = XIN(K) / SUM
2800     CONTINUE
C
         IF (SUMA .GT. 0.0) THEN
            SUM = 1.0 + SUMA
            DO 3000 K = 1, KK
               XIN(K) = XIN(K) / SUM + ADD(K)
3000        CONTINUE
         ENDIF
      ENDIF
C
C-------CALCULATION OF EQUILIBRIUM VALUES (FOR INITIAL GUESS)
C
      IF (.NOT. LESTIM) THEN
C
         NOP = 1
         KMON = 0
         NCON = 0
         CALL EQUIL (LOUT, .FALSE., 0, LEQST, .FALSE., ICKWRK,
     1               RCKWRK, LENIEQ, IEQWRK, LENREQ, REQWRK, MM,
     2               KK, ATOM, KSYM, NOP, KMON, XIN, T, T, PA,
     3               PA, NCON, KCON, XCON)
         CALL EQSOL (KK, REQWRK, XEST, RHS, TDUM, PDUM, 
     1               HDUM, VDUM, SDUM, WMDUM, CDUM, CDET)
C
         WRITE (LOUT, '(/A/)')
     1   ' FIRST SOLUTION ESTIMATE IS EQUILIBRIUM '
C
      ENDIF
C
7000  FORMAT (A4, A)
8000  FORMAT (10X, A4, A76)
C
      RETURN
      END
C
C-------------------------------------------------------------------
C
      SUBROUTINE PSRSEN (LOUT, LSAVE, KK, II, NATJ, KSYM,
     1                   CCKWRK, LENRGY, LFLRT, ABSOL, RELAT, DT, WT,
     2                   HIN, YIN, P, TAU, FLRT, V, Q, T, S, SN, F,
     3                   FN, A, LSENT, ISEN, EPSS, EPST, ICKWRK,
     4                   RCKWRK, IPVT, SCRTCH, NT, NYS, NY)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION WT(*), HIN(*), YIN(*), SN(*), S(*), F(*), FN(*),
     1          SCRTCH(KK,*), IPVT(*), ICKWRK(*), RCKWRK(*), A(NATJ,*),
     2          KEEP(3)
C
      CHARACTER KSYM(*)*(*), ISYM*30, CCKWRK(1)*(*)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   ABSOL  - ABSOLUTE PERTURBATION FOR EVALUATING THE JACOBIAN AND DF/DA
C   DT     - SIZE OF THE TIME STEPS.
C              CGS UNITS - SEC
C   EPSS   - TRESHOLD VALUE FOR SPECIES SENSITIVITY COEFFICIENTS.
C   EPST   - TRESHOLD VALUE FOR TEMPERATURE SENSITIVITY COEFFICIENTS.
C   FLRT   - THE MASS FLOW RATE.
C              CGS UNITS - GM/SEC
C   LTIME  - LTIME=.FALSE. CAUSES FUNCTION TO SET UP THE ALGEBRAIC
C            EQUATIONS.
C   HIN    - ARRAY OF SPECIES ENTHALPIES AT INLET TEMPERATURE TIN.
C              CGS UNITS - ERGS/MOLE
C              DIMENSION H(*) AT LEAST KK.
C   II     - NUMBER OF ELEMENTARY CHEMICAL REACTIONS.
C   ISEN   - LOGICAL ARRAY. IF ISEN(K)=.TRUE. THEN THE FIRST ORDER
C            SENSITIVITY COEFFICIENTS OF SPECIES K WITH RESPECT TO
C            RATE CONSTANTS WILL BE PRINTED OUT.
C              DIMENSION ISEN(*) AT LEAST KK.
C   KK     - NUMBER OF CHEMICAL SPECIES.
C   KSYM   - CHEMKIN SPECIES NAMES.
C              DIMENSION KSYM(*) AT LEAST KK.
C   LENRGY - IF LENRGY=.TRUE. THEN THE ENERGY EQUATION IS SOLVED.
C            IF LENRGY=.FALSE. THEN A SPECIFIED TEMPERATURE IS USED.
C   LFLRT  - IF LFLRT=.TRUE. THEN THE MASS FLOW RATE IS SPECIFIED AND
C              THE RESIDENCE TIME HAS TO BE CALCULATED.
C            IF LFLRT=.FALSE. THEN THE RESIDENCE TIME IS SPECIFIED AND
C              THE MASS FLOW RATE HAS TO BE CALCULATED.
C
C   LOUT   - UNIT FOR PRINTED OUTPUT.
C   LRECOV - UNIT FOR RECOVER FILE
C   LSAVE  - UNIT FOR SAVE FILE.
C   LSEN   - IF LSEN=.TRUE. THEN A FIRST ORDER SENSITIVITY ANALYSIS
C            IS CARRIED OUT.
C   LSENT  - IF LSENT=.TRUE. THEN A FIRST ORDER SENSITIVITIES OF THE
C            TEMPERATURE ARE PRINTED.
C   NATJ   - NUMBER OF DEPENDENT VARIABLES. NATJ=KK+1.
C   P      - THE PRESSURE.
C              CGS UNITS - DYNES/CM**2
C   PERTRB - PERTURBATON FOR THE JACOBIAN EVALUATION
C   Q      - THE HEAT LOSS OF THE REACTOR
C              CGS UNITS - ERGS/SEC
C   RELAT  - RELATIVE PERTURBATION FOR EVALUATING THE JACOBIAN AND DF/DA
C   S      - DEPENDENT VARIABLE ARRAY. THE TEMPERATURE IS STORED IN
C            T=S(NT), AND THE MASS FRACTIONS ARE IN Y(K)=S(NYS+K)
C              DIMENSION S(*) AT LEAST NATJ.
C   SN     - DEPENDENT VARIABLE ARRAY AT PREVIOUS TIMESTEP.
C   T      - THE USER-SPECIFIED TEMPERATURE.
C              CGS UNITS - K
C   TAU    - THE NOMINAL RESIDENCE TIME OF THE REACTOR
C              CGS UNITS - SEC
C   TIN    - THE INLET TEMPERATURE.
C              CGS UNITS - K
C   V      - THE VOLUME OF THE REACTOR
C              CGS UNITS - CM**3
C   WT     - THE ARRAY OF SPECIES MOLECULAR WEIGHTS.
C              CGS UNITS - GM/MOLE
C              DIMENSION WT(*) AT LEAST KK.
C   YIN    - ARRAY OF SPECIES INPUT MASS FRACTIONS.
C              DIMENSION REAC(*) AT LEAST KK.
C
C WORK AND SCRATCH SPACE-
C   A      - THE JACOBIAN OF THE GOVERNING EQUATIONS.
C              DIMENSION A(NATJ,*) EXACTLY NATJ FOR THE FIRST DIMENSION
C              AND AT LEAST NATJ FOR THE SECOND.
C   RCKWRK - FLOATING POINT CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   F      - ARRAY OF PERTURBED FUNCTION VALUES.
C               DIMENSION F(*) AT LEAST NATJ.
C   FN     - ARRAY OF THE UNPERTURBED FUNCTION VALUES.
C               DIMENSION F(*) AT LEAST NATJ.
C   ICKWRK - INTEGER CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   IPVT   - ARRAY OF PIVOTS.
C              DIMENSION IPVT(*) AT LEAST KK.
C
C   SCRTCH - SCRATCH SPACE.
C              DIMENSION(II, 5)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      LOGICAL LENRGY, LFLRT, LTIME, LSENT, ISEN(KK), LWRITE, IERR
      CHARACTER*16 ISENSI
      DATA ISENSI/'SENSITIVITY     '/
C
      LTIME = .FALSE.
C
C          FORM THE JACOBIAN, AND AT THE SAME TIME EVALUATE THE
C               UNPERTURBED FUNCTION FN
C
      CALL PSRJAC (KK, NATJ, LTIME, LENRGY, LFLRT, ABSOL, RELAT, DT,
     1             HIN, YIN, T, P, TAU, FLRT, V, Q, ICKWRK,
     2             RCKWRK, WT, SCRTCH, SN, S, F, FN, A, NT, NYS, NY)
C
C          FACTOR THE JACOBIAN
C
C*****precision > double
      CALL DGEFA (A, NATJ, NATJ, IPVT, INFO)
C*****END precision > double
C
C*****precision > single
C      CALL SGEFA (A, NATJ, NATJ, IPVT, INFO)
C*****END precision > single
C
      IF (INFO .NE. 0) THEN
         WRITE (LOUT,*) 'ERROR IN FACTORING THE JACOBIAN, INFO = ',INFO
         STOP
      ENDIF
C
C          COMPUTE THE RAW SENSITIVITY COEFFICIENTS WITH RESPECT TO
C             THE RATE CONSTANTS, D(MASS FRACTION)/D(RATE CONSTANT).
C
      WRITE (LSAVE) ISENSI
      WRITE (LOUT, 6001)
C
      DO 1000 I = 1, II
C
         CALL CKRDEX (I, RCKWRK, SAVEP)
         DP = RELAT*SAVEP + ABSOL
         CALL CKRDEX (-I, RCKWRK, SAVEP+DP )
C
         CALL PSRFUN (KK, LTIME, LENRGY, LFLRT, DT, HIN, YIN,
     1                T, P, TAU, FLRT, V, Q, ICKWRK, RCKWRK, WT,
     2                SCRTCH(1,1), SCRTCH(1,2), SN, S, F, NT, NYS, NY)
C
         CALL CKRDEX (-I, RCKWRK, SAVEP )
         DO 100 N = 1, NATJ
            SN(N) =  - (F(N)-FN(N)) / DP
100      CONTINUE
C
C*****precision > double
         CALL DGESL (A, NATJ, NATJ, IPVT, SN, 0)
C*****END precision > double
C
C*****precision > single
C         CALL SGESL (A, NATJ, NATJ, IPVT, SN, 0)
C*****END precision > single
C
C          SN(N) NOW CONTAINS THE RAW SENSIVITY ARRAY DS(N)/DAi
C
C          NORMALIZE THE SENSIVITY COEFFICIENTS
C
         SN(NT) = SN(NT) * SAVEP / S(NT)
C
         SUM = 0.E0
         DO 300 L = 1, KK
            SUM = SUM + SN(NYS+L) / WT(L)
300      CONTINUE
         CALL CKMMWY (S(NY), ICKWRK, RCKWRK, WTM)
C
         WTMSUM = WTM * SUM
         DO 400 K = 1, KK
            SN(NYS+K) = SAVEP * (SN(NYS+K) / S(NYS+K) - WTMSUM)
400      CONTINUE
C
         WRITE (LSAVE)  I, (SN(N),N=1,NATJ)
C
         LWRITE = LSENT .AND. (ABS(SN(NT)) .GE. EPST)
         DO 500 K = 1, KK
            LWRITE = LWRITE.OR.(ISEN(K) .AND. ABS(SN(NYS+K)).GE.EPSS)
500      CONTINUE
C
         IF (LWRITE) THEN
C
C           PRINT THE REACTION HOLLERITH
C
            CALL CKSYMR (I, LOUT, ICKWRK, RCKWRK, CCKWRK, LT, ISYM,
     1                   IERR)
            WRITE (LOUT, 6010) I, ISYM(:LT)
C
            KP = 0
            DO 600 K = 1, KK
               IF ( (ABS(SN(NYS+K)).GE.EPSS) .AND. ISEN(K) ) THEN
                  KP = KP + 1
                  KEEP(KP) = K
               ENDIF
               IF (KP.EQ.3 .OR. (K.EQ.KK .AND. KP.GT.0)) THEN
                  IF (EPSS .LT. 1.E-03) THEN
                     WRITE (LOUT, 6020)
     1               ( KSYM(KEEP(L)), SN(NYS+KEEP(L)) , L=1,KP)
                  ELSE
                     WRITE (LOUT, 6025)
     1               ( KSYM(KEEP(L)), SN(NYS+KEEP(L)) , L=1,KP)
                  ENDIF
                  KP = 0
               ENDIF
  600       CONTINUE
            IF (LSENT .AND. ABS(SN(NT)).GE.EPST)
     1         WRITE (LOUT, 6030) SN(NT)
         ENDIF
C
1000  CONTINUE
C
 6001 FORMAT (///20X,'    SENSITIVITY ANALYSIS ',//)
 6010 FORMAT (/'  ', I4, '.  ', A)
 6020 FORMAT ( 3(5X, A10, '= ', 1PE9.2))
 6025 FORMAT ( 3(5X, A10, '= ', F9.3))
 6030 FORMAT ( 5X,'TEMP      = ',1PE9.2)
C
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE PSRROP (LOUT, LSAVE, KK, II, KSYM, P, S, ICKWRK, 
     1                   RCKWRK, CCKWRK, IROP, EPSR, ROP,
     2                   CIK, CIKN, NT, NY)
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION S(*), ROP(*), CIK(*), CIKN(*), ICKWRK(*), RCKWRK(*)
      LOGICAL IROP(KK), IERR
      CHARACTER IROPRO*16, KSYM(*)*(*), ISYM*30, CCKWRK(*)*(*)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   EPSR   - TRESHOLD VALUE FOR RATE-OF-PRODUCTION COEFFICIENTS.
C   II     - NUMBER OF ELEMENTARY CHEMICAL REACTIONS.
C   IROP   - LOGICAL ARRAY. IF IROP(K)=.TRUE. THEN THE RATE-OF-PRODUC-
C            TION COEFFICIENTS OF SPECIES K WILL BE PRINTED OUT.
C              DIMENSION IROP(*) AT LEAST KK.
C   KK     - NUMBER OF CHEMICAL SPECIES.
C   KSYM   - CHEMKIN SPECIES NAMES.
C              DIMENSION KSYM(*) AT LEAST KK.
C   LOUT   - UNIT FOR PRINTED OUTPUT.
C   LRECOV - UNIT FOR RECOVER FILE
C   LROP   - IF LROP=.TRUE. THEN A RATE-OF-PRODUCTION ANALYSIS
C            IS CARRIED OUT.
C   LSAVE  - UNIT FOR SAVE FILE.
C   NATJ   - NUMBER OF DEPENDENT VARIABLES. NATJ=KK+1.
C   P      - THE PRESSURE.
C              CGS UNITS - DYNES/CM**2
C   S      - DEPENDENT VARIABLE ARRAY. THE TEMPERATURE IS STORED IN
C            T=S(NT), AND THE MASS FRACTIONS ARE IN Y(K)=S(NYS+K)
C              DIMENSION S(*) AT LEAST NATJ.
C
C WORK AND SCRATCH SPACE-
C   RCKWRK - FLOATING POINT CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C   ICKWRK - INTEGER CHEMKIN WORK SPACE.
C              DIMENSIONING - SEE CHEMKIN DOCUMENTATION.
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      DATA IROPRO /'PRODUCTION RATE '/
C
      WRITE (LOUT, 6001)
      WRITE (LSAVE) IROPRO
C
C         CALCULATE RATE-OF-PRODUCTION
C
      CNEPS = 1.0E-20
      CALL CKQYP (P, S(NT), S(NY), ICKWRK, RCKWRK, ROP)
      DO 100 K = 1, KK
         CALL CKCONT (K, ROP, ICKWRK, RCKWRK, CIK)
         WRITE (LSAVE)  K, (CIK(L),L=1,II)
C
         IF (IROP(K)) THEN
C
            WRITE (LOUT, 6010) K, KSYM(K)
C
C             NORMALIZATION
C
            CNORM1 = 0.0
            CNORM2 = 0.0
            DO 200 I = 1, II
               IF (CIK(I) .LT. 0.0) THEN
                  CNORM1 = CNORM1 - CIK(I)
               ELSE
                  CNORM2 = CNORM2 + CIK(I)
               ENDIF
200         CONTINUE

            DO 300 I = 1, II
               CIKN(I) = 0.0
               IF (CIK(I) .LT. 0.0) THEN
                  CIKN(I) = CIK(I) / MAX(CNEPS, CNORM1)
               ELSE
                  CIKN(I) = CIK(I) / MAX(CNEPS, CNORM2)
               ENDIF
               IF (ABS(CIKN(I)) .GE. EPSR) THEN
                  CALL CKSYMR (I, LOUT, ICKWRK, RCKWRK, CCKWRK, LT,
     1                         ISYM, IERR)
                  WRITE (LOUT, 6020) I, ISYM, CIKN(I), CIK(I)
               ENDIF
300         CONTINUE
C
            WRITE (LOUT, 6015) CNORM2, CNORM1
C
         ENDIF
100   CONTINUE
 6001 FORMAT (///10X,
     1   'NORMALIZED AND ABSOLUTE RATE-OF-PRODUCTION COEFFICIENTS',//)
 6010 FORMAT (/3X, I3, '. ', A10, 34X, 'NORMALIZED', 2X,
     1                                    '(MOLES/CC-SEC)' )
 6015 FORMAT (21X, 'NET RATE-OF-PRODUCTION (MOLES/CC-SEC)  = ', 1PE9.2,
     1      /21X, 'NET RATE-OF-CONSUMPTION (MOLES/CC-SEC) = ', 1PE9.2 )
 6020 FORMAT (15X, I4,'. ', A30, F9.3, 5X, '(', 1PE9.2, ')')
C
      RETURN
      END
