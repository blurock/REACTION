      PROGRAM DRIVER
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      PARAMETER (LENRWK=10000000,LENIWK=10000000,
     1            LENLWK=20000,LENCWK=10000,
     2           LIN=5, LOUT=6, LREST=14, LSAVE=15, LRCRVR=16,
     3           LINKCK=25)
      DIMENSION RWORK(LENRWK), IWORK(LENIWK)
      CHARACTER CWORK(LENCWK)*16
      LOGICAL LWORK(LENLWK)
C
      OPEN (LINKCK, FORM='UNFORMATTED',STATUS='UNKNOWN',
     1              FILE='chem.bin')
      OPEN (LREST,  FORM='UNFORMATTED',STATUS='UNKNOWN',
     1              FILE='rest.bin')
      OPEN (LRCRVR, FORM='UNFORMATTED',STATUS='UNKNOWN',
     1              FILE='recov.bin')
      OPEN (LSAVE,  FORM='UNFORMATTED',STATUS='UNKNOWN',
     1              FILE='save.bin')
C
      CALL PSR (LIN, LOUT, LINKCK,  LREST, LSAVE, LRCRVR, LENLWK,
     1          LWORK, LENIWK, IWORK, LENRWK, RWORK, LENCWK, CWORK)
C
      STOP
      END
