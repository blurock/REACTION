      PROGRAM DRIVER
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
C          LENLWK allocates the logical working space
C          LENIWK allocates the integer working space
C          LENRWK allocates the real working space
C          LENCWK allocates the character working space
C          LENSYM is the length of a character string
C          LIN is the unit from which user input is read
C          LOUT is the unit to which printed output is written
C          LRIN is the unit from which the restart file is read
C          LROUT is the unit to which the save file is written
C          LRCRVR is the unit to which the scratch save file is written
C          LINKCK is unit from which the Chemkin binary file is read
C          LINTP is unit from which the Transport binary file is read
C
      PARAMETER (LENLWK=100, LENIWK=5000, LENRWK=71000, LENCWK=100,
     1           LENSYM=16, LIN=5, LOUT=6, LRIN=14, LROUT=15,
     2           LRCRVR=16, LINKCK=25, LINKTP=35)
C
C          All storage needed by the flame code is allocated in the
C          following three arrays.  LWORK is for logical variables,
C          IWORK is for integer variables, RWORK is of real variables,
C          CWORK is for character variables.
C
      DIMENSION IWORK(LENIWK), RWORK(LENRWK)
      CHARACTER CWORK(LENCWK)*(LENSYM)
      LOGICAL LWORK(LENLWK)
C
C          NMAX is the total number of grid points allowed
      NMAX = 65
C            open the restart file
      OPEN (LRIN, FORM='UNFORMATTED', STATUS='UNKNOWN',
     1            FILE='rest.bin')
C            open the save output file
      OPEN (LROUT, FORM='UNFORMATTED', STATUS='UNKNOWN',
     1             FILE='save.bin')
C            open the recover output file
      OPEN (LRCRVR, FORM='UNFORMATTED', STATUS='UNKNOWN',
     1              FILE='recov.bin')
C            open the Chemkin binary file
      OPEN (LINKCK, FORM='UNFORMATTED', STATUS='UNKNOWN',
     1              FILE='chem.bin')
C            open the Transport binary file
      OPEN (LINKTP, FORM='UNFORMATTED', STATUS='UNKNOWN',
     1              FILE='tran.bin')
C
      CALL PREMIX (NMAX, LIN, LOUT, LINKCK, LINKTP, LRIN, LROUT,
     1             LRCRVR, LENLWK, LWORK, LENIWK, IWORK, LENRWK,
     2             RWORK, LENCWK, CWORK)
C
      STOP
      END
