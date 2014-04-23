      PROGRAM DRIVER
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
      PARAMETER (LENIWK=750000, LENRWK=6000000, LENCWK=500, LENSYM=16,
     1           LIN=5, LOUT=6, LSAVE=7, LIGN=9, LREST=10,
     2           LINKCK=25)
      DIMENSION IWORK (LENIWK), RWORK (LENRWK)
      LOGICAL LEXIST
      CHARACTER CWORK(LENCWK)*(LENSYM)
      CHARACTER NAMES(100)*(LENSYM)
      DIMENSION HML(100),HORT(100),SOR(100),CPOR(100),GML(100)
      INCLUDE 'ckstrt.h'
C
C     LIN    = Unit number for Keyword input
C     LOUT   = Unit number for text output to terminal
C     LIGN   = Unit number for text output file
C     LSAVE  = Unit number for binary output file
C     LINKCK = Unit number for CHENKIN binary file
C     LREST  = Unit number for binary restart file
C     LENIWK = Length of integer work array
C     LENRWK = Length of real work array
C     LENCWK = Length of character work array
C     LENSYM = Length of a character string in character work array
C     IWORK  = Integer work array
C     RWORK  = Real work array
C     CWORK  = Character work array
C
      OPEN (LINKCK,  FORM='UNFORMATTED', STATUS='UNKNOWN',
     1               FILE='chem.bin')
      OPEN (LSAVE,  FORM='UNFORMATTED', STATUS='UNKNOWN',
     1              FILE='save.bin')
      OPEN (LIGN,  FORM='FORMATTED', STATUS='UNKNOWN',
     1             FILE='tign.out')
      OPEN (LREST, FORM='UNFORMATTED', STATUS='UNKNOWN',
     1             FILE='rest.bin')

      IPICK = 20
C
      CALL CKLEN (LINKCK, LOUT, LENI, LENR, LENC)
      IF (LENI .LE. LENIWK 
     1      .AND. LENR.LE.LENRWK 
     2      .AND. LENC.LE.LENCWK) THEN
         CALL CKINIT (LENIWK, LENRWK, LENCWK, 
     1                LINKCK, LOUT, IWORK(IPICK), RWORK, CWORK)
         CALL CKINDX (IWORK, RWORK, MM, KK, II, NFIT)
      ENDIF



      WRITE(15,221)  NMM , NKK , NII , MXSP, MXTB, MXTP, NCP , NCP1
      
      CALL CKRP(IWORK,RWORK,RU,RUC,PA)
      WRITE(15,220) RU,RUC,PA
 220  FORMAT(2X,1PE12.4,2X,1PE12.4,2X,1PE12.4)
      CALL CKSYMS(CWORK,LOUT,NAMES,KERR)
      DO 20 ISPECIES = 1,NKK
      T=200.0
      DO 15 III = 1,10
      CALL CKHORT(T,IWORK,RWORK,HORT)
      CALL CKSOR(T,IWORK,RWORK,SOR)
      CALL CKHML(T,IWORK,RWORK,HML)
      CALL CKGML(T,IWORK,RWORK,GML)

      WRITE(15,222) NAMES(ISPECIES),T,
     1              HORT(ISPECIES)*RUC*T,
     2              SOR(ISPECIES)*RUC,
     3              HML(ISPECIES),
     4              GML(ISPECIES)*RUC/RU

 221  FORMAT('THE ENTHALPIES: ',I5,I5,I5,I5,I5,I5,I5,I5)

 222  FORMAT(2X,A10,1PE12.4,
     1               '  ENTHALPY=',1PE12.4,5X,
     2                 'ENTROPY=',1PE12.4,5X,
     3                 'ENTHALPY=',1PE12.4,
     4                 'EQUIL=',1PE12.4)
       T=T+100
 15   CONTINUE
 20   CONTINUE

C
C     PASS CONTROL TO SENKIN
C
      CALL SENKIN (LIN, LOUT, LINKCK, LSAVE, LIGN, LREST,
     1             LENRWK, RWORK, LENIWK, IWORK, LENCWK, CWORK)
C

      END
      SUBROUTINE TEMPT (A, B)
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
      RETURN
      END
C
      SUBROUTINE VOLT (A, B, C)
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
      RETURN
      END
