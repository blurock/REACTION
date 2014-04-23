#!/bin/sh
# to execute:  sh senkin.sh logname  &

sh 1> ${1}.log 2>&1 << ENDSH     # perform activities until "ENDSH" and
                                 # put standard output/error messages
                                 # into a logname.log file

set -x                           # echo each command to logname.log

#cd /scr/$LOGNAME              #go to user's scratch directory
#mkdir "${1}$$"                #make subdirectory /myrun##
#cd "${1}$$"                   #go to /myrun##

set OBJS = driver.o senkin.o cklib.o dasac.o xerror.o 
set INPS = therm.dat chem.inp senk.inp
set OUTS = chem.bin chem.out tign.out save.bin senk.out
set EXES = chem.exe senk.exe

cat << EOF > makefile

include ../../source/chemmake.h


chem.exe : ckinterp.o
	   f77 -o chem.exe ckinterp.o

senk.exe : driver.o senkin.o cklib.o dasac.o xerror.o
	   f77 -o senk.exe driver.o senkin.o cklib.o dasac.o xerror.o
EOF

cat << EOF > driver.f            # store driver.f until "EOF"
      PROGRAM DRIVER
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
      PARAMETER (LENIWK=7500, LENRWK=60000, LENCWK=500, LENSYM=16,
     1           LIN=5, LOUT=6, LSAVE=7, LIGN=9, LREST=10,
     2           LINKCK=25)
      DIMENSION IWORK (LENIWK), RWORK (LENRWK)
      LOGICAL LEXIST
      CHARACTER CWORK(LENCWK)*(LENSYM)
      DIMENSION HML(100)
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
      TEMPERATURE = 298.0
      CALL CKHML(TEMPERATURE,IWORK,RWORK,HML)
      DO 20 ISPECIES = 1,NKK
         WRITE(15,222) ISPECIES,HML(ISPECIES)
 221  FORMAT('THE ENTHALPIES: ',I5,I5,I5,I5,I5,I5,I5,I5)
 222  FORMAT(2X,'SPECIES: ',I5,5X,'ENTHALPY=',1PE12.4)
 20   CONTINUE

C
C     PASS CONTROL TO SENKIN
C
C      CALL SENKIN (LIN, LOUT, LINKCK, LSAVE, LIGN, LREST,
C     1             LENRWK, RWORK, LENIWK, IWORK, LENCWK, CWORK)
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
EOF

cat << EOF > chem.inp                  # store "chem.inp" until EOF
ELEMENTS  H   O   N  END

SPECIES H2  H  O2  O  OH  HO2  H2O2  H2O   N2 END

REACTIONS
H+O2+M=HO2+M                            3.61E17  -0.72       0.
  H2O/18.6/  H2/2.86/
H+H+M=H2+M                              1.0E18   -1.0        0.
H+H+H2=H2+H2                            9.2E16   -0.6        0.
H+H+H2O=H2+H2O                          6.0E19   -1.25       0.
H+OH+M=H2O+M                            1.6E22   -2.0        0.
  H2O/5/
H+O+M=OH+M                              6.2E16   -0.6        0.
  H2O/5/
O+O+M=O2+M                              1.89E13   0.0    -1788.
H2O2+M=OH+OH+M                          1.3E17    0.0    45500.
H2+O2=2OH                               1.7E13    0.0    47780.
OH+H2=H2O+H                             1.17E9    1.3     3626.
O+OH=O2+H                               3.61E14  -0.5        0.
O+H2=OH+H                               5.06E4    2.67    6290.
OH+HO2=H2O+O2                           7.5E12    0.0      0.0
H+HO2=2OH                               1.4E14    0.0     1073.
O+HO2=O2+OH                             1.4E13    0.0     1073.
2OH=O+H2O                               6.0E+8    1.3        0.
H+HO2=H2+O2                             1.25E13   0.0        0.
HO2+HO2=H2O2+O2                         2.0E12    0.0        0.
H2O2+H=HO2+H2                           1.6E12    0.0     3800.
H2O2+OH=H2O+HO2                         1.0E13    0.0     1800.
END
EOF

cat << EOF > senk.inp     # create senkin input file senk.inp
SENS
CONP
PRES 1.0
TEMP 1000.
TIME 2.E-4
DELT 1.E-4
REAC H2  2
REAC O2  1
REAC N2  4
END
EOF

echo make chem.exe
make -f makefile chem.exe 
echo make senk.exe
make -f makefile senk.exe
echo run
chem.exe; rm save.bin;  senk.exe < senk.inp > senk.out

ENDSH

