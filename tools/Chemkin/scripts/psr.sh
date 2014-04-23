#!/bin/sh
# to execute:  sh psr.sh logname nohup &

sh 1> ${1}.log 2>&1 << ENDSH     # shell run output is "logname.log"

set -x                          # echo all commands to stdout

#cd /scr/$LOGNAME              #go to user's scratch directory
#mkdir "${1}$$"                #make subdirectory /myrun##
#cd "${1}$$"                   #go to /myrun##

cat << EOF > makefile

include chemmake.h

OBJS  = driver.o psr.o eqlib.o stanlib.o cklib.o twopnt.o  \
         math.o $(MACH)
EXES  = chem.exe psr.exe
INPS  = therm.dat chem.inp psr.inp
OUTS  = chem.bin chem.out save.bin recov.bin psr.out

chem.exe : ckinterp.o
	     $(LINK) chem.exe ckinterp.o

psr.exe : $(OBJS)
	$(LINK)  psr.exe $(OBJS)
EOF

cat << EOF > driver.f               # PSR driver program
      PROGRAM DRIVER
C
C*****precision > double
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      PARAMETER (LENRWK=10000, LENIWK=10000, LENLWK=200, LENCWK=100,
     1           LIN=5, LOUT=6, LREST=14, LSAVE=15, LRCRVR=16,
     2           LINKCK=25)
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
EOF

cat << EOF > chem.inp                       # Chemkin chem.inpanism
ELEMENTS O H N END
SPECIES  O  O2  H  H2  OH  HO2  H2O  H2O2 N2 END
REACTIONS
  H+O2 = O+OH            5.1E16   -0.82  16510 !MILLER 81
  H2+O = H+OH            1.8E10    1.0    8830 !MILLER 77
  H2+OH = H2O+H          1.2E09    1.3    3630 !DIXON-LEWIS 77
  OH+OH = H2O+O          6.0E08    1.3    0.0 !COHEN 79
  H+OH+M = H2O+M         7.5E23   -2.6    0.0 !MILLER 77
    H2O /20.0/
  O2+M = O+O+M           1.9E11    0.5   95560 !MILLER 81
  H2+M = H+H+M           2.2E12    0.5   92600 !MILLER 77
    H2O /6.0/
    H   /2.0/
    H2  /3.0/
  H2+O2 = OH+OH           1.7E13    0.0   47780 !MILLER 77
  H+O2+M = HO2+M          2.1E18   -1.0    0.0 !SLACK 77
    H2O /21.0/
    H2  /3.3/
    O2  /0.0/
    N2  /0.0/
  H+O2+O2 = HO2+O2       6.7E19   -1.42    0.0 !SLACK 77
  H+O2+N2 = HO2+N2       6.7E19   -1.42    0.0 !SLACK 77
  HO2+H = H2+O2          2.5E13    0.0     700 !LLOYD 74
  HO2+H = OH+OH          2.5E14    0.0    1900 !LLOYD 74
  HO2+O = OH+O2          4.8E13    0.0    1000 !LLOYD 74
  HO2+OH = H2O+O2        5.0E13    0.0    1000 !LLOYD 74
  HO2+HO2 = H2O2+O2      2.0E12    0.0    0.0 !TROE 69
  H2O2+M = OH+OH+M       1.2E17    0.0   45500 !BAULCH 72
  H2O2+H = HO2 + H2      1.7E12    0.0    3750 !BAULCH 72
  H2O2+OH = H2O+HO2      1.0E13    0.0    1800 !BAULCH 72
END
EOF



cat << EOF > psr.inp
ENRG
EQUI  1.0
FUEL  H2   0.8
FUEL  N2   0.2
OXID  O2   0.21
OXID  N2   0.79
PROD  H2O
PROD  N2
PRES  1.0
VOL   67.4
TAU   3.0E-05
TINL  298.
QLOS  0.0
PRNT  2
TIME  50   1.E-6
TIM2  50   1.E-6
TEMP  1700
SFLR  1.E-10
CNTN
END
/        continuation problem
PRNT  1
SEN   H2 O2 H2O
EQUI  1.2
END
EOF

touch makefile; make chem.exe psr.exe
chem.exe; rm save.bin recov.bin; psr.exe < psr.inp > psr.out

ENDSH
