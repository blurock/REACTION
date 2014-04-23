#!/bin/sh
# to execute:  sh shock.sh logname  &

sh 1> ${1}.log 2>&1 << ENDSH

set -x

#cd /scr/$LOGNAME              #go to user's scratch directory
#mkdir "${1}$$"                #make subdirectory /myrun##
#cd "${1}$$"                   #go to /myrun##

cat << EOF > makefile

include chemmake.h

OBJS = shock.o cklib.o vode.o math.o
INPS = therm.dat chem.inp shock.inp
OUTS = chem.bin chem.out shock.out shock.bin
EXES = chem.exe shock.exe

chem.exe : ckinterp.o
	   $(LINK) chem.exe ckinterp.o

shock.exe : $(OBJS)
	  $(LINK) shock.exe $(OBJS)
EOF


cat << EOF > chem.inp
ELEMENTS O N AR END

SPECIES  O2  N2  NO  N  O  AR END

REACTIONS
N2+O2=NO+NO             9.1E24   -2.5   128500.
N2+O=NO+N               7.0E13    0.     75000.
O2+N=NO+O               1.34E10   1.0     7080.
O2+M=O+O+M              3.62E18  -1.0   118000.
N2/2/  O2/9/   O/25/
N2+M=N+N+M              1.92E17  -0.5   224900.
N2/2.5/  N/0/
N2+N=N+N+N              4.1E22   -1.5   224900.
NO+M=N+O+M              4.0E20   -1.5   150000.
NO/20/  O/20/  N/20/
END
EOF


cat << EOF > shock.inp
TITL  SHOCK TUBE CODE TEST
P1A   6.58E-3  ! ATM
T1    296.     ! K
VSHK  2.8E5    ! CM/SEC
INIT  N2   0.78118  ! MOLE FRACTION
INIT  O2   0.20948  ! MOLE FRACTION
INIT  AR   0.00934  ! MOLE FRACTION
TSTR  0.
TEND  310.E-6
DT    10.E-6
DIA   3.81          !CM**2
VISC  177.7E-6
RTOL  1.E-4
ATOL  1.E-8
CONC
ISKB
END
EOF

touch makefile; make chem.exe shock.exe
chem.exe; shock.exe < shock.inp > shock.out

ENDSH
