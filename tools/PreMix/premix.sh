#!/bin/sh
# to execute:  sh premix.sh logname &

sh 1> ${1}.log 2>&1 << ENDSH     # shell run output is "logname.log"

set -x                          # echo all commands to stdout

#cd /scr/$LOGNAME              #go to user's scratch directory
#mkdir "${1}$$"                #make subdirectory /myrun##
#cd "${1}$$"                   #go to /myrun##

cat << EOF > makefile

include chemmake.h

OBJS = driv.o premix.o cklib.o tranlib.o twopnt.o math.o $(MACH) 
INPS = therm.dat tran.dat chem.inp premix.inp
OUTS = chem.bin chem.out tran.bin tran.out premix.out save.bin recov.bin
EXES = chem.exe tran.exe premix.exe

chem.exe : ckinterp.o
	   $(LINK) chem.exe ckinterp.o

tran.exe : tranfit.o cklib.o polfit.o xerror.o $(MACH)
	    $(LINK) tran.exe tranfit.o cklib.o polfit.o xerror.o $(MACH)

premix.exe : $(OBJS)
	   $(LINK) premix.exe $(OBJS)

EOF
	
cat << EOF > driv.f              # Flame code driver program
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
EOF

cat << EOF > chem.inp                # chemkin mechanism
ELEMENTS
H  O  AR
END
SPECIES
H2 O2 H O OH HO2 H2O2 H2O AR
END
REACTIONS
H2+O2=2OH                                  1.7E13    0.0    47780.
OH+H2=H2O+H      1.17E9    1.3     3626. !D-L$W
H+O2=OH+O        5.13E16  -0.816  16507. !JAM,JCP 1981
O+H2=OH+H                                  1.8E10    1.0     8826.
H+O2+M=HO2+M     2.1E18    -1.0       0. !SLACK
   H2O/21./   H2/3.3/  O2/0.0/
H+O2+O2=HO2+O2   6.7E19   -1.42       0. !SLACK,JAN
OH+HO2=H2O+O2                              5.0E13    0.0     1000.
H+HO2=2OH                                  2.5E14    0.0     1900.
O+HO2=O2+OH                                4.8E13    0.0     1000.
2OH=O+H2O        6.0E+8    1.3        0. !COHEN-WEST.
H2+M=H+H+M                                 2.23E12   0.5    92600.
  H2O/6/   H/2/  H2/3/
O2+M=O+O+M                                 1.85E11   0.5    95560.
H+OH+M=H2O+M                               7.5E23   -2.6        0.
  H2O/20/
H+HO2=H2+O2                                2.5E13    0.0      700.
HO2+HO2=H2O2+O2                            2.0E12    0.0        0.
H2O2+M=OH+OH+M                             1.3E17    0.0    45500.
H2O2+H=HO2+H2                              1.6E12    0.0     3800.
H2O2+OH=H2O+HO2                            1.0E13    0.0     1800.
END
EOF


cat << EOF > premix.inp                      # premix premix.input data
/   flame configuration, burner stabilized with specified temperature
BURN
TGIV
/   in the event of a Newton failure, take 100 timesteps of 1.E-6
TIME    100  1.00E-6
/   begin on a uniform mesh of 6 points
NPTS      6
/      definition of the computational interval
XEND  10.0
XCEN   5.0
WMIX   10.0
/   pressure and inlet mass flow rate
PRES  0.0329 (atmospheres)
FLRT  4.63E-3 (g/cm**2-sec)
/   adaptive mesh criteria
GRAD  0.2
CURV  0.5
/   unreacted mole fractions
MOLE
REAC   O2  0.09
REAC   AR   .63
REAC   H2  0.28
/   estimated products
PROD   AR  0.68
PROD   H2O 0.12
PROD   H2  0.15
PROD   OH  0.02
PROD   O   0.02
PROD   H   0.01
/   estimated intermediate mole fractions
INTM   H2O2  .00001
INTM   HO2   .001
INTM   H2    .01
/   tolerances for the Newton iteration
ATOL  1.E-10
RTOL  1.E-4
/   tolerances for the time step Newton iteration
ATIM  1.E-5
RTIM  1.E-5
/   print control
PRNT    1
/      given temperature profile
TEMP   0.      373.7
TEMP    .1250  484.5
TEMP    .250   583.7
TEMP    .375   672.2
TEMP    .5     753.5
TEMP    .75    901.4
TEMP   1.0    1027.
TEMP   1.25   1120.
TEMP   1.5    1184.
TEMP   2.0    1260.
TEMP   3.0    1348.
TEMP   6.0    1475.
TEMP  10.0    1524.
END
EOF

touch makefile; make chem.exe tran.exe premix.exe
chem.exe; tran.exe > tran.out
rm save.bin recov.bin; premix.exe < premix.inp > premix.out

ENDSH
