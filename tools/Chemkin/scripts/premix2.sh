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
      PARAMETER (LENLWK=100, LENIWK=5000, LENRWK=500000, LENCWK=100,
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
ELEMENTS   H   O    C   N END
SPECIES    CH4 CH3 CH2 CH CH2O HCO CO2 CO H2 H O2 O OH HO2 H2O2 H2O
           N2
END
REACTIONS
CH3+H+M=CH4+M                  8.0E26      -3.         0.
CH4+O2=CH3+HO2                 7.9E13       0.     56000.
CH4+H=CH3+H2                   2.2E4        3.      8750.
CH4+O=CH3+OH                   1.6E6        2.36    7400.
CH4+OH=CH3+H2O                 1.6E6        2.1     2460.
CH3+O=CH2O+H                   6.8E13       0.         0.
CH3+OH=CH2O+H2                 1.0E12       0.         0.
CH3+OH=CH2+H2O                 1.5E13       0.      5000.
CH3+H=CH2+H2                   9.0E13       0.     15100.
CH2+H=CH+H2                    1.4E19      -2.         0.
CH2+OH=CH2O+H                  2.5E13       0.         0.
CH2+OH=CH+H2O                  4.5E13       0.      3000.
CH+O2=HCO+O                    3.3E13       0.         0.
CH+O=CO+H                      5.7E13       0.         0.
CH+OH=HCO+H                    3.0E13       0.         0.
CH+CO2=HCO+CO                  3.4E12       0.       690.
CH2+CO2=CH2O+CO                1.1E11       0.      1000.
CH2+O=CO+H+H                   3.0E13       0.         0.
CH2+O=CO+H2                    5.0E13       0.         0.
CH2+O2=CO2+H+H                 1.6E12       0.      1000.
CH2+O2=CH2O+O                  5.0E13       0.      9000.
CH2+O2=CO2+H2                  6.9E11       0.       500.
CH2+O2=CO+H2O                  1.9E10       0.     -1000.
CH2+O2=CO+OH+H                 8.6E10       0.      -500.
CH2+O2=HCO+OH                  4.3E10       0.      -500.
CH2O+OH=HCO+H2O                3.43E9       1.18    -447.
CH2O+H=HCO+H2                  2.19E8       1.77    3000.
CH2O+M=HCO+H+M                 3.31E16      0.     81000.
CH2O+O=HCO+OH                  1.81E13      0.      3082.
HCO+OH=CO+H2O                  5.0E12       0.         0.
HCO+M=H+CO+M                   1.6E14       0.     14700.
HCO+H=CO+H2                    4.0E13       0.         0.
HCO+O=CO2+H                    1.0E13       0.         0.
HCO+O2=HO2+CO                  3.3E13      -0.4        0.
CO+O+M=CO2+M                   3.2E13       0.     -4200.
CO+OH=CO2+H                    1.51E7       1.3     -758.
CO+O2=CO2+O                    1.6E13       0.     41000.
HO2+CO=CO2+OH                  5.8E13       0.     22934.
H2+O2=2OH                      1.7E13       0.     47780.
OH+H2=H2O+H                    1.17E9       1.3     3626.
H+O2=OH+O                      5.13E16     -0.816  16507.
O+H2=OH+H                      1.8E10       1.0     8826.
H+O2+M=HO2+M                   3.61E17     -0.72       0.
   H2O/18.6/  CO2/4.2/  H2/2.86/  CO/2.11/  N2/1.26/
OH+HO2=H2O+O2                  7.5E12       0.         0.
H+HO2=2OH                      1.4E14       0.      1073.
O+HO2=O2+OH                    1.4E13       0.      1073.
2OH=O+H2O                      6.0E8        1.3        0.
H+H+M=H2+M                     1.0E18      -1.0        0.
H+H+H2=H2+H2                   9.2E16      -0.6        0.
H+H+H2O=H2+H2O                 6.0E19      -1.25       0.
H+H+CO2=H2+CO2                 5.49E20     -2.0        0.
H+OH+M=H2O+M                   1.6E22      -2.0        0.
   H2O/5/
H+O+M=OH+M                     6.2E16      -0.6        0.
   H2O/5/
H+HO2=H2+O2                    1.25E13      0.         0.
HO2+HO2=H2O2+O2                2.0E12       0.         0.
H2O2+M=OH+OH+M                 1.3E17       0.     45500.
H2O2+H=HO2+H2                  1.6E12       0.      3800.
H2O2+OH=H2O+HO2                1.0E13       0.      1800.
END
EOF


cat << EOF > premix2.inp                      # premix premix.input data
/  freely propagating flame
FREE
ENRG
/  initial flow-rate estimate
FLRT  .04   ! gm/cm**2-sec
/   atmospheric pressure
PRES  1.0   ! atmospheres
/   initial grid and profile specification
NPTS      6
XEND      0.3  ! cm
XCEN      0.1  ! cm
WMIX      1.0  ! cm
/   temperature to fixed for the flame speed computation
TFIX  400.
/   mesh adaptation criteria
GRAD     0.9
CURV     0.9
/   unreacted fuel-oxidizer makeup
MOLE
REAC  CH4  0.0950
REAC  O2   0.19
REAC  N2   0.715
/   estimated product mole fractions
PROD  H2O  0.190
PROD  CO2  0.095
PROD  N2   0.715
/   estimated peak intermediate mole fractions
INTM  CO   0.08
INTM  HCO   0.00001
INTM  HO2   0.0001
INTM   O    0.0001
INTM  H2O2  0.0001
INTM   H    0.02
INTM   H2   0.01
INTM   OH   0.001
INTM   CH2  0.0001
INTM   CH   0.00001
INTM   CH2O 0.001
INTM   CH3  0.0005
/   convergence tolerance for Newton
ATOL  1.E-9
RTOL  1.E-4
/   convergence tolerance for timestepping
ATIM  1.E-5
RTIM  1.E-5
/   maximum priting
PRNT    2
/   time step control
TIME   100   5.0E-7  ! sec
TIM2   200   1.0E-6  ! sec
/   estimated temperature profile
TEMP   0.0    298.
TEMP   0.03   300.
TEMP   0.05   400.
TEMP   0.06   766.
TEMP   0.07  1512.
TEMP   0.08  1892.
TEMP   0.09  2000.
TEMP   0.1   2030.
TEMP   0.2   2111.
TEMP   0.35  2190.
TEMP  10.0   2190.
/   a contiuation run will follow
CNTN
END
/
/      This is the second continuation.  The mesh adaptation is more
/   stringent, and th computational comain is expanded.  Yet another
/   continuation will follow after this one.
/
GRAD   0.5
CURV   0.7
XSTR  -0.5
XEND   8.0
CNTN
END
/
/      This is the third and final continuation.  The mesh adaptation
/   is more stringent, and th computational domain is again expanded.
/
GRAD   0.2
CURV   0.5
XEND  10.0
XSTR  -2.0
END
EOF

touch makefile; make chem.exe tran.exe premix.exe
chem.exe; tran.exe > tran.out
rm save.bin recov.bin; premix.exe < premix2.inp > premix2.out

ENDSH
