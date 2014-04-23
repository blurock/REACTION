
#*****compiler > cray
#FFLAGS =
#LINK   = segldr -f zeros -o
#MACH = cmach.o
#*****END compiler > cray

#*****compiler > sun
FFLAGS = -static -O2
LINK = g77 -o
MACH = -lf2c -lm
#*****END compiler > sun

#*****compiler > sgi R3000
#FFLAGS = -static -O2 -mips1
#LINK   = f77 -o
#MACH = dmach.o
#*****END compiler > sgi R3000

#*****compiler > sgi R4X00
#FFLAGS = -static -g -mips2
#LINK   = f77 -o
#MACH = dmach.o
#*****END compiler > sgi R4X00

#*****compiler > ibm rsc6000
#FFLAGS = -static -O2
#LINK    = xlf -o
#MACH  = dmach.o
#*****END compiler > ibm rsc6000

#*****compiler > alpha
#FFLAGS = -static -O2
#LINK   = f77 -o
#MACH = amach.o
#*****END compiler > alpha

#*****compiler > HP
#FFLAGS = -K -O
#LINK = f77 -o
#MACH = dmach.o
#*****END compiler > HP

