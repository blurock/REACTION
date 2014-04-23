FC=gfortran
include chemmake.h

all: EXES

OBJS  = driver.o psr.o eqlib.o stanlib.o cklib.o twopnt.o math.o 
EXES:   chem.exe psr.exe
INPS  = therm.dat chem.inp psr.inp
OUTS  = chem.bin chem.out save.bin recov.bin psr.out

clean: 
	rm *.o

chem.exe : ckinterp.o
	      gfortran -o chem.exe -g ckinterp.o

psr.exe : 
	  gfortran -o psr.exe $(OBJS)  
