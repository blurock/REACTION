FFLAGS = -g -static 
LINK = f95 -g -o
MACH = dmach.o
FC = f95

all: chem.exe senk.exe

chem.exe : ckinterp.o
	   $(FC) -o chem.exe ckinterp.o

senk.exe : driver.o senkin.o cklib.o dasac.o xerror.o
	   $(FC) -g -o senk.exe driver.o senkin.o cklib.o dasac.o xerror.o
