
include chemmake.h

OBJS = conp.o cklib.o vode.o math.o
INPS = therm.dat chem.inp conp.inp
OUTS = chem.out chem.bin conp.out
EXES = chem.exe conp.exe

chem.exe: ckinterp.o
	   chem.exe ckinterp.o

conp.exe: $(OBJS) 
	   $(LINK) conp.exe $(OBJS)

