
include ../source/chemmake.h

OBJS = driver.o ../objects/senkin.o ../objects/cklib.o ../objects/dasac.o ../objects/xerror.o 
INPS = therm.dat chem.inp senk.inp
OUTS = chem.bin chem.out tign.out save.bin senk.out
EXES = chem.exe senk.exe

chem.exe : ../objects/ckinterp.o
	    chem.exe ../objects/ckinterp.o

senk.exe : 
	    senk.exe 
