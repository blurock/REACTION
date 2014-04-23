
include chemmake.h

OBJS = driv.o premix.o cklib.o tranlib.o twopnt.o math.o  
INPS = therm.dat tran.dat chem.inp premix.inp
OUTS = chem.bin chem.out tran.bin tran.out premix.out save.bin recov.bin
EXES = chem.exe tran.exe premix.exe

chem.exe : ckinterp.o
	    chem.exe ckinterp.o

tran.exe : tranfit.o cklib.o polfit.o xerror.o 
	     tran.exe tranfit.o cklib.o polfit.o xerror.o 

premix.exe : 
	    premix.exe 

