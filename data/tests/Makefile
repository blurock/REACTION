STD_INCLUDES = -I$(CCROOT)/genc\
               -I$(CCROOT)/genh\
               -I$(CCROOT)/standard

DEPLIBS = -L$(CCROOT)/lib\
           -lbasis\
           -lm

CFLAGS = $(CCOPTIONS) $(STD_INCLUDES)

AeE: AeE.o
	$(CC) -o AeE $(CCFLAGS)  AeE.o $(DEPLIBS)

