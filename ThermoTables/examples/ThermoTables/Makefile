# FILE     Makefile
# PACKAGE  StaticAtom
# AUTHOR   Edward S. Blurock
#
# CONTENT
#   Makefile for example programs of the "StaticAtom" package.
#
# COPYRIGHT (C)  1997 Reaction Library

#V PackageRoot . . . . . . . . . . . . . . The root directory of this package.
PackageRoot:=$(shell cd ../..; pwd)


#I
include ../../etc/Reaction/MakeDef

LIBS =\
	-L$(PackageRoot)/lib\
	-L$(CodeBaseRoot)/lib\
	-L$(CodeBaseRoot)/include/CC/lib\
	-lThermoTables\
        -lSystem\
        -lVector\
        -lMatrixNumeric\
        -lMatrixUtilities\
        -lPrimitiveStats\
        -lONames\
        -lMixedClasses\
        -lString\
        -lObjects\
        -lCommBuffer\
        -lbasis\
        -lm -lgdbm

OBJECTS = main.o 



#V
ALL= otest


#F Targets.

all: $(ALL)

otest: $(OBJECTS)
	CompileC++ -o otest $(OBJECTS) $(LIBS)


cleaner::
	$(RM) $(ALL)
