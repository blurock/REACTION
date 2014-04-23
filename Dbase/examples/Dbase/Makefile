# FILE     Makefile
# PACKAGE  Dbase
# AUTHOR   Edward S. Blurock
#
# CONTENT
#   Makefile for example programs of the "Dbase" package.
#
# COPYRIGHT (C)  1997 CoreObjects Library

#V PackageRoot . . . . . . . . . . . . . . The root directory of this package.
PackageRoot:=$(shell cd ../..; pwd)


#I
include ../../etc/CoreObjects/MakeDef

LIBS =\
	-L$(PackageRoot)/lib\
	-L$(CodeBaseRoot)/lib\
  -lDbase\
	-lStaticAtom\
	-lMenuObjects\
	-lInstanceObjects\
	-lParameterizedFunction\
        -lGeneralGraph\
	-lFunctionReal1DObjects\
	-lParameterizedFunction\
	-lOperationObjects\
	-lDataObjects\
	-lLogicalOperations\
	-lNumericOperations\
	-lLogicalObjects\
	-lNumericObjects\
	-lCoreDataObjects\
        -lGraph\
	-lPairs\
        -lSystem\
	-lMatrixNumeric\
	-lVector\
        -lONames\
        -lMixedClasses\
        -lString\
        -lObjects\
        -lCommBuffer\
        -lgdbm\
        -lm

OBJECTS = main.o 

#V
ALL= otest

#F Targets.

all: $(ALL)

otest: $(OBJECTS)
	CompileC++ -o otest $(OBJECTS) $(LIBS)


cleaner::
	$(RM) $(ALL)
