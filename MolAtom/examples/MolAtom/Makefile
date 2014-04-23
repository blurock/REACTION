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
	-lMolAtom\
	-lStaticAtom\
	-lParameterizedFunction\
        -lGeneralGraph\
	-lMenuObjects\
	-lInstanceObjects\
	-lFunctionReal1DObjects\
	-lParameterizedFunction\
	-lOperationObjects\
	-lDataObjects\
	-lLogicalOperations\
	-lNumericOperations\
	-lLogicalObjects\
	-lNumericObjects\
	-lCoreDataObjects\
	-lNonStandard\
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
