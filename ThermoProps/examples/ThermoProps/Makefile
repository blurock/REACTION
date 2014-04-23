# FILE     Makefile
# PACKAGE  ThermoProps
# AUTHOR   Edward S. Blurock
#
# CONTENT
#   Makefile for example programs of the "ThermoProps" package.
#
# COPYRIGHT (C)  1997 Reaction Library

#V PackageRoot . . . . . . . . . . . . . . The root directory of this package.
PackageRoot:=$(shell cd ../..; pwd)


#I
include ../../etc/Reaction/MakeDef

LIBS=\
	-L$(PackageRoot)/lib\
	-L$(CodeBaseRoot)/lib\
	-L$(CodeBaseRoot)/include/CC/lib\
	-lUtilities\
        -lThermoProps\
	-lThermoTables\
	-lRxn\
	-lMolecule\
	-lMolBond\
	-lMolAtom\
	-lDbase\
	-lStaticAtom\
	-lMolStats\
	-lParameterizedFunction\
        -lGeneralGraph\
	-lMenuObjects\
	-lAlgorithmObjects\
	-lDirectedTreeObjects\
	-lSelectObjects\
	-lInstanceObjects\
	-lFunctionReal1DObjects\
	-lParameterizedFunction\
	-lOperationObjects\
	-lDataObjects\
	-lLogicalOperations\
	-lNumericOperations\
	-lLogicalObjects\
	-lNumericObjects\
        -lDbase\
	-lCoreDataObjects\
	-lNonStandard\
	-lPrimitiveStats\
        -lGraph\
	-lPairs\
        -lAnalysisSystem\
	-lMatrixNumeric\
	-lMatrixUtilities\
	-lVector\
        -lONames\
        -lMixedClasses\
        -lString\
        -lObjects\
        -lCommBuffer\
        -lm -lgdbm

OBJECTS = main.o 

#V
ALL= otest

#F Targets.

all: $(ALL)

otest: $(OBJECTS)
	CompileC++ -g -o otest $(OBJECTS) $(LIBS)


cleaner::
	$(RM) $(ALL)
