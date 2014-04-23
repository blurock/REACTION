# FILE     Makefile
# PACKAGE  EquilibriumConst
# AUTHOR   Edward S. Blurock
#
# CONTENT
#   Makefile for example programs of the "EquilibriumConst" package.
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
	-lEquilibriumConst\
	-lThermoProps\
	-lThermoTables\
	-lUtilities\
	-lThermoProps\
	-lThermoTables\
	-lRxn\
	-lMolecule\
	-lMolBond\
	-lMolAtom\
	-lDbase\
	-lStaticAtom\
	-lExpressionTree\
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
        -lGraph\
	-lPairs\
        -lSystem\
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
	CompileC++ -o otest $(OBJECTS) $(LIBS)


cleaner::
	$(RM) $(ALL)

