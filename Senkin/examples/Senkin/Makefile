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
	-lSenkin\
	-lSenkinRates\
	-lRxnMechCreate\
	-lRxnMech\
	-lMolCreate\
	-lThermoTables\
	-lDbaseRxn\
	-lDbaseMolRxn\
	-lRxn\
	-lMoleculeSet\
	-lMolecule\
	-lSECharge\
	-lMolStats\
	-lMolValence\
	-lMolBond\
	-lMolAtom\
	-lDbase\
	-lStaticAtom\
        -lSystem\
        -lONames\
        -lEigenValues\
        -lEigenValuesOut\
        -lPCAAnalysis\
        -lMatrixOut\
        -lBasicLaTeXTable\
        -lMatrixUtilities\
        -lMatrixNumeric\
        -lVector\
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
