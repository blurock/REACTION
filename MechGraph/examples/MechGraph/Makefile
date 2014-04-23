# FILE     Makefile
# PACKAGE  MechGraph
# AUTHOR   Edward S. Blurock
#
# CONTENT
#   Makefile for example programs of the "MechGraph" package.
#
# COPYRIGHT (C)  1997 Reaction Library

#V PackageRoot . . . . . . . . . . . . . . The root directory of this package.
PackageRoot:=$(shell cd ../..; pwd)


#I
include ../../etc/Reaction/MakeDef

LIBS =\
	-L$(PackageRoot)/lib\
	-L$(CodeBaseRoot)/lib\
	-L$(CCROOT)/lib\
	-L$(CodeBaseRoot)/include/CC/lib\
	-lMechGraph\
	-lRxnMechCreate\
	-lSenkin\
	-lSenkinRates\
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
	-lLstOps\
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
