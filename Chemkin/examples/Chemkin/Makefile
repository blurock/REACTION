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
	-lChemkin\
	-lMolCreate\
	-lThermoTables\
	-lDbaseRxn\
	-lDbaseMolRxn\
	-lRxn\
	-lMolecule\
	-lSECharge\
	-lMolStats\
	-lMolValence\
	-lMolBond\
	-lMolAtom\
	-lDbase\
	-lStaticAtom\
	-lStaticAtom\
        -lSystem\
        -lONames\
        -lMixedClasses\
        -lString\
	-lMatrix\
        -lObjects\
        -lCommBuffer\
	-lsello -lbasis\
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
