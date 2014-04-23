# FILE     Makefile
# PACKAGE  MolStats
# AUTHOR   Edward S. Blurock
#
# CONTENT
#   Makefile for example programs of the "MolStats" package.
#
# COPYRIGHT (C)  1997 Reaction Library

#V PackageRoot . . . . . . . . . . . . . . The root directory of this package.
PackageRoot:=$(shell cd ../..; pwd)


#I
include ../../etc/Reaction/MakeDef

LIBS =\
	-L$(PackageRoot)/lib\
	-L$(CodeBaseRoot)/lib\
	-lMolStats\
	-lStaticAtom\
	-lFullSystem\
	-lMolecule\
	-lMolValence\
	-lMolBond\
	-lMolAtom\
	-lDbase\
	-lStaticAtom\
        -lSystem\
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
