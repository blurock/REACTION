# FILE     Makefile
# PACKAGE  Flame
# AUTHOR   Edward S. Blurock
#
# CONTENT
#   Makefile for example programs of the "Flame" package.
#
# COPYRIGHT (C)  1997 Reaction Library

#V PackageRoot . . . . . . . . . . . . . . The root directory of this package.
PackageRoot:=$(shell cd ../..; pwd)


#I
include ../../etc/Reaction/MakeDef


ALL=otest
OBJECTS=main.o
LIBS=\
	-L$(PackageRoot)/lib\
	-L$(CodeBaseRoot)/lib\
	-L/opt/local/lib\
        -lFlame\
        -lThermoProps\
	-lThermoTables\
	-lRxn\
	-lSECharge\
	-lMolecule\
	-lMolBond\
	-lMolAtom\
	-lDbase\
	-lStaticAtom\
	-lMolStats\
	-lConsecutive\
	-lVectorOperations\
	-lPopulationBasedOptimization\
	-lGeneticOperations\
	-lOptimize\
	-lCobwebCluster\
	-lDecisionTreeAlgorithms\
	-lEntropyObjects\
        -lDescriptionProbs\
	-lPredicateGen\
	-lPredicateObjects\
	-lDistributionAlgorithm\
	-lPrimitiveStats\
	-lEvaluationTree\
	-lNumericOperations\
	-lParameterizedFunction\
        -lGeneralGraph\
	-lGraphOperations\
	-lClusterTree\
	-lExpressionTree\
	-lNumericProperties\
	-lMenuObjects\
	-lUtilities\
	-lInstancePCA\
        -lGoalObjects\
	-lAlgorithmObjects\
	-lInstanceAlgorithms\
	-lInstanceObjects\
	-lDataObjects\
	-lSelectObjects\
	-lDirectedTreeObjects\
	-lOperationObjects\
	-lFunctionReal1DObjects\
	-lLogicalObjects\
	-lLogicalOperations\
	-lNumericObjects\
	-lCoreDataObjects\
	-lNonStandard\
        -lPCAAnalysis\
        -lEigenValues\
        -lEigenValuesOut\
        -lGraph\
        -lSystem\
	-lPairs\
	-lMatrixUtilities\
	-lMatrixNumeric\
	-lMatrixOut\
        -lBasicLaTeXTable\
	-lVector\
        -lONames\
        -lMixedClasses\
        -lString\
        -lObjects\
        -lCommBuffer\
        -lm  -lgdbm

#F Targets.

all: $(ALL)

otest: $(OBJECTS)
	g++ -g -o otest $(OBJECTS) $(LIBS)

cleaner::
	$(RM) $(ALL)
