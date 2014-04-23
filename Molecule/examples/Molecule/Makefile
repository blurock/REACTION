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
        -lThermoProps\
	-lThermoTables\
	-lMolecule\
	-lMolBond\
	-lMolAtom\
	-lDbase\
	-lStaticAtom\
	-lMolStats\
        -lFullSystem\
	-lConsecutive\
	-lVectorOperations\
	-lPopulationBasedOptimization\
	-lGeneticOperations\
	-lOptimize\
	-lCobwebCluster\
	-lClusterTree\
	-lInstancePCA\
	-lPCAAnalysis\
        -lEquivalentClasses\
	-lDecisionTreeAlgorithms\
	-lGraphOperations\
	-lNumericProperties\
	-lEntropyObjects\
        -lDescriptionProbs\
	-lPredicateGen\
	-lPredicateObjects\
	-lDistributionAlgorithm\
	-lEigenValuesOut\
	-lEigenValues\
	-lEigenValuesOut\
	-lPrimitiveStats\
	-lEvaluationTree\
	-lNumericOperations\
	-lParameterizedFunction\
        -lGeneralGraph\
        -lEquivalentClasses\
	-lDescriptionProbs\
	-lInstanceAlgorithms\
	-lGoalObjects\
	-lAlgorithmObjects\
	-lMenuObjects\
	-lDirectedTreeObjects\
	-lSelectObjects\
	-lInstanceObjects\
	-lFunctionReal1DObjects\
	-lDistributionAlgorithm\
	-lExpressionTree\
	-lPredicateGen\
	-lPredicateObjects\
	-lParameterizedFunction\
	-lOperationObjects\
	-lDataObjects\
	-lVectorOperations\
	-lLogicalOperations\
	-lNumericOperations\
	-lIterator\
	-lLogicalObjects\
	-lNumericObjects\
        -lDbase\
	-lCoreDataObjects\
	-lNonStandard\
	-lPrimitiveStats\
        -lGraph\
	-lPairs\
	-lMatrixUtilities\
        -lBasicLaTeXTable\
	-lMatrixNumeric\
	-lMatrixOut\
	-lMatrixUtilities\
        -lBasicLaTeXTable\
	-lMatrixNumeric\
	-lMatrixOut\
	-lVector\
        -lONames\
        -lMixedClasses\
        -lString\
        -lObjects\
        -lCommBuffer\
	-lPredicateObjects\
        -lm  -lgdbm

OBJECTS = main.o 

#V
ALL= otest

#F Targets.

all: $(ALL)

otest: $(OBJECTS)
	CompileC++ -o otest $(OBJECTS) $(LIBS)


cleaner::
	$(RM) $(ALL)
