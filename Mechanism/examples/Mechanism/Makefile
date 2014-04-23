# FILE     Makefile
# PACKAGE  Mechanism
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
	-L/$(CCROOT)/lib\
	-L$(CodeBaseRoot)/include/CC/lib\
	-lFullReaction\
	-lFlame\
	-lMechanismGraph\
	-lMechLumping\
	-lMechanism\
	-lMechLumping\
	-lMechanismGraph\
	-lRxn\
	-lUtilities\
	-lEquilibriumConst\
        -lThermoProps\
	-lThermoTables\
	-lRxn\
	-lSECharge\
	-lMolecule\
	-lMolBond\
	-lMolAtom\
	-lDbase\
	-lFullSystem\
	-lStaticAtom\
	-lMolStats\
	-lFullSystem\
	-lDbase\
	-lConsecutive\
	-lVectorOperations\
	-lGeneticOperations\
	-lPopulationBasedOptimization\
	-lOptimize\
	-lCobwebCluster\
        -lEquivalentClasses\
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
	-lInstancePCA\
        -lGoalObjects\
	-lBasisSystem\
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
        -lCommBuffe
	-lPredicateObjects\
	-lBasisSystem\
        -lmolecules\
	-lmolprops\
        -lmolecules\
	-lmolprops\
        -lstatistics\
        -lgraphs\
        -lproperty\
        -ldbase\
        -ldatmat\
        -lcomlib\
        -lbasis\
        -lm

OBJECTS = main.o 

#V
ALL= otest

#F Targets.

all: $(ALL)

otest: $(OBJECTS)
	g++ -o otest $(OBJECTS) $(LIBS)


cleaner::
	$(RM) $(ALL)
