#! /usr/bin/tcsh -f


set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA   = $REACTION_BASE/data/rxn
set REACTIONINPUTS = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

set GENERIC         = $REACTION_BASE/data/generic

rm *.dbf

# Initialize Algorithms
otest xxx Initial test    -1 Read reverse/algClass.inp reverse/alg.inp 0
otest xxx Change  test     0 SetAlgorithmClass TestAlgorithmRun

# Setup Molecule and Reaction Information (database and classes)
$MOLECULESCRIPTS/ReactionSetup.sh test 0 otest

# Read in Basic Molecule data
otest xxx Operate test     1 ReadMol Molecule $MOLECULEDATA/simple/C0HO.sdf

# Read in chemkin and move to molecule
otest xxx Operate  test     2 Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/C0HO.inp 0
otest xxx Change   test     3 Read reverse/TransferClass.inp reverse/Transfer.inp 0
otest xxx Change   test     3 RunAlgorithm MoveIt 0

# Calculate Equilibrium Constants for Molecules
otest xxx Operate test      3 Read reverse/CalcEquilibriumClass.inp reverse/CalcEquilibrium.inp 0
otest xxx Change  test      4 RunAlgorithm Operation 0
otest xxx Change  test      4 RunAlgorithm MoveIt 0
otest xxx Experiment test   4 Store Molecule Molecule InstanceNameList
# Read in Basic Reaction data
otest xxx Operate test     4 ReadRxn Reaction Molecule reverse/rxn1.sdf None
otest xxx Change  test     5 Read reverse/BaulchReactionClass.inp reverse/BaulchReaction.inp 0
otest xxx Change  test     5 Read reverse/BaulchTransferClass.inp reverse/BaulchTransfer.inp 0
otest xxx Change  test     5 RunAlgorithm MoveIt 0
otest xxx Experiment test  5 Print Instance O+HO2=OH+O2 Reaction

# Fill in molecule data into reaction (for use of molecular constants)
otest xxx Operate test     5 Read reverse/FillClass.inp reverse/Fill.inp 0
otest xxx Change  test     6 FillRxn Molecule StandardReaction ReactionNames

# Calculate Reverse Reaction Constants from molecule equilibrium constants
otest xxx Operate test     6 Read reverse/RevExpressionClass.inp reverse/RevExpression.inp 0
otest Xxx Change  test     7 RunAlgorithm Operation 0
otest xxx Experiment test  7 Print Instance O+H2=OH+H Reaction
