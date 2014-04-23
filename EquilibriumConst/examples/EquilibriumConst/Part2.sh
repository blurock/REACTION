#! /usr/bin/tcsh -f

set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA   = $REACTION_BASE/data/rxn
set REACTIONINPUTS = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

set GENERIC         = $REACTION_BASE/data/generic

# Read in Basic Reaction data
otest xxx Operate test     6 ReadRxn Reaction Molecule reverse/rxn1.sdf None

# Read and move in Forward reaction constants
otest xxx Change  test     7 Read reverse/BaulchReactionClass.inp reverse/BaulchReaction.inp 0
otest xxx Change  test     7 Read reverse/BaulchTransferClass.inp reverse/BaulchTransfer.inp 0
otest xxx Change  test     7 RunAlgorithm MoveIt 0

# Fill in Reactions with Molecules
otest xxx Operate test     7 Read reverse/FillClass.inp reverse/Fill.inp 0
otest xxx Change  test     8 FillRxn Molecule StandardReaction ReactionNames

# Calculate Reverse Rate for each reaction
otest xxx Operate test     8 Read reverse/RevExpressionClass.inp reverse/RevExpression.inp 0
otest Xxx Change  test     9 RunAlgorithm Operation 0
otest xxx Experiment test  9 Print Instance O+H2=OH+H Reaction
