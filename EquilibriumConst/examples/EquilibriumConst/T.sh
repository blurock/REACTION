#! /usr/bin/tcsh -f
#! /usr/bin/tcsh -f


set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA   = $REACTION_BASE/data/rxn
set REACTIONINPUTS = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

set GENERIC         = $REACTION_BASE/data/generic

# Calculate Reverse Reaction Constants from molecule equilibrium constants
otest xxx Operate test     8 Read reverse/RevExpressionClass.inp reverse/RevExpression.inp 0
otest Xxx Change  test     9 RunAlgorithm Operation 0
#otest xxx Experiment test  9 Print Instance O+H2=OH+H Reaction
