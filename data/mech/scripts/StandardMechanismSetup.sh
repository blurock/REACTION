#! /bin/tcsh -f
set PROGRAM         = Reaction.exe

set TOOLS        = $ANALYSIS_BASE/tools/programs

set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA   = $REACTION_BASE/data/rxn
set REACTIONINPUTS = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

set MECHANISMDATA    = $REACTION_BASE/data/mech
set MECHANISMINPUTS  = $MECHANISMDATA/inputs
set MECHANISMSCRIPTS = $MECHANISMDATA/scripts

set MECHSDIR        = $REACTROOT/data/mechs/submechanisms

set GENERIC         = $REACTION_BASE/data/generic

$GENERIC/InitialRun.sh test -1
$MECHANISMSCRIPTS/MechanismAlgorithms.sh test 0
$MECHANISMSCRIPTS/MechanismSetup.sh test 1
$MOLECULESCRIPTS/InitializeEquilibrium.sh test 2
$MOLECULESCRIPTS/ReadMolecules0.sh test 3
$MOLECULESCRIPTS/ReadMoleculeSet.sh test 4 C3H5O
$MOLECULESCRIPTS/CalculateEquilibrium.sh test 5 MoleculeEquilibrium.inp
