#! /usr/bin/tcsh -f
set PROGRAM         = Reaction.exe

set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA   = $REACTION_BASE/data/rxn
set REACTIONINPUTS = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

set MECHANISMDATA    = $REACTION_BASE/data/mech
set MECHANISMINPUTS  = $MECHANISMDATA/inputs
set MECHANISMSCRIPTS = $MECHANISMDATA/scripts

set GENERIC         = $REACTION_BASE/data/generic

if( $#argv <  2) then
echo   " Usage: $0 SaveFileRoot  SaveFileCount [ProgramName Initialize]"
echo   "        SaveFileRoot:          The rootname of the save file"
echo   "        SaveFileCount:         The count of the current save file"
echo   "        ProgramName:           The executable to use (default Reaction.exe)"
echo   "        Initialize:            if present, initialize runtime environment"
echo   ""
echo   " This increments the SaveFileCount by 1"
exit(1)
endif
set PROGRAM = Reaction.exe
if($#argv >=  3) then
    set PROGRAM = $3
endif
set FIRST = Operate
if($#argv == 4) then
set FIRST = Initial
endif

set SAVE         = $1
set SAVECOUNT    = $2

$REACTIONSCRIPTS/RxnAlgorithms.sh $SAVE $SAVECOUNT $PROGRAM
@ SAVECOUNT++
$PROGRAM xxx Change $SAVE  $SAVECOUNT Read $MECHANISMINPUTS/MechanismAlgorithmClass.inp $MECHANISMINPUTS/MechanismAlgorithm.inp 0
$PROGRAM xxx Change $SAVE  $SAVECOUNT SetAlgorithmClass MechanismAlgorithmRun
