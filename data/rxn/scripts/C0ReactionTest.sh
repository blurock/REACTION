#! /usr/bin/tcsh -f

set PROGRAM         = Reaction.exe

set MOLECULEDATA   = $REACTION_BASE/data/mol
set MOLECULEINPUTS = $MOLECULEDATA/inputs
set MOLECULESCRIPTS = $MOLECULEDATA/scripts

set REACTIONDATA   = $REACTION_BASE/data/rxn
set REACTIONINPUTS = $REACTIONDATA/inputs
set REACTIONSCRIPTS = $REACTIONDATA/scripts

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

$PROGRAM xxx Operate test $SAVECOUNT ReadRxn Reaction Molecule $REACTIONDATA/sets/rxnC0a.sdf None
@ SAVECOUNT++
$PROGRAM xxx Change  test $SAVECOUNT Read $REACTIONDATA/baulch/BaulchClass.inp $REACTIONDATA/baulch/Baulch-C0.inp 0
$PROGRAM xxx Change  test $SAVECOUNT Read $REACTIONINPUTS/C0RxnTransferClass.inp $REACTIONINPUTS/C0RxnTransfer.inp 0
$PROGRAM xxx Change  test $SAVECOUNT RunAlgorithm MoveIt 0
#$PROGRAM xxx Change  test $SAVECOUNT Read $REACTIONINPUTS/C0FillClass.inp $REACTIONINPUTS/C0Fill.inp 0
#$PROGRAM xxx Change  test $SAVECOUNT FillRxn Molecule StandardReaction ReactionNames
$PROGRAM xxx Change  test $SAVECOUNT FillRxn Molecule StandardReaction InstanceNameList

