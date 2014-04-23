#! /usr/bin/tcsh -f
set PROGRAM         = Reaction.exe

set MOLECULEDATA     = $REACTION_BASE/data/mol
set MOLECULEINPUTS   = $MOLECULEDATA/inputs
set MOLECULESCRIPTS  = $MOLECULEDATA/scripts
set GENERIC          = $REACTION_BASE/data/generic

if( $#argv <  2) then
echo   " Usage: $0 SaveFileRoot  SaveFileCount [ProgramName]"
echo   "        SaveFileRoot:          The rootname of the save file"
echo   "        SaveFileCount:         The count of the current save file"
echo   "        ProgramName:           The executable to use (default Reaction.exe)"
echo   ""
echo   " This increments the SaveFileCount by 1"
exit(1)
endif

if($#argv ==  3) then
    set PROGRAM = $3
endif

set SAVE         = $1
set SAVECOUNT    = $2

$PROGRAM xxx Operate $SAVE  $SAVECOUNT Read $MOLECULEINPUTS/MoleculeAlgorithmClass.inp $MOLECULEINPUTS/MoleculeAlgorithm.inp 0
@ SAVECOUNT++
$PROGRAM xxx Change $SAVE  $SAVECOUNT SetAlgorithmClass MoleculeAlgorithmRun
