#! /usr/bin/tcsh -f
set PROGRAM         = Reaction.exe

set MOLECULEDATA     = $REACTION_BASE/data/mol
set MOLECULEINPUTS   = $MOLECULEDATA/inputs
set MOLECULESCRIPTS  = $MOLECULEDATA/scripts
set GENERIC          = $REACTION_BASE/data/generic

set PROGRAM = Reaction.exe

if( $#argv <  2) then
echo   " Usage: $0 SaveFileRoot  SaveFileCount [ProgramName]"
echo   "        SaveFileRoot:          The rootname of the save file"
echo   "        SaveFileCount:         The count of the current save file"
echo   ""
echo   " This increments the SaveFileCount by 1"
exit(1)
endif

set SAVE         = $1
set SAVECOUNT    = $2

$PROGRAM xxx Operate $SAVE    $SAVECOUNT Read $MOLECULEINPUTS/EquilibriumClass.inp $MOLECULEINPUTS/Equilibrium.inp  0
