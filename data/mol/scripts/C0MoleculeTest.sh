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

set SAVE         = $1
set SAVECOUNT    = $2

$PROGRAM xxx Operate $SAVE $SAVECOUNT ReadMol Molecule $MOLECULEDATA/simple/C0HO.sdf
@ SAVECOUNT++
$PROGRAM xxx Change $SAVE  $SAVECOUNT Read $MOLECULEINPUTS/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/C0HO.inp 0
$PROGRAM xxx Change $SAVE  $SAVECOUNT Read $MOLECULEINPUTS/C0MoleculeTransferClass.inp $MOLECULEINPUTS/C0MoleculeTransfer.inp 0
$PROGRAM xxx Change $SAVE  $SAVECOUNT RunAlgorithm MoveIt 0
$PROGRAM xxx Change $SAVE  $SAVECOUNT Store Molecule Molecule InstanceNameList
