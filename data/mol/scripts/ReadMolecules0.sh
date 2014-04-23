#! /bin/tcsh -f

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

$PROGRAM xxx Operate $SAVE    $SAVECOUNT ReadMol Molecule $MOLECULEDATA/simple/C0HO.sdf
@ SAVECOUNT++
$PROGRAM xxx Change $SAVE     $SAVECOUNT ReadMol Molecule $MOLECULEDATA/simple/C1HO.sdf
$PROGRAM xxx Change $SAVE     $SAVECOUNT ReadMol Molecule $MOLECULEDATA/simple/C2HO.sdf
$PROGRAM xxx Change $SAVE     $SAVECOUNT ReadMol Molecule $MOLECULEDATA/simple/C3HO.sdf
$PROGRAM xxx Change $SAVE     $SAVECOUNT ReadMol Molecule $MOLECULEDATA/simple/C4HO.sdf

$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/C0HO.inp 0
$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/C1HO.inp 0
$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/C2HO.inp 0
$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/C3HO.inp 0
$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/C4HO.inp 0
$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $MOLECULEDATA/chemkin/Mol1.inp 0
$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEINPUTS/WarnatzMoldatClass.inp $MOLECULEDATA/chemkin/WarnatzMoldat.inp 0 

$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEINPUTS/MoleculeTransferClass.inp $MOLECULEINPUTS/Molecule0Transfer.inp 0
$PROGRAM xxx Change $SAVE  $SAVECOUNT RunAlgorithm MoveIt 0
$PROGRAM xxx Change $SAVE  $SAVECOUNT Store Molecule Molecule InstanceNameList
