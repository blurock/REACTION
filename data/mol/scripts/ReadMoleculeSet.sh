#! /usr/bin/tcsh -f

set PROGRAM         = Reaction.exe

if($#argv > 0) then
    if($1 == '--program') then
	shift
	set PROGRAM = $1
	shift
    endif
endif


set MOLECULEDATA     = $REACTION_BASE/data/mol
set MOLECULEINPUTS   = $MOLECULEDATA/inputs
set MOLECULESCRIPTS  = $MOLECULEDATA/scripts
set GENERIC          = $REACTION_BASE/data/generic

if( $#argv <  2) then
echo   " Usage: $0 [--program ProgramName] SaveFileRoot  SaveFileCount "
echo   "        SaveFileRoot:          The rootname of the save file"
echo   "        SaveFileCount:         The count of the current save file"
echo   "        MoleculeFileRoot       The root name of the molecule file"
echo   "        ProgramName:           The executable to use (default Reaction.exe)"
echo   ""
echo   " This increments the SaveFileCount by 1"
echo   " This script expects the following files:"
echo   "      The molecular structure:     $MOLECULEDATA/sets/MoleculeDir/structure.sdf"
echo   "      The chemkin/name information  $MOLECULEDATA/sets/MoleculeDir/chemkin.inp"
echo   "             (corresponding to class: $MOLECULEDATA/inputs/MoleculeChemkinClass.inp)"
echo   "      The moldat propoerties:     $MOLECULEDATA/sets/MoleculeDir/moldat.inp"
echo   "             (corresponding to class: $MOLECULEDATA/inputs/WarnatzMoldatClass.inp)"
echo   "      The molecule list:      $MOLECULEDATA/sets/MoleculeDir/molecules.lst"
exit(1)
endif

set SAVE         = $1
set SAVECOUNT    = $2
set MOLECULEDIR  = $3

if(!(-f $MOLECULEDATA/sets/$MOLECULEDIR/structure.sdf)) then
    echo "$MOLECULEDATA/sets/$MOLECULEDIR/structure.sdf not found"
    exit(1)
endif
if(!(-f $MOLECULEDATA/sets/$MOLECULEDIR/chemkin.inp)) then
    echo "$MOLECULEDATA/sets/$MOLECULEDIR/chemkin.inp not found"
    exit(1)
endif
if(!(-f $MOLECULEDATA/sets/$MOLECULEDIR/moldat.inp)) then
    echo "$MOLECULEDATA/sets/$MOLECULEDIR/moldat.inp not found"
    exit(1)
endif
if(!(-f $MOLECULEDATA/sets/$MOLECULEDIR/molecules.lst)) then
    echo "$MOLECULEDATA/sets/$MOLECULEDIR/molecules.lst not found"
    exit(1)
endif

set SET = $MOLECULEDATA/sets/$MOLECULEDIR

cat $MOLECULEINPUTS/MoleculeTransferPrefix.inp >! $SET/MoleculeTransfer.inp
cat $SET/molecules.lst >> $SET/MoleculeTransfer.inp
cat $MOLECULEINPUTS/MoleculeTransferPostfix.inp >> $SET/MoleculeTransfer.inp

cat <<EOF > $SET/input.prg
ReadMol Molecule $SET/structure.sdf
Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $SET/chemkin.inp 0
Read $MOLECULEINPUTS/WarnatzMoldatClass.inp $SET/moldat.inp 0 
Read $MOLECULEINPUTS/MoleculeTransferClass.inp $SET/MoleculeTransfer.inp 0
RunAlgorithm MoveIt 0
Store Molecule Molecule InstanceNameList
END
EOF
$PROGRAM xxx Change $SAVE $SAVECOUNT < $SET/input.prg


#$PROGRAM xxx Operate $SAVE    $SAVECOUNT ReadMol Molecule $SET/structure.sdf
#@ SAVECOUNT++

#$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEDATA/inputs/MoleculeChemkinClass.inp $SET/chemkin.inp 0
#$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEINPUTS/WarnatzMoldatClass.inp $SET/moldat.inp 0 

#$PROGRAM xxx Change $SAVE     $SAVECOUNT Read $MOLECULEINPUTS/MoleculeTransferClass.inp $SET/MoleculeTransfer.inp 0
#$PROGRAM xxx Change $SAVE     $SAVECOUNT RunAlgorithm MoveIt 0
#$PROGRAM xxx Change $SAVE     $SAVECOUNT Store Molecule Molecule InstanceNameList

