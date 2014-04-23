#! /usr/bin/tcsh -f

set MOLECULEDATA     = $REACTION_BASE/data/mol
set MOLECULESCRIPTS  = $MOLECULEDATA/scripts
set READSET          = $MOLECULESCRIPTS/ReadMoleculeSet.sh

set PROGRAM         = Reaction.exe

if($#argv > 0) then
    if($1 == '--program') then
	shift
	set PROGRAM = $1
	shift
    endif
endif

if( $#argv <  2) then
echo   " Usage: $0 [--program ProgramName] SaveFileRoot  SaveFileCount "
echo   "        SaveFileRoot:          The rootname of the save file"
echo   "        SaveFileCount:         The count of the current save file"
echo   "        ProgramName:           The executable to use (default Reaction.exe)"
echo   ""
exit(1)
endif

set SAVE         = $1
set SAVECOUNT    = $2

$READSET $1 $2 C0CO
$READSET $1 $2 C1CO
$READSET $1 $2 C1HO
$READSET $1 $2 C2CO
$READSET $1 $2 C3CO
$READSET $1 $2 C3H5O
$READSET $1 $2 C4CO
$READSET $1 $2 C4ket
$READSET $1 $2 C4peroxy
