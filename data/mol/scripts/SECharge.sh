#! /usr/bin/tcsh -f

set PROGRAM          = otest
set CHARGEALGORITHM  = Charge

set MOLECULEDATA     = $REACTION_BASE/data/mol
set MOLECULEINPUTS   = $MOLECULEDATA/inputs
set MOLECULESCRIPTS  = $MOLECULEDATA/scripts
set GENERIC          = $REACTION_BASE/data/generic

if( $#argv <  3) then
echo   " Usage: $0 SaveFileRoot  SaveFileCount [ProgramName]"
echo   "        SaveFileRoot:          The rootname of the save file"
echo   "        SaveFileCount:         The count of the current save file"
echo   "        MoleculeClass:         The name of the molecule class"
echo   "        Molecules:             The list of molecules to calculate with"
echo   ""
echo   " This increments the SaveFileCount by 1"
exit(1)
endif

set SAVE         = $1
set SAVECOUNT    = $2
set MOLECULECLASS = $3
shift
shift
shift

# -------------------------------------------------
#  The MoleculeNames.inp file
# -------------------------------------------------
# MoleculeNames    KeySet
#    1. The class of the molecule
#    2. Pairs of names: instance name, result vector name (of atom electronegativities)
# -------------------------------------------------
cat <<EOF >! MoleculeNames.inp
MoleculeNames -------  The MoleculeNames variable definition -----
Attributes:
$MOLECULECLASS
EOF

while($#argv > 0)
echo $1 $1-ElectronegativityVector >> MoleculeNames.inp
shift
end
cat <<EOF >> MoleculeNames.inp
END
%% No Instances -------------------------------------------------
END
EOF

# -------------------------------------------------
#  The MoleculeNamesClass.inp file
# -------------------------------------------------
cat <<EOF >! MoleculeNamesClass.inp
ObjectClasses:
%% -------------------------------------------------
END
ClassNamePairs:
MoleculeNames    KeySet
END
%% -------------------------------------------------
ObjectClasses:
END
%% -------------------------------------------------
ClassNamePairs:
END
%% -------------------------------------------------
EOF

# -------------------------------------------------
#  Calculating the Semi-Empirical Charge
#     1. Read in the MoleculeNames variable
#     2. Run the algorithm
# -------------------------------------------------
$PROGRAM xxx Operate $SAVE $SAVECOUNT Read MoleculeNamesClass.inp MoleculeNames.inp 0
@ SAVECOUNT++
$PROGRAM xxx Change $SAVE $SAVECOUNT RunAlgorithm $CHARGEALGORITHM 0
